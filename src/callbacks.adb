with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Calendar;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;

with AWS.MIME;
with AWS.Utils;
with AWS.Digest;
with AWS.Config;
with AWS.Messages;
with AWS.OS_Lib;
with AWS.Resources;
with AWS.Parameters;
with AWS.Translator;
with AWS.Response.Set;
with AWS.Server.HTTP_Utils;
with AWS.Services.Directory;
with AWS.Services.Page_Server;

with GNAT.OS_Lib;
with GNAT.Directory_Operations;

with Wiki.Utils;
with Wiki.Parser;
with Wiki.Sidebar;
with Wiki.HTML_Output;

with Users;

package body Callbacks is

   use AWS;
   use type Ada.Calendar.Time;

   function Get_WWW_Root (Request : in AWS.Status.Data) return String;

   function Get_File_Name (URI : in String) return String;
   pragma Inline (Get_File_Name);

   function Is_Folder (URI : String) return Boolean;

   procedure Write_File
     (File_Name : String;
      Data      : Ada.Streams.Stream_Element_Array);

   function Read_File (Name : String) return String
     renames Wiki.Utils.Read_File;

   function Expand_Wiki (Text : String) return String;

   Sidebar_File : constant String := "/wiki/layout.wiki";

   Wiki_Root   : Ada.Strings.Unbounded.Unbounded_String;
   Wiki_Prefix : Ada.Strings.Unbounded.Unbounded_String;
   Wiki_Suffix : Ada.Strings.Unbounded.Unbounded_String;
   Wiki_Prefix_Time : Ada.Calendar.Time;

   ----------------------
   -- Read_Wiki_Prefix --
   ----------------------

   procedure Read_Wiki_Prefix (Root : String) is
      use AWS.Resources;
      use Ada.Strings.Unbounded;
      Name : constant String := Root & "/wiki.prefix";
   begin
      if Wiki_Root /= Root
        or else Wiki_Prefix_Time < File_Timestamp (Name)
      then
         Wiki_Root   := To_Unbounded_String (Root);
         Wiki_Prefix := To_Unbounded_String (Read_File (Name));
         Wiki_Suffix := To_Unbounded_String
           (Read_File (Root & "/wiki.suffix"));
         Wiki_Prefix_Time := File_Timestamp (Name);
      end if;
   end Read_Wiki_Prefix;

   -----------------
   -- Expand_Wiki --
   -----------------

   function Expand_Wiki (Text : String) return String is
      procedure Parse is new Wiki.Parser.Parse
        (Context       => Wiki.HTML_Output.Context,
         Start_Element => Wiki.HTML_Output.Start_Element,
         End_Element   => Wiki.HTML_Output.End_Element,
         Characters    => Wiki.HTML_Output.Characters);

      Data    : Wiki.HTML_Output.Context;
   begin
      Wiki.HTML_Output.Initialize (Data, "/");
      Parse (ASCII.LF & Text, Data);

      return Wiki.HTML_Output.Get_Text (Data);
   end Expand_Wiki;

   ---------------
   -- Edit_Wiki --
   ---------------

   function Edit_Wiki (URI, Text, Root : in String) return AWS.Response.Data is
      use Ada.Strings.Unbounded;

      Preview_Start : constant String :=
        "<div class='wiki.preview'>";

      Preview_End : constant String :=
        "</div'>";

      Edit_Start : constant String :=
        "<form method='post' action='/edit_wiki/'>"
        & "<input type='hidden' name='uri' value='" & URI
        & "'><textarea name='text' rows='30' cols='80'>";

      Edit_End : constant String :=
        "</textarea><p><input type='submit' name='post' value='Post'/>"
        & "<input type='submit' name='post' value='Preview'/>"
        & "</p></form>";

      Wiki_URI : constant String :=
        Wiki.Parser.Replace (URI, "/edit_wiki/", "wiki:");

      Sidebar : constant String :=
        Wiki.Sidebar.Expand (Wiki_URI, Root & Sidebar_File, "/");

      Content : constant String := To_String (Wiki_Prefix)
        & Sidebar
        & Preview_Start
        & Expand_Wiki (Text)
        & Preview_End
        & Edit_Start
        & Text
        & Edit_End
        & To_String (Wiki_Suffix);

      Result : AWS.Response.Data
        := AWS.Response.Build (Content_Type => AWS.MIME.Text_HTML,
                               Message_Body => Content);
   begin
      return Result;
   end Edit_Wiki;

   ---------------
   -- Edit_Wiki --
   ---------------

   function Edit_Wiki
     (Request : in AWS.Status.Data) return AWS.Response.Data
   is
      use Ada.Strings.Unbounded;
      URI     : constant String :=
        Wiki.Parser.Replace (Status.URI (Request), "/edit_wiki/", "/");
      Root    : constant String := Get_WWW_Root (Request);
      File    : constant String := Root & URI & ".wiki";
   begin
      Read_Wiki_Prefix (Root);

      return Edit_Wiki (URI, Read_File (File), Root);
   exception
      when E : Ada.Text_IO.Name_Error =>
         return Edit_Wiki (URI, "", Root);
   end Edit_Wiki;

   ------------------
   -- Get_WWW_Root --
   ------------------

   function Get_WWW_Root (Request : in AWS.Status.Data) return String is
      Host : constant String := Status.Host (Request);
   begin
      if AWS.Utils.Is_Directory (Host) then
         return Host;
      else
         return Config.WWW_Root (Config.Get_Current);
      end if;
   end Get_WWW_Root;

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name (URI : in String) return String is
   begin
      if URI = "/" then
         return "/index.html";
      else
         return URI;
      end if;
   end Get_File_Name;

   ----------------------
   -- Get_Wiki_Or_HTML --
   ----------------------

   function Get_Wiki_Or_HTML
     (Request : in AWS.Status.Data)
     return AWS.Response.Data
   is
      Root    : constant String := Get_WWW_Root (Request);
      URI     : constant String := Status.URI (Request);
      File    : constant String := Root & Get_File_Name (URI);
      Wiki    : constant String := File & ".wiki";
   begin
      if Utils.Is_Regular_File (Wiki)
        and then (not Resources.Is_Regular_File (File)
                  or else Resources.File_Timestamp (File) <
                          Utils.File_Time_Stamp (Wiki))
      then
         return Get_Wiki (Request);
      elsif Resources.Is_Regular_File (File) then
         return Response.File
           (Content_Type => MIME.Content_Type (File),
            Filename     => File);
      elsif Utils.Is_Directory (File) then
         declare
            Directory_Browser_Page : constant String
              := Config.Directory_Browser_Page (Config.Get_Current);
         begin
            return Response.Build
              (Content_Type => MIME.Text_HTML,
               Message_Body =>
                 Services.Directory.Browse
                   (File, Directory_Browser_Page, Request));
         end;
      else
         return Response.Acknowledge
           (Messages.S404,
            "<p>Page '" & URI & "' Not found.");
      end if;
   end Get_Wiki_Or_HTML;

   --------------
   -- Get_Wiki --
   --------------

   function Get_Wiki (Request : in AWS.Status.Data) return AWS.Response.Data is
      use Ada.Strings.Unbounded;

      Root    : constant String := Get_WWW_Root (Request);
      URI     : constant String := Status.URI (Request);
      File    : constant String := Root & Get_File_Name (URI) & ".wiki";
      Time    : constant Ada.Calendar.Time
        := AWS.Resources.File_Timestamp (File);

      function Changed return Boolean is
         use AWS.Status;
         use AWS.Messages;
         use AWS.Server.HTTP_Utils;
         use type Ada.Calendar.Time;

         Since : constant String := If_Modified_Since (Request);
      begin
         if not Is_Valid_HTTP_Date (Since) or else Time > To_Time (Since) then
            return True;
         else
            return False;
         end if;
      end Changed;
   begin
      if not Changed then
         return AWS.Response.Build (Content_Type => AWS.MIME.Text_HTML,
                                    Message_Body => "",
                                    Status_Code  => Messages.S304);
      end if;

      Read_Wiki_Prefix (Root);

      declare
         Wiki_URI : constant String := "wiki:"
           & URI (Uri'First + 1 .. URI'Last);
         Sidebar  : constant String :=
           Wiki.Sidebar.Expand (Wiki_URI, Root & Sidebar_File, "/");
         Text     : constant String :=
           To_String (Wiki_Prefix)
           & Sidebar
           & Expand_Wiki (Read_File (File))
           & To_String (Wiki_Suffix);
         Result  : AWS.Response.Data
           := AWS.Response.Build (Content_Type => AWS.MIME.Text_HTML,
                                  Message_Body => Text);
      begin
         AWS.Response.Set.Add_Header
           (Result,
            AWS.Messages.Last_Modified_Token,
            AWS.Messages.To_HTTP_Date (Time));

         return Result;
      end;
   end Get_Wiki;

   ----------------------
   -- Has_Write_Access --
   ----------------------

   function Has_Write_Access (File_Name : String)return Boolean is
   begin
      return not Utils.Is_Regular_File (File_Name & ".ro");
   end Has_Write_Access;

   -------------------
   -- Post_Paasword --
   -------------------

   procedure Post_Password (Request : in AWS.Status.Data) is
      use AWS.Parameters;
      use Ada.Strings.Unbounded;

      User : constant String := Status.Authorization_Name (Request);
      P    : constant AWS.Parameters.List := Status.Parameters (Request);
      Pwd  : constant String := Get (P, "value");
   begin
      Users.Set_Password (User, Pwd);
   end Post_Password;

   ---------------
   -- Post_Wiki --
   ---------------

   function Post_Wiki (Request : in AWS.Status.Data) return AWS.Response.Data
   is
      use AWS.Parameters;
      use Ada.Strings.Unbounded;

      P    : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      URI  : constant String := Get (P, "uri");
      Text : constant String :=
        Wiki.Parser.Replace (Get (P, "text"), (1 => ASCII.CR), "");
      Post : constant String := Get (P, "post");
      Root : constant String := Get_WWW_Root (Request);
      File : constant String := Root & URI & ".wiki";
   begin
      if not Has_Write_Access (File) then
         return Response.Acknowledge (AWS.Messages.S404);
      end if;

      if Post = "Post" then
         Write_File (File, AWS.Translator.To_Stream_Element_Array (Text));

         return Response.URL (Location => URI);
      else
         return Edit_Wiki (URI, Text, Root);
      end if;
   end Post_Wiki;

   ---------
   -- Put --
   ---------

   function Put (Request : in Status.Data) return Response.Data is
      use AWS.Status;
      use AWS.Digest;

      URI     : constant String := Status.URI (Request);
      File    : constant String := Get_WWW_Root (Request)
                                 & Get_File_Name (URI);
      Stale   : constant Boolean :=
        not Check_Nonce (Authorization_Nonce (Request));

      User : constant String := Authorization_Name (Request);
      Pwd  : constant String := Users.Password (User);
      Mode : constant AWS.Status.Authorization_Type
        := Authorization_Mode (Request);
   begin
      if Mode /= AWS.Status.Digest
        or else User = ""
        or else Pwd = ""
        or else not Check_Digest (Request, Pwd)
        or else Stale
      then
         return Response.Authenticate
                  ("Ada_Ru private", Response.Digest, Stale);
      elsif URI = "/edit_wiki/" then
         return Post_Wiki (Request);
      elsif URI = "/password" then
         Post_Password (Request);
      elsif Is_Folder (File) then
         GNAT.Directory_Operations.Make_Dir (File);
      elsif not Has_Write_Access (File) then
         return Response.Acknowledge (AWS.Messages.S404);
      else
         Write_File (File, Binary_Data (Request));
      end if;

      return Response.Build
        (Content_Type => "text/html",
         Message_Body => "Write success ");

   exception
      when E : others =>
         return Response.Acknowledge
           (Messages.S500,
            Content_Type => "text/plain",
            Message_Body => Ada.Exceptions.Exception_Information (E));
   end Put;

   ---------------------
   -- Private_Service --
   ---------------------

   function Private_Service (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.Digest;

      Stale : constant Boolean
        := not Check_Nonce (Authorization_Nonce (Request));
      User : constant String := Authorization_Name (Request);
      Pwd  : constant String := Users.Password (User);
      Mode : constant AWS.Status.Authorization_Type
        := Authorization_Mode (Request);
   begin
      if Mode /= AWS.Status.Digest
        or else User = ""
        or else Pwd = ""
        or else not Check_Digest (Request, Pwd)
        or else Stale
      then
         return Response.Authenticate
           ("Ada_Ru private", Response.Digest, Stale);
      else
         return AWS.Services.Page_Server.Callback (Request);
      end if;
   end Private_Service;

   ----------------
   -- Write_File --
   ----------------

   procedure Write_File
     (File_Name : in String;
      Data      : in Ada.Streams.Stream_Element_Array)
   is
      use AWS.OS_Lib;
      use Ada.Streams.Stream_IO;

      function Versioned_Name (Name : String; Ver : Positive) return String is
         Img : constant String := Positive'Image (Ver);
      begin
         return Name & Img (2 .. Img'Last);
      end Versioned_Name;

      Temp    : constant String := File_Name & '_';
      Success : Boolean := True;
      Version : Positive := 1;
      File    : File_Type;

      Write_Error : exception;
   begin
      Create (File, Name => Temp);
      Write (File, Data);
      Close (File);

      if Utils.Is_Regular_File (File_Name) then
         while Utils.Is_Regular_File (Versioned_Name (File_Name, Version)) loop
            Version := Version + 1;
         end loop;

         GNAT.OS_Lib.Rename_File
           (Old_Name => File_Name,
            New_Name => Versioned_Name (File_Name, Version),
            Success  => Success);

         if not Success then
            Ada.Exceptions.Raise_Exception
             (Write_Error'Identity,
              "Can't backup " & File_Name & " to " &
              Versioned_Name (File_Name, Version));
            return;
         end if;

         GNAT.OS_Lib.Delete_File
           (Versioned_Name (File_Name, Version), Success);

      end if;

      GNAT.OS_Lib.Rename_File
        (Old_Name => Temp,
         New_Name => File_Name,
         Success  => Success);

      if not Success then
         Ada.Exceptions.Raise_Exception
          (Write_Error'Identity, "Can't move " & Temp & " to " & File_Name);
         return;
      end if;

   exception when E : others =>
      if Is_Open (File) then
         Close (File);
      end if;

      --  Restore file
      GNAT.OS_Lib.Rename_File
        (Old_Name => Versioned_Name (File_Name, Version),
         New_Name => File_Name,
         Success  => Success);

      Ada.Exceptions.Raise_Exception
        (Name_Error'Identity,
         "Write_File " & File_Name & ":"
         & Ada.Exceptions.Exception_Information (E));
      --      Ada.Exceptions.Reraise_Occurrence (E);
   end Write_File;

   ---------------
   -- Is_Folder --
   ---------------

   function Is_Folder (URI : String) return Boolean is
   begin
      if URI = "" or else URI (URI'Last) = '/' then
         return True;
      else
         return False;
      end if;
   end Is_Folder;

end Callbacks;

