with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Calendar;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;

with AWS.MIME;
with AWS.Utils;
with AWS.Digest;
with AWS.Config;
with AWS.Messages;
with AWS.Net;
with AWS.Parameters;
with AWS.Resources;
with AWS.Response.Set;
with AWS.Services.Directory;
with AWS.Translator;
with AWS.URL;

with GNAT.OS_Lib;
with GNAT.Directory_Operations;

with Wiki.Utils;
with Wiki.Parser;
with Wiki.Sidebar;
with Wiki.Ada_Format;
with Wiki.HTML_Output;
with Wiki.Special_Formats;

with Users;

package body Callbacks is

   use AWS;
   use AWS.Utils;

   use type Ada.Calendar.Time;
   package S renames Wiki.Special_Formats;

   package ASU renames Ada.Strings.Unbounded;

   function "+" (Item : in ASU.Unbounded_String) return String
     renames ASU.To_String;

   function "+" (Item : in String) return ASU.Unbounded_String
     renames ASU.To_Unbounded_String;

   function Get_WWW_Root (Request : in AWS.Status.Data) return String;

   function Get_File_Name (URI : in String) return String;
   pragma Inline (Get_File_Name);

   function Is_Folder (URI : in String) return Boolean;

   procedure Load_Redirector;

   procedure Write_File
     (File_Name : in String; Data : in Ada.Streams.Stream_Element_Array);

   function Read_File (Name : String) return String
     renames Wiki.Utils.Read_File;

   function Has_Write_Access (File_Name : String) return Boolean;

   procedure Post_Password (Request : in AWS.Status.Data);

   function Expand_Wiki
     (Text : in String;
      Args : in S.Argument_List := S.Null_Arguments) return String;

   procedure Expand_ARM
     (Prefix : in String;
      Text   : in String;
      Time   : in out Ada.Calendar.Time;
      Result :    out ASU.Unbounded_String);

   function Edit_Wiki (URI, Text, Root : in String) return AWS.Response.Data;
   procedure Read_Wiki_Prefix (Root : String);
   function Post_Wiki (Request : in AWS.Status.Data) return AWS.Response.Data;
   function To_Arguments (Request : in AWS.Status.Data) return S.Argument_List;

   Sidebar_File : constant String := "/wiki/layout.wiki";

   Wiki_Root   : ASU.Unbounded_String;
   Wiki_Prefix : ASU.Unbounded_String;
   Wiki_Suffix : ASU.Unbounded_String;
   Wiki_Prefix_Time : Ada.Calendar.Time;

   Redirect_SIP  : ASU.Unbounded_String;
   Redirect_URL  : ASU.Unbounded_String;
   Redirect_Size : Utils.File_Size_Type := Utils.File_Size_Type'Last;

   ---------------
   -- Edit_Wiki --
   ---------------

   function Edit_Wiki (URI, Text, Root : in String) return AWS.Response.Data is
      use Ada.Strings.Unbounded;

      Preview_Start : constant String := "<div class='wiki.preview'>";
      Preview_End   : constant String := "</div'>";

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

      Content : constant String := +Wiki_Prefix
        & Sidebar
        & Preview_Start
        & Expand_Wiki (Text)
        & Preview_End
        & Edit_Start
        & Text
        & Edit_End
        & (+Wiki_Suffix);

      Result : constant AWS.Response.Data
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
      use AWS.Resources;
      use Ada.Strings.Unbounded;
      URI     : constant String :=
        Wiki.Parser.Replace (Status.URI (Request), "/edit_wiki/", "/");
      Root    : constant String := Get_WWW_Root (Request);
      File    : constant String := Root & URI & ".wiki";
   begin
      Read_Wiki_Prefix (Root);

      if Exist (File) = Plain then
         return Edit_Wiki (URI, Read_File (File), Root);
      else
         declare
            use AWS.Parameters;
            List : constant AWS.Parameters.List := Status.Parameters (Request);
            Init : constant String := Get (List, "init");
            File : constant String := Root & Init & ".wiki";
         begin
            if Exist (File) = Plain then
               return Edit_Wiki (URI, Read_File (File), Root);
            else
               return Edit_Wiki (URI, "", Root);
            end if;
         end;
      end if;
   end Edit_Wiki;

   ----------------
   -- Expand_ARM --
   ----------------

   procedure Expand_ARM
     (Prefix : in String;
      Text   : in String;
      Time   : in out Ada.Calendar.Time;
      Result :    out ASU.Unbounded_String)
   is
      use Ada.Strings;
      use AWS.Resources;
      use type ASU.Unbounded_String;

      procedure Start_Element
        (Info : in Wiki.Element_Info; Data : in out Wiki.HTML_Output.Context);
      procedure End_Element
        (Info : in Wiki.Element_Info; Data : in out Wiki.HTML_Output.Context);

      procedure End_Element
        (Info : in Wiki.Element_Info; Data : in out Wiki.HTML_Output.Context)
      is
         use type Wiki.Element_Kinds;
      begin
         if Info.Kind /= Wiki.Paragraph then
            Wiki.HTML_Output.End_Element (Info, Data);
         end if;
      end End_Element;

      procedure Start_Element
        (Info : in Wiki.Element_Info; Data : in out Wiki.HTML_Output.Context)
      is
         use type Wiki.Element_Kinds;
      begin
         if Info.Kind /= Wiki.Paragraph then
            Wiki.HTML_Output.Start_Element (Info, Data);
         end if;
      end Start_Element;

      procedure Parse is new Wiki.Parser.Parse
        (Context       => Wiki.HTML_Output.Context,
         Start_Element => Start_Element,
         End_Element   => End_Element,
         Characters    => Wiki.HTML_Output.Characters);

      Load_Token : constant String := "@LOAD(";
      From       : Positive := Text'First;
      To         : Positive;
      Load       : Natural := Fixed.Index (Text, Load_Token);
   begin
      Result := ASU.Null_Unbounded_String;

      while Load /= 0 loop
         Result := Result & Text (From .. Load - 1);

         To   := Fixed.Index (Text (Load .. Text'Last), ")");

         declare
            Context   : Wiki.HTML_Output.Context;

            Wiki_File : constant String
              := Prefix & Text (Load + Load_Token'Length .. To - 1) & ".wiki";
         begin
            if Exist (Wiki_File) = Plain then
               Wiki.HTML_Output.Initialize (Context, "");
               Parse (ASCII.LF & Read_File (Wiki_File), Context);

               Result := Result & Wiki.HTML_Output.Get_Text (Context);

               if File_Timestamp (Wiki_File) > Time then
                  Time := File_Timestamp (Wiki_File);
               end if;
            end if;
         end;

         From := To + 1;

         Load := Fixed.Index (Text (From .. Text'Last), Load_Token);
      end loop;

      Result := Result & Text (From .. Text'Last);
   end Expand_ARM;

   -----------------
   -- Expand_Wiki --
   -----------------

   function Expand_Wiki
     (Text : String;
      Args : S.Argument_List := S.Null_Arguments) return String
   is
      procedure Parse is new Wiki.Parser.Parse
        (Context       => Wiki.HTML_Output.Context,
         Start_Element => Wiki.HTML_Output.Start_Element,
         End_Element   => Wiki.HTML_Output.End_Element,
         Characters    => Wiki.HTML_Output.Characters);

      Data      : Wiki.HTML_Output.Context;
   begin
      Wiki.HTML_Output.Initialize (Data, "/", Args);
      Parse (ASCII.LF & Text, Data);

      return Wiki.HTML_Output.Get_Text (Data);
   end Expand_Wiki;

   -------------
   -- Get_ARM --
   -------------

   function Get_ARM (Request : in AWS.Status.Data) return AWS.Response.Data is
      use AWS.Status;
      use AWS.Messages;

      Root    : constant String := Get_WWW_Root (Request);
      URI     : constant String := Status.URI (Request);
      File    : constant String := Root & Get_File_Name (URI);
      Text    : constant String := Read_File (File);
      Since   : constant String := If_Modified_Since (Request);
      Time    : Ada.Calendar.Time
        := AWS.Resources.File_Timestamp (File);

      Full_Text : ASU.Unbounded_String;
   begin
      Expand_ARM (Root & "/arm/", Text, Time, Full_Text);

      if Is_Valid_HTTP_Date (Since) and then Time <= To_Time (Since) then
         return AWS.Response.Build (Content_Type => AWS.MIME.Text_HTML,
                                    Message_Body => "",
                                    Status_Code  => Messages.S304);
      end if;

      declare
         Result  : AWS.Response.Data
           := AWS.Response.Build (AWS.MIME.Text_HTML, Full_Text);
      begin
         AWS.Response.Set.Add_Header
           (Result,
            AWS.Messages.Last_Modified_Token,
            AWS.Messages.To_HTTP_Date (Time));

         return Result;
      end;
   end Get_ARM;

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
      Side_Tm : constant Ada.Calendar.Time
        := AWS.Resources.File_Timestamp (Root & Sidebar_File);

      function Changed return Boolean;

      function Changed return Boolean is
         use AWS.Status;
         use AWS.Messages;

         Since : constant String := If_Modified_Since (Request);
      begin
         if not Is_Valid_HTTP_Date (Since) or else
           Time > To_Time (Since) or else
           Side_Tm > To_Time (Since)
         then
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
         Sidebar  : constant String :=
           Wiki.Sidebar.Expand (URI, Root & Sidebar_File, "/");
         Text     : constant String :=
           +Wiki_Prefix
           & Sidebar
           & Expand_Wiki (Read_File (File), To_Arguments (Request))
           & (+Wiki_Suffix);
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
   -- Get_Wiki_Or_HTML --
   ----------------------

   function Get_Wiki_Or_HTML
     (Request : in AWS.Status.Data) return AWS.Response.Data
   is
      Root : constant String := Get_WWW_Root (Request);
      URI  : constant String := Status.URI (Request);
      File : constant String := Root & Get_File_Name (URI);
      Wiki : constant String := File & ".wiki";

      function Get_File return AWS.Response.Data;

      --------------
      -- Get_File --
      --------------

      function Get_File return AWS.Response.Data is
         use type AWS.Utils.File_Size_Type;
         Content_Type : constant String := MIME.Content_Type (File);
      begin
         if Net.Get_Addr (Status.Socket (Request).all) = +Redirect_SIP
           and then Content_Type /= "text/html"
           and then Utils.File_Size (File) > Redirect_Size
         then
            return AWS.Response.URL (+Redirect_URL & URI);
         end if;

         return Response.File (Content_Type => Content_Type, Filename => File);
      end Get_File;

   begin
      if Utils.Is_Regular_File (Wiki)
        and then (not Resources.Is_Regular_File (File)
                  or else Resources.File_Timestamp (File) <
                          Utils.File_Time_Stamp (Wiki))
      then
         return Get_Wiki (Request);

      elsif Resources.Is_Regular_File (File) then
         return Get_File;

      elsif Utils.Is_Directory (File) then
         declare
            Directory_Browser_Page : constant String
              := Config.Directory_Browser_Page (Config.Get_Current);
         begin
            if Net.Get_Addr (Status.Socket (Request).all) = +Redirect_SIP then
               return AWS.Response.URL
                 (+Redirect_URL
                  & URL.Pathname_And_Parameters (Status.URI (Request)));
            else
               return Response.Build
                 (Content_Type => MIME.Text_HTML,
                  Message_Body =>
                  Services.Directory.Browse
                    (File, Directory_Browser_Page, Request));
            end if;
         end;
      else
         return Response.Acknowledge
           (Messages.S404,
            "<p>Page '" & URI & "' Not found.");
      end if;
   end Get_Wiki_Or_HTML;

   ------------------
   -- Get_WWW_Root --
   ------------------

   function Get_WWW_Root (Request : in AWS.Status.Data) return String is
      use Ada.Strings;
      Host : constant String := Status.Host (Request);
   begin
      if AWS.Utils.Is_Directory (Host)
        and then Fixed.Index (Host, Maps.To_Set ("/\")) = 0
      then
         return Host;
      else
         return Config.WWW_Root (Config.Get_Current);
      end if;
   end Get_WWW_Root;

   ----------------------
   -- Has_Write_Access --
   ----------------------

   function Has_Write_Access (File_Name : String) return Boolean is
   begin
      return not Utils.Is_Regular_File (File_Name & ".ro");
   end Has_Write_Access;

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

   ---------------------
   -- Load_Redirector --
   ---------------------

   procedure Load_Redirector is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Open (File, In_File, "redirector.ini", Form => "shared=no");
      Redirect_SIP  := +Get_Line (File);
      Redirect_Size := Utils.File_Size_Type'Value (Get_Line (File));
      Redirect_URL  := +Get_Line (File);
      Close (File);
   exception
      when Name_Error => null;
      when others =>
         Close (File);
         raise;
   end Load_Redirector;

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

   function Post_Wiki
     (Request : in AWS.Status.Data) return AWS.Response.Data
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
      then
         return Response.Authenticate
                  ("Ada_Ru private", Response.Digest);
      elsif Stale then
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
         Wiki_Root   := +Root;
         Wiki_Prefix := +Read_File (Name);
         Wiki_Suffix := +Read_File (Root & "/wiki.suffix");
         Wiki_Prefix_Time := File_Timestamp (Name);
      end if;
   end Read_Wiki_Prefix;

   ------------------
   -- To_Arguments --
   ------------------

   function To_Arguments
     (Request : in AWS.Status.Data)
     return S.Argument_List
   is
      use AWS.Parameters;
      use Ada.Strings.Unbounded;
      List   : constant AWS.Parameters.List := Status.Parameters (Request);
      Length : constant Natural := Count (List);
      Result : S.Argument_List (Length);
   begin
      for J in 1 .. Length loop
         Result.Names (J)  := +Get_Name  (List, J);
         Result.Values (J) := +Get_Value (List, J);
      end loop;

      return Result;
   end To_Arguments;

   ----------------
   -- Write_File --
   ----------------

   procedure Write_File
     (File_Name : in String;
      Data      : in Ada.Streams.Stream_Element_Array)
   is
      use Ada.Streams.Stream_IO;

      function Versioned_Name (Name : String; Ver : Positive) return String;

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

begin
   S.Register ("ada", Wiki.Ada_Format'Access);
   Load_Redirector;
end Callbacks;

