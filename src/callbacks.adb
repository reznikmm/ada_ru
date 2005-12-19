with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Streams.Stream_IO;

with AWS.MIME;
with AWS.Digest;
with AWS.Messages;
with AWS.OS_Lib;
with AWS.Resources;
with AWS.Services.Directory;

with GNAT.OS_Lib;
with GNAT.Directory_Operations;

with Users;

package body Callbacks is

   use AWS;

   function Get_WWW_Root (Host : in String) return String;
   pragma Inline (Get_WWW_Root);

   function Get_File_Name (URI : in String) return String;
   pragma Inline (Get_File_Name);

   function Get (Request : in Status.Data) return Response.Data;
   function Put (Request : in Status.Data) return Response.Data;

   function Is_Folder (URI : String) return Boolean;

   procedure Write_File
     (File_Name : String;
      Data      : Ada.Streams.Stream_Element_Array);

   function Get (Request : in Status.Data) return Response.Data is
      File : constant String := Get_WWW_Root (Status.Host (Request))
                              & Get_File_Name (Status.URI (Request));
   begin
      if AWS.Resources.Is_Regular_File (File) then
         return Response.File (Content_Type => MIME.Content_Type (File),
                               Filename     => File);
      elsif OS_Lib.Is_Directory (File) then
         return AWS.Response.Build
           (Content_Type => "text/html",
            Message_Body =>
              AWS.Services.Directory.Browse
              (File, "aws_directory.thtml", Request));
      end if;

      return Response.Acknowledge
        (Messages.S404,
         "File '" & File & "' not found.");

   end Get;

   ------------------
   -- Get_WWW_Root --
   ------------------

   function Get_WWW_Root (Host : in String) return String is
   begin
      if Host = "" then
         return "www";
      else
         return Host;
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


   function Put (Request : in Status.Data) return Response.Data is
      use AWS.Status;
      use AWS.Digest;

      File    : constant String := Get_WWW_Root (Status.Host (Request))
                                 & Get_File_Name (Status.URI (Request));
      Stale   : Boolean := not Check_Nonce (Authorization_Nonce (Request));

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
      end if;

      if Is_Folder (File) then
         GNAT.Directory_Operations.Make_Dir (File);
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

   --------------------
   -- Public_Service --
   --------------------

   function Public_Service (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use type Status.Request_Method;
   begin
      case Status.Method (Request) is
         when Status.GET | Status.HEAD =>
            return Get (Request);
         when Status.POST =>
            return Put (Request);
         when others =>
            return Response.Acknowledge
              (Messages.S405,
               Message_Body => "Unknown Request method");
      end case;
   end Public_Service;

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

      if Is_Regular_File (File_Name) then
         while Is_Regular_File (Versioned_Name (File_Name, Version)) loop
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

         GNAT.OS_Lib.Delete_File (Versioned_Name (File_Name, Version), Success);

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

      Ada.Exceptions.Reraise_Occurrence (E);
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

