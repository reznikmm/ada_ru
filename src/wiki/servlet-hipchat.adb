with Ada.Streams.Stream_IO;
with Ada.Wide_Wide_Text_IO;
with Ada.Text_IO;

with AWS.Client;
with AWS.Messages;
with AWS.Parameters;
with AWS.Response;

with League.IRIs;
with League.JSON.Documents;
with League.JSON.Objects;
with League.JSON.Values;
with League.Stream_Element_Vectors;
with League.Text_Codecs;
with League.Calendars.From_Seconds;

package body Servlet.Hipchat is

   use type League.Strings.Universal_String;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

   procedure To_JSON
     (Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Document : out League.JSON.Documents.JSON_Document);

   File_Name : constant String := "password/hipchat.bin";

   procedure On_Install
     (Self     : in out Hipchat_Servlet'Class;
      OAuth_Id : League.Strings.Universal_String);

   procedure Write_Installation_Map (Self  : Hipchat_Servlet'Class);

   procedure Refresh_Access_Token
     (Self     : in out Hipchat_Servlet'Class;
      OAuth_Id : League.Strings.Universal_String);

   ------------
   -- Do_Get --
   ------------

   overriding procedure Do_Get
    (Self     : in out Hipchat_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      procedure Uninstall (URL : League.Strings.Universal_String);

      procedure Uninstall (URL : League.Strings.Universal_String) is
         Data : constant AWS.Response.Data :=
           AWS.Client.Get (URL.To_UTF_8_String);
      begin
         if AWS.Response.Status_Code (Data) in AWS.Messages.Success then
            Ada.Text_IO.Put_Line ("GET:" & AWS.Response.Message_Body (Data));
            declare
               Document : constant League.JSON.Documents.JSON_Document :=
                 League.JSON.Documents.From_JSON
                   (AWS.Response.Message_Body (Data));
               Object : constant League.JSON.Objects.JSON_Object :=
                 Document.To_JSON_Object;
               OAuth_Id : constant League.Strings.Universal_String :=
                 Object.Value (+"oauthId").To_String;
            begin
               Self.Installations.Delete (OAuth_Id);
               Self.Capabilities.Delete (OAuth_Id);
               Write_Installation_Map (Self);
            end;
         end if;
      end Uninstall;

      Path    : constant League.Strings.Universal_String :=
        Request.Get_Path_Info;
   begin
      if Path = +"/uninstalled" then
         declare
            Redirect : constant League.Strings.Universal_String :=
              Request.Get_Parameter (+"redirect_url");
            Installable : constant League.Strings.Universal_String :=
              Request.Get_Parameter (+"installable_url");

            Location : League.IRIs.IRI;
         begin
            Ada.Wide_Wide_Text_IO.Put_Line
              ("/uninstalled: " & Redirect.To_Wide_Wide_String
                 & ", " & Installable.To_Wide_Wide_String);
            Uninstall (Installable);
            Location.Set_IRI (Redirect);
            Response.Send_Redirect (Location);
         end;
      else
         Response.Set_Status (Servlet.HTTP_Responses.Not_Found);
      end if;
   end Do_Get;

   -------------
   -- Do_Post --
   -------------

   overriding procedure Do_Post
     (Self     : in out Hipchat_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      Path    : constant League.Strings.Universal_String :=
        Request.Get_Path_Info;
   begin
      if Path = +"/installed" then
         declare
            Document : League.JSON.Documents.JSON_Document;
            Object   : League.JSON.Objects.JSON_Object;
            Value    : Installation;
         begin
            To_JSON (Request, Document);
            Object := Document.To_JSON_Object;
            Value.OAuth_Id := Object.Value (+"oauthId").To_String;
            Value.Capabilities_URL :=
              Object.Value (+"capabilitiesUrl").To_String;
            Value.OAuth_Secret := Object.Value (+"oauthSecret").To_String;
            Value.Room_Id := Natural (Object.Value (+"roomId").To_Integer);
            Value.Group_Id := Natural (Object.Value (+"groupId").To_Integer);
            Self.Installations.Include (Value.OAuth_Id, Value);
            On_Install (Self, Value.OAuth_Id);
            Write_Installation_Map (Self);
            Response.Set_Status (Servlet.HTTP_Responses.No_Content);
         end;
      elsif Path = +"/webhook" then
         declare
            Document : League.JSON.Documents.JSON_Document;
            Object   : League.JSON.Objects.JSON_Object;
         begin
            To_JSON (Request, Document);
            Object := Document.To_JSON_Object;
            Self.Listener.On_Hipchat (Object);
            Response.Set_Status (Servlet.HTTP_Responses.No_Content);
         end;
      end if;
   end Do_Post;

   ----------------------
   -- Get_Servlet_Info --
   ----------------------

   overriding function Get_Servlet_Info
     (Self : Hipchat_Servlet)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return +"HipChat Servlet";
   end Get_Servlet_Info;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self  : in out Hipchat_Servlet'Class) is
   begin
      null;
   end Initialize;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
      return Hipchat_Servlet
   is
      pragma Unreferenced (Parameters);
   begin
      return Result : Hipchat_Servlet do
         Initialize (Result);
      end return;
   end Instantiate;

   ----------------
   -- On_Install --
   ----------------

   procedure On_Install
     (Self     : in out Hipchat_Servlet'Class;
      OAuth_Id : League.Strings.Universal_String)
   is
      URL : constant League.Strings.Universal_String :=
        Self.Installations (OAuth_Id).Capabilities_URL;
      Response : constant AWS.Response.Data := AWS.Client.Get
        (URL => URL.To_UTF_8_String);
   begin
      if AWS.Response.Status_Code (Response) in AWS.Messages.Success then
         declare
            Document : constant League.JSON.Documents.JSON_Document :=
              League.JSON.Documents.From_JSON
                (AWS.Response.Message_Body (Response));
            Object : constant League.JSON.Objects.JSON_Object :=
              Document.To_JSON_Object;
            Capabilities : constant League.JSON.Objects.JSON_Object :=
              Object.Value (+"capabilities").To_Object;
            OAuth_Provider : constant League.JSON.Objects.JSON_Object :=
              Capabilities.Value (+"oauth2Provider").To_Object;
            API_Provider : constant League.JSON.Objects.JSON_Object :=
              Capabilities.Value (+"hipchatApiProvider").To_Object;
            Value    : Capability;
         begin
            Value.Token_URL := OAuth_Provider.Value (+"tokenUrl").To_String;
            Value.API_URL := API_Provider.Value (+"url").To_String;
            Self.Capabilities.Include (OAuth_Id, Value);
            Refresh_Access_Token (Self, OAuth_Id);
         end;
      end if;
   end On_Install;

   --------------------------
   -- Refresh_Access_Token --
   --------------------------

   procedure Refresh_Access_Token
     (Self     : in out Hipchat_Servlet'Class;
      OAuth_Id : League.Strings.Universal_String)
   is

      function Image (Value : Natural) return Wide_Wide_String;

      -----------
      -- Image --
      -----------

      function Image (Value : Natural) return Wide_Wide_String is
         Result : constant Wide_Wide_String := Natural'Wide_Wide_Image (Value);
      begin
         return Result (Result'First + 1 .. Result'Last);
      end Image;

      URL : constant League.Strings.Universal_String :=
        Self.Capabilities (OAuth_Id).Token_URL;
      Password : constant League.Strings.Universal_String :=
        Self.Installations (OAuth_Id).OAuth_Secret;
      Parameters : AWS.Parameters.List;
   begin
      Parameters.Add ("grant_type", "client_credentials");

      declare
         Full : constant String := AWS.Parameters.URI_Format (Parameters);
         Data : AWS.Response.Data;
      begin
         Data := AWS.Client.Post
           (URL          => URL.To_UTF_8_String,
            Data         => Full (Full'First + 1 .. Full'Last),
            Content_Type => "application/x-www-form-urlencoded",
            User         => OAuth_Id.To_UTF_8_String,
            Pwd          => Password.To_UTF_8_String);

         if AWS.Response.Status_Code (Data) in AWS.Messages.Success then
            declare
               use type League.Calendars.Date_Time;
               Document : constant League.JSON.Documents.JSON_Document :=
                 League.JSON.Documents.From_JSON
                   (AWS.Response.Message_Body (Data));
               Object : constant League.JSON.Objects.JSON_Object :=
                 Document.To_JSON_Object;
               Token : constant League.Strings.Universal_String :=
                 Object.Value (+"access_token").To_String;
               Expires : constant Positive :=
                 Positive (Object.Value (+"expires_in").To_Integer) - 60;
               Value    : Access_Token;
            begin
               Value.Token := Token;
               Value.Expires := League.Calendars.Clock +
                 League.Calendars.From_Seconds (Expires);
               Self.Access_Tokens.Include (OAuth_Id, Value);
               Self.Listener.On_Hipchat_Token
                 (Id    => OAuth_Id,
                  URL   => Self.Capabilities (OAuth_Id).API_URL
                             & "room/"
                             & Image (Self.Installations (OAuth_Id).Room_Id)
                             & "/notification",
                  Token => Token);
            end;
         end if;
      end;
   end Refresh_Access_Token;

   ------------------
   -- Set_Listener --
   ------------------

   not overriding procedure Set_Listener
     (Self  : in out Hipchat_Servlet;
      Value : access Axe.Events.Listener'Class)
   is
      Input : Ada.Streams.Stream_IO.File_Type;
   begin
      Self.Listener := Value;

      Ada.Streams.Stream_IO.Open
        (Input,
         Ada.Streams.Stream_IO.In_File,
         File_Name,
         "SHARED=NO");

      if not Ada.Streams.Stream_IO.End_Of_File (Input) then
         Installation_Maps.Map'Read
           (Ada.Streams.Stream_IO.Stream (Input), Self.Installations);

         for Value of Self.Installations loop
            On_Install (Self, Value.OAuth_Id);
         end loop;
      end if;

      Ada.Streams.Stream_IO.Close (Input);
   end Set_Listener;

   -------------
   -- To_JSON --
   -------------

   procedure To_JSON
     (Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Document : out League.JSON.Documents.JSON_Document)
   is
      use type Ada.Streams.Stream_Element_Count;
      Stream : constant access Ada.Streams.Root_Stream_Type'Class :=
        Request.Get_Input_Stream;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 512);
      Vector : League.Stream_Element_Vectors.Stream_Element_Vector;
      Last   : Ada.Streams.Stream_Element_Count;
   begin
      loop
         Stream.Read (Buffer, Last);
         exit when Last = 0;
         Vector.Append (Buffer (1 .. Last));
      end loop;

      Ada.Wide_Wide_Text_IO.Put_Line
        (League.Text_Codecs.Codec_For_Application_Locale.
           Decode (Vector).To_Wide_Wide_String);

      Document := League.JSON.Documents.From_JSON (Vector);
   end To_JSON;

   ---------------
   -- Write_Map --
   ---------------

   procedure Write_Installation_Map (Self  : Hipchat_Servlet'Class) is
      Output : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Open
        (Output,
         Ada.Streams.Stream_IO.Out_File,
         File_Name,
         "SHARED=NO");

      Installation_Maps.Map'Write
        (Ada.Streams.Stream_IO.Stream (Output), Self.Installations);

      Ada.Streams.Stream_IO.Close (Output);
   end Write_Installation_Map;

end Servlet.Hipchat;
