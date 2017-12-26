with AWS.Client;
with AWS.Messages;
with AWS.Parameters;
with AWS.Response;

with League.Base_Codecs;
with League.Holders;
with League.JSON.Documents;
with League.JSON.Objects;
with League.JSON.Values;
with League.Settings;
with League.Stream_Element_Vectors;
with League.String_Vectors;
with Servlet.Contexts;
with Servlet.HTTP_Sessions;
with XML.SAX.Input_Sources.Streams.Files;
with XML.SAX.Output_Destinations.Strings;
with XML.SAX.Pretty_Writers;
with XML.SAX.Simple_Readers;
with XML.Templates.Processors;

package body Servlet.OAuth is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

   procedure Get_Token
     (Self  : OAuth_Provider;
      Code  : League.Strings.Universal_String;
      Token : out League.Strings.Universal_String);

   procedure Decode_Facebook_Token
     (Token : League.Strings.Universal_String;
      Info  : out User_Info);

   procedure Decode_Google_Token
     (Token : League.Strings.Universal_String;
      Info  : out User_Info);

   ---------------
   -- Check_Key --
   ---------------

   not overriding function Check_Key
     (Self       : in out State_Cache;
      Session_Id : League.Strings.Universal_String;
      Key        : League.Strings.Universal_String)
      return Boolean
   is
      use type League.Strings.Universal_String;
   begin
      return Self.Map.Contains (Session_Id) and then
        Self.Map (Session_Id) = Key;
   end Check_Key;

   ----------------
   -- Create_Key --
   ----------------

   not overriding function Create_Key
     (Self       : in out State_Cache;
      Session_Id : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      use type Ada.Containers.Count_Type;
      Max_States : constant := 100;
      Result : League.Strings.Universal_String;
      Cursor : String_Lists.Cursor;
      Data   : League.Stream_Element_Vectors.Stream_Element_Vector;
   begin
      if Self.Map.Contains (Session_Id) then
         Cursor := Self.Queue.Find (Session_Id);
         Self.Queue.Delete (Cursor);
         Self.Queue.Append (Session_Id);
         return Self.Map (Session_Id);
      end if;

      for J in 1 .. 12 loop
         Data.Append (Stream_Element_Random.Random (Self.Random));
      end loop;

      Result := League.Base_Codecs.To_Base_64 (Data);

      Self.Map.Insert (Session_Id, Result);
      Self.Queue.Append (Session_Id);

      if Self.Map.Length > Max_States then
         Self.Map.Delete (Self.Queue.First_Element);
         Self.Queue.Delete_First;
      end if;

      return Result;
   end Create_Key;

   ---------------------------
   -- Decode_Facebook_Token --
   ---------------------------

   procedure Decode_Facebook_Token
     (Token : League.Strings.Universal_String;
      Info  : out User_Info)
   is
      Parameters : AWS.Parameters.List;
      Document   : League.JSON.Documents.JSON_Document;
      Object     : League.JSON.Objects.JSON_Object;
   begin
      Parameters.Add ("access_token", Token.To_UTF_8_String);
      Parameters.Add ("fields", "id,name,email,picture");

      declare
         Full : constant String := AWS.Parameters.URI_Format (Parameters);
         Data : AWS.Response.Data;
      begin
         Data := AWS.Client.Post
           (URL          => "https://graph.facebook.com/v2.11/me",
            Data         => Full (Full'First + 1 .. Full'Last),
            Content_Type => "application/x-www-form-urlencoded");

         if AWS.Response.Status_Code (Data) in AWS.Messages.S200 then
            Document := League.JSON.Documents.From_JSON
              (AWS.Response.Message_Body (Data));
            Object := Document.To_JSON_Object;
            Info (User) := Object.Value (+"id").To_String;
            Info (Name) := Object.Value (+"name").To_String;
            Info (Mail) := Object.Value (+"email").To_String;
            Object := Object.Value (+"picture").To_Object;
            Object := Object.Value (+"data").To_Object;
            Info (Avatar) := Object.Value (+"url").To_String;
         end if;
      end;
   end Decode_Facebook_Token;

   -------------------------
   -- Decode_Google_Token --
   -------------------------

   procedure Decode_Google_Token
     (Token : League.Strings.Universal_String;
      Info  : out User_Info)
   is
      Parts      : League.String_Vectors.Universal_String_Vector;
      Encoded    : League.Strings.Universal_String;
      Claim_Set  : League.JSON.Documents.JSON_Document;
      Object     : League.JSON.Objects.JSON_Object;
   begin
      Parts := Token.Split ('.');
      Encoded := Parts.Element (2);

      while Encoded.Length mod 4 /= 0 loop
         Encoded.Append ('=');
      end loop;

      Claim_Set := League.JSON.Documents.From_JSON
        (League.Base_Codecs.From_Base_64_URL (Encoded));
      Object := Claim_Set.To_JSON_Object;
      Info (User) := Object.Value (+"sub").To_String;
      Info (Mail) := Object.Value (+"email").To_String;
      Info (Name)  := Object.Value (+"name").To_String;
      Info (Avatar)  := Object.Value (+"picture").To_String;
   end Decode_Google_Token;

   ------------
   -- Do_Get --
   ------------

   overriding procedure Do_Get
     (Self     : in out OAuth_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      function Login_Page return League.Strings.Universal_String;

      Context   : constant access Servlet.Contexts.Servlet_Context'Class
        := Request.Get_Servlet_Context;

      Session : constant League.Strings.Universal_String :=
        Request.Get_Session.Get_Id;

      Path    : constant League.Strings.Universal_String :=
        Request.Get_Path_Info;

      function Login_Page return League.Strings.Universal_String is
         Key       : constant League.Strings.Universal_String :=
           Self.Cache.Create_Key (Session);
         XHTML     : constant League.Strings.Universal_String :=
           +"/login.xhtml.tmpl";
         Input     : aliased XML.SAX.Input_Sources.Streams.Files
           .File_Input_Source;
         Reader    : aliased XML.SAX.Simple_Readers.Simple_Reader;
         Filter    : aliased XML.Templates.Processors.Template_Processor;
         Writer    : aliased XML.SAX.Pretty_Writers.XML_Pretty_Writer;
         Output    : aliased XML.SAX.Output_Destinations.Strings
           .String_Output_Destination;
      begin
         --  Set template input
         Input.Open_By_File_Name (Context.Get_Real_Path (XHTML));

         --  Configure reader
         Reader.Set_Input_Source (Input'Unchecked_Access);
         Reader.Set_Content_Handler (Filter'Unchecked_Access);
         Reader.Set_Lexical_Handler (Filter'Unchecked_Access);

         --  Configure template processor
         Filter.Set_Content_Handler (Writer'Unchecked_Access);
         Filter.Set_Lexical_Handler (Writer'Unchecked_Access);

         --  Bind wiki page content
         Filter.Set_Parameter (+"state", League.Holders.To_Holder (Key));

         for P in Self.OAuth_Providers.Iterate loop
            Filter.Set_Parameter
              (OAuth_Provider_Maps.Key (P),
               League.Holders.To_Holder (Self.OAuth_Providers (P).Client_Id));
         end loop;

         --  Configure XML writer.

         Writer.Set_Output_Destination (Output'Unchecked_Access);
         --  Process template
         Reader.Parse;

         return Output.Get_Text;
      end Login_Page;

      use type League.Strings.Universal_String;

      Token : League.Strings.Universal_String;
      Info  : User_Info;
   begin
      if Self.OAuth_Providers.Contains (Path.Tail_From (2)) then
         if Self.Cache.Check_Key
           (Session, Request.Get_Parameter (+"state"))
         then
            Get_Token
              (Self.OAuth_Providers (Path.Tail_From (2)),
               Request.Get_Parameter (+"code"),
               Token);

            if Path = +"/google" then
               Decode_Google_Token (Token, Info);
            elsif Path = +"/facebook" then
               Decode_Facebook_Token (Token, Info);
            end if;

            Self.Handler.Do_Login (Info, Request, Response);

            return;
         else
            Response.Set_Status (Servlet.HTTP_Responses.Unauthorized);
            return;
         end if;
      end if;

      Response.Set_Status (Servlet.HTTP_Responses.OK);
      Response.Set_Header (+"Cache-Control", +"must-revalidate");
      Response.Set_Content_Type (+"text/html");
      Response.Set_Character_Encoding (+"utf-8");

      Response.Get_Output_Stream.Write (Login_Page);
   end Do_Get;

   ----------------------
   -- Get_Servlet_Info --
   ----------------------

   overriding function Get_Servlet_Info
     (Self : OAuth_Servlet)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return +"OAuth Servlet";
   end Get_Servlet_Info;

   ---------------
   -- Get_Token --
   ---------------

   procedure Get_Token
     (Self  : OAuth_Provider;
      Code  : League.Strings.Universal_String;
      Token : out League.Strings.Universal_String)
   is
      Parameters : AWS.Parameters.List;
      Document   : League.JSON.Documents.JSON_Document;
      Object     : League.JSON.Objects.JSON_Object;
   begin
      Parameters.Add ("code", Code.To_UTF_8_String);
      Parameters.Add ("client_id", Self.Client_Id.To_UTF_8_String);
      Parameters.Add ("client_secret", Self.Client_Secret.To_UTF_8_String);
      Parameters.Add ("redirect_uri", Self.Redirect_URI.To_UTF_8_String);
      Parameters.Add ("grant_type", "authorization_code");

      declare
         Full : constant String := AWS.Parameters.URI_Format (Parameters);
         Data : AWS.Response.Data;
      begin
         Data := AWS.Client.Post
           (URL          => Self.Token_End_Point.To_UTF_8_String,
            Data         => Full (Full'First + 1 .. Full'Last),
            Content_Type => "application/x-www-form-urlencoded");

         if AWS.Response.Status_Code (Data) in AWS.Messages.S200 then
            Document := League.JSON.Documents.From_JSON
              (AWS.Response.Message_Body (Data));
            Object := Document.To_JSON_Object;
            Token := Object.Value (Self.Token_Key).To_String;
         end if;
      end;
   end Get_Token;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
      return OAuth_Servlet
   is
      pragma Unreferenced (Parameters);

      procedure Add_OAuth_Provider
        (Map      : in out OAuth_Provider_Maps.Map;
         Name     : League.Strings.Universal_String;
         Settings : League.Settings.Settings);

      ------------------------
      -- Add_OAuth_Provider --
      ------------------------

      procedure Add_OAuth_Provider
        (Map      : in out OAuth_Provider_Maps.Map;
         Name     : League.Strings.Universal_String;
         Settings : League.Settings.Settings)
      is
         use type League.Strings.Universal_String;

         Prefix    : constant League.Strings.Universal_String :=
           "/" & Name & "/";
         Client_Id : constant League.Holders.Holder :=
           Settings.Value (Prefix & "client_id");
         Token_End_Point : constant League.Holders.Holder :=
           Settings.Value (Prefix & "token_endpoint");
         Client_Secret : constant League.Holders.Holder :=
           Settings.Value (Prefix & "client_secret");
         Redirect_URI : constant League.Holders.Holder :=
           Settings.Value (Prefix & "redirect_uri");
         Token_Key : constant League.Holders.Holder :=
           Settings.Value (Prefix & "token_key");

         Item : OAuth_Provider;
      begin
         Item.Client_Id := League.Holders.Element (Client_Id);
         Item.Token_End_Point := League.Holders.Element (Token_End_Point);
         Item.Client_Secret := League.Holders.Element (Client_Secret);
         Item.Redirect_URI := League.Holders.Element (Redirect_URI);
         Item.Token_Key := League.Holders.Element (Token_Key);

         Map.Insert (Name, Item);
      end Add_OAuth_Provider;

      Settings  : League.Settings.Settings;
   begin
      return Result : OAuth_Servlet do
         Add_OAuth_Provider (Result.OAuth_Providers, +"google", Settings);
         Add_OAuth_Provider (Result.OAuth_Providers, +"facebook", Settings);
      end return;
   end Instantiate;

   -----------------
   -- Set_Handler --
   -----------------

   not overriding procedure Set_Handler
    (Self  : in out OAuth_Servlet;
     Value : access Login_Handler'Class) is
   begin
      Self.Handler := Value;
   end Set_Handler;

end Servlet.OAuth;