with Ada.Characters.Wide_Wide_Latin_1;

with League.String_Vectors;
with League.Text_Codecs;
with League.Stream_Element_Vectors;
with League.Base_Codecs;

with AWS.SMTP.Client;

with Sessions;

package body Servlet.Forum is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

   function String_Carriage_Return
    (Text : League.Strings.Universal_String)
     return League.Strings.Universal_String;

   function Escape
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function Escape_Header
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   procedure Wrap_Lines (Text : in out League.Strings.Universal_String);

   -------------
   -- Do_Post --
   -------------

   overriding procedure Do_Post
     (Self     : in out Forum_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      pragma Unreferenced (Self);
      use type League.Strings.Universal_String;

      Session : constant Sessions.HTTP_Session_Access :=
        Sessions.HTTP_Session_Access (Request.Get_Session);

      Info   : constant Sessions.User_Info := Session.Get_User_Info;
      Status : AWS.SMTP.Status;
      From   : AWS.SMTP.E_Mail_Data;
      Header : League.String_Vectors.Universal_String_Vector;

      Topic : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"topic");

      Subject : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"subject");

      Text : League.Strings.Universal_String :=
        Escape (String_Carriage_Return (Request.Get_Parameter (+"text")));

      Receiver : constant AWS.SMTP.Receiver :=
        AWS.SMTP.Initialize ("forge.ada-ru.org");

      To : constant AWS.SMTP.E_Mail_Data :=
        AWS.SMTP.E_Mail ("", "ada_ru@yahoogroups.com");
   begin
      Header.Append (+"To: ada_ru@yahoogroups.com");

      if Info.User.Is_Empty then
         Response.Set_Status (Servlet.HTTP_Responses.Unauthorized);
         return;
      end if;

      if not Topic.Is_Empty then
         Header.Append ("In-Reply-To: " & Topic);
      end if;

      Header.Append
        ("X-Forum-User: " &
           Escape_Header (Info.Name) & " <" & Info.Mails (1) & ">");

      Header.Append
        ("From: " & Escape_Header (Info.Name) & " <ada_ru@forge.ada-ru.org>");

      From := AWS.SMTP.E_Mail
        (Escape_Header (Info.Name).To_UTF_8_String,
         "ada_ru@forge.ada-ru.org");

      Header.Append ("Subject: " & Escape_Header (Subject));
      Header.Append (+"Content-Type: text/plain; charset=utf-8");
      Header.Append (+"Content-Transfer-Encoding: base64");
      Header.Append (League.Strings.Empty_Universal_String);
      Header.Append (League.Strings.Empty_Universal_String);

      Wrap_Lines (Text);

      Text.Prepend (Header.Join (Ada.Characters.Wide_Wide_Latin_1.LF));

      AWS.SMTP.Client.Send
        (Server  => Receiver,
         From    => From,
         To      => (1 => To),
         Source  => Text.To_UTF_8_String,
         Status  => Status);

      if AWS.SMTP.Is_Ok (Status) then
         Response.Set_Status (Servlet.HTTP_Responses.OK);
         Response.Set_Content_Type (+"text/plain");
         Response.Set_Character_Encoding (+"utf-8");
         Response.Get_Output_Stream.Write (+"Message was sent");
      else
         Response.Set_Status (Servlet.HTTP_Responses.Bad_Gateway);
         Response.Set_Content_Type (+"text/plain");
         Response.Set_Character_Encoding (+"utf-8");

         Response.Get_Output_Stream.Write
           (League.Strings.From_UTF_8_String
              (AWS.SMTP.Status_Message (Status)));
      end if;
   end Do_Post;

   ------------
   -- Escape --
   ------------

   function Escape
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      UTF_8 : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec (+"utf-8");
      Raw   : constant League.Stream_Element_Vectors.Stream_Element_Vector :=
        UTF_8.Encode (Text);
      Base_64 : constant League.Strings.Universal_String :=
        League.Base_Codecs.To_Base_64 (Raw);
   begin
      return Base_64;
   end Escape;

   -------------------
   -- Escape_Header --
   -------------------

   function Escape_Header (Text : League.Strings.Universal_String)
     return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;
   begin
      return "=?utf-8?b?" & Escape (Text) & "?=";
   end Escape_Header;

   ----------------------
   -- Get_Servlet_Info --
   ----------------------

   overriding function Get_Servlet_Info (Self : Forum_Servlet)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return +"Servlet to post on forum";
   end Get_Servlet_Info;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
      return Forum_Servlet
   is
      pragma Unreferenced (Parameters);
   begin
      return Result : Forum_Servlet;
   end Instantiate;

   ----------------------------
   -- String_Carriage_Return --
   ----------------------------

   function String_Carriage_Return
    (Text : League.Strings.Universal_String)
     return League.Strings.Universal_String
   is
      List : constant League.String_Vectors.Universal_String_Vector :=
        Text.Split (Ada.Characters.Wide_Wide_Latin_1.CR);
   begin
      return List.Join ("");
   end String_Carriage_Return;

   ----------------
   -- Wrap_Lines --
   ----------------

   procedure Wrap_Lines (Text : in out League.Strings.Universal_String) is
      Result   : League.String_Vectors.Universal_String_Vector;
      Max_Line : constant := 76;
      First    : Positive;
      Last     : Natural;
   begin
      for J in 1 .. (Text.Length + Max_Line - 1) / Max_Line loop
         First := (J - 1) * Max_Line + 1;
         Last := Natural'Min (First + Max_Line - 1, Text.Length);
         Result.Append (Text.Slice (First, Last));
      end loop;

      Text := Result.Join (Ada.Characters.Wide_Wide_Latin_1.LF);
   end Wrap_Lines;

end Servlet.Forum;
