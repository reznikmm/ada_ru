with Ada.Streams;
with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Wide_Wide_Text_IO;

with League.JSON.Arrays;
with League.JSON.Documents;
with League.JSON.Objects;
with League.JSON.Values;
with League.Stream_Element_Vectors;
with League.String_Vectors;
with League.Text_Codecs;

with Servlet.Contexts;

with Axe.Read_File;

package body Servlet.Pastebin is
   use type League.Strings.Universal_String;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

   procedure Decode_Post
     (Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Text     : out League.String_Vectors.Universal_String_Vector;
      Is_JSON  : out Boolean);

   Prefix : constant Wide_Wide_String := "/pastebin/";

   -----------------
   -- Decode_Post --
   -----------------

   procedure Decode_Post
     (Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Text     : out League.String_Vectors.Universal_String_Vector;
      Is_JSON  : out Boolean)
   is
      Content_Type : constant League.String_Vectors.Universal_String_Vector :=
        Request.Get_Headers (+"Content-Type");
   begin
      Is_JSON := False;

      if Content_Type.Length > 0 and then
        Content_Type.Element (1) = +"application/json"
      then
         declare
            Stream : constant access Ada.Streams.Root_Stream_Type'Class :=
              Request.Get_Input_Stream;
            Data   : Ada.Streams.Stream_Element_Array (1 .. 1024);
            Last   : Ada.Streams.Stream_Element_Offset := 0;
            Vector : League.Stream_Element_Vectors.Stream_Element_Vector;
            JSON   : League.JSON.Documents.JSON_Document;
            Object : League.JSON.Objects.JSON_Object;
            List   : League.JSON.Arrays.JSON_Array;
         begin
            loop
               Stream.Read (Data, Last);
               exit when Last in 0;
               Vector.Append (Data (1 .. Last));
            end loop;

            JSON := League.JSON.Documents.From_JSON (Vector);

            if JSON.Is_Object then
               Is_JSON := True;
               Object := JSON.To_JSON_Object;
               List := Object.Value (+"text").To_Array;

               for J in 1 .. List.Length loop
                  Text.Append (List.Element (J).To_String);
               end loop;
            end if;
         end;
      else
         declare
            Line  : League.Strings.Universal_String;
            Value : constant League.Strings.Universal_String :=
              Request.Get_Parameter (+"text");
            List  : constant League.String_Vectors.Universal_String_Vector :=
              Value.Split (Ada.Characters.Wide_Wide_Latin_1.LF);

         begin
            for J in 1 .. List.Length loop
               Line := List (J);

               if Line (Line.Length).To_Wide_Wide_Character =
                 Ada.Characters.Wide_Wide_Latin_1.CR
               then
                  Line := Line.Head (Line.Length - 1);
               end if;

               Text.Append (Line);
            end loop;
         end;
      end if;
   end Decode_Post;

   ------------
   -- Do_Get --
   ------------

   overriding procedure Do_Get
     (Self     : in out Pastebin_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      pragma Unreferenced (Self);

      JSON    : League.JSON.Documents.JSON_Document;
      Object  : League.JSON.Objects.JSON_Object;
      Lines   : League.JSON.Arrays.JSON_Array;
      Context : constant access Servlet.Contexts.Servlet_Context'Class :=
        Request.Get_Servlet_Context;

      Id      : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"id");

      Real_Name : constant League.Strings.Universal_String :=
        Context.Get_Real_Path (Prefix & Id);

      Text  : constant League.Strings.Universal_String := Axe.Read_File
        (Real_Name, League.Text_Codecs.Codec_For_Application_Locale);

      List  : constant League.String_Vectors.Universal_String_Vector :=
        Text.Split (Ada.Characters.Wide_Wide_Latin_1.LF);
   begin
      for J in 1 .. List.Length loop
         Lines.Append (League.JSON.Values.To_JSON_Value (List (J)));
      end loop;

      Object.Insert (+"text", Lines.To_JSON_Value);
      JSON.Set_Object (Object);
      Response.Set_Status (Servlet.HTTP_Responses.OK);
      Response.Set_Content_Type (+"application/json");
      Response.Set_Character_Encoding (+"utf-8");
      Response.Get_Output_Stream.Write (JSON.To_JSON);
   end Do_Get;

   -------------
   -- Do_Post --
   -------------

   overriding procedure Do_Post
     (Self     : in out Pastebin_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      pragma Unreferenced (Self);

      Text     : League.String_Vectors.Universal_String_Vector;
      Is_JSON  : Boolean := False;
   begin
      Decode_Post (Request, Text, Is_JSON);

      declare
         File    : Ada.Wide_Wide_Text_IO.File_Type;
         Context : constant access Servlet.Contexts.Servlet_Context'Class :=
           Request.Get_Servlet_Context;

         Value : constant League.Strings.Universal_String :=
           Text.Join (Ada.Characters.Wide_Wide_Latin_1.LF);

         Image : constant Wide_Wide_String :=
           League.Hash_Type'Wide_Wide_Image (Value.Hash);

         Hash : constant League.Strings.Universal_String :=
           +Image (2 .. Image'Last);

         Real_Name : constant League.Strings.Universal_String :=
           Context.Get_Real_Path (Prefix & Hash);
      begin
         Ada.Wide_Wide_Text_IO.Create
           (File, Name => Real_Name.To_UTF_8_String, Form => "WCEM=8");

         Ada.Wide_Wide_Text_IO.Put (File, Value.To_Wide_Wide_String);
         Ada.Wide_Wide_Text_IO.Close (File);

         if Is_JSON then
            declare
               JSON   : League.JSON.Documents.JSON_Document;
               Object : League.JSON.Objects.JSON_Object;
            begin
               Object.Insert
                 (+"id",
                  League.JSON.Values.To_JSON_Value (Hash));

               JSON.Set_Object (Object);
               Response.Set_Status (Servlet.HTTP_Responses.OK);
               Response.Set_Content_Type (+"application/json");
               Response.Set_Character_Encoding (+"utf-8");
               Response.Get_Output_Stream.Write (JSON.To_JSON);
            end;
         else
            Response.Set_Status (Servlet.HTTP_Responses.Moved_Temporarily);
            Response.Set_Header
              (+"Location", "/editor.html?id=" & Hash);
         end if;
      end;
   end Do_Post;

   ----------------------
   -- Get_Servlet_Info --
   ----------------------

   overriding function Get_Servlet_Info
     (Self : Pastebin_Servlet)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return +"Servlet to manage Ada pastebins";
   end Get_Servlet_Info;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
      return Pastebin_Servlet
   is
      pragma Unreferenced (Parameters);
   begin
      return Result : Pastebin_Servlet;
   end Instantiate;

end Servlet.Pastebin;
