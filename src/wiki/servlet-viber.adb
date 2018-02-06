with Ada.Streams;
with Ada.Wide_Wide_Text_IO;

with League.JSON.Documents;
with League.JSON.Objects;
with League.Stream_Element_Vectors;
with League.Text_Codecs;

package body Servlet.Viber is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

   procedure To_JSON
     (Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Document : out League.JSON.Documents.JSON_Document);

   -------------
   -- Do_Post --
   -------------

   overriding procedure Do_Post
     (Self     : in out Viber_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      Document : League.JSON.Documents.JSON_Document;
      Object   : League.JSON.Objects.JSON_Object;
      Result   : League.JSON.Objects.JSON_Object;
   begin
      To_JSON (Request, Document);
      Object := Document.To_JSON_Object;

      if Object.Contains (+"event") then
         Self.Listener.On_Viber (Object, Result);
      end if;

      Response.Set_Status (Servlet.HTTP_Responses.OK);
      Response.Set_Content_Type (+"application/json");
      Response.Set_Character_Encoding (+"utf-8");

      if Result.Is_Empty then
         Response.Get_Output_Stream.Write (+"{}");
      else
         Document := Result.To_JSON_Document;
         Response.Get_Output_Stream.Write (Document.To_JSON);
      end if;
   end Do_Post;

   ----------------------
   -- Get_Servlet_Info --
   ----------------------

   overriding function Get_Servlet_Info
     (Self : Viber_Servlet)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return +"Viber Servlet";
   end Get_Servlet_Info;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : in out Viber_Servlet'Class;
      Token : League.Strings.Universal_String) is
   begin
      Self.Token := Token;
   end Initialize;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
      return Viber_Servlet
   is
      pragma Unreferenced (Parameters);
   begin
      return Result : Viber_Servlet;
   end Instantiate;

   ------------------
   -- Set_Listener --
   ------------------

   not overriding procedure Set_Listener
     (Self  : in out Viber_Servlet;
      Value : access Axe.Events.Listener'Class)
   is
   begin
      Self.Listener := Value;
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

end Servlet.Viber;
