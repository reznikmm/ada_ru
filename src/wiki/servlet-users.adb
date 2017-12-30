with League.JSON.Documents;
with League.JSON.Objects;
with League.JSON.Values;

with Sessions;
with Servlet.OAuth;

package body Servlet.Users is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

   ------------
   -- Do_Get --
   ------------

   overriding procedure Do_Get
     (Self     : in out User_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      pragma Unreferenced (Self);

      Session : constant Sessions.HTTP_Session_Access :=
        Sessions.HTTP_Session_Access (Request.Get_Session);

      Info : constant Servlet.OAuth.User_Info := Session.Get_User_Info;
      Object : League.JSON.Objects.JSON_Object;
      Value  : League.JSON.Values.JSON_Value;
   begin
      if Info.User.Is_Empty then
         Value := League.JSON.Values.Null_JSON_Value;
      else
         Value := League.JSON.Values.To_JSON_Value (Info.User);
      end if;

      Object.Insert (+"user", Value);
      Object.Insert (+"name", League.JSON.Values.To_JSON_Value (Info.Name));
      Object.Insert
        (+"avatar", League.JSON.Values.To_JSON_Value (Info.Avatar));

      if Info.Mails.Length > 0 then
         Value := League.JSON.Values.To_JSON_Value (Info.Mails (1));
      else
         Value := League.JSON.Values.Null_JSON_Value;
      end if;

      Object.Insert (+"mail", Value);

      Response.Set_Status (Servlet.HTTP_Responses.OK);
      Response.Set_Header (+"Cache-Control", +"no-cache");
      Response.Set_Content_Type (+"application/json");
      Response.Set_Character_Encoding (+"utf-8");
      Response.Get_Output_Stream.Write (Object.To_JSON_Document.To_JSON);
   end Do_Get;

   ----------------------
   -- Get_Servlet_Info --
   ----------------------

   overriding function Get_Servlet_Info
     (Self : User_Servlet)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return League.Strings.To_Universal_String ("User info service");
   end Get_Servlet_Info;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
      return User_Servlet
   is
      pragma Unreferenced (Parameters);
   begin
      return (Servlet.HTTP_Servlets.HTTP_Servlet with null record);
   end Instantiate;

end Servlet.Users;
