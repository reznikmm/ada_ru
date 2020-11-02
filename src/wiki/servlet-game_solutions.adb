with Ada.Characters.Wide_Wide_Latin_1;

with League.Holders;
with League.String_Vectors;
with SQL.Queries;

with Databases;
with Sessions;

package body Servlet.Game_Solutions is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

   Get_Lines_SQL : constant Wide_Wide_String :=
     "select text from solution_texts where nickname=:user" &
     " and mission=:mission order by line";

   ------------
   -- Do_Get --
   ------------

   overriding procedure Do_Get
     (Self     : in out Solution_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      pragma Unreferenced (Self);

      Slug : constant League.String_Vectors.Universal_String_Vector :=
        Request.Get_Path_Info;

      Session : constant Sessions.HTTP_Session_Access :=
        Sessions.HTTP_Session_Access (Request.Get_Session);

      Info : constant Sessions.User_Info := Session.Get_User_Info;
   begin
      if Info.User.Is_Empty or Slug.Is_Empty then
         Response.Set_Status (Servlet.HTTP_Responses.Not_Found);
      else
         declare
            DB    : Databases.SQL_Database := Session.Database;
            Query : SQL.Queries.SQL_Query := DB.Query;
            Lines : League.String_Vectors.Universal_String_Vector;
         begin
            Query.Prepare (+Get_Lines_SQL);
            Query.Bind_Value (+":user", League.Holders.To_Holder (Info.User));
            Query.Bind_Value
              (+":mission", League.Holders.To_Holder (Slug (Slug.Length)));
            Query.Execute;

            while Query.Next loop
               Lines.Append (League.Holders.Element (Query.Value (1)));
            end loop;

            Response.Set_Status (Servlet.HTTP_Responses.OK);
            Response.Set_Content_Type (+"text/html");
            Response.Set_Character_Encoding (+"utf-8");
            Response.Get_Output_Stream.Write
              (Lines.Join (Ada.Characters.Wide_Wide_Latin_1.LF));
         end;
      end if;

      Response.Set_Header (+"Cache-Control", +"no-cache");
   end Do_Get;

   ----------------------
   -- Get_Servlet_Info --
   ----------------------

   overriding function Get_Servlet_Info
     (Self : Solution_Servlet) return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return +"Game Solution Servlet";
   end Get_Servlet_Info;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Parameters : not null access
        Servlet.Generic_Servlets.Instantiation_Parameters'Class)
      return Solution_Servlet
   is
      pragma Unreferenced (Parameters);
   begin
      return Result : Solution_Servlet;
   end Instantiate;

end Servlet.Game_Solutions;
