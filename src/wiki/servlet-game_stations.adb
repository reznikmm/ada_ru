with League.Holders;
with League.JSON.Arrays;
with League.JSON.Values;
with League.JSON.Documents;
with SQL.Queries;

with Databases;
with Sessions;

package body Servlet.Game_Stations is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

   function "-" (Holder : League.Holders.Holder)
     return League.Strings.Universal_String
       renames League.Holders.Element;

   Soved_SQL : constant Wide_Wide_String :=
     "select s.mission from solved_missions s, game_missions m " &
     "where s.nickname=:user and m.station=:station and s.mission=m.mission " &
     "order by s.mission";

   ------------
   -- Do_Get --
   ------------

   overriding procedure Do_Get
     (Self     : in out Station_Servlet;
      Request  :        Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      pragma Unreferenced (Self);

      Slug : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"station");

      Session : constant Sessions.HTTP_Session_Access :=
        Sessions.HTTP_Session_Access (Request.Get_Session);

      Info   : constant Sessions.User_Info := Session.Get_User_Info;
      DB     : Databases.SQL_Database := Session.Database;
      Query  : SQL.Queries.SQL_Query := DB.Query;
      Result : League.JSON.Arrays.JSON_Array;
   begin
      if not Info.User.Is_Empty then
         Query.Prepare (+Soved_SQL);
         Query.Bind_Value (+":user", League.Holders.To_Holder (Info.User));
         Query.Bind_Value (+":station", League.Holders.To_Holder (Slug));
         Query.Execute;

         while Query.Next loop
            Result.Append
              (League.JSON.Values.To_JSON_Value (-Query.Value (1)));
         end loop;
      end if;

      Response.Set_Status (Servlet.HTTP_Responses.OK);
      Response.Set_Content_Type (+"application/json");
      Response.Set_Character_Encoding (+"utf-8");
      Response.Set_Header (+"Cache-Control", +"no-cache");

      Response.Get_Output_Stream.Write (Result.To_JSON_Document.To_JSON);
   end Do_Get;

   ----------------------
   -- Get_Servlet_Info --
   ----------------------

   overriding function Get_Servlet_Info
     (Self : Station_Servlet) return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return League.Strings.To_Universal_String ("Station info service");
   end Get_Servlet_Info;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Parameters : not null access Servlet.Generic_Servlets
        .Instantiation_Parameters'Class) return Station_Servlet
   is
      pragma Unreferenced (Parameters);
   begin
      return (Servlet.HTTP_Servlets.HTTP_Servlet with null record);
   end Instantiate;

end Servlet.Game_Stations;
