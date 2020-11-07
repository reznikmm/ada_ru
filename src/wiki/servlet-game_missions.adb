with League.Holders;
with League.JSON.Objects;
with League.JSON.Values;
with League.JSON.Documents;
with SQL.Queries;

with Databases;
with Sessions;

package body Servlet.Game_Missions is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

   function "-" (Holder : League.Holders.Holder)
     return League.Strings.Universal_String
       renames League.Holders.Element;

   Statistics_SQL : constant Wide_Wide_String :=
     "select 'total', count(*) from game_missions " &
     "where station = " &
       "(select station from game_missions where mission=:mission) " &
     "union all " &
     "select 'solved', count(*) from solved_missions s, game_missions m " &
     "where s.mission=m.mission " &
     "and s.nickname = :user " &
     "and m.station= " &
       "(select station from game_missions where mission=:mission) " &
     "union all " &
     "select 'attempted', count(distinct nickname) from solution_texts " &
     "where mission = :mission " &
     "union all " &
     "select 'successed', count(distinct nickname) from solved_missions " &
     "where mission = :mission " &
     "union all " &
     "select 'votes', coalesce(sum(vote),0) from game_mission_votes " &
     "where mission = :mission";

   Vote_SQL : constant Wide_Wide_String :=
     "insert into game_mission_votes(nickname, mission, vote) values " &
     "(:user, :mission, :vote) " &
     "on conflict (nickname, mission) do update set vote=:vote";

   ------------
   -- Do_Get --
   ------------

   overriding procedure Do_Get
     (Self     : in out Mission_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      pragma Unreferenced (Self);

      Slug : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"mission");

      Session : constant Sessions.HTTP_Session_Access :=
        Sessions.HTTP_Session_Access (Request.Get_Session);

      Info   : constant Sessions.User_Info := Session.Get_User_Info;
      DB     : Databases.SQL_Database := Session.Database;
      Query  : SQL.Queries.SQL_Query := DB.Query;
      Object : League.JSON.Objects.JSON_Object;
   begin
      if not Info.User.Is_Empty then
         Query.Prepare (+Statistics_SQL);
         Query.Bind_Value (+":user", League.Holders.To_Holder (Info.User));
         Query.Bind_Value (+":mission", League.Holders.To_Holder (Slug));
         Query.Execute;

         while Query.Next loop
            Object.Insert
              (-Query.Value (1),
               League.JSON.Values.To_JSON_Value (-Query.Value (2)));
         end loop;
      end if;

      Response.Set_Status (Servlet.HTTP_Responses.OK);
      Response.Set_Content_Type (+"application/json");
      Response.Set_Character_Encoding (+"utf-8");
      Response.Set_Header (+"Cache-Control", +"no-cache");

      Response.Get_Output_Stream.Write (Object.To_JSON_Document.To_JSON);
   end Do_Get;

   -------------
   -- Do_Post --
   -------------

   overriding procedure Do_Post
    (Self     : in out Mission_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      pragma Unreferenced (Self);

      Slug : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"mission");

      Vote : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"vote");

      Session : constant Sessions.HTTP_Session_Access :=
        Sessions.HTTP_Session_Access (Request.Get_Session);

      Info   : constant Sessions.User_Info := Session.Get_User_Info;
   begin
      if Info.User.Is_Empty then
         Response.Set_Status (Servlet.HTTP_Responses.Unauthorized);
      elsif Slug.Is_Empty or
        Vote.To_Wide_Wide_String not in "0" | "-1" | "+1" | "1"
      then
         Response.Set_Status (Servlet.HTTP_Responses.Forbidden);
      else
         declare
            DB     : Databases.SQL_Database := Session.Database;
            Query  : SQL.Queries.SQL_Query := DB.Query;
         begin
            Query.Prepare (+Vote_SQL);
            Query.Bind_Value (+":user", League.Holders.To_Holder (Info.User));
            Query.Bind_Value
              (+":mission", League.Holders.To_Holder (Slug));
            Query.Bind_Value (+":vote", League.Holders.To_Holder (Vote));
            Query.Execute;

            Response.Set_Status (Servlet.HTTP_Responses.No_Content);
         end;
      end if;
   end Do_Post;

   ----------------------
   -- Get_Servlet_Info --
   ----------------------

   overriding function Get_Servlet_Info
     (Self : Mission_Servlet) return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return League.Strings.To_Universal_String ("Mission info service");
   end Get_Servlet_Info;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Parameters : not null access Servlet.Generic_Servlets
        .Instantiation_Parameters'Class) return Mission_Servlet
   is
      pragma Unreferenced (Parameters);
   begin
      return (Servlet.HTTP_Servlets.HTTP_Servlet with null record);
   end Instantiate;

end Servlet.Game_Missions;
