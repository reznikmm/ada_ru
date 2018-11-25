with League.Base_Codecs;
with League.Holders;
with League.Settings;
with League.Stream_Element_Vectors;

with SQL.Queries;
with SQL.Options;

with Ada.Wide_Wide_Text_IO;

package body Sessions.Managers is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   --------------
   -- Do_Login --
   --------------

   overriding procedure Do_Login
    (Self     : in out HTTP_Session_Manager;
     Info     : Sessions.User_Info;
     Path     : League.Strings.Universal_String;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is

      type Boolean_Set is array (Positive range <>) of Boolean;

      function Find_User
        (DB    : in out Databases.SQL_Database;
         Nick  : in out League.Strings.Universal_String;
         Found : in out Boolean_Set) return Boolean;

      procedure Create_User (DB : in out Databases.SQL_Database);
      procedure Update_Mails
        (DB    : in out Databases.SQL_Database;
         Nick  : League.Strings.Universal_String;
         Found : Boolean_Set);

      -----------------
      -- Create_User --
      -----------------

      procedure Create_User (DB : in out Databases.SQL_Database) is
      begin
         declare
            Query : SQL.Queries.SQL_Query := DB.Query;
         begin
            Query.Prepare
              (+"insert into users(nickname,name,avatar) values (:k,:n,:a)");
            Query.Bind_Value (+":k", League.Holders.To_Holder (Info.User));
            Query.Bind_Value (+":n", League.Holders.To_Holder (Info.Name));
            Query.Bind_Value (+":a", League.Holders.To_Holder (Info.Avatar));
            Query.Execute;
         end;

         declare
            Main  : League.Strings.Universal_String := +"Y";
            Query : SQL.Queries.SQL_Query := DB.Query;
         begin
            Query.Prepare
              (+"insert into emails(email,nickname,main)values(:e,:k,:m)");

            for J in 1 .. Info.Mails.Length loop
               Query.Bind_Value
                 (+":e", League.Holders.To_Holder (Info.Mails (J)));
               Query.Bind_Value
                 (+":k", League.Holders.To_Holder (Info.User));
               Query.Bind_Value
                 (+":m", League.Holders.To_Holder (Main));
               Query.Execute;
               Main := +"N";
            end loop;
         end;

         --  Self.Events.On_User_Created (Info.Name, Info.Avatar);
      end Create_User;

      ---------------
      -- Find_User --
      ---------------

      function Find_User
        (DB    : in out Databases.SQL_Database;
         Nick  : in out League.Strings.Universal_String;
         Found : in out Boolean_Set) return Boolean
      is
         Query  : SQL.Queries.SQL_Query := DB.Query;
         Result : Boolean := False;
      begin
         Query.Prepare (+"select nickname from emails where email=:m");
         for J in 1 .. Info.Mails.Length loop
            Query.Bind_Value
              (+":m", League.Holders.To_Holder (Info.Mails (J)));
            Query.Execute;

            if Query.Next then
               Nick := League.Holders.Element (Query.Value (1));
               Result := True;
               Found (J) := True;
            end if;
         end loop;

         return Result;
      end Find_User;

      ------------------
      -- Update_Mails --
      ------------------

      procedure Update_Mails
        (DB    : in out Databases.SQL_Database;
         Nick  : League.Strings.Universal_String;
         Found : Boolean_Set)
      is
         Query  : SQL.Queries.SQL_Query := DB.Query;
      begin
         Query.Prepare (+"insert into emails(email, nickname) values (:e,:k)");
         for J in 1 .. Info.Mails.Length loop
            if not Found (J) then
               Query.Bind_Value
                 (+":e", League.Holders.To_Holder (Info.Mails (J)));
               Query.Bind_Value
                 (+":k", League.Holders.To_Holder (Nick));
               Query.Execute;
            end if;
         end loop;
      end Update_Mails;

      Found   : Boolean_Set (1 .. Info.Mails.Length);
      Session : Sessions.HTTP_Session renames
        Sessions.HTTP_Session (Request.Get_Session.all);
      DB : Databases.SQL_Database := Self.Pool.Create;
   begin
      Session.Info := Info;

      if not Find_User (DB, Session.Info.User, Found) then
         Create_User (DB);
      elsif Found'Length > 1 then
         Update_Mails (DB, Session.Info.User, Found);
      end if;

      Ada.Wide_Wide_Text_IO.Put_Line ("User=" & Info.User.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Put_Line ("Name=" & Info.Name.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Put_Line
        ("Avatar=" & Info.Avatar.To_Wide_Wide_String);

      for J in 1 .. Info.Mails.Length loop
         Ada.Wide_Wide_Text_IO.Put_Line
           ("Mail=" & Info.Mails (J).To_Wide_Wide_String);
      end loop;

      Response.Set_Status (Servlet.HTTP_Responses.See_Other);
      Response.Set_Header (+"Location", Path);
      Response.Set_Header (+"Cache-Control", +"must-revalidate");
   end Do_Login;

   -----------------
   -- Get_Session --
   -----------------

   overriding function Get_Session
     (Self       : in out HTTP_Session_Manager;
      Identifier : League.Strings.Universal_String)
      return access Servlet.HTTP_Sessions.HTTP_Session'Class
   is
   begin
      if Self.Map.Contains (Identifier) then
         return Self.Map.Element (Identifier);
      else
         return null;
      end if;
   end Get_Session;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : in out HTTP_Session_Manager;
      Events : access Axe.Events.Listener'Class)
   is
      Settings : League.Settings.Settings;
      Driver   : constant League.Holders.Holder :=
        Settings.Value (+"/db/driver");
      Database : constant League.Holders.Holder :=
        Settings.Value (+"/db/dbname");
      Option : SQL.Options.SQL_Options;
   begin
      Self.Map.Clear;
      Stream_Element_Random.Reset (Self.Random);
      Option.Set (+"dbname", League.Holders.Element (Database));
      Stream_Element_Random.Reset (Self.Random);
      Self.Pool.Initialize (League.Holders.Element (Driver), Option);
      Self.Events := Events;
   end Initialize;

   ---------------------------------
   -- Is_Session_Identifier_Valid --
   ---------------------------------

   overriding function Is_Session_Identifier_Valid
     (Self       : HTTP_Session_Manager;
      Identifier : League.Strings.Universal_String)
      return Boolean
   is
   begin
      return Self.Map.Contains (Identifier);
   end Is_Session_Identifier_Valid;

   -----------------
   -- New_Session --
   -----------------

   overriding function New_Session
     (Self : in out HTTP_Session_Manager)
      return access Servlet.HTTP_Sessions.HTTP_Session'Class
   is
      Data   : League.Stream_Element_Vectors.Stream_Element_Vector;
      New_Id : League.Strings.Universal_String;
      Result : Session_Access;
   begin
      for J  in 1 .. 12 loop
         Data.Append (Stream_Element_Random.Random (Self.Random));
      end loop;

      New_Id := League.Base_Codecs.To_Base_64 (Data);

      Result := new Sessions.HTTP_Session'
        (Servlet.HTTP_Sessions.HTTP_Session with Id => New_Id, others => <>);

      Self.Map.Insert (New_Id, Result);

      return Result;
   end New_Session;

end Sessions.Managers;
