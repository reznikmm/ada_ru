with League.Calendars.ISO_8601;
with League.Holders;
with League.String_Vectors;
with SQL.Queries;

package body Forum.Posts is

   -------------------
   -- Assign_Topics --
   -------------------

   not overriding procedure Assign_Topics
     (Self   : in out Container;
      Forums : in out Forum.Forums.Container;
      Topics : in out Forum.Topics.Container)
   is
      procedure Assign_Topic (Post : in out Posts.Post);

      ------------------
      -- Assign_Topic --
      ------------------

      procedure Assign_Topic (Post : in out Posts.Post) is
      begin
         if not Post.Topic.Is_Empty then
            return;
         elsif not Post.Parent.Is_Empty then
            if Self.Post_Map.Contains (Post.Parent) then
               Assign_Topic (Self.Post_Map (Post.Parent));
               Post.Topic := Self.Post_Map (Post.Parent).Topic;
               Topics.Add_Post (Post.Date, Post.Topic, Post.Id);

               return;
            end if;
         end if;

         Topics.Create_Topic (Forums, Post.Date, Post.Id, Post.Subject);
         Post.Topic := Post.Id;
      end Assign_Topic;
   begin
      for Post of Self.Post_Map loop
         Assign_Topic (Post);
      end loop;
   end Assign_Topics;

   ------------
   -- Before --
   ------------

   not overriding function Before
     (Self  : Container;
      Left  : League.Strings.Universal_String;
      Right : League.Strings.Universal_String) return Boolean
   is
      use type League.Calendars.Date_Time;
      use type League.Strings.Universal_String;

      L : Post renames Self.Post_Map (Left);
      R : Post renames Self.Post_Map (Right);
   begin
      return L.Date < R.Date or else
        (L.Date = R.Date and then L.Id < R.Id);
   end Before;

   ---------------
   -- Initiaize --
   ---------------

   not overriding procedure Initiaize
     (Self  : in out Container;
      Users : in out Forum.Users.Container;
      DB    : in out SQL.Databases.SQL_Database)
   is
      function To_Date
        (Holder : League.Holders.Holder)
         return League.Calendars.Date_Time;

      -------------
      -- To_Date --
      -------------

      function To_Date
        (Holder : League.Holders.Holder)
         return League.Calendars.Date_Time
      is
         use League.Calendars.ISO_8601;
         Text : constant League.Strings.Universal_String :=
           League.Holders.Element (Holder);
         Parts : constant League.String_Vectors.Universal_String_Vector :=
           Text.Split (' ');
         Date : constant League.String_Vectors.Universal_String_Vector :=
           Parts.Element (1).Split ('-');
         Time : constant League.String_Vectors.Universal_String_Vector :=
           Parts.Element (2).Split (':');
         Result         : League.Calendars.Date_Time;
         Year           : Year_Number;
         Month          : Month_Number;
         Day            : Day_Number;
         Hour           : Hour_Number;
         Minute         : Minute_Number;
         Second         : Second_Number;
      begin
         Year := Year_Number'Wide_Wide_Value
           (Date.Element (1).To_Wide_Wide_String);

         Month := Month_Number'Wide_Wide_Value
           (Date.Element (2).To_Wide_Wide_String);

         Day := Day_Number'Wide_Wide_Value
           (Date.Element (3).To_Wide_Wide_String);

         Hour := Hour_Number'Wide_Wide_Value
           (Time.Element (1).To_Wide_Wide_String);

         Minute := Minute_Number'Wide_Wide_Value
           (Time.Element (2).To_Wide_Wide_String);

         Second := Second_Number'Wide_Wide_Value
           (Time.Element (3).To_Wide_Wide_String);

         Result := Create (Year, Month, Day, Hour, Minute, Second, 0);

         return Result;
      end To_Date;

      Query : SQL.Queries.SQL_Query := DB.Query
        (+"select id,author,sent,parent,subject,text from posts");
   begin
      Query.Execute;

      while Query.Next loop
         declare
            Object : Post :=
              Post'(Id       => League.Holders.Element (Query.Value (1)),
                    From     => League.Holders.Element (Query.Value (2)),
                    Date     => To_Date (Query.Value (3)),
                    Parent   => To_String (Query.Value (4)),
                    Subject  => League.Holders.Element (Query.Value (5)),
                    Text     => League.Holders.Element (Query.Value (6)),
                    Nickname => League.Strings.Empty_Universal_String,
                    Topic    => League.Strings.Empty_Universal_String);
         begin
            Users.Create_User (Object.From, Object.Nickname);
            Self.Post_Map.Insert (Object.Id, Object);
         end;
      end loop;
   end Initiaize;

   ---------------
   -- Post_Date --
   ---------------

   not overriding function Post_Date
     (Self : Container;
      Id   : League.Strings.Universal_String)
      return League.Calendars.Date_Time is
   begin
      return Self.Post_Map (Id).Date;
   end Post_Date;

end Forum.Posts;
