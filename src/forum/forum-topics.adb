with Ada.Containers.Generic_Sort;

with SQL.Queries;

with Forum.Contexts; pragma Unreferenced (Forum.Contexts);
with Forum.Posts;

package body Forum.Topics is

   --------------
   -- Add_Post --
   --------------

   not overriding procedure Add_Post
     (Self    : in out Container;
      Date    : League.Calendars.Date_Time;
      Starter : League.Strings.Universal_String;
      Post    : League.Strings.Universal_String)
   is
      use type League.Calendars.Date_Time;
      Object : Topic renames Self.Topic_Map (Starter);
   begin
      Object.Posts.Append (Post);

      if Date > Object.Date then
         Object.Date := Date;
      end if;
   end Add_Post;

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

      L : Topic renames Self.Topic_Map (Left);
      R : Topic renames Self.Topic_Map (Right);
   begin
      return L.Date > R.Date or else
        (L.Date = R.Date and then L.Starter > R.Starter);
   end Before;

   ------------------
   -- Create_Topic --
   ------------------

   not overriding procedure Create_Topic
     (Self     : in out Container;
      Forums   : in out Forum.Forums.Container;
      Date     : League.Calendars.Date_Time;
      Starter  : League.Strings.Universal_String;
      Subject  : League.Strings.Universal_String) is
   begin
      if not Self.Topic_Map.Contains (Starter) then
         Self.Topic_Map.Insert
           (Starter, Topic'(Starter, 1, Subject, Date, Posts => <>));
         Forums.Add_Topic (1, Starter);
      end if;
   end Create_Topic;

   ---------------
   -- Initiaize --
   ---------------

   not overriding procedure Initiaize
     (Self   : in out Container;
      DB     : in out SQL.Databases.SQL_Database;
      Forums : in out Forum.Forums.Container;
      Posts  : Forum.Posts.Container)
   is
      Query : SQL.Queries.SQL_Query := DB.Query
        (+"select forum, starter, subject from topics");
   begin
      Query.Execute;

      while Query.Next loop
         declare
            Starter : constant League.Strings.Universal_String :=
              League.Holders.Element (Query.Value (2));

            Object : constant Topic :=
              Topic'(Forum   => Forum_Id_Holders.Element (Query.Value (1)),
                     Starter => Starter,
                     Subject => League.Holders.Element (Query.Value (3)),
                     Date    => Posts.Post_Date (Starter),
                     Posts   => String_Vector.Empty_Vector);
         begin
            Self.Topic_Map.Insert (Object.Starter, Object);
            Forums.Add_Topic (Object.Forum, Object.Starter);
         end;
      end loop;
   end Initiaize;

   ----------------
   -- Sort_Posts --
   ----------------

   not overriding procedure Sort_Posts (Self : in out Container) is
      procedure Sort_Posts (Posts : in out String_Vector.Vector);

      procedure Sort_Posts (Posts : in out String_Vector.Vector) is
         function "<" (Left, Right : Positive) return Boolean;
         procedure Swap (Left, Right : Positive);

         function "<" (Left, Right : Positive) return Boolean is
         begin
            return Self.Context.Posts.Before
              (Posts (Left), Posts (Right));
         end "<";

         procedure Swap (Left, Right : Positive) is
            Save : constant League.Strings.Universal_String := Posts (Left);
         begin
            Posts (Left) := Posts (Right);
            Posts (Right) := Save;
         end Swap;

         procedure Sort is new Ada.Containers.Generic_Sort
           (Positive, "<", Swap);
      begin
         Sort (Posts.First_Index, Posts.Last_Index);
      end Sort_Posts;
   begin
      for J of Self.Topic_Map loop
         Sort_Posts (J.Posts);
      end loop;   end Sort_Posts;

end Forum.Topics;
