with Ada.Containers.Generic_Sort;

with SQL.Queries;

with Forum.Contexts; pragma Unreferenced (Forum.Contexts);
with Forum.Forums.Holders;

package body Forum.Forums is

   ---------------
   -- Add_Topic --
   ---------------

   not overriding procedure Add_Topic
     (Self  : in out Container;
      Forum : Forum_Id;
      Topic : League.Strings.Universal_String) is
   begin
      Self.Forum_Map (Forum).Topics.Append (Topic);
   end Add_Topic;

   ----------
   -- Hash --
   ----------

   function Hash (Value : Forum_Id) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type'Mod (Value);
   end Hash;

   ---------------
   -- Initiaize --
   ---------------

   not overriding procedure Initiaize
     (Self : in out Container;
      DB   : in out SQL.Databases.SQL_Database)
   is
      Query : SQL.Queries.SQL_Query := DB.Query
        (+"select id, sort_key, subject from forums");
   begin
      Query.Execute;

      while Query.Next loop
         declare
            Object : constant Forum :=
              Forum'(Id       => Forum_Id_Holders.Element (Query.Value (1)),
                     Sort_Key => Forum_Id_Holders.Element (Query.Value (2)),
                     Subject  => League.Holders.Element (Query.Value (3)),
                     Topics   => String_Vector.Empty_Vector);
         begin
            Self.Forum_Map.Insert (Object.Id, Object);
         end;
      end loop;
   end Initiaize;

   not overriding function Last_Topics_Holder
     (Self  : aliased in out Container) return League.Holders.Holder
   is
      Value : constant Container_Access := Self'Access;
   begin
      return Holders.Last_Topics_Iterable_Holders.To_Holder (Value);
   end Last_Topics_Holder;

   -----------------
   -- Sort_Topics --
   -----------------

   not overriding procedure Sort_Topics (Self : in out Container) is
      procedure Sort_Topics (Topics : in out String_Vector.Vector);
      procedure Add_To_Last_Topics
        (Topic : League.Strings.Universal_String);

      Max_Last_Topic : constant := 20;

      procedure Add_To_Last_Topics
        (Topic : League.Strings.Universal_String)
      is
         use type Ada.Containers.Count_Type;
         function "<" (Left, Right : Positive) return Boolean;
         procedure Swap (Left, Right : Positive);

         function "<" (Left, Right : Positive) return Boolean is
         begin
            return not Self.Context.Posts.Before
              (Self.Last_Topics (Left), Self.Last_Topics (Right));
         end "<";

         procedure Swap (Left, Right : Positive) is
            Save : constant League.Strings.Universal_String :=
              Self.Last_Topics (Left);
         begin
            Self.Last_Topics (Left) := Self.Last_Topics (Right);
            Self.Last_Topics (Right) := Save;
         end Swap;

         procedure Sort is new Ada.Containers.Generic_Sort
           (Positive, "<", Swap);
      begin
         if Self.Last_Topics.Length < Max_Last_Topic then
            Self.Last_Topics.Append (Topic);
            if Self.Last_Topics.Length = Max_Last_Topic then
               Sort (1, Max_Last_Topic);
            end if;
         elsif not Self.Context.Posts.Before
           (Topic, Self.Last_Topics.Last_Element)
         then
            Self.Last_Topics (Max_Last_Topic) := Topic;
            Sort (1, Max_Last_Topic);
         end if;
      end Add_To_Last_Topics;

      -----------------
      -- Sort_Topics --
      -----------------

      procedure Sort_Topics (Topics : in out String_Vector.Vector) is
         function "<" (Left, Right : Positive) return Boolean;
         procedure Swap (Left, Right : Positive);

         function "<" (Left, Right : Positive) return Boolean is
         begin
            return Self.Context.Topics.Before
              (Topics (Left), Topics (Right));
         end "<";

         procedure Swap (Left, Right : Positive) is
            Save : constant League.Strings.Universal_String := Topics (Left);
         begin
            Topics (Left) := Topics (Right);
            Topics (Right) := Save;
         end Swap;

         procedure Sort is new Ada.Containers.Generic_Sort
           (Positive, "<", Swap);
      begin
         Sort (Topics.First_Index, Topics.Last_Index);

         for Topic of Topics loop
            Add_To_Last_Topics (Topic);
         end loop;
      end Sort_Topics;
   begin
      for J of Self.Forum_Map loop
         Sort_Topics (J.Topics);
      end loop;
   end Sort_Topics;

   ---------------
   -- To_Holder --
   ---------------

   not overriding function To_Holder
     (Self  : aliased in out Container) return League.Holders.Holder
   is
      function "<" (Left, Right : Positive) return Boolean;
      procedure Swap (Left, Right : Positive);

      List : Holders.Forum_List := (Self'Unchecked_Access, Id_List => <>);

      ---------
      -- "<" --
      ---------

      function "<" (Left, Right : Positive) return Boolean is
         L : Forum renames Self.Forum_Map (List.Id_List (Left));
         R : Forum renames Self.Forum_Map (List.Id_List (Right));
      begin
         return L.Sort_Key < R.Sort_Key;
      end "<";

      ----------
      -- Swap --
      ----------

      procedure Swap (Left, Right : Positive) is
         Save : constant Forum_Id := List.Id_List (Left);
      begin
         List.Id_List (Left) := List.Id_List (Right);
         List.Id_List (Right) := Save;
      end Swap;

      procedure Sort is new Ada.Containers.Generic_Sort (Positive, "<", Swap);
   begin
      for J of Self.Forum_Map loop
         List.Id_List.Append (J.Id);
      end loop;

      --  Sort forum list by Sort_Key
      Sort (List.Id_List.First_Index, List.Id_List.Last_Index);

      return Holders.Iterable_Holders.To_Holder (List);
   end To_Holder;

end Forum.Forums;
