with Ada.Text_IO;
with Ada.Calendar;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with AWS.Resources;
with Templates_Parser;

package body Sidebar is

   use Templates_Parser;
   use Ada.Strings.Unbounded;

   use type Ada.Calendar.Time;

   type Item_Kind is (Normal, Added, Changed);

   type Item;
   type Item_Access is access Item;

   type Item is record
      Name  : Unbounded_String;
      Title : Unbounded_String;
      Level : Natural   := 0;
      Kind  : Item_Kind := Normal;
      Next  : Item_Access;
      Down  : Item_Access;
   end record;

   Root      : Item_Access;
   Root_Name : Unbounded_String;
   Root_Time : Ada.Calendar.Time := Ada.Calendar.Time_Of (2000, 1, 1);

   Space : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set (" ");

   function Find (Tree : Item_Access; Name : String) return Boolean;

   procedure Expand
     (Name   : in     String;
      Tree   : in     Item_Access;
      Names  : in out Tag;
      Titles : in out Tag;
      Levels : in out Tag;
      Empty  : in out Tag);

   procedure Read (File_Name : String);

   procedure Destroy (Tree : in out Item_Access);

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Tree : in out Item_Access) is
      procedure Free is new Ada.Unchecked_Deallocation (Item, Item_Access);

      Next : Item_Access;
      Prev : Item_Access;
   begin
      if Tree /= null and then Tree.Down /= null then
         Next := Tree.Down.Next;
         Prev := Tree.Down;

         loop
            Destroy (Prev);

            exit when Next = Tree.Down;

            Prev := Next;
            Next := Next.Next;
         end loop;

         Free (Tree);
      end if;

      Free (Tree);
   end Destroy;

   ------------
   -- Expand --
   ------------

   function Expand (Name : String; File_Name : String) return String is
      Time    : constant Ada.Calendar.Time :=
        AWS.Resources.File_Timestamp (File_Name);

      Names  : Tag;
      Titles : Tag;
      Levels : Tag;
      Empty  : Tag;
   begin
      if Time > Root_Time or Root_Name /= File_Name then
         Read (File_Name);
         Root_Time := Time;
         Root_Name := To_Unbounded_String (File_Name);
      end if;

      Expand (Name, Root, Names, Titles, Levels, Empty);

      return Parse ("sidebar.thtml",
                    (1 => Assoc ("NAME", Names),
                     2 => Assoc ("TITLE", Titles),
                     3 => Assoc ("LEVEL", Levels),
                     4 => Assoc ("EMPTY", Empty)),
                    Cached => True);
   end Expand;

   ------------
   -- Expand --
   ------------

   procedure Expand
     (Name   : in     String;
      Tree   : in     Item_Access;
      Names  : in out Tag;
      Titles : in out Tag;
      Levels : in out Tag;
      Empty  : in out Tag)
   is
      Next : Item_Access := Tree.Down.Next;
   begin
      loop
         Names  := Names  & (+Next.Name);
         Titles := Titles & (+Next.Title);
         Levels := Levels & (Next.Level);
         Empty  := Empty  & (+(Next.Down /= null));

         if Find (Next, Name) and then Next.Name /= Name then
            Expand (Name, Next, Names, Titles, Levels, Empty);
         end if;

         exit when Next = Tree.Down;

         Next := Next.Next;
      end loop;
   end Expand;

   ----------
   -- Find --
   ----------

   function Find (Tree : Item_Access; Name : String) return Boolean is
   begin
      if Tree.Name = Name then
         return True;
      elsif Tree.Down = null then
         return False;
      else
         declare
            Next : Item_Access := Tree.Down.Next;
         begin
            loop
               if Find (Next, Name) then
                  return True;
               end if;

               exit when Next = Tree.Down;

               Next := Next.Next;
            end loop;

            return False;
         end;
      end if;
   end Find;

   ----------
   -- Read --
   ----------

   procedure Read (File_Name : String) is
      use Ada.Text_IO;

      Input : File_Type;

      function To_Item (Text : String) return Item_Access;

      procedure Read_Children
        (Parent : in out Item;
         Next   : in out Item_Access);

      procedure Add
        (Parent : in out Item;
         Child  : in out Item_Access);

      ---------
      -- Add --
      ---------

      procedure Add
        (Parent : in out Item;
         Child  : in out Item_Access) is
      begin
         if Parent.Down = null then
            Parent.Down := Child;
            Child.Next  := Child;
         else
            Child.Next := Parent.Down.Next;
            Parent.Down.Next := Child;
            Parent.Down := Child;
         end if;
      end Add;

      -------------------
      -- Read_Children --
      -------------------

      procedure Read_Children
        (Parent : in out Item;
         Next   : in out Item_Access)
      is
         Line  : String (1 .. 80);
         Last  : Natural;
         Prev  : Item_Access;
      begin
         if Next = null then
            Get_Line (Input, Line, Last);
            Next := To_Item (Line (1 .. Last));
         end if;

         while Next /= null and then Next.Level > Parent.Level loop
            if Next.Level = Parent.Level + 1 then
               Add (Parent, Next);
               Prev := Next;

               if End_Of_File (Input) then
                  Next := null;
               else
                  Get_Line (Input, Line, Last);
                  Next := To_Item (Line (1 .. Last));
               end if;
            else
               Read_Children (Prev.all, Next);
            end if;
         end loop;
      end Read_Children;

      -------------
      -- To_Item --
      -------------

      function To_Item (Text : String) return Item_Access is
         use Ada.Strings.Fixed;
         Result : Item;
         First  : Positive;
         Last   : Natural;
      begin
         if Text (1) = '+' then
            Result.Kind := Added;
         elsif Text (1) = '*' then
            Result.Kind := Changed;
         end if;

         Find_Token
           (Source => Text (2 .. Text'Last),
            Set    => Space,
            Test   => Ada.Strings.Outside,
            First  => First,
            Last   => Last);

         Result.Level := First / 2;
         Result.Name  := To_Unbounded_String (Text (First .. Last));

         Find_Token
           (Source => Text (Last + 1 .. Text'Last),
            Set    => Space,
            Test   => Ada.Strings.Outside,
            First  => First,
            Last   => Last);

         Result.Title  := To_Unbounded_String (Text (First .. Last));

         return new Item'(Result);
      end To_Item;

      Next : Item_Access;
   begin
      Open (Input, In_File, File_Name);
      Destroy (Root);
      Root := new Item;
      Read_Children (Root.all, Next);
      Close (Input);
   end Read;

end Sidebar;
