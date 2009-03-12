with Ada.Calendar;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Wiki.Utils;
with Wiki.Parser;

with AWS.Resources;

package body Wiki.Sidebar is
   use Ada.Strings.Unbounded;

   type Item_Kind is (Normal, Added, Changed);

   type Item;
   type Item_Access is access Item;

   type Item is record
      Name  : Unbounded_String;
      Title : Unbounded_String;
      Level : Natural   := 0;
      Kind  : Item_Kind := Normal;
      Fill  : Boolean := True;
      Next  : Item_Access;
      Down  : Item_Access;
      Up    : Item_Access;
   end record;

   Prefix    : Unbounded_String;
   Suffix    : Unbounded_String;
   Root      : Item_Access;
   Root_Name : Unbounded_String;
   Root_Time : Ada.Calendar.Time := Ada.Calendar.Time_Of (2000, 1, 1);

   procedure Destroy (Tree : in out Item_Access);

   function Find (Tree : Item_Access; Name : String) return Boolean;

   function Expand
     (Tree : Item_Access;
      Name : String) return Unbounded_String;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Tree : in out Item_Access) is
      procedure Free is new Ada.Unchecked_Deallocation (Item, Item_Access);

      Next : Item_Access;
      Prev : Item_Access;
   begin
      if Tree /= null then
         Prev := Tree.Down;

         while Prev /= null loop
            Next := Prev.Next;
            Destroy (Prev);
            Prev := Next;
         end loop;
      end if;

      Free (Tree);
   end Destroy;

--     procedure Dump (Tree : Item_Access) is
--        Next : Item_Access;
--     begin
--        if Tree = null  then
--           Ada.Text_IO.Put_Line ("<null>");
--        else
--           Ada.Text_IO.Put_Line ("Name:" & To_String (Tree.Name)
--                                 & "Title:" & To_String (Tree.Title)
--                                 & "Level:" & (Tree.Level'Img));

--           Next := Tree.Down;

--           while Next /= null loop
--              Dump (Next);
--              Next := Next.Next;
--           end loop;
--        end if;
--     end Dump;

   ------------
   -- Expand --
   ------------

   function Expand
     (Name            : String;
      File_Name       : String;
      Wiki_URI_Prefix : String) return String
   is

      In_Prefix : Boolean := False;
      In_Suffix : Boolean := False;
      Do_Suffix : Boolean := False;

      procedure Start_Element
        (Info : in     Element_Info;
         Data : in out Item_Access);

      procedure End_Element
        (Info : in     Element_Info;
         Data : in out Item_Access);

      procedure Characters
        (Text : in     String;
         Data : in out Item_Access);

      -------------------
      -- Start_Element --
      -------------------

      procedure Start_Element
        (Info : in     Element_Info;
         Data : in out Item_Access)
      is
         Next : Item_Access;
      begin
         case Info.Kind is
            when Ordered_List =>
               Next := new Item;
               Next.Level := Data.Level + 1;
               Next.Up := Data;

               --  Add to end of list of children

               if Data.Down = null then
                  Data.Down := Next;
               else
                  declare
                     Prev : Item_Access := Data.Down;
                     Step : Item_Access := Prev.Next;
                  begin
                     while Step /= null loop
                        Prev := Step;
                        Step:= Step.Next;
                     end loop;

                     Prev.Next := Next;
                  end;
               end if;

               Data := Next;
            when List_Item =>
               if not Data.Fill then
                  Next := new Item;
                  Next.Level := Data.Level;
                  Next.Up    := Data.Up;
                  Data.Next  := Next;
                  Data := Next;
               end if;
            when Boxed_Link =>
               Data.Name := Info.Link;
               Data.Title := Info.Title;
               Data.Fill := False;
            when Boxed_Wiki_Link =>
               Data.Name := To_Unbounded_String (Wiki_URI_Prefix &
                 Slice (Info.Link, 6, Length (Info.Link)));
               Data.Title := Info.Title;
               Data.Fill := False;
            when Preformat =>
               if Do_Suffix then
                  In_Suffix := True;
               else
                  In_Prefix := True;
               end if;
            when others =>
               null;
         end case;
      end Start_Element;

      -----------------
      -- End_Element --
      -----------------

      procedure End_Element
        (Info : in     Element_Info;
         Data : in out Item_Access) is
      begin
         case Info.Kind is
            when Ordered_List =>
               Data := Data.Up;

               while Data.Next /= null loop
                  Data := Data.Next;
               end loop;

               Do_Suffix := True;
            when Preformat =>
               In_Prefix := False;
               In_Suffix := False;
            when others =>
               null;
         end case;
      end End_Element;

      ----------------
      -- Characters --
      ----------------

      procedure Characters
        (Text : in     String;
         Data : in out Item_Access) is
      begin
         if In_Prefix then
            Prefix := Prefix & Text;
         elsif In_Suffix then
            Suffix := Suffix & Text;
         elsif Ada.Strings.Fixed.Index (Text, "new") /= 0 then
            Data.Kind := Added;
         elsif Ada.Strings.Fixed.Index (Text, "changed") /= 0 then
            Data.Kind := Changed;
         end if;
      end Characters;

      procedure Build is new Wiki.Parser.Parse
        (Item_Access, Start_Element, End_Element, Characters);


      use type Ada.Calendar.Time;

      Time    : constant Ada.Calendar.Time :=
        AWS.Resources.File_Timestamp (File_Name);
   begin
      if Time > Root_Time or Root_Name /= File_Name then
         if Root /= null then
            Destroy (Root);
         end if;
         Root      := new Item;
         Prefix    := Null_Unbounded_String;
         Suffix    := Null_Unbounded_String;
         In_Prefix := False;
         In_Suffix := False;
         Do_Suffix := False;

         Build (ASCII.LF & Utils.Read_File (File_Name), Root);
         Root_Time := Time;
         Root_Name := To_Unbounded_String (File_Name);
      end if;

      return To_String (Prefix & Expand (Root, Name) & Suffix);
   end Expand;

   ------------
   -- Expand --
   ------------

   function Expand
     (Tree : Item_Access;
      Name : String) return Unbounded_String
   is
      procedure Output
        (Next   : in     Item_Access;
         Result : in out Unbounded_String);

      UL    : constant String := "<ul class='sidebar'>";

      procedure Output
        (Next   : in     Item_Access;
         Result : in out Unbounded_String)
      is
         Here  : constant Boolean := Find (Next, Name);

         Empty   : constant String := "<li class='empty'>";
         Opened  : constant String := "<li class='open'>";
         Closed  : constant String := "<li class='closed'>";
         Add     : constant String := "<span class='added'>&nbsp;</span>";
         Change  : constant String := "<span class='changed'>&nbsp;</span>";
         Pointer : constant String := "<span class='pointer'>&nbsp;</span>";
      begin
         if Next.Down = null then
            -- Output no children
            Result := Result & Empty;
         elsif Here then
            -- Output open children
            Result := Result & Opened;
         else
            -- Output close children
            Result := Result & Closed;
         end if;

         Result := Result
           & "<a href='" & Next.Name & "'>" & Next.Title & "</a>"; 

         case Next.Kind is
            when Added =>
               Result := Result & Add;
            when Changed =>
               Result := Result & Change;
            when Normal =>
               null;
         end case;

         if Next.Name = Name then
            Result := Result & Pointer;
         end if;

         Result := Result & "</li>" & ASCII.LF;
      end Output;

      Result : Unbounded_String;
      Next   : Item_Access;

   begin
      if Tree.Up = null or else Find (Tree, Name) then
         Result := To_Unbounded_String (UL & ASCII.LF);

         Next := Tree.Down;

         while Next /= null loop
            Output (Next, Result);
            Result := Result & Expand (Next, Name);
            Next := Next.Next;
         end loop;

         Result := Result & "</ul>" & ASCII.LF;
      end if;

      return Result;
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
            Next : Item_Access := Tree.Down;
         begin
            while Next /= null loop
               if Find (Next, Name) then
                  return True;
               end if;

               Next := Next.Next;
            end loop;

            return False;
         end;
      end if;
   end Find;

end Wiki.Sidebar;
