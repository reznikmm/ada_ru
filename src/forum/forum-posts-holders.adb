with League.Calendars.ISO_8601;
with League.Characters.Latin;
with League.String_Vectors;

with Forum.Contexts; pragma Unreferenced (Forum.Contexts);
with Forum.Users.Holders;

package body Forum.Posts.Holders is

   function Cleanup_Text (Text : League.Strings.Universal_String)
     return League.Strings.Universal_String;

   ------------------
   -- Cleanup_Text --
   ------------------

   function Cleanup_Text (Text : League.Strings.Universal_String)
     return League.Strings.Universal_String
   is
      List   : constant League.String_Vectors.Universal_String_Vector :=
        Text.Split (League.Characters.Latin.Line_Feed);
      Result : League.String_Vectors.Universal_String_Vector;
      Last   : Natural := 0;
      Quote  : constant Natural :=
        List.Index (+"----- Исходное сообщение -----");
   begin
      for J in reverse 1 .. List.Length loop
         declare
            Line : constant League.Strings.Universal_String :=
              List.Element (J);
         begin
            if not Line.Is_Empty and not Line.Starts_With (">") then
               Last := J;
               exit;
            end if;
         end;
      end loop;

      if Quote in 1 .. Last then
         Last := Quote - 1;
      end if;

      for J in 1 .. Last loop
         declare
            Max_Line :  constant := 100;
            Last     : Natural;
            Line     : League.Strings.Universal_String :=
              List.Element (J);
         begin
            while Line.Length > Max_Line loop
               Last := Line.Last_Index (Max_Line, ' ');

               exit when Last = 0;

               Result.Append (Line.Slice (1, Last - 1));
               Line := Line.Tail_From (Last + 1);
            end loop;

            Result.Append (Line);
         end;
      end loop;

      return Result.Join (League.Characters.Latin.Line_Feed);
   end Cleanup_Text;

   ---------------
   -- Component --
   ---------------

   procedure Component
     (Self    : aliased Post_Reference;
      Name    : League.Strings.Universal_String;
      Value   : out League.Holders.Holder;
      Success : out Boolean)
   is
      use type League.Strings.Universal_String;
      Object : Post renames Self.Container.Post_Map (Self.Id);
   begin
      Success := True;

      if Name = +"subject" then
         Value := League.Holders.To_Holder (Object.Subject);
      elsif Name = +"text" then
         Value := League.Holders.To_Holder (Cleanup_Text (Object.Text));
      elsif Name = +"date" then
         declare
            Pattern : constant League.Strings.Universal_String :=
             +"yyyy-MM-dd HH:mm:ss";
            Image   : constant League.Strings.Universal_String :=
              League.Calendars.ISO_8601.Image (Pattern, Object.Date);
         begin
            Value := League.Holders.To_Holder (Image);
         end;
      elsif Name = +"author" then
         declare
            User : constant Forum.Users.Holders.User_Reference :=
              (Self.Container.Context.Users'Access, Object.Nickname);
         begin
            Value := Forum.Users.Holders.Compound_Holders.To_Holder (User);
         end;
      else
         Success := False;
      end if;
   end Component;

end Forum.Posts.Holders;
