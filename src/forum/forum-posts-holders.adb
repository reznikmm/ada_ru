with Ada.Streams;
with League.Base_Codecs;
with League.Calendars.ISO_8601;
with League.Characters.Latin;
with League.Stream_Element_Vectors;

with Forum.Contexts; pragma Unreferenced (Forum.Contexts);
with Forum.Users.Holders;

package body Forum.Posts.Holders is
   use type League.Strings.Universal_String;

   ---------------
   -- Component --
   ---------------

   procedure Component
     (Self    : aliased Post_Reference;
      Name    : League.Strings.Universal_String;
      Value   : out League.Holders.Holder;
      Success : out Boolean)
   is
      Object : Post renames Self.Container.Post_Map (Self.Id);
   begin
      Success := True;

      if Name = +"subject" then
         Value := League.Holders.To_Holder (Object.Subject);
      elsif Name = +"para" then
         Value := Para_Iterable_Holders.To_Holder (Self);
      elsif Name = +"id" then
         Value := League.Holders.To_Holder (Object.Id);
      elsif Name = +"hash" then
         declare
            use type Ada.Streams.Stream_Element_Offset;
            Hash : League.Hash_Type := Object.Id.Hash;
            Raw  : Ada.Streams.Stream_Element_Array
              (1 .. Hash'Size / Ada.Streams.Stream_Element'Size)
              with Import, Address => Hash'Address;
            V : constant League.Stream_Element_Vectors.Stream_Element_Vector :=
              League.Stream_Element_Vectors.To_Stream_Element_Vector (Raw);
            Image : League.Strings.Universal_String :=
              League.Base_Codecs.To_Base_64 (V);
         begin
            while Image.Ends_With ("=") loop
               Image := Image.Head_To (Image.Length - 1);
            end loop;

            Value := League.Holders.To_Holder (Image);
         end;
      elsif Name = +"date" then
         declare
            Pattern : constant League.Strings.Universal_String :=
             +"yyyy-MM-dd HH:mm:ss";
            Image   : constant League.Strings.Universal_String :=
              League.Calendars.ISO_8601.Image (Pattern, Object.Date);
         begin
            Value := League.Holders.To_Holder (Image);
         end;
      elsif Name = +"date_iso" then
         declare
            Pattern : constant League.Strings.Universal_String :=
             +"yyyy-MM-ddTHH:mm:ss";
            Image   : constant League.Strings.Universal_String :=
              League.Calendars.ISO_8601.Image (Pattern, Object.Date);
         begin
            Value := League.Holders.To_Holder (Image & "Z");
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

   ---------------
   -- Component --
   ---------------

   procedure Component
     (Self    : aliased Paragraph;
      Name    : League.Strings.Universal_String;
      Value   : out League.Holders.Holder;
      Success : out Boolean) is
   begin
      Success := True;

      if Name = +"text" then
         declare
            Text : League.Strings.Universal_String := Self.Text;
         begin
            if Text.Is_Empty then
               Text.Append (League.Characters.Latin.No_Break_Space);
            end if;

            Value := League.Holders.To_Holder (Text);
         end;
      elsif Name = +"quote" then
         declare
            Image : Wide_Wide_String := Natural'Wide_Wide_Image (Self.Quote);
         begin
            Image (1) := 'q';
            Value := League.Holders.To_Holder (+Image);
         end;
      else
         Success := False;
      end if;
   end Component;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self : Para_Cursor) return League.Holders.Holder
   is
      Value : constant Paragraph := Paragraph_Lists.Element (Self.Cursor);
   begin
      return Para_Compound_Holders.To_Holder (Value);
   end Element;

   -----------
   -- First --
   -----------

   function First
     (Self : aliased Post_Reference) return Iterable.Cursor'Class is
   begin
      return Para_Cursor'(Self.Container,
                          Self.Id,
                          Paragraph_Lists.No_Element);
   end First;

   ----------
   -- Next --
   ----------

   overriding function Next (Self : in out Para_Cursor) return Boolean is
   begin
      if Paragraph_Lists.Has_Element (Self.Cursor) then
         Paragraph_Lists.Next (Self.Cursor);
      else
         declare
            Object : Post renames Self.Container.Post_Map (Self.Id);
         begin
            Self.Cursor := Object.Para.First;
         end;
      end if;

      return Paragraph_Lists.Has_Element (Self.Cursor);
   end Next;

end Forum.Posts.Holders;
