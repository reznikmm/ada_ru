with Forum.Posts.Holders;
with Forum.Contexts; pragma Unreferenced (Forum.Contexts);

package body Forum.Topics.Holders is

   ---------------
   -- Component --
   ---------------

   procedure Component
     (Self    : aliased Topic_Reference;
      Name    : League.Strings.Universal_String;
      Value   : out League.Holders.Holder;
      Success : out Boolean)
   is
      use type League.Strings.Universal_String;
      Object : Topic renames Self.Container.Topic_Map (Self.Starter);
   begin
      Success := True;

      if Name = +"subject" then
         Value := League.Holders.To_Holder (Object.Subject);
      elsif Name = +"starter" then
         declare
            package PH renames Forum.Posts.Holders;
            Post : constant PH.Post_Reference :=
              (Self.Container.Context.Posts'Access,
               Self.Starter);
         begin
            Value := PH.Compound_Holders.To_Holder (Post);
         end;
      elsif Name = +"id" then
         declare
            Hash  : constant Ada.Containers.Hash_Type :=
              League.Strings.Hash (Self.Starter);
            Image : constant Wide_Wide_String :=
              Ada.Containers.Hash_Type'Wide_Wide_Image (Hash);
         begin
            Value := League.Holders.To_Holder (+Image (2 .. Image'Last));
         end;
      elsif Name = +"posts" then
         declare
            Image : constant Wide_Wide_String :=
              Natural'Wide_Wide_Image (Object.Posts.Last_Index);
         begin
            Value := League.Holders.To_Holder (+Image (2 .. Image'Last));
         end;
      elsif Name = +"pages" then
         declare
            Length : constant Natural :=
              (Object.Posts.Last_Index + Topic_Page_Size)
                / Topic_Page_Size;
            Pages : constant Page_List :=
              (Self.Container, Self.Starter, Length);
         begin
            Value := Page_Iterable_Holders.To_Holder (Pages);
         end;
      else
         Success := False;
      end if;
   end Component;

   procedure Component
     (Self    : aliased Page_Reference;
      Name    : League.Strings.Universal_String;
      Value   : out League.Holders.Holder;
      Success : out Boolean)
   is
      use type League.Strings.Universal_String;
   begin
      Success := True;

      if Name = +"index" then
         declare
            Image : constant Wide_Wide_String :=
              Positive'Wide_Wide_Image (Self.Index);
         begin
            Value := League.Holders.To_Holder (+Image (2 .. Image'Last));
         end;
      elsif Name = +"prev" then
         if Self.Index = 1 then
            League.Holders.Clear (Value);
         else
            declare
               Page : Page_Reference := Self;
            begin
               Page.Index := Page.Index - 1;
               Value := Page_Holders.To_Holder (Page);
            end;
         end if;
      elsif Name = +"posts" then
         declare
            Object : Topic renames Self.Container.Topic_Map (Self.Starter);
            From : constant Positive := (Self.Index - 1) * Topic_Page_Size + 1;
            To   : constant Natural := Natural'Min
              (From + Topic_Page_Size - 1, Object.Posts.Last_Index);
            Result : constant Post_List :=
              (Self.Container, Self.Starter, From, To);
         begin
            Value := Post_Iterable_Holders.To_Holder (Result);
         end;
      end if;
   end Component;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self : Page_Cursor) return League.Holders.Holder
   is
      Page : constant Page_Reference :=
        (Self.Container, Self.Starter, Self.Index, Self.Total);
   begin
      return Page_Holders.To_Holder (Page);
   end Element;

   overriding function Element
     (Self : Post_Cursor) return League.Holders.Holder
   is
      package PH renames Forum.Posts.Holders;
      Object : Topic renames Self.Container.Topic_Map (Self.Starter);
      Post : constant PH.Post_Reference :=
        (Self.Container.Context.Posts'Access, Object.Posts (Self.Index));
   begin
      return PH.Compound_Holders.To_Holder (Post);
   end Element;

   -----------
   -- First --
   -----------

   function First (Self : aliased Page_List) return Iterable.Cursor'Class is
   begin
      return Page_Cursor'(Self.Container, Self.Starter, 0, Self.Total);
   end First;

   function First (Self : aliased Post_List) return Iterable.Cursor'Class is
   begin
      return Post_Cursor'
        (Self.Container, Self.Starter, Self.From, Self.To, Self.From - 1);
   end First;

   ----------
   -- Next --
   ----------

   overriding function Next (Self : in out Page_Cursor) return Boolean is
   begin
      Self.Index := Self.Index + 1;
      return Self.Index <= Self.Total;
   end Next;

   overriding function Next (Self : in out Post_Cursor) return Boolean is
   begin
      Self.Index := Self.Index + 1;
      return Self.Index <= Self.To;
   end Next;

end Forum.Topics.Holders;
