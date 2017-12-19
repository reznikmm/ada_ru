with Forum.Topics.Holders;
with Forum.Contexts; pragma Unreferenced (Forum.Contexts);

package body Forum.Forums.Holders is

   ---------------
   -- Component --
   ---------------

   procedure Component
     (Self    : aliased Forum_Reference;
      Name    : League.Strings.Universal_String;
      Value   : out League.Holders.Holder;
      Success : out Boolean)
   is
      use type League.Strings.Universal_String;
      Object : Forum renames Self.Container.Forum_Map (Self.Id);
   begin
      Success := True;

      if Name = +"subject" then
         Value := League.Holders.To_Holder (Object.Subject);
      elsif Name = +"id" then
         declare
            Image : constant Wide_Wide_String :=
              Forum_Id'Wide_Wide_Image (Object.Id);
         begin
            Value := League.Holders.To_Holder (+Image (2 .. Image'Last));
         end;
      elsif Name = +"pages" then
         declare
            Length : constant Natural :=
              (Object.Topics.Last_Index + Forum_Page_Size)
              / Forum_Page_Size;
            Pages : constant Page_List := (Self.Container, Self.Id, Length);
         begin
            Value := Page_Iterable_Holders.To_Holder (Pages);
         end;
      elsif Name = +"topics" then
         declare
            Image : constant Wide_Wide_String :=
              Natural'Wide_Wide_Image (Object.Topics.Last_Index);
         begin
            Value := League.Holders.To_Holder (+Image (2 .. Image'Last));
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
      elsif Name = +"topics" then
         declare
            Object : Forum renames Self.Container.Forum_Map (Self.Id);
            From : constant Positive := (Self.Index - 1) * Forum_Page_Size + 1;
            To   : constant Positive := Positive'Min
              (From + Forum_Page_Size - 1, Object.Topics.Last_Index);
            Result : constant Topic_List :=
              (Self.Container, Self.Id, From, To);
         begin
            Value := Topic_Iterable_Holders.To_Holder (Result);
         end;
      end if;
   end Component;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self : Forum_Cursor) return League.Holders.Holder
   is
      Reference : constant Forum_Reference :=
        (Self.Container, Id_Vectors.Element (Self.Position));
   begin
      return Compound_Holders.To_Holder (Reference);
   end Element;

   overriding function Element
     (Self : Page_Cursor) return League.Holders.Holder
   is
      Page : constant Page_Reference :=
        (Self.Container, Self.Id, Self.Index, Self.Total);
   begin
      return Page_Holders.To_Holder (Page);
   end Element;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self : Topic_Cursor) return League.Holders.Holder
   is
      package TH renames Standard.Forum.Topics.Holders;
      Object : Forum renames Self.Container.Forum_Map (Self.Id);
      Topic : constant TH.Topic_Reference :=
        (Self.Container.Context.Topics'Access, Object.Topics (Self.Index));
   begin
      return TH.Compound_Holders.To_Holder (Topic);
   end Element;

   -----------
   -- First --
   -----------

   function First
     (Self : aliased Forum_List) return Iterable.Cursor'Class is
   begin
      return Forum_Cursor'(Self.Container, Self.Id_List.First, True);
   end First;

   function First (Self : aliased Page_List) return Iterable.Cursor'Class is
   begin
      return Page_Cursor'(Self.Container, Self.Id, 0, Self.Total);
   end First;

   function First (Self : aliased Topic_List) return Iterable.Cursor'Class is
   begin
      return Topic_Cursor'
        (Self.Container, Self.Id, Self.From, Self.To, Self.From - 1);
   end First;

   ----------
   -- Next --
   ----------

   overriding function Next (Self : in out Forum_Cursor) return Boolean is
   begin
      if Self.First then
         Self.First := False;
      else
         Id_Vectors.Next (Self.Position);
      end if;

      return Id_Vectors.Has_Element (Self.Position);
   end Next;

   overriding function Next (Self : in out Page_Cursor) return Boolean is
   begin
      Self.Index := Self.Index + 1;
      return Self.Index <= Self.Total;
   end Next;

   overriding function Next (Self : in out Topic_Cursor) return Boolean is
   begin
      Self.Index := Self.Index + 1;
      return Self.Index <= Self.To;
   end Next;

end Forum.Forums.Holders;
