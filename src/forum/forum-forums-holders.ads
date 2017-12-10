with League.Holders.Generic_Iterable_Holders;
with League.Holders.Generic_Compound_Holders;

private package Forum.Forums.Holders is

   package Iterable renames League.Holders.Iterable_Holder_Cursors;

   Forum_Page_Size : constant := 50;

   package Id_Vectors is new Ada.Containers.Vectors (Positive, Forum_Id);

   type Forum_List is record
      Container : Container_Access;
      Id_List   : Id_Vectors.Vector;
   end record;

   function First (Self : aliased Forum_List) return Iterable.Cursor'Class;

   package Iterable_Holders is new League.Holders.Generic_Iterable_Holders
     (Forum_List, First);

   type Forum_Cursor is new Iterable.Cursor with record
      Container : Container_Access;
      Position  : Id_Vectors.Cursor;
      First     : Boolean := True;
   end record;

   overriding function Next (Self : in out Forum_Cursor) return Boolean;

   overriding function Element
     (Self : Forum_Cursor) return League.Holders.Holder;

   type Forum_Reference is record
      Container : Container_Access;
      Id        : Forum_Id;
   end record;

   procedure Component
     (Self    : aliased Forum_Reference;
      Name    : League.Strings.Universal_String;
      Value   : out League.Holders.Holder;
      Success : out Boolean);

   package Compound_Holders is new League.Holders.Generic_Compound_Holders
     (Forum_Reference, Component);

   type Page_List is record
      Container : Container_Access;
      Id        : Forum_Id;
      Total     : Natural;
   end record;

   function First (Self : aliased Page_List) return Iterable.Cursor'Class;

   package Page_Iterable_Holders is
     new League.Holders.Generic_Iterable_Holders (Page_List, First);

   type Page_Cursor is new Iterable.Cursor with record
      Container : Container_Access;
      Id        : Forum_Id;
      Index     : Natural;
      Total     : Natural;
   end record;

   overriding function Next (Self : in out Page_Cursor) return Boolean;

   overriding function Element
     (Self : Page_Cursor) return League.Holders.Holder;

   type Page_Reference is record
      Container : Container_Access;
      Id        : Forum_Id;
      Index     : Positive;
      Total     : Natural;
   end record;

   procedure Component
     (Self    : aliased Page_Reference;
      Name    : League.Strings.Universal_String;
      Value   : out League.Holders.Holder;
      Success : out Boolean);

   package Page_Holders is new League.Holders.Generic_Compound_Holders
     (Page_Reference, Component);

   type Topic_List is record
      Container : Container_Access;
      Id        : Forum_Id;
      From      : Positive;
      To        : Positive;
   end record;

   function First (Self : aliased Topic_List) return Iterable.Cursor'Class;

   package Topic_Iterable_Holders is
     new League.Holders.Generic_Iterable_Holders (Topic_List, First);

   type Topic_Cursor is new Iterable.Cursor with record
      Container : Container_Access;
      Id        : Forum_Id;
      From      : Positive;
      To        : Positive;
      Index     : Natural;
   end record;

   overriding function Next (Self : in out Topic_Cursor) return Boolean;

   overriding function Element
     (Self : Topic_Cursor) return League.Holders.Holder;

end Forum.Forums.Holders;
