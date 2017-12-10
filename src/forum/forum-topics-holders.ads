with League.Holders.Generic_Compound_Holders;
with League.Holders.Generic_Iterable_Holders;

package Forum.Topics.Holders is
   package Iterable renames League.Holders.Iterable_Holder_Cursors;

   Topic_Page_Size : constant := 20;

   type Topic_Reference is record
      Container : Container_Access;
      Starter   : League.Strings.Universal_String;
   end record;

   procedure Component
     (Self    : aliased Topic_Reference;
      Name    : League.Strings.Universal_String;
      Value   : out League.Holders.Holder;
      Success : out Boolean);

   package Compound_Holders is new League.Holders.Generic_Compound_Holders
     (Topic_Reference, Component);

   type Page_List is record
      Container : Container_Access;
      Starter   : League.Strings.Universal_String;
      Total     : Natural;
   end record;

   function First (Self : aliased Page_List) return Iterable.Cursor'Class;

   package Page_Iterable_Holders is
     new League.Holders.Generic_Iterable_Holders (Page_List, First);

   type Page_Cursor is new Iterable.Cursor with record
      Container : Container_Access;
      Starter   : League.Strings.Universal_String;
      Index     : Natural;
      Total     : Natural;
   end record;

   overriding function Next (Self : in out Page_Cursor) return Boolean;

   overriding function Element
     (Self : Page_Cursor) return League.Holders.Holder;

   type Page_Reference is record
      Container : Container_Access;
      Starter   : League.Strings.Universal_String;
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

   type Post_List is record
      Container : Container_Access;
      Starter   : League.Strings.Universal_String;
      From      : Positive;
      To        : Natural;
   end record;

   function First (Self : aliased Post_List) return Iterable.Cursor'Class;

   package Post_Iterable_Holders is
     new League.Holders.Generic_Iterable_Holders (Post_List, First);

   type Post_Cursor is new Iterable.Cursor with record
      Container : Container_Access;
      Starter   : League.Strings.Universal_String;
      From      : Positive;
      To        : Natural;
      Index     : Natural;
   end record;

   overriding function Next (Self : in out Post_Cursor) return Boolean;

   overriding function Element
     (Self : Post_Cursor) return League.Holders.Holder;

end Forum.Topics.Holders;
