with League.Holders.Generic_Compound_Holders;
with League.Holders.Generic_Iterable_Holders;

package Forum.Posts.Holders is
   package Iterable renames League.Holders.Iterable_Holder_Cursors;

   type Post_Reference is record
      Container : Container_Access;
      Id        : League.Strings.Universal_String;
   end record;

   procedure Component
     (Self    : aliased Post_Reference;
      Name    : League.Strings.Universal_String;
      Value   : out League.Holders.Holder;
      Success : out Boolean);

   package Compound_Holders is new League.Holders.Generic_Compound_Holders
     (Post_Reference, Component);

   function First (Self : aliased Post_Reference) return Iterable.Cursor'Class;

   package Para_Iterable_Holders is
     new League.Holders.Generic_Iterable_Holders (Post_Reference, First);

   type Para_Cursor is new Iterable.Cursor with record
      Container : Container_Access;
      Id        : League.Strings.Universal_String;
      Cursor    : Paragraph_Lists.Cursor := Paragraph_Lists.No_Element;
   end record;

   overriding function Next (Self : in out Para_Cursor) return Boolean;

   overriding function Element
     (Self : Para_Cursor) return League.Holders.Holder;

   procedure Component
     (Self    : aliased Paragraph;
      Name    : League.Strings.Universal_String;
      Value   : out League.Holders.Holder;
      Success : out Boolean);

   package Para_Compound_Holders is new League.Holders.Generic_Compound_Holders
     (Paragraph, Component);

end Forum.Posts.Holders;
