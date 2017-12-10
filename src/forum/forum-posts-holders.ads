with League.Holders.Generic_Compound_Holders;

package Forum.Posts.Holders is

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

end Forum.Posts.Holders;
