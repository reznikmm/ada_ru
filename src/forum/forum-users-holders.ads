with League.Holders.Generic_Compound_Holders;

package Forum.Users.Holders is

   type User_Reference is record
      Container : Container_Access;
      Nickname  : League.Strings.Universal_String;
   end record;

   procedure Component
     (Self    : aliased User_Reference;
      Name    : League.Strings.Universal_String;
      Value   : out League.Holders.Holder;
      Success : out Boolean);

   package Compound_Holders is new League.Holders.Generic_Compound_Holders
     (User_Reference, Component);

end Forum.Users.Holders;
