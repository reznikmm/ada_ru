with League.Holders;
with League.Holders.Generic_Integers;
with League.Strings;

package Forum is
   pragma Preelaborate;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   function To_String
     (Holder : League.Holders.Holder)
        return League.Strings.Universal_String;

   type Forum_Id is new Natural;
   package Forum_Id_Holders is new League.Holders.Generic_Integers (Forum_Id);

end Forum;
