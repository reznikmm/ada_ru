with League.Strings;
package Axe.Ada_Scanners_Tokens is
   pragma Preelaborate;

   type Token is record
      Class : League.Strings.Universal_String;
      Text  : League.Strings.Universal_String;
   end record;

   function End_Of_Input return Token is
     (Class => League.Strings.Empty_Universal_String,
      Text  => League.Strings.Empty_Universal_String);

   subtype Token_Kind is Token;

end Axe.Ada_Scanners_Tokens;
