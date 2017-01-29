package Axe.Ada_Scanners_Types is
   pragma Preelaborate;

   type State is mod +86;
   subtype Valid_State is State range 0 .. State'Last - 1;

   Allow_Char : constant State := 0;
   INITIAL : constant State := 83;

   type Character_Class is mod +35;

   type Rule_Index is range 0 .. 38;

end Axe.Ada_Scanners_Types;
