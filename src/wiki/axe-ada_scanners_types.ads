package Axe.Ada_Scanners_Types is
   pragma Preelaborate;

   type State is mod +87;
   subtype Looping_State is State range 0 .. 63;
   subtype Final_State is State range 36 .. State'Last - 1;

   Error_State : constant State := State'Last;

   Allow_Char : constant State := 0;
   INITIAL : constant State := 35;

   type Character_Class is mod +36;

   type Rule_Index is range 0 .. 39;

end Axe.Ada_Scanners_Types;
