package Mission is
   type Integer_Array is array (Positive range <>) of Integer;

   function Remove_All_Before
     (Items  : Integer_Array;
      Border : Integer) return Integer_Array;
end Mission;

with Ada.Text_IO;

package body Mission is
   function Remove_All_Before
     (Items  : Integer_Array;
      Border : Integer) return Integer_Array is
   begin
      --  Put your code here
      return Items;
   end Remove_All_Before;

begin
   Ada.Text_IO.Put ("Example:");
   Ada.Text_IO.Put_Line (Remove_All_Before ((1, 2, 3, 4, 5), 3)'Image);

   -- These "asserts" are used for self-checking
   pragma Assert (Remove_All_Before ((1, 2, 3, 4, 5), 3) = (3, 4, 5));
   pragma Assert (Remove_All_Before ((1, 1, 2, 2, 3, 3), 2) = (2, 2, 3, 3));
   pragma Assert (Remove_All_Before ((1, 1, 2, 4, 2, 3, 4), 2) = (2, 4, 2, 3, 4));
   pragma Assert (Remove_All_Before ((1, 1, 5, 6, 7), 2) = (1, 1, 5, 6, 7));
   pragma Assert (Remove_All_Before ((1 .. 0 => <>), 0 ) = (1 .. 0 => <>));
   pragma Assert (Remove_All_Before ((7, 7, 7), 7) = (7, 7, 7));

   Ada.Text_IO.Put_Line
     ("Coding complete? Click 'Check' to earn cool rewards!");
end Mission;
