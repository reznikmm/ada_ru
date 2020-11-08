package Mission is
   type Integer_Array is array (Positive range <>) of Integer;
   function Easy_Unpack (Elements : Integer_Array) return Integer_Array;
end Mission;

with Ada.Text_IO;

package body Mission is
   function Easy_Unpack (Elements : Integer_Array) return Integer_Array is
   begin
      --  Put your code here
      return (1 .. 0 => <>);
   end Easy_Unpack;

begin
   Ada.Text_IO.Put ("Example:");
   Ada.Text_IO.Put_Line (Easy_Unpack ((1, 2, 3, 4, 5, 6, 7, 9))'Image);

   -- These "asserts" are used for self-checking
   pragma Assert (Easy_Unpack ((1, 2, 3, 4, 5, 6, 7, 9)) = (1, 3, 7));
   pragma Assert (Easy_Unpack ((1, 1, 1, 1)) = (1, 1, 1));
   pragma Assert (Easy_Unpack ((6, 3, 7)) = (6, 7, 3));

   Ada.Text_IO.Put_Line
     ("Coding complete? Click 'Check' to earn cool rewards!");
end Mission;
