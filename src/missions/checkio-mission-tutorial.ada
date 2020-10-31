package Mission is
   function Mult_Two (A, B : Integer) return Integer;
end Mission;

with Ada.Text_IO;

package body Mission is
   function Mult_Two (A, B : Integer) return Integer is
   begin
      --  Put your code here
      return A - B;
   end Mult_Two;

begin
   Ada.Text_IO.Put_Line ("Example:");
   Ada.Text_IO.Put_Line (Mult_Two (3, 2)'Image);

   -- These "asserts" are used for self-checking
   pragma Assert (Mult_Two (3, 2) = 6);
   pragma Assert (Mult_Two (0, 1) = 0);

   Ada.Text_IO.Put_Line
     ("Coding complete? Click 'Check' to earn cool rewards!");
end Mission;
