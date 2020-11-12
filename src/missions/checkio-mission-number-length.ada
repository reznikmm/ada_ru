package Mission is
   function Number_Length (A : Natural) return Positive;
end Mission;

with Ada.Text_IO;

package body Mission is
   function Number_Length (A : Natural) return Positive is
   begin
      --  Put your code here
      return 10;
   end Number_Length;

begin
   Ada.Text_IO.Put ("Example:");
   Ada.Text_IO.Put_Line (Number_Length (10)'Image);

   -- These "asserts" are used for self-checking
   pragma Assert (Number_Length (10) = 2);
   pragma Assert (Number_Length (0) = 1);
   pragma Assert (Number_Length (4) = 1);
   pragma Assert (Number_Length (44) = 2);

   Ada.Text_IO.Put_Line
     ("Coding complete? Click 'Check' to earn cool rewards!");
end Mission;
