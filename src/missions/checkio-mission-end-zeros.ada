package Mission is
   function End_Zeros (A : Natural) return Natural;
end Mission;

with Ada.Text_IO;

package body Mission is
   function End_Zeros (A : Natural) return Natural is
   begin
      --  Put your code here
      return 0;
   end End_Zeros;

begin
   Ada.Text_IO.Put ("Example:");
   Ada.Text_IO.Put_Line (End_Zeros (0)'Image);

   -- These "asserts" are used for self-checking
   pragma Assert (End_Zeros (0) = 1);
   pragma Assert (End_Zeros (1) = 0);
   pragma Assert (End_Zeros (10) = 1);
   pragma Assert (End_Zeros (101) = 0);
   pragma Assert (End_Zeros (245) = 0);
   pragma Assert (End_Zeros (100100) = 2);

   Ada.Text_IO.Put_Line
     ("Coding complete? Click 'Check' to earn cool rewards!");
end Mission;