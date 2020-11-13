package Mission is
   function Backward_String (Value : String) return String;
end Mission;

with Ada.Text_IO;

package body Mission is
   function Backward_String (Value : String) return String is
   begin
      --  Put your code here
      return "";
   end Backward_String;

begin
   Ada.Text_IO.Put ("Example:");
   Ada.Text_IO.Put_Line (Backward_String ("val"));

   -- These "asserts" are used for self-checking
   pragma Assert (Backward_String ("val") = "lav");
   pragma Assert (Backward_String ("") = "");
   pragma Assert (Backward_String ("ohho") = "ohho");
   pragma Assert (Backward_String ("123456789") = "987654321");

   Ada.Text_IO.Put_Line
     ("Coding complete? Click 'Check' to earn cool rewards!");
end Mission;
