package Mission is
   function Is_All_Upper (Text : String) return Boolean;
end Mission;

with Ada.Text_IO;

package body Mission is
   function Is_All_Upper (Text : String) return Boolean is
   begin
      --  Put your code here
      return False;
   end Is_All_Upper;

begin
   Ada.Text_IO.Put ("Example:");
   Ada.Text_IO.Put_Line (Is_All_Upper ("ALL UPPER")'Image);

   -- These "asserts" are used for self-checking
   pragma Assert (Is_All_Upper ("ALL UPPER"));
   pragma Assert (Is_All_Upper ("all lower") = False);
   pragma Assert (Is_All_Upper ("mixed UPPER and lower") = False);
   pragma Assert (Is_All_Upper (""));

   Ada.Text_IO.Put_Line
     ("Coding complete? Click 'Check' to earn cool rewards!");
end Mission;
