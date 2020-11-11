package Mission is
   function Is_Acceptable_Password (Password : String) return Boolean;
end Mission;

with Ada.Text_IO;

package body Mission is
   function Is_Acceptable_Password (Password : String) return Boolean is
   begin
      --  Put your code here
      return True;
   end Is_Acceptable_Password;

begin
   Ada.Text_IO.Put ("Example:");
   Ada.Text_IO.Put_Line (Is_Acceptable_Password ("short")'Image);

   -- These "asserts" are used for self-checking
   pragma Assert (Is_Acceptable_Password ("short") = False);
   pragma Assert (Is_Acceptable_Password ("muchlonger") = True);
   pragma Assert (Is_Acceptable_Password ("ashort") = False);

   Ada.Text_IO.Put_Line
     ("Coding complete? Click 'Check' to earn cool rewards!");
end Mission;
