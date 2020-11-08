package Mission is
   function First_Word (Text : String) return String;
end Mission;

with Ada.Text_IO;

package body Mission is
   function First_Word (Text : String) return String is
   begin
      --  Put your code here
      return Text (1 .. 2);
   end First_Word;

begin
   Ada.Text_IO.Put ("Example:");
   Ada.Text_IO.Put_Line (First_Word ("Hello world"));

   -- These "asserts" are used for self-checking
   pragma Assert (First_Word ("Hello world") = "Hello");
   pragma Assert (First_Word ("a word") = "a");
   pragma Assert (First_Word ("hi") = "hi");

   Ada.Text_IO.Put_Line
     ("Coding complete? Click 'Check' to earn cool rewards!");
end Mission;
