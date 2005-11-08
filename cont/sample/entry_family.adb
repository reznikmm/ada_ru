--  Family of entries usage example.
--  Author: Dmitriy Anisimkov.
--  License: GPL

with Ada.Text_IO;

procedure Entry_Family is

   task type Task_Type (Length : Positive) is
      entry Call (Positive range 1 .. Length) (Data : String);
   end Task_Type;

   task body Task_Type is
   begin
      for J in 1 .. Length loop
         accept Call (J) (Data : String) do
            Ada.Text_IO.Put_Line (Data);
         end Call;

         delay 0.125;
      end loop;
   end Task_Type;

   Task_Object : Task_Type (8);

begin
   for J in reverse 1 .. Task_Object.Length loop
      select
         Task_Object.Call (J) (Integer'Image (J));
      or delay 0.25;
         Ada.Text_IO.Put_Line (Integer'Image (J) & " Timeout");
      end select;
   end loop;

   for J in 2 .. Task_Object.Length loop
      Task_Object.Call (J) (Integer'Image (J));
   end loop;
end Entry_Family;
