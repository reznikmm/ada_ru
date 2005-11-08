--  Asynchronous Transfer of Control example.
--  Author: Dmitriy Anisimkov.
--  License: GPL
--
--  Use -gnatP switch in GNAT for Win32.

with Ada.Text_IO;
with System;

procedure Asynch_Transfer is
   type Modular_Type is mod 2 ** System.Word_Size;

   N : Modular_Type := 0;
   pragma Atomic (N);
begin
   select
      delay 1.0; -- Could be entry call.
      Ada.Text_IO.Put_Line (Modular_Type'Image (N));

   then abort
      loop
         N := N + 1;
      end loop;
   end select;

   Ada.Text_IO.Put_Line ("Done.");
end Asynch_Transfer;
