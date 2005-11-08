with Ada.Text_IO;
with SSqrt;

procedure SSqrt_Test is
   subtype Number is Long_Float;
   type Vector_Type is array (Positive range <>) of Number;

   function Square_Sum_Sqrt is new SSqrt (Number, Vector_Type);

   Sample   : Vector_Type (1 .. 200);
   Test_Sum : Number := 0.0;
begin
   for J in Sample'Range loop
      Sample (J) := Number (J);
      Test_Sum := Test_Sum + Sample (J) ** 2;
   end loop;

   Ada.Text_IO.Put_Line
     (Number'Image (Square_Sum_Sqrt (Sample, 1))
      & Number'Image (Square_Sum_Sqrt (Sample, 2))
      & Number'Image (Square_Sum_Sqrt (Sample, 3))
      & ASCII.LF
      & Number'Image (Square_Sum_Sqrt (Sample, 4) ** 2)
      & Number'Image (Test_Sum));
end SSqrt_Test;
