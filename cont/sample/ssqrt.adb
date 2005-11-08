--  Sample of the parallel calculations.
--  Distributed under the GNU General Public License
--  Author: Dmitriy Anisimkov.

with Ada.Numerics.Generic_Elementary_Functions;

function SSqrt
  (Vector : in Vector_Type;
   Tasks  : in Positive) return Number
is
   package GEL is new Ada.Numerics.Generic_Elementary_Functions (Number);
   --  Generic instantiation for numeric functions.

   function Square_Sum (First, Last : in Positive) return Number;
   --  Calculates square sum from the First to the Last elements in the
   --  vector.

   task type Calculator is
      entry Start (First, Last : in Positive);
      --  First and Last is a subrange for this task square sum calculation.

      entry Stop (Result : out Number);
      -- Get the result after calculation.
   end;

   ----------------
   -- Calculator --
   ----------------

   task body Calculator is
      First, Last : Positive;
      Sum : Number;
   begin
      accept Start (First, Last : Positive) do
         --  Copy parameters to the local variables for be able start
         --  calculation after rendezvous.

         Calculator.First := First;
         Calculator.Last  := Last;
      end Start;

      --  Calculation.

      Sum := Square_Sum (First, Last);

      accept Stop (Result : out Number) do
         --  Return result to the calling task.

         Result := Sum;
      end Stop;
   end Calculator;

   ----------------
   -- Square_Sum --
   ----------------

   function Square_Sum (First, Last : in Positive) return Number is
      Sum : Number := 0.0;
   begin
      for J in First .. Last loop
         Sum := Sum + Vector (J) ** 2;
      end loop;

      return Sum;
   end Square_Sum;

   Sub_Length : constant Positive := Vector'Length / Tasks;

   Calcs : array (1 .. Tasks - 1) of Calculator;
   --  Tasks - 1 is to rest some elements for the calculation in the main task.

   Index  : Positive := Vector'First;
   Next   : Positive;
   Sum    : Number;
   Subsum : Number;

begin
   --  Dispatch subvectors to the parallel tasks.

   for J in Calcs'Range loop
      Next := Index + Sub_Length;
      Calcs (J).Start (First => Index, Last => Next - 1);
      Index := Next;
   end loop;

   --  Calculate square sum of the rest elements in the main task.

   Sum := Square_Sum (Index, Vector'Last);

   --  Get results from the parallel tasks.

   for J in Calcs'Range loop
      Calcs (J).Stop (Subsum);
      Sum := Sum + Subsum;
   end loop;

   --  Calculate square root and return result.

   return GEL.Sqrt (Sum);

end SSqrt;
