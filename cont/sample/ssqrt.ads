--  Sample of the parallel calculations in Ada.
--  Distributed under the GNU General Public License
--  Author: Dmitriy Anisimkov.

generic
   type Number is digits <>;
   type Vector_Type is Array (Positive range <>) of Number;
function SSqrt
  (Vector : in Vector_Type;
   Tasks  : in Positive) return Number;
--  Vector is the numbers for calculation.
--  Tasks is the number of parallel tasks for calculation.
--  Number of tasks should not be more than number of processors for
--  better performance.
