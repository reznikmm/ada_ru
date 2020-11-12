with Ahven;
with Mission;

package body Mission_Tests is
   type Basics_Test is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (Self : in out Basics_Test);

   type Extra_Test is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (Self : in out Extra_Test);

   Failure : constant String := "Incorrect result.";

   procedure Basics_1 is
   begin
      Ahven.Assert (Mission.End_Zeros (0) = 1, Failure);
   end Basics_1;

   procedure Basics_2 is
   begin
      Ahven.Assert (Mission.End_Zeros (1) = 0, Failure);
   end Basics_2;

   procedure Basics_3 is
   begin
      Ahven.Assert (Mission.End_Zeros (10) = 1, Failure);
   end Basics_3;

   procedure Basics_4 is
   begin
      Ahven.Assert (Mission.End_Zeros (101) = 0, Failure);
   end Basics_4;

   procedure Basics_5 is
   begin
      Ahven.Assert (Mission.End_Zeros (245) = 0, Failure);
   end Basics_5;

   procedure Basics_6 is
   begin
      Ahven.Assert (Mission.End_Zeros (100100) = 2, Failure);
   end Basics_6;

   procedure Extra_1 is
   begin
      Ahven.Assert (Mission.End_Zeros (3456) = 0, Failure);
   end Extra_1;

   procedure Extra_2 is
   begin
      Ahven.Assert (Mission.End_Zeros (100234) = 0, Failure);
   end Extra_2;

   procedure Initialize (Self : in out Basics_Test) is
   begin
      Set_Name (Self, "Basics");
      Ahven.Framework.Add_Test_Routine (Self, Basics_1'Unrestricted_Access, "End_Zeros (0)");
      Ahven.Framework.Add_Test_Routine (Self, Basics_2'Unrestricted_Access, "End_Zeros (1)");
      Ahven.Framework.Add_Test_Routine (Self, Basics_3'Unrestricted_Access, "End_Zeros (10)");
      Ahven.Framework.Add_Test_Routine (Self, Basics_4'Unrestricted_Access, "End_Zeros (101)");
      Ahven.Framework.Add_Test_Routine (Self, Basics_5'Unrestricted_Access, "End_Zeros (245)");
      Ahven.Framework.Add_Test_Routine (Self, Basics_6'Unrestricted_Access, "End_Zeros (100100)");
   end Initialize;

   procedure Initialize (Self : in out Extra_Test) is
   begin
      Set_Name (Self, "Extra");
      Ahven.Framework.Add_Test_Routine (Self,  Extra_1'Unrestricted_Access, "End_Zeros (3456)");
      Ahven.Framework.Add_Test_Routine (Self,  Extra_2'Unrestricted_Access, "End_Zeros (100234)");
   end Initialize;

   Basics     : Basics_Test;
   Extra      : Extra_Test;

   procedure Add_Tests (Test_Suite : in out Ahven.Framework.Test_Suite) is
   begin
      Test_Suite.Add_Static_Test (Basics);
      Test_Suite.Add_Static_Test (Extra);
   end Add_Tests;

end Mission_Tests;
