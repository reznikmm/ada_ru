with Ahven;
with Mission;

package body Mission_Tests is
   type Basics_Test is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (Self : in out Basics_Test);

   type Extra_Test is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (Self : in out Extra_Test);

   procedure Basics_1 is
   begin
      Ahven.Assert
        (Mission.Mult_Two (2, 3) = 6, "Incorrect result.");
   end Basics_1;

   procedure Basics_2 is
   begin
      Ahven.Assert
        (Mission.Mult_Two (0, 1) = 0, "Incorrect result.");
   end Basics_2;

   procedure Extra_1 is
   begin
      Ahven.Assert
        (Mission.Mult_Two (6, 3) = 18, "Incorrect result.");
   end Extra_1;

   procedure Extra_2 is
   begin
      Ahven.Assert
        (Mission.Mult_Two (6, 7) = 42, "Incorrect result.");
   end Extra_2;

   procedure Initialize (Self : in out Basics_Test) is
   begin
      Set_Name (Self, "Basics");
      Ahven.Framework.Add_Test_Routine (Self, Basics_1'Unrestricted_Access, "Mult_Two (2, 3)");
      Ahven.Framework.Add_Test_Routine (Self, Basics_2'Unrestricted_Access, "Mult_Two (0, 1)");
   end Initialize;

   procedure Initialize (Self : in out Extra_Test) is
   begin
      Set_Name (Self, "Extra");
      Ahven.Framework.Add_Test_Routine (Self,  Extra_1'Unrestricted_Access, "Mult_Two (6, 3)");
      Ahven.Framework.Add_Test_Routine (Self,  Extra_2'Unrestricted_Access, "Mult_Two (6, 7)");
   end Initialize;

   Basics     : Basics_Test;
   Extra      : Extra_Test;

   procedure Add_Tests (Test_Suite : in out Ahven.Framework.Test_Suite) is
   begin
      Test_Suite.Add_Static_Test (Basics);
      Test_Suite.Add_Static_Test (Extra);
   end Add_Tests;

end Mission_Tests;
