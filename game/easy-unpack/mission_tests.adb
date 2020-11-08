with Ahven;
with Mission;

package body Mission_Tests is
   type Basics_Test is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (Self : in out Basics_Test);

   type Extra_Test is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (Self : in out Extra_Test);

   use type Mission.Integer_Array;

   procedure Basics_1 is
   begin
      Ahven.Assert
        (Mission.Easy_Unpack ((1, 2, 3, 4, 5, 6, 7, 9)) = (1, 3, 7),
         "Incorrect result.");
   end Basics_1;

   procedure Basics_2 is
   begin
      Ahven.Assert
        (Mission.Easy_Unpack ((1, 1, 1, 1)) = (1, 1, 1),
         "Incorrect result.");
   end Basics_2;

   procedure Basics_3 is
   begin
      Ahven.Assert
        (Mission.Easy_Unpack ((6, 3, 7)) = (6, 7, 3),
         "Incorrect result.");
   end Basics_3;

   procedure Extra_1 is
   begin
      Ahven.Assert
        (Mission.Easy_Unpack ((30, 40, 100)) = (30, 100, 40),
         "Incorrect result.");
   end Extra_1;

   procedure Extra_2 is
   begin
      Ahven.Assert
        (Mission.Easy_Unpack ((5, 5, 5, 5, 5, 5)) = (5, 5, 5),
         "Incorrect result.");
   end Extra_2;

   procedure Initialize (Self : in out Basics_Test) is
   begin
      Set_Name (Self, "Basics");
      Ahven.Framework.Add_Test_Routine (Self, Basics_1'Unrestricted_Access, "Easy_Unpack ((1, 2, 3, 4, 5, 6, 7, 9)");
      Ahven.Framework.Add_Test_Routine (Self, Basics_2'Unrestricted_Access, "Easy_Unpack ((1, 1, 1, 1))");
      Ahven.Framework.Add_Test_Routine (Self, Basics_2'Unrestricted_Access, "Easy_Unpack ((6, 3, 7))");
   end Initialize;

   procedure Initialize (Self : in out Extra_Test) is
   begin
      Set_Name (Self, "Extra");
      Ahven.Framework.Add_Test_Routine (Self,  Extra_1'Unrestricted_Access, "Easy_Unpack ((30, 40, 100))");
      Ahven.Framework.Add_Test_Routine (Self,  Extra_2'Unrestricted_Access, "Easy_Unpack ((5, 5, 5, 5, 5, 5))");
   end Initialize;

   Basics     : Basics_Test;
   Extra      : Extra_Test;

   procedure Add_Tests (Test_Suite : in out Ahven.Framework.Test_Suite) is
   begin
      Test_Suite.Add_Static_Test (Basics);
      Test_Suite.Add_Static_Test (Extra);
   end Add_Tests;

end Mission_Tests;
