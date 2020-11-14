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
        (Mission.Remove_All_Before ((1,2,3,4,5), 3) = (3,4,5),
         "Incorrect result.");
   end Basics_1;

   procedure Basics_2 is
   begin
      Ahven.Assert
        (Mission.Remove_All_Before ((1,1,2,2,3,3), 2) = (2,2,3,3),
         "Incorrect result.");
   end Basics_2;

   procedure Basics_3 is
   begin
      Ahven.Assert
        (Mission.Remove_All_Before ((1,1,2,4,2,3,4), 2) = (2,4,2,3,4),
         "Incorrect result.");
   end Basics_3;

   procedure Basics_4 is
   begin
      Ahven.Assert
        (Mission.Remove_All_Before ((1,1,5,6,7), 2) = (1,1,5,6,7),
         "Nothing should be removed if nothing is found.");
   end Basics_4;

   procedure Basics_5 is
   begin
      Ahven.Assert
        (Mission.Remove_All_Before ((1 .. 0 => 0), 0) = (1 .. 0 => 0),
         "Empty list stays unchanged.");
   end Basics_5;

   procedure Basics_6 is
   begin
      Ahven.Assert
        (Mission.Remove_All_Before ((7,7,7,7,7,7,7,7,7), 7) = (7,7,7,7,7,7,7,7,7),
         "Incorrect result.");
   end Basics_6;

   procedure Extra_1 is
   begin
      Ahven.Assert
        (Mission.Remove_All_Before ((10, 1, 5, 6, 7, 10), 5) = (5, 6, 7, 10),
         "Incorrect result.");
   end Extra_1;

   procedure Extra_2 is
   begin
      Ahven.Assert
        (Mission.Remove_All_Before ((1,2, 6,7,1,2,4,6,7,8,3,5,2,3), 6) = (6,7,1,2,4,6,7,8,3,5,2,3),
         "Incorrect result.");
   end Extra_2;

   procedure Initialize (Self : in out Basics_Test) is
   begin
      Set_Name (Self, "Basics");
      Ahven.Framework.Add_Test_Routine (Self, Basics_1'Unrestricted_Access, "Remove_All_Before ((1,2,3,4,5), 3");
      Ahven.Framework.Add_Test_Routine (Self, Basics_2'Unrestricted_Access, "Remove_All_Before ((1,1,2,2,3,3), 2)");
      Ahven.Framework.Add_Test_Routine (Self, Basics_3'Unrestricted_Access, "Remove_All_Before ((1,1,2,4,2,3,4), 2)");
      Ahven.Framework.Add_Test_Routine (Self, Basics_4'Unrestricted_Access, "Remove_All_Before ((1,1,5,6,7), 2");
      Ahven.Framework.Add_Test_Routine (Self, Basics_5'Unrestricted_Access, "Remove_All_Before ((1 .. 0 => 0), 0)");
      Ahven.Framework.Add_Test_Routine (Self, Basics_6'Unrestricted_Access, "Remove_All_Before ((7,7,7,7,7,7,7,7,7), 7)");
   end Initialize;

   procedure Initialize (Self : in out Extra_Test) is
   begin
      Set_Name (Self, "Extra");
      Ahven.Framework.Add_Test_Routine (Self,  Extra_1'Unrestricted_Access, "Remove_All_Before ((10, 1, 5, 6, 7, 10), 5)");
      Ahven.Framework.Add_Test_Routine (Self,  Extra_2'Unrestricted_Access, "Remove_All_Before ((1,2, 6,7,1,2,4,6,7,8,3,5,2,3), 6)");
   end Initialize;

   Basics     : Basics_Test;
   Extra      : Extra_Test;

   procedure Add_Tests (Test_Suite : in out Ahven.Framework.Test_Suite) is
   begin
      Test_Suite.Add_Static_Test (Basics);
      Test_Suite.Add_Static_Test (Extra);
   end Add_Tests;

end Mission_Tests;
