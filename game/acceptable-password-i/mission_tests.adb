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
        (not Mission.Is_Acceptable_Password ("short"),
         "Incorrect result.");
   end Basics_1;

   procedure Basics_2 is
   begin
      Ahven.Assert
        (Mission.Is_Acceptable_Password ("muchlonger"),
         "Incorrect result.");
   end Basics_2;

   procedure Basics_3 is
   begin
      Ahven.Assert
        (not Mission.Is_Acceptable_Password ("ashort"),
         "Incorrect result.");
   end Basics_3;

   procedure Extra_1 is
   begin
      Ahven.Assert
        (Mission.Is_Acceptable_Password ("this is password"),
         "Incorrect result.");
   end Extra_1;

   procedure Initialize (Self : in out Basics_Test) is
   begin
      Set_Name (Self, "Basics");
      Ahven.Framework.Add_Test_Routine (Self, Basics_1'Unrestricted_Access, "Is_Acceptable_Password (""short"")");
      Ahven.Framework.Add_Test_Routine (Self, Basics_2'Unrestricted_Access, "Is_Acceptable_Password (""muchlonger"")");
      Ahven.Framework.Add_Test_Routine (Self, Basics_3'Unrestricted_Access, "Is_Acceptable_Password (""ashort"")");
   end Initialize;

   procedure Initialize (Self : in out Extra_Test) is
   begin
      Set_Name (Self, "Extra");
      Ahven.Framework.Add_Test_Routine (Self,  Extra_1'Unrestricted_Access, "Is_Acceptable_Password (""this is password"")");
   end Initialize;

   Basics     : Basics_Test;
   Extra      : Extra_Test;

   procedure Add_Tests (Test_Suite : in out Ahven.Framework.Test_Suite) is
   begin
      Test_Suite.Add_Static_Test (Basics);
      Test_Suite.Add_Static_Test (Extra);
   end Add_Tests;

end Mission_Tests;
