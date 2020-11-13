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
      Ahven.Assert (Mission.Backward_String ("val") = "lav", Failure);
   end Basics_1;

   procedure Basics_2 is
   begin
      Ahven.Assert (Mission.Backward_String ("") = "", Failure);
   end Basics_2;

   procedure Basics_3 is
   begin
      Ahven.Assert (Mission.Backward_String ("ohho") = "ohho", Failure);
   end Basics_3;

   procedure Basics_4 is
   begin
      Ahven.Assert (Mission.Backward_String ("123456789") = "987654321", Failure);
   end Basics_4;

   procedure Extra_1 is
   begin
      Ahven.Assert (Mission.Backward_String ("aa") = "aa", Failure);
   end Extra_1;

   procedure Extra_2 is
   begin
      Ahven.Assert (Mission.Backward_String ("1,2,3,4") = "4,3,2,1", Failure);
   end Extra_2;

   procedure Extra_3 is
   begin
      Ahven.Assert (Mission.Backward_String ("Welcome to CheckiO") = "OikcehC ot emocleW", Failure);
   end Extra_3;

   procedure Initialize (Self : in out Basics_Test) is
   begin
      Set_Name (Self, "Basics");
      Ahven.Framework.Add_Test_Routine (Self, Basics_1'Unrestricted_Access, "Backward_String (""val"")");
      Ahven.Framework.Add_Test_Routine (Self, Basics_2'Unrestricted_Access, "Backward_String ("""")");
      Ahven.Framework.Add_Test_Routine (Self, Basics_3'Unrestricted_Access, "Backward_String (""ohho"")");
      Ahven.Framework.Add_Test_Routine (Self, Basics_4'Unrestricted_Access, "Backward_String (""123456789"")");
   end Initialize;

   procedure Initialize (Self : in out Extra_Test) is
   begin
      Set_Name (Self, "Extra");
      Ahven.Framework.Add_Test_Routine (Self,  Extra_1'Unrestricted_Access, "Backward_String (""aa"")");
      Ahven.Framework.Add_Test_Routine (Self,  Extra_2'Unrestricted_Access, "Backward_String (""1,2,3,4"")");
      Ahven.Framework.Add_Test_Routine (Self,  Extra_3'Unrestricted_Access, "Backward_String (""Welcome to CheckiO"")");
   end Initialize;

   Basics     : Basics_Test;
   Extra      : Extra_Test;

   procedure Add_Tests (Test_Suite : in out Ahven.Framework.Test_Suite) is
   begin
      Test_Suite.Add_Static_Test (Basics);
      Test_Suite.Add_Static_Test (Extra);
   end Add_Tests;

end Mission_Tests;
