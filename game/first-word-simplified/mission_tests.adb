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
        (Mission.First_Word ("Hello world") = "Hello",
         "Incorrect result.");
   end Basics_1;

   procedure Basics_2 is
   begin
      Ahven.Assert
        (Mission.First_Word ("a word") = "a",
         "Incorrect result.");
   end Basics_2;

   procedure Basics_3 is
   begin
      Ahven.Assert
        (Mission.First_Word ("greeting from CheckiO Planet") = "greeting",
         "Incorrect result.");
   end Basics_3;

   procedure Basics_4 is
   begin
      Ahven.Assert
        (Mission.First_Word ("hi") = "hi",
         "Incorrect result.");
   end Basics_4;

   procedure Extra_1 is
   begin
      Ahven.Assert
        (Mission.First_Word ("Holy Edison") = "Holy",
         "Incorrect result.");
   end Extra_1;

   procedure Initialize (Self : in out Basics_Test) is
   begin
      Set_Name (Self, "Basics");
      Ahven.Framework.Add_Test_Routine (Self, Basics_1'Unrestricted_Access, "First_Word (""Hello world"")");
      Ahven.Framework.Add_Test_Routine (Self, Basics_2'Unrestricted_Access, "First_Word (""a word"")");
      Ahven.Framework.Add_Test_Routine (Self, Basics_3'Unrestricted_Access, "First_Word (""greeting from CheckiO Planet"")");
      Ahven.Framework.Add_Test_Routine (Self, Basics_4'Unrestricted_Access, "First_Word (""hi"")");
   end Initialize;

   procedure Initialize (Self : in out Extra_Test) is
   begin
      Set_Name (Self, "Extra");
      Ahven.Framework.Add_Test_Routine (Self,  Extra_1'Unrestricted_Access, "First_Word (""Holy Edison"")");
   end Initialize;

   Basics     : Basics_Test;
   Extra      : Extra_Test;

   procedure Add_Tests (Test_Suite : in out Ahven.Framework.Test_Suite) is
   begin
      Test_Suite.Add_Static_Test (Basics);
      Test_Suite.Add_Static_Test (Extra);
   end Add_Tests;

end Mission_Tests;
