with Ahven;
with Mission;

package body Mission_Tests is

   type Extra_Test is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (Self   : in out Extra_Test);

   procedure Extra_1 is
   begin
      Ahven.Assert ( not Mission.Is_All_Upper ("Hi"), "Incorrect result.");
   end Extra_1;

   procedure Extra_2 is
   begin
      Ahven.Assert (Mission.Is_All_Upper ("     "), "Incorrect result.");
   end Extra_2;

   procedure Extra_3 is
   begin
      Ahven.Assert (Mission.Is_All_Upper ("123"), "Incorrect result.");
   end Extra_3;

   procedure Extra_4 is
   begin
      Ahven.Assert (Mission.Is_All_Upper ("DIGITS123"), "Incorrect result.");
   end Extra_4;

   procedure Extra_5 is
   begin
      Ahven.Assert (Mission.Is_All_Upper ("WORLD"), "Incorrect result.");
   end Extra_5;

   procedure Initialize (Self : in out Extra_Test) is
   begin
      Set_Name (Self, "Extra");
      Ahven.Framework.Add_Test_Routine
        (Self                       ,
         Extra_1'Unrestricted_Access,
         "Is_All_Upper (""Hi"")");
      Ahven.Framework.Add_Test_Routine
        (Self                       ,
         Extra_2'Unrestricted_Access,
         "Is_All_Upper (""     "")");
      Ahven.Framework.Add_Test_Routine
        (Self                       ,
         Extra_3'Unrestricted_Access,
         "Is_All_Upper (""123"")");
      Ahven.Framework.Add_Test_Routine
        (Self                       ,
         Extra_4'Unrestricted_Access,
         "Is_All_Upper (""DIGITS123"")");
      Ahven.Framework.Add_Test_Routine
        (Self                       ,
         Extra_5'Unrestricted_Access,
         "Is_All_Upper (""WORLD"")");
   end Initialize;

   Extra  : Extra_Test;

   type Basics_Test is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (Self   : in out Basics_Test);

   procedure Basics_1 is
   begin
      Ahven.Assert (Mission.Is_All_Upper ("ALL UPPER"), "Incorrect result.");
   end Basics_1;

   procedure Basics_2 is
   begin
      Ahven.Assert
        ( not Mission.Is_All_Upper ("all lower"), "Incorrect result.");
   end Basics_2;

   procedure Basics_3 is
   begin
      Ahven.Assert
        ( not Mission.Is_All_Upper ("mixed UPPER and lower"),
         "Incorrect result.");
   end Basics_3;

   procedure Basics_4 is
   begin
      Ahven.Assert (Mission.Is_All_Upper (""), "Incorrect result.");
   end Basics_4;

   procedure Initialize (Self : in out Basics_Test) is
   begin
      Set_Name (Self, "Basics");
      Ahven.Framework.Add_Test_Routine
        (Self                        ,
         Basics_1'Unrestricted_Access,
         "Is_All_Upper (""ALL UPPER"")");
      Ahven.Framework.Add_Test_Routine
        (Self                        ,
         Basics_2'Unrestricted_Access,
         "Is_All_Upper (""all lower"")");
      Ahven.Framework.Add_Test_Routine
        (Self                        ,
         Basics_3'Unrestricted_Access,
         "Is_All_Upper (""mixed UPPER and lower"")");
      Ahven.Framework.Add_Test_Routine
        (Self                        ,
         Basics_4'Unrestricted_Access,
         "Is_All_Upper ("""")");
   end Initialize;

   Basics : Basics_Test;

   procedure Add_Tests (Test_Suite : in out Ahven.Framework.Test_Suite) is
   begin
      Test_Suite.Add_Static_Test (Extra);
      Test_Suite.Add_Static_Test (Basics);
   end Add_Tests;

end Mission_Tests;
