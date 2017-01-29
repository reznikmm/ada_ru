------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--        Library for dealing with grammars for for Gela project,           --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;
with League.Strings.Cursors.Characters;

with Axe.Ada_Handlers;
with Axe.Ada_Scanners_Types;
with Axe.Ada_Scanners_Tokens;

package Axe.Ada_Scanners is
   pragma Preelaborate;
   use Axe.Ada_Scanners_Types;

   type Scanner is tagged limited private;

   procedure Set_Source
     (Self   : in out Scanner'Class;
      Source : League.Strings.Universal_String);

   procedure Set_Handler
     (Self    : in out Scanner'Class;
      Handler : not null Axe.Ada_Handlers.Handler_Access);

   subtype Start_Condition is State;

   procedure Set_Start_Condition
    (Self : in out Scanner'Class; Condition : Start_Condition);

   function Get_Start_Condition
     (Self : Scanner'Class) return Start_Condition;

   procedure Get_Token
     (Self   : access Scanner'Class;
      Result : out Axe.Ada_Scanners_Tokens.Token);

   procedure Move_Back (Self : in out Scanner'Class; Count : Positive) is null;

   function Get_Text
     (Self : Scanner'Class) return League.Strings.Universal_String;

   function Get_Token_Length (Self : Scanner'Class) return Positive;
   function Get_Token_Position (Self : Scanner'Class) return Positive;

private

   Buffer_Half_Size : constant := 1024;
   End_Of_Buffer : constant := 0;

   subtype Buffer_Index is Positive range 1 .. 2 * Buffer_Half_Size;

   type Character_Class_Array is array (Buffer_Index) of Character_Class;

   Error_Character : constant Character_Class := 0;
   Error_State : constant State := State'Last;

   type Buffer_Half is (Low, High);

   type Buffer_Offset is array (Buffer_Half) of Natural;

   type Scanner is tagged limited record
      Handler : Axe.Ada_Handlers.Handler_Access;
      Source  : League.Strings.Universal_String;
      Cursor  : League.Strings.Cursors.Characters.Character_Cursor;
      Start   : State := INITIAL;
      Next    : Buffer_Index := 1;
      From    : Buffer_Index := 1;
      To      : Natural := 0;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Offset  : Buffer_Offset := (0, 0);
      Buffer  : Wide_Wide_String (Buffer_Index) :=
        (1 => Wide_Wide_Character'Val (End_Of_Buffer),
         others => <>);
      Classes : Character_Class_Array := (1 => Error_Character, others => <>);
   end record;

   procedure Read_Buffer (Self : in out Scanner'Class);

end Axe.Ada_Scanners;
