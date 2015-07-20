with Text_Streams;
with Encodings.Classes;

generic
   type State is mod <>;
   type Character_Class is mod <>;
   type Token is (<>);
   type Switch is array (State range <>, Character_Class range <>) of State;
   type Accepted is array (State range <>) of Token;

   Table  : Switch;
   Finish : Accepted;

   with function Get_Class (C : Wide_String) return Character_Class;
   with package Encodings_Classes is
     new Encodings.Classes (Character_Class, Get_Class);

   Error_Token    : in Token := Token'First;
   File_End_Token : in Token := Token'Last;
   Buffer_Half    : in Positive := 512;

package Scaners is

   type Scaner (Input : access Text_Streams.Text_Stream'Class) is
     limited private;

   procedure Next_Token
     (Object : in out Scaner;
      Result :    out Token);

   procedure Enter
     (Object : in out Scaner;
      Start  : in     State);

   function Token_Image (Object : in Scaner) return Wide_String;

   procedure Set_Encoding
     (Object  : in out Scaner;
      Charset : in     Encodings.Encoding);

private

   Buffer_Size : constant Positive := 2 * Buffer_Half;

   subtype Index is Positive range 1 .. Buffer_Size + 1;
     -- +1 for a half of surogate symbol

   EOF         : constant Wide_Character  := Wide_Character'Last;
   Unknown     : constant Character_Class := Character_Class'Last;
   Surrogate   : constant Character_Class := Character_Class'Pred (Unknown);
   Rest_Size   : constant := 6; --  Decode leaves no more 6 chars



   type Scaner (Input : access Text_Streams.Text_Stream'Class) is limited
      record
         Started     : Boolean := False;
         Finished    : Boolean;
         Start       : State;
         Next        : Index;
         Buffer      : Wide_String (Index);
         Class       : Encodings_Classes.Character_Classes (Index);
         Rest_Lenght : Natural;
         Rest        : String (1 .. Rest_Size);
         Coder       : Encodings_Classes.Coder;
         Last_From   : Index;
         Last_To     : Natural;
      end record;

end Scaners;
