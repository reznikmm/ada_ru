limited with Axe.Ada_Scanners;
with Axe.Ada_Scanners_Tokens;
with Axe.Ada_Scanners_Types;

package Axe.Ada_Handlers is
   pragma Preelaborate;

   type Handler is abstract tagged limited null record;

   procedure Delimiter
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure Identifier
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure Numeric_Literal
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure Obsolescent_Numeric_Literal
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure Character_Literal
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure String_Literal
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure Obsolescent_String_Literal
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure Comment
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure Space
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure New_Line
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean) is abstract;

   procedure Error
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean) is abstract;

   type Handler_Access is access all Handler'Class;

end Axe.Ada_Handlers;
