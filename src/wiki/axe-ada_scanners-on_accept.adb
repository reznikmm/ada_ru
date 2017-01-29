separate (Axe.Ada_Scanners)
procedure On_Accept
  (Self    : not null access Axe.Ada_Handlers.Handler'Class;
   Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
   Rule    : Axe.Ada_Scanners_Types.Rule_Index;
   Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
   Skip    : in out Boolean) is
begin
   case Rule is
      when 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10
         | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20
         | 21 | 22 | 23 | 24 | 25 | 26 | 27 =>
         Self.Delimiter (Scanner, Rule, Token, Skip);

      when 28 =>
         Self.Identifier (Scanner, Rule, Token, Skip);

      when 29 =>
         Self.Numeric_Literal (Scanner, Rule, Token, Skip);

      when 30 =>
         Self.Obsolescent_Numeric_Literal (Scanner, Rule, Token, Skip);

      when 31 =>
         Self.Character_Literal (Scanner, Rule, Token, Skip);

      when 32 =>
         Self.String_Literal (Scanner, Rule, Token, Skip);

      when 33 =>
         Self.Obsolescent_String_Literal (Scanner, Rule, Token, Skip);

      when 34 =>
         Self.Comment (Scanner, Rule, Token, Skip);

      when 35 =>
         Self.Space (Scanner, Rule, Token, Skip);

      when 36 =>
         Self.New_Line (Scanner, Rule, Token, Skip);

      when 37 | 38 =>
         Self.Error (Scanner, Rule, Token, Skip);

      when others =>
         raise Constraint_Error;
   end case;
end On_Accept;
