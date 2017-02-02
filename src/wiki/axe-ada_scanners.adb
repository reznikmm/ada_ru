with Matreshka.Internals.Unicode;

package body Axe.Ada_Scanners is

   EOF : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#7FFF_FFFE#);

   package Tables is
      function To_Class (Value : Matreshka.Internals.Unicode.Code_Point)
        return Character_Class;
      pragma Inline (To_Class);

      function Switch (S : State; Class : Character_Class) return State;
      pragma Inline (Switch);

      function Rule (S : State) return Axe.Ada_Scanners_Types.Rule_Index;
   end Tables;

   package body Tables is separate;

   use Tables;
   subtype Token is Axe.Ada_Scanners_Tokens.Token;

   procedure On_Accept
     (Self    : not null access Axe.Ada_Handlers.Handler'Class;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean);

   -------------------------
   -- Get_Start_Condition --
   -------------------------

   function Get_Start_Condition
     (Self : Scanner'Class)
      return Start_Condition
   is
   begin
      return Self.Start;
   end Get_Start_Condition;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Self : Scanner'Class)
      return League.Strings.Universal_String
   is
   begin
      if Self.From <= Self.To then
         return League.Strings.To_Universal_String
           (Self.Buffer (Self.From .. Self.To));
      elsif Self.From = Self.To + 1 then
         return League.Strings.Empty_Universal_String;
      else
         return League.Strings.To_Universal_String
           (Self.Buffer (Self.From .. Self.Buffer'Last)
              & Self.Buffer (1 .. Self.To));
      end if;
   end Get_Text;

   ---------------
   -- Get_Token --
   ---------------

   procedure Get_Token (Self : access Scanner'Class; Result : out Token) is
      procedure Next;

      procedure Next is
      begin
         if Self.Next = Self.Buffer'Last then
            Self.Next := 1;
         else
            Self.Next := Self.Next + 1;
         end if;
      end Next;

      End_Of_Buffer : constant Wide_Wide_Character :=
        Wide_Wide_Character'Val (Axe.Ada_Scanners.End_Of_Buffer);

      Current_State : State := Self.Start;
      Char          : Character_Class;
      Next_Rule     : Rule_Index;
      Skip          : Boolean := False;
   begin
      loop
         if Self.Buffer (Self.Next) = EOF then
            Result := Axe.Ada_Scanners_Tokens.End_Of_Input;
            return;
         end if;

         Current_State := Self.Start;
         Self.Rule := 0;
         Self.From := Self.Next;
         Self.To   := Self.Next;  --  If none match step one char as error

         loop
            Char := Self.Classes (Self.Next);

            if Char /= Error_Character then
               Current_State := Switch (Current_State, Char);

               exit when Current_State = Error_State;

               Next_Rule := Tables.Rule (Current_State);

               if Next_Rule /= 0 then
                  Self.Rule := Next_Rule;
                  Self.To := Self.Next;
               end if;

               Next;
            elsif Self.Buffer (Self.Next) = End_Of_Buffer then
               Self.Read_Buffer;
            else
               exit;
            end if;
         end loop;

         Self.Next := Self.To;
         Next;

         if Self.Rule = 0 then

            Self.Rule := Rule_Index'Last;
            Self.Handler.Error (Self, Self.Rule, Result, Skip);
            return;
         else

            On_Accept (Self.Handler, Self, Self.Rule, Result, Skip);

            if not Skip then
               return;
            end if;
         end if;
      end loop;
   end Get_Token;

   ----------------------
   -- Get_Token_Length --
   ----------------------

   function Get_Token_Length (Self : Scanner'Class) return Positive is
   begin
      if Self.From <= Self.To then
         return Self.To - Self.From + 1;
      else
         return Buffer_Index'Last - Self.From + 1 + Self.To;
      end if;
   end Get_Token_Length;

   ------------------------
   -- Get_Token_Position --
   ------------------------

   function Get_Token_Position (Self : Scanner'Class) return Positive is
      Half : constant Buffer_Half :=
        Buffer_Half'Val (Boolean'Pos (Self.From >= Buffer_Half_Size));
   begin
      return Self.Offset (Half) + Self.From;
   end Get_Token_Position;

   procedure On_Accept
     (Self    : not null access Axe.Ada_Handlers.Handler'Class;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean) is separate;

   -----------------
   -- Read_Buffer --
   -----------------

   procedure Read_Buffer (Self : in out Scanner'Class) is
      use Matreshka.Internals.Unicode;

      End_Of_Buffer : constant Wide_Wide_Character :=
        Wide_Wide_Character'Val (Axe.Ada_Scanners.End_Of_Buffer);

      Next : Code_Unit_32;
      Pos  : Buffer_Index := Self.Next;
   begin
      if Self.From <= Buffer_Half_Size xor Self.Next <= Buffer_Half_Size then
         raise Constraint_Error with "Token too large";
      end if;

      if Pos = Buffer_Half_Size then
         Self.Offset (High) := Self.Offset (High) + Self.Buffer'Length;
      elsif Pos = Self.Buffer'Last then
         Self.Offset (Low) := Self.Offset (Low) + Self.Buffer'Length;
      end if;

      loop
         if Self.Cursor.Has_Element then
            Next := Wide_Wide_Character'Pos (Self.Cursor.Element);
            Self.Cursor.Next;
         else
            Self.Buffer (Pos) := EOF;
            Self.Classes (Pos) := Error_Character;
            return;
         end if;

         Self.Buffer (Pos) := Wide_Wide_Character'Val (Next);

         Self.Classes (Pos) := To_Class (Next);

         if Pos = Self.Buffer'Last then
            Pos := 1;
         else
            Pos := Pos + 1;
         end if;

         if Pos = Buffer_Half_Size or Pos = Self.Buffer'Last then
            Self.Classes (Pos) := Error_Character;
            Self.Buffer (Pos) := End_Of_Buffer;
            return;
         end if;
      end loop;
   end Read_Buffer;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (Self    : in out Scanner'Class;
      Handler : not null Axe.Ada_Handlers.Handler_Access) is
   begin
      Self.Handler := Handler;
   end Set_Handler;

   ----------------
   -- Set_Source --
   ----------------

   procedure Set_Source
     (Self   : in out Scanner'Class;
      Source : League.Strings.Universal_String)
   is
   begin
      Self.Source := Source;
      Self.Cursor.First (Self.Source);
   end Set_Source;

   -------------------------
   -- Set_Start_Condition --
   -------------------------

   procedure Set_Start_Condition
     (Self : in out Scanner'Class;
      Condition : Start_Condition)
   is
   begin
      Self.Start := Condition;
   end Set_Start_Condition;

end Axe.Ada_Scanners;
