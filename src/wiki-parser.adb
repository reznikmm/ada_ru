with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Wiki.Parser is
   package U renames Ada.Strings.Unbounded;

   type Para_Kinds is
     (None, Para, Ordered_List, Numbered_List, Table);

   Para_Stack_Size : constant := 10;

   type Para_Stack is array (1 .. Para_Stack_Size) of Para_Kinds;

   type State is record
      Have_Para : Para_Kinds;  --  Now we in Have_Para
      Need_Para : Para_Kinds;  --  Next text should be plaaced in Need_Para
      Stack     : Para_Stack;  --  Stack of outer Have_Para
      List_Deep : Natural;     --  Index in Stack
   end record;

   Initial : constant State :=
     (Need_Para => Para,
      Have_Para => None,
      List_Deep => 0,
      Stack     => (others => None));

   function Start
     (Kind : in Element_Kinds;
      Text : in String)
     return Natural;
   --  Find first instance of element of given Kind in Text
   --  Return 0 if not found, otherwise position in Text

   function At_Start_Line
     (Text    : String;
      Pattern : String) return Natural;
   --  Find Pattern just after new_line character and some spaces

   function Get_Heading_Id (Text : String) return U.Unbounded_String;
   --  Transform plain text to heading id

   function Find (Text, Str : String) return Positive;
   --  Find in current line
   --  if not found return end of line position

   function Count_Spaces (Text : String) return Positive;
   --  Return number of spaces at the begining of Text

   function "+" (Text : String) return U.Unbounded_String
     renames U.To_Unbounded_String;

   -------------------
   -- At_Start_Line --
   -------------------

   function At_Start_Line
     (Text    : String;
      Pattern : String) return Natural
   is
      use Ada.Strings;
      From : constant Natural := Index (Text, Pattern);
      Line : Natural;
      Ok   : Boolean := True;
   begin
      if From = 0 then
         return 0;
      end if;

      Line := Index (Text (Text'First .. From - 1), (1 => ASCII.LF), Backward);

      if Line /= 0
        and then Index_Non_Blank (Text (Line + 1 .. From - 1)) = 0
      then
         return Line;
      else
         return At_Start_Line (Text (From + Pattern'Length .. Text'Last),
                               Pattern);
      end if;
   end At_Start_Line;

   ------------------
   -- Count_Spaces --
   ------------------

   function Count_Spaces (Text : String) return Positive is
   begin
      return Index_Non_Blank (Text (Text'First + 1 .. Text'Last)) - Text'First;
   end Count_Spaces;

   ----------
   -- Find --
   ----------

   function Find (Text, Str : String) return Positive is
      Pos  : constant Natural := Index (Text, Str);
      Line : constant Natural := Index (Text, (1 => ASCII.LF));
   begin
      if Pos = 0 then
         if Line  = 0 then
            return Text'Last + 1;
         else
            return Line;
         end if;
      elsif Line > 0 then
         return Positive'Min (Pos, Line);
      else
         return Pos;
      end if;
   end Find;

   --------------------
   -- Get_Heading_Id --
   --------------------

   function Get_Heading_Id (Text : String) return U.Unbounded_String is
      Space : constant Positive :=
        Find (Text (Text'First + 2 .. Text'Last), " ");
      Equal : constant Positive := Find (Text (Space .. Text'Last), "=");
   begin
      return U.To_Unbounded_String
        (Replace (Text (Space .. Equal - 1), " ", ""));
   end Get_Heading_Id;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Text : in     String;
      Data : in out Context)
   is
      Pos     : Positive := Text'First;
      Current : State := Initial;
      From    : array (Element_Kinds) of Natural := (others => 0);

      procedure Put_Text (Text : in String);
      --  Output Text, start paragraph if needed

      procedure Open_Para;
      --  Start paragraph if needed

      procedure Close_Para (Deep : Natural := 0);
      --  Close paragraph if it's opened
      --  Close all lists deepper than Deep

      procedure Open_List
        (Kind    : in     Para_Kinds;
         Deep    : in     Natural);
      --  Put list element, open list if needed

      procedure Expand (Text : in String);
      --  Recursive parse Text

      procedure Element
        (Kind    : in     Element_Kinds;
         Text    : in     String;
         Next    :    out Positive);
      --  Process element of given Kind
      --  shift Next to position after element

      ----------------
      -- Close_Para --
      ----------------

      procedure Close_Para (Deep : Natural := 0) is
      begin
         case Current.Have_Para is
            when Para =>
               End_Element ((Kind => Paragraph), Data);
            when Ordered_List | Numbered_List =>
               End_Element ((Kind => List_Item), Data);
            when Table =>
               End_Element ((Kind => Table_Row, Table_Boundary => True),
                            Data);
            when None =>
               null;
         end case;

         while Current.List_Deep > Deep loop
            case Current.Stack (Current.List_Deep) is
               when Ordered_List =>
                  End_Element ((Kind => Ordered_List), Data);
               when Numbered_List =>
                  End_Element ((Kind => Numbered_List), Data);
               when others =>
                  null;
            end case;

            Current.List_Deep := Current.List_Deep - 1;
         end loop;

         Current.Have_Para := None;
      end Close_Para;

      -------------
      -- Element --
      -------------

      procedure Element
        (Kind    : in     Element_Kinds;
         Text    : in     String;
         Next    :    out Positive)
      is
         Info : Element_Info (Kind);

         procedure Process
           (Literal   : in String;
            Process   : in Boolean := True;
            Need_Para : in Boolean := True)
         is
            From : constant Natural := Text'First + Literal'Length;
            To   : Natural := Index (Text (From .. Text'Last), Literal);
         begin
            if Need_Para then
               Open_Para;
            else
               Close_Para;
            end if;


            Start_Element (Info, Data);

            if To > 0 then
               Next := To + Literal'Length;
               To   := To - 1;
            else
               To   := Text'Last;
               Next := To + 1;
            end if;

            if Process then
               Expand (Text (From .. To));
            else
               Characters (Text (From .. To), Data);
            end if;

            End_Element (Info, Data);
         end Process;

         Deep : Positive;
      begin
         case Kind is
            when Bold_Italic =>
               Process ("'''''");
            when Bold =>
               Process ("'''");
            when Italic =>
               Process ("''");
            when Underline =>
               Process ("__");
            when Monospace =>
               Process ("`", False);
            when Monospace_2 =>
               Process ("}}}", False);
            when Strike =>
               Process ("~~");
            when Superscript =>
               Process ("^");
            when Subscript =>
               Process (",,");
            when Heading_1 =>
               Current.Need_Para := None;
               Info.Heading_Id := Get_Heading_Id (Text);
               Process (" =", Need_Para => False);
               Current.Need_Para := Para;
            when Heading_2 =>
               Current.Need_Para := None;
               Info.Heading_Id := Get_Heading_Id (Text);
               Process (" ==", Need_Para => False);
               Current.Need_Para := Para;
            when Heading_3 =>
               Current.Need_Para := None;
               Info.Heading_Id := Get_Heading_Id (Text);
               Process (" ===", Need_Para => False);
               Current.Need_Para := Para;
            when Paragraph =>
               Close_Para;
               Current.Need_Para := Para;
               Next := Text'First + 1;
            when Break =>
               Next := Text'First + 6;
               Start_Element (Info, Data);
               End_Element (Info, Data);
            when Preformat =>
               Current.Need_Para := None;
               Process (ASCII.LF & "}}}", False, False);
               Current.Need_Para := Para;
            when Ordered_List =>
               Deep := Count_Spaces (Text) / 2;
               Close_Para (Deep);
               Open_List (Ordered_List, Deep);
               Next := Index (Text, " * ") + 3;
            when Numbered_List =>
               Deep := Count_Spaces (Text) / 2;
               Close_Para (Deep);
               Open_List (Numbered_List, Deep);
               Next := Index (Text, " 1. ") + 3;
            when List_Item =>
               null;
            when Table_Row =>
               declare
                  From : constant Natural := Text'First + 1;
               begin
                  if Current.Have_Para /= Table then
                     Close_Para;
                     Current.Have_Para   := Table;
                     Current.Need_Para   := None;
                     Info.Table_Boundary := True;
                  else
                     End_Element (Info, Data);
                  end if;

                  Next := Index (Text (From .. Text'Last), (1 => ASCII.LF));
                  Start_Element (Info, Data);
                  Expand (Text (From .. Next - 1));
               end;
            when Table_Cell =>
               Process ("||");

               if Index (Text (Next .. Text'Last), "||") > 0 then
                  Next := Next - 2;
               end if;
            when HTTP_Link =>
               Open_Para;
               Next      := Find (Text, " ") + 1;
               Info.Link := +Text (Text'First .. Next - 2);

               Start_Element (Info, Data);
               End_Element (Info, Data);
            when Boxed_Link | Boxed_Wiki_Link =>
               Open_Para;
               Next := Find (Text, "]") + 1;

               declare
                  From  : constant Positive := Text'First + 1;
                  Space : constant Natural := Find (Text, " ");
               begin
                  if Space >= Next then
                     Info.Link := +Text (From .. Next - 2);
                  else
                     Info.Link  := +Text (From .. Space - 1);
                     Info.Title := +Text (Space + 1 .. Next - 2);
                  end if;
               end;

               Start_Element (Info, Data);
               End_Element (Info, Data);
            when Horizontal_Line =>
               Next := Text'First + 4;
               Start_Element (Info, Data);
               End_Element (Info, Data);
            when Anchor =>
               Next := Find (Text, "]") + 1;
               Info.Anchor_Name := +Text (Text'First + 2 .. Next - 2);
               Start_Element (Info, Data);
               End_Element (Info, Data);
         end case;
      end Element;

      ------------
      -- Expand --
      ------------

      procedure Expand (Text : in String) is
         Pos   : Positive := Text'First;
         Item  : Element_Kinds := Element_Kinds'First;
         Found : Boolean;
      begin
         while Pos <= Text'Last loop
            for J in From'Range loop
               if From (J) < Pos then
                  From (J) := Start (J, Text (Pos .. Text'Last));
               end if;
            end loop;

            Found := False;

            for J in From'Range loop
               if From (J) in Text'Range and then
                 (not Found or else From (J) < From (Item))
               then
                  Found := True;
                  Item  := J;
               end if;
            end loop;

            if not Found then
               Put_Text (Text (Pos .. Text'Last));
               exit;
            end if;

            if Pos /= From (Item) then
               Put_Text (Text (Pos .. From (Item) - 1));
               Pos := From (Item);
            end if;

            Element (Item,
                     Text (Pos .. Text'Last),
                     Pos);
         end loop;
      end Expand;

      ---------------
      -- Open_List --
      ---------------

      procedure Open_List
        (Kind    : in     Para_Kinds;
         Deep    : in     Natural) is
      begin
         while Current.List_Deep < Deep loop
            Current.List_Deep := Current.List_Deep + 1;
            Current.Stack (Current.List_Deep) := Kind;

            case Kind is
               when Ordered_List =>
                  Start_Element ((Kind => Ordered_List), Data);
               when Numbered_List =>
                  Start_Element ((Kind => Numbered_List), Data);
               when others =>
                  null;
            end case;
         end loop;

         Current.Need_Para := Kind;
      end Open_List;

      ---------------
      -- Open_Para --
      ---------------

      procedure Open_Para is
      begin
         if Current.Need_Para /= None then
            case Current.Need_Para is
               when Para =>
                  Start_Element ((Kind => Paragraph), Data);
               when Ordered_List | Numbered_List =>
                  Start_Element ((Kind => List_Item), Data);
               when Table | None =>
                  null;
            end case;

            Current.Have_Para := Current.Need_Para;
            Current.Need_Para := None;
         end if;
      end Open_Para;

      --------------
      -- Put_Text --
      --------------

      procedure Put_Text (Text : in String) is
      begin
         if Index_Non_Blank (Text) = 0 then
            return;
         end if;

         Open_Para;

         Characters (Text, Data);
      end Put_Text;

   begin
      Expand (Text);
      Close_Para;
   end Parse;

   -------------
   -- Replace --
   -------------

   function Replace (Text, From, To : String) return String is
      Pos : Natural := Index (Text, From);
   begin
      if Pos = 0 then
         return Text;
      end if;

      return Text (Text'First .. Pos - 1)
        & To & Replace (Text (Pos + From'Length .. Text'Last), From, To);
   end Replace;

   -----------
   -- Start --
   -----------

   function Start
     (Kind : in Element_Kinds;
      Text : in String)
      return Natural
   is
   begin
      case Kind is
         when Preformat =>
            return Index (Text, ASCII.LF & "{{{" & ASCII.LF);
         when Bold_Italic =>
            return Index (Text, "'''''");
         when Bold =>
            return Index (Text, "'''");
         when Italic =>
            return Index (Text, "''");
         when Underline =>
            return Index (Text, "__");
         when Monospace =>
            return Index (Text, "`");
         when Monospace_2 =>
            return Index (Text, "{{{");
         when Strike =>
            return Index (Text, "~~");
         when Superscript =>
            return Index (Text, "^");
         when Subscript =>
            return Index (Text, ",,");
         when Heading_1 =>
            return Index (Text, ASCII.LF & "= ");
         when Heading_2 =>
            return Index (Text, ASCII.LF & "== ");
         when Heading_3 =>
            return Index (Text, ASCII.LF & "=== ");
         when Paragraph =>
            return Index (Text, ASCII.LF & ASCII.LF);
         when Break =>
            return Index (Text, "[[BR]]");
        when Ordered_List =>
            return At_Start_Line (Text, " * ");
         when Numbered_List =>
            return At_Start_Line (Text, " 1. ");
         when List_Item =>
            --  Dummy element, just for reporting
            return 0;
         when Table_Row =>
            return Index (Text, ASCII.LF & "||");
         when Table_Cell =>
            return Index (Text, "||");
         when HTTP_Link =>
            return Index (Text, "http://");
         when Boxed_Link =>
            declare
               Pos    : Natural := Text'First;
               Colon  : Natural;
               Failed : Boolean;
            begin
               loop
                  Pos := Index (Text (Pos .. Text'Last), "[");

                  if Pos = 0 then
                     return 0;
                  end if;

                  Colon := Index (Text (Pos .. Text'Last), ":");

                  if Colon = 0 then
                     return 0;
                  end if;

                  Failed := (Colon - Pos = 1);

                  for J in Pos + 1 .. Colon - 1 loop
                     if Text (J) not in 'a' .. 'z' then
                        Failed := True;
                        exit;
                     end if;
                  end loop;

                  if Failed then
                     Pos := Pos + 1;
                  else
                     return Pos;
                  end if;
               end loop;
            end;
         when Boxed_Wiki_Link =>
            return Index (Text, "[wiki:");
         when Horizontal_Line =>
            return Index (Text, "----");
         when Anchor =>
            return Index (Text, "[#");
      end case;
   end Start;

end Wiki.Parser;
