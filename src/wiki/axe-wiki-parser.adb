------------------------------------------------------------------------------
--  Copyright © 2016, Maxim Reznik <reznikmm@gmail.com>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--     * this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--     * notice, this list of conditions and the following disclaimer in the
--     * documentation and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
------------------------------------------------------------------------------
--  $Date:$
------------------------------------------------------------------------------

with Ada.Characters.Wide_Wide_Latin_1;

with League.Characters;
with League.Regexps;
with League.String_Vectors;

package body Axe.Wiki.Parser is
   package U renames League.Strings;
   package R renames League.Regexps;

   type Para_Kinds is
     (None, Para, Ordered_List, Numbered_List, Table);

   Para_Stack_Size : constant := 10;

   type Para_Stack is array (1 .. Para_Stack_Size) of Para_Kinds;

   type State is record
      Have_Para : Para_Kinds;  --  Now we are in Have_Para
      Need_Para : Para_Kinds;  --  Next text should be plaaced in Need_Para
      Stack     : Para_Stack;  --  Stack of outer Have_Para
      List_Deep : Natural;     --  Index in Stack
   end record;

   Initial : constant State :=
     (Need_Para => Para,
      Have_Para => None,
      List_Deep => 0,
      Stack     => (others => None));

   function "+" (X : Wide_Wide_String) return League.Regexps.Regexp_Pattern;

   procedure Parse_Internal
     (Text : U.Universal_String;
      Data : in out Wiki_Handler'Class);

   ---------
   -- "+" --
   ---------

   function "+" (X : Wide_Wide_String) return League.Regexps.Regexp_Pattern is
   begin
      return League.Regexps.Compile (U.To_Universal_String (X));
   end"+";

   LF : constant League.Characters.Universal_Character :=
     League.Characters.To_Universal_Character
       (Ada.Characters.Wide_Wide_Latin_1.LF);

   Regexp : constant
     array (Element_Kinds) of League.Regexps.Regexp_Pattern :=
     (Preformat       => +("\n\{\{\{\n(.*?)\n\}\}\}\n"),
      Special_Format  =>
        +("\n\{\{\{\n\#[a-zA-Z\!]*?\n(.*?)\n\}\}\}\n"),
      Bold_Italic     => +"\'\'\'\'\'(.*?)\'\'\'\'\'",
      Bold            => +"\'\'\'(.*?)\'\'\'",
      Italic          => +"\'\'(.*?)\'\'",
      Underline       => +"__(.*?)__",
      Monospace       => +"\`(.*?)\`",
      Monospace_2     => +"\{\{\{(.*?)\}\}\}",
      Strike          => +"\~\~(.*?)\~\~",
      Superscript     => +"\^(.*?)\^",
      Subscript       => +"\,\,(.*?)\,\,",
      Heading_1       => +("\n\= (.*?) \=\n"),
      Heading_2       => +("\n\=\= (.*?) \=\=\n"),
      Heading_3       => +("\n\=\=\= (.*?) \=\=\=\n"),
      Paragraph       => +("\n\n"),
      Break           => +"\[\[BR\]\]",
      Ordered_List    => +("\n(\ +)\*\ "),
      Numbered_List   => +("\n(\ +)1\. "),
      List_Item       => +"\#",  --  Dummy element, just for reporting
      HTTP_Link       => +"(http\:\/\/[^\ \n\,]+)",
      Boxed_Wiki_Link => +"\[wiki\:[^\ \]]*(\ ?[^\]]*)\]",
      Boxed_Link      => +"\[[a-zA-Z]+\:[^\ \]]*(\ ?[^\]]*)\]",
      Horizontal_Line => +("\n\-\-\-\-\n"),
      Anchor          => +"\[\#([^\]]+)\]",
      Table_Row       => +("\n\|\|"),
      Table_Cell      => +"([^\n]*?)\|\|");

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Text : U.Universal_String;
      Data : in out Wiki_Handler'Class)
   is
      use type League.Strings.Universal_String;

      New_Line : constant Wide_Wide_String :=
        (1 => Ada.Characters.Wide_Wide_Latin_1.LF);
   begin
      if Text.Starts_With (New_Line) then
         Parse_Internal (Text, Data);
      else
         Parse_Internal (New_Line & Text, Data);
      end if;
   end Parse;

   --------------------
   -- Parse_Internal --
   --------------------

   procedure Parse_Internal
     (Text : U.Universal_String;
      Data : in out Wiki_Handler'Class)
   is

      type Item_Info is record
         From, To  : Natural;
         Text_From : Natural;
         Text_To   : Natural;
         Looked    : Natural;
      end record;
      --  If Item found in text piece then Looked = 0; else
      --  Looked = piece'Last

      Current : State := Initial;
      Parts   : array (Element_Kinds) of Item_Info;

      procedure Put_Text (Text : U.Universal_String);
      --  Output Text, start paragraph if needed

      procedure Open_Para;
      --  Start paragraph if needed

      procedure Close_Para (Deep : Natural := 0);
      --  Close paragraph if it's opened
      --  Close all lists deepper than Deep

      procedure Open_List
        (Kind    : Para_Kinds;
         Deep    : Natural);
      --  Put list element, open list if needed

      procedure Expand (From : Positive; To : Natural);
      --  Recursive parse Text

      procedure Element
        (Kind      : Element_Kinds;
         Text_From : Natural;
         Text_To   : Natural;
         From      : Natural;
         To        : Natural);
      --  Process element of given Kind

      function Find_Match
        (J      : Element_Kinds;
         Text   : U.Universal_String;
         Offset : Natural := 0) return Item_Info;

      function Get_Heading_Id (From, To : Positive) return U.Universal_String;

      ----------------
      -- Close_Para --
      ----------------

      procedure Close_Para (Deep : Natural := 0) is
      begin
         case Current.Have_Para is
            when Para =>
               Data.End_Element ((Kind => Paragraph));
            when Ordered_List | Numbered_List =>
               Data.End_Element ((Kind => List_Item));
            when Table =>
               Data.End_Element ((Kind => Table_Row, Table_Boundary => True));
            when None =>
               null;
         end case;

         while Current.List_Deep > Deep loop
            case Current.Stack (Current.List_Deep) is
               when Ordered_List =>
                  Data.End_Element ((Kind => Ordered_List));
               when Numbered_List =>
                  Data.End_Element ((Kind => Numbered_List));
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
        (Kind      : Element_Kinds;
         Text_From : Natural;
         Text_To   : Natural;
         From      : Natural;
         To        : Natural)
      is
         procedure Process
           (Process   : Boolean := True;
            Need_Para : Boolean := True);

         Info : Element_Info (Kind);

         procedure Process
           (Process   : Boolean := True;
            Need_Para : Boolean := True) is
         begin
            if Need_Para then
               Open_Para;
            else
               Close_Para;
            end if;

            Data.Start_Element (Info);

            if Process then
               Expand (Text_From, Text_To);
            elsif Text_From > 0 then
               Data.Characters (U.Slice (Text, Text_From, Text_To));
            end if;

            Data.End_Element (Info);
         end Process;

         Deep : Positive;
      begin
         case Kind is
            when Bold_Italic | Bold | Italic | Underline |
              Strike | Superscript | Subscript
              =>
               Process;
            when Monospace | Monospace_2 =>
               Process (False);
            when Heading_1 | Heading_2 | Heading_3 =>
               Current.Need_Para := None;
               Info.Heading_Id := Get_Heading_Id (Text_From, Text_To);
               Process (Need_Para => False);
               Current.Need_Para := Para;
            when Paragraph =>
               Close_Para;
               Current.Need_Para := Para;
            when Break =>
               Data.Start_Element (Info);
               Data.End_Element (Info);
            when Preformat =>
               Current.Need_Para := None;
               Process (False, False);
               Current.Need_Para := Para;
            when Special_Format =>
               declare
                  Piece : constant U.Universal_String :=
                    U.Slice (Text, From + 6, Text_From - 2);
                  List : constant League.String_Vectors.Universal_String_Vector
                    := U.Split (Piece, ' ');
               begin
                  Info.Format := List.Element (1);

                  if List.Length > 1 then
                     Info.Argument :=
                       U.Slice (Piece, Info.Format.Length + 2, Piece.Length);
                  end if;

                  Current.Need_Para := None;
                  Process (False, False);
                  Current.Need_Para := Para;
               end;

            when Ordered_List =>
               Deep := (Text_To - Text_From + 2) / 2;
               Close_Para (Deep);
               Open_List (Ordered_List, Deep);
            when Numbered_List =>
               Deep := (Text_To - Text_From + 2) / 2;
               Close_Para (Deep);
               Open_List (Numbered_List, Deep);
            when List_Item =>
               null;
            when Table_Row =>
               if Current.Have_Para /= Table then
                  Close_Para;
                  Current.Have_Para   := Table;
                  Current.Need_Para   := None;
                  Info.Table_Boundary := True;
               else
                  Data.End_Element (Info);
               end if;

               Data.Start_Element (Info);
            when Table_Cell =>
               Process;

            when HTTP_Link =>
               Info.Link := U.Slice (Text, From, To);
               Info.Title := Info.Link;

               Open_Para;
               Data.Start_Element (Info);
               Data.End_Element (Info);
            when Boxed_Link | Boxed_Wiki_Link =>
               if Text_From > From then
                  Info.Link := U.Slice (Text, From + 1, Text_From - 1);
               elsif Kind = HTTP_Link then
                  Info.Link := U.Slice (Text, From, To);
               else
                  Info.Link := U.Slice (Text, From + 1, To - 1);
               end if;

               if Text_To > Text_From then
                  Info.Title := U.Slice (Text, Text_From + 1, Text_To);
               else
                  Info.Title := Info.Link;
               end if;

               Open_Para;
               Data.Start_Element (Info);
               Data.End_Element (Info);
            when Horizontal_Line =>
               Data.Start_Element (Info);
               Data.End_Element (Info);
            when Anchor =>
               Info.Anchor_Name := U.Slice (Text, Text_From, Text_To);
               Data.Start_Element (Info);
               Data.End_Element (Info);
         end case;
      end Element;

      ------------
      -- Expand --
      ------------

      procedure Expand (From : Positive; To : Natural) is
         use type League.Characters.Universal_Character;

         Slice : U.Universal_String := U.Slice (Text, From, To);
         Pos   : Positive := From;
         Item  : Element_Kinds := Element_Kinds'First;
         Found : Boolean;
      begin
         while Pos <= Text.Length loop
            Found := False;

            for J in Parts'Range loop
               if Parts (J).Looked < To and Parts (J).From < Pos then
                  Parts (J) := Find_Match (J, Slice, Pos - 1);
               end if;

               if Parts (J).From > 0 and then
                 Parts (J).To <= To and then
                 (not Found or else Parts (J).From < Parts (Item).From)
               then
                  Found := True;
                  Item  := J;
               end if;
            end loop;

            if not Found then
               Put_Text (Slice);
               exit;
            end if;

            if Pos /= Parts (Item).From then
               Put_Text (U.Slice (Text, Pos, Parts (Item).From - 1));
            end if;

            if U.Element (Text, Parts (Item).To) = LF then
               Pos := Parts (Item).To;
            else
               Pos := Parts (Item).To + 1;
            end if;

            Element
              (Item,
               Parts (Item).Text_From,
               Parts (Item).Text_To,
               Parts (Item).From,
               Parts (Item).To);

            Slice := U.Slice (Text, Pos, To);
         end loop;
      end Expand;

      function Find_Match
        (J      : Element_Kinds;
         Text   : U.Universal_String;
         Offset : Natural := 0) return Item_Info
      is
         Match : constant R.Regexp_Match := R.Find_Match (Regexp (J), Text);
         Result : Item_Info;
      begin
         if R.Is_Matched (Match) then
            Result.Looked := 0;
            Result.From := R.First_Index (Match) + Offset;
            Result.To   := R.Last_Index (Match) + Offset;

            if R.Capture_Count (Match) > 0 then
               Result.Text_From := R.First_Index (Match, 1) + Offset;
               Result.Text_To   := R.Last_Index (Match, 1) + Offset;
            else
               Result.Text_From := 0;
               Result.Text_To   := 0;
            end if;
         else
            Result.From      := 0;
            Result.To        := 0;
            Result.Text_From := 0;
            Result.Text_To   := 0;
            Result.Looked    := Text.Length;
         end if;

         return Result;
      end Find_Match;

      --------------------
      -- Get_Heading_Id --
      --------------------

      function Get_Heading_Id
        (From, To : Positive) return U.Universal_String is
         pragma Unreferenced (From, To);
      begin
         return U.To_Universal_String ("x");
      end Get_Heading_Id;

      ---------------
      -- Open_List --
      ---------------

      procedure Open_List
        (Kind    : Para_Kinds;
         Deep    : Natural) is
      begin
         while Current.List_Deep < Deep loop
            Current.List_Deep := Current.List_Deep + 1;
            Current.Stack (Current.List_Deep) := Kind;

            case Kind is
               when Ordered_List =>
                  Data.Start_Element ((Kind => Ordered_List));
               when Numbered_List =>
                  Data.Start_Element ((Kind => Numbered_List));
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
                  Data.Start_Element ((Kind => Paragraph));
               when Ordered_List | Numbered_List =>
                  Data.Start_Element ((Kind => List_Item));
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

      procedure Put_Text (Text : U.Universal_String) is
      begin
         Open_Para;

         Data.Characters (Text);
      end Put_Text;

   begin
      for J in Parts'Range loop
         Parts (J) := Find_Match (J, Text);
      end loop;

      Expand (1, Text.Length);
      Close_Para;
   end Parse_Internal;

end Axe.Wiki.Parser;
