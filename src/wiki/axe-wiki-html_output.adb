------------------------------------------------------------------------------
--  Copyright © 2016-2018, Maxim Reznik <reznikmm@gmail.com>
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

with XML.SAX.Attributes;
with League.String_Vectors;
with League.Characters.Latin;

package body Axe.Wiki.HTML_Output is
   package U renames League.Strings;

   function "+" (Text : Wide_Wide_String) return U.Universal_String
     renames U.To_Universal_String;

   function Improve_Typography
     (Text : League.Strings.Universal_String)
       return League.Strings.Universal_String;
   --  Improve text by replacing some characters

   Em_Dash : constant League.Characters.Universal_Character :=
     League.Characters.To_Universal_Character ('—');
   Hyphen : constant League.Characters.Universal_Character :=
     League.Characters.To_Universal_Character ('-');

   PRE    : constant U.Universal_String := +"pre";
   I      : constant U.Universal_String := +"i";
   STRONG : constant U.Universal_String := +"b";
   SPAN   : constant U.Universal_String := +"span";
   TT     : constant U.Universal_String := +"tt";
   DEL    : constant U.Universal_String := +"del";
   SUP    : constant U.Universal_String := +"sup";
   SUB    : constant U.Universal_String := +"sub";
   H1     : constant U.Universal_String := +"h1";
   H2     : constant U.Universal_String := +"h2";
   H3     : constant U.Universal_String := +"h3";
   P      : constant U.Universal_String := +"p";
   UL     : constant U.Universal_String := +"ul";
   OL     : constant U.Universal_String := +"ol";
   LI     : constant U.Universal_String := +"li";
   TR     : constant U.Universal_String := +"tr";
   TD     : constant U.Universal_String := +"td";
   TABLE  : constant U.Universal_String := +"table";
   IFRAME : constant U.Universal_String := +"iframe";
   WIDTH  : constant U.Universal_String := +"width";
   HEIGHT : constant U.Universal_String := +"height";
   IMG    : constant U.Universal_String := +"img";
   A      : constant U.Universal_String := +"a";
   BR     : constant U.Universal_String := +"br";
   HR     : constant U.Universal_String := +"hr";
   CLASS  : constant U.Universal_String := +"class";
   HREF   : constant U.Universal_String := +"href";
   SRC    : constant U.Universal_String := +"src";
   ALT    : constant U.Universal_String := +"alt";
   NAME   : constant U.Universal_String := +"name";
   WIKI   : constant U.Universal_String := +"wiki";

   use type U.Universal_String;
   use type League.Characters.Universal_Character;

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Self : in out Handler;
      Text : League.Strings.Universal_String)
   is
      Special : Axe.Wiki.Specials.Special_Format_Access;
   begin
      if Self.Preformat then
         Self.Writer.Characters (Text);
      elsif Self.Special.Is_Empty then
         Self.Writer.Characters (Improve_Typography (Text));
      elsif Self.Map.Contains (Self.Special) then
         Special := Self.Map.Element (Self.Special);
         Special.Process (Text, Self.Writer);
      else
         Self.Writer.Characters (Text);
      end if;
   end Characters;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self : in out Handler;
      Info : Element_Info)
   is
      XHTML : constant League.Strings.Universal_String := Self.Namespace;
   begin
      case Info.Kind is
         when Special_Format =>
            if not Self.Map.Contains (Self.Special) then
               Self.Writer.End_Element (XHTML, PRE, PRE);
            end if;

            Self.Special.Clear;
         when Preformat =>
            Self.Writer.End_Element (XHTML, PRE, PRE);
            Self.Preformat := False;
         when Bold_Italic =>
            Self.Writer.End_Element (XHTML, I, I);
            Self.Writer.End_Element (XHTML, STRONG, STRONG);
         when Bold =>
            Self.Writer.End_Element (XHTML, STRONG, STRONG);
         when Italic =>
            Self.Writer.End_Element (XHTML, I, I);
         when Underline =>
            Self.Writer.End_Element (XHTML, SPAN, SPAN);
         when Monospace | Monospace_2 =>
            Self.Writer.End_Element (XHTML, TT, TT);
            Self.In_Mono := False;
         when Strike =>
            Self.Writer.End_Element (XHTML, DEL, DEL);
         when Superscript =>
            Self.Writer.End_Element (XHTML, SUP, SUP);
         when Subscript =>
            Self.Writer.End_Element (XHTML, SUB, SUB);
         when Heading_1 =>
            Self.Writer.End_Element (XHTML, H1, H1);
         when Heading_2 =>
            Self.Writer.End_Element (XHTML, H2, H2);
         when Heading_3 =>
            Self.Writer.End_Element (XHTML, H3, H3);
         when Paragraph =>
            Self.Writer.End_Element (XHTML, P, P);
         when Break =>
            Self.Writer.End_Element (XHTML, BR, BR);
         when Font_Awesome =>
            Self.Writer.End_Element (XHTML, I, I);
         when Ordered_List =>
            Self.Writer.End_Element (XHTML, UL, UL);
         when Numbered_List =>
            Self.Writer.End_Element (XHTML, OL, OL);
         when List_Item =>
            Self.Writer.End_Element (XHTML, LI, LI);
         when Table_Row =>
            Self.Writer.End_Element (XHTML, TR, TR);

            if Info.Table_Boundary then
               Self.Writer.End_Element (XHTML, TABLE, TABLE);
            end if;
         when Table_Cell =>
            Self.Writer.End_Element (XHTML, TD, TD);
         when HTTP_Link | Boxed_Link | Boxed_Wiki_Link =>
            case Self.Link_Kind is
               when Image =>
                  Self.Writer.End_Element (XHTML, IMG, IMG);
               when URL =>
                  Self.Writer.End_Element (XHTML, A, A);
               when Video =>
                  Self.Writer.End_Element (XHTML, IFRAME, IFRAME);
            end case;
         when Horizontal_Line =>
            null;
         when Anchor =>
            Self.Writer.End_Element (XHTML, A, A);
      end case;
   end End_Element;

   ------------------------
   -- Improve_Typography --
   ------------------------

   function Improve_Typography
     (Text : League.Strings.Universal_String)
        return League.Strings.Universal_String
   is
      procedure Dont_Break_After_Short_Words
        (Value : in out League.Strings.Universal_String);

      procedure Replace_Quote_Characters
        (Value : in out League.Strings.Universal_String);
      --  Replace some characters like:
      --    "Some text" => «Some text», but for short word use „a“

      procedure Replace_N_Dash
        (Value : in out League.Strings.Universal_String);
      --     20-21 => 20–21 for decimal ranges

      procedure Replace_Hyphen
        (Value : in out League.Strings.Universal_String);
      --     как-то => как‐то for real hyphen

      procedure Dont_Break_After_Short_Words
        (Value : in out League.Strings.Universal_String)
      is
         List   : constant League.String_Vectors.Universal_String_Vector :=
           Value.Split (' ', Behavior => League.Strings.Skip_Empty);
         Prev   : League.Strings.Universal_String;
         Result : League.String_Vectors.Universal_String_Vector;

         Ends_With_Space : constant Boolean := Value.Ends_With (" ");
      begin
         if Value.Starts_With (" ") then
            --  Keep starting space if any
            Result.Append (League.Strings.Empty_Universal_String);
         end if;

         for J in 1 .. List.Length loop
            declare
               Word : League.Strings.Universal_String := List (J);
            begin
               Replace_Quote_Characters (Word);
               Replace_N_Dash (Word);
               Replace_Hyphen (Word);

               if Word.Length = 1 and Word.Element (1) in Hyphen | Em_Dash then
                  --  Use Em_Dash instead on Hyphen when it apears between
                  --  spaces
                  --  Keep em dash on the line
                  if not Prev.Is_Empty then
                     Result.Append
                       (Prev &
                          League.Characters.Latin.No_Break_Space &
                          Em_Dash);
                     Prev.Clear;
                  elsif Result.Length > 0 then
                     Result.Replace
                       (Result.Length,
                        Result (Result.Length) &
                          League.Characters.Latin.No_Break_Space &
                          Em_Dash);
                  else
                     Result.Append
                       (League.Strings.Empty_Universal_String & Em_Dash);
                  end if;
               elsif J = 1 or Word.Length > 2 then
                  if Prev.Is_Empty then
                     Result.Append (Word);
                  else
                     Prev.Append (League.Characters.Latin.No_Break_Space);
                     Prev.Append (Word);

                     Result.Append (Prev);
                     Prev.Clear;
                  end if;
               elsif Prev.Is_Empty then
                  Prev := Word;
               else
                  Prev.Append (League.Characters.Latin.No_Break_Space);
                  Prev.Append (Word);
               end if;
            end;
         end loop;

         if not Prev.Is_Empty then
            Result.Append (Prev);
         end if;

         if Ends_With_Space then
            --  Keep trailing space if any
            Result.Append (League.Strings.Empty_Universal_String);
         end if;

         Value := Result.Join (" ");
      end Dont_Break_After_Short_Words;

      --------------------
      -- Replace_Hyphen --
      --------------------

      procedure Replace_Hyphen (Value : in out League.Strings.Universal_String)
      is
         List : constant League.String_Vectors.Universal_String_Vector :=
           Value.Split ('-');
      begin
         if List.Length > 1 then
            Value := List (1);

            for J in 2 .. List.Length loop
               if List (J).Length > 0 and then
                 List (J).Element (1).To_Wide_Wide_Character
                    in 'а' .. 'я' | 'А' .. 'Я'
               then
                  Value.Append ("‐");
               else
                  Value.Append ("-");
               end if;

               Value.Append (List (J));
            end loop;
         end if;
      end Replace_Hyphen;

      --------------------
      -- Replace_N_Dash --
      --------------------

      procedure Replace_N_Dash
        (Value : in out League.Strings.Universal_String)
      is
         Hyphen_Index : Natural;
      begin
         Hyphen_Index := Value.Index (Hyphen);

         if Hyphen_Index > 1 and
           Value.Length - 1 <= Long_Integer'Wide_Wide_Width
         then
            for J in 1 .. Value.Length loop
               if Value (J) /= Hyphen and
                 Value (J).To_Wide_Wide_Character not in '0' .. '9'
               then
                  Hyphen_Index := 0;
                  exit;
               end if;
            end loop;

            if Hyphen_Index > 0
              and then Value.Count (Hyphen) = 1
              and then
                Long_Integer'Wide_Wide_Value
                  (Value.Head_To (Hyphen_Index - 1).To_Wide_Wide_String)
                <
                Long_Integer'Wide_Wide_Value
                    (Value.Tail_From (Hyphen_Index + 1).To_Wide_Wide_String)
            then
               Value.Replace (Hyphen_Index, Hyphen_Index, "–");
            end if;
         end if;
      end Replace_N_Dash;

      ------------------------------
      -- Replace_Quote_Characters --
      ------------------------------

      procedure Replace_Quote_Characters
        (Value : in out League.Strings.Universal_String)
      is
         Last_Quote : constant Natural := Value.Last_Index ('"');
         Ends_With_Quote : constant Boolean := Last_Quote > 0 and then
           (Value.Ends_With ("""")
            or else Value.Ends_With (""".")
            or else Value.Ends_With (""","));
      begin
         if Last_Quote = 0 then
            return;
         elsif Value.Starts_With ("""") then
            if Ends_With_Quote then
               if Last_Quote < 8 then
                  Value.Replace (1, 1, "„");
                  Value.Replace (Last_Quote, Last_Quote, "“");
               else
                  Value.Replace (1, 1, "«");
                  Value.Replace (Last_Quote, Last_Quote, "»");
               end if;
            else
               Value.Replace (1, 1, "«");
            end if;
         elsif Ends_With_Quote then
            Value.Replace (Last_Quote, Last_Quote, "»");
         end if;
      end Replace_Quote_Characters;

      Lines : constant League.String_Vectors.Universal_String_Vector :=
        Text.Split (League.Characters.Latin.Line_Feed);
      Value : League.Strings.Universal_String := Lines.Join (" ");
   begin
      Dont_Break_After_Short_Words (Value);
      return Value;
   end Improve_Typography;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self            : out Handler;
      Writer          : access XML.SAX.Writers.SAX_Writer'Class;
      Namespace       : League.Strings.Universal_String;
      Wiki_URI_Prefix : Wide_Wide_String)
   is
   begin
      Self.Wiki_URI  := U.To_Universal_String (Wiki_URI_Prefix);
      Self.Writer    := Writer;
      Self.In_Mono   := False;
      Self.Namespace := Namespace;
   end Initialize;

   ----------
   -- Link --
   ----------

   procedure Link
     (Self : in out Handler;
      Info : Element_Info)
   is
      Attributes     : XML.SAX.Attributes.SAX_Attributes
        := XML.SAX.Attributes.Empty_SAX_Attributes;

      XHTML : constant League.Strings.Universal_String := Self.Namespace;
      URL   : U.Universal_String := Info.Link;
   begin
      if URL.Starts_With ("wiki:") then
         URL.Replace (1, 5, Self.Wiki_URI);
      end if;

      if URL.Ends_With (".png")
         or URL.Ends_With (".gif")
         or URL.Ends_With (".jpg")
         or URL.Ends_With (".svg")
      then
         Attributes.Set_Value (SRC, URL);
         Attributes.Set_Value (ALT, Info.Title);
         Self.Writer.Start_Element (XHTML, IMG, IMG, Attributes);
         Self.Link_Kind := Image;
      elsif URL.Starts_With ("https://youtu.be/") then
         Attributes.Set_Value (WIDTH, +"560");
         Attributes.Set_Value (HEIGHT, +"315");
         Attributes.Set_Value
           (SRC, "https://www.youtube.com/embed/" & URL.Tail_From (18));
         Self.Writer.Start_Element (XHTML, IFRAME, IFRAME, Attributes);
         Self.Link_Kind := Video;
      else
         Attributes.Set_Value (HREF, URL);
         Self.Writer.Start_Element (XHTML, A, A, Attributes);
         Self.Characters (Info.Title);
         Self.Link_Kind := HTML_Output.URL;
      end if;
   end Link;

   -----------------------------
   -- Register_Special_Format --
   -----------------------------

   procedure Register_Special_Format
     (Self  : in out Handler;
      Name  : League.Strings.Universal_String;
      Value : Axe.Wiki.Specials.Special_Format_Access) is
   begin
      Self.Map.Include (Name, Value);
   end Register_Special_Format;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self : in out Handler;
      Info : Element_Info)
   is
      XHTML : constant League.Strings.Universal_String := Self.Namespace;

      Attributes : XML.SAX.Attributes.SAX_Attributes
        := XML.SAX.Attributes.Empty_SAX_Attributes;
   begin
      case Info.Kind is
         when Special_Format =>
            Self.Special := Info.Format;

            if not Self.Map.Contains (Self.Special) then
               Self.Writer.Start_Element (XHTML, PRE, PRE);
            end if;
         when Preformat =>
            Self.Writer.Start_Element (XHTML, PRE, PRE);
            Self.Preformat := True;
         when Bold_Italic =>
            Self.Writer.Start_Element (XHTML, STRONG, STRONG);
            Self.Writer.Start_Element (XHTML, I, I);
         when Bold =>
            Self.Writer.Start_Element (XHTML, STRONG, STRONG);
         when Italic =>
            Self.Writer.Start_Element (XHTML, I, I);
         when Underline =>
            Attributes.Set_Value (CLASS, +"underline");
            Self.Writer.Start_Element (XHTML, SPAN, SPAN, Attributes);
         when Monospace | Monospace_2 =>
            Self.Writer.Start_Element (XHTML, TT, TT);
            Self.In_Mono := True;
         when Strike =>
            Self.Writer.Start_Element (XHTML, DEL, DEL);
         when Superscript =>
            Self.Writer.Start_Element (XHTML, SUP, SUP);
         when Subscript =>
            Self.Writer.Start_Element (XHTML, SUB, SUB);
         when Heading_1 =>
            Self.Writer.Start_Element (XHTML, H1, H1);
         when Heading_2 =>
            Self.Writer.Start_Element (XHTML, H2, H2);
         when Heading_3 =>
            Self.Writer.Start_Element (XHTML, H3, H3);
         when Paragraph =>
            Self.Writer.Start_Element (XHTML, P, P);
         when Break =>
            Self.Writer.Start_Element (XHTML, BR, BR);
         when Font_Awesome =>
            Attributes.Set_Value (CLASS, Info.Icon);
            Self.Writer.Start_Element (XHTML, I, I, Attributes);
         when Ordered_List =>
            Self.Writer.Start_Element (XHTML, UL, UL);
         when Numbered_List =>
            Self.Writer.Start_Element (XHTML, OL, OL);
         when List_Item =>
            Self.Writer.Start_Element (XHTML, LI, LI);
         when Table_Row =>
            if Info.Table_Boundary then
               Attributes.Set_Value (CLASS, WIKI);
               Self.Writer.Start_Element (XHTML, TABLE, TABLE, Attributes);
            end if;

            Self.Writer.Start_Element (XHTML, TR, TR);
         when Table_Cell =>
            Self.Writer.Start_Element (XHTML, TD, TD);
         when HTTP_Link =>
            Self.Link (Info);
         when Boxed_Link | Boxed_Wiki_Link =>
            Self.Link (Info);
         when Horizontal_Line =>
            Self.Writer.Start_Element (XHTML, HR, HR);
            Self.Writer.End_Element (XHTML, HR, HR);
         when Anchor =>
            Attributes.Set_Value (NAME, Info.Anchor_Name);
            Self.Writer.Start_Element (XHTML, A, A, Attributes);
      end case;
   end Start_Element;

end Axe.Wiki.HTML_Output;
