------------------------------------------------------------------------------
--  Copyright © 2017, Maxim Reznik <reznikmm@gmail.com>
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

package body Axe.Wiki.Specials.Ada is

   -----------------------
   -- Character_Literal --
   -----------------------

   overriding procedure Character_Literal
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token;
      Skip    : in out Boolean)
   is
      pragma Unreferenced (Rule, Skip);
   begin
      Token := (Self.Literal, Scanner.Get_Text);
   end Character_Literal;

   -------------
   -- Comment --
   -------------

   overriding procedure Comment
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token;
      Skip    : in out Boolean)
   is
      pragma Unreferenced (Rule, Skip);
   begin
      Token := (Self.Comment, Scanner.Get_Text);
   end Comment;

   ---------------
   -- Delimiter --
   ---------------

   overriding procedure Delimiter
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token;
      Skip    : in out Boolean)
   is
      pragma Unreferenced (Rule, Skip);
   begin
      Token := (Self.Delimiter, Scanner.Get_Text);
   end Delimiter;

   -----------
   -- Error --
   -----------

   overriding procedure Error
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token;
      Skip    : in out Boolean)
   is
      pragma Unreferenced (Rule, Skip);
   begin
      Token := (Self.Error, Scanner.Get_Text);
   end Error;

   ----------------
   -- Identifier --
   ----------------

   overriding procedure Identifier
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token;
      Skip    : in out Boolean)
   is
      pragma Unreferenced (Rule, Skip);
   begin
      Token := (Self.Identifier, Scanner.Get_Text);
   end Identifier;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self      : in out Ada_Format;
      Namespace : League.Strings.Universal_String)
   is
      procedure Add (Text : Wide_Wide_String);

      ---------
      -- Add --
      ---------

      procedure Add (Text : Wide_Wide_String) is
         Value : League.Strings.Universal_String;
      begin
         Value.Append (Text);
         Self.Reserved.Insert (Value.To_Casefold);
      end Add;

      H : Handler renames Self.Handler;
   begin
      Self.Namespace  := Namespace;
      H.Delimiter  := League.Strings.To_Universal_String ("delimiter");
      H.Identifier := League.Strings.To_Universal_String ("identifier");
      H.Word       := League.Strings.To_Universal_String ("keyword");
      H.Literal    := League.Strings.To_Universal_String ("literal");
      H.Comment    := League.Strings.To_Universal_String ("comment");
      H.Space      := League.Strings.To_Universal_String ("space");
      H.New_Line   := League.Strings.To_Universal_String ("new_line");
      H.Error      := League.Strings.To_Universal_String ("error");
      Add ("abort"); Add ("abs"); Add ("abstract"); Add ("accept");
      Add ("access"); Add ("aliased"); Add ("all"); Add ("and");
      Add ("array"); Add ("at"); Add ("begin"); Add ("body");
      Add ("case"); Add ("constant"); Add ("declare"); Add ("delay");
      Add ("delta"); Add ("digits"); Add ("do"); Add ("else");
      Add ("elsif"); Add ("end"); Add ("entry"); Add ("exception");
      Add ("exit"); Add ("for"); Add ("function"); Add ("generic");
      Add ("goto"); Add ("if"); Add ("in"); Add ("interface");
      Add ("is"); Add ("limited"); Add ("loop"); Add ("mod"); Add ("new");
      Add ("not"); Add ("null"); Add ("of"); Add ("or"); Add ("others");
      Add ("out"); Add ("overriding"); Add ("package"); Add ("pragma");
      Add ("private"); Add ("procedure"); Add ("protected"); Add ("raise");
      Add ("range"); Add ("record"); Add ("rem"); Add ("renames");
      Add ("requeue"); Add ("return"); Add ("reverse"); Add ("select");
      Add ("separate"); Add ("some"); Add ("subtype"); Add ("synchronized");
      Add ("tagged"); Add ("task"); Add ("terminate"); Add ("then");
      Add ("type"); Add ("until"); Add ("use"); Add ("when");
      Add ("while"); Add ("with"); Add ("xor"); Add ("parallel");
   end Initialize;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token;
      Skip    : in out Boolean)
   is
      pragma Unreferenced (Rule, Skip);
   begin
      Token := (Self.New_Line, Scanner.Get_Text);
   end New_Line;

   ---------------------
   -- Numeric_Literal --
   ---------------------

   overriding procedure Numeric_Literal
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token;
      Skip    : in out Boolean)
   is
      pragma Unreferenced (Rule, Skip);
   begin
      Token := (Self.Literal, Scanner.Get_Text);
   end Numeric_Literal;

   ---------------------------------
   -- Obsolescent_Numeric_Literal --
   ---------------------------------

   overriding procedure Obsolescent_Numeric_Literal
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token;
      Skip    : in out Boolean)
   is
      pragma Unreferenced (Rule, Skip);
   begin
      Token := (Self.Literal, Scanner.Get_Text);
   end Obsolescent_Numeric_Literal;

   --------------------------------
   -- Obsolescent_String_Literal --
   --------------------------------

   overriding procedure Obsolescent_String_Literal
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token;
      Skip    : in out Boolean)
   is
      pragma Unreferenced (Rule, Skip);
   begin
      Token := (Self.Literal, Scanner.Get_Text);
   end Obsolescent_String_Literal;

   -------------
   -- Process --
   -------------

   overriding procedure Process
     (Self   : in out Ada_Format;
      Text   : League.Strings.Universal_String;
      Writer : access XML.SAX.Writers.SAX_Writer'Class)
   is
      use type League.Strings.Universal_String;

      function "+" (Value : Wide_Wide_String)
        return League.Strings.Universal_String
          renames League.Strings.To_Universal_String;

      Scanner : aliased Axe.Ada_Scanners.Scanner;
      Token   : Axe.Ada_Scanners_Tokens.Token;
      Attr    : XML.SAX.Attributes.SAX_Attributes;
      Last    : Axe.Ada_Scanners_Tokens.Token;
      Class   : constant League.Strings.Universal_String := +"class";
      Span    : constant League.Strings.Universal_String := +"span";
   begin
      Last.Class := Self.Handler.New_Line;
      Scanner.Set_Source (Text);
      Scanner.Set_Handler (Self.Handler'Unchecked_Access);

      Attr.Set_Value (+"itemscope", +"itemscope");
      Attr.Set_Value (+"itemtype", +"http://schema.org/SoftwareSourceCode");
      Writer.Start_Element
        (Namespace_URI => Self.Namespace,
         Local_Name    => +"pre",
         Attributes    => Attr);
      Attr.Clear;

      Attr.Set_Value (+"itemprop", +"programmingLanguage");
      Attr.Set_Value (+"content", +"Ada");
      Writer.Start_Element
        (Namespace_URI => Self.Namespace,
         Local_Name    => +"meta",
         Attributes    => Attr);
      Writer.End_Element
        (Namespace_URI => Self.Namespace,
         Local_Name    => +"meta");
      Attr.Clear;

      Attr.Set_Value (+"itemprop", +"codeSampleType");
      Attr.Set_Value (+"content", +"code snippet");
      Writer.Start_Element
        (Namespace_URI => Self.Namespace,
         Local_Name    => +"meta",
         Attributes    => Attr);
      Writer.End_Element
        (Namespace_URI => Self.Namespace,
         Local_Name    => +"meta");
      Attr.Clear;

      loop
         Scanner.Get_Token (Token);

         if Token.Class = Self.Handler.Identifier
           and then Self.Reserved.Contains (Token.Text.To_Casefold)
         then
            Token.Class := Self.Handler.Word;
         end if;

         if Token.Class in Self.Handler.Space | Last.Class
           and Token.Class /= Self.Handler.New_Line
         then

            Last.Text.Append (Token.Text);
         else

            Attr.Set_Value
              (Qualified_Name => Class,
               Value          => Last.Class);

            Writer.Start_Element
              (Namespace_URI  => Self.Namespace,
               Local_Name     => Span,
               Attributes     => Attr);

            Writer.Characters (Last.Text);

            Writer.End_Element
              (Namespace_URI  => Self.Namespace,
               Local_Name     => Span);

            if Token.Class = Self.Handler.New_Line then
               --  Put new line characters before corresponding span
               --  To use .new_line:before {conetent: xxx} to decorate lines
               Writer.Characters (Token.Text);
               Token.Text.Clear;
            end if;

            Last := Token;
         end if;

         exit when Token.Class.Is_Empty;
      end loop;

      Writer.End_Element
        (Namespace_URI => Self.Namespace,
         Local_Name    => +"pre");
   end Process;

   -----------
   -- Space --
   -----------

   overriding procedure Space
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token;
      Skip    : in out Boolean)
   is
      pragma Unreferenced (Rule, Skip);
   begin
      Token := (Self.Space, Scanner.Get_Text);
   end Space;

   --------------------
   -- String_Literal --
   --------------------

   overriding procedure String_Literal
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token;
      Skip    : in out Boolean)
   is
      pragma Unreferenced (Rule, Skip);
   begin
      Token := (Self.Literal, Scanner.Get_Text);
   end String_Literal;

end Axe.Wiki.Specials.Ada;
