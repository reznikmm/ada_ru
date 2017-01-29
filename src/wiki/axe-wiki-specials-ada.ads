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

with Ada.Containers.Hashed_Sets;

with Axe.Ada_Handlers;
with Axe.Ada_Scanners;
with Axe.Ada_Scanners_Types;
with Axe.Ada_Scanners_Tokens;
with League.Strings.Hash;

package Axe.Wiki.Specials.Ada is
   type Ada_Format is limited new Special_Format with private;

   procedure Initialize
     (Self      : in out Ada_Format;
      Namespace : League.Strings.Universal_String);

private

   type Handler is new Axe.Ada_Handlers.Handler with record
      Delimiter  : League.Strings.Universal_String;
      Identifier : League.Strings.Universal_String;
      Word       : League.Strings.Universal_String;
      Literal    : League.Strings.Universal_String;
      Comment    : League.Strings.Universal_String;
      Space      : League.Strings.Universal_String;
      New_Line   : League.Strings.Universal_String;
      Error      : League.Strings.Universal_String;
   end record;

   package Sets is new Standard.Ada.Containers.Hashed_Sets
     (Element_Type        => League.Strings.Universal_String,
      Hash                => League.Strings.Hash,
      Equivalent_Elements => League.Strings."=",
      "="                 => League.Strings."=");

   type Ada_Format is limited new Special_Format with record
      Namespace : League.Strings.Universal_String;
      Handler   : aliased Axe.Wiki.Specials.Ada.Handler;
      Reserved  : Sets.Set;
   end record;

   overriding procedure Process
     (Self   : in out Ada_Format;
      Text   : League.Strings.Universal_String;
      Writer : access XML.SAX.Writers.SAX_Writer'Class);

   overriding procedure Delimiter
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Identifier
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Numeric_Literal
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Obsolescent_Numeric_Literal
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Character_Literal
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure String_Literal
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Obsolescent_String_Literal
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Comment
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Space
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure New_Line
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Error
     (Self    : not null access Handler;
      Scanner : not null access Axe.Ada_Scanners.Scanner'Class;
      Rule    : Axe.Ada_Scanners_Types.Rule_Index;
      Token   : out Axe.Ada_Scanners_Tokens.Token_Kind;
      Skip    : in out Boolean);

end Axe.Wiki.Specials.Ada;
