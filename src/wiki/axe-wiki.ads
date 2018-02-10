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

with League.Strings;

package Axe.Wiki is

   type Element_Kinds is
     (Special_Format, Preformat, Bold_Italic, Bold, Italic, Underline,
      Strike, Superscript, Subscript, Monospace, Monospace_2,
      Heading_3, Heading_2, Heading_1,
      Paragraph, Break, Ordered_List, Numbered_List, List_Item,
      Table_Row, Table_Cell, HTTP_Link, Boxed_Wiki_Link, Boxed_Link,
      Horizontal_Line, Anchor, Font_Awesome);

   subtype Heading is Element_Kinds range Heading_3 .. Heading_1;

   type Element_Info (Kind : Element_Kinds) is record
      case Kind is
         when Heading =>
            Heading_Id : League.Strings.Universal_String;
         when Table_Row =>
            Table_Boundary : Boolean := False;
         when HTTP_Link
           | Boxed_Link
           | Boxed_Wiki_Link =>
            Link  : League.Strings.Universal_String;
            Title : League.Strings.Universal_String;
         when Anchor =>
            Anchor_Name : League.Strings.Universal_String;
         when Special_Format =>
            Format   : League.Strings.Universal_String;
            Argument : League.Strings.Universal_String;
         when Font_Awesome =>
            Icon : League.Strings.Universal_String;
         when others =>
            null;
      end case;
   end record;

end Axe.Wiki;
