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

with XML.SAX.Attributes;

package body Axe.Wiki.HTML_Output is
   package U renames League.Strings;

   function "+" (Text : Wide_Wide_String) return U.Universal_String
     renames U.To_Universal_String;

   XHTML  : constant U.Universal_String := +"http://www.w3.org/1999/xhtml";
   PRE    : constant U.Universal_String := +"pre";
   I      : constant U.Universal_String := +"i";
   STRONG : constant U.Universal_String := +"strong";
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

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Self : in out Context;
      Text : League.Strings.Universal_String) is
   begin
      Self.Writer.Characters (Text);
   end Characters;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self : in out Context;
      Info : Element_Info) is
   begin
      case Info.Kind is
         when Special_Format =>
            Self.Writer.End_Element (XHTML, PRE, PRE);
         when Preformat =>
            Self.Writer.End_Element (XHTML, PRE, PRE);
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
            null;
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
            if Self.Img_Link then
               Self.Writer.End_Element (XHTML, IMG, IMG);
            else
               Self.Writer.End_Element (XHTML, A, A);
            end if;
         when Horizontal_Line =>
            null;
         when Anchor =>
            Self.Writer.End_Element (XHTML, A, A);
      end case;
   end End_Element;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self            : out Context;
      Writer          : access XML.SAX.Writers.SAX_Writer'Class;
      Wiki_URI_Prefix : Wide_Wide_String)
   is
   begin
      Self.Wiki_URI  := U.To_Universal_String (Wiki_URI_Prefix);
      Self.Writer    := Writer;
      Self.In_Mono   := False;
   end Initialize;

   ----------
   -- Link --
   ----------

   procedure Link
     (Self : in out Context;
      Info : Element_Info)
   is
      Attributes     : XML.SAX.Attributes.SAX_Attributes
        := XML.SAX.Attributes.Empty_SAX_Attributes;

      URL : U.Universal_String := Info.Link;
   begin
      if URL.Starts_With ("wiki:") then
         URL.Replace (1, 5, Self.Wiki_URI);
      end if;

      if URL.Ends_With (".png")
         or URL.Ends_With (".gif")
         or URL.Ends_With (".jpg")
      then
         Attributes.Set_Value (SRC, URL);
         Attributes.Set_Value (ALT, Info.Title);
         Self.Writer.Start_Element (XHTML, IMG, IMG, Attributes);
         Self.Img_Link := True;
      else
         Attributes.Set_Value (HREF, URL);
         Self.Writer.Start_Element (XHTML, A, A, Attributes);
         Self.Writer.Characters (Info.Title);
         Self.Img_Link := False;
      end if;
   end Link;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self : in out Context;
      Info : Element_Info)
   is
      Attributes     : XML.SAX.Attributes.SAX_Attributes
        := XML.SAX.Attributes.Empty_SAX_Attributes;
   begin
      case Info.Kind is
         when Special_Format =>
            Self.Writer.Start_Element (XHTML, PRE, PRE);
         when Preformat =>
            Self.Writer.Start_Element (XHTML, PRE, PRE);
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
            Self.Writer.End_Element (XHTML, BR, BR);
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
