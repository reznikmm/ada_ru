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

with Axe.Wiki.Parser;
with XML.SAX.Writers;

package Axe.Wiki.HTML_Output is

   type Context is new Axe.Wiki.Parser.Wiki_Handler with private;

   overriding procedure Start_Element
     (Self : in out Context;
      Info : Element_Info);

   overriding procedure End_Element
     (Self : in out Context;
      Info : Element_Info);

   overriding procedure Characters
     (Self : in out Context;
      Text : League.Strings.Universal_String);

   procedure Initialize
     (Self            : out Context;
      Writer          : access XML.SAX.Writers.SAX_Writer'Class;
      Wiki_URI_Prefix : Wide_Wide_String);

private

   type Context is new Axe.Wiki.Parser.Wiki_Handler with record
      Writer   : access XML.SAX.Writers.SAX_Writer'Class;
      Wiki_URI : League.Strings.Universal_String;
      In_Mono  : Boolean;
      Img_Link : Boolean;
   end record;

   procedure Link
     (Self : in out Context;
      Info : Element_Info);
   --  make <a> or <img> element

end Axe.Wiki.HTML_Output;
