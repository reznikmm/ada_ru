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

with Axe.Wiki.Parser;

package Axe.Wiki.Titles is

   type Handler (Nested : not null access Axe.Wiki.Parser.Wiki_Handler'Class)
     is new Axe.Wiki.Parser.Wiki_Handler with private;

   procedure Initialize
     (Self  : out Handler;
      Title : League.Strings.Universal_String;
      Wiki_URI_Prefix : Wide_Wide_String);

   not overriding function Title
     (Self : Handler) return League.Strings.Universal_String;

   not overriding function Description
     (Self : Handler) return League.Strings.Universal_String;

   not overriding function Image
     (Self : Handler) return League.Strings.Universal_String;

private

   type Handler (Nested : not null access Axe.Wiki.Parser.Wiki_Handler'Class)
     is new Axe.Wiki.Parser.Wiki_Handler with record
       Title       : League.Strings.Universal_String;
       Description : League.Strings.Universal_String;
       Image       : League.Strings.Universal_String;
       Prefix      : League.Strings.Universal_String;
       In_Heading  : Boolean := False;
   end record;

   overriding procedure Start_Element
     (Self : in out Handler;
      Info : Element_Info);

   overriding procedure End_Element
     (Self : in out Handler;
      Info : Element_Info);

   overriding procedure Characters
     (Self : in out Handler;
      Text : League.Strings.Universal_String);

end Axe.Wiki.Titles;
