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
--  This package supports navigation menu
------------------------------------------------------------------------------

with League.Strings;

with XML.SAX.Writers;

package Axe.Sidebars is

   type Sidebar is tagged limited private;

   procedure Initialize
     (Self : in out Sidebar;
      Text : League.Strings.Universal_String);

   procedure Expand
     (Self            : Sidebar;
      Writer          : access XML.SAX.Writers.SAX_Writer'Class;
      URI             : League.Strings.Universal_String;
      Wiki_URI_Prefix : Wide_Wide_String);

private

   type Item;
   type Item_Access is access Item;

   type Item_Kind is (Normal, Added, Changed);

   type Item is record
      Name  : League.Strings.Universal_String;
      Title : League.Strings.Universal_String;
      Level : Natural   := 0;
      Kind  : Item_Kind := Normal;
      Fill  : Boolean := True;
      Next  : Item_Access;
      Down  : Item_Access;
      Up    : Item_Access;
   end record;

   type Sidebar is tagged limited record
      Prefix : League.Strings.Universal_String;
      Suffix : League.Strings.Universal_String;
      Root   : Item_Access;
   end record;

end Axe.Sidebars;
