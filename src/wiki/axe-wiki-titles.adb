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

package body Axe.Wiki.Titles is

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Self : in out Handler;
      Text : League.Strings.Universal_String) is
   begin
      if Self.In_Heading then
         if Self.Title.Is_Empty then
            Self.Title := Text;
         end if;
      elsif Self.Description.Length < 300 then
         Self.Description.Append (Text);
      end if;

      Self.Nested.Characters (Text);
   end Characters;

   -----------------
   -- Description --
   -----------------

   not overriding function Description
     (Self : Handler) return League.Strings.Universal_String is
   begin
      return Self.Description;
   end Description;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self : in out Handler;
      Info : Element_Info) is
   begin
      if Info.Kind in Heading then
         Self.In_Heading := False;
      end if;

      Self.Nested.End_Element (Info);
   end End_Element;

   -----------
   -- Image --
   -----------

   not overriding function Image
     (Self : Handler) return League.Strings.Universal_String is
   begin
      return Self.Image;
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : out Handler;
      Wiki_URI_Prefix : Wide_Wide_String) is
   begin
      Self.In_Heading := False;
      Self.Prefix.Append (Wiki_URI_Prefix);
   end Initialize;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self : in out Handler;
      Info : Element_Info) is
   begin
      if Info.Kind in Heading then
         Self.In_Heading := True;
      elsif Self.Image.Is_Empty and
        Info.Kind in HTTP_Link | Boxed_Link | Boxed_Wiki_Link
      then
         declare
            URL : League.Strings.Universal_String := Info.Link;
         begin
            if URL.Starts_With ("wiki:") then
               URL.Replace (1, 5, Self.Prefix);
            end if;

            if URL.Ends_With (".png")
              or else URL.Ends_With (".gif")
              or else URL.Ends_With (".jpg")
              or else URL.Ends_With (".svg")
            then
               Self.Image := URL;
            end if;
         end;
      end if;

      Self.Nested.Start_Element (Info);
   end Start_Element;

   -----------
   -- Title --
   -----------

   not overriding function Title
     (Self : Handler) return League.Strings.Universal_String is
   begin
      return Self.Title;
   end Title;

end Axe.Wiki.Titles;
