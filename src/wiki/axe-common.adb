------------------------------------------------------------------------------
--  Copyright © 2018, Maxim Reznik <reznikmm@gmail.com>
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

with GNAT.MD5;

with League.Base_Codecs;
with League.Stream_Element_Vectors;

package body Axe.Common is

   -----------------
   -- MD5_Base_64 --
   -----------------

   function MD5_Base_64
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      Digest : constant GNAT.MD5.Binary_Message_Digest :=
        GNAT.MD5.Digest (Text.To_UTF_8_String);
      Vector : constant League.Stream_Element_Vectors.Stream_Element_Vector :=
        League.Stream_Element_Vectors.To_Stream_Element_Vector (Digest);
   begin
      return League.Base_Codecs.To_Base_64_URL (Vector);
   end MD5_Base_64;

   ---------------
   -- Read_JSON --
   ---------------

   procedure Read_JSON
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Result : out League.JSON.Documents.JSON_Document)
   is
      Data   : Ada.Streams.Stream_Element_Array (1 .. 1024);
      Last   : Ada.Streams.Stream_Element_Offset := 0;
      Vector : League.Stream_Element_Vectors.Stream_Element_Vector;
   begin
      loop
         Stream.Read (Data, Last);
         exit when Last in 0;
         Vector.Append (Data (1 .. Last));
      end loop;

      Result := League.JSON.Documents.From_JSON (Vector);
   end Read_JSON;

end Axe.Common;
