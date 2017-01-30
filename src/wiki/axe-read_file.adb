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

with Ada.Directories;
with Ada.Streams.Stream_IO;

function Axe.Read_File
 (Name    : League.Strings.Universal_String;
  Decoder : League.Text_Codecs.Text_Codec)
   return League.Strings.Universal_String
is
   File_Name : constant String :=
     League.Text_Codecs.To_Exception_Message (Name);

--   Decoder : constant League.Text_Codecs.Text_Codec :=
--     League.Text_Codecs.Codec (League.Strings.To_Universal_String ("utf-8"));

   Size : constant Ada.Directories.File_Size :=
     Ada.Directories.Size (File_Name);

   Length : constant Ada.Streams.Stream_Element_Offset :=
     Ada.Streams.Stream_Element_Count (Size);

   File   : Ada.Streams.Stream_IO.File_Type;
   Data   : Ada.Streams.Stream_Element_Array (1 .. Length);
   Last   : Ada.Streams.Stream_Element_Offset;
begin
   Ada.Streams.Stream_IO.Open
     (File, Ada.Streams.Stream_IO.In_File, File_Name, "SHARED=NO");
   Ada.Streams.Stream_IO.Read (File, Data, Last);
   Ada.Streams.Stream_IO.Close (File);

   return Decoder.Decode (Data (1 .. Last));
end Axe.Read_File;
