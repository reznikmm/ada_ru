with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Wide_Wide_Text_IO;

with League.Holders.JSON_Objects;
with League.JSON.Documents;

with XML.SAX.Input_Sources.Streams.Files;
with XML.SAX.Simple_Readers;
with XML.Templates.Processors;
with XML.SAX.HTML5_Writers;
with XML.SAX.Output_Destinations.Strings;

package body IO is

   ---------------------
   -- Expand_Template --
   ---------------------

   procedure Expand_Template
     (File_Name : League.Strings.Universal_String;
      Descr     : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String;
      Context   : League.JSON.Objects.JSON_Object;
      Out_Name  : League.Strings.Universal_String)
   is
      function "+" (Text : Wide_Wide_String)
        return League.Strings.Universal_String
          renames League.Strings.To_Universal_String;

      Result : League.Strings.Universal_String;
      Input  : aliased XML.SAX.Input_Sources.Streams.Files.File_Input_Source;
      Reader : aliased XML.SAX.Simple_Readers.Simple_Reader;
      Filter : aliased XML.Templates.Processors.Template_Processor;
      Writer : aliased XML.SAX.HTML5_Writers.HTML5_Writer;
      Output : aliased XML.SAX.Output_Destinations.Strings
        .String_Output_Destination;

      Placeholder : constant Wide_Wide_String := "description_place_holder";
      Pos    : Natural;
      File   : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Input.Open_By_File_Name (File_Name);
      Reader.Set_Input_Source (Input'Unchecked_Access);
      Reader.Set_Content_Handler (Filter'Unchecked_Access);
      Reader.Set_Lexical_Handler (Filter'Unchecked_Access);
      Filter.Set_Content_Handler (Writer'Unchecked_Access);
      Filter.Set_Lexical_Handler (Writer'Unchecked_Access);
      Filter.Set_Parameter
        (+"context",
         League.Holders.JSON_Objects.To_Holder (Context));
      Writer.Set_Output_Destination (Output'Unchecked_Access);
      Reader.Parse;
      Result := Output.Get_Text;
      Pos := Result.Index (Placeholder);

      if Pos > 0  then
         Result.Replace (Pos, Pos + Placeholder'Length - 1, Descr);
      end if;

      Ada.Wide_Wide_Text_IO.Create
        (File,
         Name => Out_Name.To_UTF_8_String);
      Ada.Wide_Wide_Text_IO.Put (File, Result.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Close (File);
   end Expand_Template;

   ---------------
   -- Read_File --
   ---------------

   procedure Read_File
     (File_Name : League.Strings.Universal_String;
      Result    : out League.Strings.Universal_String)
   is
      Input : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Result.Clear;

      Ada.Wide_Wide_Text_IO.Open
        (Input, Ada.Wide_Wide_Text_IO.In_File, File_Name.To_UTF_8_String);

      while not Ada.Wide_Wide_Text_IO.End_Of_File (Input) loop
         declare
            Line : constant Wide_Wide_String :=
              Ada.Wide_Wide_Text_IO.Get_Line (Input);
         begin
            if not Result.Is_Empty then
               Result.Append (Ada.Characters.Wide_Wide_Latin_1.LF);
            end if;

            Result.Append (Line);
         end;
      end loop;

      Ada.Wide_Wide_Text_IO.Close (Input);
   end Read_File;

   ---------------
   -- Read_JSON --
   ---------------

   procedure Read_JSON
     (File_Name : League.Strings.Universal_String;
      Result    : out League.JSON.Objects.JSON_Object)
   is
      Doc  : League.JSON.Documents.JSON_Document;
      Text : League.Strings.Universal_String;
   begin
      Read_File (File_Name, Text);
      Doc := League.JSON.Documents.From_JSON (Text);
      Result := Doc.To_JSON_Object;
   end Read_JSON;

end IO;
