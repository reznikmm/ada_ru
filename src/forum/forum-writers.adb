with Ada.Streams.Stream_IO;
with League.Text_Codecs;

with XML.SAX.File_Input_Sources;
with XML.SAX.Simple_Readers;
with XML.Templates.Processors;
with XML.SAX.HTML5_Writers;
with XML.SAX.Output_Destinations.Strings;

package body Forum.Writers is

   type Parameter is record
      Name     : League.Strings.Universal_String;
      Value    : League.Holders.Holder;
   end record;

   type Parameter_Array is array (Positive range <>) of Parameter;

   procedure Write_File_Index
     (HTML     : String;
      Template : League.Strings.Universal_String;
      Arg      : Parameter_Array);

   ----------------------
   -- Write_File_Index --
   ----------------------

   procedure Write_File_Index
     (HTML     : String;
      Template : League.Strings.Universal_String;
      Arg      : Parameter_Array)
   is
      UTF_8  : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec (+"utf-8");
      Input  : aliased XML.SAX.File_Input_Sources.File_Input_Source;
      Reader : XML.SAX.Simple_Readers.Simple_Reader;
      Filter : aliased XML.Templates.Processors.Template_Processor;
      Writer : aliased XML.SAX.HTML5_Writers.HTML5_Writer;
      Output : aliased XML.SAX.Output_Destinations.Strings
        .String_Output_Destination;
      File   : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Create (File, Name => HTML);

      for J of Arg loop
         Filter.Set_Parameter (J.Name, J.Value);
      end loop;

      --  Set template input
      Input.Open_By_File_Name (Template);
      --  Configure reader
      Reader.Set_Input_Source (Input'Unchecked_Access);
      Reader.Set_Content_Handler (Filter'Unchecked_Access);
      Reader.Set_Lexical_Handler (Filter'Unchecked_Access);

      --  Configure template processor
      Filter.Set_Content_Handler (Writer'Unchecked_Access);
      Filter.Set_Lexical_Handler (Writer'Unchecked_Access);
      --  Configure template persing output
      Writer.Set_Output_Destination (Output'Unchecked_Access);

      --  Process template
      Reader.Parse;
      Ada.Streams.Stream_IO.Write
        (File, UTF_8.Encode (Output.Get_Text).To_Stream_Element_Array);
      Ada.Streams.Stream_IO.Close (File);
   end Write_File_Index;

   -----------------------
   -- Write_Forum_Index --
   -----------------------

   procedure Write_Forum_Index
     (Root  : String;
      Value : League.Holders.Holder) is
   begin
      Write_File_Index
        (Root & "index.html",
         +"index.html.tmpl",
         (1 => (+"forums", Value)));
   end Write_Forum_Index;

   procedure Write_Forum_Page
     (Root  : String;
      Forum : League.Holders.Holder;
      Page  : League.Holders.Holder)
   is
      Ok    : Boolean;
      Id    : League.Holders.Holder;
      Index : League.Holders.Holder;
      Code  : League.Strings.Universal_String;
      Text  : League.Strings.Universal_String;
   begin
      League.Holders.Component (Forum, +"id", Id, Ok);
      League.Holders.Component (Page, +"index", Index, Ok);
      Code := League.Holders.Element (Id);
      Text := League.Holders.Element (Index);
      Write_File_Index
        (Root & Code.To_UTF_8_String & "_" & Text.To_UTF_8_String & ".html",
         +"forum-page.html.tmpl",
         ((+"page", Page),
          (+"forum", Forum)));
   end Write_Forum_Page;

   ----------------------
   -- Write_Topic_Page --
   ----------------------

   procedure Write_Topic_Page
     (Root  : String;
      Forum : League.Holders.Holder;
      Topic : League.Holders.Holder;
      Page  : League.Holders.Holder)
   is
      Ok    : Boolean;
      Id    : League.Holders.Holder;
      Index : League.Holders.Holder;
      Code  : League.Strings.Universal_String;
      Text  : League.Strings.Universal_String;
   begin
      League.Holders.Component (Topic, +"id", Id, Ok);
      League.Holders.Component (Page, +"index", Index, Ok);
      Code := League.Holders.Element (Id);
      Text := League.Holders.Element (Index);
      Write_File_Index
        (Root & "p" & Code.To_UTF_8_String & "_" &
           Text.To_UTF_8_String & ".html",
         +"topic-page.html.tmpl",
         ((+"page", Page),
          (+"topic", Topic),
          (+"forum", Forum)));
   end Write_Topic_Page;

end Forum.Writers;
