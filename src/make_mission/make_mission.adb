with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Directories;
with Ada.Wide_Wide_Text_IO;

with League.Application;
with League.Holders.JSON_Objects;
with League.JSON.Arrays;
with League.JSON.Documents;
with League.JSON.Objects;
with League.JSON.Values;
with League.Regexps;
with League.String_Vectors;
with League.Strings;

with XML.SAX.Input_Sources.Streams.Files;
with XML.SAX.Simple_Readers;
with XML.Templates.Processors;
with XML.SAX.HTML5_Writers;
with XML.SAX.Output_Destinations.Strings;

procedure Make_Mission is
   use type League.Strings.Universal_String;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   procedure Read_File
     (File_Name : League.Strings.Universal_String;
      Result    : out League.Strings.Universal_String);

   procedure Read_JSON
     (File_Name : League.Strings.Universal_String;
      Result    : out League.JSON.Objects.JSON_Object);

   procedure Append_Name (Item : Ada.Directories.Directory_Entry_Type);
   procedure Copy_Media (Item : Ada.Directories.Directory_Entry_Type);

   procedure Make_Mission_File
     (Prefix  : League.Strings.Universal_String;
      Lang    : League.Strings.Universal_String;
      Context : in out League.JSON.Objects.JSON_Object);

   procedure Do_Replace
     (Full : in out League.Strings.Universal_String;
      List : League.JSON.Arrays.JSON_Array);

   Mission_Dir : constant League.Strings.Universal_String :=
     League.Application.Arguments.Element (1);
   Path        : constant League.String_Vectors.Universal_String_Vector :=
     Mission_Dir.Split ('/');
   Mission     : constant League.Strings.Universal_String :=
     Path (Path.Length);
   Transl_Dir  : constant League.Strings.Universal_String :=
     Mission_Dir & "/translations";
   Media_Dir   : constant String :=
     Mission_Dir.To_UTF_8_String & "/info/media";
   Langs       : League.JSON.Arrays.JSON_Array;
   Context     : League.JSON.Objects.JSON_Object;

   function Slug return League.Strings.Universal_String is
     (Context.Value (+"slug").To_String);

   -----------------
   -- Append_Name --
   -----------------

   procedure Append_Name (Item : Ada.Directories.Directory_Entry_Type) is
      Lang : constant League.Strings.Universal_String :=
        League.Strings.From_UTF_8_String (Ada.Directories.Simple_Name (Item));
   begin
      if not Lang.Starts_With (".") then
         Langs.Append (League.JSON.Values.To_JSON_Value (Lang));
      end if;
   end Append_Name;

   ----------------
   -- Copy_Media --
   ----------------

   procedure Copy_Media (Item : Ada.Directories.Directory_Entry_Type) is
   begin
      Ada.Directories.Copy_File
        (Source_Name => Ada.Directories.Full_Name (Item),
         Target_Name => "/tmp/game/" &
           Slug.To_UTF_8_String & "/" &
           Ada.Directories.Simple_Name (Item));
   end Copy_Media;

   ----------------
   -- Do_Replace --
   ----------------

   procedure Do_Replace
     (Full : in out League.Strings.Universal_String;
      List : League.JSON.Arrays.JSON_Array)
   is
   begin
      for J in 1 .. List.Length loop
         declare
            Item : constant League.JSON.Objects.JSON_Object :=
              List (J).To_Object;
            Text : constant League.Strings.Universal_String :=
              Item.Value (+"text").To_String;
         begin
            if Item.Contains (+"from") then
               declare
                  From : constant Natural :=
                    Full.Index (Item.Value (+"from").To_String);
                  Value : constant League.Strings.Universal_String :=
                    Item.Value (+"to").To_String;
                  To   : constant Natural := Full.Index (From + 1, Value);
               begin
                  if From > 0 and To > 0 then
                     Full.Replace (From, To + Value.Length - 1, Text);
                  end if;
               end;
            elsif Item.Contains (+"regexp") then
               declare
                  Pattern : constant League.Regexps.Regexp_Pattern :=
                    League.Regexps.Compile (Item.Value (+"regexp").To_String);
                  Match   : constant League.Regexps.Regexp_Match :=
                    Pattern.Find_Match (Full);
               begin
                  if Match.Is_Matched then
                     Full.Replace (Match.First_Index, Match.Last_Index, Text);
                  end if;
               end;
            else
               raise Program_Error;
            end if;
         end;
      end loop;
   end Do_Replace;

   -----------------------
   -- Make_Mission_File --
   -----------------------

   procedure Make_Mission_File
     (Prefix  : League.Strings.Universal_String;
      Lang    : League.Strings.Universal_String;
      Context : in out League.JSON.Objects.JSON_Object)
   is
      Input  : aliased XML.SAX.Input_Sources.Streams.Files.File_Input_Source;
      Reader : aliased XML.SAX.Simple_Readers.Simple_Reader;
      Filter : aliased XML.Templates.Processors.Template_Processor;
      Writer : aliased XML.SAX.HTML5_Writers.HTML5_Writer;
      Output : aliased XML.SAX.Output_Destinations.Strings
        .String_Output_Destination;

      Short_File : constant League.Strings.Universal_String :=
        Mission_Dir & Prefix & "/info/task_short_description.html";
      Full_File : constant League.Strings.Universal_String :=
        Mission_Dir & Prefix & "/info/task_description.html";
      Short  : League.Strings.Universal_String;
      Full   : League.Strings.Universal_String;
      Text   : League.Strings.Universal_String;
      Placeholder : constant Wide_Wide_String := "description_place_holder";
      Pos    : Natural;
      Dir    : constant String := "/tmp/game/" & Lang.To_UTF_8_String;
      Dump   : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      if Ada.Directories.Exists (Short_File.To_UTF_8_String) then
         Read_File (Short_File, Short);
         Context.Insert
           (+"short_description", League.JSON.Values.To_JSON_Value (Short));
      end if;

      if Ada.Directories.Exists (Full_File.To_UTF_8_String) then
         Read_File (Full_File, Full);
         Do_Replace (Full, Context.Value (+"replace").To_Array);
      end if;

      Context.Insert (+"lang", League.JSON.Values.To_JSON_Value (Lang));
      Input.Open_By_File_Name (+"make_mission/mission.xhtml");
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
      Text := Output.Get_Text;
      Pos := Text.Index (Placeholder);
      Text.Replace (Pos, Pos + Placeholder'Length - 1, Full);

      Ada.Directories.Create_Path (Dir);
      Ada.Wide_Wide_Text_IO.Create
        (Dump,
         Name => Dir & "/" & Slug.To_UTF_8_String & ".html");
      Ada.Wide_Wide_Text_IO.Put (Dump, Text.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Close (Dump);
   end Make_Mission_File;

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

begin
   Ada.Directories.Search
     (Directory => Transl_Dir.To_UTF_8_String,
      Pattern   => "",
      Filter    => (Ada.Directories.Directory => True, others => False),
      Process   => Append_Name'Access);

   Read_JSON (Mission_Dir & ".json", Context);
   Context.Insert (+"repo", League.JSON.Values.To_JSON_Value (Mission));
   Context.Insert (+"langs", Langs.To_JSON_Value);

   Make_Mission_File
     (Prefix  => League.Strings.Empty_Universal_String,
      Lang    => +"en",
      Context => Context);

   declare
      Short : constant League.JSON.Values.JSON_Value :=
        Context.Value (+"short_description");
      Full  : constant League.JSON.Values.JSON_Value :=
        Context.Value (+"description");
   begin
      for J in 1 .. Langs.Length loop
         Make_Mission_File
           (Prefix  => "/translations/" & Langs (J).To_String & "/",
            Lang    => Langs (J).To_String,
            Context => Context);
         Context.Insert (+"short_description", Short);
         Context.Insert (+"description", Full);
      end loop;
   end;

   Ada.Directories.Create_Path ("/tmp/game/" & Slug.To_UTF_8_String);

   Ada.Directories.Copy_File
     (Mission_Dir.To_UTF_8_String & ".ada",
      "/tmp/game/" & Slug.To_UTF_8_String & "/" &
        Slug.To_UTF_8_String & ".ada");

   if Ada.Directories.Exists (Media_Dir) then
      Ada.Directories.Search
        (Directory => Media_Dir,
         Pattern   => "",
         Filter    => (Ada.Directories.Ordinary_File => True, others => False),
         Process   => Copy_Media'Access);
   end if;
end Make_Mission;
