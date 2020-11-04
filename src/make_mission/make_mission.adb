with Ada.Directories;

with League.JSON.Arrays;
with League.JSON.Values;
with League.Regexps;
with League.String_Vectors;

with IO;

procedure Make_Mission
  (Mission_Dir : League.Strings.Universal_String;
   Station     : League.JSON.Objects.JSON_Object;
   Object      : out League.JSON.Objects.JSON_Object)
is
   use type League.Strings.Universal_String;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   procedure Append_Name (Item : Ada.Directories.Directory_Entry_Type);
   procedure Copy_Media (Item : Ada.Directories.Directory_Entry_Type);

   procedure Make_Mission_File
     (Prefix  : League.Strings.Universal_String;
      Lang    : League.Strings.Universal_String;
      Context : in out League.JSON.Objects.JSON_Object);

   procedure Do_Replace
     (Full : in out League.Strings.Universal_String;
      List : League.JSON.Arrays.JSON_Array);

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
      Lang_Dir : constant League.Strings.Universal_String :=
        "/tmp/game/" & Lang;

      Short_File : constant League.Strings.Universal_String :=
        Mission_Dir & Prefix & "/info/task_short_description.html";

      Full_File : constant League.Strings.Universal_String :=
        Mission_Dir & Prefix & "/info/task_description.html";

      Short  : League.Strings.Universal_String;
      Full   : League.Strings.Universal_String;
   begin
      if Ada.Directories.Exists (Short_File.To_UTF_8_String) then
         IO.Read_File (Short_File, Short);
         Context.Insert
           (+"short_description", League.JSON.Values.To_JSON_Value (Short));
      end if;

      if Ada.Directories.Exists (Full_File.To_UTF_8_String) then
         IO.Read_File (Full_File, Full);
         Do_Replace (Full, Context.Value (+"replace").To_Array);
         Context.Insert
           (+"description", League.JSON.Values.To_JSON_Value (Full));
      else
         Full := Context.Value (+"description").To_String;
      end if;

      Context.Insert (+"lang", League.JSON.Values.To_JSON_Value (Lang));

      Ada.Directories.Create_Path (Lang_Dir.To_UTF_8_String);

      IO.Expand_Template
        (File_Name => +"make_mission/mission.xhtml",
         Descr     => Full,
         Context   => Context,
         Out_Name  => Lang_Dir & "/" & Slug & ".html");

      IO.Expand_Template
        (File_Name => +"make_mission/solve.xhtml",
         Descr     => Full,
         Context   => Context,
         Out_Name  => Lang_Dir & "/" & Slug & "-solve.html");

   end Make_Mission_File;

   ---------------
   -- Read_File --
   ---------------

begin
   Ada.Directories.Search
     (Directory => Transl_Dir.To_UTF_8_String,
      Pattern   => "",
      Filter    => (Ada.Directories.Directory => True, others => False),
      Process   => Append_Name'Access);

   IO.Read_JSON (Mission_Dir & ".json", Context);
   Context.Insert (+"station", Station.To_JSON_Value);
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
        Slug.To_UTF_8_String & ".txt");

   if Ada.Directories.Exists (Media_Dir) then
      Ada.Directories.Search
        (Directory => Media_Dir,
         Pattern   => "",
         Filter    => (Ada.Directories.Ordinary_File => True, others => False),
         Process   => Copy_Media'Access);
   end if;

   Object := Context;
end Make_Mission;
