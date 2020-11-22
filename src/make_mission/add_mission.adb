with Ada.Directories;
with Ada.Streams;
with Ada.Wide_Wide_Text_IO;

with League.Application;
with League.JSON.Arrays;
with League.JSON.Documents;
with League.JSON.Objects;
with League.JSON.Values;
with League.Stream_Element_Vectors;
with League.String_Vectors;
with League.Strings;

with Ada_Pretty;

with Spawn.String_Vectors;
with Spawn.Processes;
with Spawn.Processes.Monitor_Loop;

with IO;

procedure Add_Mission is
   use type League.Strings.Universal_String;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   Station : constant League.Strings.Universal_String := +"library";

   Repo  : constant League.Strings.Universal_String :=
     League.Application.Arguments.Element (1);
   Slug  : constant League.Strings.Universal_String :=
     League.Application.Arguments.Element (2);
   Title : constant League.Strings.Universal_String :=
     League.Application.Arguments.Element (3);
   Call : constant League.Strings.Universal_String :=
     League.Application.Arguments.Element (4);

   procedure Git_Clone;
   --  Run `git submodule add Repo`

   function Mission return League.Strings.Universal_String;
   --  The last part of Repo

   procedure Run
     (Command   : League.Strings.Universal_String;
      Arguments : League.String_Vectors.Universal_String_Vector;
      Stdout    : out League.Stream_Element_Vectors.Stream_Element_Vector);
   --  Execute a command

   procedure Update_Station_JSON;
   procedure Make_Mission_JSON;
   procedure Read_Tests;
   procedure Append_Insert_SQL;

   procedure Append_Insert_SQL is
      Output : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Ada.Wide_Wide_Text_IO.Open
        (File => Output,
         Mode => Ada.Wide_Wide_Text_IO.Append_File,
         Name => "mail/forum.sql");
      Ada.Wide_Wide_Text_IO.New_Line (Output);
      Ada.Wide_Wide_Text_IO.Put_Line
        (Output,
         "insert into game_missions (mission, station, name, points) values");
      Ada.Wide_Wide_Text_IO.Put (Output, "('");
      Ada.Wide_Wide_Text_IO.Put (Output, Slug.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Put (Output, "', 'library', '");
      Ada.Wide_Wide_Text_IO.Put (Output, Title.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Put (Output, "', 5);");
      Ada.Wide_Wide_Text_IO.New_Line (Output);
   end Append_Insert_SQL;

   ---------------
   -- Git_Clone --
   ---------------

   procedure Git_Clone is
      Dir : constant League.Strings.Universal_String := "missions/" & Mission;
      Args : League.String_Vectors.Universal_String_Vector;
      Ignore : League.Stream_Element_Vectors.Stream_Element_Vector;
   begin
      if IO.Exists (Dir) then
         return;
      end if;

      Args.Append (+"submodule");
      Args.Append (+"add");
      Args.Append (Repo);
      Args.Append (Dir);
      Run (+"/usr/bin/git", Args, Ignore);
   end Git_Clone;

   -----------------------
   -- Make_Mission_JSON --
   -----------------------

   procedure Make_Mission_JSON is
      Five    : constant League.Strings.Universal_String := +"5";
      Empty   : constant League.Strings.Universal_String := +"empty";
      SVG     : constant League.Strings.Universal_String := +"svg";
      Item    : League.JSON.Objects.JSON_Object;
      Object  : League.JSON.Objects.JSON_Object;
      Replace : League.JSON.Arrays.JSON_Array;
   begin
      Item.Insert
        (+"from",
         League.JSON.Values.To_JSON_Value
           (League.Strings.Empty_Universal_String));

      Item.Insert
        (+"text",
         League.JSON.Values.To_JSON_Value
           (League.Strings.Empty_Universal_String));

      Replace.Append (Item.To_JSON_Value);

      Object.Insert (+"title", League.JSON.Values.To_JSON_Value (Title));
      Object.Insert (+"slug", League.JSON.Values.To_JSON_Value (Slug));
      Object.Insert (+"points", League.JSON.Values.To_JSON_Value (Five));
      Object.Insert (+"power", League.JSON.Values.To_JSON_Value (Empty));
      Object.Insert (+"svg", League.JSON.Values.To_JSON_Value (SVG));
      Object.Insert (+"station", League.JSON.Values.To_JSON_Value (Station));
      Object.Insert (+"replace", Replace.To_JSON_Value);

      IO.Write_JSON ("missions/" & Mission & ".json", Object);
   end Make_Mission_JSON;

   -------------
   -- Mission --
   -------------

   function Mission return League.Strings.Universal_String is
      List : constant League.String_Vectors.Universal_String_Vector :=
        Repo.Split ('/');
   begin
      return List (List.Length);
   end Mission;

   ----------------
   -- Read_Tests --
   ----------------

   procedure Read_Tests is
      function Image (Value : Integer) return League.Strings.Universal_String;

      -----------
      -- Image --
      -----------

      function Image
        (Value : Integer) return League.Strings.Universal_String
      is
         Image : Wide_Wide_String := Value'Wide_Wide_Image;
      begin
         Image (1) := '_';
         return +Image;
      end Image;

      use Ada_Pretty;
      F : aliased Factory;
      Unit : Node_Access;
      Pkg  : Node_Access;
      List : Node_Access;
      Regs  : Node_Access;
      Name   : Node_Access;
      Args   : League.String_Vectors.Universal_String_Vector;
      Keys   : League.String_Vectors.Universal_String_Vector;
      Output : League.Stream_Element_Vectors.Stream_Element_Vector;
      Object : League.JSON.Objects.JSON_Object;
      Tests  : League.JSON.Arrays.JSON_Array;
      Params : League.JSON.Arrays.JSON_Array;
   begin
      Ada.Directories.Create_Path ("../game/" & Slug.To_UTF_8_String);
      Args.Append (+"make_mission/dump_tests.py");
      Args.Append ("missions/" & Mission & "/verification");
      Run (+"/usr/bin/python", Args, Output);
      Object := League.JSON.Documents.From_JSON (Output).To_JSON_Object;
      Keys := Object.Keys;

      for J in 1 .. Keys.Length loop
         Tests := Object.Value (Keys (J)).To_Array;
         Name := F.New_Name (Keys (J) & "_Test");
         List := F.New_List
           (List,
            F.New_Type
              (Name       => Name,
               Definition => F.New_Record
                 (Parent      => F.New_Selected_Name
                      (+"Ahven.Framework.Test_Case"))));

         List := F.New_List
           (List,
            F.New_Subprogram_Declaration
              (Specification => F.New_Subprogram_Specification
                 (Name          => F.New_Name (+"Initialize"),
                  Parameters    => F.New_Parameter
                    (Name            => F.New_Name (+"Self"),
                     Type_Definition => Name,
                     Is_In           => True,
                     Is_Out          => True))));

         for K in 1 .. Tests.Length loop
            declare
               Param  : League.JSON.Values.JSON_Value;
               Answer : constant League.JSON.Values.JSON_Value :=
                 Tests (K).To_Object.Value (+"answer");
               Arg    : Node_Access;
               Args   : Node_Access;
               Cond   : Node_Access;
            begin
               Params := Tests (K).To_Object.Value (+"input").To_Array;

               for P in 1 .. Params.Length loop
                  Param := Params (P);
                  case Param.Kind is
                     when League.JSON.Values.String_Value =>
                        Arg := F.New_String_Literal
                          (Param.To_String.Split ('"').Join (""""""));
                     when others =>
                        raise Program_Error;
                  end case;

                  Args := F.New_List (Args, Arg);
               end loop;

               Cond := F.New_Apply
                 (F.New_Selected_Name ("Mission." & Call), Args);

               case Answer.Kind is
                  when League.JSON.Values.Boolean_Value =>
                     if Answer.To_Boolean = False then
                        Cond := F.New_Infix (+"not", Cond);
                     end if;
                  when others =>
                     raise Program_Error;
               end case;

               List := F.New_List
                 (List,
                  F.New_Subprogram_Body
                    (Specification => F.New_Subprogram_Specification
                       (Name => F.New_Name (Keys (J) & Image (K))),
                     Statements    => F.New_Statement
                       (F.New_Apply
                         (Prefix    => F.New_Selected_Name (+"Ahven.Assert"),
                          Arguments => F.New_List
                            (F.New_Argument_Association (Cond),
                             F.New_Argument_Association
                               (F.New_String_Literal (+"Incorrect result.")))))
                    ));
            end;
         end loop;

         declare
            Param     : League.JSON.Values.JSON_Value;
            Test_Name : League.Strings.Universal_String;
            Self : constant Node_Access := F.New_Name (+"Self");
            Add_Test : constant Node_Access :=
              F.New_Selected_Name (+"Ahven.Framework.Add_Test_Routine");
            Stmt     : Node_Access := F.New_Statement
              (F.New_Apply
                 (F.New_Name (+"Set_Name"),
                  F.New_List
                    (Self,
                     F.New_String_Literal (Keys (J)))));
         begin
            for K in 1 .. Tests.Length loop
               Params := Tests (K).To_Object.Value (+"input").To_Array;
               Test_Name := Call & " (";

               for P in 1 .. Params.Length loop
                  Param := Params (P);

                  if P > 1 then
                     Test_Name.Append (", ");
                  end if;

                  if Param.Is_String then
                     Test_Name.Append ("""""");
                     Test_Name.Append (Param.To_String);
                     Test_Name.Append ("""""");
                  end if;
               end loop;

               Test_Name.Append (")");

               Stmt := F.New_List
                 (Stmt, F.New_Statement
                   (F.New_Apply
                         (Add_Test,
                          F.New_List
                            ((Self,
                             F.New_Name (Keys (J) & Image (K) &
                                 "'Unrestricted_Access"),
                             F.New_String_Literal (Test_Name))))));

            end loop;

            List := F.New_List
              (List,
               F.New_Subprogram_Body
                 (Specification => F.New_Subprogram_Specification
                    (Name          => F.New_Name (+"Initialize"),
                     Parameters    => F.New_Parameter
                       (Name            => F.New_Name (+"Self"),
                        Type_Definition => Name,
                        Is_In           => True,
                        Is_Out          => True)),
                  Statements    => Stmt));

            List := F.New_List
              (List,
               F.New_Variable
                 (Name            => F.New_Name (Keys (J)),
                  Type_Definition => F.New_Name (Keys (J) & "_Test")));

            Regs := F.New_List
              (Regs,
               F.New_Statement
                 (F.New_Apply
                   (F.New_Selected_Name (+"Test_Suite.Add_Static_Test"),
                    F.New_Name (Keys (J)))));
         end;
      end loop;

      List := F.New_List
        (List,
         F.New_Subprogram_Body
           (Specification => F.New_Subprogram_Specification
              (Name          => F.New_Name (+"Add_Tests"),
               Parameters    => F.New_Parameter
                 (Name            => F.New_Name (+"Test_Suite"),
                  Type_Definition => F.New_Selected_Name
                    (+"Ahven.Framework.Test_Suite"),
                  Is_In           => True,
                  Is_Out          => True)),
            Statements    => Regs));

      Pkg := F.New_Package_Body (F.New_Name (+"Mission_Tests"), List);

      Unit := F.New_Compilation_Unit
        (Root    => Pkg,
         Clauses => F.New_List
           (F.New_With (F.New_Name (+"Ahven")),
            F.New_With (F.New_Name (+"Mission"))));

      IO.Write_File
        ("../game/" & Slug & "/mission_tests.adb", F.To_Text (Unit));
   end Read_Tests;

   ---------
   -- Run --
   ---------

   procedure Run
     (Command   : League.Strings.Universal_String;
      Arguments : League.String_Vectors.Universal_String_Vector;
      Stdout    : out League.Stream_Element_Vectors.Stream_Element_Vector)
   is
      type Listener is new Spawn.Processes.Process_Listener with record
         Output    : League.Stream_Element_Vectors.Stream_Element_Vector;
         Done      : Boolean := False;
         Exit_Code : Integer := 0;
      end record;

      overriding procedure Finished
        (Self      : in out Listener;
         Exit_Code : Integer);

      overriding procedure Standard_Output_Available (Self : in out Listener);

      overriding procedure Finished
        (Self      : in out Listener;
         Exit_Code : Integer) is
      begin
         Self.Done := True;
         Self.Exit_Code := Exit_Code;
         Ada.Wide_Wide_Text_IO.Put_Line ("Exit:" & Exit_Code'Wide_Wide_Image);
      end Finished;

      Process : Spawn.Processes.Process;

      overriding procedure Standard_Output_Available
        (Self : in out Listener)
      is
         use type Ada.Streams.Stream_Element_Offset;
         Data : Ada.Streams.Stream_Element_Array (1 .. 512);
         Last : Ada.Streams.Stream_Element_Offset;
      begin
         loop
            Process.Read_Standard_Output (Data, Last);
            exit when Last = 0;
            Self.Output.Append (Data (1 .. Last));
         end loop;
      end Standard_Output_Available;

      Args    : Spawn.String_Vectors.UTF_8_String_Vector;
      Output  : aliased Listener;
   begin
      Ada.Wide_Wide_Text_IO.Put ("Run: ");
      Ada.Wide_Wide_Text_IO.Put (Command.To_Wide_Wide_String);

      for J in 1 .. Arguments.Length loop
         Args.Append (Arguments.Element (J).To_UTF_8_String);
         Ada.Wide_Wide_Text_IO.Put (" ");
         Ada.Wide_Wide_Text_IO.Put (Arguments.Element (J).To_Wide_Wide_String);
      end loop;

      Ada.Wide_Wide_Text_IO.New_Line;
      Process.Set_Program (Command.To_UTF_8_String);
      Process.Set_Arguments (Args);
      Process.Set_Listener (Output'Unchecked_Access);
      Process.Start;

      while not Output.Done loop
         Spawn.Processes.Monitor_Loop (10);
      end loop;

      pragma Assert (Output.Exit_Code = 0);
      Stdout := Output.Output;
   end Run;

   -------------------------
   -- Update_Station_JSON --
   -------------------------

   procedure Update_Station_JSON is
      Object : League.JSON.Objects.JSON_Object;
      List   : League.JSON.Arrays.JSON_Array;
      File   : constant League.Strings.Universal_String :=
        "missions/" & Station & ".json";
   begin
      IO.Read_JSON (File, Object);
      List := Object.Value (+"missions").To_Array;

      if List.Last_Element.To_String /= Mission then
         List.Append (League.JSON.Values.To_JSON_Value (Mission));
         Object.Insert (+"missions", List.To_JSON_Value);
         IO.Write_JSON (File, Object);
      end if;
   end Update_Station_JSON;

begin
   Git_Clone;
   Update_Station_JSON;
   Make_Mission_JSON;
   Read_Tests;
   Append_Insert_SQL;
end Add_Mission;
