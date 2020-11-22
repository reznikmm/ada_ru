with League.JSON.Objects;
with League.String_Vectors;
with League.Strings;

package IO is

   procedure Read_File
     (File_Name : League.Strings.Universal_String;
      Result    : out League.Strings.Universal_String);

   procedure Read_JSON
     (File_Name : League.Strings.Universal_String;
      Result    : out League.JSON.Objects.JSON_Object);

   procedure Write_JSON
     (File_Name : League.Strings.Universal_String;
      Object    : League.JSON.Objects.JSON_Object);

   procedure Write_File
     (File_Name : League.Strings.Universal_String;
      Object    : League.String_Vectors.Universal_String_Vector);

   procedure Expand_Template
     (File_Name : League.Strings.Universal_String;
      Descr     : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String;
      Context   : League.JSON.Objects.JSON_Object;
      Out_Name  : League.Strings.Universal_String);

   function Exists (Path : League.Strings.Universal_String) return Boolean;

end IO;
