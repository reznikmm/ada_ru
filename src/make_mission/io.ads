with League.Strings;
with League.JSON.Objects;

package IO is

   procedure Read_File
     (File_Name : League.Strings.Universal_String;
      Result    : out League.Strings.Universal_String);

   procedure Read_JSON
     (File_Name : League.Strings.Universal_String;
      Result    : out League.JSON.Objects.JSON_Object);

   procedure Expand_Template
     (File_Name : League.Strings.Universal_String;
      Descr     : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String;
      Context   : League.JSON.Objects.JSON_Object;
      Out_Name  : League.Strings.Universal_String);

end IO;
