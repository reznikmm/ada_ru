package Ada_Links.Files.Regexp is

   type Regexp_File_Node is new File_Node with private;

   procedure Search_Version
     (Object : in out Regexp_File_Node);

   function Create
     (Id     : String;
      Index  : String;
      Regexp : String) return File_Node_Ptr;

private

   type Regexp_File_Node is new File_Node with record
      Index  : U.Unbounded_String;
      Regexp : U.Unbounded_String;
   end record;

end Ada_Links.Files.Regexp;
