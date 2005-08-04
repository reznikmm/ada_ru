package Ada_Links.Files.File is

   type File_File_Node is new File_Node with private;

   procedure Search_Version
     (Object : in out File_File_Node);

   function Create
     (Id  : String;
      URL : String) return File_Node_Ptr;

private

   type File_File_Node is new File_Node with null record;

end Ada_Links.Files.File;
