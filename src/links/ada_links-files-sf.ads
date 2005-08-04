package Ada_Links.Files.Sf is

   type Sourceforge_File_Node is new File_Node with private;

   procedure Search_Version
     (Object : in out Sourceforge_File_Node);

   function Create
     (Id      : String;
      Index   : String;
      Regexp  : String;
      Project : String) return File_Node_Ptr;

   function File_Name (Object : Sourceforge_File_Node) return String;

private

   type Sourceforge_File_Node is new File_Node with record
      Index   : U.Unbounded_String;
      Regexp  : U.Unbounded_String;
      Project : U.Unbounded_String;
      File    : U.Unbounded_String;
   end record;

end Ada_Links.Files.Sf;
