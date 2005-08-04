package body Ada_Links.Files.File is

   ------------
   -- Create --
   ------------

   function Create
     (Id  : String;
      URL : String)
      return File_Node_Ptr
   is
   begin
      return new File_File_Node'(Next    => null,
                                 Id      => U.To_Unbounded_String (Id),
                                 URL     => U.To_Unbounded_String (URL),
                                 Version => U.To_Unbounded_String ("-"),
                                 Size    => U.Null_Unbounded_String);
   end Create;

   --------------------
   -- Search_Version --
   --------------------

   procedure Search_Version
     (Object : in out File_File_Node)
   is
   begin
      null;
   end Search_Version;

end Ada_Links.Files.File;

