with League.Stream_Element_Vectors;

with League.Holders;

package Forum.Writers is

   type File_Information is record
      Name : League.Strings.Universal_String;
      Data : League.Stream_Element_Vectors.Stream_Element_Vector;
      Hash : League.Hash_Type;
   end record;

   function Image (Value : File_Information) return Wide_Wide_String is
     (Value.Data.Length'Wide_Wide_Image &
      Value.Hash'Wide_Wide_Image &
      " " &
      Value.Name.To_Wide_Wide_String);

   procedure Write_Forum_Atom
     (Value : League.Holders.Holder;
      Info  : out File_Information);

   procedure Write_Forum_Index
     (Value : League.Holders.Holder;
      Info  : out File_Information);

   procedure Write_Forum_Page
     (Forum : League.Holders.Holder;
      Page  : League.Holders.Holder;
      Info  : out File_Information);

   procedure Write_Topic_Page
     (Forum : League.Holders.Holder;
      Topic : League.Holders.Holder;
      Page  : League.Holders.Holder;
      Info  : out File_Information);

end Forum.Writers;
