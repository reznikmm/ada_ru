with Ada.Strings.Unbounded;

package Ada_Links.Files is

   type File_Node is abstract tagged private;

   function Id
     (Object : File_Node)
     return String;

   procedure Search_Version
     (Object : in out File_Node) is abstract;

   function Version
     (Object : File_Node)
     return String;

   function URL
     (Object : File_Node)
     return String;

   function File_Name
     (Object : File_Node)
     return String;

   function Size
     (Object : File_Node)
     return String;

   type File_Node_Ptr is access all File_Node'Class;

private

   package U renames Ada.Strings.Unbounded;

   type File_Node is abstract tagged record
      Next    : File_Node_Ptr;
      Id      : U.Unbounded_String;
      Version : U.Unbounded_String;
      URL     : U.Unbounded_String;
      Size    : U.Unbounded_String;
   end record;

   procedure Find_Max_Version
     (Page    : in     String;
      Regexp  : in     String;
      Version :    out U.Unbounded_String;
      URL     :    out U.Unbounded_String);

   function Normalize_Link
     (Base : U.Unbounded_String;
      Link : U.Unbounded_String)
     return U.Unbounded_String;

end Ada_Links.Files;
