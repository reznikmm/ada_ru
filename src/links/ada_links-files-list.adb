with Xml.Write;
with Ada.Text_IO;
with Tools.Xml_Awares;
with Ada_Links.Files.Sf;
with Ada_Links.Files.File;
with Ada_Links.Files.Regexp;
with Tools.Embeded_Link_Lists;


package body Ada_Links.Files.List is

   function "=" (Left, Right : File_Node_Ptr) return Boolean;

   function  Get_Next (Item : File_Node_Ptr) return File_Node_Ptr;
   procedure Set_Next (Item, Next : File_Node_Ptr);

   procedure Save
     (Item : in     File_Node_Ptr;
      File : in out Xml.Write.Xml_Document);

   procedure Search_Version
     (Item : File_Node_Ptr);

   package File_Lists is
      new Tools.Embeded_Link_Lists (File_Node'Class, File_Node_Ptr);


   All_Files : File_Lists.List;

   use File_Lists;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : File_Node_Ptr) return Boolean is
   begin
      return Id (Left.all) = Id (Right.all);
   end "=";

   --------------
   -- Get_Next --
   --------------

   function Get_Next (Item : File_Node_Ptr) return File_Node_Ptr is
   begin
      return Item.Next;
   end Get_Next;

   --------------------
   -- Read_From_File --
   --------------------

   procedure Read_From_File (Name : String) is
   begin
      Tools.Xml_Awares.Go (Name);
   end Read_From_File;

   ----------
   -- Save --
   ----------

   procedure Save
     (Item : in     File_Node_Ptr;
      File : in out Xml.Write.Xml_Document)
   is
      use Xml.Write;
   begin
      Add_Element   (File, "version");
      Add_Attribute (File, "id",      Id (Item.all));
      Add_Attribute (File, "url",     URL (Item.all));
      Add_Attribute (File, "version", Version (Item.all));
      Add_Attribute (File, "file",    File_Name (Item.all));
      Add_Attribute (File, "size",    Size (Item.all));
      Close_Element (File);
   end Save;

   ------------------
   -- Save_To_File --
   ------------------

   procedure Save_To_File (Name : String) is
      use Xml.Write;
      use Ada.Text_IO;

      procedure Save_All is new
        For_Each_With_Param (Xml_Document, Save);

      Output : Xml_Document;
      File   : File_Type;
   begin
      Create_Xml (Output, "utf-8");
      Add_Element (Output, "versions");
      Save_All (All_Files, Output);
      Close_Element (Output);
      Create (File, Name => Name);
      Put_Line (File, To_String (Output));
      Close (File);
   end Save_To_File;

   --------------------
   -- Search_Version --
   --------------------

   procedure Search_Version
     (Item : File_Node_Ptr) is
   begin
      Search_Version (Item.all);
   end Search_Version;

   ---------------------
   -- Search_Versions --
   ---------------------

   procedure Search_Versions is
      procedure Search_All is new
        For_Each (Search_Version);
   begin
      Search_All (All_Files);
   end Search_Versions;

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next (Item, Next : File_Node_Ptr) is
   begin
      Item.Next := Next;
   end Set_Next;

   ----------------------
   -- Loading xml file --
   ----------------------

   use Tools.Xml_Awares;

   type File_Xml_Aware is new Xml_Aware with record
      File_Type : U.Unbounded_String;
      Id        : U.Unbounded_String;
      Url       : U.Unbounded_String;
      Regexp    : U.Unbounded_String;
      Project   : U.Unbounded_String;
   end record;

   procedure Set_Value
     (X     : in out File_Xml_Aware;
      Name  :        String;
      Value :        String);

   type Other_Xml_Aware is new Xml_Aware with null record;

   procedure Set_Subitem
     (X       : in out Other_Xml_Aware;
      Name    :        String;
      Subitem :        Xml_Aware_Ptr);

   procedure Set_Value
     (X     : in out Other_Xml_Aware;
      Name  :        String;
      Value :        String);

   function Create_File (Name : String) return Xml_Aware_Ptr;
   function Create_Other (Name : String) return Xml_Aware_Ptr;

   Last_Id : U.Unbounded_String;
   Other   : aliased Other_Xml_Aware;

   procedure Set_Subitem
     (X       : in out Other_Xml_Aware;
      Name    :        String;
      Subitem :        Xml_Aware_Ptr)
   is
      use U;
   begin
      if Name = "download" then
         declare
            Download : File_Xml_Aware renames File_Xml_Aware (Subitem.all);
         begin
            if Download.File_Type = "file" then
               Add (All_Files,
                    File.Create (Id  => To_String (Download.Id),
                                 URL => To_String (Download.Url)));
            elsif Download.File_Type = "regexp" then
               Add (All_Files,
                    Regexp.Create (Id     => To_String (Download.Id),
                                   Index  => To_String (Download.Url),
                                   Regexp => To_String (Download.Regexp)));
            elsif Download.File_Type = "sf" then
               Add (All_Files,
                    Sf.Create (Id      => To_String (Download.Id),
                               Index   => To_String (Download.Url),
                               Regexp  => To_String (Download.Regexp),
                               Project => To_String (Download.Project)));
            end if;
         end;
      end if;
   end Set_Subitem;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (X     : in out File_Xml_Aware;
      Name  :        String;
      Value :        String)
   is
      use U;
   begin
      if Name = "type" then
         X.Id := Last_Id;
         X.File_Type := To_Unbounded_String (Value);
      elsif Name = "url" then
         X.Url := To_Unbounded_String (Value);
      elsif Name = "regexp" then
         X.Regexp := To_Unbounded_String (Value);
      elsif Name = "project" then
         X.Project := To_Unbounded_String (Value);
      end if;
   end Set_Value;

   procedure Set_Value
     (X     : in out Other_Xml_Aware;
      Name  :        String;
      Value :        String) is
   begin
      if Name = "id" then
         Last_Id := U.To_Unbounded_String (Value);
      end if;
   end Set_Value;

   function Create_File (Name : String) return Xml_Aware_Ptr is
   begin
      return new File_Xml_Aware;
   end Create_File;

   function Create_Other (Name : String) return Xml_Aware_Ptr is
   begin
      return new Other_Xml_Aware;
   end Create_Other;

begin
   Register ("links",    Create_Other'Access);
   Register ("folder",   Create_Other'Access);
   Register ("descr",    Create_Other'Access);
   Register ("link",     Create_Other'Access);
   Register ("file",     Create_Other'Access);
   Register ("download", Create_File'Access);
end Ada_Links.Files.List;

