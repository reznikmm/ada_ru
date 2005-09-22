with Ada.Text_IO;
with Ada.Command_Line;
with Ada_Links.Downloads;

package body Ada_Links.Files.Sf is

   ------------
   -- Create --
   ------------

   function Create
     (Id      : String;
      Index   : String;
      Regexp  : String;
      Project : String)
      return File_Node_Ptr
   is
   begin
      return
        new Sourceforge_File_Node'(Next    => null,
                                   Id      => U.To_Unbounded_String (Id),
                                   URL     => U.Null_Unbounded_String,
                                   Version => U.Null_Unbounded_String,
                                   File    => U.Null_Unbounded_String,
                                   Index   => U.To_Unbounded_String (Index),
                                   Regexp  => U.To_Unbounded_String (Regexp),
                                   Project => U.To_Unbounded_String (Project),
                                   Size    => U.Null_Unbounded_String);
   end Create;

   function File_Name (Object : Sourceforge_File_Node) return String is
   begin
      return U.To_String (Object.File);
   end File_Name;

   --------------------
   -- Search_Version --
   --------------------

   procedure Search_Version
     (Object : in out Sourceforge_File_Node)
   is
      use U;
      Page   : String := Downloads.Download_Page (To_String (Object.Index));
      Ver    : Unbounded_String;
      Url    : Unbounded_String;
      File   : Unbounded_String;
      Prefix : constant String := "http://prdownloads.sourceforge.net/"
        & To_String (Object.Project) & "/";
      Suffix : constant String := "\?download";
      Regexp : String := Prefix & To_String (Object.Regexp) & Suffix;
      Broken_Page : exception;
      SF_Net : constant String := Ada.Command_Line.Argument (3);
   begin
      Find_Max_Version (Page, Regexp, Ver, Url);
      if Ver = Null_Unbounded_String then
         Ada.Text_IO.Put_Line ("Failed to found version for " &
                               To_String (Object.Id) &
                               ":" &
                               Page);
         raise Broken_Page;
      end if;
      Object.File    := To_Unbounded_String
        (Slice (Url, Prefix'Length + 1, Length (Url) - Suffix'Length + 1));
      Object.Url     := Url;
      Object.Version := Ver;
      File := SF_Net
--        "http://belnet.dl.sourceforge.net/sourceforge/"
--        "http://easynews.dl.sourceforge.net/sourceforge/"
--        "http://mesh.dl.sourceforge.net/sourceforge/"
         & To_String (Object.Project) & "/" & Object.File;
      Object.Size :=
         To_Unbounded_String (Downloads.Get_Size (To_String (File)));
   end Search_Version;

end Ada_Links.Files.Sf;

