with Ada_Links.Downloads;
with Ada.Text_IO;

package body Ada_Links.Files.Regexp is

   ------------
   -- Create --
   ------------

   function Create
     (Id     : String;
      Index  : String;
      Regexp : String)
      return File_Node_Ptr
   is
   begin
      return new Regexp_File_Node'(Next    => null,
                                   Id      => U.To_Unbounded_String (Id),
                                   URL     => U.Null_Unbounded_String,
                                   Version => U.Null_Unbounded_String,
                                   Index   => U.To_Unbounded_String (Index),
                                   Regexp  => U.To_Unbounded_String (Regexp),
                                   Size    => U.Null_Unbounded_String);
   end Create;

   --------------------
   -- Search_Version --
   --------------------

   procedure Search_Version
     (Object : in out Regexp_File_Node)
   is
      use U;
      Page : String := Downloads.Download_Page (To_String (Object.Index));
      Ver  : Unbounded_String;
      Url  : Unbounded_String;
      Broken_Page : exception;
   begin
      Find_Max_Version (Page, To_String (Object.Regexp), Ver, Url);
      if Ver = Null_Unbounded_String then
         Ada.Text_IO.Put_Line ("Failed to found version for " &
                               To_String (Object.Id) &
                               ":" &
                               Page);
         raise Broken_Page;
      end if;
      Object.Url     := Normalize_Link (Object.Index, Url);
      Object.Version := Ver;
   end Search_Version;

end Ada_Links.Files.Regexp;

