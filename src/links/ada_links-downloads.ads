package Ada_Links.Downloads is

   subtype File_Text is String;

   function Download_Page (Url : String) return File_Text;

   function Get_Size (Url : String) return String;

end Ada_Links.Downloads;
