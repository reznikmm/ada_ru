with Wiki.Special_Formats;

package Wiki.Database_Format is

   function DB_To_Text (Text : String; Arg : Special_Formats.Argument_List)
                       return String;

   function DB_To_XSL (Text : String; Arg : Special_Formats.Argument_List)
                       return String;

end Wiki.Database_Format;
