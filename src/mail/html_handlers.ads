with XML.SAX.Content_Handlers;
with XML.SAX.Input_Sources.Streams.Files;

with League.Strings;

package HTML_Handlers is
   type My_Source is new XML.SAX.Input_Sources.Streams.Files.File_Input_Source
   with null record;

   type HTML_Handler is new XML.SAX.Content_Handlers.SAX_Content_Handler
   with record
      Result : League.Strings.Universal_String;
   end record;

   overriding function Error_String
    (Self : HTML_Handler) return League.Strings.Universal_String;

   overriding procedure Characters
    (Self    : in out HTML_Handler;
     Text    : League.Strings.Universal_String;
     Success : in out Boolean);

end HTML_Handlers;
