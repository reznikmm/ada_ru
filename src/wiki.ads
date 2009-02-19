with Ada.Strings.Unbounded;

package Wiki is

   type Element_Kinds is
     (Special_Format, Preformat, Bold_Italic, Bold, Italic, Underline,
      Strike, Superscript, Subscript, Monospace, Monospace_2,
      Heading_3, Heading_2, Heading_1,
      Paragraph, Break, Ordered_List, Numbered_List, List_Item,
      Table_Row, Table_Cell, HTTP_Link, Boxed_Wiki_Link, Boxed_Link,
      Horizontal_Line, Anchor);

   subtype Heading is Element_Kinds range Heading_3 .. Heading_1;

   type Element_Info (Kind : Element_Kinds) is record
      case Kind is
         when Heading =>
            Heading_Id : Ada.Strings.Unbounded.Unbounded_String;
         when Table_Row =>
            Table_Boundary : Boolean := False;
         when HTTP_Link
           | Boxed_Link
           | Boxed_Wiki_Link =>
            Link  : Ada.Strings.Unbounded.Unbounded_String;
            Title : Ada.Strings.Unbounded.Unbounded_String;
         when Anchor =>
            Anchor_Name : Ada.Strings.Unbounded.Unbounded_String;
         when Special_Format =>
            Format   : Ada.Strings.Unbounded.Unbounded_String;
            Argument : Ada.Strings.Unbounded.Unbounded_String;
         when others =>
            null;
      end case;
   end record;

end Wiki;
