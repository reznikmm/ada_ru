with Wiki.Parser;

package body Wiki.HTML_Output is
   use Ada.Strings.Unbounded;

   function Clean (Text : String) return String;
   --  mangle html entities

   function Link (Ref : String; Title : String; Data : Context) return String;
   --  make <a> or <img> element

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Info : in     Element_Info;
      Data : in out Context) is
   begin
      case Info.Kind is
         when Preformat =>
            Data.Buffer := Data.Buffer & "<pre>";
         when Bold_Italic =>
            Data.Buffer := Data.Buffer & "<strong><i>";
         when Bold =>
            Data.Buffer := Data.Buffer & "<strong>";
         when Italic =>
            Data.Buffer := Data.Buffer & "<i>";
         when Underline =>
            Data.Buffer := Data.Buffer & "<span class='underline'>";
         when Monospace | Monospace_2 =>
            Data.Buffer := Data.Buffer & "<tt>";
         when Strike =>
            Data.Buffer := Data.Buffer & "<del>";
         when Superscript =>
            Data.Buffer := Data.Buffer & "<sup>";
         when Subscript =>
            Data.Buffer := Data.Buffer & "<sub>";
         when Heading_1 =>
            Data.Buffer := Data.Buffer & "<h1 id='" & Info.Heading_Id & "'>";
         when Heading_2 =>
            Data.Buffer := Data.Buffer & "<h2 id='" & Info.Heading_Id & "'>";
         when Heading_3 =>
            Data.Buffer := Data.Buffer & "<h3 id='" & Info.Heading_Id & "'>";
         when Paragraph =>
            Data.Buffer := Data.Buffer & "<p>";
         when Break =>
            Data.Buffer := Data.Buffer & "<br/>";
         when Ordered_List =>
            Data.Buffer := Data.Buffer & "<ul>";
         when Numbered_List =>
            Data.Buffer := Data.Buffer & "<ol>";
         when List_Item =>
            Data.Buffer := Data.Buffer & "<li>";
         when Table_Row =>
            if Info.Table_Boundary then
               Data.Buffer := Data.Buffer & "<table class='wiki'>";
            end if;

            Data.Buffer := Data.Buffer & "<tr>";
         when Table_Cell =>
            Data.Buffer := Data.Buffer & "<td>";
         when HTTP_Link | Boxed_Link | Boxed_Wiki_Link =>
            Data.Buffer := Data.Buffer &
              Link (To_String (Info.Link), To_String (Info.Title), Data);
         when Horizontal_Line =>
            Data.Buffer := Data.Buffer & "<hr/>";
      end case;
   end Start_Element;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Info : in     Element_Info;
      Data : in out Context) is
   begin
      case Info.Kind is
         when Preformat =>
            Data.Buffer := Data.Buffer & "</pre>";
         when Bold_Italic =>
            Data.Buffer := Data.Buffer & "</i></strong>";
         when Bold =>
            Data.Buffer := Data.Buffer & "</strong>";
         when Italic =>
            Data.Buffer := Data.Buffer & "</i>";
         when Underline =>
            Data.Buffer := Data.Buffer & "</span>";
         when Monospace | Monospace_2 =>
            Data.Buffer := Data.Buffer & "</tt>";
         when Strike =>
            Data.Buffer := Data.Buffer & "</del>";
         when Superscript =>
            Data.Buffer := Data.Buffer & "</sup>";
         when Subscript =>
            Data.Buffer := Data.Buffer & "</sub>";
         when Heading_1 =>
            Data.Buffer := Data.Buffer & "</h1>";
         when Heading_2 =>
            Data.Buffer := Data.Buffer & "</h2>";
         when Heading_3 =>
            Data.Buffer := Data.Buffer & "</h3>";
         when Paragraph =>
            Data.Buffer := Data.Buffer & "</p>";
         when Break =>
            null;
         when Ordered_List =>
            Data.Buffer := Data.Buffer & "</ul>";
         when Numbered_List =>
            Data.Buffer := Data.Buffer & "</ol>";
         when List_Item =>
            Data.Buffer := Data.Buffer & "</li>";
         when Table_Row =>
            Data.Buffer := Data.Buffer & "</tr>";

            if Info.Table_Boundary then
               Data.Buffer := Data.Buffer & "</table>";
            end if;
         when Table_Cell =>
            Data.Buffer := Data.Buffer & "</td>";
         when HTTP_Link | Boxed_Link | Boxed_Wiki_Link =>
            null;
         when Horizontal_Line =>
            null;
      end case;
   end End_Element;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Text : in     String;
      Data : in out Context)
   is
   begin
      Data.Buffer := Data.Buffer & Clean (Text);
   end Characters;

   -----------
   -- Clean --
   -----------

   function Clean (Text : String) return String is
      use Wiki.Parser;
   begin
      return Replace (Replace (Replace (Text,
                                        "&", "&amp;"),
                               "<", "&lt;"),
                      ">", "&gt;");
   end Clean;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Data : Context) return String is
   begin
      return To_String (Data.Buffer);
   end Get_Text;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Data            :    out Context;
      Wiki_URI_Prefix : in     String)
   is
      use Ada.Strings.Unbounded;
   begin
      Data.Wiki_URI := To_Unbounded_String (Wiki_URI_Prefix);
      Data.Buffer   := Null_Unbounded_String;
   end Initialize;

   ----------
   -- Link --
   ----------

   function Link
     (Ref   : String;
      Title : String;
      Data  : Context) return String is
   begin
      if Ref'Length > 5
        and then Ref (Ref'First ..  Ref'First + 4) = "wiki:"
      then
         declare
            Path : constant String := Ref (Ref'First + 5 .. Ref'Last);
         begin
            if Title = "" then
               return Link (Ref   => To_String (Data.Wiki_URI) & Path,
                            Title => Path,
                            Data  => Data);
            else
               return Link (Ref   => To_String (Data.Wiki_URI) & Path,
                            Title => Title,
                            Data  => Data);
            end if;
         end;
      end if;

      if Ref'Length > 4 and then
        (Ref (Ref'Last - 3 .. Ref'Last) = ".png"
         or Ref (Ref'Last - 3 .. Ref'Last) = ".gif"
         or Ref (Ref'Last - 3 .. Ref'Last) = ".jpg")
      then
         if Title /= "" then
            return "<img src='" & Ref & "' alt='" & Title & "'/>";
         else
            return "<img src='" & Ref & "' alt='" & Ref & "'/>";
         end if;
      elsif Title /= "" then
         return "<a href='" & Ref & "'>" & Title & "</a>";
      else
         return "<a href='" & Ref & "'>" & Ref & "</a>";
      end if;
   end Link;

end Wiki.HTML_Output;
