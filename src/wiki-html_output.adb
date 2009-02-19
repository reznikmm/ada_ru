with Wiki.Parser;
with Ada.Text_IO;

package body Wiki.HTML_Output is
   use Ada.Strings.Unbounded;
   use type Special_Formats.Special_Formatter;


   function Link (Ref : String; Title : String; Data : Context) return String;
   --  make <a> or <img> element

   function "+" (Text : U.Unbounded_String) return String
     renames U.To_String;

   function "+" (Text : String) return Unbounded_String
     renames U.To_Unbounded_String;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Info : in     Element_Info;
      Data : in out Context) is
   begin
      case Info.Kind is
         when Special_Format =>
            declare
               use Special_Formats;
               Formatter : constant Special_Formatter := Get (+Info.Format);
            begin
               if Formatter = null then
                  Start_Element ((Kind => Preformat), Data);
               else
                  Data.Special_Format := Formatter;
                  Data.Special_Argument := Info.Argument;
               end if;
            end;
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
            Data.In_Mono := True;
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
         when Anchor =>
            Data.Buffer := Data.Buffer & "<a name='" & Info.Anchor_Name & "'>";
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
         when Special_Format =>
            if Data.Special_Format = null then
               End_Element ((Kind => Preformat), Data);
            else
               Data.Special_Format := null;
            end if;
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
            Data.In_Mono := False;
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
         when Anchor =>
            Data.Buffer := Data.Buffer & "</a>";
      end case;
   end End_Element;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Text : in     String;
      Data : in out Context)
   is
      use type Special_Formats.Strings;
   begin
      if Data.Special_Format = null then
         Data.Buffer := Data.Buffer & Clean (Text, Data.In_Mono);
      else
         Data.Buffer := Data.Buffer & Data.Special_Format
           (Text, (Data.Arguments.Length + 1,
                   ((+"arg") & Data.Arguments.Names),
                   (Data.Special_Argument & Data.Arguments.Values)));
      end if;
   end Characters;

   -----------
   -- Clean --
   -----------

   function Clean (Text : String; Space : Boolean := False) return String is
      use Wiki.Parser;
      Clean_Text : constant String :=
        Replace (Replace (Replace (Text,
                                   "&", "&amp;"),
                          "<", "&lt;"),
                 ">", "&gt;");
   begin
      if Space then
         return Replace (Clean_Text, " ", "&nbsp;");
      else
         return Clean_Text;
      end if;
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
      Wiki_URI_Prefix : in     String;
      Arguments       : in     Argument_List := Null_Arguments)
   is
      use Ada.Strings.Unbounded;
   begin
      Data.Wiki_URI  := To_Unbounded_String (Wiki_URI_Prefix);
      Data.Buffer    := Null_Unbounded_String;
      Data.In_Mono   := False;
      Data.Special_Format := null;
      Data.Arguments := Arguments;
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
