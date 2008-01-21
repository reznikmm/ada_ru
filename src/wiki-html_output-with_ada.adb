with Ada_Lexer;
with Encodings;
with Text_Streams.Strings;
with Ada.Text_IO;

package body Wiki.HTML_Output.With_Ada is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Data            :    out Context;
      Wiki_URI_Prefix : in     String) is
   begin
      Data.In_Preformated := False;
      Initialize (Data.Parent, Wiki_URI_Prefix);
   end Initialize;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Info : in     Element_Info;
      Data : in out Context)
   is
   begin
      Data.In_Preformated := (Info.Kind = Preformat);

      if Info.Kind /= Preformat then
         Start_Element (Info, Data.Parent);
      end if;
   end Start_Element;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Info : in     Element_Info;
      Data : in out Context)
   is
   begin
      Data.In_Preformated := False;

      if Info.Kind /= Preformat then
         End_Element (Info, Data.Parent);
      end if;
   end End_Element;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Text : in     String;
      Data : in out Context)
   is
   begin
      if Data.In_Preformated and then
        Text'Length > 5 and then
        Text (Text'First .. Text'First + 4) = "#!ada"
      then
         declare
            use Ada_Lexer;
            use Encodings;
            use Ada.Strings.Unbounded;

            Stream : aliased Text_Streams.Strings.String_Text_Stream;
            Lexer  : The_Scaners.Scaner (Stream'Access);
            Buffer : U.Unbounded_String renames Data.Parent.Buffer;
            Next   : Token;
            Span   : Boolean;
         begin
            The_Scaners.Set_Encoding (Lexer, KOI8_R);
            Text_Streams.Strings.Initialize
              (Stream, Text (Text'First + 6 .. Text'Last);

            Buffer := Buffer & "<div class='ada'><pre>";

            loop
               The_Scaners.Next_Token (Lexer, Next);

               exit when Next = End_Of_File;


               case Next is
                  when Abort_Token .. Xor_Token =>
                     Buffer := Buffer & "<span class='keyword'>";
                     Span := True;
                  when Comment =>
                     Buffer := Buffer & "<span class='comment'>";
                     Span := True;
                  when Choose_Numeric_Literal |
                    String_Literal_Token |
                    Character_Literal_Token
                    =>
                     Buffer := Buffer & "<span class='literal'>";
                     Span := True;
                  when Error =>
                     Buffer := Buffer & "<span class='error'>!!!Error!!!";
                     Span := True;
                  when others =>
                     null;
               end case;

               Buffer := Buffer &
                 Clean (Encode (The_Scaners.Token_Image (Lexer), KOI8_R));

               if Span then
                  Span := False;
                  Buffer := Buffer & "</span>";
               end if;

               exit when Next = Error;
            end loop;

            Buffer := Buffer & "</pre></div>";
         end;
      else
         Data.Parent.Buffer := Data.Parent.Buffer & "<pre>";
         Characters (Text, Data.Parent);
         Data.Parent.Buffer := Data.Parent.Buffer & "</pre>";
      end if;
   end Characters;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Data : Context) return String is
   begin
      return Get_Text (Data.Parent);
   end Get_Text;

end Wiki.HTML_Output.With_Ada;
