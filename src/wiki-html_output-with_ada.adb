with Ada_Ua;
with Encodings;
with Text_Streams.Strings;

package body Wiki.HTML_Output.With_Ada is

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
         HTML_Output.Start_Element (Info, HTML_Output.Context (Data));
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
         HTML_Output.End_Element (Info, HTML_Output.Context (Data));
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
      if Data.In_Preformated then
         declare
            use Ada_Ua;
            use Encodings;
            use Ada.Strings.Unbounded;

            Stream : aliased Text_Streams.Strings.String_Text_Stream;
            Lexer  : The_Scaners.Scaner (Stream'Access);
            Next   : Token;
            Span   : Boolean;
         begin
            Text_Streams.Strings.Initialize (Stream, Text);
            Data.Buffer := Data.Buffer & "<div class='ada'>";

            loop
               The_Scaners.Next_Token (Lexer, Next);
               exit when Next = End_Of_File;

               if Next in Abort_Token .. Xor_Token then
                  Span := True;
                  Data.Buffer := Data.Buffer & "<span class='keyword'>";
               end if;

               Data.Buffer := Data.Buffer &
                 Clean (Encode (The_Scaners.Token_Image (Lexer), KOI8_R));

               if Span then
                  Span := False;
                  Data.Buffer := Data.Buffer & "</span>";
               end if;
            end loop;

            Data.Buffer := Data.Buffer & "</div>";
         end;
      else
         HTML_Output.Characters (Text, HTML_Output.Context (Data));
      end if;
   end Characters;

end Wiki.HTML_Output.With_Ada;
