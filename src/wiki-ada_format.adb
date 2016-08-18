with Ada_Lexer;
with Encodings;
with Text_Streams.Strings;
with Ada.Strings.Unbounded;
with Wiki.HTML_Output;

function Wiki.Ada_Format (Text : String; Arg : Special_Formats.Argument_List)
   return String
is
   pragma Unreferenced (Arg);
   use Ada_Lexer;
   use Encodings;
   use Ada.Strings.Unbounded;

   Stream : aliased Text_Streams.Strings.String_Text_Stream;
   Lexer  : The_Scaners.Scaner (Stream'Access);
   Buffer : Unbounded_String;
   Next   : Token;
   Span   : Boolean;
begin
   The_Scaners.Set_Encoding (Lexer, UTF_8);
   Text_Streams.Strings.Initialize (Stream, Text);

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

      Buffer := Buffer & Wiki.HTML_Output.Clean
        (Encode (The_Scaners.Token_Image (Lexer), UTF_8));

      if Span then
         Span := False;
         Buffer := Buffer & "</span>";
      end if;

      exit when Next = Error;
   end loop;

   Buffer := Buffer & "</pre></div>";

   return To_String (Buffer);
end Wiki.Ada_Format;
