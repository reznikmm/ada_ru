--  This tool is used to import mail from Yahoo Groups or email file.
--
--  1. Download a message from Yahoo Group using this URL:
--  https://groups.yahoo.com/api/v1/groups/ada_ru/messages/XXX/raw
--  2. Download the same message from Yahoo Group in HTML form
--  https://groups.yahoo.com/api/v1/groups/ada_ru/messages/XXX
--  where XXX is message number.
--  3. Run `import_mail (1-file) (2-file)`
--
--  To import email run just `import_mail (email-file)`

with Ada.Command_Line;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Streams.Stream_IO;
with Ada.Wide_Wide_Text_IO;

with GNAT.OS_Lib;

with League.Application;
with League.Base_Codecs;
with League.Calendars.ISO_8601;
with League.Characters.Latin;
with League.Holders;
with League.Holders.Integers;
with League.JSON.Documents;
with League.JSON.Objects;
with League.JSON.Values;
with League.Stream_Element_Vectors;
with League.Strings.Internals;
with League.Strings;
with League.Strings.Hash;
with League.String_Vectors;
with League.Text_Codecs;

with XML.SAX.Simple_Readers;

with SQL.Options;
with SQL.Databases;
with SQL.Queries;

with Matreshka.Internals.SQL_Drivers.PostgreSQL.Factory;

with Matreshka.Internals.Strings;
with Matreshka.Internals.Text_Codecs;

with HTML_Handlers;

procedure Import_Mail is

   function "+"
     (Text : Wide_Wide_String) return League.Strings.Universal_String
      renames League.Strings.To_Universal_String;

   function "**"
     (Left, Right : League.Strings.Universal_String) return Boolean;
   --  Case insensetive comparison operator

   function Read_File
     (Name : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function Read_File
     (Name : League.Strings.Universal_String)
      return League.Stream_Element_Vectors.Stream_Element_Vector;

   function Unpack_JSON
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function Unpack_JSON
     (Text : League.Strings.Universal_String;
      Key  : Wide_Wide_String)
      return League.Strings.Universal_String;

   function Unescape_XML_Entities
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function Strinp_Carriage_Return
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function Decode_Quoted_Printable
     (Encoding : League.Strings.Universal_String;
      Piece    : League.Strings.Universal_String;
      Header   : Boolean)
      return League.Strings.Universal_String;

   function Format_HTML
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function Replace_Placeholders
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function Trim (Text : League.Strings.Universal_String)
                        return League.Strings.Universal_String;

   Placeholder : constant Wide_Wide_Character :=
     Wide_Wide_Character'Val (16#FFFD#);

   Content_Type : constant League.Strings.Universal_String :=
     +"Content-Type";

   Skip_Empty : constant League.Strings.Split_Behavior :=
     League.Strings.Skip_Empty;

   ----------
   -- "**" --
   ----------

   function "**"
     (Left, Right : League.Strings.Universal_String) return Boolean
   is
      use type League.Strings.Universal_String;
   begin
      return Left.To_Lowercase = Right.To_Lowercase;
   end "**";

   -----------------------------
   -- Decode_Quoted_Printable --
   -----------------------------

   function Decode_Quoted_Printable
     (Encoding : League.Strings.Universal_String;
      Piece    : League.Strings.Universal_String;
      Header   : Boolean) return League.Strings.Universal_String
   is
      use type League.Characters.Universal_Character;

      Codec : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec (Encoding);

      Bytes : League.Stream_Element_Vectors.Stream_Element_Vector;
      Index : Positive := 1;
      Item  : Ada.Streams.Stream_Element;
   begin
      while Index <= Piece.Length loop
         case Piece.Element (Index).To_Wide_Wide_Character is
            when '=' =>
               if Piece.Element (Index + 1) = League.Characters.Latin.Line_Feed
                 or else Piece.Element (Index + 1) =
                            League.Characters.Latin.Carriage_Return
               then
                  Index := Index + 2;
               else
                  Item := Ada.Streams.Stream_Element'Wide_Wide_Value
                    ("16#"
                     & Piece.Slice
                       (Index + 1, Index + 2).To_Wide_Wide_String
                     & "#");

                  Bytes.Append (Item);
                  Index := Index + 3;
               end if;
            when '_' =>
               if Header then
                  Item := Character'Pos (' ');
               else
                  Item := Character'Pos ('_');
               end if;
               Bytes.Append (Item);
               Index := Index + 1;

            when others =>
               Item := Wide_Wide_Character'Pos
                 (Piece.Element (Index).To_Wide_Wide_Character);

               Bytes.Append (Item);
               Index := Index + 1;
         end case;

      end loop;

      return Codec.Decode (Bytes);
   end Decode_Quoted_Printable;

   -----------------
   -- Format_HTML --
   -----------------

   function Format_HTML
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is

      Output : Ada.Wide_Wide_Text_IO.File_Type;
      Arg_0  : aliased String := "--dropdtd";
      Arg_1  : aliased String := "--html";
      Arg_2  : aliased String := "--xmlout";
      Arg_3  : aliased String := "--output";
      Arg_4  : aliased String := "/tmp/msg.xml";
      Arg_5  : aliased String := "/tmp/msg.html";
      Args   : constant GNAT.OS_Lib.String_List :=
        (Arg_0'Unchecked_Access,
         Arg_1'Unchecked_Access,
         Arg_2'Unchecked_Access,
         Arg_3'Unchecked_Access,
         Arg_4'Unchecked_Access,
         Arg_5'Unchecked_Access);
      Ok : Boolean;

      Source  : aliased HTML_Handlers.My_Source;
      Reader  : aliased XML.SAX.Simple_Readers.Simple_Reader;
      Handler : aliased HTML_Handlers.HTML_Handler;

   begin
      Ada.Wide_Wide_Text_IO.Create
        (File => Output,
         Name => Arg_5,
         Form => "WCEM=8");
      Ada.Wide_Wide_Text_IO.Put_Line (Output, Text.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Close (Output);

      GNAT.OS_Lib.Spawn ("/usr/bin/xmllint", Args, Ok);

      pragma Assert (Ok);

      Reader.Set_Content_Handler (Handler'Unchecked_Access);

      Source.Open_By_File_Name (League.Strings.From_UTF_8_String (Arg_4));
      Reader.Parse (Source'Access);

      return Handler.Result;
   end Format_HTML;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (Name : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is

      Buffer  : Ada.Streams.Stream_Element_Array (1 .. 1024);
      Last    : Ada.Streams.Stream_Element_Offset;
      File    : Ada.Streams.Stream_IO.File_Type;
      Data    : Matreshka.Internals.Strings.Shared_String_Access
        := Matreshka.Internals.Strings.Shared_Empty'Access;
      Factory : constant Matreshka.Internals.Text_Codecs.Decoder_Factory
        := Matreshka.Internals.Text_Codecs.Decoder
          (Matreshka.Internals.Text_Codecs.MIB_UTF8);
      Decoder : Matreshka.Internals.Text_Codecs.Abstract_Decoder'Class
        := Factory (Matreshka.Internals.Text_Codecs.Raw);

   begin
      Ada.Streams.Stream_IO.Open
        (File, Ada.Streams.Stream_IO.In_File, Name.To_UTF_8_String);

      while not Ada.Streams.Stream_IO.End_Of_File (File) loop
         Ada.Streams.Stream_IO.Read (File, Buffer, Last);
         Decoder.Decode_Append (Buffer (Buffer'First .. Last), Data);
      end loop;

      Ada.Streams.Stream_IO.Close (File);

      if Decoder.Is_Error then
         Matreshka.Internals.Strings.Dereference (Data);

         raise Constraint_Error with "mailformed UTF-8 file";
      end if;

      return League.Strings.Internals.Wrap (Data);
   end Read_File;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (Name : League.Strings.Universal_String)
      return League.Stream_Element_Vectors.Stream_Element_Vector
   is
      Result : League.Stream_Element_Vectors.Stream_Element_Vector;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1024);
      Last   : Ada.Streams.Stream_Element_Offset;
      File   : Ada.Streams.Stream_IO.File_Type;

   begin
      Ada.Streams.Stream_IO.Open
        (File, Ada.Streams.Stream_IO.In_File, Name.To_UTF_8_String);

      while not Ada.Streams.Stream_IO.End_Of_File (File) loop
         Ada.Streams.Stream_IO.Read (File, Buffer, Last);
         Result.Append (Buffer (Buffer'First .. Last));
      end loop;

      Ada.Streams.Stream_IO.Close (File);
      return Result;
   end Read_File;

   --------------------------
   -- Replace_Placeholders --
   --------------------------

   function Replace_Placeholders
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      function Get_Next_Char return Wide_Wide_Character;
      procedure Open_Input;

      Index : Positive := 1;
      Input : League.Strings.Universal_String;

      -------------------
      -- Get_Next_Char --
      -------------------

      function Get_Next_Char return Wide_Wide_Character is
         Result : Wide_Wide_Character;
      begin
         while Index <= Input.Length loop
            Result := Input (Index).To_Wide_Wide_Character;
            Index := Index + 1;

            exit when Wide_Wide_Character'Pos (Result) > 127;
         end loop;

         return Result;
      end Get_Next_Char;

      ----------------
      -- Open_Input --
      ----------------

      procedure Open_Input is
         Text : constant League.Strings.Universal_String :=
           Read_File (League.Application.Arguments.Element (2));
      begin
         Input := Unpack_JSON (Text, "messageBody");
         Ada.Wide_Wide_Text_IO.Put_Line
           (League.Application.Arguments.Element (2).To_Wide_Wide_String);
      end Open_Input;

      Result : League.Strings.Universal_String;
      Char   : Wide_Wide_Character;
   begin
      if Text.Index (Placeholder) = 0 then
         return Text;
      end if;

      Open_Input;

      for J in 1 .. Text.Length loop
         Char := Text.Element (J).To_Wide_Wide_Character;

         if Wide_Wide_Character'Pos (Char) <= 127 then
            Result.Append (Char);
         elsif Char = Placeholder then
            Result.Append (Get_Next_Char);
         else
            Result.Append (Get_Next_Char);
            Result.Append (Get_Next_Char);
         end if;

      end loop;

      return Result;
   end Replace_Placeholders;

   ----------------------------
   -- Strinp_Carriage_Return --
   ----------------------------

   function Strinp_Carriage_Return
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      List   : constant League.String_Vectors.Universal_String_Vector :=
        Text.Split (League.Characters.Latin.Carriage_Return, Skip_Empty);
   begin
      return List.Join ("");
   end Strinp_Carriage_Return;

   ----------
   -- Trim --
   ----------

   function Trim (Text : League.Strings.Universal_String)
                        return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String := Text;
   begin
      while Result.Ends_With (" ") loop
         Result := Result.Head (Result.Length - 1);
      end loop;

      return Result;
   end Trim;

   ---------------------------
   -- Unescape_XML_Entities --
   ---------------------------

   function Unescape_XML_Entities
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String;
      List   : constant League.String_Vectors.Universal_String_Vector :=
        Text.Split ('&');
   begin
      for J in 1 .. List.Length loop
         declare
            Piece : constant League.Strings.Universal_String
              := List.Element (J);
         begin
            if Piece.Starts_With ("lt;") then
               Result.Append ("<");
               Result.Append (Piece.Slice (4, Piece.Length));
            elsif Piece.Starts_With ("gt;") then
               Result.Append (">");
               Result.Append (Piece.Slice (4, Piece.Length));
            elsif Piece.Starts_With ("quot;") then
               Result.Append ("""");
               Result.Append (Piece.Slice (6, Piece.Length));
            elsif Piece.Starts_With ("#39;") then
               Result.Append ("'");
               Result.Append (Piece.Slice (5, Piece.Length));
            elsif J = 1 then
               Result.Append (Piece);
            else
               Result.Append ("&");
               Result.Append (Piece);
            end if;
         end;
      end loop;

      return Result;
   end Unescape_XML_Entities;

   -----------------
   -- Unpack_JSON --
   -----------------

   function Unpack_JSON
     (Text : League.Strings.Universal_String;
      Key  : Wide_Wide_String)
      return League.Strings.Universal_String
   is
      D : constant League.JSON.Documents.JSON_Document :=
        League.JSON.Documents.From_JSON (Text);
      O : constant League.JSON.Objects.JSON_Object := D.To_JSON_Object;
      V : constant League.JSON.Values.JSON_Value := O.Value
        (League.Strings.To_Universal_String ("ygData"));
      V2 : constant League.JSON.Values.JSON_Value := V.To_Object.Value
        (League.Strings.To_Universal_String (Key));
   begin
      return V2.To_String;
   end Unpack_JSON;

   -----------------
   -- Unpack_JSON --
   -----------------

   function Unpack_JSON
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      --  https://groups.yahoo.com/api/v1/groups/ada_ru/messages/7320/raw
   begin
      return Unpack_JSON (Text, "rawEmail");
   end Unpack_JSON;

   package Messages is
      type Message is tagged private;

      type Message_Array is array (Positive range <>) of Message;

      function Read_Message
        (Text : League.Strings.Universal_String;
         Data : Ada.Streams.Stream_Element_Array := (1 .. 0 => 0))
           return Message;

      function Header
        (Self : Message;
         Name : League.Strings.Universal_String)
         return League.Strings.Universal_String;
      --  Only first header is returned if there are more then one with
      --  given name

      function Header
        (Self  : Message;
         Name  : League.Strings.Universal_String;
         Field : League.Strings.Universal_String)
         return League.Strings.Universal_String;
      --  The same but return only one field of header

      function Get_Body_As_Text
        (Self : Message) return League.Strings.Universal_String;

      function Get_Body_As_Text
        (Self    : Message;
         Charset : League.Strings.Universal_String)
           return League.Strings.Universal_String;

      function Nested_Parts
        (Self     : Message;
         Boundary : League.Strings.Universal_String)
         return Message_Array;

      function Decode_Base64_Body
        (Self     : Message)
         return League.Stream_Element_Vectors.Stream_Element_Vector;

      function Guess_Charset
        (Data : Ada.Streams.Stream_Element_Array)
      return League.Strings.Universal_String;

   private
      package Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => League.Strings.Universal_String,
         Element_Type    => League.Strings.Universal_String,
         Hash            => League.Strings.Hash,
         Equivalent_Keys => League.Strings."=",
         "="             => League.Strings."=");

      type Message is tagged record
         Headers : Maps.Map;
         Text    : League.Strings.Universal_String;
         Data    : League.Stream_Element_Vectors.Stream_Element_Vector;
      end record;

   end Messages;

   package Mails is

      type Mail is tagged record
         From        : League.Strings.Universal_String;
         Date        : League.Calendars.Date_Time;
         In_Reply_To : League.Strings.Universal_String;
         Message_Id  : League.Strings.Universal_String;
         Subject     : League.Strings.Universal_String;
         Text        : League.Strings.Universal_String;
         Is_Flowed   : Boolean := False;
      end record;

      function Read_Mail (Text : League.Strings.Universal_String) return Mail;

      function Read_Mail
        (Data : League.Stream_Element_Vectors.Stream_Element_Vector)
           return Mail;

      function Read_Mail
        (Text : League.Strings.Universal_String;
         Data : Ada.Streams.Stream_Element_Array)
         return Mail;
   end Mails;

   package body Mails is

      function To_Date
        (Text : League.Strings.Universal_String)
         return League.Calendars.Date_Time;

      ---------------
      -- Read_Mail --
      ---------------

      function Read_Mail
        (Text : League.Strings.Universal_String) return Mail
      is
         Empty : Ada.Streams.Stream_Element_Array (1 .. 0);
      begin
         return Read_Mail (Text, Empty);
      end Read_Mail;

      ---------------
      -- Read_Mail --
      ---------------

      function Read_Mail
        (Text : League.Strings.Universal_String;
         Data : Ada.Streams.Stream_Element_Array)
         return Mail
      is
         use type League.Strings.Universal_String;

         procedure Read_Plain_Text
           (Part   : Messages.Message;
            Result : in out Mail);
         --  Read given message when it has Content-Type = text/plain

         procedure Find_Plain_Text
           (Part   : Messages.Message;
            Result : in out Mail;
            Found  : in out Boolean);
         --  Find in given multipart message a plain text part and read it

         Text_HTML : constant League.Strings.Universal_String :=
           +"text/html";
         Text_Plain : constant League.Strings.Universal_String :=
           +"text/plain";
         Multipart_Alternative : constant League.Strings.Universal_String :=
           +"multipart/alternative";
         Multipart_Mixed : constant League.Strings.Universal_String :=
           +"multipart/mixed";
         Multipart_Related : constant League.Strings.Universal_String :=
           +"multipart/related";
         Multipart_Signed : constant League.Strings.Universal_String :=
           +"multipart/signed";
         Content_Transfer_Encoding : constant League.Strings.Universal_String
           := +"Content-Transfer-Encoding";
         Base_64 : constant League.Strings.Universal_String := +"base64";
         Seven_Bit : constant League.Strings.Universal_String := +"7bit";
         Eight_Bit : constant League.Strings.Universal_String := +"8bit";
         Quoted_Printable : constant League.Strings.Universal_String :=
           +"quoted-printable";
         Flowed : constant League.Strings.Universal_String := +"flowed";
         X_Original_From  : constant League.Strings.Universal_String :=
           +"X-Original-From";
         X_Forum_User  : constant League.Strings.Universal_String :=
           +"X-Forum-User";

         function Is_Multipart (CT : League.Strings.Universal_String)
           return Boolean is
             (CT ** Multipart_Mixed
              or else CT in Multipart_Alternative
                            | Multipart_Related
                            | Multipart_Signed);
         --  Check if given Content-Type is kind of multipart

         ---------------------
         -- Find_Plain_Text --
         ---------------------

         procedure Find_Plain_Text
           (Part   : Messages.Message;
            Result : in out Mail;
            Found  : in out Boolean)
         is
            Boundary : constant League.Strings.Universal_String :=
              Part.Header (Content_Type, +"boundary");
            Parts : constant Messages.Message_Array
              := Part.Nested_Parts (Boundary);
            CT : League.Strings.Universal_String;
         begin
            for J in Parts'Range loop
               CT := Parts (J).Header (Content_Type, +"");

               if CT = Text_Plain then
                  Read_Plain_Text (Parts (J), Result);
                  Found := True;
                  exit;
               elsif Is_Multipart (CT) then
                  Find_Plain_Text (Parts (J), Result, Found);
                  exit when Found;
               end if;
            end loop;
         end Find_Plain_Text;

         ---------------------
         -- Read_Plain_Text --
         ---------------------

         procedure Read_Plain_Text
           (Part   : Messages.Message;
            Result : in out Mail)
         is
            CTE      : constant League.Strings.Universal_String :=
              Part.Header (Content_Transfer_Encoding, +"");
            Charset : constant League.Strings.Universal_String :=
              Part.Header (Content_Type, +"charset");
            Format : constant League.Strings.Universal_String :=
              Part.Header (Content_Type, +"format");
         begin
            Result.Is_Flowed := Format ** Flowed;

            if CTE ** Base_64 then
               declare
                  Codec : constant League.Text_Codecs.Text_Codec :=
                    League.Text_Codecs.Codec (Charset);
                  Bytes : constant League.Stream_Element_Vectors
                    .Stream_Element_Vector
                      := Part.Decode_Base64_Body;
               begin
                  Result.Text := Codec.Decode (Bytes);
               end;
            elsif CTE ** Quoted_Printable then
               Result.Text :=
                 Decode_Quoted_Printable
                   (Charset,
                    Part.Get_Body_As_Text,
                    Header => False);

            elsif CTE = Seven_Bit then
               Result.Text := Part.Get_Body_As_Text;

            elsif Data'Length = 0 then
               if CTE.Is_Empty or else CTE ** Eight_Bit then
                  Result.Text := Part.Get_Body_As_Text;
                  --  Bogus encoding in yahoo
                  Result.Text := Replace_Placeholders (Result.Text);
               else
                  raise Constraint_Error
                    with "Unknown CTE " & CTE.To_UTF_8_String;
               end if;
            elsif not Charset.Is_Empty then
               Result.Text := Part.Get_Body_As_Text (Charset);
            else
               raise Constraint_Error with "Empty CTE!";
            end if;
         end Read_Plain_Text;

         Root    : constant Messages.Message :=
           Messages.Read_Message (Text, Data);
         CT      : constant League.Strings.Universal_String :=
           Root.Header (Content_Type, +"");
         CTE      : constant League.Strings.Universal_String :=
           Root.Header (Content_Transfer_Encoding, +"");
         Charset : constant League.Strings.Universal_String :=
           Root.Header (Content_Type, +"charset");
         Result  : Mail;
      begin
         Result.From := Root.Header (X_Forum_User);

         if Result.From.Is_Empty then
            Result.From := Root.Header (X_Original_From);
         end if;

         if Result.From.Is_Empty then
            Result.From := Root.Header (+"From");
         end if;

         Result.Date := To_Date (Root.Header (+"Date"));
         Result.In_Reply_To := Root.Header (+"In-Reply-To");
         Result.Message_Id := Root.Header (+"Message-ID");
         Result.Subject := Root.Header (+"Subject");

         if Result.In_Reply_To.Is_Empty then
            declare
               List : constant League.String_Vectors.Universal_String_Vector :=
                 Root.Header (+"References").Split (' ', Skip_Empty);
            begin
               if List.Length > 0 then
                  declare
                     Value : constant
                       League.String_Vectors.Universal_String_Vector :=
                         List.Element (1).Split
                           (League.Characters.Latin.Character_Tabulation,
                            Skip_Empty);
                  begin
                     Result.In_Reply_To := Value.Element (1);
                  end;
               end if;
            end;
         end if;

         if CT ** Text_Plain or CT.Is_Empty then

            Read_Plain_Text (Root, Result);

         elsif Is_Multipart (CT) then
            declare
               Found : Boolean := False;
            begin
               Find_Plain_Text (Root, Result, Found);
               pragma Assert (Found);
            end;
         elsif CT = Text_HTML then
            if CTE = Eight_Bit then
               if Data'Length = 0 then
                  Result.Text := Root.Get_Body_As_Text;
                  --  Bogus encoding in yahoo
                  Result.Text := Replace_Placeholders (Result.Text);
                  Result.Text := Format_HTML (Result.Text);
               else
                  Result.Text := Root.Get_Body_As_Text (Charset);
                  Result.Text := Format_HTML (Result.Text);
               end if;
            else
               raise Constraint_Error
                 with "Unknown CTE " & CTE.To_UTF_8_String;
            end if;
         else
            raise Constraint_Error with
              "Unknown Content-Type: " & CT.To_UTF_8_String;
         end if;

         return Result;
      end Read_Mail;

      ---------------
      -- Read_Mail --
      ---------------

      function Read_Mail
        (Data : League.Stream_Element_Vectors.Stream_Element_Vector)
         return Mail
      is
         use type Ada.Streams.Stream_Element;
         use type Ada.Streams.Stream_Element_Offset;

         Result : Mail;
         Text   : League.Strings.Universal_String;
         LF     : constant Ada.Streams.Stream_Element := 10;
      begin
         for J in 2 .. Data.Length loop
            if Data.Element (J) = LF and Data.Element (J - 1) = LF then
               declare
                  Bin : constant Ada.Streams.Stream_Element_Array :=
                    Data.To_Stream_Element_Array;
                  Charset : constant League.Strings.Universal_String :=
                    Messages.Guess_Charset (Bin);
                  Codec : constant League.Text_Codecs.Text_Codec :=
                    League.Text_Codecs.Codec (Charset);
               begin
                  Text := Codec.Decode (Bin (0 .. J - 1));
                  Result := Read_Mail (Text, Bin (J .. Data.Length - 1));
                  exit;
               end;
            end if;
         end loop;

         return Result;
      end Read_Mail;

      -------------
      -- To_Date --
      -------------

      function To_Date
        (Text : League.Strings.Universal_String)
         return League.Calendars.Date_Time
      is
         use League.Calendars.ISO_8601;
         use type League.Calendars.Date_Time;

         Month_List : constant League.Strings.Universal_String
           := +"  JanFebMarAprMayJunJulAugSepNovOctDec";

         List : League.String_Vectors.Universal_String_Vector :=
           Text.Split (' ', Skip_Empty);

         Result         : League.Calendars.Date_Time;
         Time_Zone_Sign : Wide_Wide_Character;
         Time_Zone_Hour : Hour_Number;
         Time_Zone_Min  : Minute_Number;
         Time_Zone_Off  : League.Calendars.Time;
         Year           : Year_Number;
         Month          : Month_Number;
         Day            : Day_Number;
         Hour           : Hour_Number;
         Minute         : Minute_Number;
         Second         : Second_Number;
      begin
         --  Text example: Fri, 14 Jun 2002 12:52:22 +0300

         if List.Length = 5 then  -- Absent day of week
            List.Insert (1, League.Strings.Empty_Universal_String);
         end if;

         Year := Year_Number'Wide_Wide_Value
           (List.Element (4).To_Wide_Wide_String);

         Month := Month_Number (Month_List.Index (List.Element (3)) / 3);

         Day := Day_Number'Wide_Wide_Value
           (List.Element (2).To_Wide_Wide_String);

         Time_Zone_Hour := Hour_Number'Wide_Wide_Value
           (List.Element (6).Slice (2, 3).To_Wide_Wide_String);

         Time_Zone_Min := Minute_Number'Wide_Wide_Value
           (List.Element (6).Tail_From (4).To_Wide_Wide_String);

         Time_Zone_Sign := List.Element (6) (1).To_Wide_Wide_Character;

         List := List.Element (5).Split (':', Skip_Empty);

         Hour := Hour_Number'Wide_Wide_Value
           (List.Element (1).To_Wide_Wide_String);

         Minute := Minute_Number'Wide_Wide_Value
           (List.Element (2).To_Wide_Wide_String);

         Second := Second_Number'Wide_Wide_Value
           (List.Element (3).To_Wide_Wide_String);

         Time_Zone_Off :=
           Create (Year, Month, Day, Time_Zone_Hour, Time_Zone_Min, 0, 0)
           - Create (Year, Month, Day, 0, 0, 0, 0);

         Result := Create (Year, Month, Day, Hour, Minute, Second, 0);

         if Time_Zone_Sign = '+' then
            Result := Result - Time_Zone_Off;
         else
            Result := Result + Time_Zone_Off;
         end if;

         return Result;
      end To_Date;
   end Mails;

   package body Messages is

      function Decode_Header
        (Text : League.Strings.Universal_String)
         return League.Strings.Universal_String;

      ------------------------
      -- Decode_Base64_Body --
      ------------------------

      function Decode_Base64_Body
        (Self     : Message)
         return League.Stream_Element_Vectors.Stream_Element_Vector
      is
         Text : League.Strings.Universal_String := Self.Text;
         List : League.String_Vectors.Universal_String_Vector;
      begin
         if not Self.Data.Is_Empty then
            Text := League.Text_Codecs.Codec_For_Application_Locale.Decode
              (Self.Data);
         end if;

         List := Text.Split (League.Characters.Latin.Line_Feed, Skip_Empty);

         return League.Base_Codecs.From_Base_64 (List.Join (""));
      end Decode_Base64_Body;

      -------------------
      -- Decode_Header --
      -------------------

      function Decode_Header
        (Text : League.Strings.Universal_String)
         return League.Strings.Universal_String
      is
         function Decode_B
           (Encoding : League.Strings.Universal_String;
            Piece    : League.Strings.Universal_String)
            return League.Strings.Universal_String;

         --------------
         -- Decode_B --
         --------------

         function Decode_B
           (Encoding : League.Strings.Universal_String;
            Piece    : League.Strings.Universal_String)
            return League.Strings.Universal_String
         is
            Codec : constant League.Text_Codecs.Text_Codec :=
              League.Text_Codecs.Codec (Encoding);
            Bytes : constant League.Stream_Element_Vectors
              .Stream_Element_Vector
                := League.Base_Codecs.From_Base_64 (Piece);
         begin
            return Codec.Decode (Bytes);
         end Decode_B;

         Start    : constant League.Strings.Universal_String := +"=?";
         Stop     : constant League.Strings.Universal_String := +"?=";
         Result   : League.Strings.Universal_String;
         Next     : Positive := 1;
         Pos      : Natural;
         Encoding : League.Strings.Universal_String;
      begin
         loop
            Pos := Text.Index (Next, Start);
            exit when Pos = 0;

            Result.Append (Text.Slice (Next, Pos - 1));
            Next := Text.Index (Pos + 2, "?");
            Encoding := Text.Slice (Pos + 2, Next - 1);
            Pos := Text.Index (Next + 3, Stop);

            if Text (Next + 1).To_Wide_Wide_Character in 'b' | 'B' then
               Result.Append
                 (Decode_B (Encoding, Text.Slice (Next + 3, Pos - 1)));
            elsif Text (Next + 1).To_Wide_Wide_Character in 'q' | 'Q' then
               Result.Append
                 (Decode_Quoted_Printable
                    (Encoding,
                     Text.Slice (Next + 3, Pos - 1),
                     Header => True));
            else
               raise Constraint_Error with "Unknown header encoding";
            end if;

            Next := Pos + 2;
         end loop;

         Result.Append (Text.Slice (Next, Text.Length));

         return Result;
      end Decode_Header;

      ----------------------
      -- Get_Body_As_Text --
      ----------------------

      function Get_Body_As_Text
        (Self : Message) return League.Strings.Universal_String is
      begin
         if Self.Data.Is_Empty then
            return Self.Text;
         else
            return League.Text_Codecs.Codec_For_Application_Locale.Decode
              (Self.Data);
         end if;
      end Get_Body_As_Text;

      ----------------------
      -- Get_Body_As_Text --
      ----------------------

      function Get_Body_As_Text
        (Self    : Message;
         Charset : League.Strings.Universal_String)
           return League.Strings.Universal_String
      is
         UTF_8 : constant League.Strings.Universal_String := +"utf-8";
         Codec : constant League.Text_Codecs.Text_Codec :=
           League.Text_Codecs.Codec (Charset);
      begin
         if Self.Data.Is_Empty then
            if Charset ** UTF_8 then
               return Self.Text;
            else
               raise Constraint_Error with "Expected binary file";
            end if;
         end if;

         return Codec.Decode (Self.Data);
      end Get_Body_As_Text;

      -------------------
      -- Guess_Charset --
      -------------------

      function Guess_Charset
        (Data : Ada.Streams.Stream_Element_Array)
      return League.Strings.Universal_String
      is
         use type Ada.Streams.Stream_Element;
         use type Ada.Streams.Stream_Element_Offset;
         Codec  : constant League.Text_Codecs.Text_Codec :=
           League.Text_Codecs.Codec_For_Application_Locale;
         Result : League.Strings.Universal_String;
         Prev   : Ada.Streams.Stream_Element_Offset := Data'First;
         Text   : League.Strings.Universal_String;
      begin
         for J in Data'Range loop
            if Data (J) = 10 then
               if Data (Prev) = Character'Pos ('C') and
                 Data (Prev + 1) = Character'Pos ('o') and
                 Data (Prev + 2) = Character'Pos ('n')
               then
                  Text := Codec.Decode (Data (Prev .. J));

                  if Text.Starts_With (Content_Type) then
                     declare
                        Msg : constant Messages.Message :=
                          Messages.Read_Message (Text);
                     begin
                        Result := Msg.Header (Content_Type, +"charset");

                        exit;
                     end;
                  end if;
               end if;
               Prev := J + 1;
            end if;
         end loop;

         if Result.Is_Empty then
            Result.Append ("utf-8");
         end if;

         return Result;
      end Guess_Charset;

      ------------
      -- Header --
      ------------

      function Header
        (Self : Message;
         Name : League.Strings.Universal_String)
         return League.Strings.Universal_String
      is
         Found : constant Maps.Cursor := Self.Headers.Find (Name.To_Lowercase);
      begin
         if Maps.Has_Element (Found) then
            return Maps.Element (Found);
         else
            return League.Strings.Empty_Universal_String;
         end if;
      end Header;

      ------------
      -- Header --
      ------------

      function Header
        (Self  : Message;
         Name  : League.Strings.Universal_String;
         Field : League.Strings.Universal_String)
         return League.Strings.Universal_String
      is
         function Dequote (Text : League.Strings.Universal_String)
           return League.Strings.Universal_String;

         -------------
         -- Dequote --
         -------------

         function Dequote (Text : League.Strings.Universal_String)
           return League.Strings.Universal_String is
         begin
            if Text.Starts_With ("""") and Text.Ends_With ("""") then
               return Text.Slice (2, Text.Length - 1);
            else
               return Text;
            end if;
         end Dequote;

         Key   : League.Strings.Universal_String := Field;
         Value : constant League.Strings.Universal_String
           := Self.Header (Name);
         List  : constant League.String_Vectors.Universal_String_Vector :=
           Value.Split (';', Skip_Empty);
      begin
         if Value.Is_Empty then
            return Value;
         end if;

         Key.Append ("=");

         for J in 1 .. List.Length loop
            declare
               Line : League.Strings.Universal_String := List.Element (J);
            begin
               while Line.Starts_With (" ") loop
                  Line := Line.Tail_From (2);
               end loop;

               if Field.Is_Empty and Line.Index ("=") = 0 then
                  return Line;
               elsif not Field.Is_Empty and Line.Starts_With (Key) then
                  return Trim (Dequote (Line.Tail_From (Key.Length + 1)));
               end if;
            end;
         end loop;

         return League.Strings.Empty_Universal_String;
      end Header;

      ------------------
      -- Nested_Parts --
      ------------------

      function Nested_Parts
        (Self     : Message;
         Boundary : League.Strings.Universal_String)
           return Message_Array
      is
         use type League.Strings.Universal_String;

         function Count_Nested_Parts
           (Text : League.Strings.Universal_String) return Natural;

         procedure Read
           (Text : League.Strings.Universal_String;
            Msg  : out Message;
            Next : in out Positive);

         Stopper : constant League.Strings.Universal_String :=
           League.Characters.Latin.Line_Feed.To_Wide_Wide_Character
             & "--" & Boundary;
         Starter : constant League.Strings.Universal_String :=
           "--" & Boundary & League.Characters.Latin.Line_Feed;

         ------------------------
         -- Count_Nested_Parts --
         ------------------------

         function Count_Nested_Parts
           (Text : League.Strings.Universal_String) return Natural
         is
            Result : Natural := 0;
            Next   : Natural := 0;
         begin
            loop
               Next := Text.Index (Next + 1, Starter);

               if Next = 0 then
                  exit;
               else
                  Result := Result + 1;
               end if;
            end loop;

            return Result;
         end Count_Nested_Parts;

         ----------
         -- Read --
         ----------

         procedure Read
           (Text : League.Strings.Universal_String;
            Msg  : out Message;
            Next : in out Positive)
         is
            From : constant Positive :=
              Text.Index (Next, Starter) + Starter.Length;
            To   : Natural := Text.Index (From, Stopper);
         begin
            if To = 0 then
               To := Text.Length;
            end if;

            Msg := Read_Message (Text.Slice (From, To));
            Next := To;
         end Read;

         Text   : League.Strings.Universal_String := Self.Text;
         Next   : Positive := 1;
      begin
         if not Self.Data.Is_Empty then
            Text := League.Text_Codecs.Codec_For_Application_Locale.Decode
              (Self.Data);
         end if;

         declare
            Result : Message_Array (1 .. Count_Nested_Parts (Text));
         begin
            for J in Result'Range loop
               Read (Text, Result (J), Next);
            end loop;

            return Result;
         end;
      end Nested_Parts;

      ------------------
      -- Read_Message --
      ------------------

      function Read_Message
        (Text : League.Strings.Universal_String;
         Data : Ada.Streams.Stream_Element_Array := (1 .. 0 => 0))
           return Message
      is
         Tab    : constant Wide_Wide_String :=
           (1 => League.Characters.Latin.Character_Tabulation
            .To_Wide_Wide_Character);

         Result : Message;
         Name   : League.Strings.Universal_String;
         Value  : League.Strings.Universal_String;
         List   : constant League.String_Vectors.Universal_String_Vector :=
           Text.Split (League.Characters.Latin.Line_Feed);
      begin
         for J in 1 .. List.Length loop
            declare
               Line : constant League.Strings.Universal_String
                 := List.Element (J);
            begin
               if Line.Starts_With (" ")  --  Continuation of header
                 or else Line.Starts_With (Tab)
               then
                  Value.Append (Line.Slice (2, Line.Length));

                  goto Continue;
               elsif J > 1 and then not Result.Headers.Contains (Name) then
                  Value := Decode_Header (Value);
                  Result.Headers.Insert (Name, Value);
               end if;

               if Line.Is_Empty then
                  Result.Text := List.Slice (J + 1, List.Length).Join
                    (League.Characters.Latin.Line_Feed);

                  exit;
               end if;

               Name := Line.Head (Line.Index (':') - 1).To_Lowercase;
               Value := Line.Tail_From (Name.Length + 2);

               if Value.Starts_With (" ") then
                  Value := Value.Tail_From (2);
               end if;

               <<Continue>>
            end;
         end loop;

         Result.Data.Append (Data);

         return Result;
      end Read_Message;

   end Messages;

   package Storage is
      procedure Insert_Post (Value : Mails.Mail);

   end Storage;

   package body Storage is
      type Paragraph is record
         Text  : League.Strings.Universal_String;
         Quote : Natural := 0;
      end record;

      type Optional_Paragraph (Is_Set : Boolean := False) is record
         case Is_Set is
            when True =>
               Value : Paragraph;
            when False =>
               null;
         end case;
      end record;

      package Paragraph_Lists is new Ada.Containers.Doubly_Linked_Lists
        (Paragraph);

      procedure Parse_Message
        (Value  : Mails.Mail;
         Result : out Paragraph_Lists.List);
      --  Split message text to list of paragraphs

      procedure Parse_Flowed_Message
        (Value  : Mails.Mail;
         Result : out Paragraph_Lists.List);

      procedure Parse_Plain_Message
        (Value  : Mails.Mail;
         Result : out Paragraph_Lists.List);

      SQL_Text : constant League.Strings.Universal_String :=
        +("insert into posts (author, sent, parent, id, subject) " &
            "values (:f,(:d),:r,:i,:s)");
      SQL_Para_Text : constant League.Strings.Universal_String :=
        +("insert into post_lines (post, pos, quote, text) " &
            "values (:i,:p,:q,:t)");
      Format :  constant League.Strings.Universal_String :=
        +"yyyy-MM-dd HH:mm:ss";

      -----------------
      -- Insert_Post --
      -----------------

      procedure Insert_Post (Value : Mails.Mail) is
         Option : SQL.Options.SQL_Options;
         List   : Paragraph_Lists.List;
      begin
         Parse_Message (Value, List);
         Option.Set (+"dbname", +"mail");
         declare
            DB : SQL.Databases.SQL_Database :=
              SQL.Databases.Create (+"POSTGRESQL", Option);
         begin
            DB.Open;

            declare
               S : SQL.Queries.SQL_Query := DB.Query (SQL_Text);
            begin
               S.Bind_Value (+":f", League.Holders.To_Holder (Value.From));

               S.Bind_Value
                 (+":d",
                  League.Holders.To_Holder
                    (League.Calendars.ISO_8601.Image (Format, Value.Date)));

               S.Bind_Value
                 (+":r", League.Holders.To_Holder (Value.In_Reply_To));

               S.Bind_Value
                 (+":i", League.Holders.To_Holder (Value.Message_Id));

               S.Bind_Value (+":s", League.Holders.To_Holder (Value.Subject));
               S.Execute;

               if not S.Error_Message.Is_Empty then
                  Ada.Wide_Wide_Text_IO.Put_Line
                    (S.Error_Message.To_Wide_Wide_String);
                  Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
               end if;
            end;

            declare
               S : SQL.Queries.SQL_Query := DB.Query (SQL_Para_Text);
               Index : Positive := 1;
            begin
               for Para of List loop
                  S.Bind_Value
                    (+":i", League.Holders.To_Holder (Value.Message_Id));

                  S.Bind_Value
                    (+":p", League.Holders.Integers.To_Holder (Index));

                  S.Bind_Value
                    (+":q", League.Holders.Integers.To_Holder (Para.Quote));

                  S.Bind_Value (+":t", League.Holders.To_Holder (Para.Text));
                  Index := Index + 1;
                  S.Execute;

                  if not S.Error_Message.Is_Empty then
                     Ada.Wide_Wide_Text_IO.Put_Line
                       (S.Error_Message.To_Wide_Wide_String);
                     Ada.Command_Line.Set_Exit_Status
                       (Ada.Command_Line.Failure);
                  end if;
               end loop;
            end;
         end;
      end Insert_Post;

      --------------------------
      -- Parse_Flowed_Message --
      --------------------------

      procedure Parse_Flowed_Message
        (Value  : Mails.Mail;
         Result : out Paragraph_Lists.List)
      is
         Prev    : Optional_Paragraph;
         Lines   : constant League.String_Vectors.Universal_String_Vector :=
           Value.Text.Split
             (League.Characters.Latin.Line_Feed,
              League.Strings.Keep_Empty);
      begin
         for J in 1 .. Lines.Length loop
            declare
               Line  : League.Strings.Universal_String := Lines.Element (J);
               Quote : Natural := 0;
            begin
               for K in 1 .. Line.Length loop
                  if Line.Element (K).To_Wide_Wide_Character = '>' then
                     Quote := Quote + 1;
                  else
                     exit;
                  end if;
               end loop;

               Line := Line.Tail_From (Quote + 1);

               if Line.Starts_With (" ") then
                  --  line has been space-stuffed
                  Line := Line.Tail_From (2);
               end if;

               if Prev.Is_Set then
                  pragma Assert (Prev.Value.Quote = Quote);
                  Prev.Value.Text.Append (Line);

                  if not Line.Ends_With (" ") then
                     Result.Append (Prev.Value);
                     Prev := (Is_Set => False);
                  end if;
               elsif Line.Ends_With (" ") then
                  Prev := (Is_Set => True,
                           Value  => (Quote => Quote, Text => Line));
               else
                  Result.Append ((Quote => Quote, Text => Line));
               end if;
            end;
         end loop;
      end Parse_Flowed_Message;

      -------------------
      -- Parse_Message --
      -------------------

      procedure Parse_Message
        (Value  : Mails.Mail;
         Result : out Paragraph_Lists.List)
      is
      begin
         if Value.Is_Flowed then
            Parse_Flowed_Message (Value, Result);
         else
            Parse_Plain_Message (Value, Result);
         end if;

         --  Drop any empty or quoted paragraphs at the end of message
         while not Result.Is_Empty loop
            declare
               Last : constant Paragraph := Result.Last_Element;
            begin
               exit when Last.Quote = 0 and not Last.Text.Is_Empty;
               Result.Delete_Last;
            end;
         end loop;
      end Parse_Message;

      -------------------------
      -- Parse_Plain_Message --
      -------------------------

      procedure Parse_Plain_Message
        (Value  : Mails.Mail;
         Result : out Paragraph_Lists.List)
      is
         subtype Full_Line is Positive range 65 .. 79;
         --  If line length in this range suppose that it has soft line break

         Prev    : Optional_Paragraph;
         Lines   : constant League.String_Vectors.Universal_String_Vector :=
           Value.Text.Split
             (League.Characters.Latin.Line_Feed,
              League.Strings.Keep_Empty);
      begin
         for J in 1 .. Lines.Length loop
            declare
               Line  : League.Strings.Universal_String := Lines.Element (J);
               Quote : Natural := 0;
               Force : Boolean;
            begin
               if Line.Starts_With (">") then
                  for K in 1 .. Line.Length loop
                     if Line.Element (K).To_Wide_Wide_Character = '>' then
                        Quote := Quote + 1;
                     elsif Line.Element (K).To_Wide_Wide_Character /= ' ' then
                        Line := Line.Tail_From (K - 1);
                        exit;
                     end if;

                     if K = Line.Length then
                        Line.Clear;
                     end if;
                  end loop;
               end if;

               Force := Line.Starts_With ("- ")
                 or else (Line.Length > 1 and then
                          Line (2).To_Wide_Wide_Character = ')');

               if Prev.Is_Set and then
                 (Prev.Value.Quote /= Quote or Force)
               then
                  Result.Append (Prev.Value);
                  Prev := (Is_Set => False);
               end if;

               if Prev.Is_Set then
                  Prev.Value.Text.Append (" ");
                  Prev.Value.Text.Append (Line);

                  if Line.Length not in Full_Line then
                     Result.Append (Prev.Value);
                     Prev := (Is_Set => False);
                  end if;
               elsif Line.Length in Full_Line then
                  Prev := (Is_Set => True,
                           Value  => (Quote => Quote, Text => Line));
               else
                  Result.Append ((Quote => Quote, Text => Line));
               end if;
            end;
         end loop;

         if Prev.Is_Set then
            Result.Append (Prev.Value);
         end if;
      end Parse_Plain_Message;

   end Storage;

   Value : League.Strings.Universal_String :=
     League.Application.Arguments.Element (1);

   type String_Convertor is access function
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   JSON_Convertors : constant array (Positive range <>) of String_Convertor
     := (Read_File'Access,
         Unpack_JSON'Access,
         Strinp_Carriage_Return'Access,
         Unescape_XML_Entities'Access);
begin
   if Value.Ends_With (".json") then
      for Conv of JSON_Convertors loop
         Value := Conv (Value);
      end loop;

--        Ada.Wide_Wide_Text_IO.Put_Line (Value.To_Wide_Wide_String);

      if Value.Is_Empty then
         Ada.Wide_Wide_Text_IO.Put_Line
           ("rm " & League.Application.Arguments.Element (1)
                      .To_Wide_Wide_String);
         return;
      end if;

      declare
         Msg : constant Mails.Mail := Mails.Read_Mail (Value);
      begin
         Storage.Insert_Post (Msg);
      end;
   else
      declare
         Binary : constant League.Stream_Element_Vectors.Stream_Element_Vector
           := Read_File (Value);
         Msg : constant Mails.Mail := Mails.Read_Mail (Binary);
      begin
         Storage.Insert_Post (Msg);
      end;
   end if;

end Import_Mail;
