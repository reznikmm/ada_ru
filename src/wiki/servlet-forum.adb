with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;

with League.Base_Codecs;
with League.Calendars.ISO_8601;
with League.Characters.Latin;
with League.Holders;
with League.Holders.Integers;
with League.Regexps;
with League.Settings;
with League.Stream_Element_Vectors;
with League.String_Vectors;
with League.Strings.Hash;
with League.Text_Codecs;

with SQL.Options;
with SQL.Queries;
with SQL.Databases;

with Matreshka.Internals.SQL_Drivers.PostgreSQL.Factory;

with AWS.SMTP.Client;

with Forum.Contexts;
with Forum.Posts;
with Forum.Topics;
with Forum.Users;
with Forum.Writers;

with Sessions;
with Mails;

package body Servlet.Forum is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

   package Mail_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Mails.Mail, "=" => Mails."=");

   protected Storage is
      --  The storage keeps queue of main to be imported
      procedure Put (Value : Mails.Mail);
      entry Get (Value : out Mails.Mail; Last : out Boolean);
   private
      List : Mail_Lists.List;
   end Storage;

   task DB_Writter is
      entry Start (Error : out League.Strings.Universal_String);
      --  Take mails from Storage and put it into DB
   end DB_Writter;

   function Strip_Carriage_Return
    (Text : League.Strings.Universal_String)
     return League.Strings.Universal_String;

   function Escape
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   function Escape_Header
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   procedure Wrap_Lines (Text : in out League.Strings.Universal_String);

   function Next_Message_Id
     (Self : in out Forum_Servlet) return League.Strings.Universal_String;

   function Check_Token
     (Self : Forum_Servlet'Class;
      Auth : League.String_Vectors.Universal_String_Vector)
      return Boolean;
   --  Verify a token value in the "Authorization:" header (provided as Auth)

   function Read_Stream
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return League.Stream_Element_Vectors.Stream_Element_Vector;

   procedure Send_Mail (Mail : Mails.Mail);

   -------------
   -- Storage --
   -------------

   protected body Storage is

      ---------
      -- Put --
      ---------

      procedure Put (Value : Mails.Mail) is
      begin
         List.Append (Value);
      end Put;

      ---------
      -- Get --
      ---------

      entry Get
        (Value : out Mails.Mail;
         Last  : out Boolean) when not List.Is_Empty is
      begin
         Value := List.First_Element;
         List.Delete_First;
         Last := List.Is_Empty;
      end Get;
   end Storage;

   ----------------
   -- DB_Writter --
   ----------------

   task body DB_Writter is
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

      procedure Insert_Post (Value : Mails.Mail);

      procedure Parse_Message
        (Value  : Mails.Mail;
         Result : out Paragraph_Lists.List);
      --  Split message text to list of paragraphs

      procedure Parse_Plain_Message
        (Value  : Mails.Mail;
         Result : out Paragraph_Lists.List);

      procedure Parse_Flowed_Message
        (Value  : Mails.Mail;
         Result : out Paragraph_Lists.List);

      procedure Update_Files (DB : in out SQL.Databases.SQL_Database);

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
         use type Ada.Containers.Count_Type;

         procedure Find_Quoted_Message (Index : out Ada.Containers.Count_Type);
         --  Look for quoted message in Result and return Index of the
         --  first paragraph of quoted message.

         -------------------------
         -- Find_Quoted_Message --
         -------------------------

         procedure Find_Quoted_Message
           (Index : out Ada.Containers.Count_Type)
         is
            Pattern_1 : constant League.Regexps.Regexp_Pattern :=
              League.Regexps.Compile (+"From\:.*ada_ru\@ada\-ru.org\>?");
            Pattern_2 : constant League.Regexps.Regexp_Pattern :=
              League.Regexps.Compile
                (+"^\-{5}\ ?(Исходное\ сообщение|Original\ Message)");
            Cursor  : Paragraph_Lists.Cursor := Result.First;
            Count   : Ada.Containers.Count_Type := 0;
         begin
            while Paragraph_Lists.Has_Element (Cursor) loop
               declare
                  Item : constant Paragraph :=
                    Paragraph_Lists.Element (Cursor);
               begin
                  Count := Count + 1;

                  if Pattern_1.Find_Match (Item.Text).Is_Matched
                    or else Pattern_2.Find_Match (Item.Text).Is_Matched
                  then
                     Index := Count;
                     return;
                  end if;

                  Cursor := Paragraph_Lists.Next (Cursor);
               end;
            end loop;

            Index := 0;
         end Find_Quoted_Message;

         Quoted : Ada.Containers.Count_Type;
      begin
         if Value.Is_Flowed then
            Parse_Flowed_Message (Value, Result);
         else
            Parse_Plain_Message (Value, Result);
         end if;

         Find_Quoted_Message (Quoted);

         --  Quoted paragraphs counting from the end of list:
         if Quoted > 0 then
            Quoted := Result.Length - Quoted + 1;
         end if;

         --  Drop any empty or quoted paragraphs at the end of message
         while not Result.Is_Empty loop
            declare
               Last : constant Paragraph := Result.Last_Element;
            begin
               if Quoted > 0 then
                  Quoted := Quoted - 1;
               else
                  exit when Last.Quote = 0 and not Last.Text.Is_Empty;
               end if;

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

      ------------------
      -- Update_Files --
      ------------------

      procedure Update_Files (DB : in out SQL.Databases.SQL_Database) is
         package Forum renames Standard.Forum;

         type File_Check is record
            Size : Ada.Streams.Stream_Element_Count;
            Hash : League.Hash_Type;
         end record;

         package File_Check_Maps is new Ada.Containers.Hashed_Maps
           (Key_Type        => League.Strings.Universal_String,
            Element_Type    => File_Check,
            Hash            => League.Strings.Hash,
            Equivalent_Keys => League.Strings."=");

         procedure Read_Map
           (Root : String;
            Map : out File_Check_Maps.Map);

         procedure Write_File
           (Root : String;
            Map  : in out File_Check_Maps.Map;
            Info : Forum.Writers.File_Information);

         procedure Each_Forum
           (Root  : String;
            Map   : in out File_Check_Maps.Map;
            Forun : League.Holders.Holder);

         procedure Each_Page
           (Root  : String;
            Map   : in out File_Check_Maps.Map;
            Forum : League.Holders.Holder;
            Page  : League.Holders.Holder);

         procedure Each_Topic
           (Root  : String;
            Map   : in out File_Check_Maps.Map;
            Forun : League.Holders.Holder;
            Topic : League.Holders.Holder);

         Output : Ada.Wide_Wide_Text_IO.File_Type;

         ----------------
         -- Each_Forum --
         ----------------

         procedure Each_Forum
           (Root  : String;
            Map   : in out File_Check_Maps.Map;
            Forun : League.Holders.Holder)
         is
            Pages : League.Holders.Holder;
            Ok    : Boolean;
            Info  : Forum.Writers.File_Information;
         begin
            League.Holders.Component (Forun, +"pages", Pages, Ok);

            declare
               Cursor : League.Holders.Iterable_Holder_Cursors.Cursor'Class :=
                 League.Holders.First (Pages);
            begin
               while Cursor.Next loop
                  Forum.Writers.Write_Forum_Page (Forun, Cursor.Element, Info);
                  Write_File (Root, Map, Info);
                  Each_Page (Root, Map, Forun, Cursor.Element);
               end loop;
            end;
         end Each_Forum;

         ---------------
         -- Each_Page --
         ---------------

         procedure Each_Page
           (Root  : String;
            Map   : in out File_Check_Maps.Map;
            Forum : League.Holders.Holder;
            Page  : League.Holders.Holder)
         is
            Topics : League.Holders.Holder;
            Ok     : Boolean;
         begin
            League.Holders.Component (Page, +"topics", Topics, Ok);

            declare
               Cursor : League.Holders.Iterable_Holder_Cursors.Cursor'Class :=
                 League.Holders.First (Topics);
            begin
               while Cursor.Next loop
                  Each_Topic (Root, Map, Forum, Cursor.Element);
               end loop;
            end;
         end Each_Page;

         ----------------
         -- Each_Topic --
         ----------------

         procedure Each_Topic
           (Root  : String;
            Map   : in out File_Check_Maps.Map;
            Forun : League.Holders.Holder;
            Topic : League.Holders.Holder)
         is
            Pages : League.Holders.Holder;
            Ok    : Boolean;
            Info  : Forum.Writers.File_Information;
         begin
            League.Holders.Component (Topic, +"pages", Pages, Ok);

            declare
               Cursor : League.Holders.Iterable_Holder_Cursors.Cursor'Class :=
                 League.Holders.First (Pages);
            begin
               while Cursor.Next loop
                  Forum.Writers.Write_Topic_Page
                    (Forun, Topic, Cursor.Element, Info);
                  Write_File (Root, Map, Info);
               end loop;
            end;
         end Each_Topic;

         --------------
         -- Read_Map --
         --------------

         procedure Read_Map
           (Root : String;
            Map  : out File_Check_Maps.Map)
         is
            package Hash_IO is new Ada.Wide_Wide_Text_IO.Modular_IO
              (League.Hash_Type);
            package Size_IO is new Ada.Wide_Wide_Text_IO.Integer_IO
              (Ada.Streams.Stream_Element_Count);

            Input : Ada.Wide_Wide_Text_IO.File_Type;
            Item  : File_Check;
            Name  : League.Strings.Universal_String;
            Space : Wide_Wide_Character;
         begin
            Ada.Wide_Wide_Text_IO.Open
              (Input, Ada.Wide_Wide_Text_IO.In_File, Root & "hashes.txt");

            while not Ada.Wide_Wide_Text_IO.End_Of_File (Input) loop
               Size_IO.Get (Input, Item.Size);
               Hash_IO.Get (Input, Item.Hash);
               Ada.Wide_Wide_Text_IO.Get (Input, Space);
               pragma Assert (Space = ' ');
               Name := +Ada.Wide_Wide_Text_IO.Get_Line (Input);
               Map.Insert (Name, Item);
            end loop;

            Ada.Wide_Wide_Text_IO.Close (Input);
         end Read_Map;

         ----------------
         -- Write_File --
         ----------------

         procedure Write_File
           (Root : String;
            Map  : in out File_Check_Maps.Map;
            Info : Forum.Writers.File_Information)
         is
            File   : Ada.Streams.Stream_IO.File_Type;
            Cursor : File_Check_Maps.Cursor := Map.Find (Info.Name);
         begin
            Ada.Wide_Wide_Text_IO.Put_Line
              (Output, Forum.Writers.Image (Info));

            if File_Check_Maps.Has_Element (Cursor) then
               declare
                  use type League.Hash_Type;
                  use type Ada.Streams.Stream_Element_Count;
                  Have : constant File_Check := Map (Cursor);
               begin
                  Map.Delete (Cursor);

                  if Have.Size = Info.Data.Length
                    and Have.Hash = Info.Hash
                  then
                     return;
                  end if;
               end;
            end if;

            Ada.Streams.Stream_IO.Create
              (File, Name => Root & Info.Name.To_UTF_8_String);

            Ada.Streams.Stream_IO.Write
              (File, Info.Data.To_Stream_Element_Array);

            Ada.Streams.Stream_IO.Close (File);
         end Write_File;

         Root    : constant String := "install/forum/";
         Map     : File_Check_Maps.Map;
         Context : aliased Forum.Contexts.Context;
         Top     : League.Holders.Holder;
         Info    : Forum.Writers.File_Information;
      begin
         Read_Map (Root, Map);
         Ada.Wide_Wide_Text_IO.Create (Output, Name => Root & "hashes.txt");

         Context.Users.Initiaize (DB);
         Context.Posts.Initiaize (Context.Users, DB);
         Context.Forums.Initiaize (DB);
         Context.Topics.Initiaize (DB, Context.Forums, Context.Posts);
         Context.Posts.Assign_Topics (Context.Forums, Context.Topics);
         Context.Forums.Sort_Topics;
         Context.Topics.Sort_Posts;

         Top := Context.Forums.Last_Topics_Holder;
         Forum.Writers.Write_Forum_Atom (Top, Info);
         Write_File (Root, Map, Info);

         Top := Context.Forums.To_Holder;
         Forum.Writers.Write_Forum_Index (Top, Info);
         Write_File (Root, Map, Info);

         declare
            Cursor : League.Holders.Iterable_Holder_Cursors.Cursor'Class :=
              League.Holders.First (Top);
         begin
            while Cursor.Next loop
               Each_Forum (Root, Map, Cursor.Element);
            end loop;
         end;

         Ada.Wide_Wide_Text_IO.Close (Output);

         --  Delete extra files
         for J in Map.Iterate loop
            Ada.Directories.Delete_File
              (Root & File_Check_Maps.Key (J).To_UTF_8_String);
         end loop;
      end Update_Files;

      Option : SQL.Options.SQL_Options;
   begin
      Option.Set (+"dbname", +"mail");

      declare
         Mail : Mails.Mail;
         Last : Boolean;
         DB : SQL.Databases.SQL_Database :=
           SQL.Databases.Create (+"POSTGRESQL", Option);
      begin
         select
            accept Start (Error : out League.Strings.Universal_String) do
               DB.Open;
               Error := DB.Error_Message;
            end Start;
         or
            terminate;
         end select;

         loop
            Storage.Get (Mail, Last);
            Insert_Post (Mail);
            Send_Mail (Mail);

            if Last then
               Update_Files (DB);
            end if;
         end loop;
      end;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("DB_Writter dead:");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end DB_Writter;

   -----------------
   -- Check_Token --
   -----------------

   function Check_Token
     (Self : Forum_Servlet'Class;
      Auth : League.String_Vectors.Universal_String_Vector)
      return Boolean
   is
      use type League.Strings.Universal_String;
   begin
      return Auth.Length = 1
        and then Auth.Element (1) = "Token " & Self.Secret;
   end Check_Token;

   -------------
   -- Do_Post --
   -------------

   overriding procedure Do_Post
     (Self     : in out Forum_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      use type League.Strings.Universal_String;

      Session : constant Sessions.HTTP_Session_Access :=
        Sessions.HTTP_Session_Access (Request.Get_Session);

      Info   : constant Sessions.User_Info := Session.Get_User_Info;

      Topic : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"topic");

      Subject : constant League.Strings.Universal_String :=
        Request.Get_Parameter (+"subject");

      Posted : constant League.Strings.Universal_String :=
        Strip_Carriage_Return (Request.Get_Parameter (+"text"));

      Mail : Mails.Mail;
   begin
      if not Info.User.Is_Empty then
         Mail :=
           (From        => Info.Name & " <" & Info.Mails (1) & ">",
            Date        => League.Calendars.Clock,
            In_Reply_To => Topic,
            Message_Id  => Self.Next_Message_Id,
            Subject     => Subject,
            Text        => Posted,
            Is_Flowed   => False);

         Storage.Put (Mail);

         Response.Set_Status (Servlet.HTTP_Responses.See_Other);
         Response.Set_Header (+"Location", +"/forum/ok.html");
         Response.Set_Header (+"Cache-Control", +"must-revalidate");

      elsif Self.Check_Token (Request.Get_Headers (+"Authorization")) then
         declare
            Data : constant
              League.Stream_Element_Vectors.Stream_Element_Vector :=
                Read_Stream (Request.Get_Input_Stream);
         begin
            Mail := Mails.Read_Mail (Data);
            Mail.Message_Id := Self.Next_Message_Id;
            Storage.Put (Mail);
            Response.Set_Status (Servlet.HTTP_Responses.No_Content);
         end;

      else
         Response.Set_Status (Servlet.HTTP_Responses.Unauthorized);
      end if;
   end Do_Post;

   ------------
   -- Escape --
   ------------

   function Escape
     (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      UTF_8 : constant League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec (+"utf-8");
      Raw   : constant League.Stream_Element_Vectors.Stream_Element_Vector :=
        UTF_8.Encode (Text);
      Base_64 : constant League.Strings.Universal_String :=
        League.Base_Codecs.To_Base_64 (Raw);
   begin
      return Base_64;
   end Escape;

   -------------------
   -- Escape_Header --
   -------------------

   function Escape_Header (Text : League.Strings.Universal_String)
     return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;
   begin
      return "=?utf-8?b?" & Escape (Text) & "?=";
   end Escape_Header;

   ----------------------
   -- Get_Servlet_Info --
   ----------------------

   overriding function Get_Servlet_Info (Self : Forum_Servlet)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return +"Servlet to post on forum";
   end Get_Servlet_Info;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
      return Forum_Servlet
   is
      pragma Unreferenced (Parameters);

      Error     : League.Strings.Universal_String;
      Settings  : League.Settings.Settings;

      function Get (Key : Wide_Wide_String)
        return League.Strings.Universal_String is
         (League.Holders.Element (Settings.Value (+("/forum/" & Key))));
   begin
      return Result : Forum_Servlet do
         Result.Secret := Get ("secret");
         Result.Last_Id := 0;

         pragma Assert (not Result.Secret.Is_Empty);

         DB_Writter.Start (Error);
         pragma Assert (Error.Is_Empty);
      end return;
   end Instantiate;

   ---------------------
   -- Next_Message_Id --
   ---------------------

   function Next_Message_Id
     (Self : in out Forum_Servlet) return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;

      Image : Wide_Wide_String := Self.Last_Id'Wide_Wide_Image;
   begin
      Image (1) := '.';
      Self.Last_Id := Self.Last_Id + 1;

      return "<" &
        League.Calendars.ISO_8601.Image
        (+"yyyyMMddHHmmss", League.Calendars.Clock)
        & Image
        & "@ada-ru.org>";
   end Next_Message_Id;

   -----------------
   -- Read_Stream --
   -----------------

   function Read_Stream
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return League.Stream_Element_Vectors.Stream_Element_Vector
   is
      use type Ada.Streams.Stream_Element_Count;

      Data : Ada.Streams.Stream_Element_Array (1 .. 1024);
      Last : Ada.Streams.Stream_Element_Count;
   begin
      return Result : League.Stream_Element_Vectors.Stream_Element_Vector do
         loop
            Stream.Read (Data, Last);

            exit when Last = 0;

            Result.Append (Data (1 .. Last));
         end loop;
      end return;
   end Read_Stream;

   ---------------
   -- Send_Mail --
   ---------------

   procedure Send_Mail (Mail : Mails.Mail) is
      use type League.Strings.Universal_String;

      function Get_Name (Addr : League.Strings.Universal_String)
        return League.Strings.Universal_String;
      --  Return name part of the EMail addredd ("Name <email@host.net>").

      --------------
      -- Get_Name --
      --------------

      function Get_Name (Addr : League.Strings.Universal_String)
        return League.Strings.Universal_String
      is
         function Trim (Text : League.Strings.Universal_String)
           return League.Strings.Universal_String
             is (Text.Split (' ', League.Strings.Skip_Empty).Join (" "));

         Pos    : constant Natural := Addr.Index ("<");
         At_Pos : constant Natural := Addr.Index ("@");
         Result : League.Strings.Universal_String;
      begin
         if Pos > 0 then
            Result := Trim (Addr.Head_To (Pos - 1));
         end if;

         if Result.Is_Empty then
            Result := Addr.Slice (Pos + 1, At_Pos - 1);
         end if;

         return Result;
      end Get_Name;

      Sender : constant League.Strings.Universal_String :=
        Get_Name (Mail.From);

      From : constant AWS.SMTP.E_Mail_Data := AWS.SMTP.E_Mail
        (Sender.To_UTF_8_String, "ada_ru@ada-ru.org");

      BCC  : constant AWS.SMTP.E_Mail_Data :=
        AWS.SMTP.E_Mail ("", "ada_ru_list");

      Receiver : constant AWS.SMTP.Receiver :=
        AWS.SMTP.Initialize ("forge.ada-ru.org");

      Subject  : constant League.Strings.Universal_String :=
        (if Mail.Subject.Index ("[ada_ru]") > 0 then Mail.Subject
         else "[ada_ru] " & Mail.Subject);

      Header : League.String_Vectors.Universal_String_Vector;
      Text   : League.Strings.Universal_String := Escape (Mail.Text);
      Status : AWS.SMTP.Status;

   begin
      Header.Append
        ("From: " & Escape_Header (Sender) & " <ada_ru@ada-ru.org>");

      Header.Append (+"Bcc: ada_ru_list");

      if not Mail.In_Reply_To.Is_Empty then
         Header.Append ("In-Reply-To: " & Mail.In_Reply_To);
      end if;

      Header.Append
        (+"List-Unsubscribe: <mailto:ada_ru-unsubscribe@ada-ru.org>");

      Header.Append
        (+"Mailing-List: list ada_ru@ada-ru.org;" &
           " contact ada_ru-owner@ada-ru.org");

      Header.Append (+"Message-ID: " & Mail.Message_Id);
      Header.Append ("Subject: " & Escape_Header (Subject));
      Header.Append (+"List-Id: <ada_ru.ada-ru.org>");
      Header.Append (+"Content-Type: text/plain; charset=utf-8");
      Header.Append (+"Content-Transfer-Encoding: base64");
      Header.Append (League.Strings.Empty_Universal_String);
      Header.Append (League.Strings.Empty_Universal_String);

      Wrap_Lines (Text);

      Text.Prepend (Header.Join (Ada.Characters.Wide_Wide_Latin_1.LF));

      AWS.SMTP.Client.Send
        (Server  => Receiver,
         From    => From,
         To      => (1 .. 0 => <>),
         BCC     => (1 => BCC),
         Source  => Text.To_UTF_8_String,
         Status  => Status);

      if not AWS.SMTP.Is_Ok (Status) then
         Ada.Wide_Wide_Text_IO.Put ("SMTP send fails: ");
         Ada.Wide_Wide_Text_IO.Put (Mail.Message_Id.To_Wide_Wide_String);
         Ada.Wide_Wide_Text_IO.New_Line;
         Ada.Text_IO.Put_Line (AWS.SMTP.Status_Message (Status));
      end if;
   end Send_Mail;

   ---------------------------
   -- Strip_Carriage_Return --
   ---------------------------

   function Strip_Carriage_Return
    (Text : League.Strings.Universal_String)
     return League.Strings.Universal_String
   is
      List : constant League.String_Vectors.Universal_String_Vector :=
        Text.Split (Ada.Characters.Wide_Wide_Latin_1.CR);
   begin
      return List.Join ("");
   end Strip_Carriage_Return;

   ----------------
   -- Wrap_Lines --
   ----------------

   procedure Wrap_Lines (Text : in out League.Strings.Universal_String) is
      Result   : League.String_Vectors.Universal_String_Vector;
      Max_Line : constant := 76;
      First    : Positive;
      Last     : Natural;
   begin
      for J in 1 .. (Text.Length + Max_Line - 1) / Max_Line loop
         First := (J - 1) * Max_Line + 1;
         Last := Natural'Min (First + Max_Line - 1, Text.Length);
         Result.Append (Text.Slice (First, Last));
      end loop;

      Text := Result.Join (Ada.Characters.Wide_Wide_Latin_1.LF);
   end Wrap_Lines;

end Servlet.Forum;
