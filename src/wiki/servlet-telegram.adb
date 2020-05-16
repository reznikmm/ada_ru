with Ada.Streams;
with Ada.Calendar.Conversions;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;

with AWS.Client;
with AWS.Messages;
with AWS.Response;

with League.JSON.Arrays;
with League.JSON.Documents;
with League.JSON.Values;
with League.Stream_Element_Vectors;
with League.String_Vectors;
with League.Text_Codecs;

package body Servlet.Telegram is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

   procedure To_JSON
     (Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Document : out League.JSON.Documents.JSON_Document);

   procedure Delete_Message
     (Message  : League.JSON.Objects.JSON_Object;
      Response : out League.JSON.Objects.JSON_Object);

   procedure Delete_Message
     (Connection : in out AWS.Client.HTTP_Connection;
      Token      : League.Strings.Universal_String;
      Message  : Message_Identifier;
      Chat     : Chat_Identifier);

   procedure Kick_User
     (Connection : in out AWS.Client.HTTP_Connection;
      Token      : League.Strings.Universal_String;
      User       : User_Identifier;
      Chat       : Chat_Identifier);

   procedure Send_Message
     (Connection : in out AWS.Client.HTTP_Connection;
      Token      : League.Strings.Universal_String;
      Object     : League.JSON.Objects.JSON_Object;
      Message_Id : out Message_Identifier);

   procedure Callback
     (Self     : in out Telegram_Servlet;
      Query    : League.JSON.Objects.JSON_Object;
      Response : out League.JSON.Objects.JSON_Object);

   -----------------------
   --  String Constants --
   -----------------------

   chat : constant League.Strings.Universal_String := +"chat";
   chat_id : constant League.Strings.Universal_String := +"chat_id";
   from : constant League.Strings.Universal_String := +"from";
   id : constant League.Strings.Universal_String := +"id";
   left_chat_member : constant League.Strings.Universal_String :=
     +"left_chat_member";
   new_chat_members : constant League.Strings.Universal_String :=
     +"new_chat_members";
   message_id : constant League.Strings.Universal_String := +"message_id";
   method : constant League.Strings.Universal_String := +"method";
   callback_query : constant League.Strings.Universal_String :=
     +"callback_query";
   reply_to_message : constant League.Strings.Universal_String :=
     +"reply_to_message";

   ---------------------
   -- Analyze_Message --
   ---------------------

   not overriding procedure Analyze_Message
    (Self     : in out Telegram_Servlet;
     Message  : League.JSON.Objects.JSON_Object;
     Result   : out Message_Action)
   is

      procedure Greeting (User : League.JSON.Objects.JSON_Object);

      --------------
      -- Greeting --
      --------------

      procedure Greeting (User : League.JSON.Objects.JSON_Object) is
         Input   : Ada.Wide_Wide_Text_IO.File_Type;
         Pattern : constant Wide_Wide_String := "<user_id>";
         User_Id : constant User_Identifier := User.Value (id).To_Integer;
         Image   : constant Wide_Wide_String :=
           User_Identifier'Wide_Wide_Image (User_Id);
         Chat_Name : constant League.Strings.Universal_String :=
           Message.Value (chat).To_Object.Value (+"username").To_String;
         Chat_Id   : constant Chat_Identifier :=
           Message.Value (chat).To_Object.Value (id).To_Integer;
         Join_Id   : constant Servlet.Telegram.Message_Identifier :=
           Message.Value (message_id).To_Integer;
         Text      : League.Strings.Universal_String;
         Item      : League.Strings.Universal_String;
         From      : Natural;
         Reply     : League.JSON.Objects.JSON_Object;
         Inline    : League.JSON.Arrays.JSON_Array;
         --  ^^ Array of Array of InlineKeyboardButton
         Good : Natural;
         Response : League.JSON.Objects.JSON_Object;
      begin
         Self.New_Users.Add_User
           (Id      => User_Id,
            Chat    => Chat_Id,
            Message => Join_Id,
            Good    => Good);

         Ada.Wide_Wide_Text_IO.Open
           (Input,
            Ada.Wide_Wide_Text_IO.In_File,
            "install/CAPTCHA/" & Chat_Name.To_UTF_8_String & "/greeting.txt",
            "SHARED=NO");

         while not Ada.Wide_Wide_Text_IO.End_Of_File (Input) loop
            declare
               Line : constant Wide_Wide_String :=
                 Ada.Wide_Wide_Text_IO.Get_Line (Input);
            begin
               if Line /= "" then
                  Item.Append (Line);
               elsif Text.Is_Empty then
                  Text := Item;
                  Item.Clear;
                  From := Text.Index (Pattern);

                  if From > 0 then
                     Text.Replace
                       (From,
                        From + Pattern'Length - 1,
                        Image (2 .. Image'Last));
                  end if;
               else
                  declare
                     --  InlineKeyboardButton
                     Is_Good : constant Boolean := Item.Starts_With ("*");
                     Button  : League.JSON.Objects.JSON_Object;
                     Row     : League.JSON.Arrays.JSON_Array;
                     Answer  : Natural;
                  begin
                     if Is_Good then
                        Item := Item.Tail_From (2);
                        Answer := Good;
                     else
                        Self.New_Users.Get_Random (User_Id, Answer);
                     end if;

                     Button.Insert
                       (+"text", League.JSON.Values.To_JSON_Value (Item));
                     Button.Insert
                       (+"callback_data",
                        League.JSON.Values.To_JSON_Value
                          (+("JOIN" & Natural'Wide_Wide_Image (Answer))));
                     Row.Append (Button.To_JSON_Value);
                     Inline.Append (Row.To_JSON_Value);
                     Item.Clear;
                  end;
               end if;
            end;
         end loop;

         Ada.Wide_Wide_Text_IO.Close (Input);

         Reply.Insert
           (+"force_reply",
            League.JSON.Values.To_JSON_Value (True));
         Reply.Insert
           (+"selective",
            League.JSON.Values.To_JSON_Value (True));
         Reply.Insert (+"inline_keyboard", Inline.To_JSON_Value);

         Response.Insert
           (Servlet.Telegram.chat_id,
            Message.Value (chat).To_Object.Value (id));
         Response.Insert
           (+"text",
            League.JSON.Values.To_JSON_Value (Text));
         Response.Insert
           (+"parse_mode",
            League.JSON.Values.To_JSON_Value (+"Markdown"));
         Response.Insert
           (+"disable_notification",
            League.JSON.Values.To_JSON_Value (True));
         Response.Insert
           (+"reply_to_message_id",
            Message.Value (message_id));
         Response.Insert (+"reply_markup", Reply.To_JSON_Value);

         declare
            Id         : Message_Identifier;
            Connection : AWS.Client.HTTP_Connection;
         begin
            AWS.Client.Create (Connection, "https://api.telegram.org");
            Send_Message (Connection, Self.Token, Response, Id);
            Self.New_Users.New_Reply
              (Id       => Id,
               Reply_To => Join_Id);
         end;

      exception
         when Ada.Wide_Wide_Text_IO.Name_Error =>
            null;
         when E : others =>
            Ada.Text_IO.Put_Line
              (Ada.Exceptions.Exception_Information (E));
      end Greeting;

      From_Value : constant League.JSON.Values.JSON_Value :=
        Message.Value (from);
      New_Members_Value : constant League.JSON.Values.JSON_Value :=
        Message.Value (new_chat_members);
   begin
      Result := Pass;

      if New_Members_Value.Is_Array then
         declare
            List : constant League.JSON.Arrays.JSON_Array :=
              New_Members_Value.To_Array;
         begin
            for J in 1 .. List.Length loop
               declare
                  User : constant League.JSON.Objects.JSON_Object :=
                    List.Element (J).To_Object;
               begin
                  Greeting (User);
               end;
            end loop;

            Result := Skip;
         end;
      elsif Message.Contains (left_chat_member) then
            Result := Skip;
      elsif From_Value.Is_Object then
         declare
            User : constant League.JSON.Objects.JSON_Object :=
              From_Value.To_Object;

            User_Id : constant User_Identifier := User.Value (id).To_Integer;

         begin
            if Self.New_Users.Contains (User_Id) then
               Result := Delete;
            end if;
         end;
      elsif Message.Contains (reply_to_message) then
         Self.New_Users.New_Reply
           (Id       => Message.Value (message_id).To_Integer,
            Reply_To => Message.Value (reply_to_message).To_Object
              .Value (message_id).To_Integer);
      end if;
   end Analyze_Message;

   --------------
   -- Callback --
   --------------

   procedure Callback
     (Self     : in out Telegram_Servlet;
      Query    : League.JSON.Objects.JSON_Object;
      Response : out League.JSON.Objects.JSON_Object)
   is
      From_Value : constant League.JSON.Objects.JSON_Object :=
        Query.Value (from).To_Object;
      User_Id : constant User_Identifier := From_Value.Value (id).To_Integer;
      Data    : constant League.Strings.Universal_String :=
        Query.Value (+"data").To_String;
      From       : Positive := 1;
   begin
      if Data.Starts_With ("JOIN ") then
         declare
            use type Message_Identifier;
            Greeting : Servlet.Telegram.Greeting;
            List : constant League.String_Vectors.Universal_String_Vector :=
              Data.Split (' ');
            Answer : Natural;
            Connection : AWS.Client.HTTP_Connection;
         begin
            AWS.Client.Create (Connection, "https://api.telegram.org");

            Answer := Natural'Wide_Wide_Value (List (2).To_Wide_Wide_String);
            Self.New_Users.Delete_User (User_Id, Greeting);

            if Greeting.Chat /= 0 then
               if Answer = Greeting.Good then
                  From := 2;
                  Response.Insert
                    (+"text",
                     League.JSON.Values.To_JSON_Value (+"good"));
               else
                  From := 1;
                  Response.Insert
                    (+"text",
                     League.JSON.Values.To_JSON_Value (+"bad"));
                  Kick_User (Connection, Self.Token, User_Id, Greeting.Chat);
               end if;

               for J in From .. Greeting.Messages'Last loop
                  if Greeting.Messages (J) /= 0  then
                     Delete_Message
                       (Connection,
                        Self.Token,
                        Greeting.Messages (J),
                        Greeting.Chat);
                  end if;
               end loop;
            end if;
         exception
            when Constraint_Error =>
               null;
         end;
      end if;

      Response.Insert
        (method,
         League.JSON.Values.To_JSON_Value (+"answerCallbackQuery"));

      Response.Insert (+"callback_query_id", Query.Value (id));
   end Callback;

   --------------------
   -- Delete_Message --
   --------------------

   procedure Delete_Message
     (Message  : League.JSON.Objects.JSON_Object;
      Response : out League.JSON.Objects.JSON_Object) is
   begin
      Response.Insert
        (method,
         League.JSON.Values.To_JSON_Value (+"deleteMessage"));
      Response.Insert (chat_id, Message.Value (chat).To_Object.Value (id));
      Response.Insert (message_id, Message.Value (message_id));
   end Delete_Message;

   --------------------
   -- Delete_Message --
   --------------------

   procedure Delete_Message
     (Connection : in out AWS.Client.HTTP_Connection;
      Token      : League.Strings.Universal_String;
      Message    : Message_Identifier;
      Chat       : Chat_Identifier)
   is
      Result : AWS.Response.Data;
      Method : constant String := "deleteMessage";
      Header : AWS.Client.Header_List;
      Item   : League.JSON.Objects.JSON_Object;
   begin
      Item.Insert (chat_id, League.JSON.Values.To_JSON_Value (Chat));
      Item.Insert (message_id, League.JSON.Values.To_JSON_Value (Message));
      Header.Add ("Connection", "Keep-Alive");

      AWS.Client.Post
        (Connection,
         Result,
         Item.To_JSON_Document.To_JSON.To_Stream_Element_Array,
         Content_Type => "application/json",
         Headers      => Header,
         URI          => "/bot" & Token.To_UTF_8_String & "/" & Method);

      Ada.Text_IO.Put_Line (AWS.Response.Status_Code (Result)'Img);
      Ada.Text_IO.Put_Line (AWS.Response.Message_Body (Result));
   end Delete_Message;

   -------------
   -- Do_Post --
   -------------

   overriding procedure Do_Post
     (Self     : in out Telegram_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      Document : League.JSON.Documents.JSON_Document;
      Object   : League.JSON.Objects.JSON_Object;
      Result   : League.JSON.Objects.JSON_Object;
      Action   : Message_Action;
   begin
      To_JSON (Request, Document);
      Object := Document.To_JSON_Object;

      if Object.Contains (+"message") then
         Object := Object.Value (+"message").To_Object;
         Self.Analyze_Message (Object, Action);

         case Action is
            when Pass =>
               Self.Listener.On_Telegram (Object, Result);
            when Skip =>
               null;
            when Delete =>
               Delete_Message (Object, Result);
         end case;
      elsif Object.Contains (callback_query) then
         Self.Callback (Object.Value (callback_query).To_Object, Result);
      end if;

      Response.Set_Status (Servlet.HTTP_Responses.OK);
      Response.Set_Content_Type (+"application/json");
      Response.Set_Character_Encoding (+"utf-8");

      if Result.Is_Empty then
         Result := League.JSON.Objects.Empty_JSON_Object;
      end if;

      Document := Result.To_JSON_Document;
      Response.Get_Output_Stream.Write (Document.To_JSON);
      Ada.Wide_Wide_Text_IO.Put_Line (Document.To_JSON.To_Wide_Wide_String);
   end Do_Post;

   ----------------------
   -- Get_Servlet_Info --
   ----------------------

   overriding function Get_Servlet_Info
     (Self : Telegram_Servlet)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return +"Telegram Servlet";
   end Get_Servlet_Info;

   ----------
   -- Hash --
   ----------

   function Hash (Value : User_Identifier) return Ada.Containers.Hash_Type is
      use type User_Identifier;
   begin
      return Ada.Containers.Hash_Type'Val (abs Value);
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : in out Telegram_Servlet'Class;
      Token : League.Strings.Universal_String) is
   begin
      Self.Token := Token;
   end Initialize;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
      return Telegram_Servlet
   is
      pragma Unreferenced (Parameters);
   begin
      return Result : Telegram_Servlet do
         Result.New_Users.Reset;
      end return;
   end Instantiate;

   ---------------
   -- Kick_User --
   ---------------

   procedure Kick_User
     (Connection : in out AWS.Client.HTTP_Connection;
      Token      : League.Strings.Universal_String;
      User       : User_Identifier;
      Chat       : Chat_Identifier)
   is
      use type Ada.Calendar.Time;

      Result : AWS.Response.Data;
      Method : constant String := "kickChatMember";
      Header : AWS.Client.Header_List;
      Item   : League.JSON.Objects.JSON_Object;
   begin
      Item.Insert (chat_id, League.JSON.Values.To_JSON_Value (Chat));
      Item.Insert (+"user_id", League.JSON.Values.To_JSON_Value (User));
      Item.Insert
        (+"until_date",
         League.JSON.Values.To_JSON_Value
          (League.Holders.Universal_Integer
            (Ada.Calendar.Conversions.To_Unix_Time
              (Ada.Calendar.Clock + 60.0))));

      Header.Add ("Connection", "Keep-Alive");

      AWS.Client.Post
        (Connection,
         Result,
         Item.To_JSON_Document.To_JSON.To_Stream_Element_Array,
         Content_Type => "application/json",
         Headers      => Header,
         URI          => "/bot" & Token.To_UTF_8_String & "/" & Method);

      Ada.Text_IO.Put_Line (AWS.Response.Status_Code (Result)'Img);
      Ada.Text_IO.Put_Line (AWS.Response.Message_Body (Result));
   end Kick_User;

   ---------------
   -- Newcomers --
   ---------------

   protected body Newcomers is

      --------------
      -- Add_User --
      --------------

      procedure Add_User
        (Id      : User_Identifier;
         Chat    : Chat_Identifier;
         Message : League.Holders.Universal_Integer;
         Good    : out Natural) is
      begin
         Good := Natural_Random.Random (Random);

         Map.Include
           (Id,
            (Time => League.Calendars.Clock,
             Chat => Chat,
             Good => Good,
             Messages => (Message, others => 0)));
      end Add_User;

      --------------
      -- Contains --
      --------------

      function Contains (User_Id : User_Identifier) return Boolean is
      begin
         return Map.Contains (User_Id);
      end Contains;

      -----------------
      -- Delete_User --
      -----------------

      procedure Delete_User
        (Id    : User_Identifier;
         Value : out Greeting) is
      begin
         if Map.Contains (Id) then
            Value := Map (Id);
            Map.Delete (Id);
         else
            Value :=
              (Chat => 0, Good => 0, Messages => (others => 0), others => <>);
         end if;
      end Delete_User;

      ----------------
      -- Get_Random --
      ----------------

      procedure Get_Random
        (Id    : User_Identifier;
         Value : out Natural)
      is
         Good : Natural := 0;
      begin
         if Map.Contains (Id) then
            Good := Map (Id).Good;
         end if;

         Value := Good;

         while Value = Good loop
            Value := Natural_Random.Random (Random);
         end loop;
      end Get_Random;

      ---------------
      -- New_Reply --
      ---------------

      procedure New_Reply
        (Id       : Message_Identifier;
         Reply_To : Message_Identifier)
      is
         use type Message_Identifier;
      begin
         for J of Map loop
            if Reply_To = J.Messages (1) then
               J.Messages :=
                 J.Messages (1) & Id & J.Messages (2 .. J.Messages'Last - 1);
            end if;
         end loop;
      end New_Reply;

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Natural_Random.Reset (Random);
      end Reset;

   end Newcomers;

   procedure Send_Message
     (Connection : in out AWS.Client.HTTP_Connection;
      Token      : League.Strings.Universal_String;
      Object     : League.JSON.Objects.JSON_Object;
      Message_Id : out Message_Identifier)
   is
      Result : AWS.Response.Data;
      Method : constant String := "sendMessage";
      Header : AWS.Client.Header_List;
   begin
      Header.Add ("Connection", "Keep-Alive");

      AWS.Client.Post
        (Connection,
         Result,
         Object.To_JSON_Document.To_JSON.To_Stream_Element_Array,
         Content_Type => "application/json",
         Headers      => Header,
         URI          => "/bot" & Token.To_UTF_8_String & "/" & Method);

      Ada.Text_IO.Put_Line (AWS.Response.Status_Code (Result)'Img);
      Ada.Text_IO.Put_Line (AWS.Response.Message_Body (Result));

      if AWS.Response.Status_Code (Result) in AWS.Messages.Success then
         declare
            Document : constant League.JSON.Documents.JSON_Document :=
              League.JSON.Documents.From_JSON
                (AWS.Response.Message_Body (Result));
         begin
            Message_Id := Document.To_JSON_Object
              .Value (+"result").To_Object
              .Value (Telegram.message_id).To_Integer;
         end;
      else
         Message_Id := 0;
      end if;
   end Send_Message;

   ------------------
   -- Set_Listener --
   ------------------

   not overriding procedure Set_Listener
     (Self  : in out Telegram_Servlet;
      Value : access Axe.Events.Listener'Class)
   is
   begin
      Self.Listener := Value;
   end Set_Listener;

   -------------
   -- To_JSON --
   -------------

   procedure To_JSON
     (Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Document : out League.JSON.Documents.JSON_Document)
   is
      use type Ada.Streams.Stream_Element_Count;
      Stream : constant access Ada.Streams.Root_Stream_Type'Class :=
        Request.Get_Input_Stream;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 512);
      Vector : League.Stream_Element_Vectors.Stream_Element_Vector;
      Last   : Ada.Streams.Stream_Element_Count;
   begin
      loop
         Stream.Read (Buffer, Last);
         exit when Last = 0;
         Vector.Append (Buffer (1 .. Last));
      end loop;

      Ada.Wide_Wide_Text_IO.Put_Line
        (League.Text_Codecs.Codec_For_Application_Locale.
           Decode (Vector).To_Wide_Wide_String);

      Document := League.JSON.Documents.From_JSON (Vector);
   end To_JSON;

end Servlet.Telegram;
