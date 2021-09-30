with Ada.Calendar;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Wide_Wide_Text_IO;

with AWS.Headers;
with AWS.Messages;
with AWS.Response;

with League.Calendars.ISO_8601;
with League.Characters.Latin;
with League.Holders;
with League.JSON.Arrays;
with League.JSON.Documents;
with League.JSON.Values;

with SQL.Queries;

with XML.SAX.Attributes;
with XML.SAX.Pretty_Writers;

with XMPP.Presences;

package body Axe.Bots is

   use type League.Strings.Universal_String;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

   procedure Send_IRC
     (Self   : in out Bot'Class;
      Value  : Original_Message);

   procedure IRC_Send_Text
     (Self   : in out Bot'Class;
      Text   : League.Strings.Universal_String;
      Before : Boolean);

   procedure Send_Telegram
     (Self   : in out Bot'Class;
      Value  : Original_Message);

   procedure Send_Viber
     (Self   : in out Bot'Class;
      Value  : Original_Message);

   procedure Send_XMPP
     (Self   : in out Bot'Class;
      Value  : Original_Message);

   procedure Read_Subscribers
     (Vector : in out League.String_Vectors.Universal_String_Vector);

   procedure Write_Subscribers
     (Value : League.String_Vectors.Universal_String_Vector);

   procedure Telegram_Request
     (Self     : in out Bot'Class;
      Method   : String;
      Object   : League.JSON.Objects.JSON_Object;
      Response : out League.JSON.Objects.JSON_Object);

   package XMPP_Photos is
      type XMPP_Photo is new XMPP.Messages.XMPP_Message with record
         URL     : League.Strings.Universal_String;
         Caption : League.Strings.Universal_String;
      end record;

      overriding procedure Custom_Content
        (Self   : XMPP_Photo;
         Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class);
   end XMPP_Photos;

   Send : constant array (IRC_Origin .. Viber_Origin) of access
     procedure (Self  : in out Bot'Class;
                Value : Original_Message) :=
     (IRC_Origin      => Send_IRC'Access,
      XMPP_Origin     => Send_XMPP'Access,
      Telegram_Origin => Send_Telegram'Access,
      Viber_Origin    => Send_Viber'Access);

   package body XMPP_Photos is

      xhtml_im : constant League.Strings.Universal_String :=
        +"http://jabber.org/protocol/xhtml-im";

      xhtml : constant League.Strings.Universal_String :=
        +"http://www.w3.org/1999/xhtml";

      --------------------
      -- Custom_Content --
      --------------------

      overriding procedure Custom_Content
        (Self   : XMPP_Photo;
         Writer : in out XML.SAX.Pretty_Writers.XML_Pretty_Writer'Class)
      is
         Attrs : XML.SAX.Attributes.SAX_Attributes;
      begin
         Writer.Start_Prefix_Mapping (Namespace_URI => xhtml_im);

         Writer.Start_Element
           (Namespace_URI => xhtml_im,
            Local_Name    => +"html");

         Writer.Start_Prefix_Mapping (Namespace_URI => xhtml);

         Writer.Start_Element
           (Namespace_URI => xhtml,
            Local_Name    => +"body");

         Writer.Start_Element (Qualified_Name => +"p");

         Attrs.Set_Value (+"src", Self.URL);
         Attrs.Set_Value (+"alt", Self.Caption);
         Writer.Start_Element (Qualified_Name => +"img", Attributes => Attrs);

         Writer.End_Element (Qualified_Name => +"img");
         Writer.End_Element (Qualified_Name => +"p");
         Writer.End_Element (Namespace_URI => xhtml,
                             Local_Name => +"body");
         Writer.End_Element (Namespace_URI => xhtml_im,
                             Local_Name => +"html");
      end Custom_Content;

   end XMPP_Photos;

   -------------------------
   -- Bind_Resource_State --
   -------------------------

   overriding procedure Bind_Resource_State
     (Self   : in out XMPP_Listener;
      JID    : League.Strings.Universal_String;
      Status : XMPP.Bind_State)
   is
      pragma Unreferenced (JID);
   begin
      if Status in XMPP.Success then
         --  After resource binded successfull establishing session
         Self.XMPP_Session.Establish_IQ_Session;
      end if;
   end Bind_Resource_State;

   --------------
   -- Bot_Loop --
   --------------

   task body Bot_Loop is
      use type Ada.Calendar.Time;

      Time         : Ada.Calendar.Time := Ada.Calendar.Clock;
      Next_Message : Original_Message;

      Status   : GNAT.Sockets.Selector_Status;
      Item     : Schedulers.Runable_Access;
   begin
      accept Start;

      Bot.XMPP_Session.Open;

      GNAT.Sockets.Create_Selector (Bot.Selector);

      Bot.New_Runable
        ((Time  => Ada.Calendar.Clock + 1.0,
          Value => Bot.IRC_Connect'Unchecked_Access));

      loop
         Schedulers.Scheduler.Get_Ready (Item);
         Item.Start_If_Assigned;

         declare  --  Read IRC if connected
            Read  : GNAT.Sockets.Socket_Set_Type;
            Write : GNAT.Sockets.Socket_Set_Type;
            Error : GNAT.Sockets.Socket_Set_Type;
         begin
            if Bot.IRC_Online then
               GNAT.Sockets.Set (Read, Bot.IRC_Socket);
               GNAT.Sockets.Set (Error, Bot.IRC_Socket);
            end if;

            GNAT.Sockets.Check_Selector
              (Selector     => Bot.Selector,
               R_Socket_Set => Read,
               W_Socket_Set => Write,
               E_Socket_Set => Error,
               Status       => Status,
               Timeout      => Duration'Max
                 (Schedulers.Scheduler.Next (60.0) - Ada.Calendar.Clock,
                  0.0));

            if Status in GNAT.Sockets.Completed then
               if not GNAT.Sockets.Is_Empty (Read) then
                  Bot.IRC_Session.Check_Socket (Bot.IRC_Online);

                  if not Bot.IRC_Online then
                     Ada.Wide_Wide_Text_IO.Put_Line ("Disconnected from IRC");
                     Bot.New_Runable
                       ((Time  => Ada.Calendar.Clock + 10.0,
                         Value => Bot.IRC_Connect'Unchecked_Access));
                  end if;
               end if;
            end if;
         end;

         loop
            select
               Bot.Queue.Dequeue (Next_Message);

               delay until Time;
               Time := Ada.Calendar.Clock + 0.5;  --  Avoid to Flood errors

               for Origin in Send'Range loop
                  if Next_Message.Origin /= Origin
                    or else Next_Message.Origin = Viber_Origin
                  then
                     Send (Origin) (Bot.all, Next_Message);
                  end if;
               end loop;

            else
               exit;
            end select;
         end loop;
      end loop;
   exception
      when E : others =>
         Ada.Wide_Wide_Text_IO.Put_Line
           ("Bot_Loop dead: " &
              Ada.Exceptions.Wide_Wide_Exception_Name (E));
         Ada.Wide_Wide_Text_IO.Put_Line
           (League.Strings.From_UTF_8_String
              (Ada.Exceptions.Exception_Information (E)).To_Wide_Wide_String);
   end Bot_Loop;

   -----------------
   -- Get_Options --
   -----------------

   function Get_Options return SQL.Options.SQL_Options is
   begin
      return Result : SQL.Options.SQL_Options do
         Result.Set (+"dbname", +"mail");
      end return;
   end Get_Options;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : in out Bot;
      Password : League.Strings.Universal_String;
      Telegram : League.Strings.Universal_String;
      Viber    : League.Strings.Universal_String) is
   begin
      --  XMPP.Logger.Enable_Debug;
      --  AWS.Client.Set_Debug (True);

      Self.IRC_Session := new IRC.Sessions.Session
        (Self.IRC_Listener'Unchecked_Access);
      Self.IRC_Listener.Password := Password;
      Self.Network_Loop.Start;
      Self.XMPP_Session.Set_JID (+"ada_ru@jabber.ru");
      Self.XMPP_Session.Set_Password (Password);
      Self.XMPP_Session.Set_Host (+"jabber.ru");
      Self.XMPP_Session.Set_Resource (+"server");
      Self.XMPP_Session.Set_Stream_Handler
        (Self.XMPP_Listener'Unchecked_Access);

      Self.XMPP_Listener.XMPP_Session := Self.XMPP_Session'Unchecked_Access;

      AWS.Client.Create (Self.Telegram.Connection, "https://api.telegram.org");
      Self.Telegram.Token := Telegram;

      AWS.Client.Create (Self.Viber.Connection, "https://chatapi.viber.com");

      Self.DB.Open;

      Self.Viber.Token := Viber;
      Read_Subscribers (Self.Viber.Subscribed);
   end Initialize;

   -------------------
   -- IRC_Send_Text --
   -------------------

   procedure IRC_Send_Text
     (Self   : in out Bot'Class;
      Text   : League.Strings.Universal_String;
      Before : Boolean)
   is
      use type Ada.Calendar.Time;

      Ok : Boolean;
   begin
      if Self.IRC_Online and (Before or Self.IRC_Queue.Is_Empty) then
         Self.IRC_Session.Send_Message (+"#ada", Text, Ok);

         if Ok then
            return;
         end if;

         Self.IRC_Online := False;
         Self.New_Runable
           ((Time => Ada.Calendar.Clock + 60.0,
             Value => Self.IRC_Connect'Unchecked_Access));
      end if;

      if Before then
         declare
            List : League.String_Vectors.Universal_String_Vector;
         begin
            List.Append (Text);
            Self.IRC_Queue.Prepend (List);
         end;
      else
         Self.IRC_Queue.Append (Text);
      end if;
   end IRC_Send_Text;

   -------------
   -- Message --
   -------------

   overriding procedure Message
     (Self : in out XMPP_Listener;
      Msg  : XMPP.Messages.XMPP_Message'Class)
   is
      From : League.Strings.Universal_String := Msg.Get_From;
      Text : constant League.Strings.Universal_String := Msg.Get_Body;
   begin
      Ada.Wide_Wide_Text_IO.Put_Line (Msg.Get_From.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Put_Line (Msg.Get_To.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Put_Line (Text.To_Wide_Wide_String);

      if not From.Ends_With ("/ada_ru") and not Text.Is_Empty then
         From := From.Tail_From (From.Last_Index ('/') + 1);
         Self.Bot.Send_Message
           ((Axe.Bots.Text, XMPP_Origin,
            (Name => From, others => <>),
            Text));
      end if;
   end Message;

   -----------------
   -- New_Runable --
   -----------------

   not overriding procedure New_Runable
     (Self  : in out Bot;
      Value : Axe.Schedulers.Scheduled_Item) is
   begin
      Axe.Schedulers.Scheduler.Insert (Value);
      GNAT.Sockets.Abort_Selector (Self.Selector);
   end New_Runable;

   ----------------
   -- On_Message --
   ----------------

   overriding procedure On_Message
     (Self    : access IRC_Listener;
      Session : access IRC.Sessions.Session'Class;
      Target  : League.Strings.Universal_String;
      Source  : League.Strings.Universal_String;
      Text    : League.Strings.Universal_String)
   is
      pragma Unreferenced (Session);
      From : constant League.Strings.Universal_String :=
        Source.Head_To (Source.Index ("!") - 1);
   begin
      Ada.Wide_Wide_Text_IO.Put (Source.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Put (" : ");
      Ada.Wide_Wide_Text_IO.Put_Line (Text.To_Wide_Wide_String);

      if Target.Starts_With ("#") then
         if From /= +"ada_ru" then
            Self.Bot.Send_Message
              ((Axe.Bots.Text, IRC_Origin,
               (Name => From, others => <>),
               Text));
         end if;
      end if;
   end On_Message;

   ---------------
   -- On_Notice --
   ---------------

   overriding procedure On_Notice
     (Self    : access IRC_Listener;
      Session : access IRC.Sessions.Session'Class;
      Target  : League.Strings.Universal_String;
      Source  : League.Strings.Universal_String;
      Text    : League.Strings.Universal_String)
   is
      pragma Unreferenced (Target);
   begin
      if Source.Starts_With ("NickServ") and not Self.Identified then
         Ada.Wide_Wide_Text_IO.Put (Source.To_Wide_Wide_String);
         Ada.Wide_Wide_Text_IO.Put (" : ");
         Ada.Wide_Wide_Text_IO.Put_Line (Text.To_Wide_Wide_String);
         Session.Raw_Command (+"NickServ IDENTIFY " & Self.Password);
         Self.Identified := True;

         Session.Join (+"#ada");
      end if;
   end On_Notice;

   -------------
   -- On_Ping --
   -------------

   overriding procedure On_Ping
     (Self    : access IRC_Listener;
      Session : access IRC.Sessions.Session'Class;
      Source  : League.Strings.Universal_String)
   is
      pragma Unreferenced (Self);
   begin
      Session.Pong (Source);
   end On_Ping;

   ---------------------
   -- Read_Subscribers --
   ---------------------

   procedure Read_Subscribers
     (Vector : in out League.String_Vectors.Universal_String_Vector)
   is
      File : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Ada.Wide_Wide_Text_IO.Open
        (File,
         Ada.Wide_Wide_Text_IO.In_File,
         "password/viber_subscribers");

      while not Ada.Wide_Wide_Text_IO.End_Of_File (File) loop
         declare
            Line : constant Wide_Wide_String :=
              Ada.Wide_Wide_Text_IO.Get_Line (File);
         begin
            if Line'Length > 0 then
               Vector.Append (+Line);
            end if;
         end;
      end loop;

      Ada.Wide_Wide_Text_IO.Close (File);
   end Read_Subscribers;

   ---------
   -- Run --
   ---------

   overriding procedure Run (Self : aliased in out IRC_Reconnector) is
      use type GNAT.Sockets.Socket_Type;
      use type Ada.Calendar.Time;
   begin
      if not Self.Bot.IRC_Online then  --  Try to connect IRC
         Self.Bot.IRC_Session.Connect
           (Socket    => Self.Bot.IRC_Socket,
            Host      => +"irc.lucky.net",
            Port      => 7777,
            Nick      => +"ada-ru",
            Password  => +"",
            User      => +"ada_ru",
            Real_Name => +"Ada Ru Bot");

         if Self.Bot.IRC_Socket in GNAT.Sockets.No_Socket then
            Ada.Wide_Wide_Text_IO.Put_Line ("Connect to IRC failed");
            Self.Bot.New_Runable
              ((Time => Ada.Calendar.Clock + 60.0,
                Value => Self'Unchecked_Access));
         else
            Ada.Wide_Wide_Text_IO.Put_Line ("Connected to IRC");
            Self.Bot.IRC_Online := True;
            Self.Bot.New_Runable
              ((Time => Ada.Calendar.Clock + 15.0,
                Value => Self'Unchecked_Access));
         end if;
      elsif not Self.Bot.IRC_Queue.Is_Empty then
         declare
            Text : constant League.Strings.Universal_String :=
              Self.Bot.IRC_Queue.Element (1);
         begin
            Self.Bot.IRC_Queue := Self.Bot.IRC_Queue.Slice
              (2, Self.Bot.IRC_Queue.Length);

            Self.Bot.IRC_Send_Text (Text, True);

            if Self.Bot.IRC_Online and not Self.Bot.IRC_Queue.Is_Empty then
               Self.Bot.New_Runable
                 ((Time  => Ada.Calendar.Clock + 2.0,
                   Value => Self'Unchecked_Access));
            end if;
         end;
      end if;
   end Run;

   --------------
   -- Send_IRC --
   --------------

   procedure Send_IRC
     (Self   : in out Bot'Class;
      Value  : Original_Message)
   is
      Text   : constant League.Strings.Universal_String :=
        "(" & Value.Sender.Name & ") " & Value.Text;
   begin
      Self.IRC_Send_Text (Text, Before => False);
   end Send_IRC;

   ------------------
   -- Send_Message --
   ------------------

   not overriding procedure Send_Message
     (Self   : in out Bot;
      Text   : League.Strings.Universal_String) is
   begin
      Self.Send_Message
        ((Sender => (others => <>),
          Text   => Text,
          Origin => Other_Origin,
          Kind   => Axe.Bots.Text));
   end Send_Message;

   ------------------
   -- Send_Message --
   ------------------

   not overriding procedure Send_Message
     (Self   : in out Bot;
      Value  : Original_Message) is
   begin
      Self.Queue.Enqueue (Value);
      GNAT.Sockets.Abort_Selector (Self.Selector);
   end Send_Message;

   -------------------
   -- Send_Telegram --
   -------------------

   procedure Send_Telegram
     (Self   : in out Bot'Class;
      Value  : Original_Message)
   is
      Object : League.JSON.Objects.JSON_Object;
      Ignore : League.JSON.Objects.JSON_Object;
      Text   : constant League.Strings.Universal_String :=
        "(" & Value.Sender.Name & ") " & Value.Text;
   begin
      Object.Insert
        (+"chat_id",
         League.JSON.Values.To_JSON_Value (+"@ada_ru_chat"));

      Object.Insert
        (+"text",
         League.JSON.Values.To_JSON_Value (Text));

      Telegram_Request (Self, "sendMessage", Object, Ignore);
   end Send_Telegram;

   ----------------
   -- Send_Viber --
   ----------------

   procedure Send_Viber
     (Self   : in out Bot'Class;
      Value  : Original_Message)
   is
      Object : League.JSON.Objects.JSON_Object;
      Sender : League.JSON.Objects.JSON_Object;
      Result : AWS.Response.Data;
      Header : AWS.Client.Header_List;
      Nick   : League.Strings.Universal_String;
   begin
      Header.Add ("Connection", "Keep-Alive");
      Header.Add ("X-Viber-Auth-Token", Self.Viber.Token.To_UTF_8_String);

      if Value.Sender.Name.Is_Empty then
         Nick := +"AdaRu";
      else
         Nick := Value.Sender.Name;
      end if;

      if not Value.Sender.Avatar.Is_Empty then
         Sender.Insert
           (+"avatar", League.JSON.Values.To_JSON_Value (Value.Sender.Avatar));
      end if;

      Sender.Insert (+"name", League.JSON.Values.To_JSON_Value (Nick));
      Object.Insert (+"sender", Sender.To_JSON_Value);

      case Value.Kind is
         when Axe.Bots.Text =>
            Object.Insert
              (+"type", League.JSON.Values.To_JSON_Value (+"text"));
         when Axe.Bots.Photo =>
            Object.Insert
              (+"type", League.JSON.Values.To_JSON_Value (+"picture"));
            Object.Insert
              (+"media", League.JSON.Values.To_JSON_Value (Value.URL));
      end case;

      Object.Insert (+"text", League.JSON.Values.To_JSON_Value (Value.Text));

      for K in 1 .. Self.Viber.Subscribed.Length loop
         Nick := Self.Viber.Subscribed (K);

         if Value.Origin = Viber_Origin and then Value.Sender.Id = Nick then
            --  Skip echoing to the sender itself
            null;
         elsif not Nick.Is_Empty then
            Object.Insert
              (+"receiver",
               League.JSON.Values.To_JSON_Value (Nick));

            for J in 1 .. 2 loop
               AWS.Client.Post
                 (Self.Viber.Connection,
                  Result,
                  Object.To_JSON_Document.To_JSON.To_Stream_Element_Array,
                  Content_Type => "application/json",
                  URI => "/pa/send_message",
                  Headers => Header);

               exit when
                 AWS.Response.Status_Code (Result) in AWS.Messages.Success;

               AWS.Client.Clear_SSL_Session (Self.Viber.Connection);
            end loop;
         end if;
      end loop;
   end Send_Viber;

   ---------------
   -- Send_XMPP --
   ---------------

   procedure Send_XMPP
     (Self   : in out Bot'Class;
      Value  : Original_Message)
   is
      procedure Send (Output : in out XMPP.Messages.XMPP_Message'Class);

      procedure Send (Output : in out XMPP.Messages.XMPP_Message'Class) is
         Jabber : constant League.Strings.Universal_String :=
           +"ada-ru@conference.jabber.ru";
         Text   : constant League.Strings.Universal_String :=
           "(" & Value.Sender.Name & ") " & Value.Text;
      begin
         Output.Set_To (Jabber);
         Output.Set_Type (XMPP.Group_Chat);
         Output.Set_Body (Text);
         Self.XMPP_Session.Send_Object (Output);
      end Send;
   begin
      case Value.Kind is
         when Axe.Bots.Text =>
            declare
               Text_Output : XMPP.Messages.XMPP_Message;
            begin
               Send (Text_Output);
            end;

         when Axe.Bots.Photo =>
            declare
               Photo : XMPP_Photos.XMPP_Photo :=
                 (XMPP.Messages.XMPP_Message with
                  URL     => Value.URL,
                  Caption => Value.Caption);
            begin
               Send (Photo);
            end;
      end case;
   end Send_XMPP;

   -------------------
   -- Session_State --
   -------------------

   overriding procedure Session_State
     (Self   : in out XMPP_Listener;
      Status : XMPP.Session_State)
   is
      Presence : XMPP.Presences.XMPP_Presence;
   begin
      if Status in XMPP.Established then
         --  After session successfully established, sending presence
         Self.XMPP_Session.Send_Object (Presence);

         Self.XMPP_Session.Join_Multi_User_Chat
           (Room      => +"ada-ru",
            Server    => +"conference.jabber.ru",
            Nick_Name => +"ada_ru");
      end if;
   end Session_State;

   --------------
   -- Telegram --
   --------------

   not overriding procedure Telegram
     (Self    : in out Bot;
      Message : League.JSON.Objects.JSON_Object;
      Result  : out League.JSON.Objects.JSON_Object)
   is
      pragma Unreferenced (Result);

      Max_Quote : constant := 20;

      procedure Fetch_Photo
        (List : League.JSON.Arrays.JSON_Array;
         URL  : out League.Strings.Universal_String);

      function Get_Nick (Object : League.JSON.Objects.JSON_Object)
        return League.Strings.Universal_String;

      procedure Get_Document;
      --  Extract and save document from the message if any

      -----------------
      -- Fetch_Photo --
      -----------------

      procedure Fetch_Photo
        (List : League.JSON.Arrays.JSON_Array;
         URL  : out League.Strings.Universal_String)
      is
         use type League.Holders.Universal_Integer;

         Result    : AWS.Response.Data;
         Object    : League.JSON.Objects.JSON_Object;
         Response  : League.JSON.Objects.JSON_Object;
         File_Path : League.Strings.Universal_String;
         Max_Width : League.Holders.Universal_Integer := 0;
      begin
         for J in 1 .. List.Length loop
            declare
               Item  : constant League.JSON.Objects.JSON_Object :=
                 List.Element (J).To_Object;
               Width : constant League.Holders.Universal_Integer :=
                 Item.Value (+"width").To_Integer;
            begin
               if Max_Width < Width then
                  Max_Width := Width;
                  Object.Insert (+"file_id", Item.Value (+"file_id"));
               end if;
            end;
         end loop;

         Telegram_Request (Self, "getFile", Object, Response);

         if Response.Contains (+"file_path") then
            File_Path := Response.Value (+"file_path").To_String;

            AWS.Client.Get
              (Self.Telegram.Connection,
               Result,
               URI => "/file/bot" & Self.Telegram.Token.To_UTF_8_String &
                 "/" & File_Path.To_UTF_8_String);

            if AWS.Response.Status_Code (Result) in AWS.Messages.Success then
               declare
                  Date   : constant League.Strings.Universal_String :=
                    League.Calendars.ISO_8601.Image
                     (+"yyyy-MM-dd-", League.Calendars.Clock);
                  Image  : Wide_Wide_String :=
                    Positive'Wide_Wide_Image (Self.File_Number);
                  Output : Ada.Streams.Stream_IO.File_Type;
               begin
                  Self.File_Number := Self.File_Number + 1;
                  Image (1) := 'x';
                  URL.Append ("/files/bot/");
                  URL.Append (Date);
                  URL.Append (Image);
                  URL.Append (".jpg");

                  Ada.Streams.Stream_IO.Create
                    (Output, Name => "install" & URL.To_UTF_8_String);

                  Ada.Streams.Stream_IO.Write
                    (Output,
                     AWS.Response.Message_Body (Result));

                  Ada.Streams.Stream_IO.Close (Output);
                  URL.Prepend ("https://www.ada-ru.org");
               end;
            end if;
         end if;
      end Fetch_Photo;

      ------------------
      -- Get_Document --
      ------------------

      procedure Get_Document is
         Doc       : League.JSON.Objects.JSON_Object;
         File_Id   : League.Strings.Universal_String;
         File_Name : League.Strings.Universal_String;
      begin
         if not Message.Value (+"document").Is_Object then
            return;
         end if;

         Doc := Message.Value (+"document").To_Object;
         File_Id := Doc.Value (+"file_id").To_String;
         File_Name := Doc.Value (+"file_name").To_String;

         declare
            Q : SQL.Queries.SQL_Query := Self.DB.Query
              (+"select count(*) from tg_books where id=:id");
         begin
            Q.Bind_Value (+":id", League.Holders.To_Holder (File_Id));
            Q.Execute;
            if not Q.Error_Message.Is_Empty then
               Ada.Wide_Wide_Text_IO.Put_Line
                 (Q.Error_Message.To_Wide_Wide_String);
            end if;

            if Q.Next then
               if League.Holders.Element
                 (Q.Value (1)).To_Wide_Wide_String /= "0"
               then
                  return;
               end if;
            end if;
         end;

         declare
            Q : SQL.Queries.SQL_Query := Self.DB.Query
              (+"insert into tg_books (id, file_name) values (:id, :name)");
         begin
            Q.Bind_Value (+":id", League.Holders.To_Holder (File_Id));
            Q.Bind_Value (+":name", League.Holders.To_Holder (File_Name));
            Q.Execute;

            if not Q.Error_Message.Is_Empty then
               Ada.Wide_Wide_Text_IO.Put_Line
                 (Q.Error_Message.To_Wide_Wide_String);
            end if;
         end;
      end Get_Document;

      --------------
      -- Get_Nick --
      --------------

      function Get_Nick (Object : League.JSON.Objects.JSON_Object)
        return League.Strings.Universal_String is
      begin
         if Object.Contains (+"username") then
            return Object.Value (+"username").To_String;
         else
            return Object.Value (+"first_name").To_String;
         end if;
      end Get_Nick;

      Chat : constant League.JSON.Objects.JSON_Object :=
        Message.Value (+"chat").To_Object;
      From : constant  League.JSON.Objects.JSON_Object :=
        Message.Value (+"from").To_Object;
      Sender : User;
      Text : League.Strings.Universal_String;
      Nick : League.Strings.Universal_String;
      Output : League.Strings.Universal_String;
      Reply : constant League.JSON.Objects.JSON_Object :=
        Message.Value (+"reply_to_message").To_Object;
      Forward : constant League.JSON.Objects.JSON_Object :=
        Message.Value (+"forward_from").To_Object;
   begin
      if Chat.Value (+"username").To_String /= +"ada_ru_chat" then
         Get_Document;
         return;
      end if;

      Sender :=
        (Name => Get_Nick (From), others => <>);

      if not Reply.Is_Empty then
         Nick := Get_Nick (Reply.Value (+"from").To_Object);
         Text := Reply.Value (+"text").To_String;

         if Text.Length > Max_Quote then
            Text := Text.Head (Max_Quote);
            Text.Append ("…");
         end if;

         if Nick = +"ada_ru_bot" then
            Output.Append (" отвечает на <");
         else
            Output.Append (" отвечает (");
            Output.Append (Nick);
            Output.Append (") на <");
         end if;
         Output.Append (Text);
         Output.Append (">");
         Output.Append (League.Characters.Latin.Line_Feed);
      elsif not Forward.Is_Empty then
         Output.Append (" цитирует (");
         Output.Append (Get_Nick (Forward));
         Output.Append (")");
         Output.Append (League.Characters.Latin.Line_Feed);
      end if;

      Text := Message.Value (+"text").To_String;

      if not Text.Is_Empty then
         Output.Append (Text);
      elsif Message.Contains (+"photo") then
         declare
            URL     : League.Strings.Universal_String;
            Caption : League.Strings.Universal_String;
         begin
            if Message.Contains (+"caption") then
               Caption := Message.Value (+"caption").To_String;
               Output.Append (Caption);
               Output.Append (" ");
            else
               Output.Append ("картинка ");
            end if;

            Fetch_Photo (Message.Value (+"photo").To_Array, URL);
            Output.Append (URL);

            Self.Send_Message
              ((Axe.Bots.Photo,
               Telegram_Origin,
               Sender,
               Output,
               URL,
               Caption));

            return;
         end;
      elsif Message.Contains (+"audio") then
         Output.Append ("<прислал аудио файл>");
      elsif Message.Contains (+"document") then
         Get_Document;
         Output.Append ("<прислал документ>");
      elsif Message.Contains (+"sticker") then
         declare
            Sticker : constant League.JSON.Objects.JSON_Object :=
              Message.Value (+"sticker").To_Object;
         begin
            Output.Append ("<прислал наклейку>");

            if Sticker.Contains (+"emoji") then
               Output.Append (" ");
               Output.Append (Sticker.Value (+"emoji").To_String);
            end if;
         end;
      elsif Message.Contains (+"video") then
         Output.Append ("<прислал видео>");
      elsif Message.Contains (+"voice") then
         Output.Append ("<прислал запись>");
      elsif Message.Contains (+"contact") then
         Output.Append ("<прислал контакт>");
      elsif Message.Contains (+"location") then
         Output.Append ("<прислал location>");
      elsif Message.Contains (+"venue") then
         Output.Append ("<прислал venue>");
      end if;

      Self.Send_Message ((Axe.Bots.Text, Telegram_Origin, Sender, Output));
   end Telegram;

   ----------------------
   -- Telegram_Request --
   ----------------------

   procedure Telegram_Request
     (Self     : in out Bot'Class;
      Method   : String;
      Object   : League.JSON.Objects.JSON_Object;
      Response : out League.JSON.Objects.JSON_Object)
   is
      Result : AWS.Response.Data;
      Header : AWS.Client.Header_List;
   begin
      Response := League.JSON.Objects.Empty_JSON_Object;
      Header.Add ("Connection", "Keep-Alive");

      for J in 1 .. 2 loop
         AWS.Client.Post
           (Self.Telegram.Connection,
            Result,
            Object.To_JSON_Document.To_JSON.To_Stream_Element_Array,
            Content_Type => "application/json",
            URI => "/bot" & Self.Telegram.Token.To_UTF_8_String &
              "/" & Method,
            Headers => Header);

         if AWS.Response.Status_Code (Result) in AWS.Messages.Success then
            declare
               Document : League.JSON.Documents.JSON_Document;
            begin
               Document := League.JSON.Documents.From_JSON
                 (AWS.Response.Message_Body (Result));

               Response := Document.To_JSON_Object.Value (+"result").To_Object;
               exit;
            end;
         end if;

         AWS.Client.Clear_SSL_Session (Self.Telegram.Connection);
      end loop;
   end Telegram_Request;

   -----------
   -- Viber --
   -----------

   not overriding procedure Viber
     (Self    : in out Bot;
      Message : League.JSON.Objects.JSON_Object;
      Result  : out League.JSON.Objects.JSON_Object)
   is
      Object : League.JSON.Objects.JSON_Object;
      Event  : constant League.Strings.Universal_String :=
        Message.Value (+"event").To_String;
      Text   : League.Strings.Universal_String;
      Sender : User;
   begin
      if Event = +"message" then
         Object := Message.Value (+"sender").To_Object;
         Sender :=
           (Name   => Object.Value (+"name").To_String,
            Id     => Object.Value (+"id").To_String,
            Avatar => Object.Value (+"avatar").To_String);

         Object := Message.Value (+"message").To_Object;

         if Object.Contains (+"text") then
            Text := Object.Value (+"text").To_String;
            if not Text.Is_Empty then
               Self.Send_Message ((Axe.Bots.Text, Viber_Origin, Sender, Text));
            end if;
         end if;

         if Self.Viber.Subscribed.Index (Sender.Id) = 0 then
            Self.Viber.Subscribed.Append (Sender.Id);
            Write_Subscribers (Self.Viber.Subscribed);
         end if;
      elsif Event = +"conversation_started" then
         Text.Append ("Welcome to ada_ru bot!");
         Text.Append (League.Characters.Latin.Line_Feed);
         Text.Append ("You can use this to chat about Ada Programming. ");
         Text.Append ("This chat is mostly in Russian however.");
         Text.Append (League.Characters.Latin.Line_Feed);
         Text.Append ("Приветствуем Вас в чате о языке Ада!");

         Result.Insert
           (+"type",
            League.JSON.Values.To_JSON_Value (+"text"));

         Result.Insert
           (+"text",
            League.JSON.Values.To_JSON_Value (Text));

      elsif Event = +"unsubscribed" then
         Self.Viber.Subscribed.Replace
           (Self.Viber.Subscribed.Index
              (Message.Value (+"user_id").To_String),
            League.Strings.Empty_Universal_String);

         Write_Subscribers (Self.Viber.Subscribed);

      elsif Event = +"failed" then
         Ada.Wide_Wide_Text_IO.Put_Line
           ("Viber failed: " &
              Message.Value (+"desc").To_String.To_Wide_Wide_String);
      end if;
   end Viber;

   -----------------------
   -- Write_Subscribers --
   -----------------------

   procedure Write_Subscribers
     (Value : League.String_Vectors.Universal_String_Vector)
   is
      File : Ada.Wide_Wide_Text_IO.File_Type;
   begin
      Ada.Wide_Wide_Text_IO.Open
        (File,
         Ada.Wide_Wide_Text_IO.Out_File,
         "password/viber_subscribers");

      for J in 1 .. Value.Length loop
         if not Value.Element (J).Is_Empty then
            Ada.Wide_Wide_Text_IO.Put_Line
              (File, Value.Element (J).To_Wide_Wide_String);
         end if;
      end loop;

      Ada.Wide_Wide_Text_IO.Close (File);
   end Write_Subscribers;

end Axe.Bots;
