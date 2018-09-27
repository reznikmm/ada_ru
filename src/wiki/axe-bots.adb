with Ada.Calendar;
with Ada.Exceptions;
with Ada.Wide_Wide_Text_IO;

with AWS.Headers;
with AWS.Messages;
with AWS.Response;

with League.Characters.Latin;
with League.JSON.Documents;
with League.JSON.Values;

with XMPP.Presences;

package body Axe.Bots is

   use type League.Strings.Universal_String;

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

   procedure Send_IRC
     (Self   : in out Bot'Class;
      Value  : Original_Message);

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

   Send : constant array (IRC_Origin .. Viber_Origin) of access
     procedure (Self  : in out Bot'Class;
                Value : Original_Message) :=
     (IRC_Origin      => Send_IRC'Access,
      XMPP_Origin     => Send_XMPP'Access,
      Telegram_Origin => Send_Telegram'Access,
      Viber_Origin    => Send_Viber'Access);

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

      Time          : Ada.Calendar.Time;  --  Start of cycle
      Next_Message  : Original_Message;
      IRC_Reconnect : Ada.Calendar.Time := Ada.Calendar.Clock;
      --  Scheduled time to reconnect IRC. If IRC connected then Never
      Never         : constant Ada.Calendar.Time :=
        Ada.Calendar.Time_Of
          (Ada.Calendar.Year_Number'Last,
           Ada.Calendar.Month_Number'Last,
           Ada.Calendar.Day_Number'Last);

      Socket   : GNAT.Sockets.Socket_Type;  --  IRC socket
      Status   : GNAT.Sockets.Selector_Status;
   begin
      accept Start;

      Bot.XMPP_Session.Open;

      GNAT.Sockets.Create_Selector (Bot.Selector);

      loop
         Time := Ada.Calendar.Clock;

         if IRC_Reconnect <= Time then  --  Try to connect IRC
            Bot.IRC_Session.Connect
              (Socket    => Socket,
               Host      => +"irc.odessa.ua",
               Port      => 7777,
               Nick      => +"ada-ru",
               Password  => +"",
               User      => +"ada_ru",
               Real_Name => +"Ada Ru Bot");

            if Socket in GNAT.Sockets.No_Socket then
               IRC_Reconnect := Time + 60.0;
            else
               IRC_Reconnect := Never;
               Ada.Wide_Wide_Text_IO.Put_Line ("Connected to IRC");
            end if;
         end if;

         declare  --  Read IRC if connected
            Read     : GNAT.Sockets.Socket_Set_Type;
            Write    : GNAT.Sockets.Socket_Set_Type;
            Error    : GNAT.Sockets.Socket_Set_Type;
         begin
            if IRC_Reconnect = Never then
               GNAT.Sockets.Set (Read, Socket);
               GNAT.Sockets.Set (Error, Socket);
            end if;

            GNAT.Sockets.Check_Selector
              (Selector     => Bot.Selector,
               R_Socket_Set => Read,
               W_Socket_Set => Write,
               E_Socket_Set => Error,
               Status       => Status,
               Timeout      => 60.0);

            if Status in GNAT.Sockets.Completed then
               if not GNAT.Sockets.Is_Empty (Read) then
                  declare
                     IRC_Closed : Boolean;
                  begin
                     Bot.IRC_Session.Check_Socket (IRC_Closed);

                     if IRC_Closed then
                        Ada.Wide_Wide_Text_IO.Put_Line
                          ("Disconnected from IRC");
                        IRC_Reconnect := Time + 1.0;
                     end if;
                  end;
               end if;
            end if;
         end;

         loop
            select
               Bot.Queue.Dequeue (Next_Message);

               for Origin in Send'Range loop
                  if Next_Message.Origin /= Origin
                    or else Next_Message.Origin = Viber_Origin
                  then
                     Send (Origin) (Bot.all, Next_Message);
                  end if;
               end loop;

               delay until Time + 1.0;  --  Avoid to Excess Flood errors
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : in out Bot;
      Password : League.Strings.Universal_String;
      Telegram : League.Strings.Universal_String;
      Viber    : League.Strings.Universal_String)
   is
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
      Self.Viber.Token := Viber;
      Read_Subscribers (Self.Viber.Subscribed);
   end Initialize;

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
           ((XMPP_Origin,
            (Name => From, others => <>),
            Text));
      end if;
   end Message;

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
      From : constant League.Strings.Universal_String :=
        Source.Head_To (Source.Index ("!") - 1);
   begin
      Ada.Wide_Wide_Text_IO.Put (Source.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Put (" : ");
      Ada.Wide_Wide_Text_IO.Put_Line (Text.To_Wide_Wide_String);

      if Target.Starts_With ("#") then
         if From /= +"ada_ru" then
            Self.Bot.Send_Message
              ((IRC_Origin,
               (Name => From, others => <>),
               Text));
         end if;
      else
         Session.Send_Message (Source, Text);
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

   --------------
   -- Send_IRC --
   --------------

   procedure Send_IRC
     (Self   : in out Bot'Class;
      Value  : Original_Message)
   is
      Target : constant League.Strings.Universal_String := +"#ada";
      Text   : constant League.Strings.Universal_String :=
        "(" & Value.Sender.Name & ") " & Value.Text;
   begin
      Self.IRC_Session.Send_Message (Target, Text);
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
          Origin => Other_Origin));
   end Send_Message;

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
      Result : AWS.Response.Data;
      Header : AWS.Client.Header_List;
      Text   : constant League.Strings.Universal_String :=
        "(" & Value.Sender.Name & ") " & Value.Text;
   begin
      Header.Add ("Connection", "Keep-Alive");

      Object.Insert
        (+"chat_id",
         League.JSON.Values.To_JSON_Value (+"@adalang"));

      Object.Insert
        (+"text",
         League.JSON.Values.To_JSON_Value (Text));

      for J in 1 .. 2 loop
         AWS.Client.Post
           (Self.Telegram.Connection,
            Result,
            Object.To_JSON_Document.To_JSON.To_Stream_Element_Array,
            Content_Type => "application/json",
            URI => "/bot" & Self.Telegram.Token.To_UTF_8_String &
              "/sendMessage",
            Headers => Header);

         exit when AWS.Response.Status_Code (Result) in AWS.Messages.Success;

         AWS.Client.Clear_SSL_Session (Self.Telegram.Connection);
      end loop;
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

      Object.Insert (+"type", League.JSON.Values.To_JSON_Value (+"text"));
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
      Output : XMPP.Messages.XMPP_Message;
      Jabber : constant League.Strings.Universal_String :=
        +"ada-ru@conference.jabber.ru";
      Text   : constant League.Strings.Universal_String :=
        "(" & Value.Sender.Name & ") " & Value.Text;
   begin
      Output.Set_To (Jabber);
      Output.Set_Type (XMPP.Group_Chat);
      Output.Set_Body (Text);
      Self.XMPP_Session.Send_Object (Output);
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

      function Get_Nick (Object : League.JSON.Objects.JSON_Object)
        return League.Strings.Universal_String;

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
      if Chat.Value (+"username").To_String /= +"adalang" then
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
         Output.Append ("<прислал фото>");
      elsif Message.Contains (+"audio") then
         Output.Append ("<прислал аудио файл>");
      elsif Message.Contains (+"document") then
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

      Self.Send_Message ((Telegram_Origin, Sender, Output));
   end Telegram;

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
               Self.Send_Message ((Viber_Origin, Sender, Text));
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
