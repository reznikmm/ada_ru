with Ada.Calendar;
with Ada.Exceptions;
with Ada.Wide_Wide_Text_IO;

with AWS.Response;

with League.JSON.Documents;
with League.JSON.Values;
with XMPP.Presences;

package body Axe.Bots is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

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

      Time         : Ada.Calendar.Time;
      Target       : constant League.Strings.Universal_String := +"#ada";
      Jabber       : constant League.Strings.Universal_String :=
        +"ada-ru@conference.jabber.ru";
      Next_Message : Original_Message;

      Socket   : GNAT.Sockets.Socket_Type;
      Read     : GNAT.Sockets.Socket_Set_Type;
      Write    : GNAT.Sockets.Socket_Set_Type;
      Error    : GNAT.Sockets.Socket_Set_Type;
      Status   : GNAT.Sockets.Selector_Status;
   begin
      accept Start;

      Bot.XMPP_Session.Open;

      Bot.IRC_Session.Connect
        (Socket    => Socket,
         Host      => +"irc.odessa.ua",
         Port      => 7777,
         Nick      => +"ada-ru",
         Password  => +"",
         User      => +"ada_ru",
         Real_Name => +"Ada Ru Bot");

      GNAT.Sockets.Create_Selector (Bot.Selector);

      loop
         GNAT.Sockets.Set (Read, Socket);
         GNAT.Sockets.Set (Error, Socket);

         GNAT.Sockets.Check_Selector
           (Selector     => Bot.Selector,
            R_Socket_Set => Read,
            W_Socket_Set => Write,
            E_Socket_Set => Error,
            Status       => Status,
            Timeout      => 60.0);

         if Status in GNAT.Sockets.Completed then
            if not GNAT.Sockets.Is_Empty (Read) then
               Bot.IRC_Session.Check_Socket (Socket);
            end if;
         end if;

         select
            Bot.Queue.Dequeue (Next_Message);
            Time := Ada.Calendar.Clock;

            if Next_Message.Origin /= IRC_Origin then
               Bot.IRC_Session.Send_Message (Target, Next_Message.Text);
            end if;

            if Next_Message.Origin /= XMPP_Origin then
               declare
                  Output : XMPP.Messages.XMPP_Message;
               begin
                  Output.Set_To (Jabber);
                  Output.Set_Type (XMPP.Group_Chat);
                  Output.Set_Body (Next_Message.Text);
                  Bot.XMPP_Session.Send_Object (Output);
               end;
            end if;

            if Next_Message.Origin /= Telegram_Origin then
               Bot.Send_Telegram (Next_Message.Text);
            end if;

            delay until Time + 1.0;  --  Avoid to Excess Flood errors
         else
            null;
         end select;
      end loop;
   exception
      when E : others =>
         Ada.Wide_Wide_Text_IO.Put_Line
           ("Bot_Loop dead: " &
              Ada.Exceptions.Wide_Wide_Exception_Name (E));
         Ada.Wide_Wide_Text_IO.Put_Line
           (League.Strings.From_UTF_8_String
              (Ada.Exceptions.Exception_Message (E)).To_Wide_Wide_String);
   end Bot_Loop;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : in out Bot;
      Password : League.Strings.Universal_String;
      Token    : League.Strings.Universal_String) is
   begin
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

      AWS.Client.Create (Self.Telegram, Host => "https://api.telegram.org");
      Self.Token := Token;
   end Initialize;

   -------------
   -- Message --
   -------------

   overriding procedure Message
     (Self : in out XMPP_Listener;
      Msg  : XMPP.Messages.XMPP_Message'Class)
   is
      use type League.Strings.Universal_String;
      From : League.Strings.Universal_String := Msg.Get_From;
   begin
      Ada.Wide_Wide_Text_IO.Put_Line (Msg.Get_From.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Put_Line (Msg.Get_To.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Put_Line (Msg.Get_Body.To_Wide_Wide_String);

      if not From.Ends_With ("/ada_ru") then
         From := From.Tail_From (From.Last_Index ('/') + 1);
         Self.Bot.Send_Message ("(" & From & ")" & Msg.Get_Body, XMPP_Origin);
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
      use type League.Strings.Universal_String;
      From : constant League.Strings.Universal_String :=
        Source.Head_To (Source.Index ("!") - 1);
   begin
      Ada.Wide_Wide_Text_IO.Put (Source.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Put (" : ");
      Ada.Wide_Wide_Text_IO.Put_Line (Text.To_Wide_Wide_String);

      if Target.Starts_With ("#") then
         if From /= +"ada_ru" then
            Self.Bot.Send_Message ("(" & From & "): " & Text, IRC_Origin);
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
      use type League.Strings.Universal_String;
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

   ------------------
   -- Send_Message --
   ------------------

   not overriding procedure Send_Message
     (Self   : in out Bot;
      Text   : League.Strings.Universal_String;
      Origin : Origin_Kind := Other_Origin) is
   begin
      Self.Queue.Enqueue ((Text, Origin));
      GNAT.Sockets.Abort_Selector (Self.Selector);
   end Send_Message;

   -------------------
   -- Send_Telegram --
   -------------------

   not overriding procedure Send_Telegram
     (Self   : in out Bot;
      Text   : League.Strings.Universal_String)
   is
      Object : League.JSON.Objects.JSON_Object;
      Result : AWS.Response.Data;
   begin
      Object.Insert
        (+"chat_id",
         League.JSON.Values.To_JSON_Value (+"@adalang"));

      Object.Insert
        (+"text",
         League.JSON.Values.To_JSON_Value (Text));

      AWS.Client.Post
        (Self.Telegram,
         Result,
         Object.To_JSON_Document.To_JSON.To_Stream_Element_Array,
         Content_Type => "application/json",
         URI => "/bot" & Self.Token.To_UTF_8_String & "/sendMessage");
   end Send_Telegram;

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

      use type League.Strings.Universal_String;
      Chat : constant League.JSON.Objects.JSON_Object :=
        Message.Value (+"chat").To_Object;
      From : constant League.JSON.Values.JSON_Value := Message.Value (+"from");
      Text : League.Strings.Universal_String;
   begin
      if not Message.Contains (+"text") or not Chat.Contains (+"username") then
         return;
      elsif Chat.Value (+"username").To_String /= +"adalang" then
         return;
      end if;

      Text := Message.Value (+"text").To_String;

      if From.Is_Object then
         declare
            Object : constant League.JSON.Objects.JSON_Object :=
              From.To_Object;
         begin
            if Object.Contains (+"username") then
               Text.Prepend
                 ("(" & Object.Value (+"username").To_String & ") ");
            else
               Text.Prepend
                 ("(" & Object.Value (+"first_name").To_String & ") ");
            end if;
         end;
      end if;

      Self.Send_Message (Text, Telegram_Origin);
   end Telegram;

end Axe.Bots;
