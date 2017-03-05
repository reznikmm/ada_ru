with Ada.Exceptions;
with Ada.Wide_Wide_Text_IO;

with XMPP.Messages;
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
      Target       : constant League.Strings.Universal_String := +"#ada";
      Jabber       : constant League.Strings.Universal_String :=
        +"ada-ru@conference.jabber.ru";
      Next_Message : League.Strings.Universal_String;

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
            Bot.IRC_Session.Send_Message (Target, Next_Message);

            declare
               Message : XMPP.Messages.XMPP_Message;
            begin
               Message.Set_To (Jabber);
               Message.Set_Type (XMPP.Group_Chat);
               Message.Set_Body (Next_Message);
               Bot.XMPP_Session.Send_Object (Message);
            end;
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
      Password : League.Strings.Universal_String) is
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
   end Initialize;

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
      pragma Unreferenced (Self);
   begin
      Ada.Wide_Wide_Text_IO.Put (Source.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Put (" : ");
      Ada.Wide_Wide_Text_IO.Put_Line (Text.To_Wide_Wide_String);

      if not Target.Starts_With ("#") then
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
     (Self : in out Bot;
      Text : League.Strings.Universal_String) is
   begin
      Self.Queue.Enqueue (Text);
      GNAT.Sockets.Abort_Selector (Self.Selector);
   end Send_Message;

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

end Axe.Bots;
