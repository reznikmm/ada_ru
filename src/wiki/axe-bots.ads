with League.JSON.Objects;
with League.Strings;

private with AWS.Client;
private with IRC.Listeners;
private with IRC.Sessions;
private with Ada.Containers.Bounded_Synchronized_Queues;
private with Ada.Containers.Synchronized_Queue_Interfaces;
private with GNAT.Sockets;
private with XMPP.Messages;
private with XMPP.Sessions;
private with XMPP.Stream_Handlers;

package Axe.Bots is

   type Bot is tagged limited private;

   procedure Initialize
     (Self     : in out Bot;
      Password : League.Strings.Universal_String;
      Telegram : League.Strings.Universal_String;
      Viber    : League.Strings.Universal_String);

   type Origin_Kind is
     (IRC_Origin, XMPP_Origin, Telegram_Origin, Viber_Origin, Other_Origin);

   not overriding procedure Send_Message
     (Self   : in out Bot;
      Text   : League.Strings.Universal_String;
      Origin : Origin_Kind := Other_Origin);

   not overriding procedure Telegram
     (Self    : in out Bot;
      Message : League.JSON.Objects.JSON_Object;
      Result  : out League.JSON.Objects.JSON_Object);

   not overriding procedure Viber
     (Self    : in out Bot;
      Message : League.JSON.Objects.JSON_Object;
      Result  : out League.JSON.Objects.JSON_Object);

private

   task type Bot_Loop (Bot : access Axe.Bots.Bot) is
      entry Start;
   end Bot_Loop;

   type IRC_Listener (Bot : access Axe.Bots.Bot) is limited new
     IRC.Listeners.Listener with
   record
      Identified : Boolean := False;
      Password   : League.Strings.Universal_String;
   end record;

   overriding procedure On_Message
     (Self    : access IRC_Listener;
      Session : access IRC.Sessions.Session'Class;
      Target  : League.Strings.Universal_String;
      Source  : League.Strings.Universal_String;
      Text    : League.Strings.Universal_String);

   overriding procedure On_Notice
     (Self    : access IRC_Listener;
      Session : access IRC.Sessions.Session'Class;
      Target  : League.Strings.Universal_String;
      Source  : League.Strings.Universal_String;
      Text    : League.Strings.Universal_String);

   overriding procedure On_Ping
     (Self    : access IRC_Listener;
      Session : access IRC.Sessions.Session'Class;
      Source  : League.Strings.Universal_String);

   type XMPP_Listener (Bot : access Axe.Bots.Bot) is limited new
     XMPP.Stream_Handlers.XMPP_Stream_Handler
   with record
      XMPP_Session : access XMPP.Sessions.XMPP_Session;
   end record;

   overriding procedure Bind_Resource_State
     (Self   : in out XMPP_Listener;
      JID    : League.Strings.Universal_String;
      Status : XMPP.Bind_State);

   overriding procedure Session_State
     (Self   : in out XMPP_Listener;
      Status : XMPP.Session_State);

   overriding procedure Message
     (Self : in out XMPP_Listener;
      Msg  : XMPP.Messages.XMPP_Message'Class);

   type Original_Message is record
      Text   : League.Strings.Universal_String;
      Origin : Origin_Kind;
   end record;

   package Message_Queue_Interfaces is
     new Ada.Containers.Synchronized_Queue_Interfaces
       (Element_Type => Original_Message);

   package Message_Queues is new Ada.Containers.Bounded_Synchronized_Queues
     (Queue_Interfaces => Message_Queue_Interfaces,
      Default_Capacity => 5);

   subtype Message_Queue is Message_Queues.Queue;

   type Telegram_Information is record
      Connection : AWS.Client.HTTP_Connection;
      Token      : League.Strings.Universal_String;
   end record;

   type Viber_Information is record
      Connection : AWS.Client.HTTP_Connection;
      Token      : League.Strings.Universal_String;
   end record;

   type Bot is tagged limited record
      Network_Loop  : Bot_Loop (Bot'Unchecked_Access);
      Queue         : Message_Queue;
      IRC_Listener  : aliased Axe.Bots.IRC_Listener (Bot'Unchecked_Access);
      IRC_Session   : access IRC.Sessions.Session;
      Selector      : GNAT.Sockets.Selector_Type;
      XMPP_Session  : aliased XMPP.Sessions.XMPP_Session;
      XMPP_Listener : aliased Axe.Bots.XMPP_Listener (Bot'Unchecked_Access);
      Telegram      : Telegram_Information;
      Viber         : Viber_Information;
   end record;

end Axe.Bots;
