with League.Strings;
private with IRC.Listeners;
private with IRC.Sessions;
private with Ada.Containers.Bounded_Synchronized_Queues;
private with Ada.Containers.Synchronized_Queue_Interfaces;
private with GNAT.Sockets;
private with XMPP.Sessions;
private with XMPP.Stream_Handlers;

package Axe.Bots is

   type Bot is tagged limited private;

   procedure Initialize
     (Self     : in out Bot;
      Password : League.Strings.Universal_String);

   not overriding procedure Send_Message
     (Self : in out Bot;
      Text : League.Strings.Universal_String);

private

   task type Bot_Loop (Bot : access Axe.Bots.Bot) is
      entry Start;
   end Bot_Loop;

   type IRC_Listener is limited new IRC.Listeners.Listener with record
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

   type XMPP_Listener is limited new XMPP.Stream_Handlers.XMPP_Stream_Handler
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

   package Message_Queue_Interfaces is
     new Ada.Containers.Synchronized_Queue_Interfaces
       (Element_Type => League.Strings.Universal_String);

   package Message_Queues is new Ada.Containers.Bounded_Synchronized_Queues
     (Queue_Interfaces => Message_Queue_Interfaces,
      Default_Capacity => 5);

   subtype Message_Queue is Message_Queues.Queue;

   type Bot is tagged limited record
      Network_Loop  : Bot_Loop (Bot'Unchecked_Access);
      Queue         : Message_Queue;
      IRC_Listener  : aliased Axe.Bots.IRC_Listener;
      IRC_Session   : access IRC.Sessions.Session;
      Selector      : GNAT.Sockets.Selector_Type;
      XMPP_Session  : aliased XMPP.Sessions.XMPP_Session;
      XMPP_Listener : aliased Axe.Bots.XMPP_Listener;
   end record;

end Axe.Bots;
