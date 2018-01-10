private with Axe.Bots;

package Axe.Events.Logs is

   type Event_Log_Writer is limited new Axe.Events.Listener with private;
   type Event_Log_Writer_Access is access all Event_Log_Writer'Class;

   procedure Initialize
     (Self     : in out Event_Log_Writer'Class;
      File     : League.Strings.Universal_String;
      Password : League.Strings.Universal_String);

private

   type Event_Log_Writer is limited new Axe.Events.Listener with record
      File : League.Strings.Universal_String;
      Bot  : Axe.Bots.Bot;
   end record;

   overriding procedure On_Wiki_Saved
     (Self    : in out Event_Log_Writer;
      URI     : League.Strings.Universal_String;
      Text    : League.Strings.Universal_String;
      User    : League.Strings.Universal_String;
      Created : Boolean);

   overriding procedure On_User_Created
     (Self    : in out Event_Log_Writer;
      Name    : League.Strings.Universal_String;
      Avatar  : League.Strings.Universal_String);

   overriding procedure On_Telegram
     (Self    : in out Event_Log_Writer;
      Message : League.JSON.Objects.JSON_Object;
      Result  : out League.JSON.Objects.JSON_Object);

end Axe.Events.Logs;
