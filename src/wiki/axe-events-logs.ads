package Axe.Events.Logs is

   type Event_Log_Writer is new Axe.Events.Listener with private;
   type Event_Log_Writer_Access is access all Event_Log_Writer'Class;

   procedure Initialize
     (Self : in out Event_Log_Writer'Class;
      File : League.Strings.Universal_String);

private

   type Event_Log_Writer is new Axe.Events.Listener with record
      File : League.Strings.Universal_String;
   end record;

   overriding procedure On_Wiki_Saved
     (Self    : in out Event_Log_Writer;
      URI     : League.Strings.Universal_String;
      Text    : League.Strings.Universal_String;
      User    : League.Strings.Universal_String;
      Created : Boolean);

end Axe.Events.Logs;
