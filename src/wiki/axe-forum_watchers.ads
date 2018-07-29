with GNAT.Sockets;
with League.Strings;
with Interfaces.C;

package Axe.Forum_Watchers is

   type Forum_Watcher is tagged limited private;

   procedure Initialize
     (Self : in out Forum_Watcher'Class;
      Name : League.Strings.Universal_String;
      File : out GNAT.Sockets.Socket_Type);

   not overriding procedure Refresh
     (Self : in out Forum_Watcher;
      Text : out League.Strings.Universal_String);

private
   type Forum_Watcher is tagged limited record
      I_Notify_Queue   : Interfaces.C.int;
      Watch_Descriptor : Interfaces.C.int;
   end record;

end Axe.Forum_Watchers;
