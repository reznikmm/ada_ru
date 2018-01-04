with League.Strings;

package Axe.Events is

   type Listener is limited interface;

   not overriding procedure On_Wiki_Saved
     (Self    : in out Listener;
      URI     : League.Strings.Universal_String;
      Text    : League.Strings.Universal_String;
      User    : League.Strings.Universal_String;
      Created : Boolean) is null;

   not overriding procedure On_User_Created
     (Self    : in out Listener;
      Name    : League.Strings.Universal_String;
      Avatar  : League.Strings.Universal_String) is null;

   type Listener_Access is access all Listener'Class;

end Axe.Events;
