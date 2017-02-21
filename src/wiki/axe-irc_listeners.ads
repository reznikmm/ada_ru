with IRC.Listeners;

package Axe.IRC_Listeners is

   type Listener is new IRC.Listeners.Listener with private;

private
   type Listener is new IRC.Listeners.Listener with null record;

end Axe.IRC_Listeners;
