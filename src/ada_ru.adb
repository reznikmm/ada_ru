
with Ada.Text_IO;

with AWS.Log;
with AWS.Server.Log;

with Callbacks;

procedure Ada_Ru is

   use Ada;

   WS   : AWS.Server.HTTP;

begin
   Text_IO.Put_Line ("AWS " & AWS.Version);

   AWS.Server.Start (WS, "Ada_Ru Public",
                     Max_Connection => 5,
                     Port           => 80,
                     Callback       => Callbacks.Public_Service'Access);

   --  Wait for 'q' key pressed...

   loop
      delay 4 * 3600.0;
   end loop;

end Ada_Ru;
