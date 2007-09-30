
with Ada.Text_IO;

with AWS.Log;
with AWS.Config;
with AWS.Server;
with AWS.Status;

with Callbacks;
with AWS.Dispatchers.Callback;
with AWS.Services.Dispatchers.URI;
with AWS.Services.Dispatchers.Method;

procedure Ada_Ru is

   use Ada;
   use AWS.Services;
   use AWS.Dispatchers;
   use AWS.Services.Dispatchers;

   WS   : AWS.Server.HTTP;
   U    : URI.Handler;
   M    : Method.Handler;
   C    : Callback.Handler := Callback.Create
     (Callbacks.Get_Wiki_Or_HTML'Access);

begin
   Text_IO.Put_Line ("AWS " & AWS.Version);

   URI.Register (U, "/edit_wiki/", Callbacks.Edit_Wiki'Access, True);
   URI.Register_Default_Callback (U, C);

   Method.Register (M, AWS.Status.POST, Callbacks.Put'Access);
   Method.Register_Default_Callback (M, U);

   AWS.Server.Start (WS, Dispatcher => M, Config => AWS.Config.Get_Current);

   loop
      delay 4 * 3600.0;
   end loop;

end Ada_Ru;
