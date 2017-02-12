------------------------------------------------------------------------------
--  Copyright © 2016-2017, Maxim Reznik <reznikmm@gmail.com>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--     * this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--     * notice, this list of conditions and the following disclaimer in the
--     * documentation and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
------------------------------------------------------------------------------
--  $Revision: 31 $ $Date: 2016-03-17 14:04:24 +0200 (Чт, 17 мар 2016) $
------------------------------------------------------------------------------

with Ada.Text_IO;

with AWFC.Static_Resource_Servlets;
pragma Unreferenced (AWFC.Static_Resource_Servlets);

with Axe.Wiki_View_Servlets;
pragma Unreferenced (Axe.Wiki_View_Servlets);

with Axe.Events.Logs;
with League.Strings;

package body Startup is

   ----------------
   -- On_Startup --
   ----------------

   overriding procedure On_Startup
     (Self    : in out Servlet_Container_Initializer;
      Context : in out Servlet.Contexts.Servlet_Context'Class)
   is
      pragma Unreferenced (Self);
      File : constant League.Strings.Universal_String :=
        League.Strings.To_Universal_String ("/news.wiki");
      Log  : constant Axe.Events.Logs.Event_Log_Writer_Access :=
        new Axe.Events.Logs.Event_Log_Writer;
   begin
      Log.Initialize (Context.Get_Real_Path (File));
      Axe.Events.Top_Listener := Axe.Events.Listener_Access (Log);
      Ada.Text_IO.Put_Line ("I'm here!");
      --  TODO: /arm/*
      --  TODO: set_password.html
   end On_Startup;

end Startup;
