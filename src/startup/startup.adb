------------------------------------------------------------------------------
--  Copyright © 2016, Maxim Reznik <reznikmm@gmail.com>
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

with League.Strings;
with League.String_Vectors;

with Servlet.Servlet_Registrations;
with Servlet.Servlets;

with AWFC.Static_Resource_Servlets;

with Axe.Wiki_View_Servlets;

package body Startup is

   type Servlet_Access is access all Servlet.Servlets.Servlet'Class;

   function "+"
    (Item : Wide_Wide_String) return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   ----------------
   -- On_Startup --
   ----------------

   overriding procedure On_Startup
     (Self    : in out Servlet_Container_Initializer;
      Context : in out Servlet.Contexts.Servlet_Context'Class)
   is
      pragma Unreferenced (Self);

      Registry : access
        Standard.Servlet.Servlet_Registrations.Servlet_Registration'Class;

      List : League.String_Vectors.Universal_String_Vector;
   begin
      Ada.Text_IO.Put_Line ("I'm here!");

      --  Create and register servlets.

      Registry := Context.Add_Servlet
        (+"StaticResources",
         Servlet_Access'(new AWFC.Static_Resource_Servlets
                                   .Static_Resource_Servlet));
      List.Append (+"/acats/*");
      List.Append (+"/arm83/*");
      List.Append (+"/asis_ugrg_RU/*");
      List.Append (+"/files/*");
      List.Append (+"/glade_UG_RU/*");
      List.Append (+"/graphics/*");
      List.Append (+"/GtkAda/*");
      List.Append (+"/i432/*");
      List.Append (+"/icons/*");
      List.Append (+"/Lovelace/*");
      List.Append (+"/V-0.4w/*");
      List.Append (+"*.exe");
      List.Append (+"*.wiki");
      List.Append (+"*.css");
      List.Append (+"*.xml");
      List.Append (+"*.png");
      List.Append (+"*.jpg");
      List.Append (+"*.txt");

      List := Registry.Add_Mapping (List);

      if not List.Is_Empty then
         raise Program_Error;
      end if;

      Registry := Context.Add_Servlet
        (+"WikiRendering",
         Servlet_Access'(new Axe.Wiki_View_Servlets.Wiki_View_Servlet));
      Registry.Add_Mapping (+"/");
      Registry.Add_Mapping (+"/edit_wiki/*");

      --  TODO: /arm/*
      --  TODO: set_password.html
   end On_Startup;

end Startup;
