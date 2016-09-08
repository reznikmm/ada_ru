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
--  $Date:$
------------------------------------------------------------------------------
--  This servlet can be used to return rendered wiki pages
------------------------------------------------------------------------------

private with League.Calendars;
private with League.Strings;

private with Servlet.Generic_Servlets;
private with Servlet.HTTP_Requests;
private with Servlet.HTTP_Responses;
with Servlet.HTTP_Servlets;

package Axe.Wiki_View_Servlets is

   type Wiki_View_Servlet is
     new Servlet.HTTP_Servlets.HTTP_Servlet with private;

   type Wiki_View_Servlet_Access is
     access all Wiki_View_Servlet'Class;

private

   type Wiki_View_Servlet is
     new Servlet.HTTP_Servlets.HTTP_Servlet with null record;

   overriding procedure Do_Get
    (Self     : in out Wiki_View_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   overriding function Get_Last_Modified
    (Self     : in out Wiki_View_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class)
      return League.Calendars.Date_Time;

   overriding function Get_Servlet_Info
    (Self : Wiki_View_Servlet) return League.Strings.Universal_String;

   overriding function Instantiate
    (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
         return Wiki_View_Servlet;

end Axe.Wiki_View_Servlets;
