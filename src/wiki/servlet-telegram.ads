with League.Strings;
with Axe.Events;

with Servlet.HTTP_Requests;
with Servlet.HTTP_Responses;
with Servlet.HTTP_Servlets;

private with Servlet.Generic_Servlets;

package Servlet.Telegram is

   type Telegram_Servlet is
     new Servlet.HTTP_Servlets.HTTP_Servlet with private;

   not overriding procedure Set_Listener
    (Self  : in out Telegram_Servlet;
     Value : access Axe.Events.Listener'Class);

   type Telegram_Servlet_Access is access all Telegram_Servlet'Class;

private

   type Telegram_Servlet is new Servlet.HTTP_Servlets.HTTP_Servlet with record
      Token    : League.Strings.Universal_String;
      Listener : access Axe.Events.Listener'Class;
   end record;

   overriding procedure Do_Post
    (Self     : in out Telegram_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   overriding function Get_Servlet_Info
    (Self : Telegram_Servlet) return League.Strings.Universal_String;

   overriding function Instantiate
    (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
         return Telegram_Servlet;

end Servlet.Telegram;
