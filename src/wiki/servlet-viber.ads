with League.Strings;
with Axe.Events;

with Servlet.HTTP_Requests;
with Servlet.HTTP_Responses;
with Servlet.HTTP_Servlets;

private with Servlet.Generic_Servlets;

package Servlet.Viber is

   type Viber_Servlet is
     new Servlet.HTTP_Servlets.HTTP_Servlet with private;

   procedure Initialize
     (Self  : in out Viber_Servlet'Class;
      Token : League.Strings.Universal_String);

   not overriding procedure Set_Listener
    (Self  : in out Viber_Servlet;
     Value : access Axe.Events.Listener'Class);

   type Viber_Servlet_Access is access all Viber_Servlet'Class;

private

   type Viber_Servlet is new Servlet.HTTP_Servlets.HTTP_Servlet with record
      Token    : League.Strings.Universal_String;
      Listener : access Axe.Events.Listener'Class;
   end record;

   overriding procedure Do_Post
    (Self     : in out Viber_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   overriding function Get_Servlet_Info
    (Self : Viber_Servlet) return League.Strings.Universal_String;

   overriding function Instantiate
    (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
         return Viber_Servlet;

end Servlet.Viber;
