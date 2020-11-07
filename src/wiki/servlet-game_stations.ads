with League.Strings;

with Servlet.HTTP_Requests;
with Servlet.HTTP_Responses;
with Servlet.HTTP_Servlets;

private with Servlet.Generic_Servlets;

package Servlet.Game_Stations is

   type Station_Servlet is new Servlet.HTTP_Servlets.HTTP_Servlet with private;

private

   type Station_Servlet is new Servlet.HTTP_Servlets.HTTP_Servlet
     with null record;

   overriding procedure Do_Get
    (Self     : in out Station_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   overriding function Get_Servlet_Info
    (Self : Station_Servlet) return League.Strings.Universal_String;

   overriding function Instantiate
    (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
         return Station_Servlet;

end Servlet.Game_Stations;
