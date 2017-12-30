with League.Strings;

with Servlet.HTTP_Requests;
with Servlet.HTTP_Responses;
with Servlet.HTTP_Servlets;

private with Servlet.Generic_Servlets;

package Servlet.Users is

   type User_Servlet is new Servlet.HTTP_Servlets.HTTP_Servlet with private;

private

   type User_Servlet is new Servlet.HTTP_Servlets.HTTP_Servlet
     with null record;

   overriding procedure Do_Get
    (Self     : in out User_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   overriding function Get_Servlet_Info
    (Self : User_Servlet) return League.Strings.Universal_String;

   overriding function Instantiate
    (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
         return User_Servlet;

end Servlet.Users;
