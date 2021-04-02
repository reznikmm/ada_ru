with League.Strings;

with Servlet.HTTP_Requests;
with Servlet.HTTP_Responses;
with Servlet.HTTP_Servlets;

private with Servlet.Generic_Servlets;

package Servlet.Forum is

   type Forum_Servlet is
     new Servlet.HTTP_Servlets.HTTP_Servlet with private;

   type Forum_Servlet_Access is access all Forum_Servlet'Class;

private

   type Forum_Servlet is new Servlet.HTTP_Servlets.HTTP_Servlet with record
      Secret : League.Strings.Universal_String;
      BCC    : League.Strings.Universal_String;
   end record;

   overriding procedure Do_Post
    (Self     : in out Forum_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   overriding function Get_Servlet_Info
    (Self : Forum_Servlet) return League.Strings.Universal_String;

   overriding function Instantiate
    (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
         return Forum_Servlet;

end Servlet.Forum;
