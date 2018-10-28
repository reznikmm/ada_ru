with League.Strings;

with Servlet.HTTP_Requests;
with Servlet.HTTP_Responses;
with Servlet.HTTP_Servlets;

private with Servlet.Generic_Servlets;

package Servlet.Compile is

   type Compile_Servlet is
     new Servlet.HTTP_Servlets.HTTP_Servlet with private;

   type Compile_Servlet_Access is access all Compile_Servlet'Class;

private

   type Compile_Servlet is new Servlet.HTTP_Servlets.HTTP_Servlet with record
      null;
   end record;

   overriding procedure Do_Get
    (Self     : in out Compile_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   overriding procedure Do_Post
    (Self     : in out Compile_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   overriding function Get_Servlet_Info
    (Self : Compile_Servlet) return League.Strings.Universal_String;

   overriding function Instantiate
    (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
         return Compile_Servlet;

end Servlet.Compile;
