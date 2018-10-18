with League.Strings;

with Servlet.HTTP_Requests;
with Servlet.HTTP_Responses;
with Servlet.HTTP_Servlets;

private with Servlet.Generic_Servlets;

package Servlet.Pastebin is

   type Pastebin_Servlet is
     new Servlet.HTTP_Servlets.HTTP_Servlet with private;
   --  Servlet processes request
   --  POST in form:
   --  * json: {text: ["lines"]} - reply is {id: "text-hash"}
   --  * form: ?text=lines       - redirect to /editor.html?id=hash
   --  GET in form ?id=hash - reply is json: {text: ["lines"]}

   type Pastebin_Servlet_Access is access all Pastebin_Servlet'Class;

private

   type Pastebin_Servlet is new Servlet.HTTP_Servlets.HTTP_Servlet with record
      null;
   end record;

   overriding procedure Do_Get
    (Self     : in out Pastebin_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   overriding procedure Do_Post
    (Self     : in out Pastebin_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   overriding function Get_Servlet_Info
    (Self : Pastebin_Servlet) return League.Strings.Universal_String;

   overriding function Instantiate
    (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
         return Pastebin_Servlet;

end Servlet.Pastebin;
