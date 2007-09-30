with AWS.Status;
with AWS.Response;

package Callbacks is

   function Private_Service (Request : in AWS.Status.Data)
     return AWS.Response.Data;

   function Put (Request : in AWS.Status.Data) return AWS.Response.Data;

   function Get_Wiki (Request : in AWS.Status.Data) return AWS.Response.Data;
   function Edit_Wiki (Request : in AWS.Status.Data) return AWS.Response.Data;

   function Get_Wiki_Or_HTML
     (Request : in AWS.Status.Data)
     return AWS.Response.Data;

end Callbacks;
