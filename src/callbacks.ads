with AWS.Status;
with AWS.Response;

package Callbacks is

   function Public_Service (Request : in AWS.Status.Data)
     return AWS.Response.Data;

end Callbacks;
