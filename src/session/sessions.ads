
with League.Strings;

with Servlet.HTTP_Sessions;

private with Servlet.OAuth;

package Sessions is

   type HTTP_Session is new Servlet.HTTP_Sessions.HTTP_Session with private;

   type HTTP_Session_Access is access all HTTP_Session'Class;

private

   type HTTP_Session is new Servlet.HTTP_Sessions.HTTP_Session with record
      Id   : League.Strings.Universal_String;
      Info : Servlet.OAuth.User_Info;
   end record;

   overriding function Get_Id
    (Self : HTTP_Session) return League.Strings.Universal_String;

end Sessions;
