
with League.Strings;

with Servlet.HTTP_Sessions;

with Servlet.OAuth;

package Sessions is

   type HTTP_Session is new Servlet.HTTP_Sessions.HTTP_Session with private;

   type HTTP_Session_Access is access all HTTP_Session'Class;

   not overriding function Get_User_Info
     (Self : HTTP_Session) return Servlet.OAuth.User_Info;

private

   type HTTP_Session is new Servlet.HTTP_Sessions.HTTP_Session with record
      Id   : League.Strings.Universal_String;
      Info : Servlet.OAuth.User_Info;
   end record;

   overriding function Get_Id
    (Self : HTTP_Session) return League.Strings.Universal_String;

end Sessions;
