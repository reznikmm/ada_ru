
with League.Strings;
with League.String_Vectors;

with Servlet.HTTP_Sessions;
with League.Calendars;

with Databases;

package Sessions is

   type HTTP_Session is new Servlet.HTTP_Sessions.HTTP_Session with private;

   type HTTP_Session_Access is access all HTTP_Session'Class;

   type User_Info is record
      User   : League.Strings.Universal_String;
      Name   : League.Strings.Universal_String;
      Avatar : League.Strings.Universal_String;
      Mails  : League.String_Vectors.Universal_String_Vector;
   end record;

   not overriding function Get_User_Info
     (Self : HTTP_Session) return User_Info;

   function Database
    (Self : HTTP_Session'Class) return Databases.SQL_Database;
   --  Return new or reused session. Note that result is already openned.

private

   type HTTP_Session is new Servlet.HTTP_Sessions.HTTP_Session with record
      Id       : League.Strings.Universal_String;
      Info     : User_Info;
      Created  : League.Calendars.Date_Time;
      Accessed : League.Calendars.Date_Time;
      Pool     : not null access Databases.SQL_Database_Pool;
   end record;

   overriding function Get_Creation_Time
    (Self : HTTP_Session) return League.Calendars.Date_Time;

   overriding function Get_Last_Accessed_Time
    (Self : HTTP_Session) return League.Calendars.Date_Time;

   overriding function Is_New (Self : HTTP_Session) return Boolean;

   overriding function Get_Id
    (Self : HTTP_Session) return League.Strings.Universal_String;

end Sessions;
