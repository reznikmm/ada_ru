package body Sessions is

   -----------------------
   -- Get_Creation_Time --
   -----------------------

   overriding function Get_Creation_Time
    (Self : HTTP_Session) return League.Calendars.Date_Time is
   begin
      raise Program_Error with "Unimplemented";
      return Self.Created;
   end Get_Creation_Time;

   ------------
   -- Get_Id --
   ------------

   overriding function Get_Id
     (Self : HTTP_Session) return League.Strings.Universal_String is
   begin
      return Self.Id;
   end Get_Id;

   ----------------------------
   -- Get_Last_Accessed_Time --
   ----------------------------

   overriding function Get_Last_Accessed_Time
    (Self : HTTP_Session) return League.Calendars.Date_Time is
   begin
      raise Program_Error with "Unimplemented";
      return Self.Accessed;
   end Get_Last_Accessed_Time;

   -------------------
   -- Get_User_Info --
   -------------------

   not overriding function Get_User_Info
     (Self : HTTP_Session) return User_Info is
   begin
      return Self.Info;
   end Get_User_Info;

   ------------
   -- Is_New --
   ------------

   overriding function Is_New (Self : HTTP_Session) return Boolean is
      pragma Unreferenced (Self);
   begin
      raise Program_Error with "Unimplemented";
      return False;
   end Is_New;

end Sessions;
