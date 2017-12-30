package body Sessions is

   ------------
   -- Get_Id --
   ------------

   overriding function Get_Id
     (Self : HTTP_Session) return League.Strings.Universal_String is
   begin
      return Self.Id;
   end Get_Id;

   -------------------
   -- Get_User_Info --
   -------------------

   not overriding function Get_User_Info
     (Self : HTTP_Session) return Servlet.OAuth.User_Info is
   begin
      return Self.Info;
   end Get_User_Info;

end Sessions;
