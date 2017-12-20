package body Sessions is

   ------------
   -- Get_Id --
   ------------

   overriding function Get_Id
     (Self : HTTP_Session) return League.Strings.Universal_String is
   begin
      return Self.Id;
   end Get_Id;

end Sessions;
