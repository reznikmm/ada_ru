package Users is

   function Password (User : String) return String;

   procedure Set_Password
     (User, Password : String);

end Users;
