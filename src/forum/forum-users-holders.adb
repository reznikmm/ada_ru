package body Forum.Users.Holders is

   ---------------
   -- Component --
   ---------------

   procedure Component
     (Self    : aliased User_Reference;
      Name    : League.Strings.Universal_String;
      Value   : out League.Holders.Holder;
      Success : out Boolean)
   is
      use type League.Strings.Universal_String;
      Object : User renames Self.Container.User_Map (Self.Nickname);
   begin
      Success := True;

      if Name = +"nickname" then
         Value := League.Holders.To_Holder (Object.Nickname);
      elsif Name = +"name" then
         Value := League.Holders.To_Holder (Object.Name);
      elsif Name = +"avatar" then
         Value := League.Holders.To_Holder (Object.Avatar);
      else
         Success := False;
      end if;
   end Component;

end Forum.Users.Holders;
