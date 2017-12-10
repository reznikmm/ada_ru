with League.Calendars.ISO_8601;

with Forum.Contexts; pragma Unreferenced (Forum.Contexts);
with Forum.Users.Holders;

package body Forum.Posts.Holders is

   ---------------
   -- Component --
   ---------------

   procedure Component
     (Self    : aliased Post_Reference;
      Name    : League.Strings.Universal_String;
      Value   : out League.Holders.Holder;
      Success : out Boolean)
   is
      use type League.Strings.Universal_String;
      Object : Post renames Self.Container.Post_Map (Self.Id);
   begin
      Success := True;

      if Name = +"subject" then
         Value := League.Holders.To_Holder (Object.Subject);
      elsif Name = +"text" then
         Value := League.Holders.To_Holder (Object.Text);
      elsif Name = +"date" then
         declare
            Pattern : constant League.Strings.Universal_String :=
             +"yyyy-MM-dd HH:mm:ss";
            Image   : constant League.Strings.Universal_String :=
              League.Calendars.ISO_8601.Image (Pattern, Object.Date);
         begin
            Value := League.Holders.To_Holder (Image);
         end;
      elsif Name = +"author" then
         declare
            User : constant Forum.Users.Holders.User_Reference :=
              (Self.Container.Context.Users'Access, Object.Nickname);
         begin
            Value := Forum.Users.Holders.Compound_Holders.To_Holder (User);
         end;
      else
         Success := False;
      end if;
   end Component;

end Forum.Posts.Holders;
