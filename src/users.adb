with Ada.Command_Line;

package body Users is

   function Password (User : String) return String is
      use Ada.Command_Line;

      Index : Positive := 1;
   begin
      while Index < Argument_Count loop
         if User = Argument (Index) then
            return Argument (Index + 1);
         end if;

         Index := Index + 1;
      end loop;

      return "";
   end Password;

end Users;
