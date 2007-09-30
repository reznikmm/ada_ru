with Ada.Text_IO;

package body Users is

   Dir : constant String := "password/";

   function Password (User : String) return String is
      use Ada.Text_IO;

      Input : File_Type;
      Line  : String (1 .. 80);
      Last  : Natural;
   begin
      Open (Input, In_File, Dir & User);
      Get_Line (Input, Line, Last);
      Close (Input);

      return Line (1 .. Last);
   exception
      when others =>
         if Is_Open (Input) then
            Close (Input);
         end if;

         return "";
   end Password;

   procedure Set_Password (User, Password : String) is
      use Ada.Text_IO;

      Output : File_Type;
   begin
      Open (Output, Out_File, Dir & User);
      Put_Line (Output, Password);
      Close (Output);
   exception
      when others =>
         if Is_Open (Output) then
            Close (Output);
         end if;

         raise;
   end Set_Password;

end Users;
