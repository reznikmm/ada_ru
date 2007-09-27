with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Strings.Unbounded;

package body Wiki.Utils is
   ---------------
   -- Read_File --
   ---------------

   function Read_File (Name : String) return String is
      use Ada.Text_IO;
      use Ada.Exceptions;
      use Ada.Strings.Unbounded;
      Input : File_Type;
      Line  : String (1 .. 80);
      Last  : Natural;
      Text  : Unbounded_String;
   begin
      Open (Input, In_File, Name);

      while not End_Of_File (Input) loop
         Get_Line (Input, Line, Last);

         Text := Text & Line (1 .. Last);

         if Last /= Line'Last or End_Of_Line (Input) then
            Text := Text & ASCII.LF;
         end if;
      end loop;

      Close (Input);

      return To_String (Text);
   exception
      when E : Name_Error =>
         Raise_Exception
           (Name_Error'Identity,
            "Read_File " & Name & ":" & Exception_Information (E));

   end Read_File;

end Wiki.Utils;
