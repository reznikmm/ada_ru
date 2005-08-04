with Ada.Text_IO;
with Ada.Command_Line;     use Ada.Command_Line;
with Ada_Links.Files.List; use Ada_Links.Files.List;

procedure Ada_Links.Run is
begin
   if Argument_Count /= 2 then
      Ada.Text_IO.Put_Line ("Usage: ada_links-run input_file output_file");
      Set_Exit_Status (Failure);
      return;
   end if;
   Read_From_File (Argument (1));
   Search_Versions;
   Save_To_File (Argument (2));
exception
   when others =>
      Set_Exit_Status (Failure);
      raise;
end Ada_Links.Run;
