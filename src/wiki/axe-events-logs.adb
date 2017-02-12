with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Wide_Wide_Text_IO;

with Axe.Wiki.Titles;
with Axe.Wiki.Parser;

package body Axe.Events.Logs is

   New_Line : constant Wide_Wide_String :=
     (1 => Ada.Characters.Wide_Wide_Latin_1.LF);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Event_Log_Writer'Class;
      File : League.Strings.Universal_String) is
   begin
      Self.File := File;
   end Initialize;

   -------------------
   -- On_Wiki_Saved --
   -------------------

   overriding procedure On_Wiki_Saved
     (Self    : in out Event_Log_Writer;
      URI     : League.Strings.Universal_String;
      Text    : League.Strings.Universal_String;
      User    : League.Strings.Universal_String;
      Created : Boolean)
   is
      pragma Unreferenced (User);
      Name  : constant String := Self.File.To_UTF_8_String;
      File  : Ada.Wide_Wide_Text_IO.File_Type;
      Info  : League.Strings.Universal_String;
      Title : Axe.Wiki.Titles.Handler;
   begin
      if not Created then
         return;
      end if;

      Axe.Wiki.Parser.Parse (Text, Title);
      Info.Append (New_Line);
      Info.Append ("=== ");
      Info.Append (Title.Title);
      Info.Append (" ===");
      Info.Append (New_Line);
      Info.Append ("[wiki:");
      Info.Append (URI);
      Info.Append ("]");
      Info.Append (New_Line);
      Info.Append (Title.Description);

      Ada.Wide_Wide_Text_IO.Open
        (File, Ada.Wide_Wide_Text_IO.Out_File, Name, "SHARED=NO,WCEM=8");
      Ada.Wide_Wide_Text_IO.Put_Line (File, Info.To_Wide_Wide_String);
      Ada.Wide_Wide_Text_IO.Close (File);
   end On_Wiki_Saved;

end Axe.Events.Logs;
