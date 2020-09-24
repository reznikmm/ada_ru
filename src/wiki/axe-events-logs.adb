with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Wide_Wide_Text_IO;

with Axe.Read_File;
with Axe.Wiki.Parser;
with Axe.Wiki.Titles;

with League.Text_Codecs;

package body Axe.Events.Logs is

   New_Line : constant Wide_Wide_String :=
     (1 => Ada.Characters.Wide_Wide_Latin_1.LF);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : in out Event_Log_Writer'Class;
      File     : League.Strings.Universal_String;
      Password : League.Strings.Universal_String;
      Telegram : League.Strings.Universal_String;
      Viber    : League.Strings.Universal_String)
   is
      Text : League.Strings.Universal_String :=
        Axe.Read_File
          (Password, League.Text_Codecs.Codec_For_Application_Locale);
   begin
      Text := Text.Head
        (Text.Index (Ada.Characters.Wide_Wide_Latin_1.LF) - 1);
      Self.File := File;
      Self.Bot.Initialize (Text, Telegram, Viber);
   end Initialize;

   --------------------
   -- On_New_Runable --
   --------------------

   overriding procedure On_New_Runable
     (Self  : in out Event_Log_Writer;
      Value : Axe.Schedulers.Scheduled_Item) is
   begin
      Self.Bot.New_Runable (Value);
   end On_New_Runable;

   -----------------
   -- On_Telegram --
   -----------------

   overriding procedure On_Telegram
     (Self    : in out Event_Log_Writer;
      Message : League.JSON.Objects.JSON_Object;
      Result  : out League.JSON.Objects.JSON_Object) is
   begin
      Self.Bot.Telegram (Message, Result);
   end On_Telegram;

   ---------------------
   -- On_User_Created --
   ---------------------

   overriding procedure On_User_Created
     (Self    : in out Event_Log_Writer;
      Name    : League.Strings.Universal_String;
      Avatar  : League.Strings.Universal_String)
   is
      use type League.Strings.Universal_String;
   begin
      Self.Bot.Send_Message ("New site user: " & Name & " " & Avatar);
   end On_User_Created;

   --------------
   -- On_Viber --
   --------------

   overriding procedure On_Viber
     (Self    : in out Event_Log_Writer;
      Message : League.JSON.Objects.JSON_Object;
      Result  : out League.JSON.Objects.JSON_Object) is
   begin
      Self.Bot.Viber (Message, Result);
   end On_Viber;

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
      URL   : League.Strings.Universal_String;
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

      URL.Append ("https://www.ada-ru.org");
      URL.Append (URI);
      Self.Bot.Send_Message (Title.Title);
      Self.Bot.Send_Message (URL);
   end On_Wiki_Saved;

end Axe.Events.Logs;
