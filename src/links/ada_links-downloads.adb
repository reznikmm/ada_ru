with AWS.Client;
with AWS.Messages;
with AWS.Response;
with Ada.Text_IO;
with Ada.Exceptions;

package body Ada_Links.Downloads is

   Proxy : constant String := "http://localhost:3128";
--   Proxy : constant String := "http://proxy.alkar.net:3128";
--   Proxy : constant String := "";

   Connect : AWS.Client.HTTP_Connection;
   Max_Retry : constant := 25;

   subtype Success is AWS.Messages.Status_Code
     range AWS.Messages.S200 .. AWS.Messages.S206;


   -------------------
   -- Download_Page --
   -------------------

   function Download_Page (Url : String) return File_Text is
      use AWS.Response;

      Data  : AWS.Response.Data;
      Retry : Positive := 1;
   begin
      if Url'Length > 70 then
         Ada.Text_IO.Put ("Get " & Url (1 .. 70) & "..");
      else
         Ada.Text_IO.Put ("Get " & Url & "..");
      end if;
--      AWS.Client.Set_Debug (True);
      AWS.Client.Get (Connect, Data, Url);
      while Status_Code (Data) not in Success and Retry < Max_Retry loop
         AWS.Client.Get (Connect, Data, Url);
         Retry := Retry + 1;
         Ada.Text_IO.Put ("?");
         delay 1.5;
      end loop;
      if Retry < Max_Retry then
         Ada.Text_IO.Put_Line ("ok");
      else
         Ada.Text_IO.Put_Line ("err");
      end if;
      return AWS.Response.Message_Body (Data);
--   exception
--      when E : others =>
--        Ada.Text_IO.Put_Line
--          ("Exception:" & Ada.Exceptions.Exception_Name (E) &
--           " Message : " & Ada.Exceptions.Exception_Message (E));
--        raise;
--        return "";
   end Download_Page;

   --------------
   -- Get_Size --
   --------------

   function Get_Size (Url : String) return String is
      use AWS.Response;
      
      function To_String (X : Content_Length_Type) return String is
         Image : constant String := Content_Length_Type'Image (X);
      begin
         if X = Undefined_Length then
            return "";
         else
            return Image (2 .. Image'Last);
         end if;
      end To_String;
      
      Data  : AWS.Response.Data;
      Retry : Positive := 1;
   begin
      if Url'Length > 70 then
         Ada.Text_IO.Put ("Head " & Url (1 .. 70) & "..");
      else
         Ada.Text_IO.Put ("Head " & Url & "..");
      end if;
--      AWS.Client.Set_Debug (True);
      AWS.Client.Head (Connect, Data, Url);
      while Status_Code (Data) not in Success and Retry < Max_Retry loop
         AWS.Client.Head (Connect, Data, Url);
         Retry := Retry + 1;
         Ada.Text_IO.Put ("?");
         delay 1.5;
      end loop;
      if Retry < Max_Retry then
         Ada.Text_IO.Put_Line ("ok");
      else
         Ada.Text_IO.Put_Line ("err");
      end if;
      return To_String (Content_Length (Data));
   end Get_Size;

begin
   AWS.Client.Create (Connect, Proxy, Retry => 3);
end Ada_Links.Downloads;

