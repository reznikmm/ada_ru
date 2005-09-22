with AWS.Client;
with AWS.Messages;
with AWS.Response;
with Ada.Text_IO;
with Ada.Exceptions;

with AI302.Strings.Hash;
with AI302.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Ada_Links.Downloads is

   package Containers is
     new AI302.Containers.Indefinite_Hashed_Maps
     (String, Unbounded_String, AI302.Strings.Hash, "=", "=");

   Map : Containers.Map;

   Proxy : constant String := "http://localhost:3128";
--   Proxy : constant String := "http://proxy.alkar.net:3128";
--   Proxy : constant String := "";

   Max_Retry : constant := 5;

   subtype Success is AWS.Messages.Status_Code
     range AWS.Messages.S200 .. AWS.Messages.S206;

   function Get_Page (Url : String) return File_Text;

   -------------------
   -- Download_Page --
   -------------------

   function Download_Page (Url : String) return File_Text is
      use Containers;
   begin
      if Is_In (Url, Map) then
         return To_String (Element (Map, Url));
      else
         declare
            Text : File_Text := Get_Page (Url);
            Pos  : Cursor;
            Ok   : Boolean;
         begin
            Insert (Map, Url, To_Unbounded_String (Text), Pos, Ok);
            return Text;
         end;
      end if;
   end Download_Page;

   --------------
   -- Get_Page --
   --------------

   function Get_Page (Url : String) return File_Text is
      use AWS.Response;

      Data    : AWS.Response.Data;
      Retry   : Positive := 1;
      Connect : AWS.Client.HTTP_Connection;
   begin
      if Url'Length > 70 then
         Ada.Text_IO.Put ("Get " & Url (1 .. 70) & "..");
      else
         Ada.Text_IO.Put ("Get " & Url & "..");
      end if;
      --      AWS.Client.Set_Debug (True);
      if Proxy = "" then
         Data := AWS.Client.Get (Url);
      else
         AWS.Client.Create (Connect, Url, Proxy => Proxy, Retry => 3);
         AWS.Client.Get (Connect, Data);
      end if;

      while Status_Code (Data) not in Success and Retry < Max_Retry loop
         if Proxy = "" then
            Data := AWS.Client.Get (Url);
         else
            AWS.Client.Create (Connect, Url, Proxy => Proxy, Retry => 3);
            AWS.Client.Get (Connect, Data);
         end if;

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
   end Get_Page;

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

      Data    : AWS.Response.Data;
      Retry   : Positive := 1;
      Connect : AWS.Client.HTTP_Connection;
   begin
      if Url'Length > 70 then
         Ada.Text_IO.Put ("Head " & Url (1 .. 70) & "..");
      else
         Ada.Text_IO.Put ("Head " & Url & "..");
      end if;
      --      AWS.Client.Set_Debug (True);
      if Proxy = "" then
         Data := AWS.Client.Head (Url);
      else
         AWS.Client.Create (Connect, Url, Proxy => Proxy, Retry => 3);
         AWS.Client.Head (Connect, Data);
      end if;

      while Status_Code (Data) not in Success and Retry < Max_Retry loop
         if Proxy = "" then
            Data := AWS.Client.Head (Url);
         else
            AWS.Client.Create (Connect, Url, Proxy => Proxy, Retry => 3);
            AWS.Client.Head (Connect, Data);
         end if;

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

end Ada_Links.Downloads;

