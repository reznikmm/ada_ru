with AWS.Client;            use AWS.Client;
with AWS.Response;
--  with AWS.Net.SSL;
with Ada.Text_IO;
with Ada.Streams;           use Ada.Streams;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

procedure Post is
   Input   : File_Type;
   Connect : HTTP_Connection;
begin
   if Argument_Count < 4 then
      Ada.Text_IO.Put_Line ("Usage: post <url> <user> <pwd> <file_name>");
      return;
   end if;

   --  AWS.Net.SSL.Initialize ("cert.pem");

   Create (Connect, Argument (1));

   Set_WWW_Authentication
     (Connect, Argument (2), Argument (3), Digest);

   for I in 4 .. Argument_Count loop
      Open (Input, In_File, Argument (I));

      declare
         Length : Count := Size (Input);
         Data   : Stream_Element_Array (1 .. Stream_Element_Offset (Length));
         Last   : Stream_Element_Offset;
         Result : AWS.Response.Data;
      begin
         Ada.Text_IO.Put ("Posting :" & Argument (I));
         Read (Input, Data, Last, 1);
         Post (Connect, Result,
               Data => Data (1 .. Last),
               URI  => "/" & Argument (I));
         Ada.Text_IO.Put_Line (". Response :" &
           AWS.Response.Message_Body (Result));
      end;

      Close (Input);
   end loop;
end Post;
