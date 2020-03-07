with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Streams;
with League.String_Vectors;

package body IRC.Sessions is

   LF   : constant Wide_Wide_String :=
     (1 => Ada.Characters.Wide_Wide_Latin_1.LF);
   CR   : constant Wide_Wide_String :=
     (1 => Ada.Characters.Wide_Wide_Latin_1.CR);
   New_Line : constant Wide_Wide_String := CR & LF;

   ------------------
   -- Check_Socket --
   ------------------

   not overriding procedure Check_Socket
     (Self   : in out Session;
      Closed : out Boolean)
   is
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;

      procedure On_Command
        (Data : in out League.Stream_Element_Vectors.Stream_Element_Vector);

      ----------------
      -- On_Command --
      ----------------

      procedure On_Command
        (Data : in out League.Stream_Element_Vectors.Stream_Element_Vector)
      is
         use type League.Strings.Universal_String;

         List : League.String_Vectors.Universal_String_Vector;
         Text : League.Strings.Universal_String :=
           Self.Codec.Decode (Data);
      begin
         if Text.Ends_With (LF) then
            Text.Slice (1, Text.Length - 1);  --  Drop LF

            if Text.Ends_With (CR) then
               Text.Slice (1, Text.Length - 1);  --  Drop CR
            end if;
         end if;

         List := Text.Split (' ');

         declare
            Text : League.Strings.Universal_String;
         begin
            for J in 3 .. List.Length loop
               Text := List.Element (J);
               if Text.Starts_With (":") then
                  --  Drop leading ':' from last parameter
                  List.Replace (J, Text.Tail_From (2));
                  exit;
               end if;
            end loop;
         end;

         if List.Length > 3
           and then List.Element (1).Starts_With (":")
           and then List.Element (2) = +"PRIVMSG"
         then
            declare
               Text : League.Strings.Universal_String;
               From : constant League.Strings.Universal_String :=
                 List.Element (1).Tail_From (2); --  Drop leading ':'
               To   : constant League.Strings.Universal_String :=
                 List.Element (3);
            begin
               List := List.Slice (4, List.Length);
               Text := List.Join (" ");

               Self.Listener.On_Message
                 (Self'Unchecked_Access, To, From, Text);
            end;
         elsif List.Length > 3
           and then List.Element (1).Starts_With (":")
           and then List.Element (2) = +"NOTICE"
         then
            declare
               Text : League.Strings.Universal_String;
               From : constant League.Strings.Universal_String :=
                 List.Element (1).Tail_From (2); --  Drop leading ':'
               To   : constant League.Strings.Universal_String :=
                 List.Element (3);
            begin
               List := List.Slice (4, List.Length);
               Text := List.Join (" ");

               Self.Listener.On_Notice
                 (Self'Unchecked_Access, To, From, Text);
            end;
         elsif List.Length > 1 and then List.Element (1) = +"PING" then
            Self.Listener.On_Ping
              (Self'Unchecked_Access, List.Element (2));
         elsif List.Length > 2
           and then List.Element (1).Starts_With (":")
           and then List.Element (2) = +"PING"
         then
            Self.Listener.On_Ping
              (Self'Unchecked_Access, List.Element (3));
         end if;

         Data.Clear;
      end On_Command;

      Request : GNAT.Sockets.Request_Type (GNAT.Sockets.N_Bytes_To_Read);
      Data    : Ada.Streams.Stream_Element_Array (1 .. 512);
      First   : Ada.Streams.Stream_Element_Offset;
      Last    : Ada.Streams.Stream_Element_Offset;
   begin
      loop
         begin
            GNAT.Sockets.Receive_Socket (Self.Socket, Data, Last);
            Closed := Last = Data'First - 1;
         exception
            when GNAT.Sockets.Socket_Error =>
               Closed := True;
               GNAT.Sockets.Close_Socket (Self.Socket);
               return;
         end;

         First := Data'First;

         for J in 1 .. Last loop
            if Data (J) = 16#0A# then
               Self.Vector.Append (Data (First .. J));
               On_Command (Self.Vector);
               First := J + 1;
            end if;
         end loop;

         Self.Vector.Append (Data (First .. Last));

         GNAT.Sockets.Control_Socket (Self.Socket, Request);

         exit when Request.Size = 0;
      end loop;
   end Check_Socket;

   -------------
   -- Connect --
   -------------

   not overriding procedure Connect
     (Self      : in out Session;
      Socket    : out GNAT.Sockets.Socket_Type;
      Host      : League.Strings.Universal_String;
      Port      : GNAT.Sockets.Port_Type;
      Nick      : League.Strings.Universal_String;
      Password  : League.Strings.Universal_String;
      User      : League.Strings.Universal_String;
      Real_Name : League.Strings.Universal_String)
   is
      Address : GNAT.Sockets.Sock_Addr_Type :=
        (Family => GNAT.Sockets.Family_Inet,
         Addr   => <>,
         Port   => Port);
      Vector  : League.Stream_Element_Vectors.Stream_Element_Vector;
      Value   : League.Strings.Universal_String;
      Request : GNAT.Sockets.Request_Type :=
        (GNAT.Sockets.Non_Blocking_IO, True);
   begin
      GNAT.Sockets.Create_Socket (Socket);

      begin
         Address.Addr := GNAT.Sockets.Addresses
           (GNAT.Sockets.Get_Host_By_Name (Host.To_UTF_8_String), 1);
         GNAT.Sockets.Connect_Socket (Socket, Address);
      exception
         when GNAT.Sockets.Socket_Error | GNAT.Sockets.Host_Error =>
            GNAT.Sockets.Close_Socket (Socket);
            Socket := GNAT.Sockets.No_Socket;
            return;
      end;

      GNAT.Sockets.Control_Socket (Socket, Request);
      Self.Socket := Socket;

      if not Password.Is_Empty then
         Value.Append ("PASS ");
         Value.Append (Password);
         Value.Append (New_Line);
      end if;

      Value.Append ("NICK ");
      Value.Append (Nick);
      Value.Append (New_Line);

      Value.Append ("USER ");
      Value.Append (User);
      Value.Append (" 0 * :");
      Value.Append (Real_Name);
      Value.Append (New_Line);

      Vector := Self.Codec.Encode (Value);

      declare
         Last    : Ada.Streams.Stream_Element_Offset;
         pragma Unreferenced (Last);
      begin
         GNAT.Sockets.Send_Socket
           (Socket, Vector.To_Stream_Element_Array, Last);
      end;
   end Connect;

   ----------
   -- Join --
   ----------

   not overriding procedure Join
     (Self    : in out Session;
      Channel : League.Strings.Universal_String)
   is
      Last   : Ada.Streams.Stream_Element_Offset;
      pragma Unreferenced (Last);
      Value  : League.Strings.Universal_String;
      Vector : League.Stream_Element_Vectors.Stream_Element_Vector;
   begin
      Value.Append ("JOIN ");
      Value.Append (Channel);
      Value.Append (New_Line);
      Vector := Self.Codec.Encode (Value);

      GNAT.Sockets.Send_Socket
        (Self.Socket, Vector.To_Stream_Element_Array, Last);
   end Join;

   ----------
   -- Pong --
   ----------

   not overriding procedure Pong
     (Self   : in out Session;
      Target : League.Strings.Universal_String)
   is
      Last   : Ada.Streams.Stream_Element_Offset;
      pragma Unreferenced (Last);
      Value  : League.Strings.Universal_String;
      Vector : League.Stream_Element_Vectors.Stream_Element_Vector;
   begin
      Value.Append ("PONG ");
      Value.Append (Target);
      Value.Append (New_Line);
      Vector := Self.Codec.Encode (Value);

      GNAT.Sockets.Send_Socket
        (Self.Socket, Vector.To_Stream_Element_Array, Last);
   end Pong;

   -----------------
   -- Raw_Command --
   -----------------

   not overriding procedure Raw_Command
     (Self    : in out Session;
      Command : League.Strings.Universal_String)
   is
      Last   : Ada.Streams.Stream_Element_Offset;
      pragma Unreferenced (Last);
      Value  : League.Strings.Universal_String;
      Vector : League.Stream_Element_Vectors.Stream_Element_Vector;
   begin
      Value.Append (Command);
      Value.Append (New_Line);
      Vector := Self.Codec.Encode (Value);

      GNAT.Sockets.Send_Socket
        (Self.Socket, Vector.To_Stream_Element_Array, Last);
      --  FIXME: Socket_Error could be raised here
   end Raw_Command;

   ------------------
   -- Send_Message --
   ------------------

   not overriding procedure Send_Message
     (Self   : in out Session;
      Target : League.Strings.Universal_String;
      Text   : League.Strings.Universal_String)
   is
      procedure Send_Line (Text : League.Strings.Universal_String);

      ---------------
      -- Send_Line --
      ---------------

      procedure Send_Line (Text : League.Strings.Universal_String) is
         Value  : League.Strings.Universal_String;
         Vector : League.Stream_Element_Vectors.Stream_Element_Vector;
         Last   : Ada.Streams.Stream_Element_Offset;
         pragma Unreferenced (Last);
      begin
         Value.Append ("PRIVMSG ");
         Value.Append (Target);
         Value.Append (" :");
         Value.Append (Text);
         Value.Append (New_Line);
         Vector := Self.Codec.Encode (Value);

         GNAT.Sockets.Send_Socket
           (Self.Socket, Vector.To_Stream_Element_Array, Last);
      end Send_Line;

      List   : constant League.String_Vectors.Universal_String_Vector :=
        Text.Split (Ada.Characters.Wide_Wide_Latin_1.LF);
   begin
      if List.Length > 1 then
         for J in 1 .. List.Length loop
            if J > 1 then
               delay 0.3;
            end if;

            Send_Line (List (J));
         end loop;

      else
         Send_Line (Text);
      end if;

   end Send_Message;

end IRC.Sessions;
