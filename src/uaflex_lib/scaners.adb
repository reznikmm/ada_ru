package body Scaners is

   procedure Read_Buffer (Object : in out Scaner);

   -----------
   -- Enter --
   -----------

   procedure Enter
     (Object : in out Scaner;
      Start  : in     State)
   is
   begin
      Object.Start := Start;
   end Enter;

   -----------------
   -- Read_Buffer --
   -----------------

   procedure Read_Buffer (Object : in out Scaner) is
      Plain_Text  : String (1 .. Buffer_Half);
      Plain_Last  : Natural := 1;
      Decoded     : Natural;
      Buffer_Last : Positive;
   begin
      --  Restore non-decoded raw string (part of utf-8 char)
      if Object.Rest_Lenght > 0 then
         Plain_Text (1 .. Object.Rest_Lenght) :=
           Object.Rest (1 .. Object.Rest_Lenght);
         Plain_Last := Object.Rest_Lenght + 1;
      end if;

      --  Read some data
      Text_Streams.Read
        (Object => Object.Input.all,
         Text   => Plain_Text (Plain_Last .. Plain_Text'Last),
         Last   => Plain_Last);

      --  Nothing has been read
      if Plain_Last = Object.Rest_Lenght then
         Object.Finished := True;
         return;
      end if;

      --  Decode to Wide_String & Classes
      Encodings_Classes.Decode
        (Text         => Plain_Text (1 .. Plain_Last),
         Text_Last    => Decoded,
         Result       => Object.Buffer (Object.Next .. Object.Buffer'Last),
         Result_Last  => Buffer_Last,
         Classes      => Object.Class,
         Object       => Object.Coder);

      --  If end of buffer reached
      if Buffer_Last >= Buffer_Size then
         --  Move last char from reserved place to begin on buffer
         if Buffer_Last > Buffer_Size then
            Object.Buffer (1) := Object.Buffer (Buffer_Size + 1);
            Object.Class  (1) := Object.Class  (Buffer_Size + 1);
            Buffer_Last := 2;
         else
            Buffer_Last := 1;
         end if;

         if Decoded < Plain_Last then
            --  Try to decode rest of raw string
            Encodings_Classes.Decode
              (Text         => Plain_Text (Decoded + 1 .. Plain_Last),
               Text_Last    => Decoded,
               Result       =>
                 Object.Buffer (Buffer_Last .. Object.Buffer'Last),
               Result_Last  => Buffer_Last,
               Classes      => Object.Class,
               Object       => Object.Coder);

            Buffer_Last := Buffer_Last + 1;
         end if;
      else
         Buffer_Last := Buffer_Last + 1;
      end if;

      --  Mark end of buffer
      Object.Buffer (Buffer_Last) := EOF;
      Object.Class  (Buffer_Last) := Unknown;
      --  Save non-decoded raw string (part of utf-8 char)
      Object.Rest_Lenght := Plain_Last - Decoded;
      Object.Rest (1 .. Object.Rest_Lenght) :=
        Plain_Text (Decoded + 1 .. Plain_Last);
   end Read_Buffer;

   ----------------
   -- Next_Token --
   ----------------

   procedure Next_Token
     (Object : in out Scaner;
      Result :    out Token)
   is
      ----------
      -- Next --
      ----------

      procedure Next is
      begin
         if Object.Next = Buffer_Size then
            Object.Next := 1;
         else
            Object.Next := Object.Next + 1;
         end if;
      end Next;

      Char          : Character_Class;
      Current_State : State;
      Error         : constant State := State'Last;
   begin
      if not Object.Started then
         Object.Started     := True;
         Object.Finished    := False;
         Object.Start       := 0;
         Object.Next        := 1;
         Object.Buffer (1)  := EOF;
         Object.Class (1)   := Unknown;
         Object.Rest_Lenght := 0;
      elsif Object.Finished and then Object.Buffer (Object.Next) = EOF then
         Result := File_End_Token;
         return;
      end if;

      Current_State    := Object.Start;
      Result           := Error_Token;
      Object.Last_From := Object.Next;
      Object.Last_To   := Object.Next - 1;

      loop
         Char := Object.Class (Object.Next);

         if Char < Surrogate then
            Current_State := Table (Current_State, Char);

            exit when Current_State = Error;

            if Finish (Current_State) /= Error_Token then
               Result := Finish (Current_State);
               Object.Last_To := Object.Next;
            end if;

            Next;
         elsif Object.Buffer (Object.Next) = EOF then
            if Object.Finished then
               exit;
            else
               Read_Buffer (Object);
            end if;
         elsif Char = Surrogate then
            Next;  --  skip surogate character
         else
            exit;
         end if;
      end loop;

      Object.Next := Object.Last_To;
      Next;
   end Next_Token;

   ------------------
   -- Set_Encoding --
   ------------------

   procedure Set_Encoding
     (Object  : in out Scaner;
      Charset : in     Encodings.Encoding)
   is
      Coder : Encodings_Classes.Coder (Charset);
   begin
      Object.Coder := Coder;
   end Set_Encoding;

   -----------------
   -- Token_Image --
   -----------------

   function Token_Image (Object : in Scaner) return Wide_String is
   begin
      if Object.Last_From <= Object.Last_To then
         return Object.Buffer (Object.Last_From .. Object.Last_To);
      elsif Object.Last_From = Object.Last_To + 1 then
         return "";
      else
         return Object.Buffer (Object.Last_From .. Buffer_Size)
           & Object.Buffer (1 .. Object.Last_To);
      end if;
   end Token_Image;

end Scaners;

