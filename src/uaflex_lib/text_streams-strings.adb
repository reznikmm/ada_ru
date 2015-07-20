package body Text_Streams.Strings is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Object : in out String_Text_Stream;
      Text   : in String) is
   begin
      Object.Input  := U.To_Unbounded_String (Text);
      Object.Length := Text'Length;
      Object.Index  := 1;
   end Initialize;

   ----------
   -- Read --
   ----------

   procedure Read
     (Object : in out String_Text_Stream;
      Text   :    out String;
      Last   :    out Natural)
   is
      Length : constant Natural :=
        Natural'Min (Text'Length, Object.Length - Object.Index + 1);
   begin
      Last := Text'First + Length - 1;

      Text (Text'First .. Last) :=
        U.Slice (Object.Input, Object.Index, Object.Index + Length - 1);

      Object.Index := Object.Index + Length;
   end Read;

end Text_Streams.Strings;
