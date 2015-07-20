with Ada.Strings.Unbounded;

package Text_Streams.Strings is

   ------------------------
   -- String_Text_Stream --
   ------------------------

   type String_Text_Stream is new Text_Stream with private;

   procedure Initialize
     (Object : in out String_Text_Stream;
      Text   : in String);

   procedure Read
     (Object : in out String_Text_Stream;
      Text   :    out String;
      Last   :    out Natural);

private

   package U renames Ada.Strings.Unbounded;

   type String_Text_Stream is new Text_Stream with record
      Input  : U.Unbounded_String;
      Length : Natural;
      Index  : Positive;
   end record;

end Text_Streams.Strings;


