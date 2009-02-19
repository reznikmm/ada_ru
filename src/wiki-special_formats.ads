with Ada.Strings.Unbounded;

package Wiki.Special_Formats is

   type Strings is array (Positive range <>) of
     Ada.Strings.Unbounded.Unbounded_String;

   subtype Argument_Length is Natural range 0 .. 5;

   type Argument_List (Length : Argument_Length := 0) is record
      Names  : Strings (1 .. Length);
      Values : Strings (1 .. Length);
   end record;

   Null_Arguments : constant Argument_List (0) :=
     (0, others => (others => Ada.Strings.Unbounded.Null_Unbounded_String));

   type Special_Formatter is access
     function (Text : String; Arg : Argument_List) return String;

   procedure Register
     (Format   : String;
      Formatter : Special_Formatter);

   function Get (Format : String) return Special_Formatter;

end Wiki.Special_Formats;
