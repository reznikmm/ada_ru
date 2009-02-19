with Wiki.Special_Formats;

package Wiki.HTML_Output is
   use Wiki.Special_Formats;

   type Context is private;

   procedure Initialize
     (Data            :    out Context;
      Wiki_URI_Prefix : in     String;
      Arguments       : in     Argument_List := Null_Arguments);

   procedure Start_Element
     (Info : in     Element_Info;
      Data : in out Context);

   procedure End_Element
     (Info : in     Element_Info;
      Data : in out Context);

   procedure Characters
     (Text : in     String;
      Data : in out Context);

   function Get_Text (Data : Context) return String;

   function Clean (Text : String; Space : Boolean := False) return String;
   --  mangle html entities

private
   package U renames Ada.Strings.Unbounded;

   type Context is record
      Buffer           : U.Unbounded_String;
      Wiki_URI         : U.Unbounded_String;
      In_Mono          : Boolean;
      Special_Format   : Special_Formats.Special_Formatter;
      Special_Argument : U.Unbounded_String;
      Arguments        : Argument_List;
   end record;

end Wiki.HTML_Output;

