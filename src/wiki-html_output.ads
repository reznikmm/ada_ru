package Wiki.HTML_Output is

   type Context is private;

   procedure Initialize
     (Data            :    out Context;
      Wiki_URI_Prefix : in     String);

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

private
   package U renames Ada.Strings.Unbounded;

   type Context is record
      Buffer   : Ada.Strings.Unbounded.Unbounded_String;
      Wiki_URI : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Wiki.HTML_Output;

