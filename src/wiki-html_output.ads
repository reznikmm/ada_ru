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

   function Clean (Text : String; Space : Boolean := False) return String;
   --  mangle html entities

   type Context is record
      Buffer   : U.Unbounded_String;
      Wiki_URI : U.Unbounded_String;
      In_Mono  : Boolean;
   end record;

end Wiki.HTML_Output;

