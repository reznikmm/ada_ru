package Wiki.HTML_Output.With_Ada is

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
   type Context is record
      Parent         : HTML_Output.Context;
      In_Preformated : Boolean := False;
   end record;

end Wiki.HTML_Output.With_Ada;

