package Wiki.HTML_Output.With_Ada is

   type Context is new HTML_Output.Context with private;

   procedure Start_Element
     (Info : in     Element_Info;
      Data : in out Context);

   procedure End_Element
     (Info : in     Element_Info;
      Data : in out Context);

   procedure Characters
     (Text : in     String;
      Data : in out Context);

private
   type Context is new HTML_Output.Context with record
      In_Preformated : Boolean := False;
   end record;

end Wiki.HTML_Output.With_Ada;

