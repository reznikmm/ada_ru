package Wiki.Parser is

   generic
      type Context is limited private;

      with procedure Start_Element
        (Info : in     Element_Info;
         Data : in out Context);

      with procedure End_Element
        (Info : in     Element_Info;
         Data : in out Context);

      with procedure Characters
        (Text : in     String;
         Data : in out Context);

   procedure Parse
     (Text : in     String;
      Data : in out Context);

   function Replace (Text, From, To : String) return String;
   -- Replace in Text all occurences of From to To.

end Wiki.Parser;
