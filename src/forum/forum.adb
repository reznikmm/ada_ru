package body Forum is

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Holder : League.Holders.Holder)
         return League.Strings.Universal_String is
   begin
      if League.Holders.Is_Empty (Holder) then
         return League.Strings.Empty_Universal_String;
      else
         return League.Holders.Element (Holder);
      end if;
   end To_String;

end Forum;
