package body Axe.Dummy_Locators is

   --------------
   -- Base_URI --
   --------------

   overriding function Base_URI
     (Self : not null access constant Dummy_Shared_Locator)
         return League.IRIs.IRI
   is
      pragma Unreferenced (Self);
   begin
      return Result : League.IRIs.IRI;
   end Base_URI;

   --------------
   -- Encoding --
   --------------

   overriding function Encoding
     (Self : not null access constant Dummy_Shared_Locator)
        return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return League.Strings.To_Universal_String ("utf-8");
   end Encoding;

end Axe.Dummy_Locators;
