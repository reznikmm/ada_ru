with League.IRIs;
with League.Strings;

with Matreshka.Internals.SAX_Locators;

package Axe.Dummy_Locators is

   type Dummy_Shared_Locator is
     new Matreshka.Internals.SAX_Locators.Shared_Abstract_Locator with
      record
         null;  --  System_Id : League.Strings.Universal_String;
      end record;

   overriding function Line
     (Self : not null access constant Dummy_Shared_Locator)
         return Natural is (0);

   overriding function Column
     (Self : not null access constant Dummy_Shared_Locator)
         return Natural is (0);

   overriding function Encoding
     (Self : not null access constant Dummy_Shared_Locator)
         return League.Strings.Universal_String;

   overriding function Version
     (Self : not null access constant Dummy_Shared_Locator)
         return League.Strings.Universal_String is
     (League.Strings.Empty_Universal_String);

   overriding function Public_Id
     (Self : not null access constant Dummy_Shared_Locator)
         return League.Strings.Universal_String is
     (League.Strings.Empty_Universal_String);

   overriding function System_Id
     (Self : not null access constant Dummy_Shared_Locator)
         return League.Strings.Universal_String is
     (League.Strings.Empty_Universal_String);

   overriding function Base_URI
     (Self : not null access constant Dummy_Shared_Locator)
         return League.IRIs.IRI;

   Locator : aliased Dummy_Shared_Locator;

end Axe.Dummy_Locators;
