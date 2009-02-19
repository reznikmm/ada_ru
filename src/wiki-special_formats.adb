with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;

package body Wiki.Special_Formats is

   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Special_Formatter,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   Map : Maps.Map;

   --------------
   -- Register --
   --------------

   procedure Register
     (Format    : String;
      Formatter : Special_Formatter)
   is
   begin
      Maps.Insert (Map, Format, Formatter);
   end Register;

   ---------
   -- Get --
   ---------

   function Get (Format : String) return Special_Formatter is
      C : constant Maps.Cursor := Maps.Find (Map, Format);
   begin
      if Maps.Has_Element (C) then
         return Maps.Element (C);
      else
         return null;
      end if;
   end Get;

end Wiki.Special_Formats;
