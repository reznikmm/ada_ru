package body HTML_Handlers is

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Self    : in out HTML_Handler;
      Text    : League.Strings.Universal_String;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);
   begin
      Self.Result.Append (Text);
   end Characters;

   ------------------
   -- Error_String --
   ------------------

   overriding function Error_String
     (Self : HTML_Handler) return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return League.Strings.Empty_Universal_String;
   end Error_String;

end HTML_Handlers;
