with Adbi;
with Adbi.Statements.Queries;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with XSLT.Controlled;

package body Wiki.Database_Format is

   use type Ada.Strings.Unbounded.Unbounded_String;

   function "+" (Text : Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   Connect : Adbi.Session := Adbi.Null_Session;

   User     : constant String := "l2";
   Password : constant String := "l2";
   Database : constant String := "oracle:tint";

   function DB_To_Text (Text : String; Arg : Special_Formats.Argument_List)
                       return String
   is
      use Adbi;
      use Adbi.Statements.Queries;
   begin
      if Connect = Null_Session then
         Connect := Logon (User, Password, Database);
      end if;

      declare
         Query : Query_Statement := Prepare (Connect, Text);
      begin
         for J in 1 .. Parameter_Count (Query) loop
            declare
               Name : constant Field_Name := Parameter_Name (Query, J);
            begin
               for K in 1 .. Arg.Length loop
                  if Field_Name (+Arg.Names (K)) = Name then
                     Set_Parameter (Query, J, +Arg.Values (K));
                     exit;
                  end if;
               end loop;
            end;
         end loop;

         Execute (Query);

         return Column_Value (Query, 1);
      end;
   exception
      when Database_Error =>
         Connect := Adbi.Null_Session;
         raise;
   end DB_To_Text;

   function DB_To_XSL (Text : String; Arg : Special_Formats.Argument_List)
                      return String
   is
      use XSLT.Controlled;
      Link : URI := URI (+Arg.Values (1));
      XSLT : Xsl := To_XSL (Link);
   begin
      return Transform (XSLT, DB_To_Text (Text, Arg));
   end DB_To_XSL;

end Wiki.Database_Format;
