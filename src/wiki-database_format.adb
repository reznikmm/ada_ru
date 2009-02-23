with Adbi;
with Adbi.Statements.Queries;
with Ada.Strings.Unbounded;
with Users;
with Encodings;
with Ada.Text_IO;

with XSLT.Controlled;

package body Wiki.Database_Format is

   use type Ada.Strings.Unbounded.Unbounded_String;

   function "+" (Text : Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   Connect : Adbi.Session := Adbi.Null_Session;

   User     : constant String := "ada_ru";
   Password : constant String := Users.Password (User);
   Database : constant String := "firebird:localhost:ada_ru";

   use Adbi;
   use Adbi.Statements.Queries;

   function Encode (Text : String) return String is
      Result : String (1 .. Text'Length * 6);
      Last   : Natural := 0;

      procedure Append (Piece : String) is
      begin
         Result (Last + 1 .. Last + Piece'Length) := Piece;
         Last := Last + Piece'Length;
      end;
   begin
      for J in Text'Range loop
         case Text (J) is
           when '<' => Append ("&lt;");
           when '>' => Append ("&gt;");
           when '&' => Append ("&amp;");
           when ''' => Append ("&apos;");
           when '"' => Append ("&quot;");
           when others =>
              Last := Last + 1;
              Result (Last) := Text (J);
         end case;
      end loop;

      return Result (1 .. Last);
   end Encode;

   function Prepare_Query
     (Text : String; Arg : Special_Formats.Argument_List)
      return Query_Statement
   is
   begin
      if Connect = Null_Session then
         Connect := Logon (User, Password, Database, Encodings.CP_1251);
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

         return Query;
       end;
    end Prepare_Query;

   function DB_To_Text (Text : String; Arg : Special_Formats.Argument_List)
                       return String
   is
      Query : Query_Statement;
   begin
      Query := Prepare_Query (Text, Arg);
      Execute (Query);
      return Column_Value (Query, 1);
   exception
      when Database_Error =>
         Connect := Adbi.Null_Session;
         raise;
   end DB_To_Text;

   function DB_To_XSL (Text : String; Arg : Special_Formats.Argument_List)
                      return String
   is
      use XSLT.Controlled;
      Result : Ada.Strings.Unbounded.Unbounded_String;
      Buffer : String (1 .. 1024);
      Last   : Natural := 0;

      procedure Append (Piece : String) is
      begin
         if Piece'Length = 0 or Last + Piece'Length > Buffer'Last then
            --  Flush buffer
            Result := Result & Buffer (1 .. Last);
            Last   := 0;
         end if;
         Buffer (Last + 1 .. Last + Piece'Length) := Piece;
         Last := Last + Piece'Length;
      end;

      Link   : URI := URI (+Arg.Values (1));
      XSLT   : Xsl := To_XSL (Link);
      Query  : Query_Statement;
   begin
      Query := Prepare_Query (Text, Arg);
      Execute (Query);
      Append ("<?xml version='1.0' encoding='koi8-r'?>");
      Append ("<list>");
      Append ("<h>");

      for J in 1 .. Column_Count (Query) loop
         Append ("<n>" & Encode (String (Column_Name (Query, J))) & "</n>");
      end loop;

      Append ("</h>");

      while Has_Rows (Query) loop
         Append ("<r>");

         for J in 1 .. Column_Count (Query) loop
            Append ("<d>" & Encode (Column_Value (Query, J)) & "</d>");
         end loop;

         Append ("</r>");
         Fetch_Next (Query);
      end loop;

      Append ("</list>");
      Append ("");

      return Transform (XSLT, +Result);
   end DB_To_XSL;

begin
   Set_Client_Encoding (Encodings.KOI8_R);
end Wiki.Database_Format;
