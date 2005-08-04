with GNAT.Regpat;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada_Links.Downloads;

package body Ada_Links.Files is


   ---------------
   -- File_Name --
   ---------------

   function File_Name
     (Object : File_Node)
     return String
   is
      use U;
      I : Natural := Index (Object.URL, "/", Ada.Strings.Backward);
   begin
      if I > 0 then
         return Slice (Object.URL, I + 1, Length (Object.URL));
      else
         return To_String (Object.URL);
      end if;
   end File_Name;


   ----------------------
   -- Find_Max_Version --
   ----------------------

   procedure Find_Max_Version (Page    : in     String;
                               Regexp  : in     String;
                               Version :    out U.Unbounded_String;
                               URL     :    out U.Unbounded_String)
   is
      use U;
      use GNAT.Regpat;
      
      function Newer (A : String; B : String) return Boolean is
         use Ada.Strings.Fixed;
         
         I  : Natural := Index (A, ".");
         J  : Natural := Index (B, ".");
         NA : Natural;
         NB : Natural;
      begin
         if I = 0 then
            I := A'Last + 1;
         end if;
         if J = 0 then
            J := B'Last + 1;
         end if;
         NA := Natural'Value (A (A'First .. I - 1));
         NB := Natural'Value (B (B'First .. J - 1));
         if NA = NB then
            return Newer (A (I + 1 .. A'Last), B (J + 1 .. B'Last));
         else
            return NA > NB;
         end if;

      exception 
         when Constraint_Error =>
            return A > B;
      end Newer;
      
      First   : Positive := Page'First;
      Search  : String :=
        "<[Aa][[:space:]\n\r][^>]*[Hh][Rr][Ee][Ff][[:space:]\n\r]*=[[:space:]\n\r]*['""](" &
        Regexp & ")['""]";
      Engine  : Pattern_Matcher := Compile (Search);
      Matches : Match_Array (0 .. 3);
   begin
      Version := Null_Unbounded_String;
      URL     := Null_Unbounded_String;
      loop
         Match (Engine, Page (First .. Page'Last), Matches);
         if Matches (2) /= No_Match then
            declare
               New_Ver  : String renames
                 Page (Matches (2).First .. Matches (2).Last);
               New_Link : String renames
                 Page (Matches (1).First .. Matches (1).Last);
            begin
               Ada.Text_IO.Put_Line ("Found version " & New_Ver & "!");
               if Newer (New_Ver, To_String (Version)) then
                  Version := To_Unbounded_String (New_Ver);
                  URL    := To_Unbounded_String (New_Link);
               end if;
            end;
         end if;
         exit when Matches (0) = No_Match;
         First := Matches (0).Last;
      end loop;
   end Find_Max_Version;

   --------
   -- Id --
   --------

   function Id
     (Object : File_Node)
      return String
   is
   begin
      return U.To_String (Object.Id);
   end Id;

   --------------------
   -- Normalize_Link --
   --------------------

   function Normalize_Link
     (Base : U.Unbounded_String;
      Link : U.Unbounded_String)
     return U.Unbounded_String
   is
      use U;
      use Ada.Strings;
   begin
      if Index (Link, "ftp:") = 1 or Index (Link, "http:") = 1 then
         return Link;
      else
         declare
            Pos : Positive := Index (Base, "/", Backward);
         begin
            return Slice (Base, 1, Pos) & Link;
         end;
      end if;
   end Normalize_Link;

   ----------
   -- Size --
   ----------

   function Size (Object : File_Node) return String is
      use U;
   begin
      if Object.Size = "" then
         return Downloads.Get_Size (URL (Object));
      else
         return To_String (Object.Size);
      end if;
   end Size;

   ---------
   -- URL --
   ---------

   function URL
     (Object : File_Node)
      return String
   is
   begin
      return U.To_String (Object.URL);
   end URL;

   -------------
   -- Version --
   -------------

   function Version
     (Object : File_Node)
      return String
   is
   begin
      return U.To_String (Object.Version);
   end Version;

end Ada_Links.Files;

