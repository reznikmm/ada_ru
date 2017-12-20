with League.Base_Codecs;
with League.Stream_Element_Vectors;

package body Sessions.Managers is

   --------------
   -- Do_Login --
   --------------

   overriding procedure Do_Login
    (Self     : in out HTTP_Session_Manager;
     Info     : Servlet.OAuth.User_Info;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      pragma Unreferenced (Self);

      function "+" (Text : Wide_Wide_String)
        return League.Strings.Universal_String
          renames League.Strings.To_Universal_String;

      Session : Sessions.HTTP_Session renames
        Sessions.HTTP_Session (Request.Get_Session.all);
   begin
      Session.Info := Info;

      Response.Set_Status (Servlet.HTTP_Responses.See_Other);
      Response.Set_Header (+"Location", +"/");
      Response.Set_Header (+"Cache-Control", +"must-revalidate");
   end Do_Login;

   -----------------
   -- Get_Session --
   -----------------

   overriding function Get_Session
     (Self       : in out HTTP_Session_Manager;
      Identifier : League.Strings.Universal_String)
      return access Servlet.HTTP_Sessions.HTTP_Session'Class
   is
   begin
      if Self.Map.Contains (Identifier) then
         return Self.Map.Element (Identifier);
      else
         return null;
      end if;
   end Get_Session;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out HTTP_Session_Manager) is
   begin
      Self.Map.Clear;
      Stream_Element_Random.Reset (Self.Random);
   end Initialize;

   ---------------------------------
   -- Is_Session_Identifier_Valid --
   ---------------------------------

   overriding function Is_Session_Identifier_Valid
     (Self       : HTTP_Session_Manager;
      Identifier : League.Strings.Universal_String)
      return Boolean
   is
   begin
      return Self.Map.Contains (Identifier);
   end Is_Session_Identifier_Valid;

   -----------------
   -- New_Session --
   -----------------

   overriding function New_Session
     (Self : in out HTTP_Session_Manager)
      return access Servlet.HTTP_Sessions.HTTP_Session'Class
   is
      Data   : League.Stream_Element_Vectors.Stream_Element_Vector;
      New_Id : League.Strings.Universal_String;
      Result : Session_Access;
   begin
      for J  in 1 .. 12 loop
         Data.Append (Stream_Element_Random.Random (Self.Random));
      end loop;

      New_Id := League.Base_Codecs.To_Base_64 (Data);

      Result := new Sessions.HTTP_Session'
        (Servlet.HTTP_Sessions.HTTP_Session with Id => New_Id, others => <>);

      Self.Map.Insert (New_Id, Result);

      return Result;
   end New_Session;

end Sessions.Managers;
