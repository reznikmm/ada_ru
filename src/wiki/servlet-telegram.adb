with Ada.Streams;
with Ada.Wide_Wide_Text_IO;

with League.JSON.Arrays;
with League.JSON.Documents;
with League.JSON.Values;
with League.Stream_Element_Vectors;
with League.Text_Codecs;

package body Servlet.Telegram is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;

   procedure To_JSON
     (Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Document : out League.JSON.Documents.JSON_Document);

   procedure Delete_Message
     (Message  : League.JSON.Objects.JSON_Object;
      Response : out League.JSON.Objects.JSON_Object);

   -----------------------
   --  String Constants --
   -----------------------

   chat : constant League.Strings.Universal_String := +"chat";
   chat_id : constant League.Strings.Universal_String := +"chat_id";
   entities : constant League.Strings.Universal_String := +"entities";
   forward_from : constant League.Strings.Universal_String := +"forward_from";
   forward_from_chat : constant League.Strings.Universal_String :=
     +"forward_from_chat";
   forward_from_msg : constant League.Strings.Universal_String :=
     +"forward_from_message_id";
   from : constant League.Strings.Universal_String := +"from";
   id : constant League.Strings.Universal_String := +"id";
   left_chat_member : constant League.Strings.Universal_String :=
     +"left_chat_member";
   new_chat_members : constant League.Strings.Universal_String :=
     +"new_chat_members";
   mention : constant League.Strings.Universal_String := +"mention";
   message_id : constant League.Strings.Universal_String := +"message_id";
   method : constant League.Strings.Universal_String := +"method";
   hashtag  : constant League.Strings.Universal_String :=
     +"hashtag";
   url  : constant League.Strings.Universal_String := +"url";
   text_link  : constant League.Strings.Universal_String := +"text_link";
   text_mention : constant League.Strings.Universal_String := +"text_mention";

   ---------------------
   -- Analyze_Message --
   ---------------------

   not overriding procedure Analyze_Message
    (Self     : in out Telegram_Servlet;
     Message  : League.JSON.Objects.JSON_Object;
     Result   : out Message_Action)
   is
      function Has_Bad_Enity (Value : League.JSON.Values.JSON_Value)
        return Boolean;

      -------------
      -- Has_Bad_Enity --
      -------------

      function Has_Bad_Enity (Value : League.JSON.Values.JSON_Value)
        return Boolean is
      begin
         if Value.Is_Array then
            declare
               List : constant League.JSON.Arrays.JSON_Array :=
                 Value.To_Array;
            begin
               for J in 1 .. List.Length loop
                  declare
                     Entity : constant League.JSON.Objects.JSON_Object :=
                       List (J).To_Object;
                     Kind : constant League.Strings.Universal_String :=
                       Entity.Value (+"type").To_String;
                  begin
                     if Kind in
                       mention | hashtag | url | text_link | text_mention
                     then
                        return True;
                     end if;
                  end;
               end loop;

               return False;
            end;
         else
            return False;
         end if;
      end Has_Bad_Enity;

      Entities_Value : constant League.JSON.Values.JSON_Value :=
        Message.Value (entities);
      From_Value : constant League.JSON.Values.JSON_Value :=
        Message.Value (from);
      New_Members_Value : constant League.JSON.Values.JSON_Value :=
        Message.Value (new_chat_members);
   begin
      Result := Pass;

      if New_Members_Value.Is_Array then
         declare
            List : constant League.JSON.Arrays.JSON_Array :=
              New_Members_Value.To_Array;
         begin
            for J in 1 .. List.Length loop
               declare
                  User    : constant League.JSON.Objects.JSON_Object :=
                    List.Element (J).To_Object;
                  User_Id : constant User_Identifier :=
                    User.Value (id).To_Integer;
               begin
                  Self.New_Users.Include (User_Id);
               end;
            end loop;

            Result := Skip;
         end;
      elsif Message.Contains (left_chat_member) then
            Result := Skip;
      elsif From_Value.Is_Object then
         declare
            User : constant League.JSON.Objects.JSON_Object :=
              From_Value.To_Object;

            User_Id : constant User_Identifier := User.Value (id).To_Integer;

            Bad_Message : constant Boolean :=
              Message.Contains (forward_from) or
              Message.Contains (forward_from_chat) or
              Message.Contains (forward_from_msg) or
              Has_Bad_Enity (Entities_Value);
         begin
            if Self.New_Users.Contains (User_Id) then
               if Bad_Message then
                  Result := Delete;
               else
                  Self.New_Users.Delete (User_Id);
               end if;
            end if;
         end;
      end if;
   end Analyze_Message;

   --------------------
   -- Delete_Message --
   --------------------

   procedure Delete_Message
     (Message : League.JSON.Objects.JSON_Object;
      Response : out League.JSON.Objects.JSON_Object) is
   begin
      Response.Insert
        (method,
         League.JSON.Values.To_JSON_Value (+"deleteMessage"));
      Response.Insert (chat_id, Message.Value (chat).To_Object.Value (id));
      Response.Insert (message_id, Message.Value (message_id));
   end Delete_Message;

   -------------
   -- Do_Post --
   -------------

   overriding procedure Do_Post
     (Self     : in out Telegram_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      Document : League.JSON.Documents.JSON_Document;
      Object   : League.JSON.Objects.JSON_Object;
      Result   : League.JSON.Objects.JSON_Object;
      Action   : Message_Action;
   begin
      To_JSON (Request, Document);
      Object := Document.To_JSON_Object;

      if Object.Contains (+"message") then
         Object := Object.Value (+"message").To_Object;
         Self.Analyze_Message (Object, Action);

         case Action is
            when Pass =>
               Self.Listener.On_Telegram (Object, Result);
            when Skip =>
               null;
            when Delete =>
               Delete_Message (Object, Result);
         end case;
      end if;

      Response.Set_Status (Servlet.HTTP_Responses.OK);
      Response.Set_Content_Type (+"application/json");
      Response.Set_Character_Encoding (+"utf-8");

      if Result.Is_Empty then
         Response.Get_Output_Stream.Write (+"{}");
      else
         Document := Result.To_JSON_Document;
         Response.Get_Output_Stream.Write (Document.To_JSON);
      end if;
   end Do_Post;

   ----------------------
   -- Get_Servlet_Info --
   ----------------------

   overriding function Get_Servlet_Info
     (Self : Telegram_Servlet)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return +"Telegram Servlet";
   end Get_Servlet_Info;

   ----------
   -- Hash --
   ----------

   function Hash (Value : User_Identifier) return Ada.Containers.Hash_Type is
      use type User_Identifier;
   begin
      return Ada.Containers.Hash_Type'Val (abs Value);
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : in out Telegram_Servlet'Class;
      Token : League.Strings.Universal_String) is
   begin
      Self.Token := Token;
   end Initialize;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
      return Telegram_Servlet
   is
      pragma Unreferenced (Parameters);
   begin
      return Result : Telegram_Servlet;
   end Instantiate;

   ------------------
   -- Set_Listener --
   ------------------

   not overriding procedure Set_Listener
     (Self  : in out Telegram_Servlet;
      Value : access Axe.Events.Listener'Class)
   is
   begin
      Self.Listener := Value;
   end Set_Listener;

   -------------
   -- To_JSON --
   -------------

   procedure To_JSON
     (Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Document : out League.JSON.Documents.JSON_Document)
   is
      use type Ada.Streams.Stream_Element_Count;
      Stream : constant access Ada.Streams.Root_Stream_Type'Class :=
        Request.Get_Input_Stream;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 512);
      Vector : League.Stream_Element_Vectors.Stream_Element_Vector;
      Last   : Ada.Streams.Stream_Element_Count;
   begin
      loop
         Stream.Read (Buffer, Last);
         exit when Last = 0;
         Vector.Append (Buffer (1 .. Last));
      end loop;

      Ada.Wide_Wide_Text_IO.Put_Line
        (League.Text_Codecs.Codec_For_Application_Locale.
           Decode (Vector).To_Wide_Wide_String);

      Document := League.JSON.Documents.From_JSON (Vector);
   end To_JSON;

end Servlet.Telegram;
