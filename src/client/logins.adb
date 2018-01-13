with League.Strings;

with WebAPI.HTML.Globals;          use WebAPI.HTML.Globals;
with WebAPI.HTML.Input_Elements;
with WebAPI.DOM.Event_Listeners;
with WebAPI.DOM.Event_Targets;
with WebAPI.DOM.Events;
with WebAPI.XHR.Requests;

package body Logins is
   function "+"
    (Item : Wide_Wide_String) return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   type Response_Listener is new WebAPI.DOM.Event_Listeners.Event_Listener
   with record
      Request  : WebAPI.XHR.Requests.XML_Http_Request_Access;
   end record;

   type Response_Listener_Access is access all Response_Listener'Class;

   overriding procedure Handle_Event
    (Self  : not null access Response_Listener;
     Event : access WebAPI.DOM.Events.Event'Class);

   type User_Info is record
      User   : League.Strings.Universal_String;
      Name   : League.Strings.Universal_String;
      Avatar : League.Strings.Universal_String;
--      Mails  : League.String_Vectors.Universal_String_Vector;
   end record;

   ------------------
   -- Handle_Event --
   ------------------

   overriding procedure Handle_Event
    (Self  : not null access Response_Listener;
     Event : access WebAPI.DOM.Events.Event'Class)
   is
      pragma Unreferenced (Event);

      function JSON_parse
        (Text : League.Strings.Universal_String)
         return User_Info
           with Import => True,
                Convention => JavaScript_Function,
                Link_Name  => "JSON.parse";

      Known_User  : constant
        WebAPI.HTML.Input_Elements.HTML_Input_Element_Access :=
          WebAPI.HTML.Input_Elements.HTML_Input_Element_Access
            (Window.Get_Document.Get_Element_By_Id (+"known_user"));

      Info : User_Info;
   begin
      if Self.Request.Get_Ready_State = 4 then
         if Self.Request.Get_Status = 200 then
            Info := JSON_parse (Self.Request.Get_Response_Text);
            Known_User.Set_Checked (not Info.User.Is_Empty);
         end if;
      end if;
   end Handle_Event;

   -----------
   -- Start --
   -----------

   procedure Start is
      Request : WebAPI.XHR.Requests.XML_Http_Request_Access :=
        new WebAPI.XHR.Requests.XML_Http_Request;
      Listener : Response_Listener_Access := new Response_Listener;
   begin
      Listener.Request := Request;

      WebAPI.DOM.Event_Targets.Add_Event_Listener
        (Request, +"readystatechange", Listener, False);

      Request.Open
        (Method   => +"GET",
         URL      => +"/user",
         Async    => True);

      Request.Set_Response_Type (+"text");
      Request.Send;
   end Start;

end Logins;
