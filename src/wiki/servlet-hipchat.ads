with Ada.Containers.Hashed_Maps;
with League.Strings;
with Axe.Events;

with Servlet.HTTP_Requests;
with Servlet.HTTP_Responses;
with Servlet.HTTP_Servlets;

private with Servlet.Generic_Servlets;
private with League.Calendars;
private with League.Strings.Hash;

package Servlet.Hipchat is

   type Hipchat_Servlet is
     new Servlet.HTTP_Servlets.HTTP_Servlet with private;

   procedure Initialize (Self  : in out Hipchat_Servlet'Class);

   not overriding procedure Set_Listener
    (Self  : in out Hipchat_Servlet;
     Value : access Axe.Events.Listener'Class);

   type Hipchat_Servlet_Access is access all Hipchat_Servlet'Class;

private

   type Installation is record
      OAuth_Id         : League.Strings.Universal_String;
      Capabilities_URL : League.Strings.Universal_String;
      Room_Id          : Natural;
      Group_Id         : Natural;
      OAuth_Secret     : League.Strings.Universal_String;
   end record;

   package Installation_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => League.Strings.Universal_String,
      Element_Type    => Installation,
      Hash            => League.Strings.Hash,
      Equivalent_Keys => League.Strings."=");

   type Capability is record
      Token_URL : League.Strings.Universal_String;
      API_URL   : League.Strings.Universal_String;
   end record;

   package Capability_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => League.Strings.Universal_String,
      Element_Type    => Capability,
      Hash            => League.Strings.Hash,
      Equivalent_Keys => League.Strings."=");

   type Access_Token is record
      Token   : League.Strings.Universal_String;
      Expires : League.Calendars.Date_Time;
   end record;

   package Access_Token_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => League.Strings.Universal_String,
      Element_Type    => Access_Token,
      Hash            => League.Strings.Hash,
      Equivalent_Keys => League.Strings."=");

   type Hipchat_Servlet is new Servlet.HTTP_Servlets.HTTP_Servlet with record
      Installations  : Installation_Maps.Map;
      Capabilities   : Capability_Maps.Map;
      Access_Tokens  : Access_Token_Maps.Map;
      Listener       : access Axe.Events.Listener'Class;
   end record;

   overriding procedure Do_Get
    (Self     : in out Hipchat_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   overriding procedure Do_Post
    (Self     : in out Hipchat_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   overriding function Get_Servlet_Info
    (Self : Hipchat_Servlet) return League.Strings.Universal_String;

   overriding function Instantiate
    (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
         return Hipchat_Servlet;

end Servlet.Hipchat;
