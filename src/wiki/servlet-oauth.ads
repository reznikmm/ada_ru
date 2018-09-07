with League.Strings;

with Servlet.HTTP_Requests;
with Servlet.HTTP_Responses;
with Servlet.HTTP_Servlets;
with Sessions;

private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Hashed_Maps;
private with Ada.Numerics.Discrete_Random;
private with Ada.Streams;
private with League.Strings.Hash;
private with Servlet.Generic_Servlets;

package Servlet.OAuth is

   type Login_Handler is limited interface;

   not overriding procedure Do_Login
    (Self     : in out Login_Handler;
     Info     : Sessions.User_Info;
     Path     : League.Strings.Universal_String;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
       is abstract;

   type OAuth_Servlet is new Servlet.HTTP_Servlets.HTTP_Servlet with private;

   not overriding procedure Set_Handler
    (Self  : in out OAuth_Servlet;
     Value : access Login_Handler'Class);

   type OAuth_Servlet_Access is access all OAuth_Servlet'Class;

private

   type OAuth_Provider is record
      Client_Id       : League.Strings.Universal_String;
      Token_End_Point : League.Strings.Universal_String;
      Client_Secret   : League.Strings.Universal_String;
      Redirect_URI    : League.Strings.Universal_String;
      Token_Key       : League.Strings.Universal_String;
      Secure_Key      : League.Strings.Universal_String;
   end record;

   package OAuth_Provider_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => League.Strings.Universal_String,
      Element_Type    => OAuth_Provider,
      Hash            => League.Strings.Hash,
      Equivalent_Keys => League.Strings."=");

   package String_Lists is new Ada.Containers.Doubly_Linked_Lists
     (League.Strings.Universal_String, League.Strings."=");

   type State_Data is record
      State : League.Strings.Universal_String;
      --  State for given session
      Path  : League.Strings.Universal_String;
      --  Corresponding URI path to return after login
   end record;

   package State_Maps is new Ada.Containers.Hashed_Maps
     (League.Strings.Universal_String,
      State_Data,
      League.Strings.Hash,
      League.Strings."=");

   package Stream_Element_Random is new Ada.Numerics.Discrete_Random
     (Ada.Streams.Stream_Element);

   type State_Cache is tagged limited record
      Random : Stream_Element_Random.Generator;
      Queue  : String_Lists.List;
      --  List of session in authorization progress
      Map    : State_Maps.Map;
      --  Map from session to corresponding 'state' to protect against
      --  cross-site request forgery attacks. Also it keeps return path.
   end record;

   not overriding function Create_Key
     (Self        : in out State_Cache;
      Session_Id  : League.Strings.Universal_String;
      Return_Path : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   not overriding function Check_Key
     (Self       : in out State_Cache;
      Session_Id : League.Strings.Universal_String;
      Key        : League.Strings.Universal_String)
      return Boolean;

   not overriding function Get_Return_Path
     (Self       : in out State_Cache;
      Session_Id : League.Strings.Universal_String)
      return League.Strings.Universal_String;

   type OAuth_Servlet is new Servlet.HTTP_Servlets.HTTP_Servlet with record
      Handler         : access Login_Handler'Class;
      Cache           : State_Cache;
      OAuth_Providers : OAuth_Provider_Maps.Map;
   end record;

   overriding procedure Do_Get
    (Self     : in out OAuth_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   overriding function Get_Servlet_Info
    (Self : OAuth_Servlet) return League.Strings.Universal_String;

   overriding function Instantiate
    (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
         return OAuth_Servlet;

end Servlet.OAuth;
