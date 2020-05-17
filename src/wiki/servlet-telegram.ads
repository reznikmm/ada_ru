with Ada.Numerics.Discrete_Random;
with Ada.Containers.Hashed_Maps;
with Interfaces;

with League.Calendars;
with League.Holders;
with League.JSON.Objects;
with League.Strings;
with Axe.Events;

with Servlet.HTTP_Requests;
with Servlet.HTTP_Responses;
with Servlet.HTTP_Servlets;

private with Servlet.Generic_Servlets;

package Servlet.Telegram is

   type Telegram_Servlet is
     new Servlet.HTTP_Servlets.HTTP_Servlet with private;

   procedure Initialize
     (Self  : in out Telegram_Servlet'Class;
      Token : League.Strings.Universal_String);

   not overriding procedure Set_Listener
    (Self  : in out Telegram_Servlet;
     Value : access Axe.Events.Listener'Class);

   type Telegram_Servlet_Access is access all Telegram_Servlet'Class;

private

   subtype Chat_Identifier is League.Holders.Universal_Integer;
   subtype User_Identifier is League.Holders.Universal_Integer;
   subtype Message_Identifier is League.Holders.Universal_Integer;
   function Hash (Value : User_Identifier) return Ada.Containers.Hash_Type;

   type Message_Id_Array is array (1 .. 4) of Message_Identifier;

   package Natural_Random is new Ada.Numerics.Discrete_Random (Natural);

   type Greeting is record
      Time     : League.Calendars.Date_Time;
      Good     : Natural;
      Chat     : Chat_Identifier;
      Messages : Message_Id_Array;
   end record;

   package User_Sets is new Ada.Containers.Hashed_Maps
     (Key_Type        => User_Identifier,
      Element_Type    => Greeting,
      Hash            => Hash,
      Equivalent_Keys => Interfaces."=");

   protected type Newcomers is
      procedure Reset;

      procedure Add_User
        (Id      : User_Identifier;
         Chat    : Chat_Identifier;
         Message : League.Holders.Universal_Integer;
         Good    : out Natural);

      procedure Delete_User
        (Id    : User_Identifier;
         Value : out Greeting);

      procedure Get_Random
        (Id    : User_Identifier;
         Value : out Natural);

      procedure New_Reply
        (Id       : Message_Identifier;
         Reply_To : Message_Identifier);

      function Contains (User_Id : User_Identifier) return Boolean;
   private
      Map    : User_Sets.Map;
      Random : Natural_Random.Generator;
   end Newcomers;

   type Telegram_Servlet is new Servlet.HTTP_Servlets.HTTP_Servlet with record
      Token     : League.Strings.Universal_String;
      Listener  : access Axe.Events.Listener'Class;
      New_Users : Newcomers;
   end record;

   type Message_Action is (Pass, Skip, Delete);

   not overriding procedure Analyze_Message
    (Self     : in out Telegram_Servlet;
     Message  : League.JSON.Objects.JSON_Object;
     Result   : out Message_Action);

   overriding procedure Do_Post
    (Self     : in out Telegram_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   overriding function Get_Servlet_Info
    (Self : Telegram_Servlet) return League.Strings.Universal_String;

   overriding function Instantiate
    (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
         return Telegram_Servlet;

end Servlet.Telegram;
