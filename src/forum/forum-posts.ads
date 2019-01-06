with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;

with League.Calendars;
with League.Strings.Hash;
with SQL.Databases;

limited with Forum.Contexts;
with Forum.Forums;
with Forum.Topics;
with Forum.Users;

package Forum.Posts is

   type Container (Context : access Forum.Contexts.Context) is
     tagged limited private;
   type Container_Access is access all Container;

   not overriding procedure Initiaize
     (Self  : in out Container;
      Users : in out Forum.Users.Container;
      DB    : in out SQL.Databases.SQL_Database);

   not overriding procedure Assign_Topics
     (Self   : in out Container;
      Forums : in out Forum.Forums.Container;
      Topics : in out Forum.Topics.Container);

   not overriding function Post_Date
     (Self : Container;
      Id   : League.Strings.Universal_String)
        return League.Calendars.Date_Time;

   not overriding function Before
     (Self  : Container;
      Left  : League.Strings.Universal_String;
      Right : League.Strings.Universal_String) return Boolean;

   type Paragraph is record
      Text  : League.Strings.Universal_String;
      Quote : Natural := 0;
   end record;

   package Paragraph_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Paragraph);

private

   type Post is record
      Id       : League.Strings.Universal_String;
      From     : League.Strings.Universal_String;
      Date     : League.Calendars.Date_Time;
      Parent   : League.Strings.Universal_String;
      Subject  : League.Strings.Universal_String;
      Para     : Paragraph_Lists.List;
      --
      Nickname : League.Strings.Universal_String;
      Topic    : League.Strings.Universal_String;
   end record;

   package Post_Maps is new Ada.Containers.Hashed_Maps
     (League.Strings.Universal_String,
      Post,
      League.Strings.Hash,
      League.Strings."=");

   type Container (Context : access Forum.Contexts.Context) is
   tagged limited record
      Post_Map  : Post_Maps.Map;
   end record;

end Forum.Posts;
