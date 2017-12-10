with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

with League.Strings.Hash;
with League.Calendars;
with SQL.Databases;

with Forum.Forums;
limited with Forum.Posts;
limited with Forum.Contexts;

package Forum.Topics is
   type Container (Context : access Forum.Contexts.Context) is
     tagged limited private;
   type Container_Access is access all Container;

   not overriding procedure Initiaize
     (Self   : in out Container;
      DB     : in out SQL.Databases.SQL_Database;
      Forums : in out Forum.Forums.Container;
      Posts  : Forum.Posts.Container);

   not overriding procedure Create_Topic
     (Self     : in out Container;
      Forums   : in out Forum.Forums.Container;
      Date     : League.Calendars.Date_Time;
      Starter  : League.Strings.Universal_String;
      Subject  : League.Strings.Universal_String);

   not overriding procedure Add_Post
     (Self    : in out Container;
      Date    : League.Calendars.Date_Time;
      Starter : League.Strings.Universal_String;
      Post    : League.Strings.Universal_String);

   not overriding procedure Sort_Posts (Self : in out Container);

   not overriding function Before
     (Self  : Container;
      Left  : League.Strings.Universal_String;
      Right : League.Strings.Universal_String) return Boolean;

private

   package String_Vector is new Ada.Containers.Vectors
     (Positive, League.Strings.Universal_String, League.Strings."=");

   type Topic is record
      Starter  : League.Strings.Universal_String;
      Forum    : Forum_Id;
      Subject  : League.Strings.Universal_String;
      --
      Date     : League.Calendars.Date_Time;
      Posts    : String_Vector.Vector;
   end record;

   package Topic_Maps is new Ada.Containers.Hashed_Maps
     (League.Strings.Universal_String,
      Topic,
      League.Strings.Hash,
      League.Strings."=");

   package Email_Maps is new Ada.Containers.Hashed_Maps
     (League.Strings.Universal_String,
      League.Strings.Universal_String,
      League.Strings.Hash,
      League.Strings."=",
      League.Strings."=");

   type Container (Context : access Forum.Contexts.Context) is
   tagged limited record
      Topic_Map    : Topic_Maps.Map;
   end record;

end Forum.Topics;
