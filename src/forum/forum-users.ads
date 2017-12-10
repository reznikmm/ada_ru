with Ada.Containers.Hashed_Maps;

with League.Strings.Hash;
with SQL.Databases;
with SQL.Queries;

limited with Forum.Contexts;

package Forum.Users is

   type Container (Context : access Forum.Contexts.Context) is
     tagged limited private;
   type Container_Access is access all Container;

   not overriding procedure Initiaize
     (Self : in out Container;
      DB   : in out SQL.Databases.SQL_Database);

   not overriding procedure Create_User
     (Self     : in out Container;
      Email    : League.Strings.Universal_String;
      Nickname : out League.Strings.Universal_String);

private

   type User is record
      Nickname : League.Strings.Universal_String;
      Name     : League.Strings.Universal_String;
      Avatar   : League.Strings.Universal_String;
   end record;

   package User_Maps is new Ada.Containers.Hashed_Maps
     (League.Strings.Universal_String,
      User,
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
      User_Map     : User_Maps.Map;
      Email_Map    : Email_Maps.Map;
      Insert_User  : SQL.Queries.SQL_Query;
      Insert_EMail : SQL.Queries.SQL_Query;
   end record;

end Forum.Users;
