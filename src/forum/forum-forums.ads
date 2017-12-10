with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

with SQL.Databases;

limited with Forum.Contexts;

package Forum.Forums is
   type Container (Context : access Forum.Contexts.Context) is
     tagged limited private;

   not overriding procedure Initiaize
     (Self : in out Container;
      DB   : in out SQL.Databases.SQL_Database);

   not overriding procedure Add_Topic
     (Self  : in out Container;
      Forum : Forum_Id;
      Topic : League.Strings.Universal_String);

   not overriding procedure Sort_Topics (Self : in out Container);

   not overriding function To_Holder
     (Self  : aliased in out Container) return League.Holders.Holder;

private

   package String_Vector is new Ada.Containers.Vectors
     (Positive, League.Strings.Universal_String, League.Strings."=");

   type Forum is record
      Id       : Forum_Id;
      Sort_Key : Forum_Id;
      Subject  : League.Strings.Universal_String;

      Topics   : String_Vector.Vector;
   end record;

   function Hash (Value : Forum_Id) return Ada.Containers.Hash_Type;

   package Forum_Maps is new Ada.Containers.Hashed_Maps
     (Forum_Id,
      Forum,
      Hash,
      "=");

   type Container (Context : access Standard.Forum.Contexts.Context) is
   tagged limited record
      Forum_Map : Forum_Maps.Map;
   end record;

   type Container_Access is access all Container;

end Forum.Forums;
