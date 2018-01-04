with Ada.Finalization;

with League.Strings;

with SQL.Options;
with SQL.Queries;

private with Ada.Containers.Synchronized_Queue_Interfaces;
private with Ada.Containers.Unbounded_Synchronized_Queues;
private with Matreshka.Internals.SQL_Drivers;

package Databases is

   type SQL_Database (<>) is tagged limited private;

   not overriding function Query
    (Self : in out SQL_Database) return SQL.Queries.SQL_Query;

   not overriding procedure Commit (Self : in out SQL_Database);

   type SQL_Database_Pool is tagged limited private;

   procedure Initialize
    (Self    : out SQL_Database_Pool'Class;
     Driver  : League.Strings.Universal_String;
     Options : SQL.Options.SQL_Options);
   --  Initialize the database pool.

   function Create
    (Self : in out SQL_Database_Pool'Class) return SQL_Database;
   --  Return new or reused session. Note that result is already openned.

   not overriding procedure Close_All (Self : in out SQL_Database_Pool);
   --  Close all databases in the pool.

private

   type SQL_Database is new Ada.Finalization.Limited_Controlled with record
      Driver : Matreshka.Internals.SQL_Drivers.Database_Access;
      Pool   : access SQL_Database_Pool;
   end record;

   overriding procedure Finalize (Self : in out SQL_Database);

   package Queue_Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces
     (Matreshka.Internals.SQL_Drivers.Database_Access);

   package Synchronized_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues
       (Queue_Interfaces);

   type SQL_Database_Pool is new Ada.Finalization.Limited_Controlled with
   record
      Driver  : League.Strings.Universal_String;
      Options : SQL.Options.SQL_Options;
      Queue   : Synchronized_Queues.Queue;
   end record;

   overriding procedure Finalize (Self : in out SQL_Database_Pool);

end Databases;
