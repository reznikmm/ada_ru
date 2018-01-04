with SQL.Queries.Internals;
with SQL.Databases.Internals;

package body Databases is

   ---------------
   -- Close_All --
   ---------------

   not overriding procedure Close_All (Self : in out SQL_Database_Pool) is
      use type Ada.Containers.Count_Type;
      Driver : Matreshka.Internals.SQL_Drivers.Database_Access;
   begin
      while Self.Queue.Current_Use > 0 loop
         Self.Queue.Dequeue (Driver);
         Matreshka.Internals.SQL_Drivers.Dereference (Driver);
      end loop;
   end Close_All;

   ------------
   -- Commit --
   ------------

   not overriding procedure Commit (Self : in out SQL_Database) is
   begin
      Self.Driver.Commit;
   end Commit;

   ------------
   -- Create --
   ------------

   function Create
     (Self : in out SQL_Database_Pool'Class) return SQL_Database
   is
      use type Matreshka.Internals.SQL_Drivers.Database_Access;

      Driver : Matreshka.Internals.SQL_Drivers.Database_Access;
   begin
      --  Try reuse existing session
      select
         Self.Queue.Dequeue (Driver);
      else
         null;
      end select;

      if Driver = null then
         declare
            DB : SQL.Databases.SQL_Database :=
              SQL.Databases.Create (Self.Driver, Self.Options);
         begin
            DB.Open;
            Driver := SQL.Databases.Internals.Internal (DB);
            Matreshka.Internals.SQL_Drivers.Reference (Driver);
         end;
      end if;

      return Result : SQL_Database do
         Result.Driver := Driver;
         Result.Pool := Self'Unchecked_Access;
      end return;
   end Create;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out SQL_Database) is
      use type Matreshka.Internals.SQL_Drivers.Database_Access;
   begin
      if Self.Driver /= null then
         Self.Pool.Queue.Enqueue (Self.Driver);
         Self.Driver := null;
      end if;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out SQL_Database_Pool) is
   begin
      Self.Close_All;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
    (Self    : out SQL_Database_Pool'Class;
     Driver  : League.Strings.Universal_String;
     Options : SQL.Options.SQL_Options) is
   begin
      Self.Driver := Driver;
      Self.Options := Options;
   end Initialize;

   not overriding function Query
     (Self : in out SQL_Database) return SQL.Queries.SQL_Query is
   begin
      return SQL.Queries.Internals.Wrap (Self.Driver.Query);
   end Query;

end Databases;
