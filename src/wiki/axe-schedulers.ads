with Ada.Calendar;
with Ada.Containers.Ordered_Sets;

package Axe.Schedulers is

   type Runable is limited interface;

   not overriding procedure Run (Self : aliased in out Runable) is abstract;

   procedure Start_If_Assigned (Self : access Runable'Class);

   type Runable_Access is access all Runable'Class with Storage_Size => 0;

   type Scheduled_Item is record
      Time  : Ada.Calendar.Time;
      Value : not null Runable_Access;
   end record;

   function Less (Left, Right : Scheduled_Item) return Boolean;

   package Scheduled_Item_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Scheduled_Item,
      "<"          => Less);

   protected Scheduler is
      procedure Insert (Value : Scheduled_Item);
      function Next (Default : Duration) return Ada.Calendar.Time;
      procedure Get_Ready (Value : out Runable_Access);
   private
      Set : Scheduled_Item_Sets.Set;
   end Scheduler;

end Axe.Schedulers;
