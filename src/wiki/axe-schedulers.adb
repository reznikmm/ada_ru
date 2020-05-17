with System.Address_To_Access_Conversions;
with System.Storage_Elements;

package body Axe.Schedulers is

   ----------
   -- Less --
   ----------

   function Less (Left, Right : Scheduled_Item) return Boolean is
      use type Ada.Calendar.Time;
      use type System.Storage_Elements.Integer_Address;

      package Conv is new System.Address_To_Access_Conversions
        (Object => Runable'Class);

      function "+" (Addr : System.Address)
        return System.Storage_Elements.Integer_Address
          renames System.Storage_Elements.To_Integer;
   begin
      if Left.Time < Right.Time then
         return True;
      elsif Left.Time > Right.Time then
         return False;
      end if;

      return +Conv.To_Address (Conv.Object_Pointer (Left.Value)) <
        +Conv.To_Address (Conv.Object_Pointer (Right.Value));
   end Less;

   -----------------------
   -- Start_If_Assigned --
   -----------------------

   procedure Start_If_Assigned (Self : access Runable'Class) is
   begin
      if Self /= null then
         Self.Run;
      end if;
   end Start_If_Assigned;

   ---------------
   -- Scheduler --
   ---------------

   protected body Scheduler is

      ---------------
      -- Get_Ready --
      ---------------

      procedure Get_Ready (Value : out Runable_Access) is
      begin
         if Set.Is_Empty then
            Value := null;
         else
            Value := Set.First_Element.Value;
            Set.Delete_First;
         end if;
      end Get_Ready;

      ------------
      -- Insert --
      ------------

      procedure Insert (Value : Scheduled_Item) is
      begin
         Set.Include (Value);
      end Insert;

      ----------
      -- Next --
      ----------

      function Next (Default : Duration) return Ada.Calendar.Time is
         use type Ada.Calendar.Time;
      begin
         if Set.Is_Empty then
            return Ada.Calendar.Clock + Default;
         else
            return Set.First_Element.Time;
         end if;
      end Next;

   end Scheduler;

end Axe.Schedulers;
