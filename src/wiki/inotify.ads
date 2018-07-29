with Ada.Streams;
with Interfaces.C;

package iNotify is
   use Interfaces;

   function inotify_init return C.int;
   pragma Import (C, inotify_init, "inotify_init");

   function inotify_add_watch
     (Queue : C.int;
      Path  : Ada.Streams.Stream_Element_Array;
      Mask  : Unsigned_32) return C.int;
   pragma Import (C, inotify_add_watch, "inotify_add_watch");

   --  the following are legal, implemented events that
   --  user-space can watch for

   IN_ACCESS         : constant Unsigned_32 := 16#0001#;
   IN_MODIFY         : constant Unsigned_32 := 16#0002#;
   IN_ATTRIB         : constant Unsigned_32 := 16#0004#;
   IN_CLOSE_WRITE    : constant Unsigned_32 := 16#0008#;
   IN_CLOSE_NO_WRITE : constant Unsigned_32 := 16#0010#;
   IN_OPEN           : constant Unsigned_32 := 16#0020#;
   IN_MOVED_FROM     : constant Unsigned_32 := 16#0040#;
   IN_MOVED_TO       : constant Unsigned_32 := 16#0080#;
   IN_CREATE         : constant Unsigned_32 := 16#0100#;
   IN_DELETE         : constant Unsigned_32 := 16#0200#;
   IN_DELETE_SELF    : constant Unsigned_32 := 16#0400#;
   IN_MOVE_SELF      : constant Unsigned_32 := 16#0800#;

   --  the following are legal events.  they are sent as needed to any watch
   IN_UNMOUNT    : constant Unsigned_32 := 16#2000#;
   IN_Q_OVERFLOW : constant Unsigned_32 := 16#4000#;
   IN_IGNORED    : constant Unsigned_32 := 16#8000#;

   IN_ONLY_DIR    : constant Unsigned_32 := 16#01000000#;
   IN_DONT_FOLLOW : constant Unsigned_32 := 16#02000000#;
   IN_EXCL_UNLINK : constant Unsigned_32 := 16#04000000#;

   IN_MASK_ADD : constant Unsigned_32 := 16#20000000#;
   IN_IS_DIR   : constant Unsigned_32 := 16#40000000#;
   IN_ONE_SHOT : constant Unsigned_32 := 16#80000000#;

   subtype Small_Size is Ada.Streams.Stream_Element_Offset range 1 .. 1024;

   subtype File_Name is Ada.Streams.Stream_Element_Array (Small_Size);

   type Event is record
      Watch  : C.int;
      Mask   : Unsigned_32;
      Cookie : Unsigned_32;
      Length : Unsigned_32;
      Name   : File_Name;
   end record;
   pragma Convention (C, Event);

   type Event_Array is array (Positive range <>) of Event
     with Convention => C;

   procedure perror (Prefix : C.char_array);
   pragma Import (C, perror, "perror");

   function read
     (File   : C.int;
      Buffer : out Event_Array;
      Length : C.size_t) return C.size_t;
   pragma Import (C, read, "read");

end iNotify;
