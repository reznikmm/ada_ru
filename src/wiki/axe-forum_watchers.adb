with iNotify; use iNotify;
with League.Text_Codecs;
with Ada.Streams;

package body Axe.Forum_Watchers is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Forum_Watcher'Class;
      Name : League.Strings.Universal_String;
      File : out GNAT.Sockets.Socket_Type)
   is
      use type Ada.Streams.Stream_Element_Array;

      File_Name : constant Ada.Streams.Stream_Element_Array :=
        League.Text_Codecs.Codec_For_Application_Locale
          .Encode (Name).To_Stream_Element_Array & 0;
   begin
      Self.I_Notify_Queue := inotify_init;
      Self.Watch_Descriptor :=
        inotify_add_watch (Self.I_Notify_Queue, File_Name, IN_CLOSE_WRITE);
      File := GNAT.Sockets.To_Ada (Integer (Self.I_Notify_Queue));
   end Initialize;

   -------------
   -- Refresh --
   -------------

   not overriding procedure Refresh
     (Self : in out Forum_Watcher;
      Text : out League.Strings.Universal_String)
   is
      use type Interfaces.C.int;
      use type Interfaces.C.size_t;

      Input : Event_Array (1 .. 1) := (others => <>);
      Count : Interfaces.C.size_t;
   begin
      Count := read (Self.I_Notify_Queue, Input, Input'Size / 8);

      if Count > 0 and then Input (1).Watch = Self.Watch_Descriptor then
         null;
      end if;

      Text.Clear;
   end Refresh;

end Axe.Forum_Watchers;
