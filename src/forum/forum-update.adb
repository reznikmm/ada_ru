with Ada.Streams.Stream_IO;
with Ada.Wide_Wide_Text_IO;
with SQL.Databases;
with SQL.Options;

with Matreshka.Internals.SQL_Drivers.PostgreSQL.Factory;
with League.Application;

with Forum.Contexts;
with Forum.Posts;
with Forum.Topics;
with Forum.Users;
with Forum.Writers;

procedure Forum.Update is
   procedure Write_File (Value : Writers.File_Information);

   Root : constant String :=
     League.Application.Arguments.Element (1).To_UTF_8_String;

   ----------------
   -- Write_File --
   ----------------

   procedure Write_File (Value : Writers.File_Information) is
      File : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Create
        (File, Name => Root & Value.Name.To_UTF_8_String);
      Ada.Streams.Stream_IO.Write (File, Value.Data.To_Stream_Element_Array);
      Ada.Streams.Stream_IO.Close (File);
   end Write_File;

   Output : Ada.Wide_Wide_Text_IO.File_Type;
   Option : SQL.Options.SQL_Options;
begin
   Option.Set (+"dbname", +"mail");
   Ada.Wide_Wide_Text_IO.Create (Output, Name => Root & "hashes.txt");

   declare
      Context : aliased Forum.Contexts.Context;
      Top     : League.Holders.Holder;
      Info    : Forum.Writers.File_Information;
      DB      : SQL.Databases.SQL_Database :=
        SQL.Databases.Create (+"POSTGRESQL", Option);
   begin
      DB.Open;
      Context.Users.Initiaize (DB);
      Context.Posts.Initiaize (Context.Users, DB);
      Context.Forums.Initiaize (DB);
      Context.Topics.Initiaize (DB, Context.Forums, Context.Posts);
      Context.Posts.Assign_Topics (Context.Forums, Context.Topics);
      Context.Forums.Sort_Topics;
      Context.Topics.Sort_Posts;
      Top := Context.Forums.Last_Topics_Holder;
      Forum.Writers.Write_Forum_Atom (Top, Info);
      Write_File (Info);
      Ada.Wide_Wide_Text_IO.Put_Line (Output, Writers.Image (Info));
      Top := Context.Forums.To_Holder;
      Forum.Writers.Write_Forum_Index (Top, Info);
      Write_File (Info);
      Ada.Wide_Wide_Text_IO.Put_Line (Output, Writers.Image (Info));

      declare
         procedure Each_Forum (Forum : League.Holders.Holder);
         procedure Each_Page
           (Forum : League.Holders.Holder;
            Page  : League.Holders.Holder);
         procedure Each_Topic
           (Forum : League.Holders.Holder;
            Topic : League.Holders.Holder);

         procedure Each_Forum (Forum : League.Holders.Holder) is
            Pages : League.Holders.Holder;
            Ok    : Boolean;
         begin
            League.Holders.Component (Forum, +"pages", Pages, Ok);

            declare
               Cursor : League.Holders.Iterable_Holder_Cursors.Cursor'Class :=
                 League.Holders.First (Pages);
            begin
               while Cursor.Next loop
                  Writers.Write_Forum_Page (Forum, Cursor.Element, Info);
                  Write_File (Info);
                  Ada.Wide_Wide_Text_IO.Put_Line
                    (Output, Writers.Image (Info));
                  Each_Page (Forum, Cursor.Element);
               end loop;
            end;
         end Each_Forum;

         procedure Each_Page
           (Forum : League.Holders.Holder;
            Page  : League.Holders.Holder)
         is
            Topics : League.Holders.Holder;
            Ok     : Boolean;
         begin
            League.Holders.Component (Page, +"topics", Topics, Ok);

            declare
               Cursor : League.Holders.Iterable_Holder_Cursors.Cursor'Class :=
                 League.Holders.First (Topics);
            begin
               while Cursor.Next loop
                  Each_Topic (Forum, Cursor.Element);
               end loop;
            end;
         end Each_Page;

         procedure Each_Topic
           (Forum : League.Holders.Holder;
            Topic : League.Holders.Holder)
         is
            Pages : League.Holders.Holder;
            Ok    : Boolean;
         begin
            League.Holders.Component (Topic, +"pages", Pages, Ok);

            declare
               Cursor : League.Holders.Iterable_Holder_Cursors.Cursor'Class :=
                 League.Holders.First (Pages);
            begin
               while Cursor.Next loop
                  Writers.Write_Topic_Page
                    (Forum, Topic, Cursor.Element, Info);
                  Write_File (Info);
                  Ada.Wide_Wide_Text_IO.Put_Line
                    (Output, Writers.Image (Info));
               end loop;
            end;
         end Each_Topic;

         Cursor : League.Holders.Iterable_Holder_Cursors.Cursor'Class :=
           League.Holders.First (Top);
      begin
         while Cursor.Next loop
            Each_Forum (Cursor.Element);
         end loop;
      end;
   end;
end Forum.Update;
