with SQL.Databases;
with SQL.Options;

with Matreshka.Internals.SQL_Drivers.SQLite3.Factory;
with League.Application;

with Forum.Contexts;
with Forum.Posts;
with Forum.Topics;
with Forum.Users;
with Forum.Writers;

procedure Forum.Update is
   Root   : constant String :=
     League.Application.Arguments.Element (1).To_UTF_8_String;
   Option : SQL.Options.SQL_Options;
begin
   Option.Set (+"filename", +"mail.db");

   declare
      Context : aliased Forum.Contexts.Context;
      Top     : League.Holders.Holder;
      DB      : SQL.Databases.SQL_Database :=
        SQL.Databases.Create (+"SQLITE3", Option);
   begin
      DB.Open;
      Context.Users.Initiaize (DB);
      Context.Posts.Initiaize (Context.Users, DB);
      Context.Forums.Initiaize (DB);
      Context.Topics.Initiaize (DB, Context.Forums, Context.Posts);
      Context.Posts.Assign_Topics (Context.Forums, Context.Topics);
      Context.Forums.Sort_Topics;
      Context.Topics.Sort_Posts;
      Top := Context.Forums.To_Holder;
      Forum.Writers.Write_Forum_Index (Root, Top);

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
                  Writers.Write_Forum_Page (Root, Forum, Cursor.Element);
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
                    (Root, Forum, Topic, Cursor.Element);
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
