with League.Holders;

package body Forum.Users is

   -----------------
   -- Create_User --
   -----------------

   not overriding procedure Create_User
     (Self     : in out Container;
      Email    : League.Strings.Universal_String;
      Nickname : out League.Strings.Universal_String)
   is
      First  : constant Natural := Email.Index ('<');
      Value  : League.Strings.Universal_String;
      Name   : League.Strings.Universal_String;
      Cursor : Email_Maps.Cursor;
   begin
      if First > 0 and Email.Ends_With (">") then
         Value := Email.Slice (First + 1, Email.Length - 1);
      else
         Value := Email;
      end if;

      Cursor := Self.Email_Map.Find (Value);

      if Email_Maps.Has_Element (Cursor) then
         Nickname := Email_Maps.Element (Cursor);
         return;
      else
         Nickname := Value.Head (Value.Index ('@') - 1);
      end if;

      if not Self.User_Map.Contains (Nickname) then
         if First > 0 then
            if Email.Starts_With ("""") then
               Name := Email.Slice (2, Email.Index (2, '"') - 1);
            elsif First > 1 then
               Name := Email.Slice (1, First - 2);
            end if;
         end if;

         if Name.Is_Empty then
            Name := Nickname;
         end if;

         Self.Insert_User.Bind_Value
           (+":n", League.Holders.To_Holder (Nickname));
         Self.Insert_User.Bind_Value (+":f", League.Holders.To_Holder (Name));
         Self.Insert_User.Bind_Value (+":a", League.Holders.To_Holder (+""));
         Self.Insert_User.Execute;
         Self.Insert_User.Finish;

         Self.User_Map.Insert (Nickname, User'(Nickname, Name, +""));
      end if;

      Self.Insert_EMail.Bind_Value (+":e", League.Holders.To_Holder (Value));
      Self.Insert_EMail.Bind_Value
        (+":n", League.Holders.To_Holder (Nickname));
      Self.Insert_EMail.Execute;
      Self.Insert_EMail.Finish;
      Self.Email_Map.Insert (Value, Nickname);
   end Create_User;

   ---------------
   -- Initiaize --
   ---------------

   not overriding procedure Initiaize
     (Self : in out Container;
      DB   : in out SQL.Databases.SQL_Database) is
   begin
      declare
         Query : SQL.Queries.SQL_Query := DB.Query
           (+"select nickname, name, avatar from users");
      begin
         Query.Execute;

         while Query.Next loop
            declare
               Object : constant User :=
                 User'(Nickname => League.Holders.Element (Query.Value (1)),
                       Name     => League.Holders.Element (Query.Value (2)),
                       Avatar   => To_String (Query.Value (3)));
            begin
               Self.User_Map.Insert (Object.Nickname, Object);
            end;
         end loop;
      end;

      declare
         Query : SQL.Queries.SQL_Query := DB.Query
           (+"select email, nickname from emails");
      begin
         Query.Execute;

         while Query.Next loop
            declare
               Email    : constant League.Strings.Universal_String :=
                 League.Holders.Element (Query.Value (1));
               Nickname : constant League.Strings.Universal_String :=
                 League.Holders.Element (Query.Value (2));
            begin
               if not Self.User_Map.Contains (Nickname) then
                  raise Constraint_Error with "No such nickname:" &
                    Nickname.To_UTF_8_String;
               end if;

               Self.Email_Map.Insert (Email, Nickname);
            end;
         end loop;
      end;

      Self.Insert_User := DB.Query
        (+"insert into users (nickname, name, avatar) values (:n,:f,:a)");
      Self.Insert_EMail := DB.Query
        (+"insert into emails (email, nickname,main) values (:e,:n,null)");
   end Initiaize;

end Forum.Users;
