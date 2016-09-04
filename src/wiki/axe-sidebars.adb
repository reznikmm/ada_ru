------------------------------------------------------------------------------
--  Copyright © 2016, Maxim Reznik <reznikmm@gmail.com>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--     * this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--     * notice, this list of conditions and the following disclaimer in the
--     * documentation and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
------------------------------------------------------------------------------
--  $Date:$
------------------------------------------------------------------------------
--  This package supports navigation menu
------------------------------------------------------------------------------

with League.Regexps;
with Ada.Unchecked_Deallocation;
with Axe.Wiki.Parser;
with XML.SAX.Attributes;

package body Axe.Sidebars is

   use type League.Strings.Universal_String;

   package U renames League.Strings;

   function "+" (Text : Wide_Wide_String) return U.Universal_String
     renames League.Strings.To_Universal_String;

   A      : constant U.Universal_String := +"a";
   UL     : constant U.Universal_String := +"ul";
   LI     : constant U.Universal_String := +"li";
   HREF   : constant U.Universal_String := +"href";
   CLASS  : constant U.Universal_String := +"class";
   XHTML  : constant U.Universal_String := +"http://www.w3.org/1999/xhtml";

   ------------
   -- Expand --
   ------------

   procedure Expand
     (Self            : Sidebar;
      Writer          : access XML.SAX.Writers.SAX_Writer'Class;
      URI             : League.Strings.Universal_String;
      Wiki_URI_Prefix : Wide_Wide_String)
   is

      function Find
        (Tree : Item_Access;
         Name : League.Strings.Universal_String) return Boolean;

      procedure Expand (Tree : Item_Access);

      ------------
      -- Expand --
      ------------

      procedure Expand (Tree : Item_Access) is

         procedure Output (Next   : Item_Access);

         ------------
         -- Output --
         ------------

         procedure Output (Next   : Item_Access) is
            Attributes     : XML.SAX.Attributes.SAX_Attributes
              := XML.SAX.Attributes.Empty_SAX_Attributes;

            function To_Class return League.Strings.Universal_String;

            --------------
            -- To_Class --
            --------------

            function To_Class return League.Strings.Universal_String is
               Result : League.Strings.Universal_String;
            begin
               if Next.Down = null then
                  Result := +"empty";
               elsif Find (Next, URI) then
                  Result := +"open";
               else
                  Result := +"closed";
               end if;

               case Next.Kind is
                  when Added =>
                     Result.Append (+" added");
                  when Changed =>
                     Result.Append (+" changed");
                  when Normal =>
                     null;
               end case;

               if Wiki_URI_Prefix & Next.Name = URI then
                  Result.Append (+" pointer");
               end if;

               return Result;
            end To_Class;

         begin
            Attributes.Set_Value (CLASS, To_Class);
            Writer.Start_Element (XHTML, LI, LI, Attributes);

            Attributes := XML.SAX.Attributes.Empty_SAX_Attributes;

            Attributes.Set_Value (HREF, Wiki_URI_Prefix & Next.Name);
            Writer.Start_Element (XHTML, A, A, Attributes);

            Writer.Characters (Next.Title);

            Writer.End_Element (XHTML, A, A);
         end Output;

         Next   : Item_Access;

      begin
         if Tree.Up = null or else Find (Tree, URI) then
            Next := Tree.Down;

            if Next /= null then
               Writer.Start_Element
                 (XHTML, UL, UL, XML.SAX.Attributes.Empty_SAX_Attributes);

               while Next /= null loop
                  Output (Next);  --  Start_Element LI
                  Expand (Next);
                  Writer.End_Element (XHTML, LI, LI);
                  Next := Next.Next;
               end loop;

               Writer.End_Element (XHTML, UL, UL);
            end if;
         end if;
      end Expand;

      ----------
      -- Find --
      ----------

      function Find
        (Tree : Item_Access;
         Name : League.Strings.Universal_String) return Boolean is
      begin
         if Wiki_URI_Prefix & Tree.Name = Name then
            return True;
         elsif Tree.Down = null then
            return False;
         else
            declare
               Next : Item_Access := Tree.Down;
            begin
               while Next /= null loop
                  if Find (Next, Name) then
                     return True;
                  end if;

                  Next := Next.Next;
               end loop;

               return False;
            end;
         end if;
      end Find;

   begin
      Expand (Self.Root);
   end Expand;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Sidebar;
      Text : League.Strings.Universal_String)
   is
      package R renames League.Regexps;

      Status : constant R.Regexp_Pattern :=
        R.Compile (League.Strings.To_Universal_String
                     ("(?:(\p{Zs}*new)|(\p{Zs}*changed))+"));

      type Wiki_Handler is new Axe.Wiki.Parser.Wiki_Handler with record
         In_Prefix : Boolean := False;
         In_Suffix : Boolean := False;
         Do_Suffix : Boolean := False;
         Data      : Item_Access;
      end record;

      overriding procedure Start_Element
        (Item : in out Wiki_Handler;
         Info : Axe.Wiki.Element_Info);

      overriding procedure End_Element
        (Item : in out Wiki_Handler;
         Info : Axe.Wiki.Element_Info);

      overriding procedure Characters
        (Item : in out Wiki_Handler;
         Text : League.Strings.Universal_String);

      procedure Destroy (Tree : in out Item_Access);

      ----------------
      -- Characters --
      ----------------

      overriding procedure Characters
        (Item : in out Wiki_Handler;
         Text : League.Strings.Universal_String)
      is
         Found : constant R.Regexp_Match := Status.Find_Match (Text);
      begin
         if Item.In_Prefix then
            Self.Prefix.Append (Text);
         elsif Item.In_Suffix then
            Self.Suffix.Append (Text);
         elsif Found.Is_Matched then
            if Found.Last_Index (1) > 0 then
               Item.Data.Kind := Added;
            end if;

            if Found.Last_Index (2) > 0 then
               Item.Data.Kind := Changed;
            end if;
         end if;
      end Characters;

      -------------
      -- Destroy --
      -------------

      procedure Destroy (Tree : in out Item_Access) is
         procedure Free is new Ada.Unchecked_Deallocation (Item, Item_Access);

         Next : Item_Access;
         Prev : Item_Access;
      begin
         if Tree /= null then
            Prev := Tree.Down;

            while Prev /= null loop
               Next := Prev.Next;
               Destroy (Prev);
               Prev := Next;
            end loop;
         end if;

         Free (Tree);
      end Destroy;

      -----------------
      -- End_Element --
      -----------------

      overriding procedure End_Element
        (Item : in out Wiki_Handler;
         Info : Axe.Wiki.Element_Info) is
      begin
         case Info.Kind is
            when Axe.Wiki.Ordered_List =>
               Item.Data := Item.Data.Up;

               while Item.Data.Next /= null loop
                  Item.Data := Item.Data.Next;
               end loop;

               Item.Do_Suffix := True;
            when Axe.Wiki.Preformat =>
               Item.In_Prefix := False;
               Item.In_Suffix := False;
            when others =>
               null;
         end case;
      end End_Element;

      -------------------
      -- Start_Element --
      -------------------

      overriding procedure Start_Element
        (Item : in out Wiki_Handler;
         Info : Axe.Wiki.Element_Info)
      is
         Next : Item_Access;
      begin
         case Info.Kind is
            when Axe.Wiki.Ordered_List =>
               Next := new Axe.Sidebars.Item;
               Next.Level := Item.Data.Level + 1;
               Next.Up := Item.Data;

               --  Add to end of list of children

               if Item.Data.Down = null then
                  Item.Data.Down := Next;
               else
                  declare
                     Prev : Item_Access := Item.Data.Down;
                     Step : Item_Access := Prev.Next;
                  begin
                     while Step /= null loop
                        Prev := Step;
                        Step := Step.Next;
                     end loop;

                     Prev.Next := Next;
                  end;
               end if;

               Item.Data := Next;
            when Axe.Wiki.List_Item =>
               if not Item.Data.Fill then
                  Next := new Axe.Sidebars.Item;
                  Next.Level := Item.Data.Level;
                  Next.Up    := Item.Data.Up;
                  Item.Data.Next := Next;
                  Item.Data := Next;
               end if;
            when Axe.Wiki.Boxed_Link =>
               Item.Data.Name := Info.Link;
               Item.Data.Title := Info.Title;
               Item.Data.Fill := False;
            when Axe.Wiki.Boxed_Wiki_Link =>
               Item.Data.Name := Info.Link.Slice (6, Info.Link.Length);
               Item.Data.Title := Info.Title;
               Item.Data.Fill := False;
            when Axe.Wiki.Preformat =>
               if Item.Do_Suffix then
                  Item.In_Suffix := True;
               else
                  Item.In_Prefix := True;
               end if;
            when others =>
               null;
         end case;
      end Start_Element;

      Root : Wiki_Handler;

   begin
      Root.Data := new Item;
      Destroy (Self.Root);
      Self.Root := Root.Data;

      Axe.Wiki.Parser.Parse (Text, Root);
   end Initialize;

end Axe.Sidebars;
