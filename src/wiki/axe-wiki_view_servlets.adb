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

with Ada.Directories;

with League.Base_Codecs;
with League.Calendars.Ada_Conversions;
with League.Holders;
with League.Holders.JSON_Objects;
with League.IRIs;
with League.Stream_Element_Vectors;
with League.String_Vectors;
with League.Text_Codecs;
with League.JSON.Objects;
with League.JSON.Values;

with XML.SAX.Event_Writers;
with XML.SAX.HTML5_Writers;
with XML.SAX.Input_Sources.Streams.Files;
with XML.SAX.Locators;
with XML.SAX.Locators.Internals;
with XML.SAX.Output_Destinations.Strings;
with XML.SAX.Simple_Readers;
with XML.Templates.Processors;
with XML.Templates.Streams.Holders;

with Matreshka.Internals.SAX_Locators;

with Servlet.Contexts;

with Axe.Dummy_Locators;
with Axe.Read_File;
with Axe.Sidebars;
with Axe.Wiki.HTML_Output;
with Axe.Wiki.Parser;
with Axe.Wiki.Specials.Ada;
with Axe.Wiki.Titles;

with Ada.Wide_Wide_Text_IO;
with Ada.Characters.Wide_Wide_Latin_1;

package body Axe.Wiki_View_Servlets is

   use type League.Strings.Universal_String;

   function "+"
     (Text : Wide_Wide_String) return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   function String_Carriage_Return
    (Text : League.Strings.Universal_String)
      return League.Strings.Universal_String;
   --  Drop 0x0D characters from Text and return result

   procedure Render_Wiki
     (URI       : League.Strings.Universal_String;
      Text      : League.Strings.Universal_String;
      Is_Edit   : Boolean;
      Context   : access Servlet.Contexts.Servlet_Context'Class;
      Response  : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class);

   procedure Chech_Authorization
     (Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class;
      Success  : out Boolean);

   UTF_8   : constant League.Strings.Universal_String := +"utf-8";
   Decoder : constant League.Text_Codecs.Text_Codec :=
     League.Text_Codecs.Codec (UTF_8);
   Page_XHTML   : constant League.Strings.Universal_String :=
     +"/page.xhtml.tmpl";
   Edit_XHTML   : constant League.Strings.Universal_String :=
     +"/edit_wiki.xhtml.tmpl";
   Sidebar_File : constant League.Strings.Universal_String :=
     +"/wiki/layout.wiki";
   Authorization : constant League.Strings.Universal_String :=
     +"Authorization";
   XHTML         : constant League.Strings.Universal_String :=
     +"http://www.w3.org/1999/xhtml";

   Ada_Ru       : constant Wide_Wide_String := "http://www.ada-ru.org";
   Article      : constant League.Strings.Universal_String := +"article";
   Default_Img  : constant League.Strings.Universal_String
     := +(Ada_Ru & "/graphics/ada_ru.png");
   Descr        : constant League.Strings.Universal_String := +"description";
   Edit_Wiki    : constant League.Strings.Universal_String := +"edit_wiki";
   Image        : constant League.Strings.Universal_String := +"image";
   Locale       : constant League.Strings.Universal_String := +"ru_RU";
   Locale_Name  : constant League.Strings.Universal_String := +"locale";
   Location     : constant League.Strings.Universal_String := +"Location";
   OG_Name      : constant League.Strings.Universal_String := +"open_graph";
   Post         : constant League.Strings.Universal_String := +"post";
   Preview      : constant League.Strings.Universal_String := +"preview";
   Sidebar      : constant League.Strings.Universal_String := +"sidebar";
   Text         : constant League.Strings.Universal_String := +"text";
   Title_Name   : constant League.Strings.Universal_String := +"title";
   Type_Name    : constant League.Strings.Universal_String := +"type";
   URI          : constant League.Strings.Universal_String := +"uri";
   URL_Name     : constant League.Strings.Universal_String := +"url";
   Website      : constant League.Strings.Universal_String := +"website";
   Wiki_Page    : constant League.Strings.Universal_String := +"wikiPage";

   function Get_URI
    (Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class)
      return League.Strings.Universal_String;
   --  Return URI of request

   function Get_Wiki_File
    (URI : League.Strings.Universal_String)
      return League.Strings.Universal_String;
   --  Return corresponding .wiki virtual file name in context space

   -------------------------
   -- Chech_Authorization --
   -------------------------

   procedure Chech_Authorization
     (Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class;
      Success  : out Boolean)
   is
      Auth : constant League.String_Vectors.Universal_String_Vector :=
        Request.Get_Headers (Authorization);

      procedure Authenticate (Realm : League.Strings.Universal_String);

      function Valid return Boolean;

      ------------------
      -- Authenticate --
      ------------------

      procedure Authenticate
        (Realm : League.Strings.Universal_String)
      is
         Value : League.Strings.Universal_String;
      begin
         Value.Append ("Basic ");
         Value.Append ("realm=""");
         Value.Append (Realm);
         Value.Append ("""");

         Response.Set_Header (+"WWW-Authenticate", Value);

         Response.Set_Status (Servlet.HTTP_Responses.Unauthorized);
      end Authenticate;

      -----------
      -- Valid --
      -----------

      function Valid return Boolean is
         Ok   : Boolean;
         Raw  : League.Stream_Element_Vectors.Stream_Element_Vector;
         Text : League.Strings.Universal_String;
         List : League.String_Vectors.Universal_String_Vector;
      begin
         if Auth.Length < 1 then
            return False;
         end if;

         List := Auth.Element (1).Split (' ');

         if List.Length < 2 then
            return False;
         elsif List.Element (1) /= +"Basic" then
            return False;
         end if;

         League.Base_Codecs.From_Base_64 (List.Element (2), Raw, Ok);

         if not Ok then
            return False;
         end if;

         begin
            Text := Decoder.Decode (Raw);
         exception
            when Constraint_Error =>
               return False;
         end;

         declare
            Pair : constant League.String_Vectors.Universal_String_Vector :=
              Text.Split (':');
            File  : League.Strings.Universal_String;
            Text  : League.Strings.Universal_String;
         begin
            if Pair.Length < 2 then
               return False;
            end if;

            File.Append ("/password/");
            File.Append (Pair.Element (1));
            File := Request.Get_Servlet_Context.Get_Real_Path (File);
            Text := Axe.Read_File (File, Decoder);
            Text :=
              Text.Head (Text.Index (Ada.Characters.Wide_Wide_Latin_1.LF) - 1);

            return Text = Pair.Element (2);
         exception
            when Ada.Wide_Wide_Text_IO.Name_Error =>
               return False;
         end;
      end Valid;

   begin
      Success := Valid;

      if not Success then
         Authenticate (+"Ada RU WiKi");
      end if;
   end Chech_Authorization;

   ------------
   -- Do_Get --
   ------------

   overriding procedure Do_Get
     (Self     : in out Wiki_View_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      pragma Unreferenced (Self);

      Context      : constant access Servlet.Contexts.Servlet_Context'Class
        := Request.Get_Servlet_Context;
      URI          : constant League.Strings.Universal_String
        := Get_URI (Request);
      Wiki_File    : constant League.Strings.Universal_String
        := Get_Wiki_File (URI);
      Real_Name    : constant League.Strings.Universal_String
        := Context.Get_Real_Path (Wiki_File);
      Servlet_Path : constant League.String_Vectors.Universal_String_Vector
        := Request.Get_Servlet_Path;
      Is_Edit      : constant Boolean := Servlet_Path.Length = 1
        and then Servlet_Path.Element (1) = Edit_Wiki;
   begin
      if Ada.Directories.Exists (Real_Name.To_UTF_8_String) then
         Render_Wiki
           (URI       => URI,
            Text      => Axe.Read_File (Real_Name, Decoder),
            Is_Edit   => Is_Edit,
            Context   => Context,
            Response  => Response);
      elsif Is_Edit then
         Render_Wiki
           (URI       => URI,
            Text      => League.Strings.Empty_Universal_String,
            Is_Edit   => True,
            Context   => Context,
            Response  => Response);
      else
         Response.Set_Status (Servlet.HTTP_Responses.Not_Found);
      end if;
   end Do_Get;

   -------------
   -- Do_Post --
   -------------

   overriding procedure Do_Post
    (Self     : in out Wiki_View_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
     Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is

      Success : Boolean;
      Context : constant access Servlet.Contexts.Servlet_Context'Class
        := Request.Get_Servlet_Context;
      URI  : League.Strings.Universal_String :=
        Request.Get_Parameter (Axe.Wiki_View_Servlets.URI);
      Post : constant League.Strings.Universal_String :=
        Request.Get_Parameter (Axe.Wiki_View_Servlets.Post);
      Text : constant League.Strings.Universal_String :=
        String_Carriage_Return
          (Request.Get_Parameter (Axe.Wiki_View_Servlets.Text));
   begin
      Chech_Authorization (Request, Response, Success);

      if not Success then
         return;
      elsif Post = Preview then
         Render_Wiki (URI, Text, True, Context, Response);
      else
         declare
            use Ada.Wide_Wide_Text_IO;

            Wiki_File : constant League.Strings.Universal_String
              := Get_Wiki_File (URI);
            Real_Name : constant League.Strings.Universal_String
              := Context.Get_Real_Path (Wiki_File);
            Name      : constant String := Real_Name.To_UTF_8_String;
            Created   : constant Boolean := not Ada.Directories.Exists (Name);
            File : File_Type;
         begin
            Create (File, Name => Name, Form => "WCEM=8");
            Put (File, Text.To_Wide_Wide_String);
            Close (File);

            if URI.Is_Empty then
               URI.Append ("/");
            end if;

            if Self.Event_Listener /= null then
               Self.Event_Listener.On_Wiki_Saved
                 (URI, Text, League.Strings.Empty_Universal_String, Created);
            end if;

            Response.Set_Status (Servlet.HTTP_Responses.Moved_Temporarily);
            Response.Set_Header (Location, URI);
         end;
      end if;
   end Do_Post;

   -----------------------
   -- Get_Last_Modified --
   -----------------------

   overriding function Get_Last_Modified
    (Self     : in out Wiki_View_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class)
      return League.Calendars.Date_Time
   is
      procedure Check_File (File : League.Strings.Universal_String);
      --  Update Result and Exist using given "virtual" file name

      Result    : League.Calendars.Date_Time;
      Exist     : Boolean := True;
      First     : Boolean := True;
      Context   : constant access Servlet.Contexts.Servlet_Context'Class
        := Request.Get_Servlet_Context;
      Wiki_File : constant League.Strings.Universal_String
        := Get_Wiki_File (Get_URI (Request));

      ----------------
      -- Check_File --
      ----------------

      procedure Check_File (File : League.Strings.Universal_String) is
         use type League.Calendars.Date_Time;

         Real_Name : constant League.Strings.Universal_String
           := Context.Get_Real_Path (File);
         Name      : constant String := Real_Name.To_UTF_8_String;
         Date_Time : League.Calendars.Date_Time;

      begin
         if Ada.Directories.Exists (Name) then
            Date_Time := League.Calendars.Ada_Conversions.From_Ada_Time
              (Ada.Directories.Modification_Time (Name));

            if First or else Result < Date_Time then
               First := False;
               Result := Date_Time;
            end if;
         else
            Exist := False;
         end if;
      end Check_File;

   begin
      Check_File (Wiki_File);
      Check_File (Page_XHTML);
      Check_File (Sidebar_File);
      Check_File (Edit_XHTML);

      if Exist then
         return Result;
      else
         return Servlet.HTTP_Servlets.HTTP_Servlet (Self)
           .Get_Last_Modified (Request);
      end if;
   end Get_Last_Modified;

   ----------------------
   -- Get_Servlet_Info --
   ----------------------

   overriding function Get_Servlet_Info (Self : Wiki_View_Servlet)
     return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
   begin
      return League.Strings.To_Universal_String ("Wiki Rendering Servlet");
   end Get_Servlet_Info;

   -------------
   -- Get_URI --
   -------------

   function Get_URI
     (Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class)
       return League.Strings.Universal_String
   is
      Path_Info    : constant League.String_Vectors.Universal_String_Vector
        := Request.Get_Path_Info;
      Path         : League.String_Vectors.Universal_String_Vector;
   begin
      if Path_Info.Length = 1 and then Path_Info (1).Is_Empty then
         return League.Strings.Empty_Universal_String;
      end if;

      --  Add empty string to have leading '/'
      Path.Append (League.Strings.Empty_Universal_String);
      Path.Append (Path_Info);
      return Path.Join ('/');
   end Get_URI;

   -------------------
   -- Get_Wiki_File --
   -------------------

   function Get_Wiki_File
    (URI : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String := URI;
   begin
      if Result.Is_Empty then
         Result.Append ("/index.html");
      end if;

      Result.Append (".wiki");

      return Result;
   end Get_Wiki_File;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
    (Parameters : not null access
       Servlet.Generic_Servlets.Instantiation_Parameters'Class)
         return Wiki_View_Servlet
   is
      pragma Unreferenced (Parameters);
   begin
      return (Servlet.HTTP_Servlets.HTTP_Servlet with
                Event_Listener => null);
   end Instantiate;

   --------------------
   -- Render_Preview --
   --------------------

   procedure Render_Wiki
     (URI       : League.Strings.Universal_String;
      Text      : League.Strings.Universal_String;
      Is_Edit   : Boolean;
      Context   : access Servlet.Contexts.Servlet_Context'Class;
      Response  : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      procedure Wiki_Content
        (Content    : out League.Holders.Holder;
         Open_Graph : out League.JSON.Objects.JSON_Object);
      --  Return rendered wiki content wrapped into
      --  Holder of XML.Templates.Streams.Holders. Also return page title,
      --  image and description as Open_Graph.

      function Edit_Wiki_Content return League.Holders.Holder;
      --  Return edit wiki form with wiki content inside it, wrapped into
      --  Holder of XML.Templates.Streams.Holders

      function Sidebar_Content return League.Holders.Holder;
      --  Return rendered sidebar menu wrapped into
      --  Holder of XML.Templates.Streams.Holders

      Input  : aliased XML.SAX.Input_Sources.Streams.Files.File_Input_Source;
      Reader : aliased XML.SAX.Simple_Readers.Simple_Reader;
      Filter : aliased XML.Templates.Processors.Template_Processor;
      Writer : aliased XML.SAX.HTML5_Writers.HTML5_Writer;
      Output : aliased XML.SAX.Output_Destinations.Strings
        .String_Output_Destination;

      -----------------------
      -- Edit_Wiki_Content --
      -----------------------

      function Edit_Wiki_Content return League.Holders.Holder is
         Input     : aliased XML.SAX.Input_Sources.Streams.Files
           .File_Input_Source;
         Reader    : aliased XML.SAX.Simple_Readers.Simple_Reader;
         Filter    : aliased XML.Templates.Processors.Template_Processor;
         Event     : aliased XML.SAX.Event_Writers.Event_Writer;
      begin
         --  Set template input
         Input.Open_By_File_Name (Context.Get_Real_Path (Edit_XHTML));

         --  Configure reader
         Reader.Set_Input_Source (Input'Unchecked_Access);
         Reader.Set_Content_Handler (Filter'Unchecked_Access);
         Reader.Set_Lexical_Handler (Filter'Unchecked_Access);

         --  Configure template processor
         Filter.Set_Content_Handler (Event'Unchecked_Access);
         Filter.Set_Lexical_Handler (Event'Unchecked_Access);

         --  Bind wiki page content
         Filter.Set_Parameter
           (Wiki_View_Servlets.URI, League.Holders.To_Holder (URI));
         Filter.Set_Parameter (Wiki_Page, League.Holders.To_Holder (Text));

         --  Process template
         Reader.Parse;

         if not Filter.Error_String.Is_Empty then
            raise Constraint_Error with Filter.Error_String.To_UTF_8_String;
         end if;

         return XML.Templates.Streams.Holders.To_Holder (Event.Get_Stream);
      end Edit_Wiki_Content;

      ---------------------
      -- Sidebar_Content --
      ---------------------

      function Sidebar_Content return League.Holders.Holder is
         Sidebar   : Axe.Sidebars.Sidebar;
         Real_Name : constant League.Strings.Universal_String
           := Context.Get_Real_Path (Sidebar_File);
         Text      : constant League.Strings.Universal_String :=
           Axe.Read_File (Real_Name, Decoder);
         Event     : aliased XML.SAX.Event_Writers.Event_Writer;
      begin
         Sidebar.Initialize (Text);

         --  Set document locator to avoid constraint error
         Event.Set_Document_Locator
           (XML.SAX.Locators.Internals.Create (Dummy_Locators.Locator'Access));

         Sidebar.Expand (Event'Unchecked_Access, URI, "/");

         return XML.Templates.Streams.Holders.To_Holder (Event.Get_Stream);
      end Sidebar_Content;

      ------------------
      -- Wiki_Content --
      ------------------

      procedure Wiki_Content
        (Content    : out League.Holders.Holder;
         Open_Graph : out League.JSON.Objects.JSON_Object)
      is
         Handler   : aliased Axe.Wiki.HTML_Output.Handler;
         Get_Title : Axe.Wiki.Titles.Handler;
         Ada       : aliased Axe.Wiki.Specials.Ada.Ada_Format;
         Event     : aliased XML.SAX.Event_Writers.Event_Writer;
      begin
         --  Set document locator to avoid constraint error
         Event.Set_Document_Locator
           (XML.SAX.Locators.Internals.Create (Dummy_Locators.Locator'Access));

         --  Parse Wiki page
         Get_Title.Initialize (Handler'Unchecked_Access, Ada_Ru);
         Handler.Initialize (Event'Unchecked_Access, XHTML, "");
         Ada.Initialize (XHTML);
         Handler.Register_Special_Format (+"ada", Ada'Unchecked_Access);
         Axe.Wiki.Parser.Parse (Text, Get_Title);

         Content := XML.Templates.Streams.Holders.To_Holder (Event.Get_Stream);

         if Get_Title.Title.Is_Empty then
            Open_Graph.Insert
              (Title_Name, League.JSON.Values.To_JSON_Value (URI));
         else
            Open_Graph.Insert
              (Title_Name, League.JSON.Values.To_JSON_Value (Get_Title.Title));
         end if;

         if Get_Title.Image.Is_Empty then
            Open_Graph.Insert
              (Image, League.JSON.Values.To_JSON_Value (Default_Img));
         else
            Open_Graph.Insert
              (Image, League.JSON.Values.To_JSON_Value (Get_Title.Image));
         end if;

         Open_Graph.Insert
           (Descr, League.JSON.Values.To_JSON_Value (Get_Title.Description));

         Open_Graph.Insert
           (URL_Name, League.JSON.Values.To_JSON_Value (URI));
         Open_Graph.Insert
           (Locale_Name, League.JSON.Values.To_JSON_Value (Locale));

         if URI.Is_Empty then
            Open_Graph.Insert
              (Type_Name, League.JSON.Values.To_JSON_Value (Website));
         else
            Open_Graph.Insert
              (Type_Name, League.JSON.Values.To_JSON_Value (Article));
         end if;
      end Wiki_Content;

      Content    : League.Holders.Holder;
      Open_Graph : League.JSON.Objects.JSON_Object;
   begin
      --  Set template input
      Input.Open_By_File_Name (Context.Get_Real_Path (Page_XHTML));

      --  Configure reader
      Reader.Set_Input_Source (Input'Unchecked_Access);
      Reader.Set_Content_Handler (Filter'Unchecked_Access);
      Reader.Set_Lexical_Handler (Filter'Unchecked_Access);

      --  Configure template processor
      Filter.Set_Content_Handler (Writer'Unchecked_Access);
      Filter.Set_Lexical_Handler (Writer'Unchecked_Access);

      --  Bind wiki page content
      Wiki_Content (Content, Open_Graph);
      Filter.Set_Parameter (Sidebar, Sidebar_Content);
      Filter.Set_Parameter (Wiki_Page, Content);
      Filter.Set_Parameter
        (OG_Name, League.Holders.JSON_Objects.To_Holder (Open_Graph));

      if Is_Edit then
         Filter.Set_Parameter (Edit_Wiki, Edit_Wiki_Content);
      else
         Filter.Set_Parameter
           (Edit_Wiki,
            XML.Templates.Streams.Holders.To_Holder
              (XML.Templates.Streams.XML_Stream_Element_Vectors.Empty_Vector));
      end if;

      --  Configure template persing output
      Writer.Set_Output_Destination (Output'Unchecked_Access);

      --  Process template
      Reader.Parse;

      if not Filter.Error_String.Is_Empty then
         raise Constraint_Error with Filter.Error_String.To_UTF_8_String;
      end if;

      Response.Set_Status (Servlet.HTTP_Responses.OK);
      Response.Set_Content_Type (+"text/html");
      Response.Set_Character_Encoding (UTF_8);

      Response.Get_Output_Stream.Write (Output.Get_Text);
   end Render_Wiki;

   ------------------------
   -- Set_Event_Listener --
   ------------------------

   not overriding procedure Set_Event_Listener
     (Self  : in out Wiki_View_Servlet;
      Value : access Axe.Events.Listener'Class) is
   begin
      Self.Event_Listener := Value;
   end Set_Event_Listener;

   ----------------------------
   -- String_Carriage_Return --
   ----------------------------

   function String_Carriage_Return
    (Text : League.Strings.Universal_String)
     return League.Strings.Universal_String
   is
      List : constant League.String_Vectors.Universal_String_Vector :=
        Text.Split (Ada.Characters.Wide_Wide_Latin_1.CR);
   begin
      return List.Join ("");
   end String_Carriage_Return;

end Axe.Wiki_View_Servlets;
