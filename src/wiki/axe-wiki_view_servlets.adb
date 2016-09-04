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

with League.Calendars.Ada_Conversions;
with League.Holders;
with League.IRIs;
with League.String_Vectors;
with League.Text_Codecs;

with XML.SAX.Event_Writers;
with XML.SAX.HTML5_Writers;
with XML.SAX.Input_Sources.Strings;
with XML.SAX.Locators;
with XML.SAX.Locators.Internals;
with XML.SAX.Output_Destinations.Strings;
with XML.SAX.Simple_Readers;
with XML.Templates.Processors;
with XML.Templates.Streams.Holders;

with Matreshka.Internals.SAX_Locators;

with Servlet.Contexts;

with Axe.Read_File;
with Axe.Sidebars;
with Axe.Wiki.HTML_Output;
with Axe.Wiki.Parser;

package body Axe.Wiki_View_Servlets is

   use type League.Strings.Universal_String;

   package Dummy_Locators is

      type Dummy_Shared_Locator is
        new Matreshka.Internals.SAX_Locators.Shared_Abstract_Locator with
      record
         null;  --  System_Id : League.Strings.Universal_String;
      end record;

      overriding function Line
        (Self : not null access constant Dummy_Shared_Locator)
         return Natural is (0);

      overriding function Column
        (Self : not null access constant Dummy_Shared_Locator)
         return Natural is (0);

      overriding function Encoding
        (Self : not null access constant Dummy_Shared_Locator)
         return League.Strings.Universal_String;

      overriding function Version
        (Self : not null access constant Dummy_Shared_Locator)
         return League.Strings.Universal_String is
           (League.Strings.Empty_Universal_String);

      overriding function Public_Id
        (Self : not null access constant Dummy_Shared_Locator)
         return League.Strings.Universal_String is
           (League.Strings.Empty_Universal_String);

      overriding function System_Id
        (Self : not null access constant Dummy_Shared_Locator)
         return League.Strings.Universal_String is
           (League.Strings.Empty_Universal_String);

      overriding function Base_URI
        (Self : not null access constant Dummy_Shared_Locator)
         return League.IRIs.IRI;

      Locator : aliased Dummy_Shared_Locator;

   end Dummy_Locators;

   function "+"
     (Text : Wide_Wide_String) return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   UTF_8   : constant League.Strings.Universal_String := +"utf-8";
   Decoder : constant League.Text_Codecs.Text_Codec :=
     League.Text_Codecs.Codec (UTF_8);
   Page_XHTML   : constant League.Strings.Universal_String :=
     +"/page.xhtml.tmpl";
   Sidebar_File : constant League.Strings.Universal_String :=
     +"/wiki/layout.wiki";
   Wiki_Page    : constant League.Strings.Universal_String := +"wikiPage";
   Sidebar      : constant League.Strings.Universal_String := +"sidebar";

   function Get_File_Name
     (Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class)
      return League.Strings.Universal_String;
   --  Return virtual file name in context space

   --------------------
   -- Dummy_Locators --
   --------------------

   package body Dummy_Locators is

      --------------
      -- Base_URI --
      --------------

      overriding function Base_URI
        (Self : not null access constant Dummy_Shared_Locator)
         return League.IRIs.IRI
      is
         pragma Unreferenced (Self);
      begin
         return Result : League.IRIs.IRI;
      end Base_URI;

      --------------
      -- Encoding --
      --------------

      overriding function Encoding
       (Self : not null access constant Dummy_Shared_Locator)
         return League.Strings.Universal_String
      is
         pragma Unreferenced (Self);
      begin
         return UTF_8;
      end Encoding;

   end Dummy_Locators;

   ------------
   -- Do_Get --
   ------------

   overriding procedure Do_Get
     (Self     : in out Wiki_View_Servlet;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class;
      Response : in out Servlet.HTTP_Responses.HTTP_Servlet_Response'Class)
   is
      pragma Unreferenced (Self);

      function Read_Template return League.Strings.Universal_String;
      --  Read page template using wiki.prefix and wiki suffix text fixles

      function Wiki_Content return League.Holders.Holder;
      --  Return rendered wiki content wrapped into
      --  Holder of XML.Templates.Streams.Holders

      function Sidebar_Content return League.Holders.Holder;
      --  Return rendered sidebar menu wrapped into
      --  Holder of XML.Templates.Streams.Holders

      Context   : constant access Servlet.Contexts.Servlet_Context'Class
        := Request.Get_Servlet_Context;
      File_Name : constant League.Strings.Universal_String
        := Get_File_Name (Request);
      Wiki_File : constant League.Strings.Universal_String
        := File_Name & ".wiki";
      Real_Name : constant League.Strings.Universal_String
        := Context.Get_Real_Path (Wiki_File);
      Name      : constant String := Real_Name.To_UTF_8_String;
      Input     : aliased XML.SAX.Input_Sources.Strings.String_Input_Source;
      Reader    : aliased XML.SAX.Simple_Readers.Simple_Reader;
      Filter    : aliased XML.Templates.Processors.Template_Processor;
      Writer    : aliased XML.SAX.HTML5_Writers.HTML5_Writer;
      Output    : aliased XML.SAX.Output_Destinations.Strings
        .String_Output_Destination;

      -------------------
      -- Read_Template --
      -------------------

      function Read_Template return League.Strings.Universal_String is
         Template : constant League.Strings.Universal_String :=
           Context.Get_Real_Path (Page_XHTML);
         Result : League.Strings.Universal_String;
      begin
         Result.Append (Axe.Read_File (Template, Decoder));

         return Result;
      end Read_Template;

      ---------------------
      -- Sidebar_Content --
      ---------------------

      function Sidebar_Content return League.Holders.Holder is
         Sidebar   : Axe.Sidebars.Sidebar;
         File_Name : constant League.Strings.Universal_String
           := Context.Get_Real_Path (Sidebar_File);
         Text      : constant League.Strings.Universal_String :=
           Axe.Read_File (File_Name, Decoder);
         Event     : aliased XML.SAX.Event_Writers.Event_Writer;
      begin
         Sidebar.Initialize (Text);

         --  Set document locator to avoid constraint error
         Event.Set_Document_Locator
           (XML.SAX.Locators.Internals.Create (Dummy_Locators.Locator'Access));

         Sidebar.Expand (Event'Unchecked_Access, File_Name, "/");

         return XML.Templates.Streams.Holders.To_Holder (Event.Get_Stream);
      end Sidebar_Content;

      ------------------
      -- Wiki_Content --
      ------------------

      function Wiki_Content return League.Holders.Holder is
         Text      : constant League.Strings.Universal_String :=
           Axe.Read_File (Real_Name, Decoder);
         Handler   : Axe.Wiki.HTML_Output.Context;
         Event     : aliased XML.SAX.Event_Writers.Event_Writer;
      begin
         --  Set document locator to avoid constraint error
         Event.Set_Document_Locator
           (XML.SAX.Locators.Internals.Create (Dummy_Locators.Locator'Access));

         --  Parse Wiki page
         Handler.Initialize (Event'Unchecked_Access, "/");
         Axe.Wiki.Parser.Parse (Text, Handler);

         return XML.Templates.Streams.Holders.To_Holder (Event.Get_Stream);
      end Wiki_Content;

   begin
      if not Ada.Directories.Exists (Name) then
         Response.Set_Status (Servlet.HTTP_Responses.Not_Found);
         return;
      end if;

      --  Set template input
      Input.Set_String (Read_Template);

      --  Configure reader
      Reader.Set_Input_Source (Input'Unchecked_Access);
      Reader.Set_Content_Handler (Filter'Unchecked_Access);
      Reader.Set_Lexical_Handler (Filter'Unchecked_Access);

      --  Configure template processor
      Filter.Set_Content_Handler (Writer'Unchecked_Access);
      Filter.Set_Lexical_Handler (Writer'Unchecked_Access);

      --  Bind wiki page content
      Filter.Set_Parameter (Wiki_Page, Wiki_Content);
      Filter.Set_Parameter (Sidebar, Sidebar_Content);

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
   end Do_Get;

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name
     (Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class)
       return League.Strings.Universal_String
   is
      Servlet_Path : constant League.String_Vectors.Universal_String_Vector
        := Request.Get_Servlet_Path;
      Path_Info    : constant League.String_Vectors.Universal_String_Vector
        := Request.Get_Path_Info;
      Path         : League.String_Vectors.Universal_String_Vector;
   begin
      --  Add empty string to have leading '/'
      Path.Append (League.Strings.Empty_Universal_String);
      Path.Append (Servlet_Path);
      Path.Append (Path_Info);
      return Path.Join ('/');
   end Get_File_Name;

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
      Check_File (Get_File_Name (Request));
      Check_File (Page_XHTML);
      Check_File (Sidebar_File);

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

end Axe.Wiki_View_Servlets;
