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
with Ada.Streams.Stream_IO;

with League.Calendars.Ada_Conversions;
with League.Calendars.ISO_8601;
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
with Matreshka.RFC2616_Dates;

with Servlet.Contexts;

with Axe.Read_File;
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
   Wiki_Prefix : constant League.Strings.Universal_String := +"/wiki.prefix";
   Wiki_Suffix : constant League.Strings.Universal_String := +"/wiki.suffix";
   Wiki_Page   : constant League.Strings.Universal_String := +"wikiPage";

   function Get_File_Name
     (Self     : Wiki_View_Servlet'Class;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class)
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
         return League.IRIs.IRI is
      begin
         return Result : League.IRIs.IRI;
      end Base_URI;

      --------------
      -- Encoding --
      --------------

      overriding function Encoding
        (Self : not null access constant Dummy_Shared_Locator)
         return League.Strings.Universal_String is
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
      function Read_Template return League.Strings.Universal_String;
      --  Read page template using wiki.prefix and wiki suffix text fixles

      Context   : constant access Servlet.Contexts.Servlet_Context'Class
        := Request.Get_Servlet_Context;
      File_Name : constant League.Strings.Universal_String
        := Get_File_Name (Self, Request);
      Real_Name : constant League.Strings.Universal_String
        := Context.Get_Real_Path (File_Name);
      Name      : constant String := Real_Name.To_UTF_8_String;
      Text      : constant League.Strings.Universal_String :=
        Axe.Read_File (Real_Name, Decoder);
      Handler   : Axe.Wiki.HTML_Output.Context;
      Input     : aliased XML.SAX.Input_Sources.Strings.String_Input_Source;
      Reader    : aliased XML.SAX.Simple_Readers.Simple_Reader;
      Event     : aliased XML.SAX.Event_Writers.Event_Writer;
      Filter    : aliased XML.Templates.Processors.Template_Processor;
      Writer    : aliased XML.SAX.HTML5_Writers.HTML5_Writer;
      Output    : aliased XML.SAX.Output_Destinations.Strings
        .String_Output_Destination;
      Locator   : XML.SAX.Locators.SAX_Locator;

      -------------------
      -- Read_Template --
      -------------------

      function Read_Template return League.Strings.Universal_String is
         Prefix : constant League.Strings.Universal_String :=
           Context.Get_Real_Path (Wiki_Prefix);
         Suffix : constant League.Strings.Universal_String :=
           Context.Get_Real_Path (Wiki_Suffix);
         Result : League.Strings.Universal_String;
      begin
         Result.Append (Axe.Read_File (Prefix, Decoder));
         Result.Append ("${");
         Result.Append (Wiki_Page);
         Result.Append ("}");
         Result.Append (Axe.Read_File (Suffix, Decoder));

         return Result;
      end Read_Template;

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

      --  Set document locator to avoid constraint error
      Event.Set_Document_Locator
        (XML.SAX.Locators.Internals.Create (Dummy_Locators.Locator'Access));

      --  Parse Wiki page
      Handler.Initialize (Event'Unchecked_Access, "/");
      Axe.Wiki.Parser.Parse (Text, Handler);

      --  Bind wiki page content
      Filter.Set_Parameter
        (Wiki_Page,
         XML.Templates.Streams.Holders.To_Holder (Event.Get_Stream));

      --  Configure template persing output
      Writer.Set_Output_Destination (Output'Unchecked_Access);

      --  Process template
      Reader.Parse;

      Response.Set_Status (Servlet.HTTP_Responses.OK);
      Response.Set_Content_Type (+"text/html");
      Response.Set_Character_Encoding (UTF_8);

      Response.Get_Output_Stream.Write (Output.Get_Text);
   end Do_Get;

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name
     (Self     : Wiki_View_Servlet'Class;
      Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class)
       return League.Strings.Universal_String
   is
      Context      : constant access Servlet.Contexts.Servlet_Context'Class
        := Request.Get_Servlet_Context;
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
      return Path.Join ('/') & ".wiki";
   end Get_File_Name;

   -----------------------
   -- Get_Last_Modified --
   -----------------------

   overriding function Get_Last_Modified
    (Self     : in out Wiki_View_Servlet;
     Request  : Servlet.HTTP_Requests.HTTP_Servlet_Request'Class)
      return League.Calendars.Date_Time
   is
      Context   : constant access Servlet.Contexts.Servlet_Context'Class
        := Request.Get_Servlet_Context;
      File_Name : constant League.Strings.Universal_String
        := Get_File_Name (Self, Request);
      Real_Name : constant League.Strings.Universal_String
        := Context.Get_Real_Path (File_Name);
      Name      : constant String := Real_Name.To_UTF_8_String;
   begin
      if Ada.Directories.Exists (Name) then
         return League.Calendars.Ada_Conversions.From_Ada_Time
           (Ada.Directories.Modification_Time (Name));
      else
         return Servlet.HTTP_Servlets.HTTP_Servlet (Self).Get_Last_Modified
                  (Request);
      end if;
   end Get_Last_Modified;

   ----------------------
   -- Get_Servlet_Info --
   ----------------------

   overriding function Get_Servlet_Info
     (Self : Wiki_View_Servlet)
      return League.Strings.Universal_String is
   begin
      return League.Strings.To_Universal_String ("Wiki Rendering Servlet");
   end Get_Servlet_Info;

end Axe.Wiki_View_Servlets;
