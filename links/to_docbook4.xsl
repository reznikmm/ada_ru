<?xml version='1.0' encoding="koi8-r"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:exsl="http://exslt.org/common"
                extension-element-prefixes="exsl"
                version='1.1'>

<xsl:output method="xml"
            indent="no"
            omit-xml-declaration="no"
            encoding="koi8-r"/>

   <xsl:param name="lang">ru</xsl:param>

   <!-- kind = (one, list, tree) -->
   <xsl:param name="kind">one</xsl:param>

   <xsl:param name="tree_root">root</xsl:param>

   <xsl:variable name="ver" select="document('ver.xml')"/>


<xsl:template match="/">
  <xsl:apply-templates select="*"/>
</xsl:template>

<xsl:template match="links">
   <xsl:if test="$kind = 'one'">
      <webpage id="soft_full">
         <head>
            <title>Каталог ПО</title>
         </head>
      <informaltable pgwide="1">
         <tgroup cols='2' tgroupstyle="odd">
            <colspec colname="c1" colwidth="0.5in"/>
            <colspec colname="c2"/>
            <colspec colname="c3"/>
            <tbody valign="top">
               <xsl:apply-templates select="*"/>
            </tbody>
         </tgroup>
      </informaltable>
      </webpage>
   </xsl:if>
   <xsl:if test="$kind = 'list'">
      <xsl:call-template name="list_top"/>
      <xsl:apply-templates select="*" mode="split"/>
   </xsl:if>
   <xsl:if test="$kind = 'tree'">
      <xsl:call-template name="tree_top"/>
      <xsl:apply-templates select="*" mode="split">
         <xsl:with-param name="root" select="'www.ada-ru.org/'"/>
      </xsl:apply-templates>
   </xsl:if>
</xsl:template>

<xsl:template match="folder" mode="split">
   <xsl:param name="root"/>
   <xsl:param name="path"/>

   <xsl:variable name="name">
      <xsl:if test="$path = ''">
         <xsl:value-of select="@name"/>
      </xsl:if>
      <xsl:if test="$path != ''">
         <xsl:value-of select="concat($path,'/',@name)"/>
      </xsl:if>
   </xsl:variable>

   <xsl:variable name="new_root">
      <xsl:if test="$kind = 'tree'">
         <xsl:value-of select="concat('../',$root)"/>
      </xsl:if>
   </xsl:variable>

   <xsl:variable name="filename">
      <xsl:value-of
        select="concat('soft_',translate($name,'/','_'),'.xml')"/>
      <xsl:if test="$kind = 'tree'">2</xsl:if>
   </xsl:variable>

   <exsl:document href="{$tree_root}/{$filename}"
                  method="{'xml'}"
                  encoding="{'koi8-r'}"
                  indent="{'no'}"
                  omit-xml-declaration="{'no'}">
      <webpage id="{$kind}.{$name}">
         <head>
            <title>Каталог ПО</title>
         </head>

      <informaltable pgwide="1">
         <tgroup cols='2' tgroupstyle="odd">
            <colspec colname="c1" colwidth="0.5in"/>
            <colspec colname="c2"/>
            <colspec colname="c3"/>
            <tbody valign="top">

               <xsl:apply-templates select=".">
                  <xsl:with-param name="root" select="$root"/>
                  <xsl:with-param name="path" select="$path"/>
               </xsl:apply-templates>

            </tbody>
         </tgroup>
      </informaltable>
      </webpage>
   </exsl:document>

   <xsl:apply-templates select="folder" mode="split">
         <xsl:with-param name="root" select="$new_root"/>
         <xsl:with-param name="path" select="$name"/>
   </xsl:apply-templates>
</xsl:template>

<xsl:template name="list_top">
   <exsl:document href="{$tree_root}/soft_list.xml"
                  method="{'xml'}"
                  encoding="{'koi8-r'}"
                  indent="{'no'}"
                  omit-xml-declaration="{'no'}">

      <informaltable pgwide="1">
         <tgroup cols='2' tgroupstyle="odd">
            <colspec colname="c1" colwidth="0.5in"/>
            <colspec colname="c2"/>
            <colspec colname="c3"/>
            <tbody valign="top">

               <xsl:apply-templates select="folder" mode="link"/>

            </tbody>
         </tgroup>
      </informaltable>

   </exsl:document>
</xsl:template>

<xsl:template name="tree_top">
   <exsl:document href="{$tree_root}/soft_tree.xml"
                  method="{'xml'}"
                  encoding="{'koi8-r'}"
                  indent="{'no'}"
                  omit-xml-declaration="{'no'}">

      <informaltable pgwide="1">
         <tgroup cols='2' tgroupstyle="odd">
            <colspec colname="c1" colwidth="0.5in"/>
            <colspec colname="c2"/>
            <colspec colname="c3"/>
            <tbody valign="top">

               <xsl:apply-templates select="folder" mode="link"/>

            </tbody>
         </tgroup>
      </informaltable>

   </exsl:document>
</xsl:template>

<xsl:template match="folder">
   <xsl:param name="root"/>
   <xsl:param name="path"/>

   <xsl:variable name="name">
      <xsl:if test="$path = ''">
         <xsl:value-of select="@name"/>
      </xsl:if>
      <xsl:if test="$path != ''">
         <xsl:value-of select="concat($path,'/',@name)"/>
      </xsl:if>
   </xsl:variable>

   <xsl:variable name="new_root">
      <xsl:if test="$kind = 'tree'">
         <xsl:value-of select="concat('../',$root)"/>
      </xsl:if>
   </xsl:variable>

   <row>
      <entry>
         <informalfigure>
            <graphic fileref="{$new_root}graphics/folder.png"/>
         </informalfigure>
      </entry>
      <entry>
         <xsl:value-of select="$name"/>
      </entry>
      <entry valign="top">
         <xsl:apply-templates select="descr"/>
      </entry>
   </row>
   <xsl:if test="link">
      <xsl:apply-templates select="link">
         <xsl:with-param name="root" select="$new_root"/>
      </xsl:apply-templates>
      <xsl:call-template name="separator">
         <xsl:with-param name="root" select="$new_root"/>
      </xsl:call-template>
   </xsl:if>
   <xsl:if test="$kind = 'one'">
      <xsl:apply-templates select="folder">
         <xsl:with-param name="root" select="$new_root"/>
         <xsl:with-param name="path" select="$name"/>
      </xsl:apply-templates>
   </xsl:if>
   <xsl:if test="$kind != 'one'">
      <xsl:apply-templates select="folder" mode="link">
         <xsl:with-param name="root" select="$new_root"/>
         <xsl:with-param name="path" select="$name"/>
      </xsl:apply-templates>
   </xsl:if>
</xsl:template>


<xsl:template match="folder" mode="link">
   <xsl:param name="root"/>
   <xsl:param name="path"/>
   <xsl:variable name="name">
      <xsl:if test="$path = ''">
         <xsl:value-of select="@name"/>
      </xsl:if>
      <xsl:if test="$path != ''">
         <xsl:value-of select="concat($path,'/',@name)"/>
      </xsl:if>
   </xsl:variable>

   <row>
      <entry>
         <ulink>
            <xsl:attribute name="url">
               <xsl:if test="$kind = 'list'">
                  <xsl:value-of
                    select="concat('soft_',translate($name,'/','_'),'.html')"/>
               </xsl:if>
               <xsl:if test="$path = ''">
                  <xsl:value-of select="'../'"/>
               </xsl:if>
               <xsl:if test="$kind = 'tree'">
                  <xsl:value-of select="concat(@name,'/index.html')"/>
               </xsl:if>
            </xsl:attribute>
            <informalfigure>
               <graphic fileref="{$root}graphics/folder.png"/>
            </informalfigure>
         </ulink>
      </entry>
      <entry>
         <ulink>
            <xsl:attribute name="url">
               <xsl:if test="$kind = 'list'">
                  <xsl:value-of
                    select="concat('soft_',translate($name,'/','_'),'.html')"/>
               </xsl:if>
               <xsl:if test="$kind = 'tree'">
                  <xsl:if test="$path = ''">
                     <xsl:value-of select="'../'"/>
                  </xsl:if>
                  <xsl:value-of select="concat(@name,'/index.html')"/>
               </xsl:if>
            </xsl:attribute>
         <xsl:value-of select="$name"/>
         </ulink>
      </entry>
      <entry valign="top">
         <xsl:apply-templates select="descr"/>
      </entry>
   </row>
</xsl:template>

<xsl:template match="link">
   <xsl:param name="root"/>
   <row>
      <entry>
         <ulink url="{@home}">
            <informalfigure>
               <graphic fileref="{$root}graphics/pack.png"/>
            </informalfigure>
         </ulink>
      </entry>
      <entry>
         <ulink url="{@home}">
            <xsl:value-of select="@id"/>
         </ulink>
      </entry>
      <entry>
         <anchor id="{@id}"/>
         <xsl:apply-templates select="descr"/>
      </entry>
   </row>
      <xsl:apply-templates select="file">
         <xsl:with-param name="root" select="$root"/>
      </xsl:apply-templates>
   <xsl:if test="following-sibling::link">
      <xsl:call-template name="separator">
         <xsl:with-param name="root" select="$root"/>
      </xsl:call-template>
   </xsl:if>
</xsl:template>

<xsl:template match="file">
   <xsl:param name="root"/>
   <row>
      <entry>
         <ulink>
            <xsl:attribute name="url">
               <xsl:if test="$kind = 'tree'">
                  <xsl:apply-templates select="$ver" mode="file">
                     <xsl:with-param name="id" select="@id"/>
                  </xsl:apply-templates>
               </xsl:if>
               <xsl:if test="$kind != 'tree'">
                  <xsl:apply-templates select="$ver" mode="url">
                     <xsl:with-param name="id" select="@id"/>
                  </xsl:apply-templates>
               </xsl:if>
            </xsl:attribute>
            <informalfigure>
               <graphic fileref="{$root}graphics/floppy.png"/>
            </informalfigure>
         </ulink>
      </entry>
      <entry>
         <ulink>
            <xsl:attribute name="url">
               <xsl:if test="$kind = 'tree'">
                  <xsl:apply-templates select="$ver" mode="file">
                     <xsl:with-param name="id" select="@id"/>
                  </xsl:apply-templates>
               </xsl:if>
               <xsl:if test="$kind != 'tree'">
                  <xsl:apply-templates select="$ver" mode="url">
                     <xsl:with-param name="id" select="@id"/>
                  </xsl:apply-templates>
               </xsl:if>
            </xsl:attribute>
            <xsl:apply-templates select="$ver" mode="version">
               <xsl:with-param name="id" select="@id"/>
            </xsl:apply-templates>
         </ulink>
      </entry>
      <entry>
         <xsl:apply-templates select="descr"/>
      </entry>
   </row>
</xsl:template>

<xsl:template name="separator">
   <xsl:param name="root"/>
   <row>
      <entry namest="c1" nameend="c3">
            <informalfigure>
               <inlinegraphic fileref="{$root}graphics/blank.gif"
                  width="30" depth="30" />
            </informalfigure>
      </entry>
   </row>
</xsl:template>

<xsl:template match="descr">
   <xsl:if test="@lang=$lang">
      <xsl:apply-templates select="text()"/>
   </xsl:if>
</xsl:template>

<xsl:template match="/|*" mode="url">
   <xsl:param name="id"/>
   <xsl:value-of select='id($id)/@url'/>
</xsl:template>

<xsl:template match="/|*" mode="version">
   <xsl:param name="id"/>
   <xsl:value-of select='id($id)/@version'/>
</xsl:template>

<xsl:template match="/|*" mode="file">
   <xsl:param name="id"/>
   <xsl:value-of select='id($id)/@file'/>
</xsl:template>

</xsl:stylesheet>
