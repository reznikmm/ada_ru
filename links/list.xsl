<?xml version='1.0' encoding="koi8-r"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:output method="text"
            encoding="koi8-r"
	    indent="no"
	    omit-xml-declaration="yes" />

<xsl:template match="/|links">
   <xsl:apply-templates select="*"/>
</xsl:template>

<xsl:template match="folder">
   <xsl:param name="name"/>
   <xsl:apply-templates select="folder|link">
     <xsl:with-param name="name" select="concat($name,'/',@name)"/>
   </xsl:apply-templates>
</xsl:template>

<xsl:template match="link">
   <xsl:param name="name"/>
   <xsl:apply-templates select="file">
     <xsl:with-param name="name" select="$name"/>
   </xsl:apply-templates>
</xsl:template>

<xsl:template match="file">
   <xsl:param name="name"/>
   <xsl:variable name="id" select="@id"/>
   <xsl:variable name="file" select="document('ver.xml')//*[@id=$id]/@file"/>
   <xsl:variable name="size" select="document('ver.xml')//*[@id=$id]/@size"/>
   <xsl:apply-templates select="download">
     <xsl:with-param name="name" select="concat($name,'/',$file)"/>
     <xsl:with-param name="file" select="$file"/>
     <xsl:with-param name="size" select="$size"/>
   </xsl:apply-templates>
</xsl:template>

<xsl:template match="download[@type='file']">
   <xsl:param name="name"/>
   <xsl:param name="size"/>
part <xsl:value-of select="$name"/>
   <xsl:text> </xsl:text>
   <xsl:value-of select="@url"/>
   <xsl:text> </xsl:text>
   <xsl:value-of select="$size"/>
</xsl:template>

<xsl:template match="download[@type='regexp']">
   <xsl:param name="name"/>
   <xsl:param name="size"/>
   <xsl:variable name="id" select="../@id"/>
part <xsl:value-of select="$name"/>
   <xsl:text> </xsl:text>
   <xsl:value-of select="document('ver.xml')//*[@id=$id]/@url"/>
   <xsl:text> </xsl:text>
   <xsl:value-of select="$size"/>
</xsl:template>

<xsl:template match="download[@type='sf']">
   <xsl:param name="name"/>
   <xsl:param name="file"/>
   <xsl:param name="size"/>
part <xsl:value-of select="$name"/>
   <xsl:text> SF:</xsl:text>
   <xsl:value-of select="@project"/>/<xsl:value-of select="$file"/>
   <xsl:text> </xsl:text>
   <xsl:value-of select="$size"/>
</xsl:template>
</xsl:stylesheet>
