<?xml version='1.0' encoding="koi8-r"?>
<!DOCTYPE webpage [
<!ENTITY % last_mod SYSTEM "../build/last_mod.ent">
%last_mod;
]>


<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:output method="xml"
            encoding="koi8-r"
	    indent="no"
	    omit-xml-declaration="no" />
									
<xsl:template match="/">
<webpage id="old-news">
<config param="desc" value="News archive"/>
<config param="rcsdate" value="&last_mod_news;"/>
<head>
<title>Архив новостей</title>
</head>
  <xsl:apply-templates select="/news/*[position()>=6]"/>
</webpage>
</xsl:template>

<xsl:template match="item">
  <formalpara><title>
   <xsl:value-of select="@day"/>.
   <xsl:value-of select="@month"/>.
   <xsl:value-of select="@year"/>
  </title>
    <xsl:apply-templates select="*|text()"/>
  </formalpara>
</xsl:template>

<xsl:template match="*|text()|@*">
  <xsl:copy>
   <xsl:apply-templates select="*|text()|@*"/>
  </xsl:copy>
</xsl:template>
  
</xsl:stylesheet>
