<?xml version='1.0' encoding="koi8-r"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:output method="xml"
            encoding="koi8-r"
	    indent="no"
	    omit-xml-declaration="no" />
									
<xsl:template match="/">
 <section><title>Новости</title>
  <xsl:apply-templates select="/news/*[6>position()]"/>
 </section>
</xsl:template>

<xsl:template match="item">
  <formalpara><title>
   <xsl:value-of select="@day"/>.
   <xsl:value-of select="@month"/>.
   <xsl:value-of select="@year"/>
  </title>
    <xsl:apply-templates select="*|text()" mode="inside"/>
  </formalpara>
  <xsl:apply-templates select="*"/>
</xsl:template>

<xsl:template
  match="inlinegraphic|ulink|olink|text()|@*" mode="inside">
  <xsl:copy>
   <xsl:apply-templates select="*|text()|@*" mode="inside"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="*" mode="inside"/>
  
<xsl:template match="*|text()|@*">
  <xsl:copy>
   <xsl:apply-templates select="*|text()|@*"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="inlinegraphic|ulink|olink"/>


</xsl:stylesheet>
