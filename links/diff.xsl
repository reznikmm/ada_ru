<?xml version='1.0' encoding='koi8-r'?>
<!DOCTYPE stylesheet [
]>
    
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>
		
<xsl:output method="text" encoding="koi8-r" omit-xml-declaration="yes"/>

<xsl:param name="path">.</xsl:param>
<xsl:variable name="new" select="document(concat($path,'/ver.xml'))"/>
<xsl:variable name="links" select="document(concat($path,'/links4.xml'))"/>


<xsl:template match="/">
   <xsl:apply-templates select="*|text()|@*"/>
</xsl:template>

<xsl:template match="version">
 <xsl:apply-templates select="$new" mode="get-old">
   <xsl:with-param name="link_id" select="@id"/>
   <xsl:with-param name="old" select="."/>
 </xsl:apply-templates>
</xsl:template>

<xsl:template match="/" mode="get-old">
 <xsl:param name="link_id"/>
 <xsl:param name="old"/>
 <xsl:if test="$old/@version!=id($link_id)/@version or $old/@size!=id($link_id)/@size">
   <xsl:text>$DT </xsl:text>
   <xsl:value-of select="$old/@id"/>
   <xsl:text> </xsl:text>
   <xsl:apply-templates select="$links" mode="file_path">
      <xsl:with-param name="id" select="$link_id"/>
   </xsl:apply-templates>
   <xsl:value-of select="id($link_id)/@file"/>
   <xsl:text> </xsl:text>
   <xsl:value-of select="id($link_id)/@version"/>
   <xsl:text>
</xsl:text>
 </xsl:if>
</xsl:template>

<xsl:template match="/|*" mode="file_path">
  <xsl:param name="id"/>
  <xsl:param name="path"/>
   <xsl:apply-templates select="*" mode="file_path">
      <xsl:with-param name="id" select="$id"/>
      <xsl:with-param name="path" select="$path"/>
   </xsl:apply-templates>
</xsl:template>

<xsl:template match="folder" mode="file_path">
  <xsl:param name="id"/>
  <xsl:param name="path"/>
   <xsl:apply-templates select="*" mode="file_path">
      <xsl:with-param name="id" select="$id"/>
      <xsl:with-param name="path" select="concat($path,'/',@name)"/>
   </xsl:apply-templates>
</xsl:template>

<xsl:template match="file" mode="file_path">
  <xsl:param name="id"/>
  <xsl:param name="path"/>
  <xsl:if test="@id=$id">
    <xsl:value-of select="concat($path,'/')"/>
  </xsl:if>
</xsl:template>

</xsl:stylesheet>

