<?xml version='1.0' encoding="koi8-r"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:output method="xml"
            encoding="koi8-r"
	    indent="no"
	    omit-xml-declaration="no" />
									
<xsl:template match="/">
  <informaltable frame='all'><tgroup>
  <xsl:attribute name='cols'>
    <xsl:value-of select="count(//item[position()=1]/*)"/>
  </xsl:attribute>
  <tbody>
  <xsl:apply-templates select="//item">
      <xsl:sort select="en/text()" lang="en"/>
  </xsl:apply-templates>
  </tbody>
  </tgroup>
  </informaltable>
</xsl:template>

<xsl:template match="item">
<row>
  <xsl:apply-templates select="en"/>
  <xsl:apply-templates select="ru"/>
</row>
</xsl:template>

<xsl:template match="en|ru">
<entry>
  <xsl:apply-templates select="text()"/>
</entry>

</xsl:template>
  
</xsl:stylesheet>
