<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>
<xsl:output method="xml"
            encoding="utf-8"
	    indent="yes"/>
			

<xsl:template match="/|*|text()|@*">
  <xsl:copy>
   <xsl:apply-templates select="*|text()|@*"/>
  </xsl:copy>
</xsl:template>

</xsl:stylesheet>

