<?xml version='1.0' encoding='koi8-r'?>
<!DOCTYPE stylesheet [
<!ENTITY last_ver SYSTEM "../cont/last_ver.xml">
]>                                                                              

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:import href="http://docbook.sourceforge.net/release/website/2.4.1/xsl/tabular.xsl"/>

<xsl:output method="html" encoding="koi8-r" indent="yes"/>

<xsl:variable name="textbgcolor">#fffae8</xsl:variable>
<xsl:variable name="nav.icon.style">plusminus</xsl:variable>
<xsl:variable name="banner.before.navigation">1</xsl:variable>
<xsl:variable name="feedback.link.text">Отклики</xsl:variable>
<xsl:param name="nav.icon.path">graphics/navicons/</xsl:param>
<xsl:param name="toc.spacer.image">graphics/blank.gif</xsl:param>

<xsl:template name="home.navhead">
<table width="100%" border="0">
<img src="graphics/ada2_.png"/>
</table>
</xsl:template>

<xsl:template name="home.navhead.upperright">
<a href="soft.html#aws">Мы используем AWS
</a>
</xsl:template>
<!-- put your customizations here -->

<xsl:template name="allpages.banner">
<!--begin of Rambler's Top100 code -->
<a href="http://top100.rambler.ru/top100/">
<img src="http://counter.rambler.ru/top100.cnt?633267" alt="" width='1' height='1' border='0'/>
</a>
<!--end of Top100 code-->

</xsl:template>

<xsl:template name="hspacer">
  <td width="5" height="1"></td>
</xsl:template>

<xsl:template name="body.attributes">
  <xsl:attribute name="bgcolor">#fffae8</xsl:attribute>
  <!--xsl:attribute name="link">#fffae8</xsl:attribute>
  <xsl:attribute name="vlink">#fffae8</xsl:attribute-->
</xsl:template>

<xsl:template name="webpage.toc.footer">
&last_ver;

<p align="center">
<!--begin of Top100 logo-->
<a href="http://top100.rambler.ru/top100/">
<img src="http://top100-images.rambler.ru/top100/w7.gif" 
 alt="Rambler's Top100" width='88' height='31' border='0'/></a>
<!--end of Top100 logo -->
</p>

</xsl:template>

</xsl:stylesheet>
		
