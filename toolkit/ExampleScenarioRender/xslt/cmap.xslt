<?xml version="1.0" standalone="no"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="xml" omit-xml-declaration="yes" />

<xsl:template match="node()|@*">
   <xsl:copy>
   <xsl:apply-templates select="@*"/>
   <xsl:apply-templates/>
   </xsl:copy>
 </xsl:template>	
	
<xsl:template match="area">
 <xsl:copy>
  <xsl:attribute name="data-tab">#resources</xsl:attribute>
  <xsl:apply-templates select="node()|@*"/>
 </xsl:copy>
</xsl:template>

</xsl:stylesheet>


