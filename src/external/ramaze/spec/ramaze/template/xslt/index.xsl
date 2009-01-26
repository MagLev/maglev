<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="text"
              encoding="utf-8"/>

  <xsl:template match="/*">
    <xsl:value-of select="name(.)"/>
    <xsl:text> </xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

</xsl:stylesheet>
