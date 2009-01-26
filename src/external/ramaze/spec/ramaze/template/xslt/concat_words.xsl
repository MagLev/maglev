<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
		xmlns:test="urn:test"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		exclude-result-prefixes="test xsl">

  <xsl:output method="text"
              encoding="utf-8"/>

  <xsl:template match="/document">
    <xsl:value-of select="test:concat('one')"/>
    <xsl:value-of select="test:concat('one', 'two')"/>
    <xsl:value-of select="test:concat('one', 'two', 'three')"/>
  </xsl:template>

</xsl:stylesheet>
