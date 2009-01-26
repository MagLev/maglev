<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
		xmlns:test="urn:test"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		exclude-result-prefixes="test xsl">

  <xsl:output method="text"
              encoding="utf-8"/>

  <xsl:template match="/document">
    <ruby-version><xsl:value-of select="test:get-ruby-version()"/></ruby-version>
  </xsl:template>

</xsl:stylesheet>
