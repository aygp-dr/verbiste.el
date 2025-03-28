<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" encoding="UTF-8"/>
  
  <!-- Match the root element -->
  <xsl:template match="/">
    <xsl:apply-templates select="//i"/>
  </xsl:template>
  
  <!-- Match each <i> tag and output its content followed by a newline -->
  <xsl:template match="i">
    <xsl:value-of select="."/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>
</xsl:stylesheet>
