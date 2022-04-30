<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output indent="yes"/>
  <xsl:strip-space elements="*"/>

  <xsl:template match="/SoccerFeed|SoccerDocument">
      <xsl:apply-templates select="*"/>
  </xsl:template>

  <xsl:template match="Team">
      <xsl:apply-templates select="Player"/>
  </xsl:template>

  <xsl:template match="Team/@*">
    <xsl:element name="{concat('team_', name(.))}">
      <xsl:value-of select="."/>      
    </xsl:element>
  </xsl:template>

  <xsl:template match="Player">
    <xsl:copy>
      <xsl:apply-templates select="ancestor::Team/@*"/>
      <xsl:copy-of select="Name|Position"/>
      <xsl:apply-templates select="@*|Stat"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="Player/@*">
    <xsl:element name="{name(.)}">
      <xsl:value-of select="."/>      
    </xsl:element>
  </xsl:template>

  <xsl:template match="Stat">
    <xsl:element name="{@Type}">
      <xsl:value-of select="text()"/>     
    </xsl:element>
  </xsl:template>
</xsl:stylesheet>