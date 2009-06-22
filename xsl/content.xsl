<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xlink="http://www.w3.org/1999/xlink"
                xmlns:restas="restas://restas/"
                extension-element-prefixes="restas"
                version="1.0">

    <xsl:template match="/page">
        <overlay>
            <head>
                <title><xsl:value-of select="@xlink:title" /></title>
            </head>
            
            <div id="content">
                <xsl:apply-templates select="*|text()" />
            </div>
        </overlay>
    </xsl:template>

    <!-- //////////////////////////////////////////////////////////////////////////////////////////////////// -->
    <!-- //////////////////////////////////////////////////////////////////////////////////////////////////// -->

    <xsl:template match="title" />

    <xsl:template match="code">
        <!--<xsl:value-of select="restas:colorize(string(.))" />-->
        <div class="code">
            <restas:text2html select="restas:colorize(string(.))" />
        </div>
        <!--
        <pre class="code">
            <xsl:call-template name="multiline-string">
                <xsl:with-param name="str" select="string(.)" />
                <xsl:with-param name="trim">
                    <xsl:call-template name="left-trim-count">
                        <xsl:with-param name="str" select="string(.)" />
                    </xsl:call-template>
                </xsl:with-param>
            </xsl:call-template>
        </pre>
        -->
    </xsl:template>

    <xsl:template match="*|text()">
        <xsl:copy>
            <xsl:copy-of select="@*" />
            <xsl:apply-templates select="node()|text()" />
        </xsl:copy>
    </xsl:template>


    <!-- //////////////////////////////////////////////////////////////////////////////////////////////////// -->
    <!-- //////////////////////////////////////////////////////////////////////////////////////////////////// -->

    <xsl:template name="starts-space-count">
        <xsl:param name="str" />
        <xsl:param name="count" select="0" />

        <xsl:choose>
            <xsl:when test="normalize-space($str) = ''">1000</xsl:when>
            
            <xsl:when test="substring($str, 1, 1) = ' '">
                <xsl:call-template name="starts-space-count">
                    <xsl:with-param name="str" select="substring($str, 2)" />
                    <xsl:with-param name="count" select="$count + 1" />
                </xsl:call-template>
            </xsl:when>

            <xsl:otherwise>
                <xsl:value-of select="$count" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="left-trim-count">
        <xsl:param name="str" />
        <xsl:param name="count" select="1000" />

        <xsl:variable name="before" select="substring-before($str, '&#xA;')" />
        <xsl:variable name="after" select="substring-after($str, '&#xA;')" />

        <xsl:variable name="left">
            <xsl:call-template name="starts-space-count">
                <xsl:with-param name="str" select="$before" />
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="right">
            <xsl:choose>
                <xsl:when test="not($after = '')">
                    <xsl:call-template name="left-trim-count">
                        <xsl:with-param name="str" select="$after" />
                    </xsl:call-template>
                </xsl:when>

                <xsl:otherwise>
                    <xsl:value-of select="$count" />
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <xsl:choose>
            <xsl:when test="$left &lt; $right">
                <xsl:value-of select="$left" />
            </xsl:when>

            <xsl:otherwise>
                <xsl:value-of select="$right" />
            </xsl:otherwise>
        </xsl:choose>
        
    </xsl:template>


    <xsl:template name="multiline-string">
        <xsl:param name="str" />
        <xsl:param name="trim" select="0" />
        <xsl:param name="first" select="'true'" />

        <xsl:if test="$str and not(normalize-space($str) = '')">
            <xsl:if test="not($first = 'true')">
                <br />                
            </xsl:if>

            <xsl:variable name="line" select="substring(substring-before($str, '&#xA;'), $trim + 1)" />
            <xsl:value-of select="$line" />

            <xsl:variable name="nextfirst">
                <xsl:choose>
                    <xsl:when test="normalize-space($line) = ''"><xsl:text>true</xsl:text></xsl:when>
                    <xsl:otherwise><xsl:text>false</xsl:text></xsl:otherwise>
                </xsl:choose>
            </xsl:variable>

            <xsl:call-template name="multiline-string">
                <xsl:with-param name="str" select="substring-after($str, '&#xA;')" />
                <xsl:with-param name="trim" select="$trim" />
                <xsl:with-param name="first" select="$nextfirst" />
            </xsl:call-template>
        </xsl:if>
    </xsl:template>
</xsl:stylesheet>