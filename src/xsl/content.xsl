<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xlink="http://www.w3.org/1999/xlink"
                xmlns:rulisp="chrome://rulisp/"
                extension-element-prefixes="rulisp"
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
        <div class="code">
            <rulisp:text2html select="rulisp:colorize(string(.))" />
        </div>
    </xsl:template>

    <xsl:template match="*|text()">
        <xsl:copy>
            <xsl:copy-of select="@*" />
            <xsl:apply-templates select="node()|text()" />
        </xsl:copy>
    </xsl:template>

</xsl:stylesheet>