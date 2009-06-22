<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xlink="http://www.w3.org/1999/xlink"
                xmlns:restas="restas://restas/"
                version="1.0">

    <xsl:include href="content.xsl" />
    
    <xsl:template match="/page">
        <overlay>
            <head>
                <title><xsl:value-of select="@xlink:title" /></title>
            </head>
            
            <div id="content">
                <h3><xsl:value-of select="@xlink:title" /></h3>
                
                <table>
                    <tbody>
                        <tr>
                            <td><b>Автор:</b></td>
                            <td>
                                <a href="{author/@xlink:href}">
                                    <xsl:value-of select="author/@xlink:title" />
                                </a>
                            </td>
                        </tr>
                        <tr>
                            <td><b>Источник:</b></td>
                            <td><a href="{@xlink:href}"><xsl:value-of select="@xlink:href" /></a></td>
                        </tr>
                    </tbody>
                </table>

                <xsl:apply-templates select="*|text()" />
            </div>
        </overlay>
    </xsl:template>

    <xsl:template match="author" />

</xsl:stylesheet>