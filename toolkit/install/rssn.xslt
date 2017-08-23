<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:template match="/"> 
    <html>
      <head>
        <title>FHIR Notepad++ Plugin Release History</title>
  <script><![CDATA[
  function HTMLDecode(s){
    s = s.replace(new RegExp(/&lt;/gi), "<");
    s = s.replace(new RegExp(/&gt;/gi), ">");
    s = s.replace(new RegExp(/&amp;/gi), "&");
    return s;
  }]]>
  </script>
  <style>
  .title{
   font-family: verdana;
   font-weight: bold;
  }
  .itemTitle{
   font-family: verdana;
  }
  .item{
   font-family: verdana;
  }
  </style>
      </head>
      <body>
   <a href="{/rss/channel/link}" class="title">
  <xsl:value-of select="/rss/channel/title"/>
   </a>
   <table>
        <xsl:apply-templates select="/rss/channel/item"/>
  </table>
        <a href="fhirnpp.rss"><img src="http://www.megaicons.net/static/img/icons_sizes/247/599/32/rss-icon.png"/>RSS feed</a>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="item">
 <tr><td><b><xsl:value-of select="ptitle"/></b></td>
 <td><xsl:value-of select="comment"/></td>
 <td><a href="{link}" class="item">Win64</a></td>
 <td> </td>
 <td><a href="{mlink}" class="item">OSX</a></td></tr>
  </xsl:template>

</xsl:stylesheet>