<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:template match="/"> 
    <html>
      <head>
        <title>FHIRServer release history</title>
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
        <form>
  <div>
  <ul>
        <xsl:apply-templates select="/rss/channel/item"/>
  </ul>
  </div>
        </form>
        <a href="fhirserver.rss"><img src="http://www.megaicons.net/static/img/icons_sizes/247/599/32/rss-icon.png"/> RSS feed</a>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="item">
 <li>
   <a href="{link}" class="item">
  <xsl:value-of select="title"/>
   </a><br/>   
   <div>
     <xsl:attribute name="id">td<xsl:number level="any"/></xsl:attribute>
     <xsl:value-of select="description"/>
  <script>td<xsl:number level="any"/><![CDATA[.innerHTML=HTMLDecode(]]>td<xsl:number level="any"/><![CDATA[.innerHTML);]]></script>
  </div>
 </li>
  </xsl:template>
</xsl:stylesheet>