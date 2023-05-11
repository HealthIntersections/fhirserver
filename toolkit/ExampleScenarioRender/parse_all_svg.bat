java -jar saxon9he.jar -s:current.xml -xsl:diagram.xslt -o:diagram.txt --suppressXsltNamespaceCheck:on
java -jar plantuml.jar diagram.txt -tsvg 
move diagram.svg .\output\images\current-diagram.svg

echo java -jar saxon9he.jar -s:diagram.cmapx -xsl:cmap.xslt -o:diagram.cmapx2 --suppressXsltNamespaceCheck:on 
echo del diagram.cmapx
echo ren diagram.cmapx2 diagram.cmapx

java -jar saxon9he.jar -s:current.xml -xsl:full_template_simple_svg.xslt -o:current.html pref="current-" --suppressXsltNamespaceCheck:on
move current.html .\output\pages
echo del diagram.txt
echo del diagram.cmapx

pause