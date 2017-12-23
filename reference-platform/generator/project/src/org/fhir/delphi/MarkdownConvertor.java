package org.fhir.delphi;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.hl7.fhir.utilities.DirectoryIterator;
import org.hl7.fhir.utilities.TextFile;
import org.hl7.fhir.utilities.DirectoryIterator.FileProcessor;
import org.hl7.fhir.utilities.Utilities;

public class MarkdownConvertor implements FileProcessor {

  public static void main(String[] args) throws Exception {
    File[] dirs = new File("C:\\temp\\flexmark\\flexmark-java-master").listFiles();
    for (File d : dirs) {
      if ((d.getName().equals("flexmark") || Utilities.existsInList(d.getName(), "??")) && d.isDirectory()) {
        MarkdownConvertor self = new MarkdownConvertor();
        self.processPackage(d);
      }
    }
  }

  List<String> names = new ArrayList<>();
  StringBuilder i;
  StringBuilder c;
  
  private void processPackage(File d) throws Exception {
    System.out.println("Produce "+pascalify(d.getName()));
    i = new StringBuilder();
    c = new StringBuilder();
    new DirectoryIterator(Utilities.path(d.getAbsolutePath(), "src", "main"), ".*\\.java", this);
    String srcI = i.toString();
    String srcC = c.toString();
    for (String n : names) {
      srcI = srcI.replaceAll("\\b"+n+"\\b", "T"+n);
      srcC = srcC.replaceAll("\\b"+n+"\\b", "T"+n);
    }
    TextFile.stringToFile(srcI, "C:\\work\\markdown\\source\\"+pascalify(d.getName())+"-template-I.pas");
    TextFile.stringToFile(srcC, "C:\\work\\markdown\\source\\"+pascalify(d.getName())+"-template-C.pas");
  }

  private static String pascalify(String name) {
    if (name.equals("flexmark"))
      return "FlexMark";
    
    String s = name.substring(9);
    StringBuilder b = new StringBuilder();
    boolean up = true;
    for (char c : s.toCharArray()) {
      if (c == '-')
        up = true;
      else {
        if (up)
          b.append(Character.toUpperCase(c));
        else
          b.append(c);
        up = false;
      }
    }
    return "FlexMark"+b.toString();
  }

  @Override
  public void process(File f) throws Exception {
//    System.out.println("  "+f.getAbsolutePath());
    String src = TextFile.fileToString(f.getAbsolutePath());
    
    String n = f.getName();
    n = n.substring(0, n.indexOf("."));
    names.add(n);
    src = src.replace(" && ", ") and (");
    src = src.replace(" || ", ") or (");
    src = src.replace("}", "end;");
    src = src.replace("{", "begin");
    src = src.replaceAll("\\bout\\b", "res");
    src = src.replaceAll("\\bint\\b", "integer");
    src = src.replaceAll("\\bBasedSequence\\b", "IBasedSequence");
    src = src.replace(" return ", " exit(");
    src = src.replace(" == ", " ~~ ");
    src = src.replace(" = ", " := ");
    src = src.replace(" ~~ ", " = ");
    src = src.replace("\"", "'");
    i.append("// from "+f.getAbsolutePath()+"\r\n");
    c.append("// from "+f.getAbsolutePath()+"\r\n");
    i.append("  "+n+" = class ()\r\n");
    String[] lines = src.split("\\r?\\n");
    boolean prot = false;
    i.append("  private\r\n");
    for (String l : lines) {
      String s = l.trim();
      l = l.replace(" begin", ";");
      if (s.startsWith("private ")) {
        l = l.replace("private void ", "procedure ");
        if (l.endsWith(");")) {
          l = l.replace("public ", "function ");
          if (l.contains("function "))
            l = moveType(l);
          l = flipParams(l);
        } else 
          l = l.replace("private ", "");
        i.append(l+"\r\n");
      }
      if (s.startsWith("protected ")) 
        prot = true;
    }
    if (prot) {
      i.append("  protected\r\n");
      for (String l : lines) {
        String s = l.trim();
        l = l.replace(" begin", ";");
        if (s.startsWith("protected ")) {
          l = l.replace("protected void ", "procedure ");
          if (l.endsWith(");")) {
            l = l.replace("protected ", "function ");
            if (l.contains("function "))
              l = moveType(l);
            l = flipParams(l);
          } else 
            l = l.replace("protected ", "");
          i.append(l+"\r\n");
        }
      }      
    }
    i.append("  public\r\n");
    for (String l : lines) {
      String s = l.trim();
      l = l.replace(" begin", ";");
      if (s.startsWith("public ")) {
        if (s.startsWith("public "+n+"(")) 
          l = l.replace("public "+n+"(", "constructor create(")+" overload;";
        l = l.replace("public void ", "procedure ");
        if (l.endsWith(");")) {
          l = l.replace("public ", "function ");
          if (l.contains("function "))
            l = moveType(l);
          l = flipParams(l);
        } else 
          l = l.replace("public ", "");
        i.append(l+"\r\n");
      }
    }
    i.append("  end;\r\n\r\n");
    src = src.replace("public void ", "procedure ");
    src = src.replace("private void ", "procedure ");
    src = src.replace("protected void ", "procedure ");

    src = src.replace("public "+n+"(", "constructor create(");
    src = src.replace("private "+n+"(", "constructor create(");
    src = src.replace("protected "+n+"(", "constructor create(");
    
    src = src.replace("public ", "function ");
    src = src.replace("private ", "function ");
    src = src.replace("protected ", "function ");
    
    c.append(src);
    
  }

  private String flipParams(String l) {
    return l;
  }

  private String moveType(String l) {
    System.out.println(l);
    int i = l.indexOf("function ")+9;
    int j = i;
    while (l.charAt(j) != ' ') {
      j++;
      if (j == l.length())
        return l;
    }
    return l.substring(0, i) + l.substring(j+1, l.length()-1)+" : "+l.substring(i, j)+";";
  }

  
}
