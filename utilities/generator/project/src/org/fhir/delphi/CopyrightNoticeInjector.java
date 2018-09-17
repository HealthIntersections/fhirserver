package org.fhir.delphi;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import org.hl7.fhir.utilities.TextFile;

public class CopyrightNoticeInjector {

  public static void main(String[] args) throws FileNotFoundException, IOException {
    new CopyrightNoticeInjector().scanFiles(new File("C:\\work\\fhirserver"));
  }

  private void scanFiles(File dir) throws FileNotFoundException, IOException {
    if (dir.getAbsolutePath().contains("Libraries\\treeview"))
      return;
    if (dir.getAbsolutePath().contains("Libraries\\jcl"))
      return;
    if (dir.getAbsolutePath().contains("Libraries\\indysoap"))
      return;
    if (dir.getAbsolutePath().contains("Libraries\\FMM"))
      return;
    if (dir.getAbsolutePath().contains("Libraries\\db"))
      return;
    if (dir.getAbsolutePath().contains("fhirserver\\npp\\npplib"))
      return;
      
    for (File f : dir.listFiles()) {
      if (f.isDirectory())
        scanFiles(f);
      else if (f.getName().endsWith(".pas"))
        scanFile(f, false);
      else if (f.getName().endsWith(".dpr"))
        scanFile(f, true);
    }
  }

  private void scanFile(File f, boolean isDpr) throws FileNotFoundException, IOException {
    String s = TextFile.fileToString(f);
    String n = s;
    if (!s.contains("Copyright (c)")) {
//      System.out.println("File "+f.getAbsolutePath()+" needs a copyright notice");
      int i = s.indexOf("interface");
      if (i == -1)
        i = s.indexOf("Interface");
      if (isDpr) {
        if (i == -1)
          i = s.indexOf("program ");
        if (i == -1)
          i = s.indexOf("Program ");
        if (i == -1)
          i = s.indexOf("library ");
        if (i == -1)
          i = s.indexOf("Library ");
      }
      if (i == -1)
        System.out.println("can't make changes to "+f.getAbsolutePath());
      else {
        s = s.substring(0, i) + copyright()+s.substring(i);
      }
    }    
    s = s.replace("\r\n", "\r");
    s = s.replace("\n", "\r");
    s = s.replace("\r", "\r\n");
    if (!s.equals(n)) {
      System.out.println("make changes to "+f.getAbsolutePath());
      TextFile.stringToFile(s, f, false);
    }
  }

  private String copyright() {
    
    return "{\r\n" + 
        "Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)\r\n" + 
        "All rights reserved.\r\n" + 
        "\r\n" + 
        "Redistribution and use in source and binary forms, with or without modification,\r\n" + 
        "are permitted provided that the following conditions are met:\r\n" + 
        "\r\n" + 
        " * Redistributions of source code must retain the above copyright notice, this\r\n" + 
        "   list of conditions and the following disclaimer.\r\n" + 
        " * Redistributions in binary form must reproduce the above copyright notice,\r\n" + 
        "   this list of conditions and the following disclaimer in the documentation\r\n" + 
        "   and/or other materials provided with the distribution.\r\n" + 
        " * Neither the name of HL7 nor the names of its contributors may be used to\r\n" + 
        "   endorse or promote products derived from this software without specific\r\n" + 
        "   prior written permission.\r\n" + 
        "\r\n" + 
        "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND\r\n" + 
        "ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED\r\n" + 
        "WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.\r\n" + 
        "IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,\r\n" + 
        "INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT\r\n" + 
        "NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR\r\n" + 
        "PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,\r\n" + 
        "WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)\r\n" + 
        "ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE\r\n" + 
        "POSSIBILITY OF SUCH DAMAGE.\r\n" + 
        "}\r\n" + 
        "";
  }

}
