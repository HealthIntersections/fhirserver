package org.fhir.delphi.misc;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.hl7.fhir.utilities.TextFile;

public class FixConvertor {

  public static void main(String[] args) throws IOException {
    new FixConvertor().fix("C:\\work\\fhirserver\\library\\xversion\\FHIR.XVersion.Conv_30_40.pas");
  }

  private void fix(String filename) throws IOException {
    List<String> lines = loadFile(filename); 
    Boolean is3 = null;
    Boolean impl = false;
    for (int i = 0; i < lines.size(); i++) {
      String line = lines.get(i);
      if ("end;".equals(line)) {
        is3 = null;
      } else if ("implementation".equals(line)) {
        impl = true;
      } else if (!impl) {
        // do nothing
      } else if (line.contains("TVersionConvertor_30_40.convert") || line.contains("TVersionConvertor_30_40.copy") ) {
        int index = line.indexOf("(");
        is3 = line.charAt(index-1) == '3';
      } else if (line.contains("convert")) {
        if (is3 == null) {
          System.out.println("fail at line "+i+"; no mode");
        }
        int j = line.indexOf("convert");
        while (j < line.length() && line.charAt(j) != '(') {
          j++;
        }
        char ch = line.charAt(j-1);
        if (ch != '3' && ch != '4') {
          lines.set(i, line.substring(0, j)+(is3 ? "3" : "4")+line.substring(j));
        } else if ((is3 && ch == '4') || (!is3 && ch == '3'))
          lines.set(i, line.substring(0, j-1)+(is3 ? "3" : "4")+line.substring(j));
      } else if (line.contains("copy")) {
        if (is3 == null) {
          System.out.println("fail at line "+i+"; no mode");
        }
        int j = line.indexOf("copy");
        while (j < line.length() && line.charAt(j) != '(') {
          j++;
        }
        char ch = line.charAt(j-1);
        if (ch != '3' && ch != '4') {
          lines.set(i, line.substring(0, j)+(is3 ? "3" : "4")+line.substring(j));
        } else if ((is3 && ch == '4') || (!is3 && ch == '3'))
          lines.set(i, line.substring(0, j-1)+(is3 ? "3" : "4")+line.substring(j));
      }
    }    
    StringBuilder b = new StringBuilder();
    for (String line : lines) {
      b.append(line);
      b.append("\r\n");
    }
    TextFile.stringToFile(b.toString(), filename);
  }

  private List<String> loadFile(String filename) throws IOException {
    BufferedReader bufReader = new BufferedReader(new FileReader(filename)); 
    ArrayList<String> listOfLines = new ArrayList<>(); 
    String line = bufReader.readLine(); 
    while (line != null) { 
      listOfLines.add(line); 
      line = bufReader.readLine(); 
    } 
    bufReader.close();

    return listOfLines;
  }

}
