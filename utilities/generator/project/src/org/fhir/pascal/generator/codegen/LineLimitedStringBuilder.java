package org.fhir.pascal.generator.codegen;

import org.hl7.fhir.utilities.Utilities;

public class LineLimitedStringBuilder {
  private int lineLength;
  private StringBuilder b = new StringBuilder();
  private int indent;
  
  public LineLimitedStringBuilder(int lineLength, int indent) {
    super();
    this.lineLength = lineLength;
    this.indent = indent;
  }

  public void append(String s) {
    if (lineLength + s.length() > 1020) {
      b.append("\r\n"+Utilities.padLeft("", ' ', indent));
      lineLength = indent;
    }
    b.append(s);
    lineLength = lineLength + s.length();
  }

  @Override
  public String toString() {
    return b.toString();
  }
  

}
