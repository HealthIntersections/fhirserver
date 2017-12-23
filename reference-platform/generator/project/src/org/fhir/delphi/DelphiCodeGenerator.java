package org.fhir.delphi;

/*
Copyright (c) 2011+, HL7, Inc
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this 
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice, 
   this list of conditions and the following disclaimer in the documentation 
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to 
   endorse or promote products derived from this software without specific 
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
POSSIBILITY OF SUCH DAMAGE.

 */
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

public class DelphiCodeGenerator extends OutputStreamWriter {

  public static final String FULL_LICENSE_CODE = 

      "  Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)\r\n"+
          "  All rights reserved.\r\n"+
          "  \r\n"+
          "  Redistribution and use in source and binary forms, with or without modification, \r\n" +
          "  are permitted provided that the following conditions are met:\r\n"+
          "  \r\n"+
          "   * Redistributions of source code must retain the above copyright notice, this \r\n" +
          "     list of conditions and the following disclaimer.\r\n"+
          "   * Redistributions in binary form must reproduce the above copyright notice, \r\n" +
          "     this list of conditions and the following disclaimer in the documentation \r\n" +
          "     and/or other materials provided with the distribution.\r\n"+
          "   * Neither the name of HL7 nor the names of its contributors may be used to \r\n"+
          "     endorse or promote products derived from this software without specific \r\n"+
          "     prior written permission.\r\n"+
          "  \r\n"+
          "  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\" AND \r\n" +
          "  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED \r\n" +
          "  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. \r\n" +
          "  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, \r\n" +
          "  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT \r\n" +
          "  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR \r\n" +
          "  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, \r\n" +
          "  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) \r\n" +
          "  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE \r\n" +
          "  POSSIBILITY OF SUCH DAMAGE.\r\n"+
          "  \r\n";

  public static final String VERSION_MARK = 
      "{$IFNDEF FHIR%%}\r\n"+
          "This is the dstu%% version of the FHIR code\r\n"+
          "{$ENDIF}\r\n";


  // fragments
  public String name;
  public List<String> uses = new ArrayList<String>();
  public List<String> usesImpl = new ArrayList<String>();
  public List<String> comments = new ArrayList<String>();
  public List<String> precomments = new ArrayList<String>();
  public List<String> enumDefs = new ArrayList<String>();
  public List<String> enumConsts = new ArrayList<String>();
  public List<String> enumProcs = new ArrayList<String>();
  public List<String> classFwds = new ArrayList<String>();
  public List<String> classDefs = new ArrayList<String>();
  public List<String> classImpls = new ArrayList<String>();
  public List<String> inits = new ArrayList<String>();
  public List<String> procs = new ArrayList<String>();
  public List<String> procsPub = new ArrayList<String>();

  private String dstuID;

  private String sfxDstuID;

  public DelphiCodeGenerator(OutputStream out, String dstuID, String sfxDstuID) throws UnsupportedEncodingException {
    super(out, "ASCII");
    this.dstuID = dstuID;
    this.sfxDstuID = sfxDstuID;
  }

  public void start() throws Exception {
  }

  public static boolean noString(String v) {
    return v == null || v.equals("");
  }


  public String escape(String v) {
    if (noString(v))
      return "";

    StringBuilder s = new StringBuilder();
    for (char c : v.toCharArray())
      if (c == '\'')
        s.append("''");
      else
        s.append(c);
    String r = s.toString();
    int i = 250;
    while (i < r.length()) {
      r = r.substring(0, i)+"'+'"+r.substring(i);
      i = i + 253;
    }
    return r;
  }

  public void finish() throws Exception {
    if (precomments.size() > 0) {
      for (int i = 0; i < precomments.size(); i++) {
        if (precomments.get(i).charAt(0) == '!')
          write("{"+precomments.get(i)+"}\r\n");
        else
          write("// "+precomments.get(i)+"\r\n");
      }
      write("\r\n");
    }
    write("unit "+name+sfxDstuID+";\r\n");
    write("\r\n");
    write("{$I fhir.inc}\r\n");
    write("\r\n");
    write("{\r\n"+FULL_LICENSE_CODE+"}\r\n");
    write("\r\n"+VERSION_MARK.replace("%%", dstuID)+"\r\n");
    write("\r\n");
    write("interface\r\n");
    write("\r\n");

    for (int i = 0; i < comments.size(); i++) {
      if (comments.get(i).charAt(0) == '!')
        write("{"+comments.get(i)+"}\r\n");
      else
        write("// "+comments.get(i)+"\r\n");
    }
    write("\r\n");

    write("uses\r\n");
    write("  ");
    for (int i = 0; i < uses.size(); i++) {
      if (i > 0)
        write(", ");
      write(uses.get(i));
    }
    write(";\r\n");
    write("\r\n");

    if (enumDefs.size() > 0) {
      write("Type\r\n");

      for (String s : enumDefs) {
        write(s+"\r\n");
      }
    }

    if (classDefs.size() > 0) {
      write("Type\r\n");
      for (String s : classFwds) {
        write(s);
      }
      write("\r\n");
      for (String s : classDefs) {
        write(s+"\r\n");
      }
    }
    if (enumConsts.size() > 0 || enumProcs.size() > 0) {

      write("Const\r\n");
      for (String s : enumConsts) {
        write(s+"\r\n");
      }
      write("\r\n");

      for (String s : enumProcs) {
        write(s+"\r\n");
      }
      write("\r\n");
    }
    if (procsPub.size() > 0) {
      for (String s : procsPub) {
        write(s);
      }
      write("\r\n");
    }
    write("implementation\r\n");
    write("\r\n");
    if (!usesImpl.isEmpty()) {
      write("uses\r\n");
      write("  ");
      for (int i = 0; i < usesImpl.size(); i++) {
        if (i > 0)
          write(", ");
        write(usesImpl.get(i));
      }
      write(";\r\n");
      write("\r\n");
    }
    for (String s : classImpls) {
      write(s+"\r\n");
    }

    for (String s : procs) {
      write(s+"\r\n");
    }

    if (inits.size() > 0) {
      write("initialization;\r\n");
      for (String s : inits) {
        write("  "+s+"\r\n");
      }
    }
    write("end.\r\n");
    write("\r\n");
    flush();
    close();
  }

  public void ifdef(String n) {
    classFwds.add("{$IFDEF FHIR_"+n+"}\r\n");
    classDefs.add("{$IFDEF FHIR_"+n+"}\r\n");
    classImpls.add("{$IFDEF FHIR_"+n+"}\r\n");

  }

  public void endif(String n) {
    classFwds.add("{$ENDIF FHIR_"+n+"}\r\n");
    classDefs.add("{$ENDIF FHIR_"+n+"}\r\n");
    classImpls.add("{$ENDIF FHIR_"+n+"}\r\n");
  }



}
