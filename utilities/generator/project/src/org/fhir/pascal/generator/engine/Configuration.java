package org.fhir.pascal.generator.engine;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.hl7.fhir.utilities.IniFile;
import org.hl7.fhir.utilities.TextFile;
import org.hl7.fhir.utilities.Utilities;

public class Configuration {

  public static final SimpleDateFormat DATE_FORMAT() {
    return new SimpleDateFormat("EEE, MMM d, yyyy HH:mmZ", new Locale("en", "US"));
  }

  private Map<String, String> templates = new HashMap<>();
  private String source;
  private String dest;
  private IniFile ini;
  
  public Configuration(String templatePath, String source, String dest) throws FileNotFoundException, IOException {
    this.source = source;
    this.dest = dest;
    System.out.println("Loading Templates");
    for (File jfn : new File(templatePath).listFiles()) {
      System.out.println(" .. "+jfn.getName());
      templates.put(jfn.getName(), TextFile.fileToString(jfn));
    }
    ini = new IniFile(Utilities.path(templatePath, "configuration.ini"));
  }
  
  public String getTemplate(String name) {
    if (templates.containsKey(name))
      return templates.get(name);
    if (templates.containsKey(name+".pas"))
      return templates.get(name+".pas");
    throw new Error("unable to find "+name);
  }
    
  public String masterSource() {
    return source;
  }
    
  public String destination() {
    return dest;
  }

  public boolean hasTemplate(String name) {
    if (templates.containsKey(name))
      return true;
    if (templates.containsKey(name+".pas"))
      return true;
    return false;
  }

  public IniFile getIni() {
    return ini;
  }
  
  
}
