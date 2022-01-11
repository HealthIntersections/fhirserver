package org.fhir.pascal.misc;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import org.hl7.fhir.exceptions.FHIRFormatError;
import org.hl7.fhir.r5.formats.JsonParser;
import org.hl7.fhir.r5.model.Bundle;
import org.hl7.fhir.r5.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.utilities.Utilities;

public class SearchParamMover {
  
  public static void main(String[] args) throws Exception {
    SearchParamMover self = new SearchParamMover(args[0], args[1]);
    self.execute();        
  }

  private String source;
  private String dest;

  public SearchParamMover(String source, String dest) {
    this.source = source;
    this.dest = dest;
  }

  private void execute() throws FHIRFormatError, FileNotFoundException, IOException {
    Bundle bnd = (Bundle) new JsonParser().parse(new FileInputStream(source));
    for (BundleEntryComponent be : bnd.getEntry()) {
      if (be.getResource().fhirType().equals("SearchParameter")) {
        String fn = Utilities.path(dest, "SearchParameter-"+be.getResource().getId()+".json");
        new JsonParser().compose(new FileOutputStream(fn), be.getResource());
      }
    }
    
  }

}
