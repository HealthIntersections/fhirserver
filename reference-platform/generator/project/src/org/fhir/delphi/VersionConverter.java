package org.fhir.delphi;

import org.hl7.fhir.dstu2.model.DateTimeType;
import org.hl7.fhir.dstu2.model.Enumerations.SearchParamType;
import org.hl7.fhir.dstu2.model.SearchParameter.XPathUsageType;

public class VersionConverter {

  public static DateTimeType convert(org.hl7.fhir.dstu21.model.DateTimeType dateElement) {
    return new DateTimeType(dateElement.toCalendar());
  }

  public static SearchParamType convert(org.hl7.fhir.dstu21.model.Enumerations.SearchParamType type) {
    if (type == null)
      return null;
    switch (type) {
    case NUMBER: return SearchParamType.NUMBER; 
    case DATE : return SearchParamType.DATE;
    case STRING : return SearchParamType.STRING;
    case TOKEN : return SearchParamType.TOKEN;
    case REFERENCE : return SearchParamType.REFERENCE;
    case COMPOSITE : return SearchParamType.COMPOSITE;
    case QUANTITY : return SearchParamType.QUANTITY;
    case URI: return SearchParamType.URI;
    default: return SearchParamType.NULL;
    }
  }

  public static XPathUsageType convert(org.hl7.fhir.dstu21.model.SearchParameter.XPathUsageType usage) {
    if (usage == null)
      return null;
    switch (usage) {
    case NORMAL: return XPathUsageType.NORMAL;
    case PHONETIC: return XPathUsageType.PHONETIC;
    case NEARBY: return XPathUsageType.NEARBY;
    case DISTANCE: return XPathUsageType.DISTANCE;
    case OTHER: return XPathUsageType.OTHER;
    default: return XPathUsageType.NULL;
    }
  }


}
