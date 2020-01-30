package org.fhir.pascal.generator.codegen;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.fhir.pascal.generator.analysis.Analysis;
import org.fhir.pascal.generator.analysis.EnumInfo;
import org.fhir.pascal.generator.analysis.TypeInfo;
import org.fhir.pascal.generator.engine.Configuration;
import org.fhir.pascal.generator.engine.Definitions;
import org.hl7.fhir.r5.model.CodeSystem;
import org.hl7.fhir.r5.model.CodeSystem.ConceptDefinitionComponent;
import org.hl7.fhir.r5.model.ElementDefinition;
import org.hl7.fhir.r5.model.ElementDefinition.ElementDefinitionBindingComponent;
import org.hl7.fhir.r5.model.ElementDefinition.TypeRefComponent;
import org.hl7.fhir.r5.model.Enumerations.BindingStrength;
import org.hl7.fhir.r5.model.StructureDefinition;
import org.hl7.fhir.r5.model.StructureDefinition.StructureDefinitionKind;
import org.hl7.fhir.r5.model.ValueSet;
import org.hl7.fhir.r5.model.ValueSet.ConceptSetComponent;
import org.hl7.fhir.utilities.Utilities;


public class BaseGenerator {
  private static final boolean MARKERS = true;

  protected Definitions definitions;
  protected Configuration config;
  protected String version;
  protected Date genDate;
  protected Map<String, String> simpleTypes = new HashMap<String, String>();

  public static class PascalArrayEntry {
    private String constant;
    private String define;
    private String comment;
    public PascalArrayEntry(String constant, String define, String comment) {
      super();
      this.constant = constant;
      this.define = define;
      this.comment = comment;
    }
    public String getConstant() {
      return constant;
    }
    public String getDefine() {
      return define;
    }
    public String getComment() {
      return comment;
    }
    
  }

  public static class PascalArrayBuilder {
    private List<PascalArrayEntry> entries = new ArrayList<BaseGenerator.PascalArrayEntry>();
    private String name;
    private String type;
    private boolean oneEntryPerLine;
    private String itemType;
    private String define;
    
    public PascalArrayBuilder(String type) {
      super();
      this.itemType = "String";
      this.name = "CODES_"+type;
      this.type = type;
    }
    public PascalArrayBuilder(String name, String type) {
      super();
      this.itemType = "String";
      this.name = name;
      this.type = type;
    }
    public PascalArrayBuilder(String type, boolean oneEntryPerLine) {
      super();
      this.itemType = "String";
      this.name = "CODES_"+type;
      this.type = type;
      this.oneEntryPerLine = oneEntryPerLine;   
    }
    public PascalArrayBuilder(String name, String type, boolean oneEntryPerLine) {
      super();
      this.itemType = "String";
      this.name = name;
      this.type = type;
      this.oneEntryPerLine = oneEntryPerLine;   
    }
    public List<PascalArrayEntry> getEntries() {
      return entries;
    }
    public String getName() {
      return name;
    }
    public String getType() {
      return type;
    }
    
    public String getDefine() {
      return define;
    }
    public void setDefine(String define) {
      this.define = define;
    }
    public String getItemType() {
      return itemType;
    }
    public void setItemType(String itemType) {
      this.itemType = itemType;
    }
    public boolean isOneEntryPerLine() {
      return oneEntryPerLine;
    }
    public void setOneEntryPerLine(boolean oneEntryPerLine) {
      this.oneEntryPerLine = oneEntryPerLine;
    }
    public void addEntry(String constant) {
      entries.add(new PascalArrayEntry(constant, null, null));
    }
    public void addEntry(String constant, String define, String comment) {
      entries.add(new PascalArrayEntry(constant, define, comment));
    }
    public void addEntryDefine(String constant, String define) {
      entries.add(new PascalArrayEntry(constant, define, null));
    }
    public void addEntryComment(String constant, String comment) {
      entries.add(new PascalArrayEntry(constant, null, comment));
    }
    
    public String build() {
      StringBuilder b = new StringBuilder();
      if (define != null) {
        b.append("{$IFDEF "+define+"}\r\n");
      }
      if (isOneEntryPerLine()) {
        b.append("const\r\n");
        b.append("  "+name+" : Array["+type+"] of "+itemType+" = (\r\n");
        int i = 1;
        for (PascalArrayEntry e : entries) {
          b.append("    ");
          if (e.define != null) {
            b.append("{$IFDEF "+e.define+"} ");
          }
          if ("String".equals(itemType)) {
            b.append("'");
          }
          b.append(e.constant);
          if ("String".equals(itemType)) {
            b.append("'");
          }
          b.append(i == entries.size() ? ");" : ","); 
          if (e.define != null) {
            b.append(" {$ENDIF}");
          }
          if (e.comment != null) {
            b.append(" // "+e.comment);
          }
          b.append("\r\n");
          i++;
        }
        b.append("\r\n");
      } else {
        b.append("  "+name+" : Array["+type+"] of "+itemType+" = (");
        int i = 1;
        int l = name.length()+type.length()+itemType.length()+19;
        for (PascalArrayEntry e : entries) {
          int il = 3+("String".equals(itemType) ? 2 : 0)+e.constant.length()+(e.define == null ? 0 : e.define.length()+20)+(e.comment == null ? 0 : e.comment.length()+3);
          if (il+l > 1019) {
            b.append("\r\n      ");
            l = 6;
          }
          if (i != 1) {
            b.append(" ");
            l++;
          }
          if (e.define != null) {
            b.append("{$IFDEF "+e.define+"} ");
            l = l + 10 + e.define.length();
          }
          if ("String".equals(itemType)) {
            b.append("'");
            l++;
          }
          b.append(e.constant);
          l = l + e.constant.length();
          if ("String".equals(itemType)) {
            b.append("'");
            l++;
          }
          b.append(i == entries.size() ? ");" : ","); 
          l++;
          if (e.define != null) {
            b.append(" {$ENDIF}");
            l = l + 10;
          }
          if (e.comment != null) {
            b.append(" {"+e.comment+"}");
            l = l + e.comment.length()+3;
          }
          i++;
        }
        b.append("\r\n");
      }
      if (define != null) {
        b.append("{$ENDIF}\r\n");
      }
      return b.toString();
    }
  }
  
  public BaseGenerator(Definitions definitions, Configuration config, String version, Date genDate) throws UnsupportedEncodingException {
    super ();
    this.definitions = definitions;
    this.config = config;
    this.version = version;
    this.genDate = genDate;
    init();
  }

  private void init() {
    simpleTypes.put("TFhirDate", "TFslDateTime");
    simpleTypes.put("TFhirDateTime", "TFslDateTime");
    simpleTypes.put("TFhirString", "String");
    simpleTypes.put("TFhirInteger", "String");
    simpleTypes.put("TFhirInteger64", "String");
    simpleTypes.put("TFhirUri", "String");
    simpleTypes.put("TFhirInstant", "TFslDateTime");
    simpleTypes.put("TFhirXhtml", "String");
    simpleTypes.put("TFhirBoolean", "Boolean");
    simpleTypes.put("TFhirBase64Binary", "TBytes");
    simpleTypes.put("TFhirTime", "String");
    simpleTypes.put("TFhirDecimal", "String");
    simpleTypes.put("TFhirCode", "String");
    simpleTypes.put("TFhirCanonical", "String");
    simpleTypes.put("TFhirOid", "String");
    simpleTypes.put("TFhirUuid", "String");
    simpleTypes.put("TFhirUrl", "String");
    simpleTypes.put("TFhirMarkdown", "String");
    simpleTypes.put("TFhirUnsignedInt", "String");
    simpleTypes.put("TFhirId", "String");
    simpleTypes.put("TFhirPositiveInt", "String");
  }

  public String startVMarkValue() {
    return "// Generated on "+config.DATE_FORMAT().format(genDate)+" for FHIR v"+version+"\r\n\r\n";
//    return "// Generated on Thu, Dec 13, 2018 14:07+1100 for FHIR v4.0.0\r\n\r\n";
  }


  public static boolean isJavaReservedWord(String word) {
    if (word.equals("abstract")) return true;   
    if (word.equals("assert")) return true;
    if (word.equals("boolean")) return true;
    if (word.equals("break")) return true;  
    if (word.equals("byte")) return true;   
    if (word.equals("case")) return true;
    if (word.equals("catch")) return true;  
    if (word.equals("char")) return true;   
    if (word.equals("class")) return true;  
    if (word.equals("const")) return true;  
    if (word.equals("continue")) return true;   
    if (word.equals("default")) return true;
    if (word.equals("double")) return true;   
    if (word.equals("do")) return true;   
    if (word.equals("else")) return true;   
    if (word.equals("enum")) return true;   
    if (word.equals("extends")) return true;  
    if (word.equals("false")) return true;
    if (word.equals("final")) return true;  
    if (word.equals("finally")) return true;  
    if (word.equals("float")) return true;  
    if (word.equals("for")) return true;  
    if (word.equals("goto")) return true;   
    if (word.equals("if")) return true;
    if (word.equals("implements")) return true;   
    if (word.equals("import")) return true;   
    if (word.equals("instanceof")) return true;   
    if (word.equals("int")) return true;  
    if (word.equals("interface")) return true;  
    if (word.equals("long")) return true;
    if (word.equals("native")) return true;   
    if (word.equals("new")) return true;  
    if (word.equals("null")) return true;   
    if (word.equals("package")) return true;  
    if (word.equals("private")) return true;  
    if (word.equals("protected")) return true;
    if (word.equals("public")) return true;   
    if (word.equals("return")) return true;   
    if (word.equals("short")) return true;  
    if (word.equals("static")) return true;   
    if (word.equals("strictfp")) return true;   
    if (word.equals("super")) return true;
    if (word.equals("switch")) return true;   
    if (word.equals("synchronized")) return true;   
    if (word.equals("this")) return true;   
    if (word.equals("throw")) return true;  
    if (word.equals("throws")) return true;   
    if (word.equals("transient")) return true;
    if (word.equals("true")) return true;   
    if (word.equals("try")) return true;  
    if (word.equals("void")) return true;   
    if (word.equals("volatile")) return true;
    if (word.equals("while")) return true;
    if (word.equals("Exception")) return true;
    return false;
  }
 
  protected boolean isJavaPrimitive(ElementDefinition e) {
    return e.getType().size() == 1 && (isPrimitive(e.getType().get(0).getWorkingCode()));
  }

  protected boolean isPrimitive(String name) {
    return definitions.getStructures().has(typeNs(name)) && definitions.getStructures().get(typeNs(name)).getKind() == StructureDefinitionKind.PRIMITIVETYPE;
  }

	private String typeNs(String name) {
    return "http://hl7.org/fhir/StructureDefinition/"+name;
  }

  protected String getElementName(String name, boolean alone) {
	  if (name.equals("[type]"))
	    return "value";
	  else if ((alone && isJavaReservedWord(name)) || (!alone && name.equals("class")))
	    return name+"_";
	  else if (name.equals("[x]"))
      return "value";
	  else
	    return name.replace("[x]", "");
	}

	protected String getTypeName(ElementDefinition e) throws Exception {
		if (e.getType().size() > 1) {
			return "DataType";
		} else if (e.getType().size() == 0) {
			throw new Exception("not supported");
		} else {
			return getTypename(e.getType().get(0));
		}
	}

	protected String getTypename(TypeRefComponent type) throws Exception {
	  if (type.hasExtension("http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type")) {
	    return type.getExtensionString("http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type");
	  } else {
		  return getTypeName(type.getCode());
	  }
	}

	protected String getTypeName(String tn) {
		if (tn.equals("string")) {
			return "TFhirString";
		} else if (tn.equals("Any")) {
			return "TFhirReference";
    } else if (isPrimitive(tn)) {
      return "TFhir"+getTitle(tn);
		} else {
			return "TFhir"+getTitle(tn);
		}
	}

	protected String getTitle(String name) {
		return Utilities.noString(name) ? "Value" : name.substring(0, 1).toUpperCase()+ name.substring(1);
	}


  protected List<ConceptDefinitionComponent> listAllCodes(CodeSystem cs) {
    List<ConceptDefinitionComponent> result = new ArrayList<ConceptDefinitionComponent>();
    addAllCodes(result, cs.getConcept());
    return result;
  }

  private void addAllCodes(List<ConceptDefinitionComponent> result, List<ConceptDefinitionComponent> concept) {
    for (ConceptDefinitionComponent c : concept) {
      result.add(c);
      addAllCodes(result, c.getConcept());
    }
  }

  protected String makeConst(String cc) {
    if (cc.equals("*"))
      cc = "ASTERISK";
    if (Utilities.isOid(cc))
      cc = "OID_"+cc;
    if (cc.equals("%"))
      cc = "pct";
    else if (cc.equals("<"))
      cc = "less_Than";
    else if (cc.equals("<="))
      cc = "less_Or_Equal";
    else if (cc.equals(">"))
      cc = "greater_Than";
    else if (cc.equals(">="))
      cc = "greater_Or_Equal";
    else if (cc.equals("="))
      cc = "equal";
    else if (cc.equals("!="))
      cc = "not_equal";
    else if (allPlusMinus(cc))
      cc = cc.replace("-", "Minus").replace("+", "Plus");
    else
      cc = cc.replace("-", "").replace("+", "");
    cc = cc.replace("(", "_").replace(")", "_");
    cc = cc.replace("{", "_").replace("}", "_");
    cc = cc.replace("<", "_").replace(">", "_");
    cc = cc.replace(".", "_").replace("/", "_");
    cc = cc.replace(":", "_");
    cc = cc.replace("%", "pct");
    if (Utilities.isInteger(cc.substring(0, 1)))
      cc = "_"+cc;
    cc = cc.toUpperCase();
    if (isJavaReservedWord(cc))
      cc = cc + "_";
    return cc;
  }

  private boolean allPlusMinus(String cc) {
    for (char c : cc.toCharArray())
      if (!(c == '-' || c == '+'))
        return false;
    return true;
  }

  protected boolean isEnum(ElementDefinitionBindingComponent cd) {
    boolean ok = cd != null && cd.getStrength() == BindingStrength.REQUIRED;
    if (ok) {
      if (cd.getValueSet() != null) {
        ValueSet vs = definitions.getValuesets().get(cd.getValueSet()); 
        if (vs != null && vs.hasCompose() && vs.getCompose().getInclude().size() == 1) {
          ConceptSetComponent inc = vs.getCompose().getIncludeFirstRep();
          if (inc.hasSystem() && !inc.hasFilter() && !inc.hasConcept() && !(inc.getSystem().startsWith("http://hl7.org/fhir") || inc.getSystem().startsWith("http://terminology.hl7.org")))
            ok = false;
        }
      }
    }
    return ok;
  }

  protected String getCodeListType(String binding) {
    StringBuilder b = new StringBuilder();
    boolean up = true;
    for (char ch: binding.toCharArray()) {
      if (ch == '-' || ch == ' ' || ch == '.')
        up = true;
      else if (up) {
        b.append(Character.toUpperCase(ch));
        up = false;
      }
      else        
        b.append(ch);
    }
    return "ResourceType".equals(b.toString()) ? "ResourceTypeEnum" : b.toString();
  }
  
  
  protected ElementDefinition matchingInheritedElement(List<ElementDefinition> children, ElementDefinition m) {
    if (children == null) {
      return null;
    }
    String mtail = m.getPath().substring(m.getPath().indexOf("."));
    for (ElementDefinition t : children) {
      String ttail = t.getPath().substring(t.getPath().indexOf("."));
      if (ttail.equals(mtail)) {
        return t;
      }
      
    }
    return null;
  }

  
  protected void line(StringBuilder b, String line) {
    b.append(line+"\r\n");
  }

  protected void frag(StringBuilder b, String line) {
    b.append(line);
  }

  protected String getDefineName(StructureDefinition sd) {
    return "FHIR_"+sd.getType().toUpperCase();
  }

  protected String makeDocoSafe(String string, String prefix) {
    if (string == null)
      return "";
    string = Utilities.normaliseEolns(string).replace("\r\n", " ");
    while (string.contains("]{") && string.contains("}") && string.indexOf("]{") < string.indexOf("}")) {
      string = string.substring(0, string.indexOf("]{")+1)+string.substring(string.indexOf("}")+1);
    }
    string = string.replace("}", "))");
    if (string.length() > 980) {
      int i = 979;
      while (string.charAt(i) != ' ')
        i--;
      string = string.substring(0, i)+"\r\n    "+prefix+string.substring(i);
    }
    return string;
  }

  protected String listForm(String name) {
    return name+"List";
  }

  public static boolean isDelphiReservedWord(String word) {
    if (word.equals("and")) return true;
    if (word.equals("array")) return true;
    if (word.equals("as")) return true;
    if (word.equals("asm")) return true;
    if (word.equals("begin")) return true;
    if (word.equals("case")) return true;
    if (word.equals("class")) return true;
    if (word.equals("const")) return true;
    if (word.equals("constructor")) return true;
    if (word.equals("create")) return true;
    if (word.equals("destructor")) return true;
    if (word.equals("dispinterface")) return true;
    if (word.equals("div")) return true;
    if (word.equals("do")) return true;
    if (word.equals("downto")) return true;
    if (word.equals("else")) return true;
    if (word.equals("end")) return true;
    if (word.equals("except")) return true;
    if (word.equals("exports")) return true;
    if (word.equals("file")) return true;
    if (word.equals("finalization")) return true;
    if (word.equals("finally")) return true;
    if (word.equals("for")) return true;
    if (word.equals("function")) return true;
    if (word.equals("goto")) return true;
    if (word.equals("if")) return true;
    if (word.equals("implementation")) return true;
    if (word.equals("in")) return true;
    if (word.equals("inherited")) return true;
    if (word.equals("initialization")) return true;
    if (word.equals("inline")) return true;
    if (word.equals("interface")) return true;
    if (word.equals("is")) return true;
    if (word.equals("label")) return true;
    if (word.equals("library")) return true;
    if (word.equals("link")) return true;
    if (word.equals("mod")) return true;
    if (word.equals("nil")) return true;
    if (word.equals("not")) return true;
    if (word.equals("object")) return true;
    if (word.equals("of")) return true;
    if (word.equals("or")) return true;
    if (word.equals("out")) return true;
    if (word.equals("packed")) return true;
    if (word.equals("procedure")) return true;
    if (word.equals("program")) return true;
    if (word.equals("property")) return true;
    if (word.equals("raise")) return true;
    if (word.equals("record")) return true;
    if (word.equals("repeat")) return true;
    if (word.equals("resourcestring")) return true;
    if (word.equals("set")) return true;
    if (word.equals("shl")) return true;
    if (word.equals("shr")) return true;
    if (word.equals("string")) return true;
    if (word.equals("then")) return true;
    if (word.equals("threadvar")) return true;
    if (word.equals("to")) return true;
    if (word.equals("try")) return true;
    if (word.equals("type")) return true;
    if (word.equals("unit")) return true;
    if (word.equals("until")) return true;
    if (word.equals("uses")) return true;
    if (word.equals("var")) return true;
    if (word.equals("while")) return true;
    if (word.equals("with")) return true;
    if (word.equals("xor")) return true;
    return false;
  }

  protected boolean typeIsSimple(String tn) {
    if (tn == null)
      return false;
    return tn.equals("String") || tn.equals("Integer")  || tn.equals("unsignedInt")  || tn.equals("positiveInt") || tn.equals("Boolean") || tn.equals("TDateTimeEx") || tn.equals("TFhirXHtmlNode");
  }

  protected String getPropertyName(String name) {
    return name.replace("[x]", "").replace("[type]", "value");
  }

  protected boolean typeIsPrimitive(String tn) {
    return Utilities.existsInList(tn, "uri", "datetime", "code", "boolean", "integer", "integer64", "unsignedInt", "positiveInt", "instant", "time", "dateTime", 
        "date", "id", "oid", "decimal", "string", "base64Binary", "markdown", "url", "canonical", "uuid");
  }

  protected String getElementName(String name) {
    name = name.replace("[x]", "").replace("[type]", "value");
    if (isDelphiReservedWord(name) || name.equals("fScore"))
      return name+"_";
    else
      return name;
  }

  protected int getEnumSize(ElementDefinition e) {
    EnumInfo ei = (EnumInfo) e.getUserData("pascal.enum");
    ValueSet vse = (ValueSet) ei.getValueSet().getUserData("expansion");
    return vse == null ? 0 : vse.getExpansion().getContains().size();
  }

  protected String marker() {
    if (MARKERS) {
      return " {L"+Integer.toString(new Exception().getStackTrace()[1].getLineNumber())+"}";
    } else {
      return "";
    }
  }

  protected String removeEoln(String s) {
    return s.replace("\r", " ").replace("\n", " ").replace("\t", "  ");
  }

  public String escape(String v) {
    if (Utilities.noString(v))
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
      r = r.substring(0, i)+"'\r\n      +'"+r.substring(i);
      i = i + 253;
    }
    return r;
  }

  protected boolean isResource(String name) {
    StructureDefinition type = definitions.getType(name);
    return type != null && type.getKind() == StructureDefinitionKind.RESOURCE;
  }

  protected ElementDefinition getInheritedElement(Analysis analysis, TypeInfo ti, ElementDefinition c) {
    if (analysis.getAncestor().hasExtension("http://hl7.org/fhir/StructureDefinition/structuredefinition-interface")
        && ti == analysis.getRootType()) {
      String name = tail(c.getPath());
      StructureDefinition sd = analysis.getAncestor();
      while (sd != null) {
        ElementDefinition ed = getElement(sd, sd.getType()+"."+name);
        if (ed != null) {
          return ed;
        }
        sd = definitions.getStructures().get(sd.getBaseDefinition());
      }      
    }
    return null;
  }

  private ElementDefinition getElement(StructureDefinition sd, String path) {
    for (ElementDefinition ed : sd.getSnapshot().getElement()) {
      if (ed.getPath().equals(path)) {
        return ed;
      }
    }
    return null;
  }

  protected String tail(String path) {
    return path.contains(".") ? path.substring(path.lastIndexOf(".")+1) : "";
  }


}
