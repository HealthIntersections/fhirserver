package org.fhir.pascal.misc;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;

import org.hl7.fhir.utilities.CommaSeparatedStringBuilder;
import org.hl7.fhir.utilities.Utilities;

import com.github.javaparser.JavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.LiteralExpr;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.expr.NameExpr;
import com.github.javaparser.ast.expr.ObjectCreationExpr;
import com.github.javaparser.ast.expr.VariableDeclarationExpr;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.ExpressionStmt;
import com.github.javaparser.ast.stmt.ForeachStmt;
import com.github.javaparser.ast.stmt.IfStmt;
import com.github.javaparser.ast.stmt.ReturnStmt;
import com.github.javaparser.ast.stmt.SwitchEntryStmt;
import com.github.javaparser.ast.stmt.SwitchStmt;

public class VersionConvertorTranslator {

  public class ParametersContext {

    Map<String, ClassInfo> params = new HashMap<String, ClassInfo>();
    
    public void addParam(String name, ClassInfo ci) {
     params.put(name, ci);
    }

    public ParametersContext copy() {
      ParametersContext self = new ParametersContext();
      self.params.putAll(params);
      return self;
    }
  }

  public static class ClassInfo {
    private String name;
    private boolean enumeration;
    private boolean isResource;
    List<String> lists = new ArrayList<String>();
    Map<String, String> methodTypes = new HashMap<String, String>();
    
    public boolean isEnumeration() {
      return enumeration;
    }

    public void setEnumeration(boolean enumeration) {
      this.enumeration = enumeration;
    }

    public boolean isResource() {
      return isResource;
    }

    public void setResource(boolean isResource) {
      this.isResource = isResource;
    }

    public String summary() {
      CommaSeparatedStringBuilder b1 = new CommaSeparatedStringBuilder();
      for (String s : lists)
        b1.append(s);
      CommaSeparatedStringBuilder b2 = new CommaSeparatedStringBuilder();
      for (Entry<String, String> e : methodTypes.entrySet())
        b2.append(e.getKey()+':'+e.getValue());
      return String.valueOf(enumeration)+"|"+String.valueOf(isResource)+"|"+b1.toString()+"|"+b2.toString();
    }

    public static ClassInfo fromSummary(String name, String summary) {
      
      String[] p = summary.split("\\|");
      ClassInfo self = new ClassInfo();
      self.name = name;
      self.enumeration = Boolean.valueOf(p[0]);
      self.isResource = Boolean.valueOf(p[1]);
      if (p.length > 2)
        for (String s : p[2].split("\\,"))
          self.lists.add(s.trim());
      if (p.length > 3)
        for (String s : p[3].split("\\,")) {
          String v[] = s.trim().split("\\:");
          self.methodTypes.put(v[0], v[1]);
        }
      return self;
    }
  }

  public static class EnumInfo {
    private String name;
    private String url;
    private List<String> codes = new ArrayList<String>();
    public static EnumInfo fromCache(String line) {
      line = Utilities.stripBOM(line);
      String[] p = line.split("\\=");
      EnumInfo self = new EnumInfo();
      self.name = p[0];
      String[] n = p[1].split("\\|");
      self.url = n[0];
      if (n.length > 1) {
        String[] v = n[1].split("\\,");
        for (String s : v)
          self.codes.add(s.trim());
      }
      return self;
    }
  }

  private static final String CACHEFILE = "c:\\temp\\classes.cache";


  public static void main(String[] args) throws Exception {
    new VersionConvertorTranslator().convert("C:\\work\\org.hl7.fhir\\build\\implementations\\java\\org.hl7.fhir.validation\\src\\org\\hl7\\fhir\\r4\\validation", 
        "C:\\work\\fhirserver\\library\\r4", "InstanceValidator");
  }

  Map<String, Integer> nameCount = new HashMap<String, Integer>();
  
  public void convert(String source, String dest, String className) throws Exception {    
    System.out.println("Parsing "+source);
    CompilationUnit jcode = JavaParser.parse(new File(Utilities.path(source, className+".java")));
    System.out.println("Loaded");
    Optional<ClassOrInterfaceDeclaration> clss = jcode.getClassByName(className);
//    DelphiCodeGenerator gen = startGen(className, dest);
//    genMethods(clss.get(), gen, className);
//    finish(gen);
    System.out.println("Generated");
  }

//
//  private void finish(DelphiCodeGenerator gen) throws Exception {
//    gen.classDefs.add("  end;");
//    gen.classDefs.add("");   
//    gen.finish("4");
//  }
//
//
//  private DelphiCodeGenerator startGen(String className, String dest) throws UnsupportedEncodingException, FileNotFoundException, IOException {
//    DelphiCodeGenerator gen = new DelphiCodeGenerator(new FileOutputStream(Utilities.path(dest, className+".pas")), "");
//    gen.name = className;
//    gen.setInclude(false);
//    gen.uses.add("SysUtils");
//    gen.uses.add("Classes");
//    gen.usesBreak();
//    gen.uses.add("FHIR.Support.Base");
//    gen.usesBreak();
//    gen.uses.add("FHIR.Base.Objects");
//    gen.uses.add("FHIR.Base.Xhtml");
//    gen.uses.add("FHIR.Base.Factory");
//    gen.usesBreak();
//    gen.uses.add("FHIR.R4.Types");
//    gen.uses.add("FHIR.R4.Resources");
//    gen.uses.add("FHIR.R4.Utilities");
//    gen.uses.add("FHIR.R4.ElementModel");
//    gen.uses.add("FHIR.R4.Validator");
//    gen.uses.add("FHIR.R4.Adaptor");
//    gen.classDefs.add("  T"+className+" = class (TFslObject)");
//    return gen;
//  }
//
//  private void genMethods(ClassOrInterfaceDeclaration clss, DelphiCodeGenerator gen, String className) {
//    gen.classDefs.add("  private");
//    for (MethodDeclaration f : clss.getMethods()) {
//      if (!f.isPublic()) {
//        if (passNameCount(f.getNameAsString()))
//          processMethod(gen, className, f);
//      }
//    }
//    gen.classDefs.add("  public");
//    for (MethodDeclaration f : clss.getMethods()) {
//      if (f.isPublic()) {
//        if (passNameCount(f.getNameAsString()))
//          processMethod(gen, className, f);
//      }
//    }
//  }
//
//
//
//  private boolean passNameCount(String name) {
//    Integer i = nameCount.get(name);
//    if (i == null)
//      i = 0;
//    i++;
//    nameCount.put(name, i);
//    return (Utilities.existsInList(name, "convertSequenceType")) ? i <= 1 : i <= 2;
//  }
//
//
//  private void processMethod(DelphiCodeGenerator gen, String className, MethodDeclaration f) {
//    String dec = "    ";
//    StringBuilder impl = new StringBuilder(); 
//    StringBuilder var = new StringBuilder(); 
//    if (f.isStatic()) {
//      dec = dec+"class ";
//      impl.append("class ");
//    }
//    if (f.getTypeAsString().equals("void")) {
//      dec = dec+"procedure ";
//      impl.append("procedure ");
//    } else {
//      dec = dec+"function ";
//      impl.append("function ");
//    }
//    impl.append("T"+className+".");
//    dec = dec+f.getNameAsString()+"(";
//    impl.append(f.getNameAsString());
//    impl.append("(");
//    ParametersContext pc = new ParametersContext();
//    for (int i = 0; i < f.getParameters().size(); i++) {
//      dec = paramHeader(dec, impl, i, f.getParameters().get(i), f.getNameAsString(), pc);
//    }
//    dec = dec+")";
//    impl.append(")");
//    if (!f.getTypeAsString().equals("void")) {
//      impl.append(" : "+convertType(f.getTypeAsString(), f.getNameAsString(), "return", pc));
//      dec = dec+" : "+convertType(f.getTypeAsString(), f.getNameAsString(), "return", pc);
//    }
//    dec = dec+"; overload; virtual;";
//    impl.append(";\r\n");
//    impl.append("%var%");
//    impl.append("begin\r\n");
//    processBlock(0, impl, var, f.getBody().get(), f.getNameAsString(), pc);
//    impl.append("end;\r\n");
//
//    gen.classDefs.add(dec);
//    if (var.length() > 0)
//      gen.classImpls.add(impl.toString().replace("%var%", "var\r\n"+var.toString()));
//    else
//      gen.classImpls.add(impl.toString().replace("%var%", ""));
//  }
//

  private void processBlock(int level, StringBuilder impl, StringBuilder vars, BlockStmt stmt, String ctxt, ParametersContext pc) {
    for (Node c : stmt.getChildNodes()) {
      processNode(level, impl, vars, c, ctxt, pc);
    }
  }


  private void processNode(int level, StringBuilder impl, StringBuilder vars, Node c, String ctxt, ParametersContext pc) {
    if (isIfSrcNull(c)) {
      if (!pc.params.get("src").isEnumeration())
        impl.append("  if (src = nil) then\r\n    exit(nil);\r\n");
    } else if (c instanceof BlockStmt)
      processBlock(level+2, impl, vars, (BlockStmt) c, ctxt, pc);
    else if (c instanceof ExpressionStmt)
      convertExpressionStmt(level+2, impl, vars, (ExpressionStmt) c, true, pc, false, ctxt);
    else if (c instanceof IfStmt)
      convertIfStmt(level+2, impl, vars, (IfStmt) c, ctxt, pc);
    else if (c instanceof ForeachStmt)
      convertForeachStmt(level+2, impl, vars, (ForeachStmt) c, ctxt, pc);
    else if (c instanceof SwitchStmt)
      convertSwitchStmt(level+2, impl, vars, (SwitchStmt) c, ctxt, pc);
    else if (c instanceof ReturnStmt)
      convertReturnStmt(level+2, impl, vars, (ReturnStmt) c, pc, ctxt);
    else
      impl.append("{"+c.getClass().getName()+" - A} "+c.toString()+"\r\n");
  }


  private void convertReturnStmt(int level, StringBuilder impl, StringBuilder vars, ReturnStmt c, ParametersContext pc, String ctxt) {
    //pad(level, impl);    
    impl.append("exit(");
    if (c.getExpression().isPresent()) {
      if (c.getExpression().get() instanceof LiteralExpr)
        impl.append(c.getExpression().get().toString().replace('"', '\''));
      else
        convertExpression(level, impl, vars, c.getExpression().get(), pc, true, ctxt);
    }
    impl.append(");\r\n");
    
    
  }


  private void convertSwitchStmt(int level, StringBuilder impl, StringBuilder vars, SwitchStmt c, String ctxt, ParametersContext pc) {
    pad(level, impl);    
    String sel = c.getSelector().toString();
    String type = pc.params.containsKey(sel) ? pc.params.get(sel).name : null;
//    EnumInfo e = type != null ? getJavaEnum(type) : null;
//    EnumInfo p = null;
//    if (e != null) {
//      boolean r4 = type.contains(".r4.");
//      p = r4 ? getPascalEnum4(e.url) : getPascalEnum3(e.url);
//    }
    impl.append("case (");    
    convertExpression(level+4, impl, vars, c.getSelector(), pc, false, ctxt);
    impl.append(") of\r\n");
    for (SwitchEntryStmt cs : c.getEntries()) {
      pad(level+2, impl);
      if (cs.getLabel().isPresent()) {
        boolean done = false;
        String lbl = cs.getLabel().get().toString();
//        if (e != null && e.codes.contains(lbl)) {
//          boolean r4 = type.contains(".r4.");
//          if (p != null) {
//            int ndx = e.codes.indexOf(lbl);
//            if (ndx >= 0 && ndx < p.codes.size()) {
//              impl.append(r4 ? "FHIRTypes4." : "FHIRTypes3.");
//              impl.append(p.codes.get(ndx));
//              done = true;
//            }
//          }
//        } 
        if (!done)
          convertExpression(level+4, impl, vars, cs.getLabel().get(), pc, false, ctxt);
        impl.append(": ");    
      } else
        impl.append("else ");    
      for (Node ce : cs.getChildNodes()) {
        if (!cs.getLabel().isPresent() || ce != cs.getLabel().get())
        processNode(level+2, impl, vars, ce, ctxt, pc);
      }
    }
    pad(level, impl);
    impl.append("end;\r\n");    
  }


  private void convertForeachStmt(int level, StringBuilder impl, StringBuilder vars, ForeachStmt c, String ctxt, ParametersContext pc) {
    pad(level, impl);    
    impl.append("for ");
    VariableDeclarationExpr v = c.getVariable();
    pc = pc.copy();
    vars.append("  "+v.getVariable(0).getNameAsString()+" : "+convertType(v.getVariable(0).getTypeAsString(), ctxt, v.getVariable(0).getNameAsString(), pc)+";\r\n");
    impl.append(v.getVariable(0).getNameAsString());
    impl.append(" in ");
    convertExpression(level, impl, vars, c.getIterable(), pc, false, ctxt);
    impl.append(" do\r\n");
    pad(level, impl);    
    impl.append("begin\r\n");    
    processNode(level, impl, vars, c.getBody(), ctxt, pc);
    pad(level, impl);
    impl.append("end;\r\n");    
  }


  private void convertIfStmt(int level, StringBuilder impl, StringBuilder vars, IfStmt c, String ctxt, ParametersContext pc) {
    pad(level, impl);    
    impl.append("if (");    
    convertExpression(level+4, impl, vars, c.getCondition(), pc, false, ctxt);
    impl.append(") then\r\n");    
//    pad(level, impl);    
//    impl.append("begin\r\n");    
    processNode(level, impl, vars, c.getThenStmt(), ctxt, pc);
    if (c.getElseStmt().isPresent()) {
      impl.append("else\r\n");    
//      pad(level, impl);
//      impl.append("begin\r\n");    
      processNode(level, impl, vars, c.getElseStmt().get(), ctxt, pc);
    } 
//    pad(level, impl);
    impl.append("\r\n");
  }


  private ParametersContext convertExpression(int level, StringBuilder impl, StringBuilder vars, Expression c, ParametersContext pc, boolean willOwn, String ctxt) {
    if (isHas(c)) {
      convertHas(level, impl, c, pc);
    } else if (isGet(c)) {
      convertGet(level, impl, c, pc, willOwn);
    } else if (isAdd(c)) {
      convertAdd(level, impl, vars, c, pc, ctxt);
    } else if (isSimple(c)) {
      impl.append(c.toString());
    } else if (isPropertyCopy(c)) {
      convertPropertyCopy(level, impl, c, pc);
    } else if (isPropertyConstant(c)) {
      convertPropertyConstant(level, impl, c, pc);
    } else if (isPropertyConvert(c)) {
      convertPropertyConvert(level, impl, c, pc);
    } else if (c instanceof NameExpr) {
      impl.append(c.toString());
    } else if (c instanceof FieldAccessExpr) {
      impl.append(convertEnumReference((FieldAccessExpr) c, pc));
    } else if (c instanceof VariableDeclarationExpr) {
      pc = convertVariableDeclarationExpr(level, (VariableDeclarationExpr) c, impl, vars, ctxt, pc);
    } else {
      impl.append("{"+c.getClass().getName()+" - C} ");
      impl.append(c.toString());
    }
    return pc;
  }


  private ParametersContext convertVariableDeclarationExpr(int level, VariableDeclarationExpr v, StringBuilder impl, StringBuilder vars, String ctxt, ParametersContext pc) {
    String type = convertType(v.getVariable(0).getTypeAsString(), ctxt, v.getVariable(0).getNameAsString(), pc);
    vars.append("  "+v.getVariable(0).getNameAsString()+" : "+type+";\r\n");
    impl.append(v.getVariable(0).getNameAsString());
    impl.append(" := ");
    if (v.getVariable(0).getInitializer().isPresent()) {
      Expression vi = v.getVariable(0).getInitializer().get();
      if (vi instanceof ObjectCreationExpr) {
        ObjectCreationExpr vin = (ObjectCreationExpr) vi;
        impl.append(type);
        impl.append(".Create(");
        boolean first = true;
        for (Expression p : vin.getArguments()) {
          if (first) first = false; else impl.append(", ");
          convertExpression(level, impl, vars, p, pc, false, ctxt);
        }
        impl.append(")");
      } else
        convertExpression(level, impl, vars, vi, pc, false, ctxt);
    }
//    if (v.get)
    return pc.copy();
  }


  private Object convertEnumReference(FieldAccessExpr c, ParametersContext pc) {
    String en = c.toString();
    boolean r4 = en.contains(".r4.");
    String t = en.substring(0, en.lastIndexOf("."));
    String v = en.substring(en.lastIndexOf(".")+1);
//    EnumInfo j = getJavaEnum(t);
//    if (j != null) {
//      EnumInfo p = r4 ? getPascalEnum4(j.url) :  getPascalEnum3(j.url);
//      if (p != null) {
//        int i = j.codes.indexOf(v);
//        if (i >= 0 && i < p.codes.size())
//          return (r4 ? "FHIRTypes4." : "FHIRTypes3.") + p.codes.get(i);
//      }
//    }
    return en;
  }


  private void convertPropertyCopy(int level, StringBuilder impl, Expression c, ParametersContext pc) {
    MethodCallExpr mo = (MethodCallExpr) c;
    MethodCallExpr mi = (MethodCallExpr) mo.getArguments().get(0);
    impl.append(mo.getScope().get());
    impl.append(".");
    impl.append(Utilities.uncapitalize(mo.getNameAsString().substring(3)));
    impl.append(" := ");
    impl.append(mi.getScope().get());
    impl.append(".");
    impl.append(Utilities.uncapitalize(mi.getNameAsString().substring(3)));
    String scope = mo.getScope().get().toString();
    String link = ".link";
    if (pc.params.containsKey(scope) && pc.params.get(scope).methodTypes.containsKey(mo.getNameAsString())) {
      String type = pc.params.get(scope).methodTypes.get("get"+mo.getNameAsString().substring(3));
      if ("String".equals(type))
        link = "";
    }
    impl.append(link);
  }

  private void convertPropertyConstant(int level, StringBuilder impl, Expression c, ParametersContext pc) {
    MethodCallExpr mo = (MethodCallExpr) c;
    LiteralExpr v = (LiteralExpr) mo.getArguments().get(0);
    impl.append(mo.getScope().get());
    impl.append(".");
    impl.append(Utilities.uncapitalize(mo.getNameAsString().substring(3)));
    impl.append(" := ");
    impl.append(v.toString().replace('"', '\''));
  }

  private void convertPropertyConvert(int level, StringBuilder impl, Expression c, ParametersContext pc) {
    MethodCallExpr mo = (MethodCallExpr) c;
    MethodCallExpr mc = (MethodCallExpr) mo.getArguments().get(0);
    MethodCallExpr mi = (MethodCallExpr) mc.getArguments().get(0);
    impl.append(mo.getScope().get());
    impl.append(".");
    impl.append(Utilities.uncapitalize(mo.getNameAsString().substring(3)));
    impl.append(" := ");
    impl.append(mc.getNameAsString());
    impl.append("(");
    impl.append(mi.getScope().get());
    impl.append(".");
    impl.append(Utilities.uncapitalize(mi.getNameAsString().substring(3)));
    impl.append(")");
  }

  
  private void convertAdd(int level, StringBuilder impl, StringBuilder vars, Expression c, ParametersContext pc, String ctxt) {
    MethodCallExpr mo = (MethodCallExpr) c;
    impl.append(mo.getScope().get());
    impl.append(".");
    impl.append(Utilities.uncapitalize(mo.getNameAsString().substring(3)));
    if (Utilities.existsInList(mo.getNameAsString(), "addExtension", "addModifierExtension", "addContained") || pc.params.containsKey(mo.getScope().get()) && pc.params.get(mo.getScope().get()).lists.contains(mo.getNameAsString()))
      impl.append("List");
    impl.append(".add(");
    convertExpression(level+2, impl, vars, mo.getArgument(0), pc, true, ctxt);
    impl.append(")");
  }

  private void convertHas(int level, StringBuilder impl, Expression c, ParametersContext pc) {
    MethodCallExpr mo = (MethodCallExpr) c;
    impl.append(mo.getScope().get());
    impl.append(".");
    impl.append(Utilities.uncapitalize(mo.getNameAsString().substring(3)));
    String check = " <> nil";
    String scope = mo.getScope().get().toString();
    if (pc.params.containsKey(scope) && pc.params.get(scope).methodTypes.containsKey(mo.getNameAsString())) {
      String type = pc.params.get(scope).methodTypes.get("get"+mo.getNameAsString().substring(3));
      if ("String".equals(type))
        check = " <> ''";
    }
    impl.append(check);
  }

  private void convertGet(int level, StringBuilder impl, Expression c, ParametersContext pc, boolean willOwn) {
    MethodCallExpr mo = (MethodCallExpr) c;
    impl.append(mo.getScope().get());
    impl.append(".");
    impl.append(Utilities.uncapitalize(mo.getNameAsString().substring(3)));
    if (Utilities.existsInList(mo.getNameAsString(), "getExtension", "getModifierExtension", "getContained") || pc.params.containsKey(mo.getScope().get()) && pc.params.get(mo.getScope().get()).lists.contains(mo.getNameAsString()))
      impl.append("List");
  }

  private boolean isPropertyCopy(Expression c) {
    if (c.isMethodCallExpr()) {
      MethodCallExpr m = (MethodCallExpr) c;
      if (m.getNameAsString().startsWith("set") && m.getScope().isPresent() &&  m.getArguments().size() == 1 & m.getArguments().get(0) instanceof MethodCallExpr) {
        m = ((MethodCallExpr) m.getArguments().get(0));
        return m.getNameAsString().startsWith("get") && m.getScope().isPresent() &&  m.getArguments().size() == 0; 
      } 
    }  
     return false;
  }

  private boolean isPropertyConstant(Expression c) {
    if (c.isMethodCallExpr()) {
      MethodCallExpr m = (MethodCallExpr) c;
      if (m.getNameAsString().startsWith("set") && m.getScope().isPresent() &&  m.getArguments().size() == 1 & m.getArguments().get(0) instanceof LiteralExpr) {
        return true; 
      } 
    }  
    return false;
  }

  private boolean isPropertyConvert(Expression c) {
    if (c.isMethodCallExpr()) {
      MethodCallExpr m = (MethodCallExpr) c;
      if (m.getNameAsString().startsWith("set") && m.getScope().isPresent() &&  m.getArguments().size() == 1 & m.getArguments().get(0) instanceof MethodCallExpr) {
        m = ((MethodCallExpr) m.getArguments().get(0));
        if (m.getNameAsString().startsWith("convert") && !m.getScope().isPresent() &&  m.getArguments().size() == 1 & m.getArguments().get(0) instanceof MethodCallExpr) {
          m = ((MethodCallExpr) m.getArguments().get(0));
          return m.getNameAsString().startsWith("get") && m.getScope().isPresent() &&  m.getArguments().size() == 0;
        }
      } 
    }  
     return false;
  }

  private boolean isAdd(Expression c) {
    if (c.isMethodCallExpr()) {
      MethodCallExpr m = (MethodCallExpr) c;
      return m.getNameAsString().startsWith("add") && m.getScope().isPresent() &&  m.getArguments().size() == 1; 
    }  
     return false;
  }


  private boolean isHas(Expression c) {
    if (c.isMethodCallExpr()) {
      MethodCallExpr m = (MethodCallExpr) c;
      if (m.getNameAsString().startsWith("has") && m.getScope().isPresent() &&  m.getArguments().size() == 0)
        return true;
    }  
     return false;
  }

  private boolean isGet(Expression c) {
    if (c.isMethodCallExpr()) {
      MethodCallExpr m = (MethodCallExpr) c;
      if (m.getNameAsString().startsWith("get") && m.getScope().isPresent() &&  m.getArguments().size() == 0)
        return true;
    }  
     return false;
  }


  private void pad(int level, StringBuilder impl) {
    impl.append(Utilities.padLeft(" ", ' ', level));
  }


  private void convertExpressionStmt(int level, StringBuilder impl, StringBuilder vars, ExpressionStmt c, boolean pad, ParametersContext pc, boolean willOwn, String ctxt) {
    if (pad)
      pad(level, impl);   
    convertExpression(level, impl, vars, c.getExpression(), pc, willOwn, ctxt);
    impl.append(";");
    if (pad)
      impl.append("\r\n");
//    if (pad)
//      pad(level, impl);   
//    impl.append("{"+c.getClass().getName()+" - B} ");
//    impl.append(c.toString());
//    if (pad)
//      impl.append("\r\n");
  }


  private boolean isSimple(Expression c) {
    if (c instanceof MethodCallExpr) {
      MethodCallExpr m = (MethodCallExpr) c;
      return m.getArguments().size() == 0;
    }
    else 
      return false;
  }


  private boolean isIfSrcNull(Node c) {
    return (c instanceof IfStmt) && c.toString().startsWith("if (src == null)");
  }


  private String paramHeader(String dec, StringBuilder impl, int i, Parameter parameter, String ctxt, ParametersContext pc) {
    String name = fixParamName(parameter.getNameAsString());
    String type = convertType(parameter.getTypeAsString(), ctxt, name, pc);
    if (i > 0) {
      impl.append("; ");
      dec = dec + "; ";
    }
    impl.append(name);
    impl.append(" : ");
    impl.append(type);
    return dec+name+" : "+type;
  }

  private String convertType(String tn, String ctxt, String name, ParametersContext pc) {
    tn = tn.trim();
    if (tn.equals("Set<String>"))
      return "TFslStringSet";
    if (tn.equals("List<String>"))
      return "TStringList";
    if (tn.startsWith("List<"))
      return "TFslList<"+convertType(tn.substring(5, tn.length()-1), ctxt, name, pc)+">";
    if (tn.startsWith("Map<String,"))
      return "TFslMap<"+convertType(tn.substring(11, tn.length()-1), ctxt, name, pc)+">";
    if (tn.equals("Map<Element,Element>"))
      return "TFHIRElementMap";
    if (Utilities.existsInList(tn, "boolean", "String"))
      return tn;
    if (Utilities.existsInList(tn, "int"))
      return "integer";
    if (Utilities.existsInList(tn, "NodeStack", "StringBuilder", "ElementInfo") || tn.startsWith("Fhir"))
      return "T"+tn;
    if (tn.equals("org.hl7.fhir.r4.elementmodel.Element"))               
      return "TFHIRMMElement";
    if (tn.equals("org.hl7.fhir.r4.model.Element"))               
      return "TFHIRObject";
    if (tn.equals("IResourceValidator"))
      return "TInstanceValidator";
    if (tn.equals("IWorkerContext"))
      return "TFHIRValidatorContext";
    if (tn.equals("InputStream"))
      return "TStream";
    if (tn.equals("ResourceProfiles") || tn.equals("ValidationProfileSet")) 
      return "TValidationProfileSet";
    if (tn.equals("TypeRefComponent"))
      return "TFhirElementDefinitionType";
    if (tn.equals("ConceptDefinitionComponent"))
      return "TFhirCodeSystemConcept";
    if (tn.endsWith("Component"))
      tn = tn.substring(0, tn.length()-9);
    return "TFhir"+tn;
  }


  private String fixParamName(String name) {
    return name.equals("type") ? "type_" : name;
  }



}    

