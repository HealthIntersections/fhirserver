package org.fhir.pascal.misc;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;

import org.hl7.fhir.utilities.CommaSeparatedStringBuilder;
import org.hl7.fhir.utilities.TextFile;
import org.hl7.fhir.utilities.Utilities;

import com.github.javaparser.JavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.BodyDeclaration;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.body.TypeDeclaration;
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

public class ValidatorTranslator {

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
    new ValidatorTranslator().convert("C:\\work\\org.hl7.fhir\\build\\implementations\\java\\org.hl7.fhir.convertors\\src\\org\\hl7\\fhir\\convertors", 
        "C:\\work\\fhirserver\\reference-platform\\xversion", "VersionConvertor_30_40", 
        "C:\\work\\org.hl7.fhir\\build\\implementations\\java\\org.hl7.fhir.r4\\src\\org\\hl7\\fhir\\r4\\model", "C:\\work\\org.hl7.fhir\\build\\implementations\\java\\org.hl7.fhir.dstu3\\src\\org\\hl7\\fhir\\dstu3\\model");
  }

  Map<String, ClassInfo> models = new HashMap<String, ClassInfo>();
  List<EnumInfo> enumsJ3 = new ArrayList<EnumInfo>();
  List<EnumInfo> enumsJ4 = new ArrayList<EnumInfo>();
  List<EnumInfo> enumsP3 = new ArrayList<EnumInfo>();
  List<EnumInfo> enumsP4 = new ArrayList<EnumInfo>();
  Map<String, Integer> nameCount = new HashMap<String, Integer>();
  
  public void convert(String source, String dest, String className, String srcModels, String tgtModels) throws Exception {
    if (new File(CACHEFILE).exists()) {
      System.out.println("Load "+CACHEFILE);
      loadCache();
    } else {
      loadModels(srcModels);
      loadModels(tgtModels);
      saveCache();
    }
    loadEnums("c:\\temp\\delphi.enums.r3.cache", enumsP3);
    loadEnums("c:\\temp\\delphi.enums.r4.cache", enumsP4);
    loadEnums("c:\\temp\\java.enums.r3.cache", enumsJ3);
    loadEnums("c:\\temp\\java.enums.r4.cache", enumsJ4);
    
    System.out.println("Parsing "+source);
    CompilationUnit jcode = JavaParser.parse(new File(Utilities.path(source, className+".java")));
    System.out.println("Loaded");
    Optional<ClassOrInterfaceDeclaration> clss = jcode.getClassByName(className);
//    DelphiCodeGenerator gen = startGen(className, dest);
//    genMethods(clss.get(), gen, className);
//    finish(gen);
    System.out.println("Generated");
  }


  private void loadEnums(String source, List<EnumInfo> enums) throws IOException {
    System.out.println("Load enums from "+source);
    BufferedReader bufferedReader = new BufferedReader(new FileReader(new File(source)));
    String line;
    while ((line = bufferedReader.readLine()) != null) {
      enums.add(EnumInfo.fromCache(line));
    }
    new FileReader(new File(CACHEFILE)).close();
    bufferedReader.close();
  }


  private void loadCache() throws IOException {
    BufferedReader bufferedReader = new BufferedReader(new FileReader(new File(CACHEFILE)));
    String line;
    while ((line = bufferedReader.readLine()) != null) {
      String[] p = line.split("\\=");
      models.put(p[0], ClassInfo.fromSummary(p[0], p[1]));
    }
    new FileReader(new File(CACHEFILE)).close();
    bufferedReader.close();
   
  }


  private void saveCache() throws IOException {
    StringBuilder b = new StringBuilder();
    for (Entry<String, ClassInfo> e : models.entrySet()) {
      b.append(e.getKey()+"="+e.getValue().summary()+"\r\n");
    }
    TextFile.stringToFile(b.toString(), new File(CACHEFILE));
  }


  private void loadModels(String source) throws FileNotFoundException {
    System.out.println("Parsing "+source);
    for (File f : new File(source).listFiles()) {
      if (f.getAbsolutePath().endsWith(".java")) {
        CompilationUnit jcode = JavaParser.parse(f);
        for (TypeDeclaration<?> c : jcode.getTypes()) {
          String ns = jcode.getPackageDeclaration().get().getNameAsString()+"."+c.getNameAsString();
          ClassInfo ci = new ClassInfo();
          ci.setEnumeration(c.isEnumDeclaration());
          ci.setResource(c.getAnnotationByName("ResourceDef").isPresent());
         
          models.put(ns, ci);
          processMembers(ns, c, ci);
        }
      }
    }

  }


  private void processMembers(String ns, TypeDeclaration<?> c, ClassInfo cp) {
    for (BodyDeclaration<?> m : c.getMembers()) {
      if (m instanceof MethodDeclaration) {
        MethodDeclaration md = (MethodDeclaration) m;
        cp.methodTypes.put(md.getNameAsString(), md.getTypeAsString());
        if (md.getNameAsString().startsWith("get") && md.getParameters().size() == 0 && md.getTypeAsString().startsWith("List<"))
          cp.lists.add(md.getNameAsString());
        
      }
      if (m instanceof TypeDeclaration) {
        TypeDeclaration<?> t = (TypeDeclaration<?>) m;
        String n = ns+"."+t.getNameAsString();
        ClassInfo ci = new ClassInfo();
        ci.setEnumeration(t.isEnumDeclaration());
        models.put(n, ci);
        processMembers(n, t, ci);
      }
    }    
  }


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
//    gen.uses.add("AdvObjects");
//    gen.uses.add("FHIRTypes3");
//    gen.uses.add("FHIRResources3");
//    gen.uses.add("FHIRUtilities3");
//    gen.uses.add("FHIRTypes4");
//    gen.uses.add("FHIRResources4");
//    gen.uses.add("FHIRUtilities4");
//    gen.classDefs.add("  T"+className+" = class (TAdvObject)");
//    return gen;
//  }
//
//  private void genMethods(ClassOrInterfaceDeclaration clss, DelphiCodeGenerator gen, String className) {
//    gen.classDefs.add("  private");
//    for (MethodDeclaration f : clss.getMethods()) {
//      if (!f.isPublic()) {
//        if (!onSkippedResourceType(f) && passNameCount(f.getNameAsString()))
//          processMethod(gen, className, f);
//      }
//    }
//    gen.classDefs.add("  public");
//    for (MethodDeclaration f : clss.getMethods()) {
//      if (f.isPublic()) {
//        if (!onSkippedResourceType(f) && passNameCount(f.getNameAsString()))
//          processMethod(gen, className, f);
//      }
//    }
//  }
//

  private boolean onSkippedResourceType(MethodDeclaration f) {
    String rn = getResourceName(f.getTypeAsString());
    return Utilities.existsInList(rn, "EligibilityRequest", "EligibilityResponse", "ProcessRequest", "ProcessResponse", "TestReport", "TestScript");
  }



  private boolean passNameCount(String name) {
    Integer i = nameCount.get(name);
    if (i == null)
      i = 0;
    i++;
    nameCount.put(name, i);
    return (Utilities.existsInList(name, "convertSequenceType")) ? i <= 1 : i <= 2;
  }

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
    EnumInfo e = type != null ? getJavaEnum(type) : null;
    EnumInfo p = null;
    if (e != null) {
      boolean r4 = type.contains(".r4.");
      p = r4 ? getPascalEnum4(e.url) : getPascalEnum3(e.url);
    }
    impl.append("case (");    
    convertExpression(level+4, impl, vars, c.getSelector(), pc, false, ctxt);
    impl.append(") of\r\n");
    for (SwitchEntryStmt cs : c.getEntries()) {
      pad(level+2, impl);
      if (cs.getLabel().isPresent()) {
        boolean done = false;
        String lbl = cs.getLabel().get().toString();
        if (e != null && e.codes.contains(lbl)) {
          boolean r4 = type.contains(".r4.");
          if (p != null) {
            int ndx = e.codes.indexOf(lbl);
            if (ndx >= 0 && ndx < p.codes.size()) {
              impl.append(r4 ? "FHIRTypes4." : "FHIRTypes3.");
              impl.append(p.codes.get(ndx));
              done = true;
            }
          }
        } 
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
    EnumInfo j = getJavaEnum(t);
    if (j != null) {
      EnumInfo p = r4 ? getPascalEnum4(j.url) :  getPascalEnum3(j.url);
      if (p != null) {
        int i = j.codes.indexOf(v);
        if (i >= 0 && i < p.codes.size())
          return (r4 ? "FHIRTypes4." : "FHIRTypes3.") + p.codes.get(i);
      }
    }
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

  private String fixParamName(String name) {
    return name.equals("type") ? "type_" : name;
  }


  private String convertType(String type, String ctxt, String name, ParametersContext pc) {
    if (Utilities.existsInList(type, "String", "int"))
      return type;
    
    String v = "";
    boolean list = false;
    if (type.startsWith("List<")) {
      list = true;
      type = type.substring(5, type.length()-1);
    }
    if (inNS(type, "org.hl7.fhir.dstu3.model")) {
      v = "3";
    } else if (inNS(type, "org.hl7.fhir.r4.model")) {
      v = "4";
    } else
      throw new Error("Unknown type namespace: "+type+" @ "+ctxt);
    String[] nl = type.split("\\.");

    ClassInfo cr = models.get(nl[0]+"."+nl[1]+"."+nl[2]+"."+nl[3]+"."+nl[4]+"."+nl[5]);
    ClassInfo ci = models.get(type);
    if (ci == null)
      return type;
    pc.addParam(name, ci);
    if (nl[5].endsWith("Type") && !nl[5].equals("Type"))
      nl[5] = nl[5].substring(0, nl[5].length()-4);
    if (nl[5].equals("ListResource"))
      nl[5] = "List";
      
    if (nl[nl.length-1].endsWith("Component"))
      nl[nl.length-1] = nl[nl.length-1].substring(0, nl[nl.length-1].length()-9);
        
    boolean r = nl[5].equals("Resource") || nl[5].equals("DomainResource") || cr.isResource();
    if (ci.isEnumeration()) {
      EnumInfo j = getJavaEnum(nl[0]+"."+nl[1]+"."+nl[2]+"."+nl[3]+"."+nl[4]+"."+nl[5]+"."+nl[6]);
      if (j == null)
        return lookupEnum("FHIRTypes"+v, "TFhir"+nl[6]+"Enum");
      EnumInfo p = nl[3].equals("r4") ? getPascalEnum4(j.url) : getPascalEnum3(j.url);
      if (p == null)
        return lookupEnum("FHIRTypes"+v, "TFhir"+nl[6]+"Enum");
      return "FHIRTypes"+v+"."+p.name;
    } else
      return lookupType("FHIR"+(r ? "Resources" : "Types")+v, "TFhir"+nl[nl.length-1], nl[5])+(list ? "List" : "");
  }


  private String lookupType(String space, String name, String rn) {
    if (name.equals("TFhirTypeRef"))               return space+".TFhirElementDefinitionType";
    if (name.equals("TFhirSimpleQuantity"))        return space+".TFhirQuantity";
    if (name.equals("TFhirResourceInteraction"))   return space+".TFhirCapabilityStatementRestResourceInteraction";
    if (name.equals("TFhirSystemInteraction"))     return space+".TFhirCapabilityStatementRestInteraction";
    if (name.equals("TFhirRelatedClaim"))          return space+".TFhirClaimRelated";
    if (name.equals("TFhirPayee"))                 return space+".TFhirClaimPayee";
    if (name.equals("TFhirSpecialCondition"))      return space+".TFhirClaimInformation";
    if (name.equals("TFhirDiagnosis"))             return space+".TFhirClaimDiagnosis";
    if (name.equals("TFhirProcedure") && rn.equals("Claim"))        return space+".TFhirClaimProcedure";
    if (name.equals("TFhirItem") && rn.equals("Claim"))        return space+".TFhirClaimItem";
    if (name.equals("TFhirDetail") && rn.equals("Claim"))        return space+".TFhirClaimItemDetail";
    if (name.equals("TFhirInsurance") && rn.equals("Claim"))        return space+".TFhirClaimInsurance";
    if (name.equals("TFhirAccident") && rn.equals("Claim"))        return space+".TFhirClaimAccident";
    if (name.equals("TFhirSubDetail") && rn.equals("Claim"))        return space+".TFhirClaimItemDetailSubDetail";

    if (name.equals("TFhirProperty") && rn.equals("CodeSystem"))        return space+".TFhirCodeSystemProperty";
    if (name.equals("TFhirConceptDefinition") && rn.equals("CodeSystem"))        return space+".TFhirCodeSystemConcept";
    if (name.equals("TFhirConceptDefinitionDesignation") && rn.equals("CodeSystem"))        return space+".TFhirCodeSystemConceptDesignation";
    if (name.equals("TFhirConceptProperty") && rn.equals("CodeSystem"))        return space+".TFhirCodeSystemConceptProperty";
    
    if (name.equals("TFhirSection") && rn.equals("Composition"))        return space+".TFhirCompositionSection";
    
    if (name.equals("TFhirSourceElement") && rn.equals("ConceptMap"))        return space+".TFhirConceptMapGroupElement";
    if (name.equals("TFhirTargetElement") && rn.equals("ConceptMap"))        return space+".TFhirConceptMapGroupElementTarget";
    if (name.equals("TFhirOtherElement") && rn.equals("ConceptMap"))        return space+".TFhirConceptMapGroupElementTargetDependsOn";
    
    if (name.equals("TFhirStatusHistory") && rn.equals("Encounter"))        return space+".TFhirEncounterStatusHistory";
    if (name.equals("TFhirClassHistory") && rn.equals("Encounter"))        return space+".TFhirEncounterClassHistory";
    
    if (name.equals("TFhirDesignationInclude") && rn.equals("ExpansionProfile"))        return space+".TFhirExpansionProfileDesignationInclude";
    if (name.equals("TFhirDesignationExclude") && rn.equals("ExpansionProfile"))        return space+".TFhirExpansionProfileDesignationExclude";
    if (name.equals("TFhirDesignationIncludeDesignation") && rn.equals("ExpansionProfile"))        return space+".TFhirExpansionProfileDesignationIncludeDesignation";
    if (name.equals("TFhirDesignationExcludeDesignation") && rn.equals("ExpansionProfile"))        return space+".TFhirExpansionProfileDesignationExcludeDesignation";
    
    if (name.equals("TFhirMessageDestination") && rn.equals("MessageHeader"))        return space+".TFhirMessageHeaderDestination";
    if (name.equals("TFhirMessageSource") && rn.equals("MessageHeader"))        return space+".TFhirMessageHeaderSource";

    if (name.equals("TFhirContact") && rn.equals("Patient"))        return space+".TFhirPatientContact";
    if (name.equals("TFhirAnimal") && rn.equals("Patient"))        return space+".TFhirPatientAnimal";

    if (name.equals("TFhirConceptSet") && rn.equals("ValueSet"))        return space+".TFhirValueSetComposeInclude";
    if (name.equals("TFhirConceptReference") && rn.equals("ValueSet"))        return space+".TFhirValueSetComposeIncludeConcept";
    if (name.equals("TFhirConceptSetFilter") && rn.equals("ValueSet"))        return space+".TFhirValueSetComposeIncludeConcept";
    if (name.equals("TFhirConceptReferenceDesignation") && rn.equals("ValueSet"))        return space+".TFhirValueSetComposeIncludeFilter";
    return space+"."+name;
  }


  private String lookupEnum(String scope, String name) {
    if (name.equals("TFhirSequenceTypeEnum"))
      return "String";
    if (name.equals("TFhirObservationRelationshipTypeEnum"))
      return scope+".TFhirObservationRelationshipTypesEnum";
    return scope+"."+name;
  }


  private EnumInfo getPascalEnum3(String url) {
    for (EnumInfo e : enumsP3) 
      if (url.equals(e.url))
        return e;
    System.out.println("Unable to find "+url);
    return null;
  }



  private EnumInfo getPascalEnum4(String url) {
    for (EnumInfo e : enumsP4) 
      if (url.equals(e.url))
        return e;
    System.out.println("Unable to find "+url);
    return null;
  }


  private EnumInfo getJavaEnum(String name) {
    for (EnumInfo e : enumsJ3) 
      if (name.equals(e.name))
        return e;
    for (EnumInfo e : enumsJ4) 
      if (name.equals(e.name))
        return e;
    System.out.println("Unabel to find "+name);
    return null;
  }


  private boolean inNS(String type, String ns) {
    return type.startsWith(ns+".");
  }


  private String getResourceName(String type) {
    if (!inNS(type, "org.hl7.fhir.dstu3.model") && !inNS(type, "org.hl7.fhir.r4.model")) 
      return "!";
    else {
      String[] nl = type.split("\\.");
      return nl[5];
    }
  }

}    

