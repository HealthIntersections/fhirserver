unit fsl_rdf;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

{$I fhir.inc}

interface

uses
  SysUtils, Classes, Generics.Collections,
  fsl_base, fsl_utilities, fsl_stream, fsl_turtle;

const
  GOOD_IRI_CHAR = 'a-zA-Z0-9\u00A0-\uFFFE';
  IRI_URL = '(([a-z])+:)*((%[0-9a-fA-F]{2})|[&''\\(\\)*+,;:@_~?!$\\/\\-\\#.\\='+GOOD_IRI_CHAR+'])+';
  LANG_REGEX = '[a-z]{2}(\\-[a-zA-Z]{2})?';


{
This RDF writer is orientated around producing legible TTL syntax.

}
type
  TRDFFormat = (rdfTurtle, rdfXMl, rdfNTriple);

  // First: a smart Turtle writer. The focus here is on producing human readable turtle
  TRDFGenerator = class;
  TRDFPredicate = class;

  TRDFTriple = class abstract (TFslObject)
  private
    uri : String;
  public
  end;

  TRDFString = class (TRDFTriple)
  private
    FValue : String;
    FType : String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(value : String);
  end;

  TRDFComplex = class (TRDFTriple)
  private
    FGen : TRDFGenerator;
    FPredicates : TFslList<TRDFPredicate>;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(gen : TRDFGenerator);
    destructor Destroy; override;
    function write(b : TStringBuilder; indent : integer) : boolean;
    function predicate(predicate, obj : String) : TRDFComplex; overload;
    function predicate(predicate, obj, xtype : String) : TRDFComplex; overload;
    function predicate(predicate : String; obj : TRDFTriple) : TRDFComplex; overload;
    function predicate(predicate : String) : TRDFComplex; overload;
    function complex : TRDFComplex;
  end;

  TRDFPredicate = class (TFslObject)
  private
    FPredicate : String;
    FObj : TRDFTriple;
    FComment : String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; override;
    function Link : TRDFPredicate; overload;
  end;

  TRDFSubject = class (TRDFComplex)
  private
    id : String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(gen : TRDFGenerator);
    destructor Destroy; override;
    function Link : TRDFSubject; overload;

    function predicate(predicate : String; obj : TRDFTriple; comment : String) : TRDFPredicate; overload;
    procedure comment(comment : String);
    procedure labl(labl: String);
  end;

  TRDFSection = class (TFslObject)
  private
    FGen : TRDFGenerator;
    FName : String;
    FSubjects : TFslList<TRDFSubject>;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(gen : TRDFGenerator);
    destructor Destroy; override;
    function Link : TRDFSection; overload;

    property name : String read FName;

    function triple(subject : String; predicate : String; obj : String; comment : String): TRDFSubject; overload;
    function triple(subject : String; predicate : String; obj : String): TRDFSubject; overload;
    function triple(subject : String; predicate : String; obj : TRDFTriple): TRDFSubject; overload;
    function triple(subject : String; predicate : String; obj : TRDFTriple; comment : String): TRDFSubject; overload;
    procedure comment(subject : String; comment : String);
    procedure labl(subject : String; labl : String);
    function subject(subject : String): TRDFSubject;
  end;

  TRDFGenerator = class (TFslObject)
  private
    FSections : TFslList<TRDFSection>;
    FSubjectSet : TList<String>;
    FPredicateSet : TList<String>;
    FObjectSet : TList<String>;
    FPrefixes : TFslStringDictionary;
    FFormat: TRDFFormat;
    FLastId : integer;

    procedure ln(b: TStringBuilder; s : String);
    function sorted(list : TEnumerable<String>) : TArray<String>;
    procedure checkPrefix(pname : String); overload;
    procedure checkPrefix(obj : TRDFTriple);  overload;
    function hasSection(sn : String) : boolean;
    procedure writeTurtlePrefixes(b : TStringBuilder; header : boolean);
    procedure writeTurtleSection(b : TStringBuilder; section : TRDFSection);
    procedure writeNTripleSection(b : TStringBuilder; section : TRDFSection);
    procedure writeNTripleComplex(b: TStringBuilder; complex: TRDFComplex);
    procedure writeNTriple(b: TStringBuilder; url1, url2, url3: String);
    function fullUrl(s: String): String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TRDFGenerator; overload;

    property format : TRDFFormat read FFormat write FFormat;
    procedure prefix(code, url : String);
    function section(sn: String) : TRDFSection;
    procedure generate(b : TStringBuilder; header : boolean);
  end;

function ttlLiteral(s : String) : String;

implementation

function ttlLiteral(s : String) : String;
begin
  result := '"'+jsonEscape(s, true)+'"';
end;

function pctEncode(s : String; isString : boolean) : String;
var
  b : TStringBuilder;
  c : char;
begin
  if s = '' then
    exit('');
  b := TStringBuilder.Create;
  try
    for c in s do
    begin
      if (c >= 'A') and (c <= 'Z') then
        b.append(c)
      else if (c >= 'a') and (c <= 'z') then
        b.append(c)
      else if (c >= '0') and (c <= '9') then
        b.append(c)
      else if (c = '.') then
        b.append(c)
      else if (ord(c) < 255) then
        b.append('%'+inttohex(ord(c), 2))
      else
        b.append('%'+inttohex(ord(c), 4));
    end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;


function literal(s : String) : TRDFString;
begin
  result := TRDFString.create('"'+jsonEscape(s, true)+'"');
end;


{ TRDFString }

constructor TRDFString.create(value: String);
begin
  inherited Create;
  FValue := value;
end;

function TRDFString.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
  inc(result, (FType.length * sizeof(char)) + 12);
end;

{ TRDFComplex }

function TRDFComplex.complex: TRDFComplex;
begin
  result := TRDFComplex.Create(FGen);
end;

constructor TRDFComplex.Create(gen : TRDFGenerator);
begin
  inherited Create;
  FPredicates := TFslList<TRDFPredicate>.create;
  FGen := gen;
end;

destructor TRDFComplex.Destroy;
begin
  FPredicates.Free;
  inherited;
end;


function TRDFComplex.predicate(predicate, obj: String): TRDFComplex;
begin
  result := self.predicate(predicate, TRDFString.create(obj));
end;

function TRDFComplex.predicate(predicate: String; obj: TRDFTriple): TRDFComplex;
var
  p : TRDFPredicate;
begin
  if not Fgen.FPredicateSet.contains(predicate) then
    Fgen.FPredicateSet.add(predicate);
  if (obj is TRDFString) and not Fgen.FObjectSet.contains(TRDFString(obj).FValue) then
    Fgen.FObjectSet.add(TRDFString(obj).FValue);

  p := TRDFPredicate.Create;
  try
    p.FPredicate := predicate;
    p.FObj := obj;
    FPredicates.Add(p.Link);
  finally
    p.Free;
  end;
  result := self;
end;

function TRDFComplex.write(b: TStringBuilder; indent: integer): boolean;
var
  left : String;
  i : integer;
  po : TRDFPredicate;
begin
  if (Fpredicates.Count = 0) then
    exit(false);

  if (Fpredicates.Count = 1) and (Fpredicates[0].Fobj is TRDFString) and (Fpredicates[0].Fcomment = '') then
  begin
    b.Append(' '+Fpredicates[0].Fpredicate+' '+TRDFString(Fpredicates[0].Fobj).Fvalue);
      if TRDFString(Fpredicates[0].FObj).FType <> '' then
        b.Append('^^'+TRDFString(Fpredicates[0].FObj).FType);
    exit(false);
  end;

  result := true;
  left := StringpadLeft('', ' ', indent);
  i := 0;
  for po in Fpredicates do
  begin
    b.Append(#13#10);
    if (po.FObj is TRDFString) then
    begin
      b.Append(left+' '+po.FPredicate+ ' '+TRDFString(po.FObj).Fvalue);
      if TRDFString(po.FObj).FType <> '' then
        b.Append('^^'+TRDFString(po.FObj).FType);
    end
    else
    begin
      b.Append(left+' '+po.FPredicate+' [');
      if TRDFComplex(po.FObj).write(b, indent+2) then
        b.Append(#13#10+left+' ]')
      else
        b.Append(' ]');
    end;
    inc(i);
    if (i < Fpredicates.count) then
          b.Append(';');
    if (po.Fcomment <> '') then
      b.Append(' # '+jsonEscape(po.Fcomment, false));
  end;
end;

function TRDFComplex.predicate(predicate: String): TRDFComplex;
begin
  result := complex;
  self.predicate(predicate, result);
end;

function TRDFComplex.predicate(predicate, obj, xtype: String): TRDFComplex;
var
  s : TRDFString;
begin
  s := TRDFString.create(obj);
  result := self.predicate(predicate, s);
  s.FType := xtype;
end;

function TRDFComplex.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FGen.sizeInBytes);
  inc(result, FPredicates.sizeInBytes);
end;

{ TRDFPredicate }

destructor TRDFPredicate.Destroy;
begin
  FObj.Free;
  inherited;
end;

function TRDFPredicate.Link: TRDFPredicate;
begin
  result := TRDFPredicate(inherited Link);
end;

function TRDFPredicate.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FPredicate.length * sizeof(char)) + 12);
  inc(result, FObj.sizeInBytes);
  inc(result, (FComment.length * sizeof(char)) + 12);
end;

{ TRDFSubject }

constructor TRDFSubject.Create(gen: TRDFGenerator);
begin
  inherited Create(gen);
end;

destructor TRDFSubject.Destroy;
begin
  inherited;
end;

function TRDFSubject.Link: TRDFSubject;
begin
  result := TRDFSubject(Inherited Link);
end;

function TRDFSubject.predicate(predicate: String; obj: TRDFTriple; comment: String): TRDFPredicate;
var
  p : TRDFPredicate;
begin
  if not FGen.FSubjectSet.contains(id) then
    FGen.FSubjectSet.add(id);
  if not Fgen.FPredicateSet.contains(predicate) then
    Fgen.FPredicateSet.add(predicate);
  if (obj is TRDFString) and not Fgen.FObjectSet.contains(TRDFString(obj).FValue) then
    Fgen.FObjectSet.add(TRDFString(obj).FValue);

  p := TRDFPredicate.Create;
  try
    p.FPredicate := predicate;
    p.FObj := obj;
    p.FComment := comment;
    FPredicates.Add(p.Link);
    result := p;
  finally
    p.Free;
  end;
end;

procedure TRDFSubject.labl(labl: String);
begin
  if (labl <> '')  then
  begin
    predicate('rdfs:label', literal(labl));
    predicate('dc:title', literal(labl));
  end;
end;

procedure TRDFSubject.comment(comment: String);
begin
  if (comment <> '')  then
  begin
    predicate('rdfs:comment', literal(comment));
    predicate('dcterms:description', literal(comment));
  end;
end;


function TRDFSubject.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (id.length * sizeof(char)) + 12);
end;

{ TRDFSection }

constructor TRDFSection.Create(gen: TRDFGenerator);
begin
  inherited create;
  FGen := gen;
  FSubjects := TFslList<TRDFSubject>.create;
end;

destructor TRDFSection.Destroy;
begin
  FSubjects.Free;
  inherited;
end;

function TRDFSection.Link: TRDFSection;
begin
  result := TRDFSection(inherited Link);
end;

function TRDFSection.subject(subject: String): TRDFSubject;
var
  ss : TRDFSubject;
begin
  for ss in Fsubjects do
    if (ss.id = subject) then
      exit(ss);

  result := TRDFSubject.Create(FGen);
  FSubjects.Add(result);
  result.id := subject;
end;

function TRDFSection.triple(subject, predicate, obj, comment: String): TRDFSubject;
begin
  result := triple(subject, predicate, TRDFString.create(obj), comment);
end;

function TRDFSection.triple(subject, predicate, obj: String): TRDFSubject;
begin
  result := triple(subject, predicate, TRDFString.create(obj), '');
end;

function TRDFSection.triple(subject, predicate: String; obj: TRDFTriple): TRDFSubject;
begin
  result := triple(subject, predicate, obj, '');
end;

function TRDFSection.triple(subject, predicate: String; obj: TRDFTriple; comment: String): TRDFSubject;
begin
  result := self.subject(subject);
  result.predicate(predicate, obj, comment);
end;

procedure TRDFSection.comment(subject, comment: String);
begin
  if (comment <> '')  then
  begin
    triple(subject, 'rdfs:comment', literal(comment));
    triple(subject, 'dcterms:description', literal(comment));
  end;
end;

procedure TRDFSection.labl(subject, labl: String);
begin
  if (labl <> '')  then
  begin
    triple(subject, 'rdfs:label', literal(labl));
    triple(subject, 'dc:title', literal(labl));
  end;
end;

function TRDFSection.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FGen.sizeInBytes);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FSubjects.sizeInBytes);
end;

{ TRDFGenerator }

constructor TRDFGenerator.Create();
begin
  inherited Create;
  FSections := TFslList<TRDFSection>.create;
  FSubjectSet := TList<String>.create;
  FPredicateSet := TList<String>.create;
  FObjectSet := TList<String>.create;
  FPrefixes := TFslStringDictionary.create;
end;

destructor TRDFGenerator.Destroy;
begin
  FSections.Free;
  FSubjectSet.Free;
  FPredicateSet.Free;
  FObjectSet.Free;
  FPrefixes.Free;
  inherited;
end;

function TRDFGenerator.Link: TRDFGenerator;
begin
  result := TRDFGenerator(inherited link);
end;

procedure TRDFGenerator.ln(b: TStringBuilder; s: String);
begin
  b.Append(s);
  b.Append(#13#10);
end;

function TRDFGenerator.hasSection(sn: String): boolean;
var
  s : TRDFSection;
begin
  result := false;
  for s in FSections do
    if (s.FName = sn) then
      exit(true);
end;

function TRDFGenerator.section(sn: String): TRDFSection;
begin
  if (hasSection(sn)) then
    raise ERdfException.create('Duplicate section name '+sn);

  result := TRDFSection.Create(self);
  FSections.add(result);
  result.FName := sn;
end;


function TRDFGenerator.sorted(list: TEnumerable<String>): TArray<String>;
var
  sort : TStringList;
  s : String;
begin
  sort := TStringList.Create;
  try
    for s in list do
      sort.Add(s);
    sort.Sort;
    result := sort.ToStringArray;
  finally
    sort.Free;
  end;
end;

procedure TRDFGenerator.prefix(code, url: String);
begin
  Fprefixes.AddOrSetValue(code, url);
end;

procedure TRDFGenerator.checkPrefix(obj: TRDFTriple);
var
  co : TRDFComplex;
  po : TRDFPredicate;
begin
  if (obj is TRDFString) then
    checkPrefix(TRDFString(obj).Fvalue)
  else
  begin
    co := TRDFComplex(obj);
    for po in co.FPredicates do
    begin
      checkPrefix(po.FPredicate);
      checkPrefix(po.FObj);
    end;
  end;
end;

procedure TRDFGenerator.checkPrefix(pname: String);
var
  prefix : string;
begin
  if (pname.startsWith('(')) then
    exit;
  if (pname.startsWith('"')) then
    exit;
  if (pname.startsWith('<')) then
    exit;

  if (pname.contains(':')) then
  begin
    prefix := pname.substring(0, pname.indexOf(':'));
    if (not Fprefixes.containsKey(prefix) and (prefix <> 'http') and (prefix <> 'urn')) then
      raise ERdfException.create('undefined prefix '+prefix);
  end;
end;

function TRDFGenerator.fullUrl(s : String) : String;
var
  prefix, tail : string;
begin
  if s = 'a' then
    result := fullUrl('rdfs:type')
  else if s.StartsWith('"') then
    result := s
  else if not s.Contains(':') then
    result := '"'+s+'"'
  else if s.StartsWith('<') then
    result := s.Substring(1, s.Length-2)
  else
  begin
    StringSplit(s, ':', prefix, tail);
    if FPrefixes.ContainsKey(prefix) then
      result := FPrefixes[prefix]+tail
    else
      result := '"'+s+'"';
  end;
end;

procedure TRDFGenerator.writeNTriple(b: TStringBuilder; url1, url2, url3 : String);
begin
  b.Append(url1);
  b.Append(' ');
  b.Append(url2);
  b.Append(' ');
  b.Append(url3);
  b.Append(#13#10);
end;

procedure TRDFGenerator.writeNTripleComplex(b: TStringBuilder; complex : TRDFComplex);
var
  pred : TRDFPredicate;
begin
  for pred in complex.FPredicates do
    if (pred.FObj is TRDFComplex) then
    begin
      inc(FLastId);
      if not complex.uri.Contains('#') then
        pred.FObj.uri := complex.uri + '#' + inttostr(FLastId)
      else
        pred.FObj.uri := complex.uri + '.' + inttostr(FLastId);
      writeNTriple(b, complex.uri, fullUrl(pred.FPredicate), pred.FObj.uri);
    end
    else
      writeNTriple(b, complex.uri, fullUrl(pred.FPredicate), fullUrl(TRDFString(pred.FObj).FValue));

  for pred in complex.FPredicates do
    if (pred.FObj is TRDFComplex) then
      writeNTripleComplex(b, pred.FObj as TRDFComplex);
end;

procedure TRDFGenerator.writeNTripleSection(b: TStringBuilder; section: TRDFSection);
var
  subject : TRDFSubject;
begin
  for subject in section.FSubjects do
  begin
    subject.uri := fullUrl(subject.id);
    writeNTripleComplex(b, subject);
  end;
end;

procedure TRDFGenerator.writeTurtlePrefixes(b: TStringBuilder; header : boolean);
var
  p : String;
begin
  if (header) then
  begin
    ln(b, '# FHIR Turtle');
    ln(b, '# see http://hl7.org/fhir/rdf.html');
    ln(b, '');
  end;
  for p in sorted(Fprefixes.Keys) do
    ln(b, '@prefix '+p+': <'+Fprefixes[p]+'> .');
  ln(b, '');
end;

procedure TRDFGenerator.writeTurtleSection(b: TStringBuilder; section: TRDFSection);
var
  sbj : TRDFSubject;
  i : integer;
  p : TRDFPredicate;
  comment : String;
begin
  ln(b, '# - '+section.name+' '+StringpadLeft('', '-', 75-section.name.length));
  ln(b, '');
  for sbj in section.Fsubjects do
  begin
    b.append(sbj.id);
    b.append(' ');
    i := 0;

    for p in sbj.Fpredicates do
    begin
      b.append(p.Fpredicate);
      b.append(' ');
      if (p.FObj is TRDFString) then
        b.append(TRDFString(p.FObj).Fvalue)
      else
      begin
        b.append('[');
        if (TRDFComplex(p.FObj).write(b, 4)) then
          b.append(#13#10+'  ]')
        else
          b.append(']');
      end;
      comment := '';
      if p.Fcomment <> '' then
        comment := ' # '+p.Fcomment;
      inc(i);
      if (i < sbj.Fpredicates.count) then
        b.append(';'+comment+#13#10+'  ')
      else
        b.append('.'+comment+#13#10);
    end;
  end;
end;

procedure TRDFGenerator.generate(b: TStringBuilder; header: boolean);
var
  s : TRDFSection;
begin
  case FFormat of
    rdfTurtle:
      begin
      writeTurtlePrefixes(b, header);
      for s in Fsections do
        writeTurtleSection(b, s);
      ln(b, '# -------------------------------------------------------------------------------------');
      end;
    rdfXMl:
      begin
      raise ERdfException.create('Not supported yet');
      end;
    rdfNTriple:
      begin
      FLastId := 0;
      for s in Fsections do
        writeNTripleSection(b, s);
      end;
  end;
end;

function TRDFGenerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FSections.sizeInBytes);
  inc(result, FPrefixes.sizeInBytes);
end;

end.
