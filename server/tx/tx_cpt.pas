unit tx_cpt;

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

{$i fhir.inc}

interface

uses
  SysUtils, Classes, Generics.Collections,
  fsl_base, fsl_utilities, fsl_http, fsl_threads, fsl_lang, fsl_logging,
  fdb_manager, fdb_dialects,
  fhir_objects, fhir_common, fhir_factory, fhir_utilities, fhir_features, fhir_uris,
  fhir_cdshooks,
  ftx_service;

type
  { TCPTConceptDesignation }

  TCPTConceptDesignation = class (TFslObject)
  private
    FKind: String;
    FValue: String;
  public
    Function Link : TCPTConceptDesignation; overload;

    property kind : String read FKind write FKind;
    property value : String read FValue write FValue;
  end;

  { TCPTConceptProperty }

  TCPTConceptProperty = class (TFslObject)
  private
    FName: String;
    FValue: String;
  public
    Function Link : TCPTConceptProperty; overload;

    property name : String read FName write FName;
    property value : String read FValue write FValue;
  end;

  { TCPTConcept }

  TCPTConcept = class (TCodeSystemProviderContext)
  private
    FCode: String;
    FModifier: boolean;
    FDesignations: TFslList<TCPTConceptDesignation>;
    FProperties: TFslList<TCPTConceptProperty>;
  public
    constructor Create; override;
    destructor Destroy; Override;
    Function Link : TCPTConcept; overload;

    property code : String read FCode write FCode;
    property modifier : boolean read FModifier write FModifier;
    property designations : TFslList<TCPTConceptDesignation> read FDesignations;
    property properties : TFslList<TCPTConceptProperty> read FProperties;

    procedure addProperty(name, value: String);
    function hasProperty(name, value: String) : boolean;
    procedure addDesignation(kind, value: String);
    function getDesignation(kind: String) : String;
  end;

  { TCPTFilterContext }

  TCPTFilterContext = class (TCodeSystemProviderFilterContext)
  private
    FName : String;
    FClosed : boolean;
    FIndex : integer;
    FList : TFslList<TCPTConcept>;
  public
    constructor create(name : String; list : TFslList<TCPTConcept>; closed : boolean);
    destructor Destroy; override;

    property closed : boolean read FClosed;
    property index : integer read FIndex;
    property list : TFslList<TCPTConcept> read FList;

    procedure next;
  end;

  { TCPTExpression }

  TCPTExpression  = class (TCodeSystemProviderContext)
  private
    FFocus: TCPTConcept;
    FModifiers: TFslList<TCPTConcept>;
    procedure SetFocus(AValue: TCPTConcept);
  public      
    constructor Create; override;
    destructor Destroy; Override;     
    Function Link : TCPTExpression; overload;

    property focus : TCPTConcept read FFocus write SetFocus;
    property modifiers : TFslList<TCPTConcept> read FModifiers;

    function expression : String;
  end;

  { TCPTIteratorContext }

  TCPTIteratorContext = class (TCodeSystemIteratorContext)
  private
    FList : TFslList<TCPTConcept>;
  public
    constructor Create(list : TFslList<TCPTConcept>);
    destructor Destroy; Override;
  end;

  { TCPTServices }

  TCPTServices = class (TCodeSystemProvider)
  private
    db : TFDBManager;
    FVersion : String;
    FMap : TFslMap<TCPTConcept>;
    FList : TFslList<TCPTConcept>;
    FBase : TFslList<TCPTConcept>;
    FModifier : TFslList<TCPTConcept>;

    function parse(code : String; var msg : String) : TCPTExpression;
    procedure load;
  public
    constructor Create(languages : TIETFLanguageDefinitions; db : TFDBManager);
    destructor Destroy; Override;
    Function Link : TCPTServices; overload;

    class function checkDB(conn : TFDBConnection) : String;

    function expandLimitation : Integer; override;
    function systemUri(context : TCodeSystemProviderContext) : String; override;
    function version(context : TCodeSystemProviderContext) : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function description : String; override;
    function TotalCount : integer;  override;
    function getIterator(context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    function getDisplay(code : String; const lang : THTTPLanguages):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function sameContext(a, b : TCodeSystemProviderContext) : boolean; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string; override;
    procedure Designations(context : TCodeSystemProviderContext; list : TConceptDesignations); override;
    function Definition(context : TCodeSystemProviderContext) : string; override;

    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure getCDSInfo(card : TCDSHookCard; const lang : THTTPLanguages; baseURL, code, display : String); override;
    procedure extendLookup(factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; const lang : THTTPLanguages; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;
    //function subsumes(codeA, codeB : String) : String; override;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
    procedure defineFeatures(features : TFslList<TFHIRFeature>); override;
  end;

implementation

{ TCPTFilterContext }

constructor TCPTFilterContext.create(name : String; list: TFslList<TCPTConcept>; closed: boolean);
var
  i : integer;
  s : String;
begin
  inherited create;
  FName := name;
  FList := list;
  FClosed := closed;
  FIndex := -1;
  s := '';
  for i := 0 to integerMin(list.count, 50) do
    s := s+list[i].code+',';
  for i := integerMax(0, list.count - 10) to list.count - 1 do
    s := s+','+list[i].code;
  Logging.log('CPT filter '+name+': '+inttostr(list.count)+' concepts in filter (closed = '+boolToStr(FClosed)+'): '+s);
end;

destructor TCPTFilterContext.Destroy;
begin
  FList.free;
  inherited Destroy;
end;

procedure TCPTFilterContext.next;
begin
  inc(FIndex);
end;

{ TCPTIteratorContext }

constructor TCPTIteratorContext.Create(list: TFslList<TCPTConcept>);
begin
  if list = nil then
    inherited create(nil, 0)
  else
    inherited create(nil, list.Count);
  FList := list;
end;

destructor TCPTIteratorContext.Destroy;
begin
  FList.free;
  inherited Destroy;
end;

{ TCPTConceptProperty }

function TCPTConceptProperty.Link: TCPTConceptProperty;
begin
  result := TCPTConceptProperty(inherited Link);
end;


{ TCPTConceptDesignation }

function TCPTConceptDesignation.Link: TCPTConceptDesignation;
begin
  result := TCPTConceptDesignation(inherited link);
end;

{ TCPTConcept }

constructor TCPTConcept.Create;
begin
  inherited Create;
  FDesignations := TFslList<TCPTConceptDesignation>.create;
  FProperties := TFslList<TCPTConceptProperty>.create;
end;

destructor TCPTConcept.Destroy;
begin
  FProperties.Free;
  FDesignations.Free;
  inherited Destroy;
end;

function TCPTConcept.Link: TCPTConcept;
begin
  result := TCPTConcept(inherited link);
end;

procedure TCPTConcept.addProperty(name, value: String);
var
  p : TCPTConceptProperty;
begin
  p := TCPTConceptProperty.create;
  try
    p.name := name;
    p.value := value;
    FProperties.add(p.link);
  finally
    p.free;
  end;
end;

function TCPTConcept.hasProperty(name, value: String): boolean;
var
  c : TCPTConceptProperty;
begin
  result := false;
  for c in Properties do
    if (c.name = name) and (c.value = value) then
        exit(true);
end;

procedure TCPTConcept.addDesignation(kind, value: String);
var
  d : TCPTConceptDesignation;
begin
  d := TCPTConceptDesignation.create;
  try
    d.kind := kind;
    d.value := value;
    FDesignations.add(d.link);
  finally
    d.free;
  end;
end;

function TCPTConcept.getDesignation(kind: String): String; 
var
  d : TCPTConceptDesignation;
begin
  result := '';           
  for d in FDesignations do
    if (d.kind = kind) then
        exit(d.value);
end;

{ TCPTExpression }

constructor TCPTExpression.Create;
begin
  inherited Create;
  FModifiers := TFslList<TCPTConcept>.create;
end;

destructor TCPTExpression.Destroy;
begin
  FModifiers.Free;
  FFocus.Free;
  inherited Destroy;
end;

function TCPTExpression.Link: TCPTExpression;
begin
  result := TCPTExpression(inherited link);
end;

function TCPTExpression.expression: String;
var
  m  : TCPTConcept;
begin
  result := focus.code;
  for m in modifiers do
    result := result + ':' + m.code;
end;

procedure TCPTExpression.SetFocus(AValue: TCPTConcept);
begin
  FFocus.Free;
  FFocus := AValue;
end;

{ TCPTServices }

constructor TCPTServices.Create(languages : TIETFLanguageDefinitions; db : TFDBManager);
begin
  inherited Create(languages);
  FMap := TFslMap<TCPTConcept>.create;
  FMap.defaultValue := nil;
  FList := TFslList<TCPTConcept>.create;
  FBase := TFslList<TCPTConcept>.create;
  FModifier := TFslList<TCPTConcept>.create;
  self.db := db.link;
  load;
end;

destructor TCPTServices.Destroy;
begin
  db.Free;
  FMap.Free;
  FBase.Free;
  FModifier.Free;
  FList.Free;
  inherited Destroy;
end;

function TCPTServices.Link: TCPTServices;
begin
  result := TCPTServices(inherited link);
end;

class function TCPTServices.checkDB(conn : TFDBConnection) : String;
var
  meta : TFDBMetaData;
begin
  meta := conn.FetchMetaData;
  try
    if not meta.HasTable('Information') or not meta.HasTable('Concepts') or not meta.HasTable('Designations') or not meta.HasTable('Properties') then
      result := 'Missing Tables - needs re-importing'
    else
      result := 'OK ('+inttostr(conn.countSql('Select count(*) from Concepts'))+' Concepts)';
  finally
    meta.free;
  end;
end;

function TCPTServices.expandLimitation: Integer;
begin
  Result := 1000; // agreement with AMA
end;

function TCPTServices.parse(code: String; var msg: String): TCPTExpression;
var
  parts : TArray<String>;
  i : integer;
  c : TCPTConcept;
  exp : TCPTExpression;
begin
  result := nil;
  if (code = '') then
    msg := 'No Expression Found'
  else
  begin
    msg := '';
    parts := code.split([':']);
    c := FMap[parts[0]];
    if (c = nil) then
      msg := 'Base CPT Code '''+parts[0]+''' not found'
    else
    begin
      exp := TCPTExpression.create;
      try
        exp.focus := c.link;
        for i := 1 to length(parts) - 1 do
        begin
          c := FMap[parts[i]];
          if c = nil then
          begin
            msg := 'Modifier CPT code '''+parts[i]+''' not found';
            exit(nil);
          end
          else
            exp.modifiers.add(c.link);
        end;
        result := exp.link;
      finally
        exp.free;
      end;
    end;
  end;
end;

procedure TCPTServices.load;
var
  conn : TFDBConnection;
  c : TCPTConcept;
begin
  conn :=  db.GetConnection('load');
  try
    conn.SQL := 'Select * from Information';
    conn.prepare;
    conn.Execute;
    while conn.FetchNext do
      if conn.ColStringByName['name'] = 'version' then
        FVersion := conn.ColStringByName['value'];
    conn.terminate;

    conn.SQL := 'Select * from Concepts';
    conn.prepare;
    conn.Execute;
    while conn.FetchNext do
    begin
      c := TCPTConcept.create;
      try
        c.code := conn.ColStringByName['code'];
        c.modifier :=  conn.ColIntegerByName['modifier'] = 1;
        FMap.Add(c.code, c.link);
        if c.modifier then
          FModifier.Add(c.link)
        else
          FBase.Add(c.link);
        FList.add(c.Link);
      finally
        c.Free;
      end;
    end;
    conn.terminate;

    conn.SQL := 'Select * from Properties'; 
    conn.prepare;
    conn.Execute;
    while conn.FetchNext do
    begin
      c := FMap[conn.ColStringByName['code']];
      c.addProperty(conn.ColStringByName['name'], conn.ColStringByName['value']);
    end;
    conn.terminate;

    conn.SQL := 'Select * from Designations';
    conn.prepare;
    conn.Execute;
    while conn.FetchNext do
    begin
      c := FMap[conn.ColStringByName['code']];
      c.addDesignation(conn.ColStringByName['type'], conn.ColStringByName['value']);
    end;
    conn.terminate;

    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;
end;

function TCPTServices.systemUri(context : TCodeSystemProviderContext) : String;
begin
  result := 'http://www.ama-assn.org/go/cpt';
end;

function TCPTServices.version(context : TCodeSystemProviderContext) : String;
begin
  result := FVersion;
end;

function TCPTServices.name(context : TCodeSystemProviderContext) : String;
begin
  result := 'AmaCPT';
end;

function TCPTServices.description : String;
begin
  result := 'CPT © Copyright 2019 American Medical Association. All rights reserved. AMA and CPT are registered trademarks of the American Medical Association.';
end;

function TCPTServices.TotalCount : integer;
begin
  result := FMap.Count;
end;

function TCPTServices.locate(code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext;
begin
  if code.Contains(':') then
    result := parse(code, message)
  else
  begin
    result := FMap[code].link;
    if result = nil then
      message := 'Code '''+code+''' not found in CPT';
  end;
end;

function TCPTServices.getDisplay(code : String; const lang : THTTPLanguages):String;
var
  c : TCPTConcept;
begin
  c := FMap[code];
  if (c = nil) or c.designations.Empty then
    result := ''
  else
    result := c.designations[0].value;
end;

function TCPTServices.getDefinition(code : String):String;
var
  c : TCPTConcept;
begin
  c := FMap[code];
  if (c = nil) or c.designations.Empty then
    result := ''
  else
    result := c.designations[0].value;
end;


function TCPTServices.locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  result := nil;
end;

function TCPTServices.sameContext(a, b : TCodeSystemProviderContext) : boolean;
begin
  result := a = b;
end;

function TCPTServices.IsAbstract(context : TCodeSystemProviderContext) : boolean;
var
  e : TCPTExpression;
  c : TCPTConcept;
begin
  if (context is TCPTExpression) then
  begin
    e := (context as TCPTExpression);
    result := false;
  end
  else
  begin
    c := (context as TCPTConcept);
    result := c.hasProperty('kind', 'metadata');
  end;
end;

function TCPTServices.Code(context : TCodeSystemProviderContext) : string;
var
  e : TCPTExpression;
  c : TCPTConcept;
begin
  if (context is TCPTExpression) then
  begin
    e := (context as TCPTExpression);
    result := e.expression;
  end
  else
  begin
    c := (context as TCPTConcept);
    result := c.code;
  end;
end;

function TCPTServices.Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string;
var
  e : TCPTExpression;
  c : TCPTConcept;
begin
  if (context is TCPTExpression) then
  begin
    e := (context as TCPTExpression);
    result := '';
  end
  else
  begin
    c := (context as TCPTConcept);
    if c.designations.Empty then
      result := ''
    else
      result := c.designations[0].value;
  end;
end;

procedure TCPTServices.Designations(context : TCodeSystemProviderContext; list : TConceptDesignations);
var
  c : TCPTConcept;
  d : TCPTConceptDesignation;
  e : TCPTExpression;
begin
  if (context is TCPTExpression) then
  begin
    e := (context as TCPTExpression);
    // no text for expressions
  end
  else
  begin
    c := (context as TCPTConcept);
    c := (context as TCPTConcept);
    for d in c.designations do
      list.addDesignation('en', d.value, d.kind = 'display');
  end;
end;

function TCPTServices.Definition(context : TCodeSystemProviderContext) : string;
begin
  result := Display(context, defLang);
end;

procedure TCPTServices.getCDSInfo(card : TCDSHookCard; const lang : THTTPLanguages; baseURL, code, display : String);
begin
end;

procedure TCPTServices.extendLookup(factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; const lang : THTTPLanguages; props : TArray<String>; resp : TFHIRLookupOpResponseW);

var
  c : TCPTConcept;
  d : TCPTConceptDesignation;
  p : TCPTConceptProperty;
  pp: TFHIRLookupOpRespPropertyW;
  pp1 : TFHIRLookupOpRespSubPropertyW;
  e : TCPTExpression;
begin
  if (ctxt is TCPTExpression) then
  begin
    e := (ctxt as TCPTExpression);
    extendLookup(factory, e.focus, lang, props, resp);
    for c in e.modifiers do
    begin
      pp := resp.addProp('modifier');
      pp1 := pp.addSubProp('code');
      pp1.value := c.code;
      if (not c.designations.Empty) then
      begin
        pp1 := pp.addSubProp('definition');
        pp1.value := c.designations[0].value;
      end;
    end;
  end
  else
  begin
    pp := resp.addProp('copyright');
    pp.value := factory.makeString('This response content from SNOMED CT, which is copyright ) 2002+ International Health Terminology Standards Development Organisation (IHTSDO), and distributed '+'by agreement between IHTSDO and HL7. Implementer use of SNOMED CT is not covered by this agreement');

    c := (ctxt as TCPTConcept);
    if hasProp(props, 'designation', true) then
      for d in c.designations do
        resp.addDesignation('en', 'http://www.ama-assn.org/go/cpt', '', d.kind, d.value);

    for p in c.properties do
    begin
      if hasProp(props, p.name, true) then
      begin
        pp := resp.addProp(p.name);
        pp.value := factory.makeString(p.value);
      end;
    end;
  end;
end;


function TCPTServices.getIterator(context : TCodeSystemProviderContext) : TCodeSystemIteratorContext;
begin
  if (context = nil) then
    result := TCPTIteratorContext.create(FList.link)
  else
    result := TCPTIteratorContext.create(nil);
end;

function TCPTServices.getNextContext(context : TCodeSystemIteratorContext) : TCodeSystemProviderContext;
var
  c : TCPTIteratorContext;
begin
  c := context as TCPTIteratorContext;
  if (c.FList = nil) or (c.current >= c.FList.Count) then
    result := nil
  else
  begin
    result := c.FList[c.current].link;
    context.next;
  end;
end;


function TCPTServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.create('Not supported yet');
end;

function TCPTServices.filter(forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
var
  list : TFslList<TCPTConcept>;
  item : TCPTConcept;
  b : boolean;
begin
  // filters supported
  //  * modified = false
  //  * modifier = true / false
  //  * kind = x

  // todo:
  //   code in 86701-86703;87389-87389

  if (prop = 'modifier') then
  begin
    b := value = 'true';
    if b then
      result := TCPTFilterContext.create('modifier:true', FModifier.link, true)
    else
      result := TCPTFilterContext.create('modifier:false', FBase.link, true)
  end
  else if (prop = 'modified') and (op = foEqual) then
  begin
    b := value = 'true';
    if (b) then
      result := TCPTFilterContext.create('modified:true', TFslList<TCPTConcept>.create, false)
    else
      result := TCPTFilterContext.create('modified:false', FList.link, true);
  end
  else if (prop = 'kind') and (op = foEqual) then
  begin
    list := TFslList<TCPTConcept>.create;
    try
      for item in Flist do
        if item.hasProperty('kind', value) then
          list.add(item.link);
      result := TCPTFilterContext.create('kind:'+value, list.link, true);
    finally
      list.free;
    end;
  end
  else
    result := nil;
end;

function TCPTServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
var
  fc : TCPTFilterContext;
  c : TCPTConcept;
begin
  fc := ctxt as TCPTFilterContext;
  result := nil;
  for c in fc.FList do
    if (c.code = code) then
      exit(c.link);
end;

function TCPTServices.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
var
  fc : TCPTFilterContext;
begin             
  fc := ctxt as TCPTFilterContext;
  fc.next;
  result := (fc.Index < fc.Flist.count);
end;

function TCPTServices.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
var
  fc : TCPTFilterContext;
begin        
  fc := ctxt as TCPTFilterContext;
  result := fc.FList[fc.index].link;
end;

function TCPTServices.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
var
  fc : TCPTFilterContext;
  e : TCPTExpression;
  c : TCPTConcept;
begin            
  fc := ctxt as TCPTFilterContext;
  if (concept is TCPTExpression) then
  begin
    e := (concept as TCPTExpression);
    result := not fc.closed;
  end
  else
  begin
    c := (concept as TCPTConcept);
    result := fc.FList.contains(c);
    //Logging.log(c.code +' in '+fc.FName+': '+boolToStr(result));
  end;
end;

function TCPTServices.isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean;
var
  fc : TCPTFilterContext;
begin
  if propFilter = nil then
    result := true
  else
  begin
    fc := propFilter as TCPTFilterContext;
    result := not fc.closed;
  end;
end;

procedure TCPTServices.Close(ctxt : TCodeSystemProviderFilterPreparationContext);
begin
  ctxt.free;
end;

procedure TCPTServices.Close(ctxt : TCodeSystemProviderContext);
begin
  ctxt.free;
end;

procedure TCPTServices.Close(ctxt : TCodeSystemProviderFilterContext);
begin
 ctxt.free;
end;

procedure TCPTServices.defineFeatures(features : TFslList<TFHIRFeature>);
begin
 // nothing
end;


end.

