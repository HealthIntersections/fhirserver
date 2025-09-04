unit tx_omop;

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
  Classes, SysUtils,
  fsl_base, fsl_utilities, fsl_http, fsl_threads, fsl_lang, fsl_i18n, fsl_logging,
  fdb_manager, fdb_dialects,
  fhir_objects, fhir_common, fhir_factory, fhir_utilities, fhir_features, fhir_uris,
  fhir_cdshooks,
  ftx_service;

type

  { TOMOPConcept }

  TOMOPConcept = class (TCodeSystemProviderContext)
  private
    FCode : String;
    FDisplay : String;
    FDomain: String;
    FConceptClass: String;
    FStandard: String;
    FVocabulary: String;
  public
    property Code : String read FCode write FCode;
    property Display : String read FDisplay write FDisplay;
    property standard : String read FStandard write FStandard;
    property domain : String read FDomain write FDomain;
    property conceptClass : String read FConceptClass write FConceptClass;
    property vocabulary : String read FVocabulary write FVocabulary;
  end;

  { TOMOPFilter }

  TOMOPFilter = class (TCodeSystemProviderFilterContext)
  private
    FConn : TFDBConnection;
    procedure SetConn(AValue: TFDBConnection);
  public
    destructor Destroy; override;

    property conn : TFDBConnection read FConn write SetConn;
  end;

  { TOMOPServices }

  TOMOPServices = class (TCodeSystemProvider)
  private
    db : TFDBManager;
    FVersion : String;
    procedure loadVersion;
    function makeCM(id, src, tgt : String; factory : TFHIRFactory): TFHIRConceptMapW;
  public
    constructor Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport; db : TFDBManager);
    destructor Destroy; Override;
    Function Link : TOMOPServices; overload;

    class function checkDB(conn : TFDBConnection) : String;

    function systemUri : String; override;
    function version : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function description : String; override;
    function TotalCount : integer;  override;
    function getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    function getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList):String; override;
    function getDefinition(opContext : TTxOperationContext; code : String):String; override;
    function locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(opContext : TTxOperationContext; code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function sameContext(opContext : TTxOperationContext; a, b : TCodeSystemProviderContext) : boolean; override;
    function IsAbstract(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean; override;
    function Code(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;
    function Display(opContext : TTxOperationContext; context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string; override;
    procedure Designations(opContext : TTxOperationContext; context : TCodeSystemProviderContext; list : TConceptDesignations); override;
    function Definition(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;

    function getPrepContext(opContext : TTxOperationContext) : TCodeSystemProviderFilterPreparationContext; override;
    function prepare(opContext : TTxOperationContext; prep : TCodeSystemProviderFilterPreparationContext) : boolean; override;

    function searchFilter(opContext : TTxOperationContext; filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function FilterMore(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function filterSize(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : integer; override;
    function FilterConcept(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(opContext : TTxOperationContext; textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure getCDSInfo(opContext : TTxOperationContext; card : TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display : String); override;
    procedure extendLookup(opContext : TTxOperationContext; factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; langList : THTTPLanguageList; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;
    //function subsumes(codeA, codeB : String) : String; override;
    function buildValueSet(factory : TFHIRFactory; id : String) : TFhirValueSetW;
    procedure getTranslations(coding: TFHIRCodingW; target : String; codes : TFslList<TCodeTranslation>); override;

    procedure registerConceptMaps(list : TFslList<TFHIRConceptMapW>; factory : TFHIRFactory); override;
    procedure defineFeatures(opContext : TTxOperationContext; features : TFslList<TFHIRFeature>); override;
  end;


implementation

{ TOMOPFilter }

procedure TOMOPFilter.SetConn(AValue: TFDBConnection);
begin
  FConn.free;
  FConn:=AValue;
end;

destructor TOMOPFilter.Destroy;
begin
  FConn.terminate;
  FConn.release;
  inherited Destroy;
end;

{ TOMOPServices }

constructor TOMOPServices.Create(languages: TIETFLanguageDefinitions; i18n : TI18nSupport; db: TFDBManager);
begin
  inherited Create(languages, i18n);
  self.db := db;
  loadVersion;
end;

destructor TOMOPServices.Destroy;
begin
  db.free;
  inherited Destroy;
end;

function TOMOPServices.Link: TOMOPServices;
begin
  result := TOMOPServices(inherited link);
end;

class function TOMOPServices.checkDB(conn: TFDBConnection): String;
var
  meta : TFDBMetaData;
begin
  meta := conn.FetchMetaData;
  try
    if not meta.HasTable('Relationships') or not meta.HasTable('Domains') or not meta.HasTable('ConceptClasses') or not meta.HasTable('Vocabularies')
        or not meta.HasTable('Concepts') or not meta.HasTable('ConceptSynonyms') or not meta.HasTable('ConceptRelationships') then
      result := 'Missing Tables - needs re-importing (by java)'
    else
      result := 'OK ('+inttostr(conn.countSql('Select count(*) from Concepts'))+' Concepts)';
  finally
    meta.free;
  end;
end;

procedure TOMOPServices.loadVersion;
var
  ver : String;
  conn : TFDBConnection;
begin
  conn := db.GetConnection('Version');
  try
    ver := conn.Lookup('Vocabularies', 'vocabulary_id', 'OMOP Extension', 'vocabulary_version', '');
    FVersion := ver.Substring(ver.LastIndexOf(' ')+1);
    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
    end;
  end;

end;

function TOMOPServices.systemUri: String;
begin
  result := 'https://fhir-terminology.ohdsi.org';
end;

function TOMOPServices.version: String;
begin
  Result := FVersion;
end;

function TOMOPServices.name(context: TCodeSystemProviderContext): String;
begin
  Result := 'OMOP Concepts';
end;

function TOMOPServices.description: String;
begin
  Result := 'OMOP Concepts, release '+FVersion;
end;

function TOMOPServices.TotalCount: integer;
begin
  result := db.countSql('Select count(*) from Concepts', 'TotalCount');
end;

function TOMOPServices.getDisplay(opContext : TTxOperationContext; code: String; langList : THTTPLanguageList): String;
var
  c : TOMOPConcept;
  msg : String;
begin
  c := locate(opContext, code, nil, msg) as TOMOPConcept;
  try
    if c <> nil then
      result := c.Display
    else
      result := '';
  finally
    c.free;
  end;
end;

function TOMOPServices.getDefinition(opContext : TTxOperationContext; code: String): String;
begin
  result := '';
end;

function TOMOPServices.locate(opContext : TTxOperationContext; code: String; altOpt : TAlternateCodeOptions; var message: String): TCodeSystemProviderContext;
var
  conn : TFDBConnection;
  c : TOMOPConcept;
begin
  conn := db.GetConnection('locate');
  try
    conn.sql := 'Select concept_id, concept_name, standard_concept, Domains.domain_id, ConceptClasses.concept_class_id, Vocabularies.vocabulary_id from Concepts, Domains, ConceptClasses, Vocabularies where Concepts.domain_id = Domains.domain_concept_id and ConceptClasses.concept_class_concept_id = Concepts.concept_class_id and Concepts.vocabulary_id = Vocabularies.vocabulary_concept_id and concept_id = '''+SQLWrapString(code)+'''';
    conn.Prepare;
    conn.Execute;
    if conn.FetchNext and (conn.ColStringByName['concept_id'] = code) then
    begin
      c := TOMOPConcept.Create;
      try
        c.code := code;
        c.display := conn.ColStringByName['concept_name'];
        c.standard := conn.ColStringByName['standard_concept'];
        if (c.standard = '') then
          c.standard := 'NS';
        c.domain := conn.ColStringByName['domain_id'];
        c.conceptClass := conn.ColStringByName['concept_class_id'];
        c.vocabulary := conn.ColStringByName['concept_class_id'];
        result := c.link;
      finally
        c.free;
      end;
    end
    else
      result := nil;
    conn.terminate;
    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
      raise
    end;
  end;
end;

function TOMOPServices.locateIsA(opContext : TTxOperationContext; code, parent: String; disallowParent: boolean): TCodeSystemProviderContext;
begin
  result := nil; // none
end;

function TOMOPServices.sameContext(opContext : TTxOperationContext; a, b: TCodeSystemProviderContext): boolean;
begin
  result := (a is TOMOPConcept) and (b is TOMOPConcept) and ((a as TOMOPConcept).code = (b as TOMOPConcept).code);
end;

function TOMOPServices.getIterator(opContext : TTxOperationContext; context: TCodeSystemProviderContext): TCodeSystemIteratorContext;
var
  qry : TFDBConnection;
begin
  qry := db.GetConnection('getIterator');
  try
    result := TCodeSystemIteratorContext.Create(nil, qry.CountSQL('Select count(concept_id) from Concepts'));
    qry.Release;
  except
    on e : Exception do
    begin
      qry.Error(e);
      recordStack(e);
      raise;
    end;
  end;
end;

function TOMOPServices.getNextContext(opContext : TTxOperationContext; context: TCodeSystemIteratorContext): TCodeSystemProviderContext;
begin
  raise ETerminologyError.Create('getNextContext not supported by RXNorm', itException); // only used when iterating the entire code system. and RxNorm is too big
end;

function TOMOPServices.IsAbstract(opContext : TTxOperationContext; context: TCodeSystemProviderContext): boolean;
begin
  result := false;
end;

function TOMOPServices.Code(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
begin
  if (context is TOMOPConcept) then
    result := (context as TOMOPConcept).code
  else
    result := '';
end;

function TOMOPServices.Display(opContext : TTxOperationContext; context: TCodeSystemProviderContext; langList : THTTPLanguageList): string;
begin
  if (context is TOMOPConcept) then
    result := (context as TOMOPConcept).display
  else
    result := '';
end;

function getLang(s :String) : String;
begin
  if      (s = 'English language') then result := 'en'
  else if (s = 'Spanish language') then result := 'es'
  else
  begin
    result := 'en'; // default
  end;
end;

procedure TOMOPServices.Designations(opContext : TTxOperationContext; context: TCodeSystemProviderContext; list: TConceptDesignations);
var
  conn : TFDBConnection;
begin
  if (context is TOMOPConcept) then
  begin
    list.addDesignation(true, true, '', 'en', (context as TOMOPConcept).Display);
    conn := db.GetConnection('display');
    try
      conn.sql := 'Select concept_synonym_name, concept_name from ConceptSynonyms, Concepts where ConceptSynonyms.language_concept_id = Concepts.concept_id and ConceptSynonyms.concept_id = '''+SQLWrapString((context as TOMOPConcept).code)+'''';
      conn.Prepare;
      conn.Execute;
      while conn.FetchNext do
        list.addDesignation(false, false, '', getLang(conn.ColStringByName['concept_name']), conn.ColStringByName['concept_synonym_name']);
      conn.terminate;
      conn.Release;
    except
      on e : Exception do
      begin
        conn.Error(e);
        raise
      end;
    end;
  end;
end;

function TOMOPServices.Definition(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TOMOPServices.getPrepContext(opContext : TTxOperationContext): TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

function TOMOPServices.prepare(opContext : TTxOperationContext; prep: TCodeSystemProviderFilterPreparationContext): boolean;
begin
  result := false;
end;

function TOMOPServices.searchFilter(opContext : TTxOperationContext; filter: TSearchFilterText; prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.Create('not done yet: searchFilter', itBusinessRule);
end;

function TOMOPServices.filter(opContext : TTxOperationContext; forExpansion, forIteration: boolean; prop: String; op: TFhirFilterOperator; value: String; prep: TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
var
  f : TOMOPFilter;
begin
  SetThreadStatus(ClassName+'.filter('+prop+CODES_TFhirFilterOperator[op]+value+')');
  if (prop = 'domain') and (op = foEqual) then
  begin
    f := TOMOPFilter.Create;
    try
      f.conn := db.GetConnection('filter');
      if (forExpansion or forIteration) then
      begin
        f.conn.sql := 'Select concept_id, concept_name, domain_id from Concepts where standard_concept = ''S'' and domain_id in (Select domain_concept_id from Domains where domain_id = '''+SQLWrapString(value)+''')';
        f.conn.Prepare;
        f.conn.Execute;
      end
      else
      begin
        f.conn.sql := 'Select concept_id, concept_name, domain_id from Concepts where standard_concept = ''S'' and domain_id in (Select domain_concept_id from Domains where domain_id = '''+SQLWrapString(value)+''') and concept_id = :cid';
        f.conn.Prepare;
      end;
      result := f.link;
    finally
      f.free;
    end;
  end
  else
    raise ETerminologyError.Create('filter "'+prop+' '+CODES_TFhirFilterOperator[op]+' '+value+'" not understood for OMOP', itBusinessRule);
end;

function TOMOPServices.filterLocate(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext; code: String; var message: String): TCodeSystemProviderContext;
var
  filter : TOMOPFilter;
  c : TOMOPConcept;
begin
  result := nil;
  filter := ctxt as TOMOPFilter;
  filter.conn.BindString('cid', code);
  filter.conn.execute;
  if (filter.conn.FetchNext) and (filter.conn.ColStringByName['concept_id'] = code) then
  begin
    c := TOMOPConcept.Create;
    try
      c.code := code;
      c.display := filter.conn.ColStringByName['concept_name'];
      c.domain := filter.conn.ColStringByName['domain_id'];
      result := c.link;
    finally
      c.free;
    end;
  end
  else
    message := 'Code '''+code+''' is not in the value set';
end;

function TOMOPServices.FilterMore(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): boolean;
begin
  result := (ctxt as TOMOPFilter).Conn.FetchNext;
end;

function TOMOPServices.filterSize(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): integer;
begin
  result := (ctxt as TOMOPFilter).Conn.RowsAffected;
end;

function TOMOPServices.FilterConcept(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
var
  conn : TFDBConnection;
  c : TOMOPConcept;
begin
  conn := (ctxt as TOMOPFilter).Conn;
  c := TOMOPConcept.Create;
  try
    c.code := conn.ColStringByName['concept_id'];
    c.display := conn.ColStringByName['concept_name'];
    c.domain := conn.ColStringByName['domain_id'];
    result := c.link;
  finally
    c.free;
  end;
end;

function TOMOPServices.InFilter(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext; concept: TCodeSystemProviderContext): Boolean;
begin
  raise ETerminologyError.Create('not done yet: InFilter', itBusinessRule);
end;

function TOMOPServices.isNotClosed(opContext : TTxOperationContext; textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

procedure TOMOPServices.getCDSInfo(opContext : TTxOperationContext; card: TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display: String);
begin
  raise ETerminologyError.Create('not done yet: getCDSInfo', itBusinessRule);
end;

function getVocabId(url : String) : Integer;
begin
  if (url = 'http://hl7.org/fhir/sid/icd-9-cm') then result := 5046
  else if (url = 'http://snomed.info/sct') then result := 44819097
  else if (url = 'http://hl7.org/fhir/sid/icd-10-cm') then result := 44819098
  else if (url = 'http://hl7.org/fhir/sid/icd-9-cm') then result := 44819099
  else if (url = 'http://www.ama-assn.org/go/cpt') then result := 44819100
  else if (url = 'http://terminology.hl7.org/CodeSystem/HCPCS-all-codes') then result := 44819101
  else if (url = 'http://loinc.org') then result := 44819102
  else if (url = 'http://www.nlm.nih.gov/research/umls/rxnorm') then result := 44819104
  else if (url = 'http://hl7.org/fhir/sid/ndc') then result := 44819105
  else if (url = 'http://unitsofmeasure.org') then result := 44819107
  else if (url = 'http://nucc.org/provider-taxonomy') then result := 44819137
  else if (url = 'http://www.whocc.no/atc') then result := 44819117
  else
    result := -1;

end;

function getUri(key : integer) : String;
begin
  case key of
//    252	RxNorm Extension
//5029	Cost Type
   5046: exit('http://hl7.org/fhir/sid/icd-9-cm'); //	ICD9CM
//32044	UB04 Typ bill
//32045	UB04 Point of Origin
//32046	UB04 Pri Typ of Adm
//32047	UB04 Pt dis status
//32471	Plan
//32472	Sponsor
//32473	SOPT
//32474	Plan Stop Reason
//32485	CDM
//32523	Episode
//32541	OSM
//32570	US Census
//32675	Metadata
//32724	Korean Revenue Code
//32758	OMOP Extension
//32808	Type Concept
//32887	Condition Status
//33069	Language
//581457	Cost
//44819096	None
  44819097: exit('http://snomed.info/sct'); //	SNOMED
  44819098: exit('http://hl7.org/fhir/sid/icd-10-cm'); //	ICD10CM
  44819099: exit('http://hl7.org/fhir/sid/icd-9-cm'); // 	ICD9Proc
  44819100: exit('http://www.ama-assn.org/go/cpt'); //	CPT4
  44819101: exit('http://terminology.hl7.org/CodeSystem/HCPCS-all-codes'); //	HCPCS
  44819102: exit('http://loinc.org'); //	LOINC
  44819104: exit('http://www.nlm.nih.gov/research/umls/rxnorm'); //	RxNorm
  44819105: exit('http://hl7.org/fhir/sid/ndc'); //	NDC
  44819107: exit('http://unitsofmeasure.org'); //	UCUM
//44819108	Gender
//44819109	Race
//44819110	CMS Place of Service
  44819117: exit('http://www.whocc.no/atc'); //	ATC
//44819119	Visit
//44819126	Drug Type
//44819127	Condition Type
//44819128	Procedure Type
//44819129	Observation Type
//44819133	Revenue Code
//44819134	Ethnicity
//44819135	Death Type
  44819137: exit('http://nucc.org/provider-taxonomy'); //	NUCC
//44819138	Medicare Specialty
//44819140	SPL
//44819146	Note Type
//44819147	Domain
//44819149	Obs Period Type
//44819150	Visit Type
//44819151	Device Type
//44819152	Meas Type
//44819153	Currency
//44819232	Vocabulary
//44819233	Concept Class
//44819234	Cohort Type
//44819235	Relationship
//45756746	ABMS
else
  exit('');
end;
end;

function getUriOrError(key : integer) : String;
begin
  result := getUri(key);
  if result = '' then
    raise ETerminologyError.create('Unmapped OMOP Vocabulary id: '+inttostr(key));
end;

procedure TOMOPServices.extendLookup(opContext : TTxOperationContext; factory: TFHIRFactory; ctxt: TCodeSystemProviderContext; langList : THTTPLanguageList; props: TArray<String>; resp: TFHIRLookupOpResponseW);
var
  conn : TFDBConnection;
  st : TStringList;
begin
  if hasProp(props, 'domain-id', true) then
    resp.addProp('domain-id').value := factory.makeCode((ctxt as TOMOPConcept).domain);
  if hasProp(props, 'concept-class-id', true) then
    resp.addProp('concept-class-id').value := factory.makeCode((ctxt as TOMOPConcept).domain);
  if hasProp(props, 'standard-concept', true) then
    resp.addProp('standard-concept').value := factory.makeCode((ctxt as TOMOPConcept).standard);
  if hasProp(props, 'vocabulary-id', true) then
    resp.addProp('vocabulary-id').value := factory.makeCode((ctxt as TOMOPConcept).vocabulary);
  conn := db.GetConnection('lookup');
  try
    conn.sql := 'Select concept_synonym_name, concept_name from ConceptSynonyms, Concepts where ConceptSynonyms.language_concept_id = Concepts.concept_id and ConceptSynonyms.concept_id = '''+SQLWrapString((ctxt as TOMOPConcept).code)+'''';
   conn.Prepare;
    conn.Execute;
    while conn.FetchNext do    
      resp.addDesignation(getLang(conn.ColStringByName['concept_name']), '', '', '', conn.ColStringByName['concept_synonym_name']);
    conn.terminate;
    conn.sql := 'Select * from Concepts where concept_id = '''+SQLWrapString((ctxt as TOMOPConcept).code)+'''';
    conn.Prepare;
    conn.Execute;
    if conn.FetchNext then
    begin
      if hasProp(props, 'concept-class-concept-id', true) then
        resp.addProp('concept-class-concept-id').value := factory.makeCode(conn.ColStringByName['concept_class_id']);
      if hasProp(props, 'domain-concept-id', true) then
        resp.addProp('domain-concept-id').value := factory.makeCode(conn.ColStringByName['domain_id']);
      if hasProp(props, 'valid-start-date', true) and not conn.ColNullByName['valid_start_date'] then
        resp.addProp('valid-start-date').value := factory.makeDate(TFslDateTime.fromHL7(conn.ColStringByName['valid_start_date']));
      if hasProp(props, 'valid-end-date', true) and not conn.ColNullByName['valid_end_date'] then
        resp.addProp('valid-end-date').value := factory.makeDate(TFslDateTime.fromHL7(conn.ColStringByName['valid_end_date']));
      if hasProp(props, 'source-concept-code', true) and not conn.ColNullByName['concept_code'] and (getUri(conn.ColIntegerByName['vocabulary_id']) <> '') then
        resp.addProp('source-concept-code').value := factory.makeCoding(getUriOrError(conn.ColIntegerByName['vocabulary_id']), conn.ColStringByName['concept_code']);
      if hasProp(props, 'vocabulary-concept-id', true) then
        resp.addProp('vocabulary-concept-id').value := factory.makeCode(conn.ColStringByName['vocabulary_id']);
      if hasProp(props, 'invalid-reason', true) and not conn.ColNullByName['invalid_reason'] then
        resp.addProp('invalid-reason').value := factory.makeCode(conn.ColStringByName['invalid_reason']);
    end;
    conn.terminate;
    st := TStringList.create;
    try
      conn.sql := 'Select Concepts.concept_id, Concepts.concept_name, Relationships.relationship_id '+
         'from Concepts, ConceptRelationships, Relationships where '+
          'ConceptRelationships.relationship_id = Relationships.relationship_concept_id '+
          'and ConceptRelationships.concept_id_2 = Concepts.concept_id '+
          'and ConceptRelationships.concept_id_1 = '''+SQLWrapString((ctxt as TOMOPConcept).code)+'''';
      conn.prepare;
      conn.execute;
      while conn.fetchNext do
      begin
        st.add(conn.ColStringByName['concept_id']);
        if hasProp(props, conn.ColStringByName['relationship_id'], true) then
          resp.addProp(conn.ColStringByName['relationship_id']).value := factory.makeCoding(systemUri, conn.ColStringByName['concept_id'], conn.ColStringByName['concept_name']);
      end;
      conn.terminate;

      conn.sql := 'Select Concepts.concept_id, Concepts.concept_name, Relationships.reverse_relationship_id '+
         'from Concepts, ConceptRelationships, Relationships where '+
          'ConceptRelationships.relationship_id = Relationships.relationship_concept_id '+
          'and ConceptRelationships.concept_id_1 = Concepts.concept_id '+
          'and ConceptRelationships.concept_id_2 = '''+SQLWrapString((ctxt as TOMOPConcept).code)+'''';
      conn.prepare;
      conn.execute;
      while conn.fetchNext do
      begin
        if st.IndexOf(conn.ColStringByName['concept_id']) = -1 then
          if hasProp(props, conn.ColStringByName['reverse_relationship_id'], true) then
            resp.addProp(conn.ColStringByName['reverse_relationship_id']).value := factory.makeCoding(systemUri, conn.ColStringByName['concept_id'], conn.ColStringByName['concept_name']);
      end;
      conn.terminate;
    finally
      st.free;
    end;

    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
      raise
    end;
  end;
end;

function TOMOPServices.buildValueSet(factory: TFHIRFactory; id: String): TFhirValueSetW;
var
  domain : String;
  msg : String;
  inc : TFhirValueSetComposeIncludeW;
  filt :  TFhirValueSetComposeIncludeFilterW;
  conn : TFDBConnection;
begin
  result := nil;
  domain := id.subString(44);
  conn := db.GetConnection('locate');
  try
    conn.sql := 'Select concept_id, concept_name, Domains.domain_id from Concepts, Domains where Domains.domain_id = '''+SQLWrapString(domain)+''' and Domains.domain_concept_id = Concepts.concept_id';
    conn.Prepare;
    conn.Execute;
    if conn.FetchNext and (conn.ColStringByName['domain_id'] = domain) then
    begin
      result := factory.wrapValueSet(factory.makeByName('ValueSet') as TFHIRResourceV);
      try
        result.url := id;
        result.status := psActive;
        result.version := version;
        result.name := 'OMOPDomain'+domain;
        result.description := 'OMOP value set for domain '+conn.ColStringByName['concept_name'];
        result.date := TFslDateTime.makeUTC;
        result.experimental := false;
        inc := result.addInclude;
        try
          inc.systemUri := systemUri;
          filt := inc.addFilter;
          try
            filt.prop := 'domain';
            filt.op := foEqual;
            filt.value := domain;
          finally
            filt.free;
          end;
        finally
          inc.free;
        end;
        result.link;
      finally
        result.free;
      end;
    end
    else    
      raise ETerminologyError.create('Unknown Value Domain '+id);
    conn.terminate;
    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
      raise
    end;
  end;
end;

procedure TOMOPServices.getTranslations(coding: TFHIRCodingW; target: String; codes: TFslList<TCodeTranslation>);
var
  vid : integer; 
  conn : TFDBConnection;
  t : TCodeTranslation;
begin
  vid := getVocabId(target);
  if (vid > -1) then
  begin
    conn := db.GetConnection('getTranslations');
    try
      conn.sql := 'Select concept_code, concept_name from Concepts where concept_id = '''+coding.code+''' and vocabulary_id = '+inttostr(vid);
      conn.Prepare;
      conn.Execute;
      while conn.FetchNext do
      begin
        t := TCodeTranslation.create;
        try
          t.uri := target;
          t.code := conn.ColStringByName['concept_code'];
          t.display := conn.ColStringByName['concept_name'];
          t.equivalence := cmeEquivalent;
          t.map := systemUri+'/ConceptMap/to-'+inttostr(vid)+'|'+version;
          codes.add(t.link);
        finally
          t.free;
        end;
      end;
      conn.terminate;
      conn.Release;
    except
      on e : Exception do
      begin
        conn.Error(e);
        raise
      end;
    end;

  end;
end;

function TOMOPServices.makeCM(id, src, tgt : String; factory : TFHIRFactory) : TFHIRConceptMapW;
var
  g : TFhirConceptMapGroupW;
begin
  result := factory.wrapConceptMap(factory.makeResource('ConceptMap'));
  result.id := id;
  result.url := systemUri+'/ConceptMap/'+id;
  result.addGroup(src,tgt).free;
end;

procedure TOMOPServices.registerConceptMaps(list: TFslList<TFHIRConceptMapW>; factory : TFHIRFactory);
var
  conn : TFDBConnection;
  key : integer;
  uri : String;
begin                         
  conn := db.GetConnection('registerConceptMaps');
  try
    conn.sql := 'Select DISTINCT vocabulary_id from Concepts';
    conn.Prepare;
    conn.Execute;
    while conn.FetchNext do
    begin
      key := conn.ColIntegerByName['vocabulary_id'];
      uri := getUri(key);
      if (uri <> '') then
      begin
        list.add(makeCM('to-'+inttostr(key), systemUri, uri, factory));
        list.add(makeCM('from-'+inttostr(key), uri, systemUri, factory));
      end;
    end;
    conn.terminate;
    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
      raise
    end;
  end;
end;

procedure TOMOPServices.defineFeatures(opContext : TTxOperationContext; features: TFslList<TFHIRFeature>);
begin
  raise ETerminologyError.Create('not done yet: defineFeatures', itBusinessRule);
end;

end.

