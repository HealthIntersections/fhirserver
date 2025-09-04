unit tx_ndc;

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
  SysUtils, Classes, Generics.Collections, {$IFDEF DELPHI} IOUtils, {$ENDIF}
  fsl_base, fsl_utilities, fsl_collections, fsl_stream, fsl_http, fsl_threads, fsl_lang, fsl_fpc, fsl_json, fsl_logging, fsl_i18n,
  fdb_manager, fdb_dialects,
  fhir_objects, fhir_common, fhir_factory, fhir_utilities, fhir_features, fhir_uris,
  fhir_cdshooks,
  ftx_service;

const
  LOOP_STEP_COUNT = 500;

type
  { TNdcObject }

  TNdcObject = class (TFslObject)
  private
    Factive: boolean;
    FCode: String;
    FDateSeen : String;
    function GetKey: String;
  public
    property dateSeen : String read FDateSeen write FDateSeen;
    property code : String read FCode write FCode;
    property key : String read GetKey;
    property active : boolean read Factive write FActive;
  end;

  { TNdcPackage }

  TNdcPackage = class (TNdcObject)
  private
    FCode11: String;
    FDescription: String;
    FProductCode: String;
  public
    function link : TNdcPackage; overload;

    property code11 : String read FCode11 write Fcode11;
    property productCode : String read FProductCode write FProductCode;
    property description : String read FDescription write FDescription;
  end;

  { TNdcProduct }

  TNdcProduct = class (TNdcObject)
  private
    FDosageFormName: String;
    FEndMarketingDate: TFslDateTime;
    FLabelerName: String;
    FMarketingCategoryName: String;
    FNonProprietaryName: String;
    FProductTypeName: String;
    FProprietaryName: String;
    FProprietaryNameSuffix: String;
    FRouteName: String;
    FStartMarketingDate: TFslDateTime;
  public
    function link : TNdcProduct; overload;

    property productTypeName : String read FProductTypeName write FProductTypeName;
    property proprietaryName : String read FProprietaryName write FProprietaryName;
    property proprietaryNameSuffix : String read FProprietaryNameSuffix write FProprietaryNameSuffix;
    property nonProprietaryName : String read FNonProprietaryName write FNonProprietaryName;
    property dosageFormName : String read FDosageFormName write FDosageFormName;
    property routeName : String read FRouteName write FRouteName;
    property startMarketingDate : TFslDateTime read FStartMarketingDate write FStartMarketingDate;
    property endMarketingDate : TFslDateTime read FEndMarketingDate write FEndMarketingDate;
    property marketingCategoryName : String read FMarketingCategoryName write FMarketingCategoryName;
    property labelerName : String read FLabelerName write FLabelerName;
  end;

  { TNdcImporter }

  TNdcImporter = class (TFslObject)
  private
    FSource: String;
    FConn : TFDBConnection;
    FCodes : TDictionary<String, integer>;
    FTypes : TDictionary<String, integer>;
    FOrgs : TDictionary<String, integer>;
    FRoutes : TDictionary<String, integer>;
    FDoseforms : TDictionary<String, integer>;
    FKey : integer;
    FStepBase, FStepSpan : Double;

    FFieldProductNDC : integer;
    FFieldProductTypeName : integer;
    FFieldProprietaryName : integer;
    FFieldProprietaryNameSuffix : integer;
    FFieldNonProprietaryName : integer;
    FFieldDosageFormName : integer;
    FFieldRouteName : integer;
    FFieldStartMarketingDate : integer;
    FFieldEndMarketingDate : integer;
    FFieldMarketingCategoryName : integer;
    FFieldApplicationNumber : integer;
    FFieldLabelerName : integer;
    FFieldSubstanceName : integer;
    FFieldStrengthNumber : integer;
    FFieldStrengthUnit : integer;
    FFieldPharm_Classes : integer;
    FFieldDEASchedule : integer;
    FFieldNDC_Exclude_Flag : integer;
//    FFieldListing_Record_Certified_Through : integer;

    FPckFieldProductNDC : integer;
    FPckFieldNDCPackageCode : integer;
    FPckFieldPackageDescription : integer;
    FPckFieldNDC_Exclude_Flag : integer;

    FProducts : TFslMap<TNdcProduct>;
    FPackages : TFslMap<TNdcPackage>;

    function prog(pct : integer) : integer; overload;
    function prog(i, t : integer) : integer; overload;
    procedure step;

    function findVersions : TArray<String>;
    procedure readPackages(v : String; callback: TWorkProgressEvent);
    procedure readProducts(v : String; callback: TWorkProgressEvent);
    procedure readVersion(v : string; callback: TWorkProgressEvent);
    procedure checkMissingCodes;

    procedure prepareDatabase;
    procedure processVersion(v : String);
    procedure processProduct(p : TNDCProduct);
    procedure processPackage(p : TNDCPackage);
    procedure processProducts(callback: TWorkProgressEvent);
    procedure processPackages(callback: TWorkProgressEvent);
    procedure finishDatabase;
  public
    constructor Create(source : String; conn : TFDBConnection);
    destructor Destroy; override;

    property source : String read FSource write FSource;

    procedure Doinstall(sender : TObject; context: TObject; callback : TWorkProgressEvent);
  end;

  TNDCProviderContext = class (TCodeSystemProviderContext)
  private
    FKey: integer;
    FPackage: boolean;
    FCode: String;
    FDisplay : String;
  public
    constructor Create(package : boolean; key : integer);
    property package : boolean read FPackage write FPackage;
    property key : integer read FKey write FKey;
    property code : String read FCode write FCode;
    property display : String read FDisplay write FDisplay;
  end;

  TNDCIteratorContext = class (TCodeSystemIteratorContext)
  private
    FMore : boolean;
    FProducts : boolean;
    FSecond : boolean;
    FConn : TFDBConnection;
  public
    destructor Destroy; override;
    function more : boolean; override;
  end;

  TNDCFilterPreparationContext = class (TCodeSystemProviderFilterPreparationContext)
  end;

  TNDCFilterContextMode = (fcmCode11, fcmCode10, fcmProduct);

  TNDCFilterContext = class (TCodeSystemProviderFilterContext)
  private
    FMode : TNDCFilterContextMode;
    FConn : TFDBConnection;
  public
    constructor Create(mode : TNDCFilterContextMode; conn : TFDBConnection);
    destructor Destroy; override;
  end;

  { TNDCServices }

  TNDCServices = class (TCodeSystemProvider)
  private
    FDb : TFDBManager;
    FVersion : string;
    FPackageCount : integer;
    FProductCount : integer;

    FTypes : TDictionary<integer, String>;
    FOrgs : TDictionary<integer, String>;
    FRoutes : TDictionary<integer, String>;
    FDoseforms : TDictionary<integer, String>;
    procedure load;
    procedure loadDict(conn: TFDBConnection; dict: TDictionary<integer, String>; sql: String);
    function packageDisplay(conn : TFDBConnection) : String;
    function productDisplay(conn : TFDBConnection) : String;
  public
    constructor Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport; db : TFDBManager; version : String);
    destructor Destroy; Override;
    Function Link : TNDCServices; overload;

    class function checkDB(conn : TFDBConnection) : String;

    function description : String; override;
    function TotalCount : integer;  override;
    function getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    function systemUri : String; override;
    function getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList):String; override;
    function getDefinition(opContext : TTxOperationContext; code : String):String; override;
    function locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(opContext : TTxOperationContext; code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function IsAbstract(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean; override;
    function Code(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;
    function Display(opContext : TTxOperationContext; context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string; override;
    procedure Designations(opContext : TTxOperationContext; context : TCodeSystemProviderContext; list : TConceptDesignations); override;
    function Definition(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;

    function getPrepContext(opContext : TTxOperationContext) : TCodeSystemProviderFilterPreparationContext; override;
    function prepare(opContext : TTxOperationContext; prep : TCodeSystemProviderFilterPreparationContext) : boolean; override;

    function searchFilter(opContext : TTxOperationContext; filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function doesFilter(opContext : TTxOperationContext; prop : String; op : TFhirFilterOperator; value : String) : boolean; override;
    function filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function FilterMore(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function filterSize(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : integer; override;
    function FilterConcept(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(opContext : TTxOperationContext; textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure getCDSInfo(opContext : TTxOperationContext; card : TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display : String); override;
    procedure extendLookup(opContext : TTxOperationContext; factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; langList : THTTPLanguageList; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;

    procedure defineFeatures(opContext : TTxOperationContext; features : TFslList<TFHIRFeature>); override;
  end;

implementation

{ TNdcObject }

function TNdcObject.GetKey: String;
begin
  result := StringPadLeft(code.Replace('-', ''), '0', 11);
end;

{ TNdcPackage }

function TNdcPackage.link: TNdcPackage;
begin
  result := TNdcPackage(inherited link);
end;

{ TNdcProduct }

function TNdcProduct.link: TNdcProduct;
begin
  result := TNdcProduct(inherited link);
end;


{ TNdcImporter }

constructor TNdcImporter.Create(source: String; conn : TFDBConnection);
begin
  inherited Create;
  FSource := source;
  FConn := conn;
  FTypes := TDictionary<String, integer>.Create;
  FCodes := TDictionary<String, integer>.Create;
  FOrgs := TDictionary<String, integer>.Create;
  FRoutes := TDictionary<String, integer>.Create;
  FDoseForms := TDictionary<String, integer>.Create;

  FProducts := TFslMap<TNdcProduct>.Create;
  FPackages := TFslMap<TNdcPackage>.Create;
end;

destructor TNdcImporter.Destroy;
begin
  FProducts.free;
  FPackages.free;

  FConn.free;
  FDoseForms.free;
  FRoutes.free;
  FOrgs.free;
  FTypes.free;
  FCodes.free;
  inherited;
end;

procedure TNdcImporter.Doinstall(sender: TObject; context: TObject; callback: TWorkProgressEvent);
var
  s, v : String;
begin
  // 1. load all the versions
  for s in findVersions do
  begin
    readVersion(s, callback);
    v := s;
  end;
  // checkMissingCodes;

  // 2. do the install
  FOrgs.Clear;
  FTypes.Clear;
  prepareDatabase;
  processVersion(v);
  processProducts(callback);
  Step;
  processPackages(callback);
  Step;
  finishDatabase;
end;

function fixDate(date : string) : String;
begin
  if date.startsWith('2388') then
    result := '2018'+date.Substring(4)
  else if date.startsWith('3030') then
      result := '2020'+date.Substring(4)
  else
    result := date;
end;

function fixEndDate(date : string) : String;
begin
  if date.startsWith('3031') then
    result := '2031'+date.Substring(4)
  else
    result := fixDate(date);
end;

function parseDate(s : String) : TFslDateTime;
var
  d : TArray<string>;
begin
  result := TFslDateTime.makeNull;
  s := fixDate(s);
  if s.contains('/') then
  begin
    d := s.Split(['/']);
    if length(d) = 3 then
      exit(TFslDateTime.makeDay(StrToInt(d[1]), StrToInt(d[0]), StrToInt(d[2])));
  end;
  if s.contains('-') then
  begin
    d := s.Split(['-']);
    if length(d) = 3 then
      exit(TFslDateTime.makeDay(StrToInt(d[1]), StrToInt(d[0]), StrToInt(d[2])));
  end;
  result := TFslDateTime.fromHL7(s);
end;

function genCode11(s : String) : String;
var
  p : TArray<String>;
begin
  p := s.Split(['-']);
  result := StringPadLeft(p[0], '0', 5)+
            StringPadLeft(p[1], '0', 4)+
            StringPadLeft(p[2], '0', 2);
end;

function TNdcImporter.prog(pct: integer): integer;
begin
  result := trunc(FStepBase + FStepSpan * (pct / 100));
end;

function TNdcImporter.prog(i, t: integer): integer;
begin
  result := prog(trunc((i / t) * 100));
end;

procedure TNdcImporter.step;
begin
  FStepBase := FStepBase + FStepSpan;
end;

function TNdcImporter.findVersions : TArray<String>;
var
  ts : TStringList;
  s : String;
begin
  ts := TStringList.Create;
  try
    for s in TDirectory.getDirectories(FSource) do
      ts.add(ExtractFileName(s));
    ts.sort;
    result := ts.ToStringArray;
    FStepBase := 0;
    FStepSpan := 100 / ((ts.count + 1) * 2)
  finally
    ts.free;
  end;
end;

var
  tttt : integer = 0;

function findField(fields : TFslStringList; field, v : String) : integer;
begin
  result := fields.IndexOf(field);
  if (result = -1) and not StringArrayExists(['NDC_EXCLUDE_FLAG'], field) then
    inc(tttt); // nothing - for debugging
end;

procedure TNdcImporter.readProducts(v: String; callback: TWorkProgressEvent);
var
  f : TFslCSVExtractor;
  fields : TFslStringList;
  p : TNdcProduct;
  lc : integer;
begin
  fields := TFslStringList.Create;
  try
    f := TFslCSVExtractor.Create(Path([FSource, v, 'product.txt']), TEncoding.ASCII, false, 8192);
    try
      f.Separator := #9;
      f.IgnoreWhitespace := false;
      f.ConsumeEntries(fields); // headers
      FFieldProductNDC := findField(fields, 'PRODUCTNDC', v);
      FFieldProductTypeName := findField(fields, 'PRODUCTTYPENAME', v);
      FFieldProprietaryName := findField(fields, 'PROPRIETARYNAME', v);
      FFieldProprietaryNameSuffix := findField(fields, 'PROPRIETARYNAMESUFFIX', v);
      FFieldNonProprietaryName := findField(fields, 'NONPROPRIETARYNAME', v);
      FFieldDosageFormName := findField(fields, 'DOSAGEFORMNAME', v);
      FFieldRouteName := findField(fields, 'ROUTENAME', v);
      FFieldStartMarketingDate := findField(fields, 'STARTMARKETINGDATE', v);
      FFieldEndMarketingDate := findField(fields, 'ENDMARKETINGDATE', v);
      FFieldMarketingCategoryName := findField(fields, 'MARKETINGCATEGORYNAME', v);
      FFieldApplicationNumber := findField(fields, 'APPLICATIONNUMBER', v);
      FFieldLabelerName := findField(fields, 'LABELERNAME', v);
      FFieldSubstanceName := findField(fields, 'SUBSTANCENAME', v);
      FFieldStrengthNumber := findField(fields, 'ACTIVE_NUMERATOR_STRENGTH', v);
      FFieldStrengthUnit := findField(fields, 'ACTIVE_INGRED_UNIT', v);
      FFieldPharm_Classes := findField(fields, 'PHARM_CLASSES', v);
      FFieldDEASchedule := findField(fields, 'DEASCHEDULE', v);
      FFieldNDC_Exclude_Flag := findField(fields, 'NDC_EXCLUDE_FLAG', v);
//      FFieldListing_Record_Certified_Through := findField(fields, 'LISTING_RECORD_CERTIFIED_THROUGH', v);

      lc := 0;
      while f.More do
      begin
        inc(lc);
        if (lc mod LOOP_STEP_COUNT = 0) then
          callback(self, prog(f.percent), false, 'Processing Products for '+v+' ('+inttostr(FProducts.Count)+'|'+inttostr(FPackages.Count)+')');
        f.ConsumeEntries(fields);
        if fields.count >= 10 then
        begin
          p := TNdcProduct.Create;
          try
            p.dateSeen := v;
            p.code := fields[FFieldProductNDC];
            if (FFieldNDC_Exclude_Flag > -1) and (fields.Count > FFieldNDC_Exclude_Flag) then
              p.active := fields[FFieldNDC_Exclude_Flag] = 'Y'
            else
              p.active := true;

            p.productTypeName := fields[FFieldProductTypeName];
            p.proprietaryName := fields[FFieldProprietaryName];
            p.proprietaryNameSuffix := fields[FFieldProprietaryNameSuffix];
            p.nonProprietaryName := fields[FFieldNonProprietaryName];
            p.dosageFormName := fields[FFieldDosageFormName];
            p.routeName := fields[FFieldRouteName];
            p.startMarketingDate := parseDate(fields[FFieldStartMarketingDate]);
            p.endMarketingDate := parseDate(fixEndDate(fields[FFieldEndMarketingDate]));
            p.marketingCategoryName := fields[FFieldMarketingCategoryName];
            p.labelerName := fields[FFieldLabelerName];
            FProducts.AddOrSetValue(p.key, p.link);
          finally
            p.free;
          end;
        end;
      end;
    finally
      f.free;
    end;
  finally
    fields.free;
  end;
end;


procedure TNdcImporter.readPackages(v : String; callback: TWorkProgressEvent);
var
  f : TFslCSVExtractor;
  fields : TFslStringList;
  p : TNdcPackage;
  lc : integer;
begin
  fields := TFslStringList.Create;
  try
    f := TFslCSVExtractor.Create(Path([FSource, v, 'package.txt']), TEncoding.ASCII, false, 8192);
    try
      f.Separator := #9;
      f.IgnoreWhitespace := false;
      f.ConsumeEntries(fields); // headers
      FPckFieldProductNDC := findField(fields, 'PRODUCTNDC', v);
      FPckFieldNDCPackageCode := findField(fields, 'NDCPACKAGECODE', v);
      FPckFieldPackageDescription := findField(fields, 'PACKAGEDESCRIPTION', v);
      FPckFieldNDC_Exclude_Flag := findField(fields, 'NDC_EXCLUDE_FLAG', v);

      lc := 0;
      while f.More do
      begin
        inc(lc);
        if (lc mod LOOP_STEP_COUNT = 0) then
          callback(self, prog(f.percent), false, 'Processing Packages for '+v+' ('+inttostr(FProducts.Count)+'|'+inttostr(FPackages.Count)+')');
        f.ConsumeEntries(fields);
        if fields.count >= 4 then
        begin
          p := TNdcPackage.Create;
          try
            p.dateSeen := v;
            p.code := fields[FPckFieldNDCPackageCode];
            if (FPckFieldNDC_Exclude_Flag > -1) and (fields.Count > FPckFieldNDC_Exclude_Flag) then
              p.active := fields[FPckFieldNDC_Exclude_Flag] = 'Y'
            else
              p.active := true;
            p.code11 := fields[FPckFieldNDCPackageCode];
            p.description := fields[FPckFieldPackageDescription];
            p.productCode := fields[FPckFieldProductNDC];
            if (p.code.length <= 12) then
              FPackages.AddOrSetValue(p.Key, p.link);
          finally
            p.free;
          end;
        end;
      end;
    finally
      f.free;
    end;
  finally
    fields.free;
  end;
end;

procedure TNdcImporter.readVersion(v: string; callback: TWorkProgressEvent);
begin
  readPackages(v, callback);
  Step;
  readProducts(v, callback);
  Step;
end;

procedure TNdcImporter.checkMissingCodes;
var
  json, j2 : TJsonObject;
  a, a2 : TJsonArray;
  i, t, tt : integer;
  c : String;
begin
  json := TJsonParser.ParseFile(FilePath([FSource, 'all-ndc-codes.json']));
  try
    j2 := TJsonObject.Create;
    try
      a := json.forceObj['ndcList'].forceArr['ndc'];
      a2 := j2.forceObj['ndcList'].forceArr['ndc'];
      t := 0;
      tt := a.count;
      for i := 0 to a.Count - 1 do
      begin
        c := a.Value[i];
        if (not FProducts.ContainsKey(c)) and (not FPackages.ContainsKey(c)) then
          a2.add(c);
      end;
      BytesToFile(TJSONWriter.WriteObject(j2, false), FilePath([FSource, 'unknown-ndc-codes.json']));
    finally
      j2.free;
    end;
  finally
    json.free;
  end;
end;

procedure TNdcImporter.prepareDatabase;
var
  md : TFDBMetaData;
begin
  md := FConn.FetchMetaData;
  try
    if md.HasTable('NDCProducts') then
      FConn.DropTable('NDCProducts');
    if md.HasTable('NDCPackages') then
      FConn.DropTable('NDCPackages');
    if md.HasTable('NDCProductTypes') then
      FConn.DropTable('NDCProductTypes');
    if md.HasTable('NDCOrganizations') then
      FConn.DropTable('NDCOrganizations');
    if md.HasTable('NDCDoseForms') then
      FConn.DropTable('NDCDoseForms');
    if md.HasTable('NDCRoutes') then
      FConn.DropTable('NDCRoutes');
    FConn.ExecSQL('CREATE TABLE NDCProducts ('+
        'NDCKey int NOT NULL, '+
        'Code nchar(11) NOT NULL, '+
        'LastSeen '+DBDateTimeType(FConn.Owner.Platform)+' NULL, '+
        'Active int NOT NULL, '+
        'Type int NOT NULL, '+
        'TradeName nchar(255) NOT NULL, '+
        'Suffix nchar(180) NOT NULL, '+
//        'Generic nchar(80) NOT NULL, '+
        'DoseForm int NOT NULL, '+
        'Route int NOT NULL, '+
        'StartDate '+DBDateTimeType(FConn.Owner.Platform)+' NULL, '+
        'EndDate '+DBDateTimeType(FConn.Owner.Platform)+' NULL, '+
        'Category nchar(40) NOT NULL, '+
        'Company int NOT NULL, '+
        'Generics '+DBBlobType(FConn.Owner.Platform)+' NULL, '+
        'CONSTRAINT PK_NDCProducts PRIMARY KEY (NDCKey ASC))');
    FConn.ExecSQL('CREATE UNIQUE INDEX [NDCProductsCode] ON NDCProducts ( Code ASC )');
    FConn.ExecSQL('CREATE TABLE NDCPackages ('+
        'NDCKey int NOT NULL, '+
        'ProductKey int NOT NULL, '+
        'Code nchar(12) NOT NULL, '+
        'Code11 nchar(11) NOT NULL, '+
        'LastSeen '+DBDateTimeType(FConn.Owner.Platform)+' NULL, '+
        'Active int NOT NULL, '+
        'Description nchar(255) NOT NULL, '+
        'CONSTRAINT PK_NDCPackages PRIMARY KEY (NDCKey ASC))');
    FConn.ExecSQL('CREATE UNIQUE INDEX [NDCPackagesCode] ON NDCPackages ( Code ASC )');
    FConn.ExecSQL('CREATE INDEX NDCPackagesProductCode ON NDCPackages (ProductKey ASC, Code ASC )');
    FConn.ExecSQL('CREATE INDEX NDCPackagesProductCode11 ON NDCPackages (ProductKey ASC, Code11 ASC )');
    FConn.ExecSQL('CREATE TABLE NDCVersion (Version String NOT NULL)');
  finally
    md.free;
  end;
end;


procedure TNdcImporter.finishDatabase;
var
  s : String;
  l : integer;
begin
  l := 0;
  for s in FTypes.Keys do
    l := IntegerMax(l, s.Length);
  FConn.ExecSQL('CREATE TABLE NDCProductTypes (NDCKey int NOT NULL, Name nchar('+inttostr(l)+') NOT NULL, CONSTRAINT PK_NDCProductTypes PRIMARY KEY (NDCKey ASC))');
  FConn.ExecSQL('delete from NDCProductTypes');
  for s in FTypes.Keys do
    FConn.ExecSQL('Insert into NDCProductTypes (NDCKey, Name) values ('+inttostr(FTypes[s])+', '''+SQLWrapString(s)+''')');

  for s in FOrgs.Keys do
    l := IntegerMax(l, s.Length);
  FConn.ExecSQL('CREATE TABLE NDCOrganizations (NDCKey int NOT NULL, Name nchar('+inttostr(l)+') NOT NULL, CONSTRAINT PK_NDCOrganizations PRIMARY KEY (NDCKey ASC))');
  FConn.ExecSQL('delete from NDCOrganizations');
  for s in FOrgs.Keys do
    FConn.ExecSQL('Insert into NDCOrganizations (NDCKey, Name) values ('+inttostr(FOrgs[s])+', '''+SQLWrapString(s)+''')');

  for s in FDoseforms.Keys do
    l := IntegerMax(l, s.Length);
  FConn.ExecSQL('CREATE TABLE NDCDoseForms (NDCKey int NOT NULL, Name nchar('+inttostr(l)+') NOT NULL, CONSTRAINT PK_NDCDoseForms PRIMARY KEY (NDCKey ASC))');
  FConn.ExecSQL('delete from NDCDoseForms');
  for s in FDoseforms.Keys do
    FConn.ExecSQL('Insert into NDCDoseForms (NDCKey, Name) values ('+inttostr(FDoseforms[s])+', '''+SQLWrapString(s)+''')');

  for s in FRoutes.Keys do
    l := IntegerMax(l, s.Length);
  FConn.ExecSQL('CREATE TABLE NDCRoutes (NDCKey int NOT NULL, Name nchar('+inttostr(l)+') NOT NULL, CONSTRAINT PK_NDCRoutes PRIMARY KEY (NDCKey ASC))');
  FConn.ExecSQL('delete from NDCRoutes');
  for s in FRoutes.Keys do
    FConn.ExecSQL('Insert into NDCRoutes (NDCKey, Name) values ('+inttostr(FRoutes[s])+', '''+SQLWrapString(s)+''')');
end;

function checkLength(s : String; name : String; len : integer) : String;
begin
  if (s.length > 255) and (len = 255) then
    exit(copy(s, 1, 255));

  if s.Length > len then
    raise EFslException.Create(name+' Too long ('+inttostr(s.Length)+'): '+s);
  result := s;
end;

procedure TNdcImporter.processPackage(p: TNDCPackage);
begin
  if not FCodes.containsKey(p.productCode) then
    exit;
  if FCodes.containsKey(p.code) then
    exit;
  inc(FKey);
  FCodes.add(p.code, FKey);
  FConn.BindInteger('NDCKey', FKey);
  FConn.BindInteger('ProductKey', FCodes[p.productCode]);
  FConn.BindString('Code', checklength(p.code, 'PACK_FIELD_NDCPackageCode', 12));
  FConn.BindString('Code11', genCode11(p.code));
  FConn.BindString('Description', p.description);
  FConn.BindIntegerFromBoolean('Active', p.active);
  FConn.Execute;
end;

procedure TNdcImporter.processProduct(p: TNDCProduct);
begin
  if FCodes.containsKey(p.code) then
    exit;
  inc(FKey);
  FConn.BindInteger('NDCKey', FKey);
  FConn.BindString('Code', checklength(p.code, 'ProductNDC', 11));
  FCodes.Add(p.code, FKey);
  FConn.BindIntegerFromBoolean('Active', p.active);
  if not FTypes.containsKey(p.productTypeName) then
    FTypes.Add(p.productTypeName, FTypes.Count+1);
  FConn.BindInteger('Type', FTypes[p.productTypeName]);
  FConn.BindString('TradeName', checklength(p.ProprietaryName, 'ProprietaryName', 255));
  FConn.BindString('Suffix', checklength(p.ProprietaryNameSuffix, 'ProprietaryNameSuffix', 180));
  FConn.BindBlobFromString('Generics', p.NonProprietaryName);
  if not FDoseforms.containsKey(p.DosageFormName) then
    FDoseforms.Add(p.DosageFormName, FDoseforms.Count+1);
  FConn.BindInteger('DoseForm', FDoseforms[p.DosageFormName]);
  if not FRoutes.containsKey(p.RouteName) then
    FRoutes.Add(p.RouteName, FRoutes.Count+1);
  FConn.BindInteger('Route', FRoutes[p.RouteName]);
  FConn.BindDateTimeEx('StartDate', p.StartMarketingDate);
  FConn.BindDateTimeEx('EndDate', p.EndMarketingDate);
  FConn.BindString('Category', checklength(p.MarketingCategoryName, 'MarketingCategoryName', 40));
  if not FOrgs.containsKey(p.LabelerName) then
    FOrgs.Add(p.LabelerName, FOrgs.Count+1);
  FConn.BindInteger('Company', FOrgs[p.LabelerName]);
  FConn.Execute;
end;


procedure TNdcImporter.processPackages(callback: TWorkProgressEvent);
var
  i, lc : integer;
  p : TNdcPackage;
begin
  if FConn.Owner.Platform = kdbSQLite then
    FConn.StartTransact;
  try
    FConn.SQL := 'Insert into NDCPackages (NDCKey, Code, Code11, ProductKey, Active, Description) values (' +
                 ':NDCKey, :Code, :Code11, :ProductKey, :Active, :Description)';
    FConn.prepare;
    FKey := 0;
    i := 0;
    lc := 0;
    for p in FPackages.values do
    begin
      inc(lc);
      if (lc mod LOOP_STEP_COUNT = 0) then
        callback(self,  prog(i, FPackages.Count), false, 'Processing Packages');
      processPackage(p);
      inc(i);
    end;
    FConn.Terminate;
  finally
    if FConn.Owner.Platform = kdbSQLite then
      FConn.commit;
  end;
  callback(self, prog(100), true, 'Finished Processing');
end;

procedure TNdcImporter.processVersion(v : String);
begin
  FConn.ExecSQL('Insert into NDCVersion (Version) values ('''+SQLWrapString(v)+''')');
end;

procedure TNdcImporter.processProducts(callback: TWorkProgressEvent);
var
  i, lc : integer;
  p : TNdcProduct;
begin
  if FConn.Owner.Platform = kdbSQLite then
    FConn.StartTransact;
  try
    FConn.SQL := 'Insert into NDCProducts (NDCKey, Code, Active, Type, TradeName, Suffix, DoseForm, Route, StartDate, EndDate, Category, Company, Generics) values (' +
                 ':NDCKey, :Code, :Active, :Type, :TradeName, :Suffix, :DoseForm, :Route, :StartDate, :EndDate, :Category, :Company, :Generics)';
    FConn.prepare;
    FKey := 0;
    i := 0;
    lc := 0;
    for p in FProducts.values do
    begin
      inc(lc);
      if (lc mod LOOP_STEP_COUNT = 0) then
        callback(self, prog(i, FProducts.Count), false, 'Processing Products');
      processProduct(p);
      inc(i);
    end;
    FConn.Terminate;
  finally
    if FConn.Owner.Platform = kdbSQLite then
      FConn.commit;
  end;
end;

{ TNDCServices }

constructor TNDCServices.Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport; db: TFDBManager; version : String);
begin
  inherited Create(languages, i18n);

  self.FDb := db;
  self.FVersion := version;
  FTypes := TDictionary<integer, String>.Create;
  FOrgs := TDictionary<integer, String>.Create;
  FRoutes := TDictionary<integer, String>.Create;
  FDoseforms := TDictionary<integer, String>.Create;
  load();
end;

procedure TNDCServices.defineFeatures(opContext : TTxOperationContext; features: TFslList<TFHIRFeature>);
begin

end;

destructor TNDCServices.Destroy;
begin
  FDb.free;
  FTypes.free;
  FOrgs.free;
  FRoutes.free;
  FDoseforms.free;
  inherited;
end;

function TNDCServices.Link: TNDCServices;
begin
  result := TNDCServices(inherited link);
end;

class function TNDCServices.checkDB(conn: TFDBConnection): String;
var
  meta : TFDBMetaData;
begin
  meta := conn.FetchMetaData;
  try
    if not meta.HasTable('NDCPackages') or not meta.HasTable('NDCProducts') or not meta.HasTable('NDCProductTypes') or not meta.HasTable('NDCOrganizations') or not meta.HasTable('NDCDoseForms') or not meta.HasTable('NDCRoutes') then
      result := 'Missing Tables - needs re-importing'
    else
      result := 'OK ('+inttostr(conn.countSql('Select count(*) from NDCProducts'))+' products, '+inttostr(conn.countSql('Select count(*) from NDCPackages'))+' packages)';
  finally
    meta.free;
  end;
end;

procedure TNDCServices.loadDict(conn : TFDBConnection; dict : TDictionary<integer, String>; sql : String);
begin
  conn.SQL := sql;
  conn.Prepare;
  conn.Execute;
  while conn.FetchNext do
    dict.Add(conn.ColInteger[1], conn.ColString[2]);
  conn.Terminate;
end;

procedure TNDCServices.load;
var
  conn : TFDBConnection;
begin
  conn := FDB.getConnection('load');
  try
    Logging.log('Load NDC metadata');
    loadDict(conn, FTypes, 'select NDCKey, Name from NDCProductTypes');
    loadDict(conn, FOrgs, 'select NDCKey, Name from NDCOrganizations');
    loadDict(conn, FRoutes, 'select NDCKey, Name from NDCRoutes');
    loadDict(conn, FDoseforms, 'select NDCKey, Name from NDCDoseForms');
    Logging.log('Load NDC counts');
    FPackageCount := conn.countSql('Select count(NDCKey) from NDCPackages');
    FProductCount := conn.countSql('Select count(NDCKey) from NDCProducts');
    conn.release;
  except
    on e : Exception do
    begin
      conn.error(e);
      raise;
    end;
  end;
end;

function TNDCServices.Code(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
var
  c : string;
  code : TNDCProviderContext;
  conn : TFDBConnection;
begin
  code := context as TNDCProviderContext;
  if code.FCode <> '' then
    result := code.FCode
  else
  begin
    conn := FDB.getConnection('Code');
    try
      if code.package then
        c := conn.Lookup('NDCPackages', 'NDCKey', inttostr(code.key), 'Code', '')
      else
        c := conn.Lookup('NDCProducts', 'NDCKey', inttostr(code.key), 'Code', '');
      conn.release;
    except
      on e : Exception do
      begin
        conn.error(e);
        raise;
      end;
    end;
    result := c;
  end;
end;

function TNDCServices.getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext;
var
  conn : TFDBConnection;
  iter : TNDCIteratorContext;
begin
  if (context = nil) then
  begin
    conn := FDB.getconnection('ChildCount');
    try
      iter := TNDCIteratorContext.Create(nil, conn.CountSQL('select count(*) from NDCProducts') + conn.CountSQL('select count(*) from NDCPackages'));
      try
        iter.FConn := conn;
        iter.FConn.SQL := 'Select NDCPackages.NDCKey, NDCPackages.Code, Code11, TradeName, Suffix, Description from NDCProducts, NDCPackages where NDCProducts.NDCKey = NDCPackages.ProductKey';
        iter.FConn.Prepare;
        iter.FConn.Execute;
        iter.FMore := iter.FConn.FetchNext;
        result := iter.Link;
      finally
        iter.free;
      end;
    except
      on e : Exception do
      begin
        conn.error(e);
        raise;
      end;
    end;
  end
  else
    result := TCodeSystemIteratorContext.Create(nil, 0);
end;

function TNDCServices.getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext;
var
  iter : TNDCIteratorContext;
  ctxt : TNDCProviderContext;
begin
  iter := context as TNDCIteratorContext;
  if iter.FProducts then
  begin
    ctxt := TNDCProviderContext.Create(false, iter.FConn.GetColIntegerByName('NDCKey'));
    try
      ctxt.FDisplay := productDisplay(iter.FConn);
      ctxt.code := iter.FConn.GetColStringByName('Code');
      iter.FMore := iter.FConn.FetchNext;
      result := ctxt.link;
    finally
      ctxt.free;
    end;
  end
  else
  begin
    ctxt := TNDCProviderContext.Create(true, iter.FConn.GetColIntegerByName('NDCKey'));
    try
      ctxt.FDisplay := packageDisplay(iter.FConn);
      if iter.FSecond then
      begin
        ctxt.code := iter.FConn.GetColStringByName('Code11');
        iter.FSecond := false;
        iter.FMore := iter.FConn.FetchNext;
        if not iter.FMore then
        begin
          iter.FConn.Terminate;
          iter.FProducts := true;
          iter.FConn.SQL := 'Select NDCKey, Code, Tradename, Suffix from NDCProducts';
          iter.FConn.Prepare;
          iter.FConn.Execute;
          iter.FMore := iter.FConn.FetchNext;
          result := getNextContext(opContext, iter);
          exit;
        end;
      end
      else
      begin
        ctxt.code := iter.FConn.GetColStringByName('Code');
        iter.FSecond := true;
      end;

      result := ctxt.link;
    finally
       ctxt.free;
    end;
  end;
end;

function TNDCServices.Definition(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TNDCServices.description: String;
begin
  result := 'NDC Codes';
end;

function TNDCServices.Display(opContext : TTxOperationContext; context: TCodeSystemProviderContext; langList : THTTPLanguageList): string;
var
  c : string;
  code : TNDCProviderContext;
  conn : TFDBConnection;
begin
  code := context as TNDCProviderContext;
  if (code.FDisplay <> '') then
    result := code.FDisplay
  else
  begin
    c := '';
    conn := FDB.getconnection('Display');
    try
      if code.package then
      begin
        conn.sql := 'Select TradeName, Suffix, Description from NDCProducts, NDCPackages where NDCProducts.NDCKey = NDCPackages.ProductKey and NDCPackages.NDCKey = '+inttostr(code.key);
        conn.prepare;
        conn.execute;
        if conn.fetchnext then
          c := packageDisplay(conn);
        conn.terminate;
      end
      else
      begin
        conn.sql := 'Select TradeName, Suffix from NDCProducts where NDCKey = '+inttostr(code.key);
        conn.prepare;
        conn.execute;
        if conn.fetchnext then
          c := productDisplay(conn);
        conn.terminate;
      end;
      conn.release;
    except
      on e : Exception do
      begin
        conn.error(e);
        raise;
      end;
    end;
    result := c;
  end;
end;

procedure TNDCServices.Designations(opContext : TTxOperationContext; context: TCodeSystemProviderContext; list: TConceptDesignations);
begin
  list.addDesignation(true, true, '', '', Display(opContext, context, nil));
end;

function TNDCServices.doesFilter(opContext : TTxOperationContext; prop: String; op: TFhirFilterOperator; value: String): boolean;
begin
  result := (prop = 'code-type') and (op = foEqual) and StringArrayExistsSensitive(['10-digit', '11-digit', 'product'], value);
end;

procedure TNDCServices.extendLookup(opContext : TTxOperationContext; factory: TFHIRFactory; ctxt: TCodeSystemProviderContext; langList : THTTPLanguageList; props: TArray<String>; resp: TFHIRLookupOpResponseW);
var
  code : TNDCProviderContext;
  conn : TFDBConnection;
begin
  code := ctxt as TNDCProviderContext;
  conn := FDB.getconnection('Display');
  try
    if code.package then
      conn.sql := 'Select [NDCPackages].Code, [NDCProducts].Code as PCode, [NDCPackages].Code11, [NDCPackages].Active, Description, Suffix, Type, TradeName, DoseForm, Route, Company, Category, Generics '+'from NDCProducts, NDCPackages where NDCProducts.NDCKey = NDCPackages.ProductKey and NDCPackages.NDCKey = '+inttostr(code.key)
    else
      conn.sql := 'Select * from NDCProducts where NDCKey = '+inttostr(code.key);
    conn.prepare;
    conn.execute;
    if conn.fetchnext then
    begin
      if not code.package then
      begin
        resp.addProp('code-type').value := factory.makeString('product');
        resp.addProp('description').value := factory.makeString(productDisplay(conn));
      end
      else if (code.code.Contains('-')) then
      begin
        resp.addProp('synonym').value := factory.makeString(conn.ColStringByName['Code11']);
        resp.addProp('code-type').value := factory.makeString('10-digit');
        resp.addProp('description').value := factory.makeString(packageDisplay(conn));
        resp.addProp('product').value := factory.makeString(conn.ColStringByName['PCode']);
      end
      else
      begin
        resp.addProp('synonym').value := factory.makeString(conn.ColStringByName['Code']);
        resp.addProp('code-type').value := factory.makeString('11-digit');
        resp.addProp('description').value := factory.makeString(packageDisplay(conn));
        resp.addProp('product').value := factory.makeString(conn.ColStringByName['PCode']);
      end;
      resp.addProp('type').value := factory.makeString(FTypes[conn.ColIntegerByName['Type']]);
      resp.addProp('active').value := factory.makeString(BooleanToString(conn.ColIntegerByName['Active'] = 1));
      resp.addProp('trade-name').value := factory.makeString(conn.ColStringByName['TradeName']);
      resp.addProp('dose-form').value := factory.makeString(FDoseforms[conn.ColIntegerByName['DoseForm']]);
      resp.addProp('route').value := factory.makeString(FRoutes[conn.ColIntegerByName['Route']]);
      resp.addProp('company').value := factory.makeString(FOrgs[conn.ColIntegerByName['Company']]);
      resp.addProp('category').value := factory.makeString(conn.ColStringByName['Category']);
      resp.addProp('generic').value := factory.makeString(conn.ColBlobAsStringByName['Generics']);
    end;
    conn.terminate;
    conn.release;
  except
    on e : Exception do
    begin
      conn.error(e);
      raise;
    end;
  end;
end;

function TNDCServices.filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop: String; op: TFhirFilterOperator; value: String; prep: TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
var
  ctxt : TNDCFilterPreparationContext;
  res : TNDCFilterContext;
begin
  SetThreadStatus(ClassName+'.filter('+prop+CODES_TFhirFilterOperator[op]+value+')');
  res := nil;
  try
    ctxt := prep as TNDCFilterPreparationContext;
    if (prop = 'code-type') and (op = foEqual) then
    begin
      if value = '10-digit' then
        res := TNDCFilterContext.Create(fcmCode10, FDb.GetConnection('ndc.filter'))
      else if value = '11-digit' then
        res := TNDCFilterContext.Create(fcmCode11, FDb.GetConnection('ndc.filter'))
      else if value = 'product' then
        res := TNDCFilterContext.Create(fcmProduct, FDb.GetConnection('ndc.filter'))
      else
        raise EFslException.Create('The filter "'+prop+' '+CODES_TFhirFilterOperator[op]+' '+value+'" is not understood for NDC');
      if forIteration then
      begin
        if res.FMode = fcmProduct then
          res.FConn.SQL := 'Select * from NDCProducts'
        else
          res.FConn.SQL := 'Select NDCPackages.NDCKey, NDCPackages.Code, Code11, TradeName, Suffix, Description from NDCProducts, NDCPackages where NDCProducts.NDCKey = NDCPackages.ProductKey';
        res.FConn.prepare;
        res.FConn.Execute;
      end;
    end
    else
      raise EFslException.Create('The filter "'+prop+' '+CODES_TFhirFilterOperator[op]+' '+value+'" is not understood for NDC');
    result := res.link;
  finally
    res.free;
  end;
end;

function TNDCServices.FilterConcept(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
var
  context : TNDCFilterContext;
  res : TNDCProviderContext;
begin
  context := ctxt as TNDCFilterContext;
  res := TNDCProviderContext.Create(true, context.FConn.GetColIntegerByName('NDCKey'));
  try
    if context.FMode = fcmProduct then
      res.FDisplay := productDisplay(context.FConn)
    else
      res.FDisplay := packageDisplay(context.FConn);
    if context.FMode = fcmCode11 then
      res.code := context.FConn.GetColStringByName('Code11')
    else
      res.code := context.FConn.GetColStringByName('Code');
    result := res.link;
  finally
    res.free;
  end;
end;

function TNDCServices.filterLocate(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext; code: String; var message: String): TCodeSystemProviderContext;
var
  context : TNDCFilterContext;
  res : TNDCProviderContext;
  const sqlp = 'Select NDCPackages.NDCKey, NDCPackages.Code, Code11, TradeName, Suffix, Description from NDCProducts, NDCPackages where NDCProducts.NDCKey = NDCPackages.ProductKey and ';
begin
  context := ctxt as TNDCFilterContext;
  case context.FMode of
    fcmCode11: context.FConn.sql := sqlp + '(NDCPackages.Code11 = '''+SQLWrapString(code)+''')';
    fcmCode10: context.FConn.sql := sqlp + '(NDCPackages.Code = '''+SQLWrapString(code)+''')';
    fcmProduct: context.FConn.sql := 'Select * from  NDCProducts where Code = '''+SQLWrapString(code)+'''';
  else
    exit(nil);
  end;
  context.FConn.prepare;
  try
    context.FConn.execute;
    if context.FConn.fetchnext then
    begin
      res := TNDCProviderContext.Create(true, context.FConn.GetColIntegerByName('NDCKey'));
      try
        if context.FMode = fcmProduct then
          res.FDisplay := productDisplay(context.FConn)
        else
          res.FDisplay := packageDisplay(context.FConn);
        if context.FMode = fcmCode11 then
          res.code := context.FConn.GetColStringByName('Code11')
        else
          res.code := context.FConn.GetColStringByName('Code');
        result := res.link;
      finally
        res.free;
      end;
    end
    else
      result := nil;
  finally
    context.FConn.Terminate;
  end;
end;

function TNDCServices.FilterMore(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): boolean;
var
  context : TNDCFilterContext;
begin
  context := ctxt as TNDCFilterContext;
  result := context.FConn.FetchNext;
end;

function TNDCServices.filterSize(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): integer;
var
  context : TNDCFilterContext;
begin
  context := ctxt as TNDCFilterContext;
  result := context.FConn.RowsAffected; // todo
end;

procedure TNDCServices.getCDSInfo(opContext : TTxOperationContext; card: TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display: String);
begin
  raise ETerminologyTodo.Create('Not done yet: TNDCServices.getCDSInfo');
end;

function TNDCServices.getDefinition(opContext : TTxOperationContext; code: String): String;
begin
  result := getDisplay(opContext, code, nil);
end;

function TNDCServices.getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList): String;
var
  c : string;
  conn : TFDBConnection;
begin
  c := '';
  conn := FDB.getconnection('getDisplay');
  try
    conn.sql := 'Select TradeName, Suffix from NDCProducts where Code = '''+SQLWrapString(code)+'''';
    conn.prepare;
    conn.execute;
    if conn.fetchnext then
      c := productDisplay(conn)
    else
    begin
      conn.terminate;
      conn.sql := 'Select TradeName, Suffix, Description from NDCProducts, NDCPackages where NDCProducts.NDCKey = NDCPackages.ProductKey and (NDCPackages.Code = '''+SQLWrapString(code)+''' or NDCPackages.Code11 = '''+SQLWrapString(code)+''')';
      conn.prepare;
      conn.execute;
      if conn.fetchnext then
        c := packageDisplay(conn)
    end;
    conn.terminate;
    conn.release;
  except
    on e : Exception do
    begin
      conn.error(e);
      raise;
    end;
  end;
  result := c;
end;

function TNDCServices.getPrepContext(opContext : TTxOperationContext): TCodeSystemProviderFilterPreparationContext;
begin
  result := TNDCFilterPreparationContext.Create;
end;

function TNDCServices.InFilter(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext; concept: TCodeSystemProviderContext): Boolean;
var
  context : TNDCFilterContext;
  res : TNDCProviderContext;
  cc : TNDCProviderContext;
begin
  context := ctxt as TNDCFilterContext;
  cc := concept as TNDCProviderContext;
  case context.FMode of
    fcmCode11: result := context.FConn.CountSQL('Select * from NDCPackages where Code11 = '''+SQLWrapString(cc.FCode)) > 0;
    fcmCode10: result := context.FConn.CountSQL('Select * from NDCPackages where Code = '''+SQLWrapString(cc.FCode)) > 0;
    fcmProduct: result := context.FConn.CountSQL('Select * from NDCProducts where Code = '''+SQLWrapString(cc.FCode)) > 0;
  else
    result := false;
  end;
end;

function TNDCServices.IsAbstract(opContext : TTxOperationContext; context: TCodeSystemProviderContext): boolean;
begin
  result := false;
end;

function TNDCServices.isNotClosed(opContext : TTxOperationContext; textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TNDCServices.locate(opContext : TTxOperationContext; code: String; altOpt : TAlternateCodeOptions; var message: String): TCodeSystemProviderContext;
var
  c : TNDCProviderContext;
  k : integer;
  conn : TFDBConnection;
begin
  c := nil;
  try
    conn := FDB.getconnection('locate');
    try
      if code.contains('-') then
        k := conn.CountSQL('Select NDCKey from NDCPackages where code = '''+SQLWrapString(code)+'''')
      else
        k := conn.CountSQL('Select NDCKey from NDCPackages where code11 = '''+SQLWrapString(code)+'''');
      if (k <> 0) then
        c := TNDCProviderContext.Create(true, k)
      else
      begin
        k := conn.CountSQL('Select NDCKey from NDCProducts where code = '''+SQLWrapString(code)+'''');
        if k <> 0 then
          c := TNDCProviderContext.Create(false, k)
      end;
      if c = nil then
        message := 'Code "'+code+'" not found in NDC'
      else
        c.code := code;
      result := c.link;
      conn.release;
    except
      on e : Exception do
      begin
        conn.error(e);
        raise;
      end;
    end;
  finally
    c.free;
  end;
end;

function TNDCServices.locateIsA(opContext : TTxOperationContext; code, parent: String; disallowParent : boolean = false): TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.Create('Not done yet: TNDCServices.locateIsA');
end;

function TNDCServices.packageDisplay(conn: TFDBConnection): String;
begin
  if conn.ColStringByName['Suffix'] <> '' then
    result := StringNormalizeWhitespace(conn.ColStringByName['TradeName']+' '+conn.ColStringByName['Suffix']+', '+conn.ColStringByName['Description']+' (package)')
  else
    result := StringNormalizeWhitespace(conn.ColStringByName['TradeName']+', '+conn.ColStringByName['Description']+' (package)');
end;

function TNDCServices.productDisplay(conn: TFDBConnection): String;
begin
  result := conn.ColStringByName['TradeName']+' '+conn.ColStringByName['Suffix']+' (product)';
end;

function TNDCServices.prepare(opContext : TTxOperationContext; prep: TCodeSystemProviderFilterPreparationContext): boolean;
var
  ctxt : TNDCFilterPreparationContext;
begin
  ctxt := prep as TNDCFilterPreparationContext;
  result := false;
end;

function TNDCServices.searchFilter(opContext : TTxOperationContext; filter: TSearchFilterText; prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.Create('Not done yet: TNDCServices.searchFilter');
end;

function TNDCServices.systemUri: String;
begin
  result := URI_NDC;
end;

function TNDCServices.TotalCount: integer;
begin
  result := FProductCount + FPackageCount;
end;

{ TNDCProviderContext }

constructor TNDCProviderContext.Create(package: boolean; key: integer);
begin
  inherited Create;
  self.package := package;
  self.key := key;
end;

{ TNDCIteratorContext }

destructor TNDCIteratorContext.Destroy;
begin
  FConn.Terminate;
  FConn.Release;
  inherited;
end;

function TNDCIteratorContext.more: boolean;
begin
  result := FMore;

end;

{ TNDCFilterContext }

constructor TNDCFilterContext.Create(mode: TNDCFilterContextMode; conn: TFDBConnection);
begin
  inherited Create;
  FMode := mode;
  FConn := conn;
end;

destructor TNDCFilterContext.Destroy;
begin
  if FConn.Prepared then
    FConn.Terminate;
  FConn.Release;
  inherited;
end;

end.
