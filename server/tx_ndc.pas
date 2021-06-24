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
  SysUtils, Classes, Generics.Collections,
  fsl_base, fsl_utilities, fsl_collections, fsl_stream, fsl_http, fsl_threads, fsl_lang,
  fdb_manager, fdb_dialects,
  fhir_objects, fhir_common, fhir_factory, fhir_utilities, fhir_features,
  fhir_cdshooks,
  ftx_service;

type

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
    procedure importProducts(callback: TWorkProgressEvent);
    procedure importPackages(callback: TWorkProgressEvent);
    procedure processProduct(fields: TFslStringList);
    procedure processPackage(fields: TFslStringList);
    procedure prepareDatabase;
    procedure finishDatabase;
  public
    constructor Create(source : String; conn : TFDBConnection);
    destructor Destroy; override;

    property source : String read FSource write FSource;

    procedure process(callback : TInstallerCallback);
    procedure Doinstall(sender : TObject; callback : TWorkProgressEvent);
  end;

  TNDCProviderContext = class (TCodeSystemProviderContext)
  private
    FKey: integer;
    FPackage: boolean;
    FCode: String;
    FDisplay : String;
  public
    constructor create(package : boolean; key : integer);
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
    constructor Create(languages : TIETFLanguageDefinitions; db : TFDBManager; version : String);
    destructor Destroy; Override;
    Function Link : TNDCServices; overload;

    class function checkDB(conn : TFDBConnection) : String;

    function description : String; override;
    function TotalCount : integer;  override;
    function getIterator(context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    function systemUri(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; const lang : THTTPLanguages):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string; override;
    procedure Displays(context : TCodeSystemProviderContext; list : TCodeDisplays); override;
    function Definition(context : TCodeSystemProviderContext) : string; override;

    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
    function prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean; override;

    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function doesFilter(prop : String; op : TFhirFilterOperator; value : String) : boolean; override;
    function filter(forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure getCDSInfo(card : TCDSHookCard; const lang : THTTPLanguages; baseURL, code, display : String); override;
    procedure extendLookup(factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; const lang : THTTPLanguages; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
    procedure defineFeatures(features : TFslList<TFHIRFeature>); override;
  end;

implementation

const
  PROD_FIELD_ProductID = 0;
  PROD_FIELD_ProductNDC = 1;
  PROD_FIELD_ProductTypeName = 2;
  PROD_FIELD_ProprietaryName = 3;
  PROD_FIELD_ProprietaryNameSuffix = 4;
  PROD_FIELD_NonProprietaryName = 5;
  PROD_FIELD_DosageFormName = 6;
  PROD_FIELD_RouteName = 7;
  PROD_FIELD_StartMarketingDate = 8;
  PROD_FIELD_EndMarketingDate = 9;
  PROD_FIELD_MarketingCategoryName = 10;
  PROD_FIELD_ApplicationNumber = 11;
  PROD_FIELD_LabelerName = 12;
  PROD_FIELD_SubstanceName = 13;
  PROD_FIELD_StrengthNumber = 14;
  PROD_FIELD_StrengthUnit = 15;
  PROD_FIELD_Pharm_Classes = 16;
  PROD_FIELD_DEASchedule = 17;
  PROD_FIELD_NDC_Exclude_Flag = 18;
  PROD_FIELD_Listing_Record_Certified_Through = 19;

  PACK_FIELD_ProductID = 0;
  PACK_FIELD_ProductNDC = 1;
  PACK_FIELD_NDCPackageCode = 2;
  PACK_FIELD_PackageDescription = 3;
  PACK_FIELD_NDC_Exclude_Flag = 4;

{ TNdcImporter }

constructor TNdcImporter.Create(source: String; conn : TFDBConnection);
begin
  inherited create;
  FSource := source;
  FConn := conn;
  FTypes := TDictionary<String, integer>.create;
  FCodes := TDictionary<String, integer>.create;
  FOrgs := TDictionary<String, integer>.create;
  FRoutes := TDictionary<String, integer>.create;
  FDoseForms := TDictionary<String, integer>.create;
end;

destructor TNdcImporter.Destroy;
begin
  FConn.Free;
  FDoseForms.Free;
  FRoutes.Free;
  FOrgs.Free;
  FTypes.Free;
  FCodes.Free;
  inherited;
end;

procedure TNdcImporter.finishDatabase;
var
  s : String;
  l : integer;
begin
  l := 0;
  for s in FTypes.Keys do
    l := IntegerMax(l, s.Length);
  FConn.ExecSQL('CREATE TABLE NDCProductTypes (NDCKey int NOT NULL, Name nchar('+inttostr(l)+') NOT NULL, CONSTRAINT PK_NDCProductTypes PRIMARY KEY CLUSTERED (NDCKey ASC))');
  FConn.ExecSQL('delete from NDCProductTypes');
  for s in FTypes.Keys do
    FConn.ExecSQL('Insert into NDCProductTypes (NDCKey, Name) values ('+inttostr(FTypes[s])+', '''+SQLWrapString(s)+''')');

  for s in FOrgs.Keys do
    l := IntegerMax(l, s.Length);
  FConn.ExecSQL('CREATE TABLE NDCOrganizations (NDCKey int NOT NULL, Name nchar('+inttostr(l)+') NOT NULL, CONSTRAINT PK_NDCOrganizations PRIMARY KEY CLUSTERED (NDCKey ASC))');
  FConn.ExecSQL('delete from NDCOrganizations');
  for s in FOrgs.Keys do
    FConn.ExecSQL('Insert into NDCOrganizations (NDCKey, Name) values ('+inttostr(FOrgs[s])+', '''+SQLWrapString(s)+''')');

  for s in FDoseforms.Keys do
    l := IntegerMax(l, s.Length);
  FConn.ExecSQL('CREATE TABLE NDCDoseForms (NDCKey int NOT NULL, Name nchar('+inttostr(l)+') NOT NULL, CONSTRAINT PK_NDCDoseForms PRIMARY KEY CLUSTERED (NDCKey ASC))');
  FConn.ExecSQL('delete from NDCDoseForms');
  for s in FDoseforms.Keys do
    FConn.ExecSQL('Insert into NDCDoseForms (NDCKey, Name) values ('+inttostr(FDoseforms[s])+', '''+SQLWrapString(s)+''')');

  for s in FRoutes.Keys do
    l := IntegerMax(l, s.Length);
  FConn.ExecSQL('CREATE TABLE NDCRoutes (NDCKey int NOT NULL, Name nchar('+inttostr(l)+') NOT NULL, CONSTRAINT PK_NDCRoutes PRIMARY KEY CLUSTERED (NDCKey ASC))');
  FConn.ExecSQL('delete from NDCRoutes');
  for s in FRoutes.Keys do
    FConn.ExecSQL('Insert into NDCRoutes (NDCKey, Name) values ('+inttostr(FRoutes[s])+', '''+SQLWrapString(s)+''')');
end;

procedure TNdcImporter.process(callback: TInstallerCallback);
begin
  //FOrgs.Clear;
  //FTypes.Clear;
  //prepareDatabase;
  //importProducts(callback);
  //importPackages(callback);
  //finishDatabase;
  //FConn.Release;
end;

procedure TNdcImporter.Doinstall(sender: TObject; callback: TWorkProgressEvent);
begin
  FOrgs.Clear;
  FTypes.Clear;
  prepareDatabase;
  importProducts(callback);
  importPackages(callback);
  finishDatabase;
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
        'CONSTRAINT PK_NDCProducts PRIMARY KEY CLUSTERED (NDCKey ASC))');
    FConn.ExecSQL('CREATE UNIQUE NONCLUSTERED INDEX [NDCProductsCode] ON NDCProducts ( Code ASC )');
    FConn.ExecSQL('CREATE TABLE NDCPackages ('+
        'NDCKey int NOT NULL, '+
        'ProductKey int NOT NULL, '+
        'Code nchar(12) NOT NULL, '+
        'Code11 nchar(11) NOT NULL, '+
        'Active int NOT NULL, '+
        'Description nchar(255) NOT NULL, '+
        'CONSTRAINT PK_NDCPackages PRIMARY KEY CLUSTERED (NDCKey ASC))');
    FConn.ExecSQL('CREATE UNIQUE NONCLUSTERED INDEX [NDCPackagesCode] ON NDCPackages ( Code ASC )');
    FConn.ExecSQL('CREATE NONCLUSTERED INDEX NDCPackagesProductCode ON NDCPackages (ProductKey ASC, Code ASC )');
    FConn.ExecSQL('CREATE NONCLUSTERED INDEX NDCPackagesProductCode11 ON NDCPackages (ProductKey ASC, Code11 ASC )');
  finally
    md.Free;
  end;
end;

function checkLength(s : String; name : String; len : integer) : String;
begin
  if (s.length > 255) and (len = 255) then
    exit(copy(s, 1, 255));

  if s.Length > len then
    raise Exception.Create(name+' Too long ('+inttostr(s.Length)+')');
  result := s;
end;

function fixEndDate(date : string) : String;
begin
  if date.startsWith('3031') then
    result := '2031'+date.Substring(4)
  else
    result := date;
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

procedure TNdcImporter.processPackage(fields: TFslStringList);
begin
  if fields.count < 4 then
    exit;
  if not FCodes.containsKey(fields[PACK_FIELD_ProductNDC]) then
    exit;
  if FCodes.containsKey(fields[PACK_FIELD_NDCPackageCode]) then
    exit;
  inc(FKey);
  FCodes.add(fields[PACK_FIELD_NDCPackageCode], FKey);
  FConn.BindInteger('NDCKey', FKey);
  FConn.BindInteger('ProductKey', FCodes[fields[PACK_FIELD_ProductNDC]]);
  FConn.BindString('Code', checklength(fields[PACK_FIELD_NDCPackageCode], 'PACK_FIELD_NDCPackageCode', 12));
  FConn.BindString('Code11', genCode11(fields[PACK_FIELD_NDCPackageCode]));

  FConn.BindString('Description', checklength(fields[PACK_FIELD_PackageDescription], 'ProprietaryName', 255));
  if fields.Count > PACK_FIELD_NDC_Exclude_Flag then
    FConn.BindIntegerFromBoolean('Active', fields[PACK_FIELD_NDC_Exclude_Flag] = 'Y')
  else
    FConn.BindIntegerFromBoolean('Active', true);
  FConn.Execute;
end;

procedure TNdcImporter.processProduct(fields : TFslStringList);
begin
  if fields.count < 10 then
    exit;
  if FCodes.containsKey(fields[PROD_FIELD_ProductNDC]) then
    exit;
  inc(FKey);
  FConn.BindInteger('NDCKey', FKey);
  FConn.BindString('Code', checklength(fields[PROD_FIELD_ProductNDC], 'ProductNDC', 11));
  FCodes.Add(fields[PROD_FIELD_ProductNDC], FKey);
  if fields.Count > PROD_FIELD_NDC_Exclude_Flag then
    FConn.BindIntegerFromBoolean('Active', fields[PROD_FIELD_NDC_Exclude_Flag] = 'Y')
  else
    FConn.BindIntegerFromBoolean('Active', true);
  if not FTypes.containsKey(fields[PROD_FIELD_ProductTypeName]) then
    FTypes.Add(fields[PROD_FIELD_ProductTypeName], FTypes.Count+1);
  FConn.BindInteger('Type', FTypes[fields[PROD_FIELD_ProductTypeName]]);
  FConn.BindString('TradeName', checklength(fields[PROD_FIELD_ProprietaryName], 'ProprietaryName', 255));
  FConn.BindString('Suffix', checklength(fields[PROD_FIELD_ProprietaryNameSuffix], 'ProprietaryNameSuffix', 180));
  FConn.BindBlobFromString('Generics', fields[PROD_FIELD_NonProprietaryName]);
  if not FDoseforms.containsKey(fields[PROD_FIELD_DosageFormName]) then
    FDoseforms.Add(fields[PROD_FIELD_DosageFormName], FDoseforms.Count+1);
  FConn.BindInteger('DoseForm', FDoseforms[fields[PROD_FIELD_DosageFormName]]);
  if not FRoutes.containsKey(fields[PROD_FIELD_RouteName]) then
    FRoutes.Add(fields[PROD_FIELD_RouteName], FRoutes.Count+1);
  FConn.BindInteger('Route', FRoutes[fields[PROD_FIELD_RouteName]]);
  FConn.BindDateTimeEx('StartDate', TFslDateTime.fromHL7(fields[PROD_FIELD_StartMarketingDate]));
  FConn.BindDateTimeEx('EndDate', TFslDateTime.fromHL7(fixEndDate(fields[PROD_FIELD_EndMarketingDate])));
  FConn.BindString('Category', checklength(fields[PROD_FIELD_MarketingCategoryName], 'MarketingCategoryName', 40));
  if not FOrgs.containsKey(fields[PROD_FIELD_LabelerName]) then
    FOrgs.Add(fields[PROD_FIELD_LabelerName], FOrgs.Count+1);
  FConn.BindInteger('Company', FOrgs[fields[PROD_FIELD_LabelerName]]);
  FConn.Execute;
end;

procedure TNdcImporter.importPackages(callback: TWorkProgressEvent);
var
  f : TFslCSVExtractor;
  fields : TFslStringList;
begin
  FConn.SQL := 'Insert into NDCPackages (NDCKey, Code, Code11, ProductKey, Active, Description) values (' +
               ':NDCKey, :Code, :Code11, :ProductKey, :Active, :Description)';
  FConn.prepare;
  FKey := 0;
  fields := TFslStringList.create;
  try
    f := TFslCSVExtractor.Create(Path([FSource, 'package.txt']), TEncoding.ASCII, false, 8192);
    try
      f.Separator := #9;
      f.IgnoreWhitespace := false;
      f.ConsumeEntries(fields); // headers
      while f.More do
      begin
        callback(self, 50 + f.percent div 2, false, 'Processing Packages');
        f.ConsumeEntries(fields);
        processPackage(fields);
      end;
    finally
      f.free;
    end;
  finally
    fields.free;
  end;
  FConn.Terminate;
  callback(self, 100, true, 'Finished Processing');
end;

procedure TNdcImporter.importProducts(callback: TWorkProgressEvent);
var
  f : TFslCSVExtractor;
  fields : TFslStringList;
begin
  FConn.SQL := 'Insert into NDCProducts (NDCKey, Code, Active, Type, TradeName, Suffix, DoseForm, Route, StartDate, EndDate, Category, Company, Generics) values (' +
               ':NDCKey, :Code, :Active, :Type, :TradeName, :Suffix, :DoseForm, :Route, :StartDate, :EndDate, :Category, :Company, :Generics)';
  FConn.prepare;
  FKey := 0;
  fields := TFslStringList.create;
  try
    f := TFslCSVExtractor.Create(Path([FSource, 'product.txt']), TEncoding.ASCII, false, 8192);
    try
      f.Separator := #9;
      f.IgnoreWhitespace := false;
      f.ConsumeEntries(fields); // headers
      while f.More do
      begin
        callback(self, f.percent div 2, false, 'Processing Products');
        f.ConsumeEntries(fields);
        processProduct(fields);
      end;
    finally
      f.free;
    end;
  finally
    fields.free;
  end;
  FConn.Terminate;
end;

{ TNDCServices }

constructor TNDCServices.Create(languages : TIETFLanguageDefinitions; db: TFDBManager; version : String);
begin
  inherited Create(languages);

  self.FDb := db;
  self.FVersion := version;
  FTypes := TDictionary<integer, String>.create;
  FOrgs := TDictionary<integer, String>.create;
  FRoutes := TDictionary<integer, String>.create;
  FDoseforms := TDictionary<integer, String>.create;
  load();
end;

procedure TNDCServices.defineFeatures(features: TFslList<TFHIRFeature>);
begin

end;

destructor TNDCServices.Destroy;
begin
  FDb.Free;
  FTypes.Free;
  FOrgs.Free;
  FRoutes.Free;
  FDoseforms.Free;
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

procedure TNDCServices.Close(ctxt: TCodeSystemProviderFilterContext);
begin
  ctxt.Free;
end;

procedure TNDCServices.Close(ctxt: TCodeSystemProviderContext);
begin
  ctxt.Free;
end;

procedure TNDCServices.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  ctxt.Free;
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
    loadDict(conn, FTypes, 'select NDCKey, Name from NDCProductTypes');
    loadDict(conn, FOrgs, 'select NDCKey, Name from NDCOrganizations');
    loadDict(conn, FRoutes, 'select NDCKey, Name from NDCRoutes');
    loadDict(conn, FDoseforms, 'select NDCKey, Name from NDCDoseForms');
    FPackageCount := conn.countSql('Select count(*) from NDCPackages');
    FProductCount := conn.countSql('Select count(*) from NDCProducts');
    conn.release;
  except
    on e : Exception do
    begin
      conn.error(e);
      raise;
    end;
  end;
end;

function TNDCServices.Code(context: TCodeSystemProviderContext): string;
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

function TNDCServices.getIterator(context : TCodeSystemProviderContext) : TCodeSystemIteratorContext;
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
        iter.Free;
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

function TNDCServices.getNextContext(context : TCodeSystemIteratorContext) : TCodeSystemProviderContext;
var
  iter : TNDCIteratorContext;
  ctxt : TNDCProviderContext;
begin
  iter := context as TNDCIteratorContext;
  if iter.FProducts then
  begin
    ctxt := TNDCProviderContext.create(false, iter.FConn.GetColIntegerByName('NDCKey'));
    try
      ctxt.FDisplay := productDisplay(iter.FConn);
      ctxt.code := iter.FConn.GetColStringByName('Code');
      iter.FMore := iter.FConn.FetchNext;
      result := ctxt.link;
    finally
      ctxt.Free;
    end;
  end
  else
  begin
    ctxt := TNDCProviderContext.create(true, iter.FConn.GetColIntegerByName('NDCKey'));
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
          result := getNextContext(iter);
        end;
      end
      else
      begin
        ctxt.code := iter.FConn.GetColStringByName('Code');
        iter.FSecond := true;
      end;

      result := ctxt.link;
    finally
       ctxt.Free;
    end;
  end;
end;

function TNDCServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TNDCServices.description: String;
begin
  result := 'NDC Codes';
end;

function TNDCServices.Display(context: TCodeSystemProviderContext; const lang : THTTPLanguages): string;
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

procedure TNDCServices.Displays(context: TCodeSystemProviderContext; list: TCodeDisplays);
begin
  list.see(Display(context, THTTPLanguages.Create('')));
end;

function TNDCServices.doesFilter(prop: String; op: TFhirFilterOperator; value: String): boolean;
begin
  result := (prop = 'code-type') and (op = foEqual) and StringArrayExistsSensitive(['10-digit', '11-digit', 'product'], value);
end;

procedure TNDCServices.extendLookup(factory: TFHIRFactory; ctxt: TCodeSystemProviderContext; const lang : THTTPLanguages; props: TArray<String>; resp: TFHIRLookupOpResponseW);
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

function TNDCServices.filter(forIteration : boolean; prop: String; op: TFhirFilterOperator; value: String; prep: TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
var
  ctxt : TNDCFilterPreparationContext;
  res : TNDCFilterContext;
begin
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
        raise Exception.Create('The filter "'+prop+' '+CODES_TFhirFilterOperator[op]+' '+value+'" is not understood for NDC');
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
      raise Exception.Create('The filter "'+prop+' '+CODES_TFhirFilterOperator[op]+' '+value+'" is not understood for NDC');
    result := res.link;
  finally
    res.free;
  end;
end;

function TNDCServices.FilterConcept(ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
var
  context : TNDCFilterContext;
  res : TNDCProviderContext;
begin
  context := ctxt as TNDCFilterContext;
  res := TNDCProviderContext.create(true, context.FConn.GetColIntegerByName('NDCKey'));
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
    res.Free;
  end;
end;

function TNDCServices.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String; var message: String): TCodeSystemProviderContext;
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
      res := TNDCProviderContext.create(true, context.FConn.GetColIntegerByName('NDCKey'));
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
        res.Free;
      end;
    end
    else
      result := nil;
  finally
    context.FConn.Terminate;
  end;
end;

function TNDCServices.FilterMore(ctxt: TCodeSystemProviderFilterContext): boolean;
var
  context : TNDCFilterContext;
begin
  context := ctxt as TNDCFilterContext;
  result := context.FConn.FetchNext;
end;

procedure TNDCServices.getCDSInfo(card: TCDSHookCard; const lang : THTTPLanguages; baseURL, code, display: String);
begin
  raise ETerminologyTodo.Create('Not done yet: TNDCServices.getCDSInfo');
end;

function TNDCServices.getDefinition(code: String): String;
begin
  result := getDisplay(code, THTTPLanguages.Create(''));
end;

function TNDCServices.getDisplay(code : String; const lang : THTTPLanguages): String;
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

function TNDCServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := TNDCFilterPreparationContext.Create;
end;

function TNDCServices.InFilter(ctxt: TCodeSystemProviderFilterContext; concept: TCodeSystemProviderContext): Boolean;
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

function TNDCServices.IsAbstract(context: TCodeSystemProviderContext): boolean;
begin
  result := false;
end;

function TNDCServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TNDCServices.locate(code: String; var message: String): TCodeSystemProviderContext;
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
      if (k <> 0) or not code.contains('-') then
        c := TNDCProviderContext.create(true, k)
      else
      begin
        k := conn.CountSQL('Select NDCKey from NDCProducts where code = '''+SQLWrapString(code)+'''');
        if k <> 0 then
          c := TNDCProviderContext.create(false, k)
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

function TNDCServices.locateIsA(code, parent: String; disallowParent : boolean = false): TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.Create('Not done yet: TNDCServices.locateIsA');
end;

function TNDCServices.packageDisplay(conn: TFDBConnection): String;
begin
  if conn.ColStringByName['Suffix'] <> '' then
    result := conn.ColStringByName['TradeName']+' '+conn.ColStringByName['Suffix']+', '+conn.ColStringByName['Description']+' (package)'
  else
    result := conn.ColStringByName['TradeName']+', '+conn.ColStringByName['Description']+' (package)';
end;

function TNDCServices.productDisplay(conn: TFDBConnection): String;
begin
  result := conn.ColStringByName['TradeName']+' '+conn.ColStringByName['Suffix']+' (product)';
end;

function TNDCServices.prepare(prep: TCodeSystemProviderFilterPreparationContext): boolean;
var
  ctxt : TNDCFilterPreparationContext;
begin
  ctxt := prep as TNDCFilterPreparationContext;
  result := true;
end;

function TNDCServices.searchFilter(filter: TSearchFilterText; prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.Create('Not done yet: TNDCServices.searchFilter');
end;

function TNDCServices.systemUri(context: TCodeSystemProviderContext): String;
begin
  result := 'http://hl7.org/fhir/sid/ndc';
end;

function TNDCServices.TotalCount: integer;
begin
  result := FProductCount + FPackageCount;
end;

{ TNDCProviderContext }

constructor TNDCProviderContext.create(package: boolean; key: integer);
begin
  inherited create;
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
