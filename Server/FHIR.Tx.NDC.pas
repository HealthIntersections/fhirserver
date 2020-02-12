unit FHIR.Tx.NDC;

interface

uses
  SysUtils, Classes, Generics.Collections,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Collections, FHIR.Support.Stream,
  FHIR.Database.Manager, FHIR.Database.Dialects,
  FHIR.Base.Objects, FHIR.Base.Common, FHIR.Base.Factory, FHIR.base.Utilities,
  FHIR.CdsHooks.Utilities,
  FHIR.Tx.Service;

type
  TNdcImporter = class (TFslObject)
  private
    FSource: String;
    FDatabase: TFslDBManager;
    FConn : TFslDBConnection;
    FCodes : TDictionary<String, integer>;
    FTypes : TDictionary<String, integer>;
    FOrgs : TDictionary<String, integer>;
    FRoutes : TDictionary<String, integer>;
    FDoseforms : TDictionary<String, integer>;
    FKey : integer;
    procedure SetDatabase(const Value: TFslDBManager);
    procedure importProducts(callback: TInstallerCallback);
    procedure importPackages(callback: TInstallerCallback);
    procedure processProduct(fields: TFslStringList);
    procedure processPackage(fields: TFslStringList);
    procedure prepareDatabase;
    procedure finishDatabase;
  public
    constructor Create(source : String);
    destructor Destroy; override;

    property Database : TFslDBManager read FDatabase write SetDatabase;
    property source : String read FSource write FSource;

    procedure process(callback : TInstallerCallback);
  end;

  TNDCProviderContext = class (TCodeSystemProviderContext)
  private
    FKey: integer;
    FPackage: boolean;
  public
    constructor create(package : boolean; key : integer);
    property package : boolean read FPackage write FPackage;
    property key : integer read FKey write FKey;
  end;

  TNDCServices = class (TCodeSystemProvider)
  private
    FDb : TFslDBManager;
    FVersion : string;

    FTypes : TDictionary<integer, String>;
    FOrgs : TDictionary<integer, String>;
    FRoutes : TDictionary<integer, String>;
    FDoseforms : TDictionary<integer, String>;
    procedure load;
    procedure loadDict(conn: TFslDBConnection; dict: TDictionary<integer, String>; sql: String);
  public
    constructor Create(db : TFslDBManager; version : String);
    destructor Destroy; Override;
    Function Link : TNDCServices; overload;

    function TotalCount : integer;  override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function system(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; lang : String):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; lang : String) : string; override;
    procedure Displays(code : String; list : TStringList; lang : String); override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList; lang : String); override;
    function Definition(context : TCodeSystemProviderContext) : string; override;

    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
    function prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean; override;

    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure getCDSInfo(card : TCDSHookCard; lang, baseURL, code, display : String); override;
    procedure extendLookup(factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; lang : String; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
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

constructor TNdcImporter.Create(source: String);
begin
  inherited create;
  FSource := source;
  FTypes := TDictionary<String, integer>.create;
  FCodes := TDictionary<String, integer>.create;
  FOrgs := TDictionary<String, integer>.create;
  FRoutes := TDictionary<String, integer>.create;
  FDoseForms := TDictionary<String, integer>.create;
end;

destructor TNdcImporter.Destroy;
begin
  FDatabase.Free;
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
  FOrgs.Clear;
  FTypes.Clear;
  FConn := FDatabase.GetConnection('NDC');
  try
    prepareDatabase;
    importProducts(callback);
    importPackages(callback);
    finishDatabase;
    FConn.Release;
  except
    on e : Exception do
      FConn.Error(e);
  end;
end;

procedure TNdcImporter.prepareDatabase;
var
  md : TFslDBMetaData;
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
        'StartDate '+DBDateTimeType(FDatabase.Platform)+' NULL, '+
        'EndDate '+DBDateTimeType(FDatabase.Platform)+' NULL, '+
        'Category nchar(40) NOT NULL, '+
        'Company int NOT NULL, '+
        'Generics '+DBBlobType(FDatabase.Platform)+' NULL, '+
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

procedure TNdcImporter.importPackages(callback: TInstallerCallback);
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
        callback(50 + f.percent div 2, 'Processing Packages');
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
end;

procedure TNdcImporter.importProducts(callback: TInstallerCallback);
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
        callback(f.percent div 2, 'Processing Products');
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

procedure TNdcImporter.SetDatabase(const Value: TFslDBManager);
begin
  FDatabase := Value;
end;

{ TNDCServices }

constructor TNDCServices.Create(db: TFslDBManager; version : String);
begin
  inherited Create;

  self.FDb := db;
  self.FVersion := version;
  FTypes := TDictionary<integer, String>.create;
  FOrgs := TDictionary<integer, String>.create;
  FRoutes := TDictionary<integer, String>.create;
  FDoseforms := TDictionary<integer, String>.create;
  load();
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

procedure TNDCServices.loadDict(conn : TFslDBConnection; dict : TDictionary<integer, String>; sql : String);
begin
  conn.SQL := sql;
  conn.Prepare;
  conn.Execute;
  while conn.FetchNext do
    dict.Add(conn.ColInteger[1], conn.ColString[2]);
  conn.Terminate;
end;

procedure TNDCServices.load;
begin
  FDB.connection('load', procedure (conn : TFslDBConnection) begin
    loadDict(conn, FTypes, 'select NDCKey, Name from NDCProductTypes');
    loadDict(conn, FOrgs, 'select NDCKey, Name from NDCOrganizations');
    loadDict(conn, FRoutes, 'select NDCKey, Name from NDCRoutes');
    loadDict(conn, FDoseforms, 'select NDCKey, Name from NDCDoseForms');
  end);
end;

function TNDCServices.Code(context: TCodeSystemProviderContext): string;
var
  c : string;
  code : TNDCProviderContext;
begin
  code := context as TNDCProviderContext;
  FDB.connection('Code', procedure (conn : TFslDBConnection) begin
    if code.package then
      c := conn.Lookup('NDCPackages', 'NDCKey', inttostr(code.key), 'Code', '')
    else
      c := conn.Lookup('NDCProducts', 'NDCKey', inttostr(code.key), 'Code', '');
  end);
  result := c;
end;

function TNDCServices.ChildCount(context: TCodeSystemProviderContext): integer;
var
  count : integer;
  code : TNDCProviderContext;
begin
  code := context as TNDCProviderContext;
  if code.package then
    exit(0);
  FDB.connection('ChildCount', procedure (conn : TFslDBConnection) begin
    count := conn.CountSQL('select count(*) from NDCPackages where ProductKey = '+inttostr(code.key));
  end);
  result := count;
end;

function TNDCServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := Display(context, '');
end;

function TNDCServices.Display(context: TCodeSystemProviderContext; lang: String): string;
var
  c : string;
  code : TNDCProviderContext;
begin
  code := context as TNDCProviderContext;
  c := '';
  FDB.connection('Display', procedure (conn : TFslDBConnection) begin
    if code.package then
    begin
      conn.sql := 'Select TradeName, Suffix, Description from NDCProducts, NDCPackages where NDCProducts.NDCKey = NDCPackages.ProductKey and NDCPackages.NDCKey = '+inttostr(code.key);
      conn.prepare;
      conn.execute;
      if conn.fetchnext then
        c := conn.ColStringByName['TradeName']+' '+conn.ColStringByName['Suffix']+' '+conn.ColStringByName['Description'];
      conn.terminate;
    end
    else
    begin
      conn.sql := 'Select TradeName, Suffix from NDCProducts where NDCKey = '+inttostr(code.key);
      conn.prepare;
      conn.execute;
      if conn.fetchnext then
        c := conn.ColStringByName['TradeName']+' '+conn.ColStringByName['Suffix'];
      conn.terminate;
    end;
  end);
  result := c;
end;

procedure TNDCServices.Displays(context: TCodeSystemProviderContext; list: TStringList; lang: String);
var
  c : string;
  code : TNDCProviderContext;
begin
  code := context as TNDCProviderContext;
  FDB.connection('Displays', procedure (conn : TFslDBConnection) begin
    if code.package then
      c := conn.Lookup('NDCPackages', 'NDCKey', inttostr(code.key), 'Description', '')
    else
      c := conn.Lookup('NDCProducts', 'NDCKey', inttostr(code.key), 'TradeName', '')+' '+conn.Lookup('NDCProducts', 'NDCKey', inttostr(code.key), 'Suffix', '');
  end);
  list.Add(c.Trim);
end;

procedure TNDCServices.Displays(code: String; list: TStringList; lang: String);
var
  c : string;
begin
  FDB.connection('Displays', procedure (conn : TFslDBConnection) begin
    c := conn.Lookup('NDCPackages', 'Code', code, 'Description', '');
    if c = '' then
      c := conn.Lookup('NDCProducts', 'Code', code, 'TradeName', '')+' '+conn.Lookup('NDCProducts', 'Code', code, 'Suffix', '');
  end);
  list.Add(c.Trim);
end;

procedure TNDCServices.extendLookup(factory: TFHIRFactory; ctxt: TCodeSystemProviderContext; lang: String; props: TArray<String>; resp: TFHIRLookupOpResponseW);
var
  code : TNDCProviderContext;
begin
  code := ctxt as TNDCProviderContext;
  FDB.connection('Display', procedure (conn : TFslDBConnection) begin
    if code.package then
      conn.sql := 'Select Type, TradeName, DoseForm, Route, Company, Category, Generics from NDCProducts, NDCPackages where NDCProducts.NDCKey = NDCPackages.ProductKey and NDCPackages.NDCKey = '+inttostr(code.key)
    else
      conn.sql := 'Select * from NDCProducts where NDCKey = '+inttostr(code.key);
    conn.prepare;
    conn.execute;
    if conn.fetchnext then
    begin
      resp.addProp('type').value := factory.makeString(FTypes[conn.ColIntegerByName['Type']]);
      resp.addProp('trade-name').value := factory.makeString(conn.ColStringByName['TradeName']);
      resp.addProp('dose-form').value := factory.makeString(FDoseforms[conn.ColIntegerByName['DoseForm']]);
      resp.addProp('route').value := factory.makeString(FRoutes[conn.ColIntegerByName['Route']]);
      resp.addProp('company').value := factory.makeString(FOrgs[conn.ColIntegerByName['Company']]);
      resp.addProp('category').value := factory.makeString(conn.ColStringByName['Category']);
      resp.addProp('generic').value := factory.makeString(conn.ColBlobAsStringByName['Generics']);
    end;
    conn.terminate;
  end);
end;

function TNDCServices.filter(prop: String; op: TFhirFilterOperator; value: String; prep: TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.Create('Not done yet');
end;

function TNDCServices.FilterConcept(ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.Create('Not done yet');
end;

function TNDCServices.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String; var message: String): TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.Create('Not done yet');
end;

function TNDCServices.FilterMore(ctxt: TCodeSystemProviderFilterContext): boolean;
begin
  raise ETerminologyTodo.Create('Not done yet');
end;

procedure TNDCServices.getCDSInfo(card: TCDSHookCard; lang, baseURL, code, display: String);
begin
  raise ETerminologyTodo.Create('Not done yet');
end;

function TNDCServices.getcontext(context: TCodeSystemProviderContext; ndx: integer): TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.Create('Not done yet');
end;

function TNDCServices.getDefinition(code: String): String;
begin
  result := getDisplay(code, '');
end;

function TNDCServices.getDisplay(code, lang: String): String;
var
  c : string;
begin
  c := '';
  FDB.connection('getDisplay', procedure (conn : TFslDBConnection) begin
    conn.sql := 'Select TradeName, Suffix from NDCProducts where Code = '''+SQLWrapString(code)+'''';
    conn.prepare;
    conn.execute;
    if conn.fetchnext then
      c := conn.ColStringByName['TradeName']+' '+conn.ColStringByName['Suffix']
    else
    begin
      conn.terminate;
      conn.sql := 'Select TradeName, Suffix, Description from NDCProducts, NDCPackages where NDCProducts.NDCKey = NDCPackages.ProductKey and (NDCPackages.Code = '''+SQLWrapString(code)+''' or NDCPackages.Code11 = '''+SQLWrapString(code)+''')';
      conn.prepare;
      conn.execute;
      if conn.fetchnext then
        c := conn.ColStringByName['TradeName']+' '+conn.ColStringByName['Suffix']+' '+conn.ColStringByName['Description'];
    end;
    conn.terminate;
  end);
  result := c;
end;

function TNDCServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  raise ETerminologyTodo.Create('Not done yet');
end;

function TNDCServices.InFilter(ctxt: TCodeSystemProviderFilterContext; concept: TCodeSystemProviderContext): Boolean;
begin
  raise ETerminologyTodo.Create('Not done yet');
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
  c : TCodeSystemProviderContext;
  k : integer;
begin
  c := nil;
  FDB.connection('getDisplay', procedure (conn : TFslDBConnection) begin
    k := conn.CountSQL('Select NDCKey from NDCPackages where code = '''+SQLWrapString(code)+''' or code11 = '''+SQLWrapString(code)+'''');
    if k <> 0 then
      c := TNDCProviderContext.create(true, k)
    else
    begin
      k := conn.CountSQL('Select NDCKey from NDCProducts where code = '''+SQLWrapString(code)+'''');
      if k <> 0 then
        c := TNDCProviderContext.create(false, k)
    end;
  end);
  result := c;
end;

function TNDCServices.locateIsA(code, parent: String): TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.Create('Not done yet');
end;

function TNDCServices.prepare(prep: TCodeSystemProviderFilterPreparationContext): boolean;
begin
  raise ETerminologyTodo.Create('Not done yet');
end;

function TNDCServices.searchFilter(filter: TSearchFilterText; prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.Create('Not done yet');
end;

function TNDCServices.system(context: TCodeSystemProviderContext): String;
begin
  result := 'http://hl7.org/fhir/sid/ndc';
end;

function TNDCServices.TotalCount: integer;
var
  count : cardinal;
begin
  FDB.connection('ChildCount', procedure (conn : TFslDBConnection) begin
    count := conn.CountSQL('select count(*) from NDCPackages');
    count := count + conn.CountSQL('select count(*) from NDCProducts');
  end);
  result := count;
end;

{ TNDCProviderContext }

constructor TNDCProviderContext.create(package: boolean; key: integer);
begin
  inherited create;
  self.package := package;
  self.key := key;
end;

end.
