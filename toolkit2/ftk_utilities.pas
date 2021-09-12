unit ftk_utilities;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_utilities, fsl_xml, fsl_json, fsl_http, fsl_fetcher,
  fhir_objects, fhir_factory, fhir_client, fhir_utilities, fhir_client_http;

type

  { TFHIRServerEntry }

  TFHIRServerEntry = class (TFHIRServerDetails)
  private
    FId : String;
    FClient: TFHIRClientV;
    FInstantiates: TStringList;
    FLogFileName: String;
    FWorkerObject: TObject;
    FPinned: boolean;
    procedure SetClient(AValue: TFHIRClientV);
    function hasCapability(s : string) : boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRServerEntry; overload;
    function clone : TFHIRServerEntry; overload;
    procedure assign(other : TFslObject); override;

    class function fromJson(json : TJsonObject) : TFHIRServerEntry;
    function toJson : TJsonObject;

    procedure getInformation(list : TStrings);

    property id : String read FId write FId;
    property pinned : boolean read FPinned write FPinned;
    property workerObject : TObject read FWorkerObject write FWorkerObject;
    property client : TFHIRClientV read FClient write SetClient;
    property logFileName : String read FLogFileName write FLogFileName;
    property instantiates : TStringList read FInstantiates;

  end;

function makeFactory(version : TFHIRVersion) : TFHIRFactory;
//function makeClient(version : TFHIRVersion; url : String) : TFhirClientV;

function checkWellKnown(url : String; server : TFHIRServerEntry; var msg : String) : boolean;
function checkMetadata(url : String; server : TFHIRServerEntry; var msg : String) : boolean;

implementation

uses
  fhir3_client, fhir4_client, fhir3_factory, fhir4_factory;

function makeFactory(version : TFHIRVersion) : TFHIRFactory;
begin
  case version of
    fhirVersionRelease3 : result := TFHIRFactoryR3.create;
    fhirVersionRelease4 : result := TFHIRFactoryR4.create;
    else
      raise EFHIRException.create('The version '+CODES_TFHIRVersion[version]+' is not supported at this time');
  end;
end;

function checkWellKnown(url : String; server : TFHIRServerEntry; var msg : String) : boolean;
begin
  msg := '';
  try
    server.smartConfig := TInternetFetcher.fetchJson(UrlPath([url, '.well-known/smart-configuration']));
    result := true;
  except
    on e : Exception do
    begin
      msg := 'Unable to access /.well-known/smart-configuration: '+e.message;
      result := false;
    end;
  end;
end;

procedure check(test : boolean; message : String);
begin
  if not test then
    raise EFHIRClientException.create(message);
end;

procedure checkJsonMetadata(server : TFHIRServerEntry; bytes : TBytes);
var
  json : TJsonObject;
  fmts : TJsonArray;
  i : integer;
  fmt : String;
begin
  json := TJSONParser.Parse(bytes);
  try
    check(json.str['fhirVersion'] <> '', 'No fhirVersion found in Metadata');
    server.version := TFHIRVersions.readVersion(json.str['fhirVersion']);
//    check(json.forceArr['rest'].Count = 1, 'Unable to process metada: found '+inttostr(json.forceArr['rest'].Count)+' rest statements');
//    rest := json.forceArr['rest'][0];
    fmts := json.forceArr['format'];
    for i := 0 to fmts.count - 1 do
    begin
      fmt := fmts.Value[i];
      if (fmt.contains('xml')) then
        server.xml := true;
      if (fmt.contains('json')) then
        server.json := true;
    end;
    server.instantiates.clear;
    for i := 0 to json.forceArr['instantiates'].Count - 1 do
      server.instantiates.add(json.forceArr['instantiates'].Value[i]);
  finally
    json.free;
  end;
end;

procedure checkXmlMetadata(server : TFHIRServerEntry; bytes : TBytes);
var
  xml : TMXmlDocument;
  e, fv, r, f : TMXmlElement;
  list : TFslList<TMXmlElement>;
  fmt : String;
begin
  xml := TMXmlParser.parse(bytes, [xpResolveNamespaces, xpDropWhitespace, xpDropComments, xpHTMLEntities]);
  try
    e := xml.docElement;
    fv := e.element('fhirVersion');
    check(fv <> nil, 'No fhirVersion found in Metadata');
    server.version := TFHIRVersions.readVersion(fv.attribute['value']);
    r := e.element('rest');
    list := TFslList<TMXmlElement>.create;
    try
      e.listElements('format', list);
      for f in list do
      begin
        fmt := f.attribute['value'];
        if (fmt.contains('xml')) then
          server.xml := true;
        if (fmt.contains('json')) then
          server.json := true;
      end;
    finally
      list.free;
    end;
  finally
    xml.free;
  end;
end;

function checkMetadata(url : String; server : TFHIRServerEntry; var msg : String) : boolean;
var
  fetcher : TInternetFetcher;
begin
  msg := '';
  try
    fetcher := TInternetFetcher.create;
    try
      fetcher.Accept := 'application/json, application/xml';
      fetcher.URL := UrlPath([url, 'metadata']);
      fetcher.fetch;
      result := true;
      if fetcher.contentType.contains('xml') then
        checkXmlMetadata(server, fetcher.Buffer.AsBytes)
      else if fetcher.contentType.contains('json') then
        checkJsonMetadata(server, fetcher.Buffer.AsBytes)
      else
      begin
        msg := 'Unrecognised content type reading metadata: '+fetcher.contentType;
        result := false;
      end;
      result := true;
    finally
      fetcher.free;
    end;
  except
    on e : Exception do
    begin
      msg := 'Unable to access metadata: '+e.message;
      result := false;
    end;
  end;
end;

{ TFHIRServerEntry }

procedure TFHIRServerEntry.SetClient(AValue: TFHIRClientV);
begin
  FClient.Free;
  FClient := AValue;
end;

function TFHIRServerEntry.hasCapability(s: string): boolean;
var
  i : integer;
begin
  result := false;
  if smartConfig <> nil then
    for i := 0 to smartConfig.forceArr['capabilities'].Count - 1 do
      if (smartConfig.forceArr['capabilities'].Value[i] = s) then
          exit(true);
end;

constructor TFHIRServerEntry.Create;
begin
  inherited Create;
  FInstantiates := TStringList.create;
end;

destructor TFHIRServerEntry.Destroy;
begin
  FClient.Free;
  FInstantiates.Free;
  inherited Destroy;
end;

function TFHIRServerEntry.link: TFHIRServerEntry;
begin
  result := TFHIRServerEntry(inherited link);
end;

function TFHIRServerEntry.clone: TFHIRServerEntry;
begin
  result := TFHIRServerEntry(inherited clone);
end;

procedure TFHIRServerEntry.assign(other: TFslObject);
var
  o : TFHIRServerEntry;
begin
  inherited assign(other);
  o := other as TFHIRServerEntry;
  client := nil;
  id := o.id;
  LogFileName := o.logFileName;
//  FWorkerObject don't change
  Pinned := o.pinned;
end;

class function TFHIRServerEntry.fromJson(json: TJsonObject): TFHIRServerEntry;
var
  i : integer;
begin
  result := TFHIRServerEntry.create;
  try
    result.id := json.str['id'];
    result.pinned := json.bool['pinned'];
    result.name := json.str['name'];
    result.URL := json.str['url'];
    result.version := TFHIRVersions.readVersion(json.str['fhir-version']);
    result.json := json.bool['json'];
    result.xml := json.bool['xml'];
    result.logFileName := json.str['logFileName'];
    result.format := TFHIRFormat(StringArrayIndexOf(CODES_TFHIRFormat, json.str['format']));
    result.smartConfig := json.obj['smart'].link;
    for i := 0 to json.forceArr['instantiates'].Count - 1 do
      result.instantiates.add(json.forceArr['instantiates'].Value[i]);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRServerEntry.toJson: TJsonObject;
var
  s : String;
begin
  result := TJsonObject.create;
  try
    result.bool['pinned'] := pinned;
    result.str['id'] := id;
    result.str['name'] := name;
    result.str['url'] := URL;
    result.str['fhir-version'] := CODES_TFHIRVersion[version];
    result.bool['json'] := json;
    result.bool['xml'] := xml;
    result.str['format'] := CODES_TFHIRFormat[format];
    result.str['logFileName'] := logFileName;
    result.obj['smart'] := smartConfig.link;
    for s in instantiates do
      result.forceArr['instantiates'].add(s);
    result.link;
  finally
    result.free;
  end;
end;

procedure TFHIRServerEntry.getInformation(list : TStrings);
begin
  if smartConfig <> nil then
  begin
    list.add('This server is a Smart-App-Launch server');
    if hasCapability('launch-ehr') and hasCapability('launch-standalone') then
      list.add('Good to go: Server supports standalone and ehr modes')
    else if hasCapability('launch-standalone') then
      list.add('Good to go: Server supports standalone mode')
    else if hasCapability('launch-ehr') then
      list.add('No go: Server does not support standalone mode (ehr only)')
    else
      list.add('No go: No launch modes found');

    if hasCapability('client-public') and hasCapability('client-confidential-symmetric') then
      list.add('Server supports both public and confidential clients')
    else if hasCapability('client-public') then
      list.add('Server supports ony public clients')
    else if hasCapability('client-confidential-symmetric') then
      list.add('Server supports ony Confidential clients')
    else
      list.add('Server doesn''t support either public or confidential clients');

    if hasCapability('sso-openid-connect') then
      list.add('Server supports OpenIDConnect');

    if hasCapability('context-standalone-patient') and hasCapability('context-standalone-encounter') then
      list.add('Server supports Patient and Encounter Launch mode')
    else if hasCapability('context-standalone-patient') then
      list.add('Server supports Patient Launch mode')
    else if hasCapability('context-standalone-encounter') then
      list.add('Server supports Encounter Launch mode');

    if hasCapability('permission-patient') then
      list.add('Server supports Patient Level scopes');
    if hasCapability('permission-user') then
      list.add('Server supports User Level Scopes');
  end
  else
    list.add('This server does not support Smart-App-Launch');
  if instantiates.indexOf('http://hl7.org/fhir/CapabilityStatement/terminology-server') > -1 then
    list.add('This server is a terminology server');
end;


//function makeClient(version : TFHIRVersion; url : String) : TFhirClientV;
//begin
//  case version of
//    fhirVersionRelease3 : result := TFhirClient3.create(nil, defLang, TFHIRHTTPCommunicator.create(url));
//    fhirVersionRelease4 : result := TFhirClient4.create(nil, defLang, TFHIRHTTPCommunicator.create(url));
//  else
//    raise EFHIRException.create('The version '+CODES_TFHIRVersion[version]+' is not supported at this time');
//  end;
//end;

end.

