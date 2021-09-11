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
    FClient: TFHIRClientV;
    FWorkerObject: TObject;
    FPinned: boolean;
    procedure SetClient(AValue: TFHIRClientV);

  public
    destructor Destroy; override;
    function link : TFHIRServerEntry; overload;

    class function fromJson(json : TJsonObject) : TFHIRServerEntry;
    function toJson : TJsonObject;

    property pinned : boolean read FPinned write FPinned;
    property workerObject : TObject read FWorkerObject write FWorkerObject;
    property client : TFHIRClientV read FClient write SetClient;

  end;

function makeFactory(version : TFHIRVersion) : TFHIRFactory;
//function makeClient(version : TFHIRVersion; url : String) : TFhirClientV;

function checkWellKnown(url : String; server : TFHIRServerDetails; var msg : String) : boolean;
function checkMetadata(url : String; server : TFHIRServerDetails; var msg : String) : boolean;

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

function checkWellKnown(url : String; server : TFHIRServerDetails; var msg : String) : boolean;
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

procedure checkJsonMetadata(server : TFHIRServerDetails; bytes : TBytes);
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
  finally
    json.free;
  end;
end;

procedure checkXmlMetadata(server : TFHIRServerDetails; bytes : TBytes);
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

function checkMetadata(url : String; server : TFHIRServerDetails; var msg : String) : boolean;
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

destructor TFHIRServerEntry.Destroy;
begin
  FClient.Free;
  inherited Destroy;
end;

function TFHIRServerEntry.link: TFHIRServerEntry;
begin
  result := TFHIRServerEntry(inherited link);
end;

class function TFHIRServerEntry.fromJson(json: TJsonObject): TFHIRServerEntry;
begin
  result := TFHIRServerEntry.create;
  try
    result.pinned := json.bool['pinned'];
    result.name := json.str['name'];
    result.URL := json.str['url'];
    result.version := TFHIRVersions.readVersion(json.str['fhir-version']);
    result.json := json.bool['json'];
    result.xml := json.bool['xml'];
    result.format := TFHIRFormat(StringArrayIndexOf(CODES_TFHIRFormat, json.str['format']));
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRServerEntry.toJson: TJsonObject;
begin
  result := TJsonObject.create;
  try
    result.bool['pinned'] := pinned;
    result.str['name'] := name;
    result.str['url'] := URL;
    result.str['fhir-version'] := CODES_TFHIRVersion[version];
    result.bool['json'] := json;
    result.bool['xml'] := xml;
    result.str['format'] := CODES_TFHIRFormat[format];
    result.link;
  finally
    result.free;
  end;
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

