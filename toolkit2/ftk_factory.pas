unit ftk_factory;

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  Dialogs,

  fsl_base, fsl_utilities, fsl_xml, fsl_json,
  fhir_objects,

  ftk_context, ftk_store,
  ftk_editor_text, ftk_editor_ini, ftk_editor_xml, ftk_editor_json, ftk_editor_html,
  ftk_editor_md, ftk_editor_js, ftk_editor_hl7, ftk_editor_fhir,
  ftk_frame_resource, ftk_frame_codesystem;

type

  { TToolkitFactory }

  TToolkitFactory = class (TFslObject)
  private
    FContext : TToolkitContext;
    FHandle: TComponent;
  public
    constructor Create(context : TToolkitContext; handle: TComponent);
    destructor Destroy; override;
    function makeNewSession(kind : TSourceEditorKind) : TToolkitEditSession;
    function examineFile(filename : String; const bytes : TBytes) : TToolkitEditSession;

    function makeEditor(session : TToolkitEditSession) : TToolkitEditor;
    function makeResourceFrame(name : String) : TResourceEditorFrame;
  end;

implementation

{ TToolkitFactory }

constructor TToolkitFactory.Create(context: TToolkitContext; handle: TComponent);
begin
  inherited Create;
  FCOntext := context;
  FHandle := handle;
end;

destructor TToolkitFactory.Destroy;
begin
  FCOntext.free;
  inherited Destroy;
end;

function TToolkitFactory.makeNewSession(kind: TSourceEditorKind): TToolkitEditSession;
begin
  result := TToolkitEditSession.create;
  result.guid := NewGuidId;
  result.kind := kind;
end;

function loadJson(bytes : TBytes) : TJsonObject;
var
  parser : TJSONParser;
begin
  parser := TJSONParser.create;
  try
    result := parser.Parse(bytes, 0, false);
  finally
    parser.free;
  end;
end;

function loadXml(bytes : TBytes) : TMXmlDocument;
var
  parser : TMXmlParser;
begin
  parser := TMXmlParser.create;
  try
    result := parser.parse(bytes, [xpHTMLEntities]);
  finally
    parser.free;
  end;
end;

procedure readNamespace(xml : TMXmlElement; var ns, name : String);
var
  p : String;
  a : TMXmlAttribute;
begin
  name := xml.Name;
  if (name.contains(':')) then
  begin
    StringSplit(name, ':', p, name);
    for a in xml.Attributes do
      if a.Name = 'xmlns:'+p then
        ns := a.Value;
  end
  else
  begin
    for a in xml.Attributes do
      if a.Name = 'xmlns' then
        ns := a.Value;
  end;
end;

function examineXml(xml : TMXmlDocument) : TToolkitEditSession;
var
  ns, name : String;
begin
  readNamespace(xml.docElement, ns, name);
  if (ns = 'http://hl7.org/fhir') then
    result := TToolkitEditSession.createResource(ffXml, 'resourceType', name)
  else if (ns = 'urn:hl7-org:v3') and (name = 'ClinicalDocument') then
    result := TToolkitEditSession.create(sekCDA)
  else
    result := TToolkitEditSession.create(sekXml);
end;

function examineJson(json : TJsonObject) : TToolkitEditSession;
var
  rt : String;
begin
  rt := json.str['resourceType'];
  if (rt <> '') then
    result := TToolkitEditSession.createResource(ffJson, 'resourceType', rt)
  else
    result := TToolkitEditSession.create(sekJson);
end;

function TToolkitFactory.examineFile(filename: String; const bytes: TBytes): TToolkitEditSession;
var
  ext : String;
  xml : TMXmlDocument;
  json : TJsonObject;
begin
  result := nil;
  ext := Lowercase(ExtractFileExt(filename));
  if (ext = '.ini') then
    result := TToolkitEditSession.create(sekIni)
  else if (ext = '.html') then
    result := TToolkitEditSession.create(sekHTML)
  else if (ext = '.md') then
    result := TToolkitEditSession.create(sekMD)
  else if (ext = '.js') then
    result := TToolkitEditSession.create(sekJS)
  else if (ext = '.hl7') or (ext = '.msg') then
    result := TToolkitEditSession.create(sekv2)
  else if (ext = '.txt') then
    result := TToolkitEditSession.create(sekText)
  else if (ext = '.xml') then
  begin
    try
      xml := loadXml(bytes);
      try
        exit(examineXml(xml));
      finally
        xml.free;
      end;
    except
      // right, we'll just treat it as plain XML
    end;
    result := TToolkitEditSession.create(sekXml);
  end
  else if (ext = '.json') then
  begin
    try
      json := loadJson(bytes);
      try
        exit(examineJson(json));
      finally
        json.free;
      end;
    except
      // right, we'll just treat it as plain JSON
    end;
    result := TToolkitEditSession.create(sekJson);
  end
  else
  begin
    try
      xml := loadXml(bytes);
      try
        exit(examineXml(xml));
      finally
        xml.free;
      end;
    except
    end;
    try
      json := loadJson(bytes);
      try
        exit(examineJson(json));
      finally
        json.free;
      end;
    except
    end;
    ShowMessage('The file '+filename+' isn''t recognised by this application (unknown extension, and not xml or json)');
  end;
end;

function TToolkitFactory.makeEditor(session : TToolkitEditSession): TToolkitEditor;
var
  store : TStorageService;
begin
  store := FContext.StorageForAddress(session.address);
  case session.kind of
  sekFHIR : result := TFHIREditor.create(FContext{.link}, session, store.link);
  sekIni : result := TIniEditor.create(FContext{.link}, session, store.link);
  sekText : result := TTextEditor.create(FContext{.link}, session, store.link);
  sekXml : result := TXmlEditor.create(FContext{.link}, session, store.link);
  sekJson : result := TJsonEditor.create(FContext{.link}, session, store.link);
  sekHtml : result := THtmlEditor.create(FContext{.link}, session, store.link);
  sekMD : result := TMarkdownEditor.create(FContext{.link}, session, store.link);
  sekJS : result := TJavascriptEditor.create(FContext{.link}, session, store.link);
  sekv2 : result := THL7Editor.create(FContext{.link}, session, store.link);
  else
    raise Exception.create('not supported yet');
  end;
end;

function TToolkitFactory.makeResourceFrame(name: String): TResourceEditorFrame;
begin
  result := nil;
end;


end.
