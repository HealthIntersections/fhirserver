unit FHIR.Toolkit.Factory;

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  Dialogs,

  FHIR.Support.Base, FHIR.Support.Utilities,

  FHIR.Toolkit.Context, FHIR.Toolkit.Store,
  FHIR.Toolkit.TextEditor, FHIR.Toolkit.IniEditor, FHIR.Toolkit.XmlEditor, FHIR.Toolkit.JsonEditor, FHIR.Toolkit.HtmlEditor,
  FHIR.Toolkit.MarkdownEditor, FHIR.Toolkit.JavascriptEditor,
  SynHighlighterHL7;

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

function TToolkitFactory.examineFile(filename: String; const bytes: TBytes): TToolkitEditSession;
var
  ext : String;
begin
  result := nil;
  ext := Lowercase(ExtractFileExt(filename));
  if (ext = '.ini') then
  begin
    result := TToolkitEditSession.create;
    result.guid := NewGuidId;
    result.kind := sekIni;
  end
  else if (ext = '.xml') then
  begin
    result := TToolkitEditSession.create;
    result.guid := NewGuidId;
    result.kind := sekXml;
  end
  else if (ext = '.json') then
  begin
    result := TToolkitEditSession.create;
    result.guid := NewGuidId;
    result.kind := sekJson;
  end
  else if (ext = '.html') then
  begin
    result := TToolkitEditSession.create;
    result.guid := NewGuidId;
    result.kind := sekHtml;
  end
  else if (ext = '.md') then
  begin
    result := TToolkitEditSession.create;
    result.guid := NewGuidId;
    result.kind := sekMD;
  end
  else if (ext = '.js') then
  begin
    result := TToolkitEditSession.create;
    result.guid := NewGuidId;
    result.kind := sekJS;
  end
  else if (ext = '.txt') then
  begin
    result := TToolkitEditSession.create;
    result.guid := NewGuidId;
    result.kind := sekText;
  end
  else
    ShowMessage('The file '+filename+' isn''t recognised by this application');
end;

function TToolkitFactory.makeEditor(session : TToolkitEditSession): TToolkitEditor;
var
  store : TStorageService;
begin
  store := FContext.StorageForAddress(session.address);
  case session.kind of
  sekIni : result := TIniEditor.create(FContext{.link}, session, store.link);
  sekText : result := TTextEditor.create(FContext{.link}, session, store.link);
  sekXml : result := TXmlEditor.create(FContext{.link}, session, store.link);
  sekJson : result := TJsonEditor.create(FContext{.link}, session, store.link);
  sekHtml : result := THtmlEditor.create(FContext{.link}, session, store.link);
  sekMD : result := TMarkdownEditor.create(FContext{.link}, session, store.link);
  sekJS : result := TJavascriptEditor.create(FContext{.link}, session, store.link);
  else
    raise Exception.create('not supported yet');
  end;
end;


end.
