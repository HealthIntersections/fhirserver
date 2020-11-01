unit FHIR.Toolkit.Factory;

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  Dialogs,

  FHIR.Support.Base, FHIR.Support.Utilities,

  FHIR.Toolkit.Context,
  FHIR.Toolkit.TextEditor, FHIR.Toolkit.IniEditor;

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

function TToolkitFactory.makeEditor(session : TToolkitEditSession): TToolkitEditor;
begin
  case session.kind of
    sekIni : result := TIniEditor.create(FContext{.link}, session);
  else
    raise Exception.create('not supported yet');
  end;
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
  else
    ShowMessage('The file '+filename+' isn''t recognised by this application');
end;


end.
