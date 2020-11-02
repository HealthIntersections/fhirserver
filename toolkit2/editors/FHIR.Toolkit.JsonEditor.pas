unit FHIR.Toolkit.JsonEditor;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, SynEditHighlighter, SynHighlighterJScript,
  FHIR.Support.Base, FHIR.Support.Json, FHIR.Support.Logging,
  FHIR.Toolkit.Context, FHIR.Toolkit.Store,
  FHIR.Toolkit.TextEditor;

type

  { TJsonEditor }

  TJsonEditor = class (TTextEditor)
  private
    FParser : TJsonParser;
  protected
    function makeHighlighter : TSynCustomHighlighter; override;
    procedure getNavigationList(navpoints : TStringList); override;
  public
    constructor Create(context : TToolkitContext; session : TToolkitEditSession; store : TStorageService); override;
    destructor Destroy; override;

    procedure newContent(); override;
    function FileExtension : String; override;
    procedure validate; override;
  end;


implementation

function TJsonEditor.makeHighlighter: TSynCustomHighlighter;
begin
  Result := TSynJScriptSyn.create(nil);
end;

procedure TJsonEditor.getNavigationList(navpoints: TStringList);
begin
end;

constructor TJsonEditor.Create(context: TToolkitContext; session: TToolkitEditSession; store: TStorageService);
begin
  inherited Create(context, session, store);
  FParser := TJsonParser.create;
end;

destructor TJsonEditor.Destroy;
begin
  FParser.free;
  inherited Destroy;
end;

procedure TJsonEditor.newContent();
begin
  Session.HasBOM := false;
  Session.EndOfLines := PLATFORM_DEFAULT_EOLN;
  Session.Encoding := senUTF8;

  TextEditor.Text := '{'+#13#10+'  "name": "value'+#13#10+'}'+#13#10;
  updateToolbarButtons;
end;

function TJsonEditor.FileExtension: String;
begin
  result := 'Json';
end;

procedure TJsonEditor.validate;
var
  i : integer;
  s : String;
  Json : TJsonNode;
  t : QWord;
begin
  t := GetTickCount64;
  StartValidating;
  try
    for i := 0 to TextEditor.lines.count - 1 do
    begin
      s := TextEditor.lines[i];
      checkForEncoding(s, i);
    end;
    try
      Json := FParser.parseNode(TextEditor.text);
      try
        // todo: any semantic validation?
      finally
        Json.free;
      end;
    except
      on e : EParserException do
      begin
        validationError(e.Line, e.Col, e.message);
      end;
      on e : Exception do
      begin
        validationError(1, 1, 'Error Parsing Json: '+e.message);
      end;
    end;
  finally
    finishValidating;
  end;
  Logging.log('Validate '+describe+' in '+inttostr(GetTickCount64 - t)+'ms');
end;


end.

