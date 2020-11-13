unit ftk_editor_js;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, SynEditHighlighter, SynHighlighterJscript,
  fsl_base, fsl_logging, fsl_stream,
  ftk_context, ftk_store,
  ftk_editor_base;

type

  { TJavascriptEditor }

  TJavascriptEditor = class (TBaseEditor)
  protected
    function makeHighlighter : TSynCustomHighlighter; override;
    procedure getNavigationList(navpoints : TStringList); override;
  public
    constructor Create(context : TToolkitContext; session : TToolkitEditSession; store : TStorageService); override;
    destructor Destroy; override;

    procedure newContent(); override;
    function FileExtension : String; override;
    procedure validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList); override;
  end;


implementation

function TJavascriptEditor.makeHighlighter: TSynCustomHighlighter;
begin
  Result := TSynJscriptSyn.create(nil);
end;

procedure TJavascriptEditor.getNavigationList(navpoints: TStringList);
begin
end;

constructor TJavascriptEditor.Create(context: TToolkitContext; session: TToolkitEditSession; store: TStorageService);
begin
  inherited Create(context, session, store);
end;

destructor TJavascriptEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TJavascriptEditor.newContent();
begin
  Session.HasBOM := false;
  Session.EndOfLines := PLATFORM_DEFAULT_EOLN;
  Session.Encoding := senUTF8;

  TextEditor.Text := 'function test() {'+#13#10+'  name = "value";'+#13#10+'}'+#13#10;
  updateToolbarButtons;
end;

function TJavascriptEditor.FileExtension: String;
begin
  result := 'Javascript';
end;

procedure TJavascriptEditor.validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList);
var
  i : integer;
  s : String;
  t : QWord;
begin
  updateToContent;
  t := StartValidating;
  try
    if (validate) then
    begin
      for i := 0 to FContent.count - 1 do
      begin
        s := TextEditor.lines[i];
        checkForEncoding(s, i);
      end;
    end;
  finally
    finishValidating(validate, t);
  end;
end;


end.

