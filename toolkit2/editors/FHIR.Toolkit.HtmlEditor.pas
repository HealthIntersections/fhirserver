unit FHIR.Toolkit.HtmlEditor;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, SynEditHighlighter, SynHighlighterXml,
  FHIR.Support.Base, FHIR.Support.Html, FHIR.Support.Logging,
  FHIR.Toolkit.Context, FHIR.Toolkit.Store,
  FHIR.Toolkit.TextEditor;

type

  { THtmlEditor }

  THtmlEditor = class (TTextEditor)
  protected
    function makeHighlighter : TSynCustomHighlighter; override;
    procedure getNavigationList(navpoints : TStringList); override;
  public
    constructor Create(context : TToolkitContext; session : TToolkitEditSession; store : TStorageService); override;
    destructor Destroy; override;

    procedure newContent(); override;
    function FileExtension : String; override;
    procedure validate; override;

    function hasPages : boolean; override;
  end;


implementation

function THtmlEditor.makeHighlighter: TSynCustomHighlighter;
begin
  Result := TSynHtmlSyn.create(nil);
end;

procedure THtmlEditor.getNavigationList(navpoints: TStringList);
begin
end;

constructor THtmlEditor.Create(context: TToolkitContext; session: TToolkitEditSession; store: TStorageService);
begin
  inherited Create(context, session, store);
  FParser := TMHtmlParser.create;
end;

destructor THtmlEditor.Destroy;
begin
  FParser.free;
  inherited Destroy;
end;

procedure THtmlEditor.newContent();
begin
  Session.HasBOM := false;
  Session.EndOfLines := PLATFORM_DEFAULT_EOLN;
  Session.Encoding := senUTF8;

  TextEditor.Text := '<html>'+#13#10+'<head></head>'+#13#10+'<body></body>'+#13#10+'</html>'+#13#10;
  updateToolbarButtons;
end;

function THtmlEditor.FileExtension: String;
begin
  result := 'html';
end;

procedure THtmlEditor.validate;
var
  i : integer;
  s : String;
  //Html : TMHtmlDocument;
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
    //try
    //  Html := FParser.parse(TextEditor.text, [xpResolveNamespaces]);
    //  try
    //    // todo: any semantic validation?
    //  finally
    //    Html.free;
    //  end;
    //except
    //  on e : EParserException do
    //  begin
    //    validationError(e.Line, e.Col, e.message);
    //  end;
    //  on e : Exception do
    //  begin
    //    validationError(1, 1, 'Error Parsing Html: '+e.message);
    //  end;
    //end;
  finally
    finishValidating;
  end;
  Logging.log('Validate '+describe+' in '+inttostr(GetTickCount64 - t)+'ms');
end;

function THtmlEditor.hasPages: boolean;
begin
  Result := true;
end;


end.

