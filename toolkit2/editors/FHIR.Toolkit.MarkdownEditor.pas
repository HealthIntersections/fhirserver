unit FHIR.Toolkit.MarkdownEditor;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Controls,
  SynEditHighlighter, SynHighlighterHtml, HTMLView,
  MarkdownProcessor,
  FHIR.Support.Base, FHIR.Base.XHtml, FHIR.Support.Logging,
  FHIR.Toolkit.Context, FHIR.Toolkit.Store,
  FHIR.Toolkit.BaseEditor;

type

  { TMarkdownEditor }

  TMarkdownEditor = class (TBaseEditor)
  private
    FHtml : THtmlViewer;
  protected
    function makeHighlighter : TSynCustomHighlighter; override;
    procedure getNavigationList(navpoints : TStringList); override;
  public
    constructor Create(context : TToolkitContext; session : TToolkitEditSession; store : TStorageService); override;
    destructor Destroy; override;

    procedure newContent(); override;
    function FileExtension : String; override;
    procedure validate; override;

    function hasDesigner : boolean; override;
    procedure makeDesigner; override;
    procedure showDesigner; override;
  end;


implementation

function TMarkdownEditor.makeHighlighter: TSynCustomHighlighter;
begin
  Result := TSynHtmlSyn.create(nil);
end;

procedure TMarkdownEditor.getNavigationList(navpoints: TStringList);
begin
end;

procedure TMarkdownEditor.makeDesigner;
begin
  inherited makeDesigner;
  FHtml := THtmlViewer.create(FDesignerPanelWork);
  FHtml.parent := FDesignerPanelWork;
  FHtml.align := alClient;
end;

constructor TMarkdownEditor.Create(context: TToolkitContext; session: TToolkitEditSession; store: TStorageService);
begin
  inherited Create(context, session, store);
//  FParser := TMHtmlParser.create;
end;

destructor TMarkdownEditor.Destroy;
begin
//  FParser.free;
  inherited Destroy;
end;

procedure TMarkdownEditor.newContent();
begin
  Session.HasBOM := false;
  Session.EndOfLines := PLATFORM_DEFAULT_EOLN;
  Session.Encoding := senUTF8;

  TextEditor.Text := '# Heading'+#13#10+'Some Markdown content'+#13#10+''+#13#10+'* Item 1'+#13#10+'* Item 2'+#13#10+''+#13#10;
  updateToolbarButtons;
end;

function TMarkdownEditor.FileExtension: String;
begin
  result := 'md';
end;

procedure TMarkdownEditor.validate;
var
  i : integer;
  s : String;
  //Html : TMHtmlDocument;
  t : QWord;
begin
  t := GetTickCount64;
  updateToContent;
  StartValidating;
  try
    for i := 0 to FContent.count - 1 do
    begin
      s := TextEditor.lines[i];
      checkForEncoding(s, i);
    end;
  finally
    finishValidating;
  end;
  Logging.log('Validate '+describe+' in '+inttostr(GetTickCount64 - t)+'ms');
end;

function TMarkdownEditor.hasDesigner : boolean;
begin
  Result := true;
end;

procedure TMarkdownEditor.showDesigner;
var
  html : String;
  proc : TMarkdownProcessor;
begin
  inherited showDesigner;

  proc := TMarkdownProcessor.createDialect(mdCommonMark); // or flavor of your choice
  try
    proc.unsafe := false;
    html := proc.process(FContent.text);
  finally
    proc.free;
  end;
  FHtml.LoadFromString(html);
end;


end.

