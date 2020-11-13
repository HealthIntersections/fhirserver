unit ftk_editor_md;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Controls,
  SynEditHighlighter, SynHighlighterHtml, HTMLView,
  MarkdownProcessor,
  fsl_base, fhir_xhtml, fsl_logging, fsl_stream,
  ftk_context, ftk_store,
  ftk_editor_base;

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
    procedure validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList); override;

    function hasDesigner : boolean; override;
    procedure makeDesigner; override;
    procedure updateDesigner; override;
  end;


implementation

function TMarkdownEditor.makeHighlighter: TSynCustomHighlighter;
begin
  Result := TSynHtmlSyn.create(nil);
end;

procedure TMarkdownEditor.getNavigationList(navpoints: TStringList);
var
  i : integer;
  s : String;
begin
  updateToContent;
  for i := 0 to FContent.count - 1 do
  begin
    s := FContent[i];
    if (s.StartsWith('# ') or s.StartsWith('## ') or s.StartsWith('### ') or s.StartsWith('#### ')) then
      navpoints.AddObject(s, TObject(i))
  end;
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

procedure TMarkdownEditor.validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList);
var
  i : integer;
  s : String;
  //Html : TMHtmlDocument;
  t : QWord;
  s1, s2, s3, s4 : String;
begin
  updateToContent;
  t := StartValidating;
  try
    for i := 0 to FContent.count - 1 do
    begin
      s := TextEditor.lines[i];
      if (validate) then
        checkForEncoding(s, i);
      if (s.StartsWith('# ')) then
      begin
        s1 := s.Substring(2);
        s2 := '';
        s3 := '';
        s4 := '';
      end
      else if (s.StartsWith('## ')) then
      begin
        s2 := s.Substring(3);
        s3 := '';
        s4 := '';
      end
      else if (s.StartsWith('### ')) then
      begin
        s3 := s.Substring(4);
        s4 := '';
      end
      else if (s.StartsWith('#### ')) then
      begin
        s4 := s.Substring(5);
      end;

      if (i = cursor.line) then
      begin
        if s1 <> '' then
          inspection.AddPair('H1', s1);
        if s2 <> '' then
          inspection.AddPair('H2', s2);
        if s3 <> '' then
          inspection.AddPair('H3', s3);
        if s4 <> '' then
          inspection.AddPair('H4', s4);
      end;
    end;
  finally
    finishValidating(validate, t);
  end;
end;

function TMarkdownEditor.hasDesigner : boolean;
begin
  Result := true;
end;

procedure TMarkdownEditor.updateDesigner;
var
  html : String;
  proc : TMarkdownProcessor;
begin
  inherited updateDesigner;

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

