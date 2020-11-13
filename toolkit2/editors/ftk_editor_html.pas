unit ftk_editor_html;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Controls,
  SynEditHighlighter, SynHighlighterHtml, HTMLView,
  fsl_base, fsl_xml, fsl_logging, fsl_stream, fsl_http,
  ftk_context, ftk_store,
  ftk_editor_base;

type

  { THtmlEditor }

  THtmlEditor = class (TBaseEditor)
  private
    FHtmlViewer : THtmlViewer;
    FParser : TMXmlParser;
    FXml : TMXmlDocument;
  protected
    function makeHighlighter : TSynCustomHighlighter; override;
    procedure getNavigationList(navpoints : TStringList); override;
    procedure ContentChanged; override;
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

function THtmlEditor.makeHighlighter: TSynCustomHighlighter;
begin
  Result := TSynHtmlSyn.create(nil);
end;

procedure listHeadings(navpoints: TStringList; element : TMXmlElement);
var
  e : TMXmlElement;
begin
  if (element.Name.ToLower = 'h1') or (element.Name.ToLower = 'h2') then
    navpoints.AddObject(element.Name+' '+element.allText, TObject(element.Start.line));
  for e in element.Children do
    listHeadings(navpoints, e);
end;

procedure THtmlEditor.getNavigationList(navpoints: TStringList);
begin
  if (FXml = nil) then
  try
    FXml := FParser.parse(FContent.text, [xpResolveNamespaces, xpHTMLEntities]);
  except
  end;
  if (FXml <> nil) then
  begin
    listHeadings(navPoints, FXml);
  end;
end;

procedure THtmlEditor.makeDesigner;
begin
  inherited makeDesigner;
  FHtmlViewer := THtmlViewer.create(FDesignerPanelWork);
  FHtmlViewer.parent := FDesignerPanelWork;
  FHtmlViewer.align := alClient;
end;

constructor THtmlEditor.Create(context: TToolkitContext; session: TToolkitEditSession; store: TStorageService);
begin
  inherited Create(context, session, store);
  FParser := TMXmlParser.create;
end;

destructor THtmlEditor.Destroy;
begin
  FParser.free;
  FXml.free;
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

procedure THtmlEditor.validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList);
var
  i : integer;
  s : String;
  t : QWord;
  path : TFslList<TMXmlNamedNode>;
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
     FXml.Free;
     FXml := nil;
     try
       FXml := FParser.parse(FContent.text, [xpResolveNamespaces, xpHTMLEntities]);
       path := FXml.findLocation(cursor);
       try
         inspection.AddPair('Path', FXml.describePath(path));
       finally
         path.free;
       end;
     except
       on e : EParserException do
       begin
         validationError(e.Location, e.message);
       end;
       on e : Exception do
       begin
         validationError(TSourceLocation.CreateNull, 'Error Parsing XHTML: '+e.message);
       end;
     end;
  finally
    finishValidating(validate, t);
  end;
end;

function THtmlEditor.hasDesigner : boolean;
begin
  Result := true;
end;

procedure THtmlEditor.updateDesigner;
begin
  FHtmlViewer.LoadFromString(FContent.Text);
end;

procedure THtmlEditor.ContentChanged;
begin
  FXml.Free;
  FXml := nil;
end;

end.

