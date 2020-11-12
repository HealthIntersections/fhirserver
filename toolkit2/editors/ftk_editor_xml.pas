unit ftk_editor_xml;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, SynEditHighlighter, SynHighlighterXml,
  fsl_base, fsl_xml, fsl_logging, fsl_stream,
  ftk_context, ftk_store,
  ftk_editor_base;

type

  { TXmlEditor }

  TXmlEditor = class (TBaseEditor)
  private
    FParser : TMXmlParser;
    FXml : TMXmlDocument;
    procedure DoMnuPretty(sender : TObject);
    procedure DoMnuCondense(sender : TObject);
  protected
    function makeHighlighter : TSynCustomHighlighter; override;
    procedure getNavigationList(navpoints : TStringList); override;
    function hasFormatCommands: boolean; override;
    procedure makeTextTab; override;
  public
    constructor Create(context : TToolkitContext; session : TToolkitEditSession; store : TStorageService); override;
    destructor Destroy; override;

    procedure ContentChanged; override;
    procedure newContent(); override;
    function FileExtension : String; override;
    procedure validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList); override;
  end;


implementation

procedure TXmlEditor.DoMnuPretty(sender: TObject);
var
  x : TMXmlDocument;
begin
  x := TMXmlParser.parse(TextEditor.Text, [xpDropWhitespace]);
  try
    SetContentUndoable(x.ToXml(true, false));
  finally
    x.Free;
  end;
end;

procedure TXmlEditor.DoMnuCondense(sender: TObject);
var
  x : TMXmlDocument;
begin
  x := TMXmlParser.parse(TextEditor.Text, [xpDropWhitespace]);
  try
    SetContentUndoable(x.ToXml(false, false));
  finally
    x.Free;
  end;
end;

function TXmlEditor.makeHighlighter: TSynCustomHighlighter;
begin
  Result := TSynXmlSyn.create(nil);
end;

procedure TXmlEditor.getNavigationList(navpoints: TStringList);
var
  de, e : TMXmlElement;
  c : integer;
begin
  if (FXml = nil) then
  try
    FXml := FParser.parse(FContent.text, [xpResolveNamespaces]);
  except
  end;
  if FXml <> nil then
  begin
    de := FXml.docElement;
    if (de.HasChildren) then
    begin
      c := 0;
      for e in de.children do
        if e.NodeType = ntElement then
          inc(c);
      if (c < 20) then
      begin
        for e in de.children do
        begin
          if e.NodeType = ntElement then
          begin
            if (e.HasAttribute['name']) then
              navpoints.AddObject(e.Name+' ('+e.attribute['name']+')', TObject(e.Start.line))
            else if (e.HasAttribute['id']) then
              navpoints.AddObject(e.Name+' ('+e.attribute['id']+')', TObject(e.Start.line))
            else
              navpoints.AddObject(e.Name, TObject(e.Start.line));
          end;
        end;
      end;
    end;
  end;
end;

function TXmlEditor.hasFormatCommands: boolean;
begin
  Result := true;
end;

procedure TXmlEditor.makeTextTab;
begin
  inherited makeTextTab;
  makeSubAction(actFormat, 'Pretty', 88, 0, DoMnuPretty);
  makeSubAction(actFormat, 'Condensed', 87, 0, DoMnuCondense);
end;

constructor TXmlEditor.Create(context: TToolkitContext; session: TToolkitEditSession; store: TStorageService);
begin
  inherited Create(context, session, store);
  FParser := TMXmlParser.create;
end;

destructor TXmlEditor.Destroy;
begin
  FParser.free;
  FXml.Free;
  inherited Destroy;
end;

procedure TXmlEditor.ContentChanged;
begin
  FXml.Free;
  FXml := nil;
end;

procedure TXmlEditor.newContent();
begin
  Session.HasBOM := false;
  Session.EndOfLines := PLATFORM_DEFAULT_EOLN;
  Session.Encoding := senUTF8;

  TextEditor.Text := '<xml>'+#13#10+'</xml>'+#13#10;
  updateToolbarButtons;
end;

function TXmlEditor.FileExtension: String;
begin
  result := 'xml';
end;

procedure TXmlEditor.validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList);
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
      FXml := FParser.parse(FContent.text, [xpResolveNamespaces]);
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
        validationError(TSourceLocation.CreateNull, 'Error Parsing XML: '+e.message);
      end;
    end;
  finally
    finishValidating(validate, t);
  end;
end;


end.

