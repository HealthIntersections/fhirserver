unit FHIR.Toolkit.XmlEditor;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, SynEditHighlighter, SynHighlighterXml,
  FHIR.Support.Base, FHIR.Support.MXml, FHIR.Support.Logging,
  FHIR.Toolkit.Context, FHIR.Toolkit.Store,
  FHIR.Toolkit.BaseEditor;

type

  { TXmlEditor }

  TXmlEditor = class (TBaseEditor)
  private
    FParser : TMXmlParser;
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

function TXmlEditor.makeHighlighter: TSynCustomHighlighter;
begin
  Result := TSynXmlSyn.create(nil);
end;

procedure TXmlEditor.getNavigationList(navpoints: TStringList);
begin
end;

constructor TXmlEditor.Create(context: TToolkitContext; session: TToolkitEditSession; store: TStorageService);
begin
  inherited Create(context, session, store);
  FParser := TMXmlParser.create;
end;

destructor TXmlEditor.Destroy;
begin
  FParser.free;
  inherited Destroy;
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

procedure TXmlEditor.validate;
var
  i : integer;
  s : String;
  xml : TMXmlDocument;
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
    try
      xml := FParser.parse(FContent.text, [xpResolveNamespaces]);
      try
        // todo: any semantic validation?
      finally
        xml.free;
      end;
    except
      on e : EParserException do
      begin
        validationError(e.Line, e.Col, e.message);
      end;
      on e : Exception do
      begin
        validationError(1, 1, 'Error Parsing XML: '+e.message);
      end;
    end;
  finally
    finishValidating;
  end;
  Logging.log('Validate '+describe+' in '+inttostr(GetTickCount64 - t)+'ms');
end;


end.

