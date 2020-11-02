unit FHIR.Toolkit.XmlEditor;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, SynEditHighlighter, SynHighlighterXml,
  FHIR.Support.MXml,
  FHIR.Toolkit.Context, FHIR.Toolkit.TextEditor;

type

  { TXmlEditor }

  TXmlEditor = class (TTextEditor)
  private
    FParser : TMXmlParser;
  protected
    function makeHighlighter : TSynCustomHighlighter; override;
    procedure getNavigationList(navpoints : TStringList); override;
  public
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
var
  i : integer;
  s : String;
begin
  for i := 0 to TextEditor.Lines.count - 1 do
  begin
    s := TextEditor.Lines[i];
    if (s.StartsWith('[') and s.EndsWith(']')) then
     navpoints.AddObject(s, TObject(i));
  end;
end;

procedure TXmlEditor.newContent();
begin
  Session.HasBOM := false;
  Session.EndOfLines := slCRLF;
  Session.Encoding := senASCII;

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
  xml : TMxmlParser;
begin
  StartValidating;
  try
    for i := 0 to TextEditor.lines.count - 1 do
    begin
      s := TextEditor.lines[i];
      checkForEncoding(s, i);
    end;
  finally
    finishValidating;
  end;
end;


end.

