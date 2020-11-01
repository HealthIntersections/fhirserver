unit FHIR.Toolkit.IniEditor;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, SynEditHighlighter, SynHighlighterIni,
  FHIR.Toolkit.Context, FHIR.Toolkit.TextEditor;

type

  { TIniEditor }

  TIniEditor = class (TTextEditor)
  protected
    function makeHighlighter : TSynCustomHighlighter; override;
    procedure getNavigationList(navpoints : TStringList); override;
  public
    procedure newContent(); override;
    function FileExtension : String; override;
  end;


implementation

function TIniEditor.makeHighlighter: TSynCustomHighlighter;
begin
  Result := TSynIniSyn.create(nil);
end;

procedure TIniEditor.getNavigationList(navpoints: TStringList);
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

procedure TIniEditor.newContent;
begin
  Session.HasBOM := false;
  Session.EndOfLines := slCRLF;
  Session.Encoding := senASCII;

  TextEditor.Text := '[Section]'+#13#10+'; comments here'+#13#10+'name=value'+#13#10;
  updateToolbarButtons;
end;

function TIniEditor.FileExtension: String;
begin
  result := 'ini';
end;


end.

