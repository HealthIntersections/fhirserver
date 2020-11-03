unit FHIR.Toolkit.IniEditor;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, SynEditHighlighter, SynHighlighterIni,
  FHIR.Support.Logging,
  FHIR.Toolkit.Context, FHIR.Toolkit.BaseEditor;

type

  { TIniEditor }

  TIniEditor = class (TBaseEditor)
  protected
    function makeHighlighter : TSynCustomHighlighter; override;
    procedure getNavigationList(navpoints : TStringList); override;
  public
    procedure newContent(); override;
    function FileExtension : String; override;
    procedure validate; override;
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
  updateToContent;
  for i := 0 to FContent.count - 1 do
  begin
    s := FContent[i];
    if (s.StartsWith('[') and s.EndsWith(']')) then
     navpoints.AddObject(s, TObject(i));
  end;
end;

procedure TIniEditor.newContent();
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

procedure TIniEditor.validate;
var
  i : integer;
  s : String;
  t : QWord;
begin
  t := GetTickCount64;
  updateToContent;
  StartValidating;
  try
    for i := 0 to FContent.count - 1 do
    begin
      s := FContent[i];
      checkForEncoding(s, i);
      if (s <> '') and not s.StartsWith(';') then
      begin
        s := s.trim;
        if s.StartsWith('[') then
        begin
          if not s.EndsWith(']') then
            validationError(i+1, 1, 'Improperly terminated section name - doesn''t end with ]');
        end
        else if not s.contains('=') then
          validationWarning(i+1, 1, 'No = found on non-comment line');
      end;
    end;
  finally
    finishValidating;
  end;
  Logging.log('Validate '+describe+' in '+inttostr(GetTickCount64 - t)+'ms');
end;


end.

