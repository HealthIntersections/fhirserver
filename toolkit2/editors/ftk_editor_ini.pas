unit ftk_editor_ini;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, SynEditHighlighter, SynHighlighterIni,
  fsl_logging, fsl_stream,
  ftk_context, ftk_editor_base;

type

  { TIniEditor }

  TIniEditor = class (TBaseEditor)
  protected
    function makeHighlighter : TSynCustomHighlighter; override;
    procedure getNavigationList(navpoints : TStringList); override;
  public
    procedure newContent(); override;
    function FileExtension : String; override;
    procedure validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList); override;
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

procedure TIniEditor.validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList);
var
  i : integer;
  s : String;
  t : QWord;
  section : String;
begin
  updateToContent;
  t := StartValidating;
  try
    for i := 0 to FContent.count - 1 do
    begin
      s := FContent[i];
      if (validate) then
        checkForEncoding(s, i);
      if (s <> '') and not s.StartsWith(';') then
      begin
        s := s.trim;
        if s.StartsWith('[') then
        begin
          if s.EndsWith(']') then
            section := s.Substring(1, length(s)-2)
          else if validate then
            validationError(TSourceLocation.Create(i, 0), 'Improperly terminated section name - doesn''t end with ]');
        end
        else if (validate) and (not s.contains('=')) then
          validationWarning(TSourceLocation.Create(i, 0), 'No = found on non-comment line');
      end;
      if (i = cursor.line) then
        inspection.AddPair('Section', section);
    end;
  finally
    finishValidating(validate, t);
  end;
end;


end.

