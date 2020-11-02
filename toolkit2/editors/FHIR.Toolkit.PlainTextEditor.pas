unit FHIR.Toolkit.PlainTextEditor;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  FHIR.Support.Logging,
  FHIR.Toolkit.Context, FHIR.Toolkit.TextEditor;

type

  { TPlainTextEditor }

  TPlainTextEditor = class (TTextEditor)
  protected
    procedure getNavigationList(navpoints : TStringList); override;
  public
    procedure newContent(); override;
    function FileExtension : String; override;
    procedure validate; override;
  end;


implementation

procedure TPlainTextEditor.getNavigationList(navpoints: TStringList);
begin
end;

procedure TPlainTextEditor.newContent();
begin
  Session.HasBOM := false;
  Session.EndOfLines := slCRLF;
  Session.Encoding := senASCII;

  TextEditor.Text := 'A text file'+#13#10;
  updateToolbarButtons;
end;

function TPlainTextEditor.FileExtension: String;
begin
  result := 'txt';
end;

procedure TPlainTextEditor.validate;
var
  i : integer;
  s : String;
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
  finally
    finishValidating;
  end;
  Logging.log('Validate '+describe+' in '+inttostr(GetTickCount64 - t)+'ms');
end;


end.

