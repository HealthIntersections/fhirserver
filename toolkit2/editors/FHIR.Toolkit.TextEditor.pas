unit FHIR.Toolkit.TextEditor;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  Controls, ComCtrls, Menus, SynEdit, SynEditHighlighter,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.Logging,
  FHIR.Toolkit.Context, FHIR.Toolkit.BaseEditor;

type

  { TTextEditor }

  TTextEditor = class (TBaseEditor)
  protected
    procedure getNavigationList(navpoints : TStringList); override;
  public
    procedure newContent(); override;
    function FileExtension : String; override;
    procedure validate(ts : TStringList; line, col : integer); override;
  end;


implementation


procedure TTextEditor.getNavigationList(navpoints: TStringList);
begin
end;

procedure TTextEditor.newContent();
begin
  Session.HasBOM := false;
  Session.EndOfLines := slCRLF;
  Session.Encoding := senASCII;

  TextEditor.Text := 'A text file'+#13#10;
  updateToolbarButtons;
end;

function TTextEditor.FileExtension: String;
begin
  result := 'txt';
end;

procedure TTextEditor.validate(ts : TStringList; line, col : integer);
var
  i : integer;
  s : String;
  t : QWord;
begin
  t := GetTickCount64;
  updateToContent;
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

