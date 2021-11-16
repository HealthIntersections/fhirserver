unit dlg_clipboard_process;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ClipBrd,
  fsl_utilities, fsl_fpc,
  fui_lcl_utilities;

const
  ppAsIs = 0;
  ppHTMLEscaped = 1;
  ppXMLTextEscaped = 2;
  ppXMLAttrEscaped = 3;
  ppJsonEscaped = 4;
  ppJavaEscaped = 5;
  ppPascalEscaped = 6;
  ppRawANSI = 7;
  ppRawUTF8 = 8;
  ppRawUTF16L = 9;
  ppRawUTF16B = 10;

type

  { TTextPasteProcessorForm }

  TTextPasteProcessorForm = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    btnProcess: TButton;
    lbProcesses: TListBox;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure btnProcessClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbProcessesClick(Sender: TObject);
  private
    FRawText : String;
    FProcessedText : String;
  public
    property Text : String read FProcessedText;
  end;

var
  TextPasteProcessorForm: TTextPasteProcessorForm;

implementation

{$R *.lfm}

function javaEscape(s : String):String;
var
  ch : UnicodeChar;
begin
  result := '"';
  for ch in unicodeChars(s) do
    if ord(ch) > 127 then
      result := result + '\u'+IntToHex(ord(ch), 4)
    else if (ch = #9) then
      result := result + '\t'
    else if (ch = #10) then
      result := result + '\n'
    else if (ch = #13) then
      result := result + '\r'
    else if (ch = '"') or (ch = '\') then
      result := result + '\'+ch
    else
      result := result + ch;
  result := result + '"';
end;

function pascalEscape(s : String):String;
var
  ch : UnicodeChar;
  quoted : boolean;
  procedure quote;
  begin
    if not quoted then
    begin
      result := result + '''';
      quoted := true;
    end;
  end;
  procedure unquote;
  begin
    if quoted then
    begin
      result := result + '''';
      quoted := false;
    end;
  end;
begin
  result := '';
  quoted := false;
  quote;
  for ch in unicodeChars(s) do
  begin
    if ord(ch) > 127 then
    begin
      unquote;
      result := result + '#$'+IntToHex(ord(ch), 4)
    end
    else if (ch = #9) then
    begin
      unquote;
      result := result + '#09'
    end
    else if (ch = #10) then
    begin
      unquote;
      result := result + '#10'
    end
    else if (ch = #13) then
    begin
      unquote;
      result := result + '#10'
    end
    else if (ch = '''') then
    begin
      quote;
      result := result + ''''
    end
    else
    begin
      quote;
      result := result + ch;
    end;
  end;
  unquote;
  result := result + '''';
end;

function HexEncode(bytes : TBytes) : String;
var
  b : byte;
begin
  result := '';
  for b in bytes do
    result := result + ' '+inttohex(b, 2);
  result := result.trim;
end;

{ TTextPasteProcessorForm }

procedure TTextPasteProcessorForm.lbProcessesClick(Sender: TObject);
begin
  case lbProcesses.ItemIndex of
    ppAsIs: FProcessedText := FRawText;
    ppHTMLEscaped: FProcessedText := FormatTextToXML(FRawText, xmlText);
    ppXMLTextEscaped: FProcessedText := FormatTextToXML(FRawText, xmlText);
    ppXMLAttrEscaped: FProcessedText := FormatTextToXML(FRawText, xmlAttribute);
    ppJsonEscaped: FProcessedText := jsonEscape(FRawText, true);
    ppJavaEscaped: FProcessedText := javaEscape(FRawText);
    ppPascalEscaped: FProcessedText := pascalEscape(FRawText);
    ppRawANSI: FProcessedText := HexEncode(TEncoding.ANSI.GetBytes(FRawText));
    ppRawUTF8: FProcessedText := HexEncode(TEncoding.ANSI.GetBytes(FRawText));
    ppRawUTF16L: FProcessedText := HexEncode(TEncoding.Unicode.GetBytes(FRawText));
    ppRawUTF16B: FProcessedText := HexEncode(TEncoding.BigEndianUnicode.GetBytes(FRawText));
  end;
  Memo1.Text := FProcessedText;
  btnOk.Enabled := FProcessedText <> '';
end;

procedure TTextPasteProcessorForm.FormCreate(Sender: TObject);
begin
  setForOS(btnOk, btnCancel);
  lbProcesses.ItemIndex := 0;
  btnProcessClick(nil);
end;

procedure TTextPasteProcessorForm.btnProcessClick(Sender: TObject);
var
  clip : TClipboard;
begin
  clip := TClipboard.create;
  try
    FRawText := clip.AsText;
  finally
    clip.free;
  end;
  lbProcessesClick(nil);
end;

end.

