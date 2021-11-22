unit dlg_clipboard_process;

{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ClipBrd,
  fsl_utilities, fsl_fpc,
  fui_lcl_utilities;

const
  ppAsIs = 0;
  ppHTMLEscaped = ppAsIs + 1;
  ppXMLTextEscaped = ppHTMLEscaped + 1;
  ppXMLAttrEscaped = ppXMLTextEscaped + 1;
  ppXMLUnescaped = ppXMLAttrEscaped+ 1;
  ppJsonEscaped = ppXMLUnescaped + 1;
  ppJavaEscaped = ppJsonEscaped + 1;
  ppPascalEscaped = ppJavaEscaped + 1;
  ppHttpUrlEncoded = ppPascalEscaped + 1;
  ppHttpUrlDecoded = ppHttpUrlEncoded + 1;
  ppBase64Encoded = ppHttpUrlDecoded + 1;
  ppBase64Decoded = ppBase64Encoded + 1;
  ppBase64UrlEncoded = ppBase64Decoded + 1;
  ppBase64UrlDecoded = ppBase64UrlEncoded + 1;
  ppAnsiOnly = ppBase64UrlDecoded + 1;
  ppRawANSI = ppAnsiOnly + 1;
  ppRawUTF8 = ppRawANSI + 1;
  ppRawUTF16L = ppRawUTF8 + 1;
  ppRawUTF16B = ppRawUTF16L + 1;

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

function StripUnicode(s : String):String;
var
  ch : UnicodeChar;
begin
  result := '';
  for ch in unicodeChars(s) do
    if ord(ch) > 127 then
      result := result + '?'
    else
      result := result + ch;
end;


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
    ppXMLUnescaped: FProcessedText := DecodeXML(FRawText);
    ppJsonEscaped: FProcessedText := jsonEscape(FRawText, true);
    ppJavaEscaped: FProcessedText := javaEscape(FRawText);
    ppPascalEscaped: FProcessedText := pascalEscape(FRawText);
    ppHttpUrlEncoded: FProcessedText := EncodePercent(FRawText);
    ppHttpUrlDecoded: FProcessedText := DecodePercent(FRawText);
    ppBase64Encoded: FProcessedText := EncodeBase64(TEncoding.UTF8.GetBytes(FRawText));
    ppBase64Decoded: FProcessedText := TEncoding.UTF8.GetString(DecodeBase64(FRawText));
    ppBase64UrlEncoded: FProcessedText := EncodeBase64Url(TEncoding.UTF8.GetBytes(FRawText));
    ppBase64UrlDecoded: FProcessedText := TEncoding.UTF8.GetString(DecodeBase64Url(FRawText));
    ppAnsiOnly : FProcessedText := StripUnicode(FRawText);
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

