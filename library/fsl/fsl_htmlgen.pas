unit fsl_htmlgen;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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

{$I fhir.inc}

interface

uses
  SysUtils,
  fsl_base, fsl_utilities, fsl_stream, fsl_http, fsl_fpc;

Type

  { THtmlPublisher }

  THtmlPublisher = class (TFslObject)
  private
    FBuilder : TFslStringBuilder;

    FBaseURL: String;
    FLangList : THTTPLanguageList;
    FVersion: String;
    FLogId: String;
    procedure SetLangList(AValue: THTTPLanguageList); 
    procedure escapeText(AStr: String; attr : boolean);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; Override;

    procedure Done;

    procedure Line;
    procedure StartPre;
    procedure endPre;

    procedure Heading(level : integer; text : String);
    procedure StartParagraph;
    procedure EndParagraph;
    procedure AddParagraph(text : String = '');
    procedure AddTextPlain(text : String);
    procedure AddTitle(text : String);
    procedure AddText(text : String; bold, italics : boolean);

    procedure URL(text, url : String; hint : string = '');
    procedure ParaURL(text, url : String);

    procedure StartTable(borders : boolean; clss : String = '');
    procedure StartTableRow;
    procedure StartRow(bgcolor : string = '');
    procedure StartTableCell(span : integer = 1);
    procedure EndTableCell;
    procedure EndTableRow;
    procedure EndTable;
    procedure AddTableCellURL(text, url : String; hint : String = '');
    procedure AddTableCell(text : String; bold : boolean = false);
    procedure AddTableCellHint(text, hint : String);

    procedure AddListItem(text : String);
    procedure StartList(ordered : boolean = false);
    procedure EndList(ordered : boolean = false);
    procedure StartListItem;
    procedure EndListItem;
    procedure StartBlockQuote;
    procedure EndBlockQuote;
    procedure startDiv;
    procedure endDiv;

    procedure break;

    procedure StartForm(method, action : String);
    procedure TextInput(name : String; length : integer = 20); overload;
    procedure TextInput(name, value : String; length : integer = 20); overload;
    procedure TextInput(name, value, text : String; length : integer = 20); overload;
    procedure Memo(name, value, text : String); overload;
    procedure checkbox(name : String; value : boolean; text : String);
    procedure hiddenInput(name, value : String);
    procedure Submit(name : String);
    procedure EndForm;
//    procedure writeXhtml(node : TFhirXHtmlNode);
    procedure Spacer;

    function output : String;
    Property BaseURL : String read FBaseURL write FBaseURL;
    Property LangList : THTTPLanguageList read FLangList write SetLangList;
    Property Version : String read FVersion write FVersion;
    Property LogId : String read FLogId write FLogid;
  end;



implementation

{ THtmlPublisher }


procedure THtmlPublisher.escapeText(AStr: String; attr : boolean);
var
  c : UnicodeChar;
begin
  for c in unicodeChars(aStr) do
  begin
    case c of
      '"':  if attr then FBuilder.append('&quot;') else FBuilder.append('"');
      '''': if attr then FBuilder.append('&apos;') else FBuilder.append('''');
      '&':  FBuilder.append('&amp;');
      '<':  FBuilder.append('&lt;');
      '>':  FBuilder.append('&gt;');
      #32:  FBuilder.append(' ');

      #13, #10:
        if attr then
        begin
          FBuilder.append('&#');
          FBuilder.append(IntToStr(Ord(c)));
          FBuilder.append(';')
        end
        else
          FBuilder.append(char(c));
      else if (ord(c) < 127) and (Ord(c) >= 32) then
        FBuilder.append(char(c))
      else
      begin
        FBuilder.append('&#x');
        FBuilder.append(IntToHex(Ord(c), 2));
        FBuilder.append(';');
      end;
    end;
  end;
end;

procedure THtmlPublisher.AddListItem(text: String);
begin
  StartListItem;
  AddTextPlain(text);
  EndListItem;
end;

procedure THtmlPublisher.AddParagraph(text: String);
begin
  StartParagraph;
  AddTextPlain(text);
  EndParagraph;
end;

procedure THtmlPublisher.AddTableCell(text: String; bold: boolean);
begin
  StartTableCell;
  addtext(text, bold, false);
  EndTableCell;
end;

procedure THtmlPublisher.AddTableCellHint(text, hint: String);
begin
  StartTableCell;
  FBuilder.append('<span title="');
  escapeText(hint, true);
  FBuilder.append('">');
  addtext(text, false, false);
  FBuilder.append('</span>');
  EndTableCell;
end;

procedure THtmlPublisher.AddTableCellURL(text, url: String; hint : String = '');
begin
  StartTableCell;
  self.URL(text, url, hint);
  EndTableCell;
end;

procedure THtmlPublisher.AddText(text: String; bold, italics: boolean);
begin
  if bold then
    FBuilder.append('<b>');
  if italics then
    FBuilder.append('<i>');
  AddTextPlain(text);
  if italics then
    FBuilder.append('</i>');
  if bold then
    FBuilder.append('</b>');
end;

procedure THtmlPublisher.AddTextPlain(text: String);
begin
  escapeText(text, false);
end;

procedure THtmlPublisher.AddTitle(text: String);
begin
  AddText(text, true, false);
end;

procedure THtmlPublisher.break;
begin
  FBuilder.append('<br/>');
end;

procedure THtmlPublisher.checkbox(name : String; value : boolean; text : String);
begin
  FBuilder.append('<input type="checkbox" name="');
  FBuilder.append(name);
  if value then
    FBuilder.append('" checked');
  FBuilder.append(' value="1"/> ');
  FBuilder.append(text);
end;

procedure THtmlPublisher.endDiv;
begin
  FBuilder.append('</div>')
end;

constructor THtmlPublisher.Create();
begin
  inherited Create;
  FBuilder := TFslStringBuilder.create;
end;

destructor THtmlPublisher.Destroy;
begin
  FLangList.free;
  FBuilder.free;
  inherited;
end;

procedure THtmlPublisher.Done;
begin
  FBuilder.append('');
end;

procedure THtmlPublisher.EndBlockQuote;
begin
  FBuilder.append('</blockquote>'#13#10);
end;

procedure THtmlPublisher.EndForm;
begin
  FBuilder.append('</form>'#13#10);
end;

procedure THtmlPublisher.EndList(ordered: boolean);
begin
  if ordered then
    FBuilder.append('</ol>'#13#10)
  else
    FBuilder.append('</ul>'#13#10);
end;

procedure THtmlPublisher.EndListItem;
begin
  FBuilder.append('</li>'#13#10);
end;

procedure THtmlPublisher.EndParagraph;
begin
  FBuilder.append('<p>'#13#10);
end;

procedure THtmlPublisher.endPre;
begin
  FBuilder.append('<pre>'#13#10);
end;

procedure THtmlPublisher.EndTable;
begin
  FBuilder.append('</table>'#13#10);
end;

procedure THtmlPublisher.EndTableCell;
begin
  FBuilder.append('</td>'#13#10);
end;

procedure THtmlPublisher.EndTableRow;
begin
  FBuilder.append('</tr>'#13#10);
end;

procedure THtmlPublisher.Heading(level: integer; text: String);
begin
  FBuilder.append('<h');
  FBuilder.append(inttostr(level));
  FBuilder.append('>');
  AddTextPlain(text);
  FBuilder.append('</h');
  FBuilder.append(inttostr(level));
  FBuilder.append('>');
end;

procedure THtmlPublisher.hiddenInput(name, value: String);
begin
  FBuilder.append('<input type="hidden" name="');
  FBuilder.append(name);
  FBuilder.append('" value="');
  FBuilder.append(value);
  FBuilder.append('"/>');
end;


procedure THtmlPublisher.Line;
begin
  FBuilder.append('<hr/>'#13#10);
end;

procedure THtmlPublisher.Memo(name, value, text: String);
begin
  FBuilder.append(text);
  FBuilder.append('<textArea name="');
  FBuilder.append(name);
  FBuilder.append('">'#13#10);
  FBuilder.append(value);
  FBuilder.append('</textArea>');
end;

function THtmlPublisher.output: String;
begin
  result := FBuilder.toString;
end;

procedure THtmlPublisher.ParaURL(text, url: String);
begin
  StartParagraph;
  self.URL(text, url);
  EndParagraph;
end;

procedure THtmlPublisher.Spacer;
begin
  FBuilder.append('&nbsp;');
end;

procedure THtmlPublisher.StartBlockQuote;
begin
  FBuilder.append('<blockquote>');
end;

procedure THtmlPublisher.startDiv;
begin
  FBuilder.append('<div>')
end;

procedure THtmlPublisher.StartForm(method, action: String);
begin
  FBuilder.append('<form method="');
  FBuilder.append(method);
  FBuilder.append('" action="');
  FBuilder.append(action);
  FBuilder.append('">'#13#10);
end;

procedure THtmlPublisher.StartList(ordered: boolean);
begin
  if ordered then
    FBuilder.append('<ol>')
  else
    FBuilder.append('<ul>');
end;

procedure THtmlPublisher.StartListItem;
begin
  FBuilder.append('<li>');
end;

procedure THtmlPublisher.StartParagraph;
begin
  FBuilder.append('<p>');
end;

procedure THtmlPublisher.StartPre;
begin
  FBuilder.append('<pre>'#13#10);
end;

procedure THtmlPublisher.StartRow(bgcolor : string = '');
begin
  if (bgcolor <> '') then
  begin
    FBuilder.append('<tr style="background-color: ');
    FBuilder.append(bgcolor);
    FBuilder.append('">')
  end
  else
    FBuilder.append('<tr>')
end;

procedure THtmlPublisher.StartTable(borders: boolean; clss : String);
begin
  if clss <> '' then
    clss := ' class="'+clss+'"';
  if borders then
    FBuilder.append('<table border="1"'+clss+'>')
  else
    FBuilder.append('<table border="0"'+clss+'>');
end;

procedure THtmlPublisher.StartTableCell(span: integer);
begin
  if (span <> 1) then
  begin
    FBuilder.append('<td colspan="');
    FBuilder.append(inttostr(span));
    FBuilder.append('">')
  end
  else
   FBuilder.append('<td>')
end;

procedure THtmlPublisher.StartTableRow;
begin
  FBuilder.append('<tr>')
end;

procedure THtmlPublisher.Submit(name: String);
begin
  FBuilder.append('<input type="submit" value="');
  FBuilder.append(name);
  FBuilder.append('"/>');
end;

procedure THtmlPublisher.TextInput(name, value: String; length: integer);
begin
  FBuilder.append('<input type="text" name="');
  FBuilder.append(name);
  FBuilder.append('" value="');
  FBuilder.append(value);
  FBuilder.append('" size="');
  FBuilder.append(inttostr(length));
  FBuilder.append('"/>');
end;

procedure THtmlPublisher.TextInput(name: String; length: integer);
begin
  FBuilder.append('<input type="text" name="');
  FBuilder.append(name);
  FBuilder.append('" size="');
  FBuilder.append(inttostr(length));
  FBuilder.append('"/>');
end;

procedure THtmlPublisher.URL(text, url: String; hint: string);
begin
  if (hint <> '') then
  begin
    FBuilder.append('<a href="');
    FBuilder.append(url);
    FBuilder.append('" title="');
    escapeText(hint, true);
    FBuilder.append('">')
  end
  else
  begin
    FBuilder.append('<a href="');
    FBuilder.append(url);
    FBuilder.append('">');
  end;
  AddTextPlain(text);
  FBuilder.append('</a>');
end;

procedure THtmlPublisher.TextInput(name, value, text: String; length: integer);
begin
  FBuilder.append('<input type="text" name="');
  FBuilder.append(name);
  FBuilder.append('" value="');
  FBuilder.append(value);
  FBuilder.append('" size="');
  FBuilder.append(inttostr(length));
  FBuilder.append('"/> ');
  FBuilder.append(text);
end;

procedure THtmlPublisher.SetLangList(AValue: THTTPLanguageList);
begin
  FLangList.free;
  FLangList := AValue;
end;

function THtmlPublisher.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FBuilder.memSize());
  inc(result, (FBaseURL.length * sizeof(char)) + 12);
  inc(result, FLangList.sizeInBytes(magic));
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, (FLogId.length * sizeof(char)) + 12);
end;

end.
