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
  fsl_base, fsl_utilities, fsl_stream, fsl_http;

Type

  { THtmlPublisher }

  THtmlPublisher = class (TFslObject)
  private
    FSource : String;
    FCursor : Integer;

    FBaseURL: String;
    FLangList : THTTPLanguageList;
    FVersion: String;
    FLogId: String;
    procedure SetLangList(AValue: THTTPLanguageList);
    procedure doAppend(s : String);
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
  doAppend('<span title="'+FormatTextToXML(hint, xmlAttribute)+'">');
  addtext(text, false, false);
  doAppend('</span>');
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
    doAppend('<b>');
  if italics then
    doAppend('<i>');
  AddTextPlain(text);
  if italics then
    doAppend('</i>');
  if bold then
    doAppend('</b>');
end;

procedure THtmlPublisher.AddTextPlain(text: String);
begin
  doAppend(FormatTextToXml(text, xmlText));
end;

procedure THtmlPublisher.AddTitle(text: String);
begin
  AddText(text, true, false);
end;

procedure THtmlPublisher.break;
begin
  doAppend('<br/>');
end;

procedure THtmlPublisher.checkbox(name : String; value : boolean; text : String);
begin
  if value then
    doAppend('<input type="checkbox" name="'+name+'" checked value="1"/> '+text)
  else
    doAppend('<input type="checkbox" name="'+name+'" value="1"/> '+text);
end;

procedure THtmlPublisher.endDiv;
begin
  doAppend('</div>')
end;

constructor THtmlPublisher.Create();
begin
  inherited Create;
  FSource := '';
  FCursor := 0;
end;

destructor THtmlPublisher.Destroy;
begin
  FLangList.free;
  inherited;
end;

procedure THtmlPublisher.Done;
begin
  doAppend('');
end;

procedure THtmlPublisher.EndBlockQuote;
begin
  doAppend('</blockquote>'#13#10);
end;

procedure THtmlPublisher.EndForm;
begin
  doAppend('</form>'#13#10);
end;

procedure THtmlPublisher.EndList(ordered: boolean);
begin
  if ordered then
    doAppend('</ol>'#13#10)
  else
    doAppend('</ul>'#13#10);
end;

procedure THtmlPublisher.EndListItem;
begin
  doAppend('</li>'#13#10);
end;

procedure THtmlPublisher.EndParagraph;
begin
  doAppend('<p>'#13#10);
end;

procedure THtmlPublisher.endPre;
begin
  doAppend('<pre>'#13#10);
end;

procedure THtmlPublisher.EndTable;
begin
  doAppend('</table>'#13#10);
end;

procedure THtmlPublisher.EndTableCell;
begin
  doAppend('</td>'#13#10);
end;

procedure THtmlPublisher.EndTableRow;
begin
  doAppend('</tr>'#13#10);
end;

procedure THtmlPublisher.Heading(level: integer; text: String);
begin
  doAppend('<h'+inttostr(level)+'>');
  AddTextPlain(text);
  doAppend('</h'+inttostr(level)+'>');
end;

procedure THtmlPublisher.hiddenInput(name, value: String);
begin
  doAppend('<input type="hidden" name="'+name+'" value="'+value+'"/>');
end;


procedure THtmlPublisher.Line;
begin
  doAppend('<hr/>'#13#10);
end;

procedure THtmlPublisher.Memo(name, value, text: String);
begin
  doAppend(text+'<textArea name="'+name+'">'#13#10+value+'</textArea>');
end;

function THtmlPublisher.output: String;
begin
  result := FSource.subString(0, FCursor);
end;

procedure THtmlPublisher.ParaURL(text, url: String);
begin
  StartParagraph;
  self.URL(text, url);
  EndParagraph;
end;

procedure THtmlPublisher.Spacer;
begin
  doAppend('&nbsp;');
end;

procedure THtmlPublisher.StartBlockQuote;
begin
  doAppend('<blockquote>');
end;

procedure THtmlPublisher.startDiv;
begin
  doAppend('<div>')
end;

procedure THtmlPublisher.StartForm(method, action: String);
begin
  doAppend('<form method="'+method+'" action="'+action+'">'#13#10);
end;

procedure THtmlPublisher.StartList(ordered: boolean);
begin
  if ordered then
    doAppend('<ol>')
  else
    doAppend('<ul>');
end;

procedure THtmlPublisher.StartListItem;
begin
  doAppend('<li>');
end;

procedure THtmlPublisher.StartParagraph;
begin
  doAppend('<p>');
end;

procedure THtmlPublisher.StartPre;
begin
  doAppend('<pre>'#13#10);
end;

procedure THtmlPublisher.StartRow(bgcolor : string = '');
begin
  if (bgcolor <> '') then
    doAppend('<tr style="background-color: '+bgcolor+'">')
  else
    doAppend('<tr>')
end;

procedure THtmlPublisher.StartTable(borders: boolean; clss : String);
begin
  if clss <> '' then
    clss := ' class="'+clss+'"';
  if borders then
    doAppend('<table border="1"'+clss+'>')
  else
    doAppend('<table border="0"'+clss+'>');
end;

procedure THtmlPublisher.StartTableCell(span: integer);
begin
  if (span <> 1) then
    doAppend('<td colspan="'+inttostr(span)+'">')
  else
   doAppend('<td>')
end;

procedure THtmlPublisher.StartTableRow;
begin
  doAppend('<tr>')
end;

procedure THtmlPublisher.Submit(name: String);
begin
  doAppend('<input type="submit" value="'+name+'"/>');
end;

procedure THtmlPublisher.TextInput(name, value: String; length: integer);
begin
  doAppend('<input type="text" name="'+name+'" value="'+value+'" size="'+inttostr(length)+'"/>');
end;

procedure THtmlPublisher.TextInput(name: String; length: integer);
begin
  doAppend('<input type="text" name="'+name+'" size="'+inttostr(length)+'"/>');
end;

procedure THtmlPublisher.URL(text, url: String; hint: string);
begin
  if (hint <> '') then
    doAppend('<a href="'+url+'" title="'+FormatTextToXml(hint, xmlAttribute)+'">')
  else
    doAppend('<a href="'+url+'">');
  AddTextPlain(text);
  doAppend('</a>');
end;

//procedure THtmlPublisher.writeXhtml(node: TFhirXHtmlNode);
//var
//  i : integer;
//begin
//  case node.NodeType of
//    fhntElement, fhntDocument:
//      begin
//        doAppend('<'+node.Name);
//        if node.HasAttributes then
//          for i := 0 to node.Attributes.Count - 1 do
//            doAppend(' '+node.Attributes[i].Name+'="'+FormatTextToXml(node.Attributes[i].value, xmlAttribute)+'"');
//        if node.ChildNodes.Count = 0 then
//          doAppend('/>')
//        else
//        begin
//          doAppend('>');
//          for i := 0 to node.ChildNodes.Count - 1 do
//            writeXhtml(node.ChildNodes[i]);
//          doAppend('</'+node.Name+'>');
//        end;
//      end;
//    fhntText:
//      AddTextPlain(node.Content);
//    fhntComment:
//      doAppend('<!-- '+FormatTextToXml(node.Content, xmlText)+' -->');
//  end;
//end;
//
procedure THtmlPublisher.TextInput(name, value, text: String; length: integer);
begin
  doAppend('<input type="text" name="'+name+'" value="'+value+'" size="'+inttostr(length)+'"/> '+text);
end;

procedure THtmlPublisher.SetLangList(AValue: THTTPLanguageList);
begin
  FLangList.free;
  FLangList := AValue;
end;

procedure THtmlPublisher.doAppend(s: String);
var
  delta : Integer;
begin
  if (s <> '') then
  begin
    if (s.length + FCursor > FSource.length) then
    begin
      delta := 2048;
      while delta < s.length do
        delta := delta + 2048;
      SetLength(FSource, length(FSource)+delta);
    end;
    move(s[1], FSource[FCursor+1], s.length * sizeof(char));
    inc(FCursor, s.length);
  end;
end;

function THtmlPublisher.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, (FSource.length * sizeof(char)) + 12);
  inc(result, (FBaseURL.length * sizeof(char)) + 12);
  inc(result, FLangList.sizeInBytes(magic));
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, (FLogId.length * sizeof(char)) + 12);
end;

end.
