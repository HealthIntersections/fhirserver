unit FHIR.Npp.FormatUtilities;


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

interface

uses
  Windows, SysUtils, Classes, Forms, Vcl.Dialogs, Messages, Consts, UITypes, System.Generics.Defaults, ActiveX, Vcl.Clipbrd,
  MarkdownProcessor,
  FHIR.Npp.BaseFU, FHIR.Npp.ScintillaFU,
  fsl_base, fsl_utilities, fsl_stream, fsl_xml, fsl_json;


type
  TFormatUtilitiesPlugin = class(TNppPlugin)
  public
    constructor Create;

    // user interface
    procedure FuncXmlPrettyPrint;
    procedure FuncXmlCollapse;
    procedure FuncXmlEscape;
    procedure FuncXmlUnescape;
    procedure FuncXmlCheck;
    procedure FuncJsonPrettyPrint;
    procedure FuncJsonCollapse;
    procedure FuncJsonEscape;
    procedure FuncJsonUnescape;
    procedure FuncJsonCheck;
    procedure FuncHtmlLink;
    procedure FuncHtmlCode;
    procedure FuncHtmlMarkdown;
    procedure FuncHtmlWrap;
    procedure FuncHtmlParapraph;
    procedure FuncHtmlEscape;
    procedure FuncHtmlUnescape;
    procedure FuncTextUUID;
    procedure FuncUrlEscape;
    procedure FuncUrlUnEscape;
    procedure FuncRemoveDuplicates;
    procedure FuncPasteHtml;
  end;

procedure _FuncPrettyPrintXml; cdecl;
procedure _FuncCollapseXml; cdecl;
procedure _FuncEscapeXml; cdecl;
procedure _FuncUnescapeXml; cdecl;
procedure _FuncCheckXml; cdecl;
procedure _FuncPrettyPrintJson; cdecl;
procedure _FuncCollapseJson; cdecl;
procedure _FuncEscapeJson; cdecl;
procedure _FuncUnescapeJson; cdecl;
procedure _FuncCheckJson; cdecl;
procedure _FuncHtmlLink; cdecl;
procedure _FuncHtmlCode; cdecl;
procedure _FuncHtmlWrap; cdecl;
procedure _FuncHtmlMarkdown; cdecl;
procedure _FuncHtmlParapraph; cdecl;
procedure _FuncHtmlEscape; cdecl;
procedure _FuncHtmlUnescape; cdecl;
procedure _FuncTextUUID; cdecl;
procedure _FuncUrlEscape; cdecl;
procedure _FuncUrlUnescape; cdecl;
procedure _FuncRemoveDuplicates; cdecl;
procedure _FuncPasteHtml; cdecl;

var
  FNpp: TFormatUtilitiesPlugin;

implementation

var
  CF_HTML : Integer = 0;

{ TFormatUtilitiesPlugin }

constructor TFormatUtilitiesPlugin.Create;
begin
  inherited;
  CF_HTML := RegisterClipboardFormat('HTML Format');
  self.PluginName := 'Format &Utils';
  self.AddFuncItem('&Pretty Print XML', _FuncPrettyPrintXml);
  self.AddFuncItem('&Collapse XML', _FuncCollapseXml);
  self.AddFuncItem('&Escape XML', _FuncEscapeXml);
  self.AddFuncItem('&Unescape XML', _FuncUnescapeXml);
  self.AddFuncItem('&Validate XML', _FuncCheckXml);
  self.AddFuncItem('-', Nil);
  self.AddFuncItem('&Pretty Print Json', _FuncPrettyPrintJson);
  self.AddFuncItem('&Collapse Json', _FuncCollapseJson);
  self.AddFuncItem('&Escape Json', _FuncEscapeJson);
  self.AddFuncItem('&Unescape Json', _FuncUnescapeJson);
  self.AddFuncItem('&Validate Json', _FuncCheckJson);
  self.AddFuncItem('-', Nil);
  self.AddFuncItem('HTML &Link', _FuncHtmlLink);
  self.AddFuncItem('HTML &Code', _FuncHtmlCode);
  self.AddFuncItem('HTML &Wrap', _FuncHtmlWrap);
  self.AddFuncItem('HTML &Markdown', _FuncHtmlMarkdown);
  self.AddFuncItem('HTML &Paragraph', _FuncHtmlParapraph);
  self.AddFuncItem('HTML &Escape', _FuncHtmlEscape);
  self.AddFuncItem('HTML &Unescape', _FuncHtmlUnescape);
  self.AddFuncItem('Paste Html', _FuncPasteHtml);
  self.AddFuncItem('-', Nil);
  self.AddFuncItem('Text &UUID', _FuncTextUUID);
  self.AddFuncItem('-', Nil);
  self.AddFuncItem('Url &Escape', _FuncUrlEscape);
  self.AddFuncItem('Url &Unescape', _FuncUrlUnescape);
  self.AddFuncItem('-', Nil);
  self.AddFuncItem('Remove Duplicate Lines', _FuncRemoveDuplicates);
end;

procedure _FuncPrettyPrintXml; cdecl;
begin
  FNpp.FuncXmlPrettyPrint;
end;

procedure _FuncCollapseXml; cdecl;
begin
  FNpp.FuncXmlCollapse;
end;

procedure _FuncEscapeXml; cdecl;
begin
  FNpp.FuncXmlEscape;
end;

procedure _FuncUnescapeXml; cdecl;
begin
  FNpp.FuncXmlUnescape;
end;

procedure _FuncCheckXml; cdecl;
begin
  FNpp.FuncXmlCheck;
end;

procedure _FuncPrettyPrintJson; cdecl;
begin
  FNpp.FuncJsonPrettyPrint;
end;

procedure _FuncCollapseJson; cdecl;
begin
  FNpp.FuncJsonCollapse;
end;

procedure _FuncEscapeJson; cdecl;
begin
  FNpp.FuncJsonEscape;
end;

procedure _FuncUnescapeJson; cdecl;
begin
  FNpp.FuncJsonUnescape;
end;

procedure _FuncCheckJson; cdecl;
begin
  FNpp.FuncJsonCheck;
end;

procedure _FuncHtmlLink;
begin
  FNpp.FuncHtmlLink;
end;

procedure _FuncHtmlCode;
begin
  FNpp.FuncHtmlCode;
end;

procedure _FuncHtmlWrap;
begin
  FNpp.FuncHtmlWrap;
end;

procedure _FuncHtmlMarkdown;
begin
  FNpp.FuncHtmlMarkdown;
end;

procedure _FuncHtmlParapraph;
begin
  FNpp.FuncHtmlParapraph;
end;

procedure _FuncHtmlEscape;
begin
  FNpp.FuncHtmlEscape;
end;

procedure _FuncHtmlUnescape;
begin
  FNpp.FuncHtmlUnescape;
end;

procedure _FuncTextUUID;
begin
  FNpp.FuncTextUUID;
end;

procedure _FuncUrlEscape;
begin
  FNpp.FuncUrlEscape;
end;

procedure _FuncUrlUnescape;
begin
  FNpp.FuncUrlUnescape;
end;

procedure _FuncRemoveDuplicates;
begin
  FNpp.FuncRemoveDuplicates;
end;

procedure _FuncPasteHtml;
begin
  FNpp.FuncPasteHtml;
end;

procedure TFormatUtilitiesPlugin.FuncXmlCheck;
var
  ok : boolean;
  sp : integer;
begin
  ok := false;
  try
    TMXmlParser.parse(CurrentBytes, [xpDropWhitespace]).free;
    ok := true;
  except
    on e : EParserException do
    begin
      // goto location....
      sp := SendMessage(NppData.ScintillaMainHandle, SCI_FINDCOLUMN, e.location.Line-1, e.location.Col-1);
      SendMessage(NppData.ScintillaMainHandle, SCI_SETSEL, sp, sp);
      ShowMessage('Parser Exception: '+e.Message);
    end;
    on e : Exception do
    begin
      ShowMessage('Exception: '+e.Message);
    end;
  end;
  if ok then
    MessageBeep(MB_ICONHAND);
end;

procedure TFormatUtilitiesPlugin.FuncXmlCollapse;
var
  x : TMXmlDocument;
begin
  x := TMXmlParser.parse(SelectedBytes, [xpDropWhitespace]);
  try
    SelectedBytes := TEncoding.UTF8.GetBytes(x.ToXml(false, false));
  finally
    x.Free;
  end;
end;

procedure TFormatUtilitiesPlugin.FuncXmlEscape;
var
  s : String;
begin
  s := TEncoding.UTF8.getString(SelectedBytes);
  s := FormatTextToXML(s, xmlText);
  SelectedBytes := TEncoding.UTF8.GetBytes(s);
end;

procedure TFormatUtilitiesPlugin.FuncXmlPrettyPrint;
var
  x : TMXmlDocument;
begin
  x := TMXmlParser.parse(SelectedBytes, [xpDropWhitespace]);
  try
    SelectedBytes := TEncoding.UTF8.GetBytes(x.ToXml(true, true));
  finally
    x.Free;
  end;
end;

procedure TFormatUtilitiesPlugin.FuncXmlUnescape;
var
  s : String;
begin
  s := TEncoding.UTF8.getString(SelectedBytes);
  s := DecodeXML(s);
  SelectedBytes := TEncoding.UTF8.GetBytes(s);
end;

procedure TFormatUtilitiesPlugin.FuncJsonCheck;
var
  ok : boolean;
  sp : integer;
begin
  ok := false;
  try
    TJSONParser.Parse(CurrentBytes).Free;
    ok := true;
  except
    on e : EParserException do
    begin
      // goto location....
      sp := SendMessage(NppData.ScintillaMainHandle, SCI_FINDCOLUMN, e.location.Line-1, e.location.Col-1);
      SendMessage(NppData.ScintillaMainHandle, SCI_SETSEL, sp, sp);
      ShowMessage('Parser Exception: '+e.Message);
    end;
    on e : Exception do
    begin
      ShowMessage('Exception: '+e.Message);
    end;
  end;
  if ok then
  begin
    MessageBeep(MB_ICONHAND);
  end;
end;

procedure TFormatUtilitiesPlugin.FuncJsonCollapse;
var
  j : TJsonObject;
begin
  j := TJSONParser.Parse(SelectedBytes);
  try
    SelectedBytes := TEncoding.UTF8.GetBytes(TJsonWriter.writeObjectStr(j, false));
  finally
    j.Free;
  end;
end;

procedure TFormatUtilitiesPlugin.FuncJsonEscape;
var
  s : String;
begin
  s := TEncoding.UTF8.getString(SelectedBytes);
  s := JsonString(s);
  SelectedBytes := TEncoding.UTF8.GetBytes(s);
end;

procedure TFormatUtilitiesPlugin.FuncHtmlCode;
var
  s : String;
begin
  s := TEncoding.UTF8.getString(SelectedBytes);
  s := '<code>'+s+'</code>';
  SelectedBytes := TEncoding.UTF8.GetBytes(s);
end;

procedure TFormatUtilitiesPlugin.FuncHtmlEscape;
var
  s : String;
begin
  s := TEncoding.UTF8.getString(SelectedBytes);
  s := FormatTextToHTML(s);
  SelectedBytes := TEncoding.UTF8.GetBytes(s);
end;

procedure TFormatUtilitiesPlugin.FuncHtmlLink;
var
  u, s : String;
begin
  s := TEncoding.UTF8.getString(SelectedBytes);
  u := Clipboard.AsText;
  if isAbsoluteUrl(u) or u.Contains('.htm') then
    s := '<a href="'+u+'">'+s+'</a>'
  else
    s := '<a href="">'+s+'</a>';
  SelectedBytes := TEncoding.UTF8.GetBytes(s);
end;

procedure TFormatUtilitiesPlugin.FuncHtmlMarkdown;
var
  s : String;
  md : TMarkdownProcessor;
begin
  md := TMarkdownProcessor.CreateDialect(mdCommonMark);
  try
    s := TEncoding.UTF8.getString(SelectedBytes);
    s := md.process(s);
    SelectedBytes := TEncoding.UTF8.GetBytes(s);
  finally
    md.Free;
  end;
end;

procedure TFormatUtilitiesPlugin.FuncHtmlParapraph;
var
  s : String;
begin
  s := TEncoding.UTF8.getString(SelectedBytes);
  s := '<p>'+s+'</p>';
  SelectedBytes := TEncoding.UTF8.GetBytes(s);
end;

procedure TFormatUtilitiesPlugin.FuncHtmlUnescape;
var
  s : String;
begin
  s := TEncoding.UTF8.getString(SelectedBytes);
  s := DecodeXML(s);
  SelectedBytes := TEncoding.UTF8.GetBytes(s);
end;

procedure TFormatUtilitiesPlugin.FuncHtmlWrap;
var
  t, s : String;
begin
  t := Clipboard.AsText;
  s := TEncoding.UTF8.getString(SelectedBytes);
  s := '<'+t+'>'+s+'</'+t+'>';
  SelectedBytes := TEncoding.UTF8.GetBytes(s);
end;

procedure TFormatUtilitiesPlugin.FuncJsonPrettyPrint;
var
  j : TJsonObject;
begin
  j := TJSONParser.Parse(SelectedBytes);
  try
    SelectedBytes := TEncoding.UTF8.GetBytes(TJsonWriter.writeObjectStr(j, true));
  finally
    j.Free;
  end;
end;

procedure TFormatUtilitiesPlugin.FuncJsonUnescape;
var
  s : String;
begin
  s := TEncoding.UTF8.getString(SelectedBytes);
  s := UnJsonString(s);
  SelectedBytes := TEncoding.UTF8.GetBytes(s);
end;


procedure TFormatUtilitiesPlugin.FuncRemoveDuplicates;
var
  sl : TStringList;
  i, j : integer;
begin
  sl := TStringList.Create;
  try
    sl.Text := TEncoding.UTF8.getString(SelectedBytes);
    for i := 0 to sl.Count - 1 do
    begin
      for j := sl.Count - 1 downto i+1 do
        if sl[i] = sl[j] then
          sl.Delete(j);
    end;
    SelectedBytes := TEncoding.UTF8.GetBytes(sl.Text);
  finally
    sl.Free;
  end;
end;

procedure TFormatUtilitiesPlugin.FuncPasteHtml;
var
  ClipboardData: Windows.HGLOBAL;
  Ptr: Pointer;
  Size: DWORD;
  b : TBytes;
begin
  Clipboard.Open;
  try
    if Clipboard.HasFormat(CF_HTML) then
    begin
      ClipboardData := Clipboard.GetAsHandle(CF_HTML);
      if ClipboardData <> 0 then
      begin
        Ptr := Windows.GlobalLock(ClipboardData);
        if Ptr <> nil then
        begin
          try
            Size := Windows.GlobalSize(ClipboardData);
            setLength(b, Size);
            Move(ptr^, b[0], Size);
            SelectedBytes := b;
          finally
            Windows.GlobalUnlock(ClipboardData);
          end;
        end;
      end;
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure TFormatUtilitiesPlugin.FuncTextUUID;
var
  s : String;
begin
  s := TEncoding.UTF8.getString(SelectedBytes);
  s := NewGuidId;
  SelectedBytes := TEncoding.UTF8.GetBytes(s);
end;

procedure TFormatUtilitiesPlugin.FuncUrlEscape;
var
  s : String;
begin
  s := TEncoding.UTF8.getString(SelectedBytes);
  s := EncodePercent(s);
  SelectedBytes := TEncoding.UTF8.GetBytes(s);
end;

procedure TFormatUtilitiesPlugin.FuncUrlUnescape;
var
  s : String;
begin
  s := TEncoding.UTF8.getString(SelectedBytes);
  s := DecodePercent(s);
  SelectedBytes := TEncoding.UTF8.GetBytes(s);
end;

initialization
  FNpp := TFormatUtilitiesPlugin.Create;
end.


