unit ftk_engine_text;

{
Copyright (c) 2001+, Health Intersections Pty Ltd [http://www.healthintersections.com.au]
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
  Classes, SysUtils,
  fsl_base, fsl_utilities, fsl_fpc, MarkdownProcessor;

type
  TTextEngineTransform = (
    tetNone,
    tetHTMLEscape,
    tetHTMLUnescape,
    tetXMLEscapeText,
    tetXMLEscapeAttr,
    tetXMLUnescape,
    tetJSONEscape,
    tetJSONUnescape,
    tetJavaStringEscape,
    tetJavaStringUnescape,
    tetPascalStringEscape,
    tetPascalStringUnescape,
    tetHTTPURLEncode,
    tetHTTPURLDecode,
    tetBase64Encode,
    tetBase64Decode,
    tetBase64UrlEncode,
    tetBase64UrlDecode,
    tetMarkdown,
    tetCommonMark,
    tetASCIIOnly,
    tetRawANSI,
    tetRawUTF8,
    tetRawUTF16LE,
    tetRawUTF16BE
  );

  { TTextEngineStep }

  TTextEngineStep = class (TFslObject)
  private
    FTransform: TTextEngineTransform;
    function StripUnicode(s : String):String;
    function javaEscape(s : String):String;
    function pascalEscape(s : String):String;
    function pascalUnEscape(s : String):String;
    function hexEncode(bytes : TBytes) : String;
    function processMarkdown(s : string; dialect : TMarkdownProcessorDialect): String;
  public
    constructor Create(transform : TTextEngineTransform);
    property transform : TTextEngineTransform read FTransform write FTransform;

    function execute(src : String) : String;
  end;

  TTextEngine = class (TFslObject)
  private
    FSteps : TFslList<TTextEngineStep>;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure clear;
    procedure addStep(transform : TTextEngineTransform);

    function execute(src : String) : String;
  end;

implementation

{ TTextEngine }

constructor TTextEngine.Create;
begin
  inherited Create;
  FSteps := TFslList<TTextEngineStep>.create;
end;

destructor TTextEngine.Destroy;
begin
  FSteps.free;
  inherited Destroy;
end;

procedure TTextEngine.clear;
begin
  FSteps.clear;
end;

procedure TTextEngine.addStep(transform: TTextEngineTransform);
begin
  FSteps.add(TTextEngineStep.create(transform));
end;

function TTextEngine.execute(src: String): String;
var
  step : TTextEngineStep;
begin
  result := src;
  for step in FSteps do
    result := step.execute(result);
end;

{ TTextEngineStep }

constructor TTextEngineStep.Create(transform: TTextEngineTransform);
begin
  inherited Create;
  self.transform := transform;
end;

function TTextEngineStep.StripUnicode(s : String):String;
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


function TTextEngineStep.javaEscape(s : String):String;
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

function TTextEngineStep.pascalEscape(s : String):String;
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

function TTextEngineStep.pascalUnEscape(s : String):String;
begin
  result := 'not done yet';
end;

function TTextEngineStep.hexEncode(bytes : TBytes) : String;
var
  b : byte;
begin
  result := '';
  for b in bytes do
    result := result + ' '+inttohex(b, 2);
  result := result.trim;
end;

function TTextEngineStep.processMarkdown(s: string; dialect: TMarkdownProcessorDialect): String;
var
  processor : TMarkdownProcessor;
begin
  processor := TMarkdownProcessor.CreateDialect(dialect);
  try
    processor.AllowUnsafe := false;
    result := processor.process(s);
  finally
    processor.free;
  end;
end;

function TTextEngineStep.execute(src: String): String;
begin
  case transform of
    tetNone: result := src;
    tetHTMLEscape: result := FormatTextToXML(src, xmlText);
    tetHTMLUnescape: result := DecodeXML(src);
    tetXMLEscapeText: result := FormatTextToXML(src, xmlText);
    tetXMLEscapeAttr: result := FormatTextToXML(src, xmlAttribute);
    tetXMLUnescape: result := DecodeXML(src);
    tetJSONEscape: result := jsonEscape(src, true);
    tetJSONUnescape: result := jsonUnEscape(src);
    tetJavaStringEscape: result := javaEscape(src);
    tetJavaStringUnescape: result := jsonUnEscape(src);
    tetPascalStringEscape: result := pascalEscape(src);
    tetPascalStringUnescape: result := pascalUnescape(src);
    tetHTTPURLEncode: result := EncodePercent(src);
    tetHTTPURLDecode: result := DecodePercent(src);
    tetBase64Encode: result := EncodeBase64(TEncoding.UTF8.GetBytes(src));
    tetBase64Decode: result := TEncoding.UTF8.GetString(DecodeBase64(src));
    tetBase64UrlEncode: result := EncodeBase64Url(TEncoding.UTF8.GetBytes(src));
    tetBase64UrlDecode: result := TEncoding.UTF8.GetString(DecodeBase64Url(src));
    tetMarkdown: result := processMarkdown(src, mdDaringFireball);
    tetCommonMark: result := processMarkdown(src, mdCommonMark);
    tetASCIIOnly: result := StripUnicode(src);
    tetRawANSI: result := HexEncode(TEncoding.ANSI.GetBytes(src));
    tetRawUTF8: result := HexEncode(TEncoding.ANSI.GetBytes(src));
    tetRawUTF16LE: result := HexEncode(TEncoding.Unicode.GetBytes(src));
    tetRawUTF16BE: result := HexEncode(TEncoding.BigEndianUnicode.GetBytes(src));
  end;
end;

end.

