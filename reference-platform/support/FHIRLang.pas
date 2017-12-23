unit FHIRLang;

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
  SysUtils, classes, Generics.Collections;

function GetFhirMessage(id, lang : String):String; overload;
function GetFhirMessage(id, lang, def : String):String; overload;

procedure LoadMessages;

var
  FHIRExeModuleName : String;

Type
  EFHIRException = class (Exception)
  public
    constructor CreateLang(code, lang : String); overload;
    constructor CreateLang(code, lang : String; const Args: array of const); overload;
  end;

implementation

{$R FHIRTranslations.res}

uses
  StringSupport, TextUtilities,
  AdvObjects, AdvGenerics, AdvExceptions,
  {$IFDEF MSWINDOWS}AfsResourceVolumes, AfsVolumes,{$ENDIF}
  MXML;

Type
  TFHIRMessage = class (TAdvObject)
  private
    FMessages : TDictionary<String,String>;
  public
    constructor Create; override;
    Destructor Destroy; override;
  end;
var
  GMessages : TAdvMap<TFHIRMessage>;

Function LoadSource : TBytes;
{$IFDEF MACOS}
begin
  result := TextUtilities.FileToBytes(IncludeTrailingPathDelimiter(ExtractFilePath(paramstr(0)))+'translations.xml');
end;
{$ELSE}
var
  LRes : TAfsResourceVolume;
  LHnd : TAfsHandle;
begin
  LRes := TAfsResourceVolume.create;
  try
    LHnd := LRes.Open(FHIRExeModuleName, 'FHIR_Translations,#10', amRead, asRead);
    try
      SetLength(result, LRes.GetSize(LHnd));
      LRes.Read(LHnd, result[0], length(result));
    finally
      LRes.Close(LHnd);
    end;
  finally
    LRes.Free;
  end;
end;
{$ENDIF}

procedure LoadMessages;
var
  source : TMXmlDocument;
  child, lang : TMXmlElement;
  msg : TFHIRMessage;
begin
  if FHIRExeModuleName = '##' then
    exit;

  source := TMXmlParser.parse(LoadSource, [xpDropWhitespace, xpDropComments]);
  try
    GMessages := TAdvMap<TFHIRMessage>.create;
    child := source.document.firstElement;
    while child <> nil do
    begin
      msg := TFHIRMessage.Create;
      GMessages.Add(child.attribute['id'], msg);
      lang := child.firstElement;
      while lang <> nil do
      begin
        msg.FMessages.add(lang.attribute['lang'], lang.text);
        lang := lang.nextElement;
      end;
      child := child.nextElement;
    end;
  finally
    source.free;
  end;
end;

function GetFhirMessage(id, lang : String):String;
var
  msg : TFHIRMessage;
  l : string;
begin
  result := '';
  if GMessages = nil then
    LoadMessages;
  if GMessages = nil then
    exit(id);
  if not GMessages.ContainsKey(id) then
    result := 'Unknown message '+id
  else
  begin
    msg := GMessages[id];
    while (result = '') and (lang <> '') do
    begin
      StringSplit(lang, [';', ','], l, lang);
      if msg.FMessages.ContainsKey(l) then
        result := msg.FMessages[l];
    end;
    if result = '' then
      result := msg.FMessages['en'];
    if result = '' then
      result := '??';
  end;
end;

function GetFhirMessage(id, lang, def : String):String;
var
  msg : TFHIRMessage;
  l : string;
begin
  result := '';
  if GMessages = nil then
    LoadMessages;
  if not GMessages.ContainsKey(id) then
    result := def
  else
  begin
    msg := GMessages[id];
    while (result = '') and (lang <> '') do
    begin
      StringSplit(lang, [';', ','], l, lang);
      if msg.FMessages.ContainsKey(l) then
        result := msg.FMessages[l];
    end;
    if result = '' then
      result := msg.FMessages['en'];
    if result = '' then
      result := '??';
    if result = '' then
      result := def;
  end;
end;

{ TFHIRMessage }

constructor TFHIRMessage.Create;
begin
  inherited;
  FMessages := TDictionary<String,String>.create;
end;

destructor TFHIRMessage.Destroy;
begin
  FMessages.Free;
  inherited;
end;

{ EFHIRException }

constructor EFHIRException.CreateLang(code, lang: String);
begin
  inherited Create(GetFhirMessage(code, lang));
end;

constructor EFHIRException.CreateLang(code, lang: String; const Args: array of const);
begin
  inherited Create(Format(GetFhirMessage(code, lang), args));
end;

initialization
  GMessages := nil;
finalization
  GMessages.Free;
end.
