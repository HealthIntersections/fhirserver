unit fsl_i18n;

{
Copyright (c) 2001+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$I fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_utilities, fsl_lang, fsl_http;

type

  { TI18nSupport }

  TI18nSupport = class (TFslObject)
  private
    FLanguages : TIETFLanguageDefinitions;
    FMessages : TFslMap<TFslStringMap>;

    function chooseForLang(lang : String) : TFslStringMap;
    function chooseForLangs(langs: THTTPLanguageList): TFslStringMap;
  public
    constructor Create(languages : TIETFLanguageDefinitions);
    destructor Destroy; override;

    function Link : TI18nSupport; overload;

    property Languages : TIETFLanguageDefinitions read FLanguages;
    // the file name has the language appended to the filename
    procedure loadPropertiesFile(filename : String);

    function translateS(id, lang : String; const args : TStringArray) :  String; overload;
    function translate(id : String; langs : THTTPLanguageList; const args : TStringArray) :  String; overload;
    function translatePluralS(count : integer; id, lang : String; const args : TStringArray) :  String;  overload;
    function translatePlural(count : integer; id : String; langs : THTTPLanguageList; const args : TStringArray) :  String;  overload;
  end;

implementation

{ TI18nSupport }

constructor TI18nSupport.Create(languages: TIETFLanguageDefinitions);
begin
  inherited Create;
  FLanguages := languages;
  FMessages := TFslMap<TFslStringMap>.Create;
  FMessages.DefaultValue := nil;
end;

destructor TI18nSupport.Destroy;
begin
  FMessages.free;
  FLanguages.free;
  inherited;
end;

function TI18nSupport.Link: TI18nSupport;
begin
  result := TI18nSupport(inherited Link);
end;

procedure TI18nSupport.loadPropertiesFile(filename: String);
var
  lang, s, l, r : String;
  f : System.Text;
  msgs : TFslStringMap;
begin
  if filename.Contains('_') then
    lang := filename.Substring(filename.indexOf('_')+1).replace('.properties', '')
  else
    lang := 'en';
  if not FileExists(filename) then
    raise EFslException.create('Properties File "'+filename+'" not found');
  msgs := TFslStringMap.Create;
  try
    assignfile(f, filename);
    reset(f);
    while not eof(f) do
    begin
      readLn(f, s);
      s := s.trim();
      if (not s.startsWith('#')) and (s.contains('=')) then
      begin
        StringSplit(s, '=', l, r);
        msgs[l.trim()] := r.trim();
      end;
    end;
    closeFile(f);

    FMessages.Add(lang, msgs.link);
  finally
    msgs.free;
  end;
end;

function TI18nSupport.translateS(id, lang: String; const args : TStringArray): String;
var
  fmt : String;
  i : integer;
begin
  fmt := chooseForLang(lang)[id];
  if (fmt = '') then
    fmt := chooseForLang('')[id];
  if fmt = '' then
    result := id
  else
  begin
    for i := 0 to length(args) - 1 do
      fmt := fmt.replace('{'+inttostr(i)+'}', args[i]);
    result := fmt.replace('''''', '''');
  end;
end;


function TI18nSupport.translate(id: String; langs : THTTPLanguageList; const args : TStringArray): String;
var
  fmt : String;
  i : integer;
begin
  fmt := chooseForLangs(langs)[id];
  if (fmt = '') then
    fmt := chooseForLang('')[id];
  if fmt = '' then
    result := id
  else
  begin
    for i := 0 to length(args) - 1 do
      fmt := fmt.replace('{'+inttostr(i)+'}', args[i]);
    result := fmt.replace('''''', '''');
  end;
end;


function TI18nSupport.translatePluralS(count: integer; id, lang: String; const args: TStringArray): String;
var
  fmt : String;
  i : integer;
begin
  if (count = 1) then
    id := id + '_one'
  else
    id := id + '_other';

  fmt := chooseForLang(lang)[id];
  if (fmt = '') then
    fmt := chooseForLang('')[id];
  if fmt = '' then
    result := id
  else
  begin
    fmt := fmt.replace('{0}', inttostr(count));
    for i := 0 to length(args) - 1 do
      fmt := fmt.replace('{'+inttostr(i+1)+'}', args[i]);
    result := fmt.replace('''''', '''');
  end;
end;


function TI18nSupport.translatePlural(count: integer; id: String; langs : THTTPLanguageList; const args: TStringArray): String;
var
  fmt : String;
  i : integer;
begin
  if (count = 1) then
    id := id + '_one'
  else
    id := id + '_other';

  fmt := chooseForLangs(langs)[id];
  if (fmt = '') then
    fmt := chooseForLang('')[id];
  if fmt = '' then
    result := id
  else
  begin
    fmt := fmt.replace('{0}', inttostr(count));
    for i := 0 to length(args) - 1 do
      fmt := fmt.replace('{'+inttostr(i+1)+'}', args[i]);
    result := fmt.replace('''''', '''');
  end;
end;

function TI18nSupport.chooseForLang(lang: String): TFslStringMap;
begin
  if lang.Length > 2 then
    lang := lang.subString(0, 2);
  if FMessages.ContainsKey(lang) then
    result := FMessages[lang]
  else
    result := FMessages['en'];
end;

function TI18nSupport.chooseForLangs(langs: THTTPLanguageList): TFslStringMap;
var
  e : THTTPLanguageEntry;
  l : String;
begin
  if (langs <> nil) then
  begin
    for e in langs.langs do
    begin
      l := e.lang;        
      if l.length > 2 then
        l := l.subString(0, 2);
      if FMessages.ContainsKey(l) then
        exit(FMessages[l]);
    end;
  end;
  result := FMessages['en'];
end;

end.

