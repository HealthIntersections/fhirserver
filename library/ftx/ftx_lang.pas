unit ftx_lang;

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
  SysUtils, Classes, Generics.Collections,
  fsl_utilities, fsl_stream, fsl_base, fsl_http,
  fhir_common,
  ftx_service;

type
  TIETFLanguageCodeConcept = class (TCodeSystemProviderContext)
  private
    code : String;
    language : string;
    extLang : TArray<String>;
    script : string;
    region : String;
    variant : String;
    extension : String;
    privateUse : TArray<String>;
    procedure addExtLang(s : String);
    procedure addPrivateUse(s : String);
  public
    constructor Create(code : String);
    function Link : TIETFLanguageCodeConcept; overload;

    function isLangRegion : boolean;
  end;

  TIETFLanguageEntry = class (TFslObject)
  private
    FCode: String;
    FDisplay: String;
  public
    property code : String read FCode write FCode;
    property display : String read FDisplay write FDisplay;
  end;

  TIETFLanguageLanguage = class (TIETFLanguageEntry)
  private
    FSScript: String;
    FScope: String;
  public
    function Link : TIETFLanguageLanguage; overload;
    property sscript : String read FSScript write FSScript;
    property scope : String read FScope write FScope;
  end;

  TIETFLanguageExtLang = class (TIETFLanguageEntry)
  public
    function Link : TIETFLanguageExtLang; overload;
  end;

  TIETFLanguageScript = class (TIETFLanguageEntry)
  public
    function Link : TIETFLanguageScript; overload;
  end;

  TIETFLanguageRegion = class (TIETFLanguageEntry)
  public
    function Link : TIETFLanguageRegion; overload;
  end;

  TIETFLanguageVariant = class (TIETFLanguageEntry)
  public
    function Link : TIETFLanguageVariant; overload;
  end;

  { TIETFLanguageDefinitions }

  TIETFLanguageDefinitions = class (TFslObject)
  private
    FLanguages : TFslMap<TIETFLanguageLanguage>;
    FExtLanguages : TFslMap<TIETFLanguageExtLang>;
    FScripts : TFslMap<TIETFLanguageScript>;
    FRegions : TFslMap<TIETFLanguageRegion>;
    FVariants : TFslMap<TIETFLanguageVariant>;
    function readVars(st : TStringList; i : integer; vars : TFslStringDictionary) :integer;
    function loadLanguage(vars : TFslStringDictionary; i : integer) :integer;
    function loadExtLang(vars : TFslStringDictionary; i : integer) :integer;
    function loadScript(vars : TFslStringDictionary; i : integer) :integer;
    function loadRegion(vars : TFslStringDictionary; i : integer) :integer;
    function loadVariant(vars : TFslStringDictionary; i : integer) :integer;
    procedure Load(source : String);
  public
    constructor Create(source : String);
    destructor Destroy; override;

    class function checkSource(source : String) : String;
    function parse(code : String; var msg : String) : TIETFLanguageCodeConcept;
    function present(code : TIETFLanguageCodeConcept) : String; overload;
    function present(code : TIETFLanguageCodeConcept; template : String) : String; overload;

    function getDisplayForRegion(code : String):String;
    function getDisplayForLang(code : String):String;
  end;

  TIETFLanguageComponent = (languageComponentLang, languageComponentExtLang, languageComponentScript, languageComponentRegion, languageComponentVariant, languageComponentExtension, languageComponentPrivateUse);

  TIETFLanguageCodeFilter = class (TCodeSystemProviderFilterContext)
  private
    FComponent : TIETFLanguageComponent;
    FStatus : boolean;
  public
    constructor Create(component : TIETFLanguageComponent; status : boolean);
    property component : TIETFLanguageComponent read FComponent write FComponent;
    property status : boolean read FStatus write FStatus;
  end;

  TIETFLanguageCodePrep = class (TCodeSystemProviderFilterPreparationContext)
  end;

  { TIETFLanguageCodeServices }

  TIETFLanguageCodeServices = class (TCodeSystemProvider)
  private
    FDefinitions : TIETFLanguageDefinitions;
  public
    constructor Create(sourceFile : String);
    destructor Destroy; Override;
    Function Link : TIETFLanguageCodeServices; overload;

    class function checkFile(sourceFile : String) : String;

    function TotalCount : integer;  override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function systemUri(context : TCodeSystemProviderContext) : String; override;
    function version(context : TCodeSystemProviderContext) : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; const lang : THTTPLanguages):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string; override;
    procedure Displays(code : String; list : TStringList; const lang : THTTPLanguages); override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList; const lang : THTTPLanguages); override;
    function Definition(context : TCodeSystemProviderContext) : string; override;

    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
    function prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean; override;

    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
  end;

const
  CODES_TIETFLanguageComponent : array [TIETFLanguageComponent] of String = ('language', 'ext-lang', 'script', 'region', 'variant', 'extension', 'private-use');

implementation

{
a single primary language subtag based on a two-letter language code from ISO 639-1 (2002) or a three-letter code from ISO 639-2 (1998), ISO 639-3 (2007) or ISO 639-5 (2008), or registered through the BCP 47 process and composed of five to eight letters;
up to three optional extended language subtags composed of three letters each, separated by hyphens; (There is currently no extended language subtag registered in the Language Subtag Registry without an equivalent and preferred primary language subtag. This component of language tags is preserved for backwards compatibility and to allow for future parts of ISO 639.)
an optional script subtag, based on a four-letter script code from ISO 15924 (usually written in title case);
an optional region subtag based on a two-letter country code from ISO 3166-1 alpha-2 (usually written in upper case), or a three-digit code from UN M.49 for geographical regions;

optional variant subtags, separated by hyphens, each composed of five to eight letters, or of four characters starting with a digit; (Variant subtags are registered with IANA and not associated with any external standard.)
optional extension subtags, separated by hyphens, each composed of a single character, with the exception of the letter x, and a hyphen followed by one or more subtags of two to eight characters each, separated by hyphens;
an optional private-use subtag, composed of the letter x and a hyphen followed by subtags of one to eight characters each, separated by hyphens.

 langtag       = language
                 ["-" script]
                 ["-" region]
                 *("-" variant)
                 *("-" extension)
                 ["-" privateuse]

 language      = 2*3ALPHA            ; shortest ISO 639 code
                 ["-" extlang]       ; sometimes followed by
                                     ; extended language subtags
               / 4ALPHA              ; or reserved for future use
               / 5*8ALPHA            ; or registered language subtag

 extlang       = 3ALPHA              ; selected ISO 639 codes
                 *2("-" 3ALPHA)      ; permanently reserved

 script        = 4ALPHA              ; ISO 15924 code

 region        = 2ALPHA              ; ISO 3166-1 code
               / 3DIGIT              ; UN M.49 code

 variant       = 5*8alphanum         ; registered variants
               / (DIGIT 3alphanum)

 extension     = singleton 1*("-" (2*8alphanum))

                                     ; Single alphanumerics
                                     ; "x" reserved for private use
 singleton     = DIGIT               ; 0 - 9
               / %x41-57             ; A - W
               / %x59-5A             ; Y - Z
               / %x61-77             ; a - w
               / %x79-7A             ; y - z

 privateuse    = "x" 1*("-" (1*8alphanum))

 grandfathered = irregular           ; non-redundant tags registered
               / regular             ; during the RFC 3066 era

 irregular     = "en-GB-oed"         ; irregular tags do not match
               / "i-ami"             ; the 'langtag' production and
               / "i-bnn"             ; would not otherwise be
               / "i-default"         ; considered 'well-formed'
               / "i-enochian"        ; These tags are all valid,
               / "i-hak"             ; but most are deprecated
               / "i-klingon"         ; in favor of more modern
               / "i-lux"             ; subtags or subtag
               / "i-mingo"           ; combination

http://www.iana.org/assignments/language-subtag-registry/language-subtag-registry
http://r12a.github.io/apps/subtags/
https://www.w3.org/International/articles/language-tags/index.en


}

{ TIETFLanguageCodeServices }

constructor TIETFLanguageCodeServices.Create(sourceFile: String);
begin
  inherited Create;
  FDefinitions := TIETFLanguageDefinitions.Create(FileToString(sourceFile, TEncoding.ASCII));
end;


function TIETFLanguageCodeServices.TotalCount : integer;
begin
  result := -1;   // not bounded
end;


function TIETFLanguageCodeServices.version(context: TCodeSystemProviderContext): String;
begin
  result := '';
end;

function TIETFLanguageCodeServices.systemUri(context : TCodeSystemProviderContext) : String;
begin
  result := 'urn:ietf:bcp:47';
end;

function TIETFLanguageCodeServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TIETFLanguageCodeServices.getDisplay(code : String; const lang : THTTPLanguages):String;
var
  c : TIETFLanguageCodeConcept;
  msg : String;
begin
  if (code = '') then
    result := '??'
  else
  begin
    c := FDefinitions.parse(code, msg);
    try
      if c <> nil then
        result := FDefinitions.present(c).Trim
      else
        result := '??';
    finally
      c.Free;
    end;
  end;
end;

function TIETFLanguageCodeServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

procedure TIETFLanguageCodeServices.Displays(code : String; list : TStringList; const lang : THTTPLanguages);
var
  c : TIETFLanguageCodeConcept;
  msg : String;
begin
  if (code <> '') then
  begin
    c := FDefinitions.parse(code, msg);
    try
      if c <> nil then
        list.Add(FDefinitions.present(c).Trim);
      if c.isLangRegion then
        list.Add(FDefinitions.present(c, '{{lang}} ({{region}})').Trim);
    finally
      c.Free;
    end;
  end;
end;


function TIETFLanguageCodeServices.locate(code : String; var message : String) : TCodeSystemProviderContext;
begin
  result := FDefinitions.parse(code, message);
end;


function TIETFLanguageCodeServices.Code(context : TCodeSystemProviderContext) : string;
begin
  result := TIETFLanguageCodeConcept(context).code;
end;

function TIETFLanguageCodeServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

destructor TIETFLanguageCodeServices.Destroy;
begin
  FDefinitions.Free;
  inherited;
end;

function TIETFLanguageCodeServices.Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string;
begin
  result := getDisplay(TIETFLanguageCodeConcept(context).code, lang);
end;

procedure TIETFLanguageCodeServices.Displays(context: TCodeSystemProviderContext; list: TStringList; const lang : THTTPLanguages);
begin
  list.Add(Display(context, lang));
end;

function TIETFLanguageCodeServices.IsAbstract(context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // IETFLanguageCode doesn't do abstract
end;

function TIETFLanguageCodeServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := true;
end;

function TIETFLanguageCodeServices.Link: TIETFLanguageCodeServices;
begin
  result := TIETFLanguageCodeServices(Inherited Link);
end;

class function TIETFLanguageCodeServices.checkFile(sourceFile: String): String;
begin
  try
    result := TIETFLanguageDefinitions.checkSource(FileToString(sourceFile, TEncoding.ASCII));
  except
    on e : Exception do
      result := 'Error: '+e.message;
  end;
end;

function TIETFLanguageCodeServices.ChildCount(context : TCodeSystemProviderContext) : integer;
begin
  result := -1;
end;

function TIETFLanguageCodeServices.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.create('TIETFLanguageCodeServices.getcontext');
end;

function TIETFLanguageCodeServices.locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  result := nil; // no subsumption
end;


function TIETFLanguageCodeServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'IETF langauge';
end;

function TIETFLanguageCodeServices.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  result := false;
end;

function TIETFLanguageCodeServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.create('TIETFLanguageCodeServices.searchFilter');
end;

function TIETFLanguageCodeServices.filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
var
  i : integer;
begin
  i := StringArrayIndexOfSensitive(CODES_TIETFLanguageComponent, prop);
  if (i >= 0) and (op = foExists) and ((value = 'true') or (value = 'false')) then
    result := TIETFLanguageCodeFilter.Create(TIETFLanguageComponent(i), value = 'true')
  else
    raise ETerminologyError.Create('Not a supported filter');
end;

function TIETFLanguageCodeServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
var
  cc : TIETFLanguageCodeConcept;
  filter : TIETFLanguageCodeFilter;
  ok : boolean;
begin
  result := nil;
  cc := FDefinitions.parse(code, message);
  try
    filter := TIETFLanguageCodeFilter(ctxt);
    ok := false;
    if cc <> nil then
    begin
      case filter.component of
        languageComponentLang: ok := filter.status = (cc.language <> '');
        languageComponentExtLang: ok := filter.status = (length(cc.extLang) > 0);
        languageComponentScript: ok := filter.status = (cc.script <> '');
        languageComponentRegion: ok := filter.status = (cc.region <> '');
        languageComponentVariant: ok := filter.status = (cc.variant <> '');
        languageComponentExtension: ok := filter.status = (cc.extension <> '');
        languageComponentPrivateUse: ok := filter.status = (length(cc.privateUse) > 0);
      end;
    end;
    if ok then
      result := cc.Link
    else if filter.status then
      message := 'The language code '+code+' does not contain a '+CODES_TIETFLanguageComponent[filter.component]+', and it is required to'
    else
      message := 'The language code '+code+' contains a '+CODES_TIETFLanguageComponent[filter.component]+', and it is not allowed to';
  finally
    cc.free;
  end;
end;

function TIETFLanguageCodeServices.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  raise ETerminologyError.create('Language valuesets cannot be expanded as they are based on a grammar');
end;

function TIETFLanguageCodeServices.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.create('TIETFLanguageCodeServices.FilterConcept');
end;

function TIETFLanguageCodeServices.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyTodo.create('TIETFLanguageCodeServices.InFilter');
end;

procedure TIETFLanguageCodeServices.Close(ctxt: TCodeSystemProviderContext);
begin
  ctxt.free;
end;

procedure TIETFLanguageCodeServices.Close(ctxt : TCodeSystemProviderFilterContext);
begin
  ctxt.free;
end;

procedure TIETFLanguageCodeServices.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  ctxt.free;
end;


{ TIETFLanguageCodeConcept }

procedure TIETFLanguageCodeConcept.addExtLang(s: String);
begin
  SetLength(extLang, length(extLang)+1);
  extLang[length(extLang)-1] := s;
end;

procedure TIETFLanguageCodeConcept.addPrivateUse(s: String);
begin
  SetLength(privateUse, length(privateUse)+1);
  privateUse[length(privateUse)-1] := s;
end;

constructor TIETFLanguageCodeConcept.create(code : String);
begin
  inherited create;
  self.code := code;
end;

function TIETFLanguageCodeConcept.isLangRegion: boolean;
begin
  result := (language <> '') and (region <> '') and
    (length(extLang) = 0) and (script = '') and (variant = '') and (extension = '') and (length(privateUse) = 0);

end;

function TIETFLanguageCodeConcept.Link: TIETFLanguageCodeConcept;
begin
  result := TIETFLanguageCodeConcept(inherited link);
end;


{ TIETFLanguageDefinitions }

constructor TIETFLanguageDefinitions.Create(source : String);
begin
  inherited Create;
  FLanguages := TFslMap<TIETFLanguageLanguage>.create('tx.lang');
  FExtLanguages := TFslMap<TIETFLanguageExtLang>.create('tx.lang.ext');
  FScripts := TFslMap<TIETFLanguageScript>.create('tx.lang.scripts');
  FRegions := TFslMap<TIETFLanguageRegion>.create('tx.lang.reg');
  FVariants := TFslMap<TIETFLanguageVariant>.create('tx.lang.var');
  Load(source);
end;

destructor TIETFLanguageDefinitions.Destroy;
begin
  FVariants.Free;
  FScripts.Free;
  FExtLanguages.Free;
  FRegions.Free;
  FLanguages.Free;
  inherited;
end;

class function TIETFLanguageDefinitions.checkSource(source: String): String;
begin
  if source.StartsWith('%%') then
    result := 'Ok'
  else
    result := 'Invalid';
end;

function TIETFLanguageDefinitions.getDisplayForRegion(code: String): String;
begin
//  if not FRegions.TryGetValue(code, result) then
//    result := '??';
end;

function TIETFLanguageDefinitions.getDisplayForLang(code: String): String;
begin
//  if not FLanguages.TryGetValue(code, result) then
//    result := '??';
end;


function TIETFLanguageDefinitions.parse(code : String; var msg : String) : TIETFLanguageCodeConcept;
var
  parts : TArray<String>;
  res : TIETFLanguageCodeConcept;
  c, i, t : integer;
begin
  msg := '';
  res := TIETFLanguageCodeConcept.create(code);
  try
    parts := code.Split(['-']);
    c := 0;
    t := length(parts);
    if not FLanguages.ContainsKey(parts[c]) then
      msg := 'Invalid Language code "'+parts[c]+'"'
    else
    begin
      res.language := parts[c];
      inc(c);
      for i := 1 to 3 do
      begin
        if (c < t) and FExtLanguages.ContainsKey(parts[c]) then
        begin
          res.addExtLang(parts[c]);
          inc(c);
        end;
      end;
      if (c < t) and FScripts.ContainsKey(parts[c]) then
      begin
        res.script := parts[c];
        inc(c);
      end;
      if (c < t) and FRegions.ContainsKey(parts[c]) then
      begin
        res.region := parts[c];
        inc(c);
      end;
      if (c < t) and FVariants.ContainsKey(parts[c]) then
      begin
        res.variant := parts[c];
        inc(c);
      end;
      while (c < t) and parts[c].StartsWith('x') do
      begin
        res.addPrivateUse(parts[c]);
        inc(c);
      end;
      if (c < t) then
        msg := 'Unable to recognise part '+inttostr(c+1)+' ("'+parts[c]+'") as a valid language part';
    end;
    if msg = '' then
      result := res.Link
    else
      result := nil;
  finally
    res.Free;
  end;
end;


function TIETFLanguageDefinitions.present(code: TIETFLanguageCodeConcept; template: String): String;
begin
  result := template.Replace('{{lang}}', FLanguages[code.language].display).Replace('{{region}}', FRegions[code.region].display);
end;

function TIETFLanguageDefinitions.present(code: TIETFLanguageCodeConcept): String;
var
  b : TStringBuilder;
  first : boolean;
  procedure note(n, v : String);
  begin
    if first then
      first := false
    else
      b.Append(', ');
    b.Append(n);
    b.Append('=');
    b.Append(v);
  end;
begin
  b := TStringBuilder.Create;
  try
    b.append(FLanguages[code.language].display);
    if (code.region <> '') or (code.script <> '') or (code.variant <> '') then
    begin
      b.Append(' (');
      first := true;
      if (code.script <> '') then
        note('Script', FScripts[code.script].display);
      if (code.region <> '') then
        note('Region', FRegions[code.region].display);
      if (code.variant <> '') then
        note('Region', FVariants[code.variant].display);
      b.Append(')');
    end;

    result := b.ToString;
  finally
    b.Free;
  end;
end;

function TIETFLanguageDefinitions.readVars(st: TStringList; i: integer; vars: TFslStringDictionary): integer;
var
  l, r : String;
begin
  vars.Clear;
  while (i < st.Count) and (st[i] <> '%%') do
  begin
    if not st[i].StartsWith(' ') then
    begin
      StringSplit(st[i], ':', l, r);
      if not vars.ContainsKey(l.trim) then
        vars.Add(l.trim, r.Trim);
    end;
    inc(i);
  end;
  result := i;
end;

//procedure TIETFLanguageDefinitions.LoadCountries;
//begin
//  FCountries.Add('AD', 'Andorra');
//  FCountries.Add('AE', 'United Arab Emirates');
//  FCountries.add('AF', 'Afghanistan');
//  FCountries.add('AG', 'Antigua and Barbuda');
//  FCountries.add('AI', 'Anguilla');
//  FCountries.add('AL', 'Albania');
//  FCountries.add('AM', 'Armenia');
//  FCountries.add('AO', 'Angola');
//  FCountries.add('AQ', 'Antarctica');
//  FCountries.add('AR', 'Argentina');
//  FCountries.add('AS', 'American Samoa');
//  FCountries.add('AT', 'Austria');
//  FCountries.add('AU', 'Australia');
//  FCountries.add('AW', 'Aruba');
//  FCountries.add('AX', 'Eland Islands');
//  FCountries.add('AZ', 'Azerbaijan');
//  FCountries.add('BA', 'Bosnia and Herzegovina');
//  FCountries.add('BB', 'Barbados');
//  FCountries.add('BD', 'Bangladesh');
//  FCountries.add('BE', 'Belgium');
//  FCountries.add('BF', 'Burkina Faso');
//  FCountries.add('BG', 'Bulgaria');
//  FCountries.add('BH', 'Bahrain');
//  FCountries.add('BI', 'Burundi');
//  FCountries.add('BJ', 'Benin');
//  FCountries.add('BL', 'Saint Barthilemy');
//  FCountries.add('BM', 'Bermuda');
//  FCountries.add('BN', 'Brunei Darussalam');
//  FCountries.add('BO', 'Bolivia, Plurinational State of');
//  FCountries.add('BQ', 'Bonaire, Sint Eustatius and Saba');
//  FCountries.add('BR', 'Brazil');
//  FCountries.add('BS', 'Bahamas');
//  FCountries.add('BT', 'Bhutan');
//  FCountries.add('BV', 'Bouvet Island');
//  FCountries.add('BW', 'Botswana');
//  FCountries.add('BY', 'Belarus');
//  FCountries.add('BZ', 'Belize');
//  FCountries.add('CA', 'Canada');
//  FCountries.add('CC', 'Cocos (Keeling) Islands');
//  FCountries.add('CD', 'Congo, the Democratic Republic of the');
//  FCountries.add('CF', 'Central African Republic');
//  FCountries.add('CG', 'Congo');
//  FCountries.add('CH', 'Switzerland');
//  FCountries.add('CI', 'Ctte d''Ivoire');
//  FCountries.add('CK', 'Cook Islands');
//  FCountries.add('CL', 'Chile');
//  FCountries.add('CM', 'Cameroon');
//  FCountries.add('CN', 'China');
//  FCountries.add('CO', 'Colombia');
//  FCountries.add('CR', 'Costa Rica');
//  FCountries.add('CU', 'Cuba');
//  FCountries.add('CV', 'Cabo Verde');
//  FCountries.add('CW', 'Curagao');
//  FCountries.add('CX', 'Christmas Island');
//  FCountries.add('CY', 'Cyprus');
//  FCountries.add('CZ', 'Czech Republic');
//  FCountries.add('DE', 'Germany');
//  FCountries.add('DJ', 'Djibouti');
//  FCountries.add('DK', 'Denmark');
//  FCountries.add('DM', 'Dominica');
//  FCountries.add('DO', 'Dominican Republic');
//  FCountries.add('DZ', 'Algeria');
//  FCountries.add('EC', 'Ecuador');
//  FCountries.add('EE', 'Estonia');
//  FCountries.add('EG', 'Egypt');
//  FCountries.add('EH', 'Western Sahara');
//  FCountries.add('ER', 'Eritrea');
//  FCountries.add('ES', 'Spain');
//  FCountries.add('ET', 'Ethiopia');
//  FCountries.add('FI', 'Finland');
//  FCountries.add('FJ', 'Fiji');
//  FCountries.add('FK', 'Falkland Islands (Malvinas)');
//  FCountries.add('FM', 'Micronesia, Federated States of');
//  FCountries.add('FO', 'Faroe Islands');
//  FCountries.add('FR', 'France');
//  FCountries.add('GA', 'Gabon');
//  FCountries.add('GB', 'United Kingdom');
//  FCountries.add('GD', 'Grenada');
//  FCountries.add('GE', 'Georgia');
//  FCountries.add('GF', 'French Guiana');
//  FCountries.add('GG', 'Guernsey');
//  FCountries.add('GH', 'Ghana');
//  FCountries.add('GI', 'Gibraltar');
//  FCountries.add('GL', 'Greenland');
//  FCountries.add('GM', 'Gambia');
//  FCountries.add('GN', 'Guinea');
//  FCountries.add('GP', 'Guadeloupe');
//  FCountries.add('GQ', 'Equatorial Guinea');
//  FCountries.add('GR', 'Greece');
//  FCountries.add('GS', 'South Georgia and the South Sandwich Islands');
//  FCountries.add('GT', 'Guatemala');
//  FCountries.add('GU', 'Guam');
//  FCountries.add('GW', 'Guinea-Bissau');
//  FCountries.add('GY', 'Guyana');
//  FCountries.add('HK', 'Hong Kong');
//  FCountries.add('HM', 'Heard Island and McDonald Islands');
//  FCountries.add('HN', 'Honduras');
//  FCountries.add('HR', 'Croatia');
//  FCountries.add('HT', 'Haiti');
//  FCountries.add('HU', 'Hungary');
//  FCountries.add('ID', 'Indonesia');
//  FCountries.add('IE', 'Ireland');
//  FCountries.add('IL', 'Israel');
//  FCountries.add('IM', 'Isle of Man');
//  FCountries.add('IN', 'India');
//  FCountries.add('IO', 'British Indian Ocean Territory');
//  FCountries.add('IQ', 'Iraq');
//  FCountries.add('IR', 'Iran, Islamic Republic of');
//  FCountries.add('IS', 'Iceland');
//  FCountries.add('IT', 'Italy');
//  FCountries.add('JE', 'Jersey');
//  FCountries.add('JM', 'Jamaica');
//  FCountries.add('JO', 'Jordan');
//  FCountries.add('JP', 'Japan');
//  FCountries.add('KE', 'Kenya');
//  FCountries.add('KG', 'Kyrgyzstan');
//  FCountries.add('KH', 'Cambodia');
//  FCountries.add('KI', 'Kiribati');
//  FCountries.add('KM', 'Comoros');
//  FCountries.add('KN', 'Saint Kitts and Nevis');
//  FCountries.add('KP', 'Korea, Democratic People''s Republic of');
//  FCountries.add('KR', 'Korea, Republic of');
//  FCountries.add('KW', 'Kuwait');
//  FCountries.add('KY', 'Cayman Islands');
//  FCountries.add('KZ', 'Kazakhstan');
//  FCountries.add('LA', 'Lao People''s Democratic Republic');
//  FCountries.add('LB', 'Lebanon');
//  FCountries.add('LC', 'Saint Lucia');
//  FCountries.add('LI', 'Liechtenstein');
//  FCountries.add('LK', 'Sri Lanka');
//  FCountries.add('LR', 'Liberia');
//  FCountries.add('LS', 'Lesotho');
//  FCountries.add('LT', 'Lithuania');
//  FCountries.add('LU', 'Luxembourg');
//  FCountries.add('LV', 'Latvia');
//  FCountries.add('LY', 'Libya');
//  FCountries.add('MA', 'Morocco');
//  FCountries.add('MC', 'Monaco');
//  FCountries.add('MD', 'Moldova, Republic of');
//  FCountries.add('ME', 'Montenegro');
//  FCountries.add('MF', 'Saint Martin (French part)');
//  FCountries.add('MG', 'Madagascar');
//  FCountries.add('MH', 'Marshall Islands');
//  FCountries.add('MK', 'Macedonia, the former Yugoslav Republic of');
//  FCountries.add('ML', 'Mali');
//  FCountries.add('MM', 'Myanmar');
//  FCountries.add('MN', 'Mongolia');
//  FCountries.add('MO', 'Macao');
//  FCountries.add('MP', 'Northern Mariana Islands');
//  FCountries.add('MQ', 'Martinique');
//  FCountries.add('MR', 'Mauritania');
//  FCountries.add('MS', 'Montserrat');
//  FCountries.add('MT', 'Malta');
//  FCountries.add('MU', 'Mauritius');
//  FCountries.add('MV', 'Maldives');
//  FCountries.add('MW', 'Malawi');
//  FCountries.add('MX', 'Mexico');
//  FCountries.add('MY', 'Malaysia');
//  FCountries.add('MZ', 'Mozambique');
//  FCountries.add('NA', 'Namibia');
//  FCountries.add('NC', 'New Caledonia');
//  FCountries.add('NE', 'Niger');
//  FCountries.add('NF', 'Norfolk Island');
//  FCountries.add('NG', 'Nigeria');
//  FCountries.add('NI', 'Nicaragua');
//  FCountries.add('NL', 'Netherlands');
//  FCountries.add('NO', 'Norway');
//  FCountries.add('NP', 'Nepal');
//  FCountries.add('NR', 'Nauru');
//  FCountries.add('NU', 'Niue');
//  FCountries.add('NZ', 'New Zealand');
//  FCountries.add('OM', 'Oman');
//  FCountries.add('PA', 'Panama');
//  FCountries.add('PE', 'Peru');
//  FCountries.add('PF', 'French Polynesia');
//  FCountries.add('PG', 'Papua New Guinea');
//  FCountries.add('PH', 'Philippines');
//  FCountries.add('PK', 'Pakistan');
//  FCountries.add('PL', 'Poland');
//  FCountries.add('PM', 'Saint Pierre and Miquelon');
//  FCountries.add('PN', 'Pitcairn');
//  FCountries.add('PR', 'Puerto Rico');
//  FCountries.add('PS', 'Palestine, State of');
//  FCountries.add('PT', 'Portugal');
//  FCountries.add('PW', 'Palau');
//  FCountries.add('PY', 'Paraguay');
//  FCountries.add('QA', 'Qatar');
//  FCountries.add('RE', 'Riunion');
//  FCountries.add('RO', 'Romania');
//  FCountries.add('RS', 'Serbia');
//  FCountries.add('RU', 'Russian Federation');
//  FCountries.add('RW', 'Rwanda');
//  FCountries.add('SA', 'Saudi Arabia');
//  FCountries.add('SB', 'Solomon Islands');
//  FCountries.add('SC', 'Seychelles');
//  FCountries.add('SD', 'Sudan');
//  FCountries.add('SE', 'Sweden');
//  FCountries.add('SG', 'Singapore');
//  FCountries.add('SH', 'Saint Helena, Ascension and Tristan da Cunha');
//  FCountries.add('SI', 'Slovenia');
//  FCountries.add('SJ', 'Svalbard and Jan Mayen');
//  FCountries.add('SK', 'Slovakia');
//  FCountries.add('SL', 'Sierra Leone');
//  FCountries.add('SM', 'San Marino');
//  FCountries.add('SN', 'Senegal');
//  FCountries.add('SO', 'Somalia');
//  FCountries.add('SR', 'Suriname');
//  FCountries.add('SS', 'South Sudan');
//  FCountries.add('ST', 'Sao Tome and Principe');
//  FCountries.add('SV', 'El Salvador');
//  FCountries.add('SX', 'Sint Maarten (Dutch part)');
//  FCountries.add('SY', 'Syrian Arab Republic');
//  FCountries.add('SZ', 'Swaziland');
//  FCountries.add('TC', 'Turks and Caicos Islands');
//  FCountries.add('TD', 'Chad');
//  FCountries.add('TF', 'French Southern Territories');
//  FCountries.add('TG', 'Togo');
//  FCountries.add('TH', 'Thailand');
//  FCountries.add('TJ', 'Tajikistan');
//  FCountries.add('TK', 'Tokelau');
//  FCountries.add('TL', 'Timor-Leste');
//  FCountries.add('TM', 'Turkmenistan');
//  FCountries.add('TN', 'Tunisia');
//  FCountries.add('TO', 'Tonga');
//  FCountries.add('TR', 'Turkey');
//  FCountries.add('TT', 'Trinidad and Tobago');
//  FCountries.add('TV', 'Tuvalu');
//  FCountries.add('TW', 'Taiwan, Province of China');
//  FCountries.add('TZ', 'Tanzania, United Republic of');
//  FCountries.add('UA', 'Ukraine');
//  FCountries.add('UG', 'Uganda');
//  FCountries.add('UM', 'United States Minor Outlying Islands');
//  FCountries.add('US', 'United States');
//  FCountries.add('UY', 'Uruguay');
//  FCountries.add('UZ', 'Uzbekistan');
//  FCountries.add('VA', 'Holy See (Vatican City State)');
//  FCountries.add('VC', 'Saint Vincent and the Grenadines');
//  FCountries.add('VE', 'Venezuela, Bolivarian Republic of');
//  FCountries.add('VG', 'Virgin Islands, British');
//  FCountries.add('VI', 'Virgin Islands, U.S.');
//  FCountries.add('VN', 'Viet Nam');
//  FCountries.add('VU', 'Vanuatu');
//  FCountries.add('WF', 'Wallis and Futuna');
//  FCountries.add('WS', 'Samoa');
//  FCountries.add('YE', 'Yemen');
//  FCountries.add('YT', 'Mayotte');
//  FCountries.add('ZA', 'South Africa');
//  FCountries.add('ZM', 'Zambia');
//  FCountries.add('ZW', 'Zimbabwe');
//end;

procedure TIETFLanguageDefinitions.Load(source : String);
var
  st : TStringList;
  i : integer;
  vars : TFslStringDictionary;
begin
  st := TStringList.Create;
  try
    st.Text := source;
    i := 0;
    vars := TFslStringDictionary.create;
    try
      while (i < st.Count) and (st[i] = '%%') do
      begin
        inc(i);
        i := readVars(st, i, vars);
        if vars['Type'] = 'language' then
          i := LoadLanguage(vars, i)
        else if vars['Type'] = 'extlang' then
          i := LoadExtLang(vars, i)
        else if vars['Type'] = 'script' then
          i := LoadScript(vars, i)
        else if vars['Type'] = 'region' then
          i := LoadRegion(vars, i)
        else if vars['Type'] = 'variant' then
          i := LoadVariant(vars, i)
        else if (vars['Type'] <> 'grandfathered') and (vars['Type'] <> 'redundant') then
           raise ETerminologyError.create('IETFLang: Unable to parse definitions expecting Type: found '+vars['Type']+' at line '+inttostr(i+1))
      end;
    finally
      vars.Free;
    end;
    if i < st.count then
      raise ETerminologyError.create('IETFLang: Unable to parse definitions - premature end at line '+inttostr(i+1))
  finally
    st.Free;
  end;
end;

function TIETFLanguageDefinitions.loadExtLang(vars: TFslStringDictionary; i: integer): integer;
var
  cc : TIETFLanguageExtLang;
begin
  cc := TIETFLanguageExtLang.Create;
  try
    cc.code := vars['Subtag'];
    cc.display := vars['Description'];
    if FExtLanguages.ContainsKey(cc.code) then
      raise ETerminologyError.create('IETFLang: Unable to parse definitions expecting Type: duplicate extlang code '+cc.code+' at line '+inttostr(i+1));
    FExtLanguages.Add(cc.code, cc.Link);
    result := i;
  finally
    cc.Free;
  end;
end;

function TIETFLanguageDefinitions.loadLanguage(vars : TFslStringDictionary; i: integer): integer;
var
  cc : TIETFLanguageLanguage;
begin
  cc := TIETFLanguageLanguage.Create;
  try
    cc.code := vars['Subtag'];
    cc.display := vars['Description'];
    if (vars.ContainsKey('Suppress-Script')) then
      cc.sscript := vars['Suppress-Script'];
    if (vars.ContainsKey('Scope')) then
      cc.scope := vars['Scope'];
    if FLanguages.ContainsKey(cc.code) then
      raise ETerminologyError.create('IETFLang: Unable to parse definitions expecting Type: duplicate language code '+cc.code+' at line '+inttostr(i+1));
    FLanguages.Add(cc.code, cc.Link);
    result := i;
  finally
    cc.Free;
  end;
end;

function TIETFLanguageDefinitions.loadRegion(vars: TFslStringDictionary; i: integer): integer;
var
  cc : TIETFLanguageRegion;
begin
  cc := TIETFLanguageRegion.Create;
  try
    cc.code := vars['Subtag'];
    cc.display := vars['Description'];
    if FRegions.ContainsKey(cc.code) then
      raise ETerminologyError.create('IETFLang: Unable to parse definitions expecting Type: duplicate region code '+cc.code+' at line '+inttostr(i+1));
    FRegions.Add(cc.code, cc.Link);
    result := i;
  finally
    cc.Free;
  end;
end;

function TIETFLanguageDefinitions.loadScript(vars: TFslStringDictionary; i: integer): integer;
var
  cc : TIETFLanguageScript;
begin
  cc := TIETFLanguageScript.Create;
  try
    cc.code := vars['Subtag'];
    cc.display := vars['Description'];
    if FScripts.ContainsKey(cc.code) then
      raise ETerminologyError.create('IETFLang: Unable to parse definitions expecting Type: duplicate script code '+cc.code+' at line '+inttostr(i+1));
    FScripts.Add(cc.code, cc.Link);
    result := i;
  finally
    cc.Free;
  end;
end;

function TIETFLanguageDefinitions.loadVariant(vars: TFslStringDictionary; i: integer): integer;
var
  cc : TIETFLanguageVariant;
begin
  cc := TIETFLanguageVariant.Create;
  try
    cc.code := vars['Subtag'];
    cc.display := vars['Description'];
    if FVariants.ContainsKey(cc.code) then
      raise ETerminologyError.create('IETFLang: Unable to parse definitions expecting Type: duplicate region code '+cc.code+' at line '+inttostr(i+1));
    FVariants.Add(cc.code, cc.Link);
    result := i;
  finally
    cc.Free;
  end;
end;

{ TIETFLanguageLanguage }


function TIETFLanguageLanguage.Link: TIETFLanguageLanguage;
begin
  result := TIETFLanguageLanguage(inherited link);
end;

{ TIETFLanguageExtLang }

function TIETFLanguageExtLang.Link: TIETFLanguageExtLang;
begin
  result := TIETFLanguageExtLang(inherited link);
end;

{ TIETFLanguageScript }

function TIETFLanguageScript.Link: TIETFLanguageScript;
begin
  result := TIETFLanguageScript(inherited link);
end;

{ TIETFLanguageRegion }

function TIETFLanguageRegion.Link: TIETFLanguageRegion;
begin
  result := TIETFLanguageRegion(inherited link);
end;

{ TIETFLanguageVariant }

function TIETFLanguageVariant.Link: TIETFLanguageVariant;
begin
  result := TIETFLanguageVariant(inherited link);
end;

{ TIETFLanguageCodeFilter }

constructor TIETFLanguageCodeFilter.create(component: TIETFLanguageComponent; status: boolean);
begin
  inherited create;
  FComponent := component;
  FStatus := status;
end;

end.
