unit ToolKitUtilities;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  SysUtils, Classes,
  IdHTTP, IdSSLOpenSSL, IdComponent,
  FHIR.Ui.OSX,
  FMX.Edit, FMX.ListBox, FMX.StdCtrls,
  fsl_base, fsl_threads,
  fsl_npm_cache,
  fhir_objects, fhir_parser, fhir_factory, fhir_utilities,
  FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Resources.Base, FHIR.Version.Utilities, FHIR.Version.Client, FHIR.Version.Profiles;

function checkSSL : boolean;
function checkUpgrade : String;
procedure doUpgrade(newVersion : String);
function translationsImageIndex(element : TFHIRElement): integer;
function langDesc(code : String) : String;
function langCode(desc : String) : String;
function langList : TStringList;

function readExtension(src : TFHIRDomainResource; url : String) : String; overload;
function readExtension(src : TFHIRElement; url : String) : String; overload;
procedure writeExtension(src : TFHIRElement; url : String; value : String; clss : TFHIRPrimitiveTypeClass); overload;
procedure writeExtension(src : TFHIRDomainResource; url : String; value : String; clss : TFHIRPrimitiveTypeClass); overload;
procedure writeExtension(src : TFHIRDomainResource; url : String; value : TFhirType); overload;

type
  TCodeableConceptLookup = class (TFslObject)
  private
    FValueSet: String;
    FChanging : boolean;
    FEdit : TEdit;
    FList : TListBox;
    FLabel : TLabel;
    FServer : TFHIRClient;
    FExpansion : TFhirValueSet;
    procedure SetValueSet(const Value: String);
    procedure editChange(sender : TObject);
    procedure listDblClick(sender : TObject);
  public
    constructor Create(edit : TEdit; list : TListBox; labl : TLabel; txServer : TFHIRClient);
    destructor Destroy; override;
    property ValueSet : String read FValueSet write SetValueSet;
    procedure initialise(cc : TFHIRCodeableConcept);
    procedure populate(cc : TFHIRCodeableConcept);
  end;

  TToolkitWorkerContext = class (TBaseWorkerContext)
  public
    function Link : TToolkitWorkerContext; overload;

    function expand(vs : TFhirValueSet; options : TExpansionOperationOptionSet = []) : TFHIRValueSet; overload; override;
    function validateCode(system, version, code : String; vs : TFhirValueSet) : TValidationResult; overload; override;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; overload; override;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; overload; override;
//    function getChildMap(profile : TFHIRStructureDefinition; element : TFhirElementDefinition) : TFHIRElementDefinitionList; override;
//    function getStructure(url : String) : TFHIRStructureDefinition; overload; override;
//    function getStructure(ns, name : String) : TFHIRStructureDefinition; overload; override;
//    function getCustomResource(name : String) : TFHIRCustomResourceInformation; override;
//    function hasCustomResource(name : String) : boolean; override;
    function supportsSystem(system, version : string) : boolean; overload; override;
//    function validateCode(system, version, code, display : String) : TValidationResult; overload; virtual; abstract;
//    function validateCode(system, version, code : String; vs : TFhirValueSetW) : TValidationResult; overload; virtual; abstract;
//    function allResourceNames : TArray<String>; overload; virtual; abstract;
//    function nonSecureResourceNames : TArray<String>; overload; virtual; abstract;
//    procedure listStructures(list : TFslList<TFhirStructureDefinitionW>); overload; virtual; abstract;
    function getSearchParameter(resourceType, name : String) : TFHIRResourceV; override;
    procedure loadStructures(id : integer; structures : TFslObject);
    procedure LoadingFinished; override;
  end;

  TBackgroundContextLoadingInformation = class (TFslObject)
  private
    FPackage : String;
    Fversion: String;
    FPackageManager: TFHIRPackageManager;
    FContext: TToolkitWorkerContext;
  public
    constructor Create(packageMgr : TFHIRPackageManager; package, version : string; context : TToolkitWorkerContext);
    destructor Destroy; override;
    property packageMgr : TFHIRPackageManager read FPackageManager;
    property package : String read FPackage;
    property version : String read Fversion;
    property context : TToolkitWorkerContext read FContext;
  end;

  TBackgroundContextLoader = class (TBackgroundTaskEngine)
  private
    FLoadInfo : TPackageLoadingInformation;
    function details : TBackgroundContextLoadingInformation;
    function collection : TFslList<TFHIRStructureDefinition>;
    procedure loadResourceJson(rType, id : String; json : TStream);
  public
    destructor Destroy; override;
    function name : String; override;
    procedure execute; override;
  end;

implementation

function checkSSL : boolean;
var
  http : TIdHTTP;
  ssl : TIdSSLIOHandlerSocketOpenSSL;
begin
  http := TIdHTTP.Create(nil);
  try
    ssl := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
    Try
      http.IOHandler := ssl;
      ssl.SSLOptions.Mode := sslmClient;
      ssl.SSLOptions.Method := sslvTLSv1_2;
      try
        http.Get('https://null.nowhere/something');
        result := true;
      except
      end;
    Finally
      ssl.Free;
    End;
  finally
    http.free;
  end;
end;

function checkUpgrade : String;
var
  http : TIdHTTP;
  ssl : TIdSSLIOHandlerSocketOpenSSL;
  inc : String;
begin
  http := TIdHTTP.Create(nil);
  try
    ssl := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
    Try
      http.IOHandler := ssl;
      ssl.SSLOptions.Mode := sslmClient;
      ssl.SSLOptions.Method := sslvTLSv1_2;
      http.Request.host := 'www.healthintersections.com.au';
      http.Request.CacheControl := 'max-age=0';
      http.Request.UserAgent := 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/66.0.3359.139 Safari/537.36';
      http.Request.Accept := 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8';
      http.Request.AcceptLanguage := 'en-US,en;q=0.9';
      inc := http.Get('http://www.healthintersections.com.au/FhirServer/toolkit.txt');
      inc := inc.substring(inc.indexof('<td>v')+5);
      inc := inc.substring(0, inc.indexof('</td>'));
      result := inc;
    Finally
      ssl.Free;
    End;
  finally
    http.free;
  end;
end;

procedure doUpgrade(newVersion : String);
begin
  {$IFDEF MACOS}
  OpenURL('http://www.healthintersections.com.au/FhirServer/fhir-toolkit-osx-'+newVersion+'.zip');
  {$ELSE}
  OpenURL('http://www.healthintersections.com.au/FhirServer/fhir-toolkit-install-'+newVersion+'.exe');
  {$ENDIF}
end;

function translationsImageIndex(element : TFHIRElement): integer;
var
  ext : TFhirExtension;
begin
  result := 0;
  if element <> nil then
    for ext in element.extensionList do
      if ext.url = 'http://hl7.org/fhir/StructureDefinition/translation' then
        exit(1);
end;

function langDesc(code : String) : String;
begin
  if code = 'ar' then
    result := 'ar - Arabic'
  else if code = 'bn' then
    result := 'bn - Bengali'
  else if code = 'cs' then
    result := 'cs - Czech'
  else if code = 'da' then
    result := 'da - Danish'
  else if code = 'de' then
    result := 'de - German'
  else if code = 'de-AT' then
    result := 'de-AT - German (Austria)'
  else if code = 'de-CH' then
    result := 'de-CH - German (Switzerland)'
  else if code = 'de-DE' then
    result := 'de-DE - German (Germany)'
  else if code = 'el' then
    result := 'el - Greek'
  else if code = 'en' then
    result := 'en - English'
  else if code = 'en-AU' then
    result := 'en-AU - English (Australia)'
  else if code = 'en-CA' then
    result := 'en-CA - English (Canada)'
  else if code = 'en-GB' then
    result := 'en-GB - English (Great Britain)'
  else if code = 'en-IN' then
    result := 'en-IN - English (India)'
  else if code = 'en-NZ' then
    result := 'en-NZ - English (New Zeland)'
  else if code = 'en-SG' then
    result := 'en-SG - English (Singapore)'
  else if code = 'en-US' then
    result := 'en-US - English (United States)'
  else if code = 'es' then
    result := 'es - Spanish'
  else if code = 'es-AR' then
    result := 'es-AR - Spanish (Argentina)'
  else if code = 'es-ES' then
    result := 'es-ES - Spanish (Spain)'
  else if code = 'es-UY' then
    result := 'es-UY - Spanish (Uruguay)'
  else if code = 'fi' then
    result := 'fi - Finnish'
  else if code = 'fr' then
    result := 'fr - French'
  else if code = 'fr-BE' then
    result := 'fr-BE - French (Belgium)'
  else if code = 'fr-CH' then
    result := 'fr-CH - French (Switzerland)'
  else if code = 'fr-FR' then
    result := 'fr-FR - French (France)'
  else if code = 'fy' then
    result := 'fy - Frysian'
  else if code = 'fy-NL' then
    result := 'fy-NL - Frysian (Netherlands)'
  else if code = 'hi' then
    result := 'hi - Hindi'
  else if code = 'hr' then
    result := 'hr - Croatian'
  else if code = 'it' then
    result := 'it - Italian'
  else if code = 'it-CH' then
    result := 'it-CH - Italian (Switzerland)'
  else if code = 'it-IT' then
    result := 'it-IT - Italian (Italy)'
  else if code = 'ja' then
    result := 'ja - Japanese'
  else if code = 'ko' then
    result := 'ko - Korean'
  else if code = 'nl' then
    result := 'nl - Dutch'
  else if code = 'nl-BE' then
    result := 'nl-BE - Dutch (Belgium)'
  else if code = 'nl-NL' then
    result := 'nl-NL - Dutch (Netherlands)'
  else if code = 'no' then
    result := 'no - Norwegian'
  else if code = 'no-NO' then
    result := 'no-NO - Norwegian (Norway)'
  else if code = 'pa' then
    result := 'pa - Punjabi'
  else if code = 'pt' then
    result := 'pt - Portuguese'
  else if code = 'pt-BR' then
    result := 'pt-BR - Portuguese (Brazil)'
  else if code = 'ru' then
    result := 'ru - Russian'
  else if code = 'ru-RU' then
    result := 'ru-RU - Russian (Russia)'
  else if code = 'sr' then
    result := 'sr - Serbian'
  else if code = 'sr-SP' then
    result := 'sr-SP - Serbian (Serbia)'
  else if code = 'sv' then
    result := 'sv - Swedish'
  else if code = 'sv-SE' then
    result := 'sv-SE - Swedish (Sweden)'
  else if code = 'te' then
    result := 'te - Telegu'
  else if code = 'zh' then
    result := 'zh - Chinese'
  else if code = 'zh-CN' then
    result := 'zh-CN - Chinese (China)'
  else if code = 'zh-HK' then
    result := 'zh-HK - Chinese (Hong Kong)'
  else if code = 'zh-SG' then
    result := 'zh-SG - Chinese (Singapore)'
  else if code = 'zh-TW' then
    result := 'zh-TW - Chinese (Taiwan)'
  else
    result := code;
end;

function langCode(desc : String) : String;
var
  i : integer;
begin
  i := desc.IndexOf(' ');
  if i = 0 then
    result := desc
  else
    result := desc.Substring(0, i);
end;

function langList : TStringList;
begin
  result := TStringList.Create;
  result.add('ar');
  result.add('bn');
  result.add('cs');
  result.add('da');
  result.add('de');
  result.add('de-AT');
  result.add('de-CH');
  result.add('de-DE');
  result.add('el');
  result.add('en');
  result.add('en-AU');
  result.add('en-CA');
  result.add('en-GB');
  result.add('en-IN');
  result.add('en-NZ');
  result.add('en-SG');
  result.add('en-US');
  result.add('es');
  result.add('es-AR');
  result.add('es-ES');
  result.add('es-UY');
  result.add('fi');
  result.add('fr');
  result.add('fr-BE');
  result.add('fr-CH');
  result.add('fr-FR');
  result.add('fy');
  result.add('fy-NL');
  result.add('hi');
  result.add('hr');
  result.add('it');
  result.add('it-CH');
  result.add('it-IT');
  result.add('ja');
  result.add('ko');
  result.add('nl');
  result.add('nl-BE');
  result.add('nl-NL');
  result.add('no');
  result.add('no-NO');
  result.add('pa');
  result.add('pt');
  result.add('pt-BR');
  result.add('ru');
  result.add('ru-RU');
  result.add('sr');
  result.add('sr-SP');
  result.add('sv');
  result.add('sv-SE');
  result.add('te');
  result.add('zh');
  result.add('zh-CN');
  result.add('zh-HK');
  result.add('zh-SG');
  result.add('zh-TW');
end;

function readExtension(src : TFHIRDomainResource; url : String) : String;
var
  e : TFHIRExtension;
begin
  result := '';
  for e in src.extensionList do
    if e.url = url then
    begin
      if e.value.isPrimitive then
        exit(e.value.primitiveValue)
      else
        exit(gen(e.value));
    end;
end;

function readExtension(src : TFHIRElement; url : String) : String;
var
  e : TFHIRExtension;
begin
  result := '';
  for e in src.extensionList do
    if e.url = url then
    begin
      if e.value.isPrimitive then
        exit(e.value.primitiveValue)
      else
        exit(gen(e.value));
    end;
end;

procedure writeExtension(src : TFHIRDomainResource; url : String; value : TFhirType); overload;
var
  e : TFHIRExtension;
begin
  if value = nil then
    src.removeExtension(url)
  else
  begin
    for e in src.extensionList do
      if e.url = url then
      begin
        e.value := value;
        exit;
      end;
    e := TFhirExtension.Create;
    try
      e.url := url;
      e.value := value;
      src.extensionList.Add(e.Link);
    finally
      e.Free;
    end;
  end;
end;

procedure writeExtension(src : TFHIRDomainResource; url : String; value : String; clss : TFHIRPrimitiveTypeClass);
var
  e : TFHIRExtension;
begin
  if value = '' then
    src.removeExtension(url)
  else
  begin
    for e in src.extensionList do
      if e.url = url then
      begin
        e.value := clss.Create(value);
        exit;
      end;
    e := TFhirExtension.Create;
    try
      e.url := url;
      e.value := clss.Create(value);
      src.extensionList.Add(e.Link);
    finally
      e.Free;
    end;
  end;
end;

procedure writeExtension(src : TFHIRElement; url : String; value : String; clss : TFHIRPrimitiveTypeClass);
var
  e : TFHIRExtension;
begin
  if value = '' then
    src.removeExtension(url)
  else
  begin
    for e in src.extensionList do
      if e.url = url then
      begin
        e.value := clss.Create(value);
        exit;
      end;
    e := TFhirExtension.Create;
    try
      e.url := url;
      e.value := clss.Create(value);
      src.extensionList.Add(e.Link);
    finally
      e.Free;
    end;
  end;
end;

{ TCodeableConceptLookup }

constructor TCodeableConceptLookup.Create(edit: TEdit; list: TListBox; labl : TLabel; txServer : TFHIRClient);
begin
  inherited create;
  FEdit := edit;
  FList := list;
  FLabel := labl;
  FLabel.text := '';
  FEdit.OnChange := editChange;
  FEdit.OnChangeTracking := editChange;
  FList.OnDblClick := listDblClick;
  FServer := txServer;
end;

destructor TCodeableConceptLookup.Destroy;
begin
  FServer.Free;
  FExpansion.Free;
  inherited;
end;

procedure TCodeableConceptLookup.editChange(sender: TObject);
var
  params : TFHIRParameters;
  ec : TFhirValueSetExpansionContains;
begin
  if FChanging  then
    exit;

  FLabel.text := '';

  if FExpansion <> nil then
  begin
    FExpansion.Free;
    FExpansion := nil;
  end;

  try
    FList.Items.Clear;
    if (FEdit.text <> '') and (FValueSet <> '') then
    begin
      params := TFhirParameters.Create;
      try
        params.AddParameter('url', FValueSet);
        params.AddParameter('filter', FEdit.Text);
        FExpansion := FServer.operation(frtValueSet, 'expand', params) as TFHIRValueSet;
        for ec in FExpansion.expansion.containsList do
          FList.Items.Add(ec.display);
      finally
        params.Free;
      end;
    end;
  except
    on e : exception do
      FLabel.text := e.message;
  end;
end;

procedure TCodeableConceptLookup.initialise(cc: TFHIRCodeableConcept);
begin
  FChanging := true;
  try
    FEdit.Text := cc.text;
    if cc.codingList.Count > 0 then
    begin
      FList.Items.Add(cc.codingList[0].display);
      FExpansion := TFhirValueSet.Create;
      FExpansion.expansion := TFhirValueSetExpansion.Create;
      with FExpansion.expansion.containsList.Append do
      begin
        system := cc.codingList[0].system;
        code := cc.codingList[0].code;
        display := cc.codingList[0].display;
      end;
    end;
  finally
    FChanging := false;
  end;
end;

procedure TCodeableConceptLookup.listDblClick(sender: TObject);
begin
  FChanging := true;
  try
    FEdit.Text := FList.Items[FList.ItemIndex];
  finally
    FChanging := false;
  end;
end;

procedure TCodeableConceptLookup.populate(cc: TFHIRCodeableConcept);
begin
  cc.text := FEdit.Text;
  cc.codingList.Clear;
  if FList.ItemIndex >= 0 then
    with cc.codingList.Append do
    begin
      system := FExpansion.expansion.containsList[FList.ItemIndex].system;
      code := FExpansion.expansion.containsList[FList.ItemIndex].code;
      display := FExpansion.expansion.containsList[FList.ItemIndex].display;
    end;
end;

procedure TCodeableConceptLookup.SetValueSet(const Value: String);
begin
  FList.Items.Clear;
  FValueSet := Value;
  if FEdit.Text <> '' then
    editChange(nil);
end;

{ TToolkitWorkerContext }

function TToolkitWorkerContext.expand(vs: TFhirValueSet; options : TExpansionOperationOptionSet = []): TFHIRValueSet;
begin
  raise EFslException.Create('Not implemented in the toolkit');
end;

function TToolkitWorkerContext.validateCode(system, version, code: String; vs: TFhirValueSet): TValidationResult;
begin
  raise EFslException.Create('Not implemented in the toolkit');
end;

function TToolkitWorkerContext.validateCode(code: TFHIRCoding; vs: TFhirValueSet): TValidationResult;
begin
  raise EFslException.Create('Not implemented in the toolkit');
end;

function TToolkitWorkerContext.getSearchParameter(resourceType, name: String): TFHIRResourceV;
begin
  result := nil;
end;

function TToolkitWorkerContext.Link: TToolkitWorkerContext;
begin
  result := TToolkitWorkerContext(inherited link);
end;

procedure TToolkitWorkerContext.LoadingFinished;
begin
  inherited;

end;

procedure TToolkitWorkerContext.loadStructures(id: integer; structures: TFslObject);
var
  sd : TFHIRStructureDefinition;
begin
  for sd in Structures as TFslList<TFHIRStructureDefinition> do
    seeResource(sd);
end;

function TToolkitWorkerContext.supportsSystem(system, version: string): boolean;
begin
  raise EFslException.Create('Not implemented in the toolkit');
end;

function TToolkitWorkerContext.validateCode(code: TFHIRCodeableConcept; vs: TFhirValueSet): TValidationResult;
begin
  raise EFslException.Create('Not implemented in the toolkit');
end;

{ TBackgroundContextLoadingInformation }

constructor TBackgroundContextLoadingInformation.Create(packageMgr: TFHIRPackageManager; package, version: string; context : TToolkitWorkerContext);
begin
  inherited create;
  FPackageManager := packageMgr;
  FPackage := package;
  Fversion := version;
  FContext := context;
end;

destructor TBackgroundContextLoadingInformation.Destroy;
begin
  FPackageManager.Free;
  FContext.Free;
  inherited;
end;

{ TBackgroundContextLoader }

function TBackgroundContextLoader.collection: TFslList<TFHIRStructureDefinition>;
begin
  result := Response as TFslList<TFHIRStructureDefinition>;
end;

destructor TBackgroundContextLoader.Destroy;
begin
  FLoadInfo.Free;
  inherited;
end;

function TBackgroundContextLoader.details: TBackgroundContextLoadingInformation;
begin
  result := request as TBackgroundContextLoadingInformation;
end;

procedure TBackgroundContextLoader.execute;
begin
  response := TFslList<TFHIRStructureDefinition>.create;
  if FLoadInfo = nil then
  begin
    FLoadInfo := TPackageLoadingInformation.Create(details.version);
    FLoadInfo.OnLoadEvent := loadResourceJson;
  end;
  details.packageMgr.loadPackage(details.package,  details.version, ['StructureDefinition'], FLoadInfo);
end;

procedure TBackgroundContextLoader.loadResourceJson(rType, id: String; json: TStream);
var
  p : TFHIRParser;
begin
  p := details.Context.Factory.makeParser(details.context.link, ffJson, details.Context.lang);
  try
    p.source := json;
    p.Parse;
    collection.Add((p.resource as TFHIRStructureDefinition).Link);
  finally
    p.Free;
  end;
end;

function TBackgroundContextLoader.name: String;
begin
  result := 'Core Package Loader';
end;

end.
