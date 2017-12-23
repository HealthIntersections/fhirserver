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
  OSXUIUtils,
  FHIRTypes;

function checkUpgrade : String;
procedure doUpgrade(newVersion : String);
function translationsImageIndex(element : TFHIRElement): integer;
function langDesc(code : String) : String;
function langCode(desc : String) : String;
function langList : TStringList;

implementation

function checkUpgrade : String;
var
  http : TIdHTTP;
  inc : String;
begin
  http := TIdHTTP.Create(nil);
  try
    // init openSSL
    try
      http.Get('https://hl7.org/fhir/version.info');
    except
    end;
    inc := http.Get('http://www.healthintersections.com.au/FhirServer/toolkit.inc');
    inc := inc.substring(inc.indexof('<td>v')+5);
    inc := inc.substring(0, inc.indexof('</td>'));
    result := inc;
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

end.
