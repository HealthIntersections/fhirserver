unit ResourceEditingSupport;

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
  SysUtils,
  fsl_utilities, fsl_http,
  FHIR.Version.Types, FHIR.Version.Resources,
  FMX.DateTimeCtrls;

function displayLang(lang : String) : string;
function displayUse(coding : TFHIRCoding) : string;
function CodeForUse(s : String) : String;

procedure presentDateTime(date : TFHIRDateTime; ded: TDateEdit; tdt: TTimeEdit);
function storeDateTime(ded: TDateEdit; tdt: TTimeEdit): TFHIRDateTime;

function getJurisdictionSearch(i: integer): string;
function readJurisdiction(res : TFhirMetadataResource): String;

function FMXescape(s : String) : String;

implementation

function displayLang(lang : String) : string;
begin
  if lang = '' then
    result := ''
  else if lang.StartsWith('bn') then result := 'bn (Bengali)'
  else if lang.startsWith('cs') then result := 'cs (Czech)'
  else if lang.startsWith('da') then result := 'da (Danish)'
  else if lang.startsWith('de') then result := 'de (German)'
  else if lang.startsWith('el') then result := 'el (Greek)'
  else if lang.startsWith('en') then result := 'en (English)'
  else if lang.startsWith('es') then result := 'es (Spanish)'
  else if lang.startsWith('fi') then result := 'fi (Finnish)'
  else if lang.startsWith('fr') then result := 'fr (French)'
  else if lang.startsWith('fy') then result := 'fy (Frysian)'
  else if lang.startsWith('hi') then result := 'hi (Hindi)'
  else if lang.startsWith('hr') then result := 'hr (Croatian)'
  else if lang.startsWith('it') then result := 'it (Italian)'
  else if lang.startsWith('ja') then result := 'ja (Japanese)'
  else if lang.startsWith('ko') then result := 'ko (Korean)'
  else if lang.startsWith('nl') then result := 'nl (Dutch)'
  else if lang.startsWith('no') then result := 'no (Norwegian)'
  else if lang.startsWith('pa') then result := 'pa (Punjabi)'
  else if lang.startsWith('pt') then result := 'pt (Portuguese)'
  else if lang.startsWith('ru') then result := 'ru (Russian)'
  else if lang.startsWith('sr') then result := 'sr (Serbian)'
  else if lang.startsWith('sv') then result := 'sv (Swedish)'
  else if lang.startsWith('te') then result := 'te (Telegu)'
  else if lang.startsWith('zh') then result := 'zh (Chinese))'
  else
    result := lang;
end;

function displayUse(coding : TFHIRCoding) : string;
begin
  if (coding = nil) then
    exit('');

  if (coding.system = 'http://snomed.info/sct') then
  begin
    if coding.code = '900000000000003001' then
      exit('Fully specified name');
    if coding.code = '900000000000013009' then
      exit('Synonym');
    if coding.code = '900000000000550004' then
      exit('Definition');
  end;
  if coding.display <> '' then
    result := coding.display
  else
    result := '';
end;

function CodeForUse(s : String) : String;
begin
  if (s = 'Fully specified name') then
    result := '900000000000003001'
  else if (s = 'Synonym') then
    result := '900000000000013009'
  else
    result := '900000000000550004';
end;

procedure presentDateTime(date : TFHIRDateTime; ded: TDateEdit; tdt: TTimeEdit);
begin
  if date = nil then
  begin
    ded.Text := '';
    tdt.Text := '';
  end
  else
  begin
    ded.DateTime := trunc(date.value.DateTime);
    if ded.DateTime = date.value.DateTime then
      tdt.Text := ''
    else
      tdt.DateTime := date.value.DateTime - ded.DateTime;
  end;
end;


function storeDateTime(ded: TDateEdit; tdt: TTimeEdit): TFHIRDateTime;
begin
  if ded.Text = '' then
    result := nil
  else
    result := TFhirDateTime.Create(TFslDateTime.make(trunc(ded.DateTime) + tdt.Time, dttzLocal));
end;

function getJurisdictionSearch(i: integer): string;
begin
  case i of
    1:result := 'urn:iso:std:iso:3166|AT';
    2:result := 'urn:iso:std:iso:3166|AU';
    3:result := 'urn:iso:std:iso:3166|BR';
    4:result := 'urn:iso:std:iso:3166|CA';
    5:result := 'urn:iso:std:iso:3166|CH';
    6:result := 'urn:iso:std:iso:3166|CL';
    7:result := 'urn:iso:std:iso:3166|CN';
    8:result := 'urn:iso:std:iso:3166|DE';
    9:result := 'urn:iso:std:iso:3166|DK';
    10:result := 'urn:iso:std:iso:3166|EE';
    11:result := 'urn:iso:std:iso:3166|ES';
    12:result := 'urn:iso:std:iso:3166|FI';
    13:result := 'urn:iso:std:iso:3166|FR';
    14:result := 'urn:iso:std:iso:3166|GB';
    15:result := 'urn:iso:std:iso:3166|NL';
    16:result := 'urn:iso:std:iso:3166|NO';
    17:result := 'urn:iso:std:iso:3166|NZ';
    18:result := 'urn:iso:std:iso:3166|RU';
    19:result := 'urn:iso:std:iso:3166|US';
    21:result := 'urn:iso:std:iso:3166|VN';
    22:result := 'http://unstats.un.org/unsd/methods/m49/m49.htm|001';
    23:result := 'http://unstats.un.org/unsd/methods/m49/m49.htm|002';
    24:result := 'http://unstats.un.org/unsd/methods/m49/m49.htm|019';
    25:result := 'http://unstats.un.org/unsd/methods/m49/m49.htm|142';
    26:result := 'http://unstats.un.org/unsd/methods/m49/m49.htm|150';
    27:result := 'http://unstats.un.org/unsd/methods/m49/m49.htm|053';
  else
    result := '';
  end;
end;

function readJurisdiction(res : TFhirMetadataResource): String;
var
  cc : TFhirCodeableConcept;
  c : TFhirCoding;
begin
  result := '';
  for cc in res.jurisdictionList do
    for c in cc.codingList do
    begin
      if c.system = 'urn:iso:std:iso:3166' then
      begin
        if c.code = 'AT' then exit('Austraia');
        if c.code = 'AU' then exit('Australia');
        if c.code = 'BR' then exit('Brazil');
        if c.code = 'CA' then exit('Canada');
        if c.code = 'CH' then exit('Switzerland');
        if c.code = 'CL' then exit('Chile');
        if c.code = 'CN' then exit('China');
        if c.code = 'DE' then exit('Germany');
        if c.code = 'DK' then exit('Denmark');
        if c.code = 'EE' then exit('Estonia');
        if c.code = 'ES' then exit('Spain');
        if c.code = 'FI' then exit('Finland');
        if c.code = 'FR' then exit('France');
        if c.code = 'GB' then exit('UK');
        if c.code = 'NL' then exit('Netherlands');
        if c.code = 'NO' then exit('Norway');
        if c.code = 'NZ' then exit('NZ');
        if c.code = 'RU' then exit('Russia');
        if c.code = 'US' then exit('USA');
        if c.code = 'VN' then exit('Vietnam');
      end
      else if c.system = 'http://unstats.un.org/unsd/methods/m49/m49.htm' then
      begin
        if c.code = '001' { World } then exit('World');
        if c.code = '002' { Africa } then exit('Africa');
        if c.code = '019' { Americas } then exit('Americas');
        if c.code = '142' { Asia } then exit('Asia');
        if c.code = '150' { Europe } then exit('Europe');
        if c.code = '053' { Australia and New Zealand } then exit('Australasia');
      end
    end;
end;

function FMXescape(s : String) : String;
begin
  result := s.replace('&', '&&');
end;

end.
