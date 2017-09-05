unit ResourceEditingSupport;

interface

uses
  SysUtils,
  FHIRTypes;

function displayLang(lang : String) : string;
function displayUse(coding : TFHIRCoding) : string;
function CodeForUse(s : String) : String;


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


end.
