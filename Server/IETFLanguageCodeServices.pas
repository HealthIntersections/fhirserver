unit IETFLanguageCodeServices;

interface

uses
  SysUtils, Classes,
  StringSupport,
  AdvObjects, AdvObjectLists, AdvFiles, AdvTextExtractors, AdvStringIntegerMatches,
  FHIRTypes, FHIRComponents, FHIRResources, TerminologyServices, DateAndTime;

type
  TIETFLanguageCodeConcept = class (TCodeSystemProviderContext)
  private
    code : string;
  public
    constructor create(code : String);
  end;

  TIETFLanguageCodeFilter = class (TCodeSystemProviderFilterContext)
  end;

  TIETFLanguageCodePrep = class (TCodeSystemProviderFilterPreparationContext)
  end;

  TIETFLanguageCodeServices = class (TCodeSystemProvider)
  private
    function describeLanguage(code : string) : String;
    function describeCountry(code : string) : String;
  public
    Constructor Create;
    Destructor Destroy; Override;
    Function Link : TIETFLanguageCodeServices; overload;

    function TotalCount : integer;  override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function system(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String) : TCodeSystemProviderContext; override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext) : string; override;
    procedure Displays(code : String; list : TStringList); override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList); override;
    function Definition(context : TCodeSystemProviderContext) : string; override;

    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
    function prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean; override;

    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
  end;

implementation

{
a single primary language subtag based on a two-letter language code from ISO 639-1 (2002) or a three-letter code from ISO 639-2 (1998), ISO 639-3 (2007) or ISO 639-5 (2008), or registered through the BCP 47 process and composed of five to eight letters;
up to three optional extended language subtags composed of three letters each, separated by hyphens; (There is currently no extended language subtag registered in the Language Subtag Registry without an equivalent and preferred primary language subtag. This component of language tags is preserved for backwards compatibility and to allow for future parts of ISO 639.)
an optional script subtag, based on a four-letter script code from ISO 15924 (usually written in title case);
an optional region subtag based on a two-letter country code from ISO 3166-1 alpha-2 (usually written in upper case), or a three-digit code from UN M.49 for geographical regions;
optional variant subtags, separated by hyphens, each composed of five to eight letters, or of four characters starting with a digit; (Variant subtags are registered with IANA and not associated with any external standard.)
optional extension subtags, separated by hyphens, each composed of a single character, with the exception of the letter x, and a hyphen followed by one or more subtags of two to eight characters each, separated by hyphens;
an optional private-use subtag, composed of the letter x and a hyphen followed by subtags of one to eight characters each, separated by hyphens.

}

{ TIETFLanguageCodeServices }

Constructor TIETFLanguageCodeServices.create;
begin
  inherited Create;

end;


function TIETFLanguageCodeServices.TotalCount : integer;
begin
  result := -1;   // not bounded
end;


function TIETFLanguageCodeServices.system(context : TCodeSystemProviderContext) : String;
begin
  result := 'urn:ietf:params:language';
end;

function TIETFLanguageCodeServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TIETFLanguageCodeServices.getDisplay(code : String):String;
var
  parts : TArray<String>;
begin
  if (code = '') then
    result := '??'
  else
  begin
    parts := code.Split(['-']);
    result := describeLanguage(parts[0]);
    if length(parts) > 1 then
      result := result + '('+describeCountry(parts[1])+')';
  end;
end;

function TIETFLanguageCodeServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  raise Exception.Create('not done yet');
end;

procedure TIETFLanguageCodeServices.Displays(code : String; list : TStringList);
begin
  list.Add(getDisplay(code));
end;


function TIETFLanguageCodeServices.locate(code : String) : TCodeSystemProviderContext;
begin
  result := TIETFLanguageCodeConcept.Create(code);
end;


function TIETFLanguageCodeServices.Code(context : TCodeSystemProviderContext) : string;
begin
  result := TIETFLanguageCodeConcept(context).code;
end;

function TIETFLanguageCodeServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TIETFLanguageCodeServices.describeCountry(code: string): String;
begin
  if (code = 'US') then
    result := 'US'
  else if (code = 'PT') then
    result := 'Portugal'
  else if (code = 'ES') then
    result := 'Spain'
  else if (code = 'CN') then
    result := 'China'
  else if (code = 'AD') then result := 'Andorra'
  else if (code = 'AE') then result := 'United Arab Emirates'
  else if (code = 'AF') then result := 'Afghanistan'
  else if (code = 'AG') then result := 'Antigua and Barbuda'
  else if (code = 'AI') then result := 'Anguilla'
  else if (code = 'AL') then result := 'Albania'
  else if (code = 'AM') then result := 'Armenia'
  else if (code = 'AO') then result := 'Angola'
  else if (code = 'AQ') then result := 'Antarctica'
  else if (code = 'AR') then result := 'Argentina'
  else if (code = 'AS') then result := 'American Samoa'
  else if (code = 'AT') then result := 'Austria'
  else if (code = 'AU') then result := 'Australia'
  else if (code = 'AW') then result := 'Aruba'
  else if (code = 'AX') then result := 'Åland Islands'
  else if (code = 'AZ') then result := 'Azerbaijan'
  else if (code = 'BA') then result := 'Bosnia and Herzegovina'
  else if (code = 'BB') then result := 'Barbados'
  else if (code = 'BD') then result := 'Bangladesh'
  else if (code = 'BE') then result := 'Belgium'
  else if (code = 'BF') then result := 'Burkina Faso'
  else if (code = 'BG') then result := 'Bulgaria'
  else if (code = 'BH') then result := 'Bahrain'
  else if (code = 'BI') then result := 'Burundi'
  else if (code = 'BJ') then result := 'Benin'
  else if (code = 'BL') then result := 'Saint Barthélemy'
  else if (code = 'BM') then result := 'Bermuda'
  else if (code = 'BN') then result := 'Brunei Darussalam'
  else if (code = 'BO') then result := 'Bolivia, Plurinational State of'
  else if (code = 'BQ') then result := 'Bonaire, Sint Eustatius and Saba'
  else if (code = 'BR') then result := 'Brazil'
  else if (code = 'BS') then result := 'Bahamas'
  else if (code = 'BT') then result := 'Bhutan'
  else if (code = 'BV') then result := 'Bouvet Island'
  else if (code = 'BW') then result := 'Botswana'
  else if (code = 'BY') then result := 'Belarus'
  else if (code = 'BZ') then result := 'Belize'
  else if (code = 'CA') then result := 'Canada'
  else if (code = 'CC') then result := 'Cocos (Keeling) Islands'
  else if (code = 'CD') then result := 'Congo, the Democratic Republic of the'
  else if (code = 'CF') then result := 'Central African Republic'
  else if (code = 'CG') then result := 'Congo'
  else if (code = 'CH') then result := 'Switzerland'
  else if (code = 'CI') then result := 'Côte d''Ivoire'
  else if (code = 'CK') then result := 'Cook Islands'
  else if (code = 'CL') then result := 'Chile'
  else if (code = 'CM') then result := 'Cameroon'
  else if (code = 'CN') then result := 'China'
  else if (code = 'CO') then result := 'Colombia'
  else if (code = 'CR') then result := 'Costa Rica'
  else if (code = 'CU') then result := 'Cuba'
  else if (code = 'CV') then result := 'Cabo Verde'
  else if (code = 'CW') then result := 'Curaçao'
  else if (code = 'CX') then result := 'Christmas Island'
  else if (code = 'CY') then result := 'Cyprus'
  else if (code = 'CZ') then result := 'Czech Republic'
  else if (code = 'DE') then result := 'Germany'
  else if (code = 'DJ') then result := 'Djibouti'
  else if (code = 'DK') then result := 'Denmark'
  else if (code = 'DM') then result := 'Dominica'
  else if (code = 'DO') then result := 'Dominican Republic'
  else if (code = 'DZ') then result := 'Algeria'
  else if (code = 'EC') then result := 'Ecuador'
  else if (code = 'EE') then result := 'Estonia'
  else if (code = 'EG') then result := 'Egypt'
  else if (code = 'EH') then result := 'Western Sahara'
  else if (code = 'ER') then result := 'Eritrea'
  else if (code = 'ES') then result := 'Spain'
  else if (code = 'ET') then result := 'Ethiopia'
  else if (code = 'FI') then result := 'Finland'
  else if (code = 'FJ') then result := 'Fiji'
  else if (code = 'FK') then result := 'Falkland Islands (Malvinas)'
  else if (code = 'FM') then result := 'Micronesia, Federated States of'
  else if (code = 'FO') then result := 'Faroe Islands'
  else if (code = 'FR') then result := 'France'
  else if (code = 'GA') then result := 'Gabon'
  else if (code = 'GB') then result := 'United Kingdom'
  else if (code = 'GD') then result := 'Grenada'
  else if (code = 'GE') then result := 'Georgia'
  else if (code = 'GF') then result := 'French Guiana'
  else if (code = 'GG') then result := 'Guernsey'
  else if (code = 'GH') then result := 'Ghana'
  else if (code = 'GI') then result := 'Gibraltar'
  else if (code = 'GL') then result := 'Greenland'
  else if (code = 'GM') then result := 'Gambia'
  else if (code = 'GN') then result := 'Guinea'
  else if (code = 'GP') then result := 'Guadeloupe'
  else if (code = 'GQ') then result := 'Equatorial Guinea'
  else if (code = 'GR') then result := 'Greece'
  else if (code = 'GS') then result := 'South Georgia and the South Sandwich Islands'
  else if (code = 'GT') then result := 'Guatemala'
  else if (code = 'GU') then result := 'Guam'
  else if (code = 'GW') then result := 'Guinea-Bissau'
  else if (code = 'GY') then result := 'Guyana'
  else if (code = 'HK') then result := 'Hong Kong'
  else if (code = 'HM') then result := 'Heard Island and McDonald Islands'
  else if (code = 'HN') then result := 'Honduras'
  else if (code = 'HR') then result := 'Croatia'
  else if (code = 'HT') then result := 'Haiti'
  else if (code = 'HU') then result := 'Hungary'
  else if (code = 'ID') then result := 'Indonesia'
  else if (code = 'IE') then result := 'Ireland'
  else if (code = 'IL') then result := 'Israel'
  else if (code = 'IM') then result := 'Isle of Man'
  else if (code = 'IN') then result := 'India'
  else if (code = 'IO') then result := 'British Indian Ocean Territory'
  else if (code = 'IQ') then result := 'Iraq'
  else if (code = 'IR') then result := 'Iran, Islamic Republic of'
  else if (code = 'IS') then result := 'Iceland'
  else if (code = 'IT') then result := 'Italy'
  else if (code = 'JE') then result := 'Jersey'
  else if (code = 'JM') then result := 'Jamaica'
  else if (code = 'JO') then result := 'Jordan'
  else if (code = 'JP') then result := 'Japan'
  else if (code = 'KE') then result := 'Kenya'
  else if (code = 'KG') then result := 'Kyrgyzstan'
  else if (code = 'KH') then result := 'Cambodia'
  else if (code = 'KI') then result := 'Kiribati'
  else if (code = 'KM') then result := 'Comoros'
  else if (code = 'KN') then result := 'Saint Kitts and Nevis'
  else if (code = 'KP') then result := 'Korea, Democratic People''s Republic of'
  else if (code = 'KR') then result := 'Korea, Republic of'
  else if (code = 'KW') then result := 'Kuwait'
  else if (code = 'KY') then result := 'Cayman Islands'
  else if (code = 'KZ') then result := 'Kazakhstan'
  else if (code = 'LA') then result := 'Lao People''s Democratic Republic'
  else if (code = 'LB') then result := 'Lebanon'
  else if (code = 'LC') then result := 'Saint Lucia'
  else if (code = 'LI') then result := 'Liechtenstein'
  else if (code = 'LK') then result := 'Sri Lanka'
  else if (code = 'LR') then result := 'Liberia'
  else if (code = 'LS') then result := 'Lesotho'
  else if (code = 'LT') then result := 'Lithuania'
  else if (code = 'LU') then result := 'Luxembourg'
  else if (code = 'LV') then result := 'Latvia'
  else if (code = 'LY') then result := 'Libya'
  else if (code = 'MA') then result := 'Morocco'
  else if (code = 'MC') then result := 'Monaco'
  else if (code = 'MD') then result := 'Moldova, Republic of'
  else if (code = 'ME') then result := 'Montenegro'
  else if (code = 'MF') then result := 'Saint Martin (French part)'
  else if (code = 'MG') then result := 'Madagascar'
  else if (code = 'MH') then result := 'Marshall Islands'
  else if (code = 'MK') then result := 'Macedonia, the former Yugoslav Republic of'
  else if (code = 'ML') then result := 'Mali'
  else if (code = 'MM') then result := 'Myanmar'
  else if (code = 'MN') then result := 'Mongolia'
  else if (code = 'MO') then result := 'Macao'
  else if (code = 'MP') then result := 'Northern Mariana Islands'
  else if (code = 'MQ') then result := 'Martinique'
  else if (code = 'MR') then result := 'Mauritania'
  else if (code = 'MS') then result := 'Montserrat'
  else if (code = 'MT') then result := 'Malta'
  else if (code = 'MU') then result := 'Mauritius'
  else if (code = 'MV') then result := 'Maldives'
  else if (code = 'MW') then result := 'Malawi'
  else if (code = 'MX') then result := 'Mexico'
  else if (code = 'MY') then result := 'Malaysia'
  else if (code = 'MZ') then result := 'Mozambique'
  else if (code = 'NA') then result := 'Namibia'
  else if (code = 'NC') then result := 'New Caledonia'
  else if (code = 'NE') then result := 'Niger'
  else if (code = 'NF') then result := 'Norfolk Island'
  else if (code = 'NG') then result := 'Nigeria'
  else if (code = 'NI') then result := 'Nicaragua'
  else if (code = 'NL') then result := 'Netherlands'
  else if (code = 'NO') then result := 'Norway'
  else if (code = 'NP') then result := 'Nepal'
  else if (code = 'NR') then result := 'Nauru'
  else if (code = 'NU') then result := 'Niue'
  else if (code = 'NZ') then result := 'New Zealand'
  else if (code = 'OM') then result := 'Oman'
  else if (code = 'PA') then result := 'Panama'
  else if (code = 'PE') then result := 'Peru'
  else if (code = 'PF') then result := 'French Polynesia'
  else if (code = 'PG') then result := 'Papua New Guinea'
  else if (code = 'PH') then result := 'Philippines'
  else if (code = 'PK') then result := 'Pakistan'
  else if (code = 'PL') then result := 'Poland'
  else if (code = 'PM') then result := 'Saint Pierre and Miquelon'
  else if (code = 'PN') then result := 'Pitcairn'
  else if (code = 'PR') then result := 'Puerto Rico'
  else if (code = 'PS') then result := 'Palestine, State of'
  else if (code = 'PT') then result := 'Portugal'
  else if (code = 'PW') then result := 'Palau'
  else if (code = 'PY') then result := 'Paraguay'
  else if (code = 'QA') then result := 'Qatar'
  else if (code = 'RE') then result := 'Réunion'
  else if (code = 'RO') then result := 'Romania'
  else if (code = 'RS') then result := 'Serbia'
  else if (code = 'RU') then result := 'Russian Federation'
  else if (code = 'RW') then result := 'Rwanda'
  else if (code = 'SA') then result := 'Saudi Arabia'
  else if (code = 'SB') then result := 'Solomon Islands'
  else if (code = 'SC') then result := 'Seychelles'
  else if (code = 'SD') then result := 'Sudan'
  else if (code = 'SE') then result := 'Sweden'
  else if (code = 'SG') then result := 'Singapore'
  else if (code = 'SH') then result := 'Saint Helena, Ascension and Tristan da Cunha'
  else if (code = 'SI') then result := 'Slovenia'
  else if (code = 'SJ') then result := 'Svalbard and Jan Mayen'
  else if (code = 'SK') then result := 'Slovakia'
  else if (code = 'SL') then result := 'Sierra Leone'
  else if (code = 'SM') then result := 'San Marino'
  else if (code = 'SN') then result := 'Senegal'
  else if (code = 'SO') then result := 'Somalia'
  else if (code = 'SR') then result := 'Suriname'
  else if (code = 'SS') then result := 'South Sudan'
  else if (code = 'ST') then result := 'Sao Tome and Principe'
  else if (code = 'SV') then result := 'El Salvador'
  else if (code = 'SX') then result := 'Sint Maarten (Dutch part)'
  else if (code = 'SY') then result := 'Syrian Arab Republic'
  else if (code = 'SZ') then result := 'Swaziland'
  else if (code = 'TC') then result := 'Turks and Caicos Islands'
  else if (code = 'TD') then result := 'Chad'
  else if (code = 'TF') then result := 'French Southern Territories'
  else if (code = 'TG') then result := 'Togo'
  else if (code = 'TH') then result := 'Thailand'
  else if (code = 'TJ') then result := 'Tajikistan'
  else if (code = 'TK') then result := 'Tokelau'
  else if (code = 'TL') then result := 'Timor-Leste'
  else if (code = 'TM') then result := 'Turkmenistan'
  else if (code = 'TN') then result := 'Tunisia'
  else if (code = 'TO') then result := 'Tonga'
  else if (code = 'TR') then result := 'Turkey'
  else if (code = 'TT') then result := 'Trinidad and Tobago'
  else if (code = 'TV') then result := 'Tuvalu'
  else if (code = 'TW') then result := 'Taiwan, Province of China'
  else if (code = 'TZ') then result := 'Tanzania, United Republic of'
  else if (code = 'UA') then result := 'Ukraine'
  else if (code = 'UG') then result := 'Uganda'
  else if (code = 'UM') then result := 'United States Minor Outlying Islands'
  else if (code = 'US') then result := 'United States'
  else if (code = 'UY') then result := 'Uruguay'
  else if (code = 'UZ') then result := 'Uzbekistan'
  else if (code = 'VA') then result := 'Holy See (Vatican City State)'
  else if (code = 'VC') then result := 'Saint Vincent and the Grenadines'
  else if (code = 'VE') then result := 'Venezuela, Bolivarian Republic of'
  else if (code = 'VG') then result := 'Virgin Islands, British'
  else if (code = 'VI') then result := 'Virgin Islands, U.S.'
  else if (code = 'VN') then result := 'Viet Nam'
  else if (code = 'VU') then result := 'Vanuatu'
  else if (code = 'WF') then result := 'Wallis and Futuna'
  else if (code = 'WS') then result := 'Samoa'
  else if (code = 'YE') then result := 'Yemen'
  else if (code = 'YT') then result := 'Mayotte'
  else if (code = 'ZA') then result := 'South Africa'
  else if (code = 'ZM') then result := 'Zambia'
  else if (code = 'ZW') then result := 'Zimbabwe'
  else
    raise Exception.Create('Unknown Country "'+code+'"');
end;



function TIETFLanguageCodeServices.describeLanguage(code: string): String;
begin
  if (code = 'en') then
    result := 'English'
  else if (code = 'es') then
    result := 'Spanish'
  else if (code = 'pt') then
    result := 'Portuguese'
  else if (code = 'zh') then
    result := 'Chinese'
  else if (code = 'ab') then result := 'Abkhazian'
  else if (code = 'aa') then result := 'Afar'
  else if (code = 'af') then result := 'Afrikaans'
  else if (code = 'sq') then result := 'Albanian'
  else if (code = 'am') then result := 'sqsqAmharic'
  else if (code = 'ar') then result := 'amamamArabic'
  else if (code = 'an') then result := 'Aragonese'
  else if (code = 'hy') then result := 'Armenian'
  else if (code = 'as') then result := 'Assamese'
  else if (code = 'ay') then result := 'Aymara'
  else if (code = 'az') then result := 'Azerbaijani'
  else if (code = 'ba') then result := 'Bashkir'
  else if (code = 'eu') then result := 'Basque'
  else if (code = 'bn') then result := 'Bengali (Bangla)'
  else if (code = 'dz') then result := 'Bhutani'
  else if (code = 'bh') then result := 'dzdzBihari'
  else if (code = 'bi') then result := 'bhbhBislama'
  else if (code = 'br') then result := 'bibiBreton'
  else if (code = 'bg') then result := 'Bulgarian'
  else if (code = 'my') then result := 'bgbgBurmese'
  else if (code = 'be') then result := 'mymyByelorussian (Belarusian)'
  else if (code = 'km') then result := 'Cambodian'
  else if (code = 'ca') then result := 'Catalan'
  else if (code = 'zh') then result := 'Chinese'
  else if (code = 'co') then result := 'Corsican'
  else if (code = 'hr') then result := 'Croatian'
  else if (code = 'cs') then result := 'Czech'
  else if (code = 'da') then result := 'Danish'
  else if (code = 'nl') then result := 'Dutch'
  else if (code = 'en') then result := 'English'
  else if (code = 'eo') then result := 'Esperanto'
  else if (code = 'et') then result := 'Estonian'
  else if (code = 'fo') then result := 'Faeroese'
  else if (code = 'fa') then result := 'Farsi'
  else if (code = 'fj') then result := 'Fiji'
  else if (code = 'fi') then result := 'Finnish'
  else if (code = 'fr') then result := 'French'
  else if (code = 'fy') then result := 'Frisian'
  else if (code = 'gl') then result := 'Galician'
  else if (code = 'gd') then result := 'Gaelic (Scottish)'
  else if (code = 'gv') then result := 'Gaelic (Manx)'
  else if (code = 'ka') then result := 'Georgian'
  else if (code = 'de') then result := 'German'
  else if (code = 'el') then result := 'Greek'
  else if (code = 'kl') then result := 'Greenlandic'
  else if (code = 'gn') then result := 'Guarani'
  else if (code = 'gu') then result := 'Gujarati'
  else if (code = 'ht') then result := 'Haitian Creole'
  else if (code = 'ha') then result := 'Hausa'
  else if (code = 'he') then result := 'Hebrew'
  else if (code = 'iw') then result := 'Hebrew'
  else if (code = 'hi') then result := 'Hindi'
  else if (code = 'hu') then result := 'Hungarian'
  else if (code = 'is') then result := 'Icelandic'
  else if (code = 'io') then result := 'Ido'
  else if (code = 'id') then result := 'Indonesian'
  else if (code = 'in') then result := 'Indonesian'
  else if (code = 'ia') then result := 'Interlingua'
  else if (code = 'ie') then result := 'Interlingue'
  else if (code = 'iu') then result := 'Inuktitut'
  else if (code = 'ik') then result := 'Inupiak'
  else if (code = 'ga') then result := 'Irish'
  else if (code = 'it') then result := 'Italian'
  else if (code = 'ja') then result := 'Japanese'
  else if (code = 'jv') then result := 'Javanese'
  else if (code = 'kn') then result := 'Kannada'
  else if (code = 'ks') then result := 'Kashmiri'
  else if (code = 'kk') then result := 'Kazakh'
  else if (code = 'rw') then result := 'Kinyarwanda (Ruanda)'
  else if (code = 'ky') then result := 'Kirghiz'
  else if (code = 'rn') then result := 'Kirundi (Rundi)'
  else if (code = 'ko') then result := 'Korean'
  else if (code = 'ku') then result := 'Kurdish'
  else if (code = 'lo') then result := 'Laothian'
  else if (code = 'la') then result := 'Latin'
  else if (code = 'lv') then result := 'Latvian (Lettish)'
  else if (code = 'li') then result := 'Limburgish ( Limburger)'
  else if (code = 'ln') then result := 'liliLingala'
  else if (code = 'lt') then result := 'lnlnlnLithuanian'
  else if (code = 'mk') then result := 'ltltMacedonian'
  else if (code = 'mg') then result := 'mkmkMalagasy'
  else if (code = 'ms') then result := 'mgmgMalay'
  else if (code = 'ml') then result := 'msmsMalayalam'
  else if (code = 'mt') then result := 'mlmlMaltese'
  else if (code = 'mi') then result := 'mtmtMaori'
  else if (code = 'mr') then result := 'Marathi'
  else if (code = 'mo') then result := 'Moldavian'
  else if (code = 'mn') then result := 'momoMongolian'
  else if (code = 'na') then result := 'mnmnNauru'
  else if (code = 'ne') then result := 'Nepali'
  else if (code = 'no') then result := 'Norwegian'
  else if (code = 'oc') then result := 'Occitan'
  else if (code = 'or') then result := 'Oriya'
  else if (code = 'om') then result := 'Oromo (Afaan Oromo)'
  else if (code = 'ps') then result := 'Pashto (Pushto)'
  else if (code = 'pl') then result := 'Polish'
  else if (code = 'pt') then result := 'Portuguese'
  else if (code = 'pa') then result := 'Punjabi'
  else if (code = 'qu') then result := 'Quechua'
  else if (code = 'rm') then result := 'Rhaeto-Romance'
  else if (code = 'ro') then result := 'Romanian'
  else if (code = 'ru') then result := 'Russian'
  else if (code = 'sm') then result := 'Samoan'
  else if (code = 'sg') then result := 'Sangro'
  else if (code = 'sa') then result := 'Sanskrit'
  else if (code = 'sr') then result := 'Serbian'
  else if (code = 'sh') then result := 'Serbo-Croatian'
  else if (code = 'st') then result := 'Sesotho'
  else if (code = 'tn') then result := 'Setswana'
  else if (code = 'sn') then result := 'Shona'
  else if (code = 'ii') then result := 'Sichuan Yi'
  else if (code = 'sd') then result := 'Sindhi'
  else if (code = 'si') then result := 'Sinhalese'
  else if (code = 'ss') then result := 'Siswati'
  else if (code = 'sk') then result := 'Slovak'
  else if (code = 'sl') then result := 'Slovenian'
  else if (code = 'so') then result := 'Somali'
  else if (code = 'es') then result := 'Spanish'
  else if (code = 'su') then result := 'Sundanese'
  else if (code = 'sw') then result := 'Swahili (Kiswahili)'
  else if (code = 'sv') then result := 'Swedish'
  else if (code = 'tl') then result := 'Tagalog'
  else if (code = 'tg') then result := 'Tajik'
  else if (code = 'ta') then result := 'Tamil'
  else if (code = 'tt') then result := 'Tatar'
  else if (code = 'te') then result := 'Telugu'
  else if (code = 'th') then result := 'Thai'
  else if (code = 'bo') then result := 'Tibetan'
  else if (code = 'ti') then result := 'Tigrinya'
  else if (code = 'to') then result := 'Tonga'
  else if (code = 'ts') then result := 'Tsonga'
  else if (code = 'tr') then result := 'Turkish'
  else if (code = 'tk') then result := 'Turkmen'
  else if (code = 'tw') then result := 'Twi'
  else if (code = 'ug') then result := 'Uighur'
  else if (code = 'uk') then result := 'Ukrainian'
  else if (code = 'ur') then result := 'Urdu'
  else if (code = 'uz') then result := 'Uzbek'
  else if (code = 'vi') then result := 'Vietnamese'
  else if (code = 'vo') then result := 'Volapük'
  else if (code = 'wa') then result := 'Wallon'
  else if (code = 'cy') then result := 'Welsh'
  else if (code = 'wo') then result := 'Wolof'
  else if (code = 'xh') then result := 'Xhosa'
  else if (code = 'yi') then result := 'Yiddish'
  else if (code = 'yo') then result := 'Yoruba'
  else if (code = 'zu') then result := 'Zulu'
  else
    raise Exception.Create('Unknown Language "'+code+'"');
end;


destructor TIETFLanguageCodeServices.Destroy;
begin
// this does not own it:   db.Free;
  inherited;
end;

function TIETFLanguageCodeServices.Display(context : TCodeSystemProviderContext) : string;
begin
  result := getDisplay(TIETFLanguageCodeConcept(context).code);
end;

procedure TIETFLanguageCodeServices.Displays(context: TCodeSystemProviderContext; list: TStringList);
begin
  list.Add(Display(context));
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

function TIETFLanguageCodeServices.ChildCount(context : TCodeSystemProviderContext) : integer;
begin
  result := -1;
end;

function TIETFLanguageCodeServices.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  raise Exception.Create('not done yet');
end;

function TIETFLanguageCodeServices.locateIsA(code, parent : String) : TCodeSystemProviderContext;
begin
  result := nil; // no subsumption
end;


function TIETFLanguageCodeServices.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  raise Exception.Create('not done yet');
end;

function TIETFLanguageCodeServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise Exception.Create('not done yet');
end;

function TIETFLanguageCodeServices.filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise Exception.Create('not done yet');
end;

function TIETFLanguageCodeServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext;
begin
  raise Exception.Create('not done yet');
end;

function TIETFLanguageCodeServices.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  raise Exception.Create('not done yet');
end;

function TIETFLanguageCodeServices.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise Exception.Create('not done yet');
end;

function TIETFLanguageCodeServices.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise Exception.Create('not done yet');
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
  raise Exception.Create('not done yet');
end;


{ TIETFLanguageCodeConcept }

constructor TIETFLanguageCodeConcept.create(code: String);
begin
  inherited create;
  self.code := code;
end;

end.
