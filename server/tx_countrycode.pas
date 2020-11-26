unit tx_countrycode;

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
  SysUtils, Classes, {$IFDEF DELPHI} RegularExpressions, {$ENDIF}
  fsl_utilities, fsl_base, fsl_stream, fsl_http, fsl_fpc,
  fhir_common,
  ftx_service;

type
  TCountryCodeConcept = class (TCodeSystemProviderContext)
  private
    FDisplay: String;
    FCode: String;
  public
    function link : TCountryCodeConcept; overload;

    property code : String read FCode write FCode;
    property display : String read FDisplay write FDisplay;
  end;

  TCountryCodeConceptFilter = class (TCodeSystemProviderFilterContext)
  private
    FList : TFslList<TCountryCodeConcept>;
    FCursor : integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TCountryCodeConceptFilter; overload;
  end;

  TCountryCodeServices = class (TCodeSystemProvider)
  private
    FCodes : TFslList<TCountryCodeConcept>;
    FMap : TFslMap<TCountryCodeConcept>;

    procedure load;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TCountryCodeServices; overload;

    function description : String; override;
    function TotalCount : integer;  override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function systemUri(context : TCodeSystemProviderContext) : String; override;
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
    function subsumesTest(codeA, codeB : String) : String; override;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
  end;

implementation

{ TCountryCodeServices }

Constructor TCountryCodeServices.create;
begin
  inherited Create;
  FCodes := TFslList<TCountryCodeConcept>.create;
  FMap := TFslMap<TCountryCodeConcept>.create('tx.countrycode');
  Load;
end;


function TCountryCodeServices.TotalCount : integer;
begin
  result := FCodes.Count;
end;


function TCountryCodeServices.systemUri(context : TCodeSystemProviderContext) : String;
begin
  result := 'urn:iso:std:iso:3166';
end;

function TCountryCodeServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TCountryCodeServices.getDisplay(code : String; const lang : THTTPLanguages):String;
begin
  result := FMap[code].display.Trim;
end;

function TCountryCodeServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

procedure TCountryCodeServices.Displays(code : String; list : TStringList; const lang : THTTPLanguages);
begin
  list.Add(getDisplay(code, lang));
end;


procedure TCountryCodeServices.load;
  procedure doLoad(code, display : String);
  var
    c : TCountryCodeConcept;
  begin
    c := TCountryCodeConcept.Create;
    try
      c.code := code;
      c.display := display;
      FCodes.Add(c.Link);
      FMap.Add(code, c.Link);
    finally
      c.Free;
    end;
  end;
begin
  doLoad('AD', 'Andorra');
  doLoad('AE', 'United Arab Emirates');
  doLoad('AF', 'Afghanistan');
  doLoad('AG', 'Antigua and Barbuda');
  doLoad('AI', 'Anguilla');
  doLoad('AL', 'Albania');
  doLoad('AM', 'Armenia');
  doLoad('AO', 'Angola');
  doLoad('AQ', 'Antarctica');
  doLoad('AR', 'Argentina');
  doLoad('AS', 'American Samoa');
  doLoad('AT', 'Austria');
  doLoad('AU', 'Australia');
  doLoad('AW', 'Aruba');
  doLoad('AX', 'Eland Islands');
  doLoad('AZ', 'Azerbaijan');
  doLoad('BA', 'Bosnia and Herzegovina');
  doLoad('BB', 'Barbados');
  doLoad('BD', 'Bangladesh');
  doLoad('BE', 'Belgium');
  doLoad('BF', 'Burkina Faso');
  doLoad('BG', 'Bulgaria');
  doLoad('BH', 'Bahrain');
  doLoad('BI', 'Burundi');
  doLoad('BJ', 'Benin');
  doLoad('BL', 'Saint Barthilemy');
  doLoad('BM', 'Bermuda');
  doLoad('BN', 'Brunei Darussalam');
  doLoad('BO', 'Bolivia, Plurinational State of');
  doLoad('BQ', 'Bonaire, Sint Eustatius and Saba');
  doLoad('BR', 'Brazil');
  doLoad('BS', 'Bahamas');
  doLoad('BT', 'Bhutan');
  doLoad('BV', 'Bouvet Island');
  doLoad('BW', 'Botswana');
  doLoad('BY', 'Belarus');
  doLoad('BZ', 'Belize');
  doLoad('CA', 'Canada');
  doLoad('CC', 'Cocos (Keeling) Islands');
  doLoad('CD', 'Congo, the Democratic Republic of the');
  doLoad('CF', 'Central African Republic');
  doLoad('CG', 'Congo');
  doLoad('CH', 'Switzerland');
  doLoad('CI', 'Ctte d''Ivoire');
  doLoad('CK', 'Cook Islands');
  doLoad('CL', 'Chile');
  doLoad('CM', 'Cameroon');
  doLoad('CN', 'China');
  doLoad('CO', 'Colombia');
  doLoad('CR', 'Costa Rica');
  doLoad('CU', 'Cuba');
  doLoad('CV', 'Cabo Verde');
  doLoad('CW', 'Curagao');
  doLoad('CX', 'Christmas Island');
  doLoad('CY', 'Cyprus');
  doLoad('CZ', 'Czechia');
  doLoad('DE', 'Germany');
  doLoad('DJ', 'Djibouti');
  doLoad('DK', 'Denmark');
  doLoad('DM', 'Dominica');
  doLoad('DO', 'Dominican Republic');
  doLoad('DZ', 'Algeria');
  doLoad('EC', 'Ecuador');
  doLoad('EE', 'Estonia');
  doLoad('EG', 'Egypt');
  doLoad('EH', 'Western Sahara');
  doLoad('ER', 'Eritrea');
  doLoad('ES', 'Spain');
  doLoad('ET', 'Ethiopia');
  doLoad('FI', 'Finland');
  doLoad('FJ', 'Fiji');
  doLoad('FK', 'Falkland Islands (Malvinas)');
  doLoad('FM', 'Micronesia, Federated States of');
  doLoad('FO', 'Faroe Islands');
  doLoad('FR', 'France');
  doLoad('GA', 'Gabon');
  doLoad('GB', 'United Kingdom of Great Britain and Northern Ireland');
  doLoad('GD', 'Grenada');
  doLoad('GE', 'Georgia');
  doLoad('GF', 'French Guiana');
  doLoad('GG', 'Guernsey');
  doLoad('GH', 'Ghana');
  doLoad('GI', 'Gibraltar');
  doLoad('GL', 'Greenland');
  doLoad('GM', 'Gambia');
  doLoad('GN', 'Guinea');
  doLoad('GP', 'Guadeloupe');
  doLoad('GQ', 'Equatorial Guinea');
  doLoad('GR', 'Greece');
  doLoad('GS', 'South Georgia and the South Sandwich Islands');
  doLoad('GT', 'Guatemala');
  doLoad('GU', 'Guam');
  doLoad('GW', 'Guinea-Bissau');
  doLoad('GY', 'Guyana');
  doLoad('HK', 'Hong Kong');
  doLoad('HM', 'Heard Island and McDonald Islands');
  doLoad('HN', 'Honduras');
  doLoad('HR', 'Croatia');
  doLoad('HT', 'Haiti');
  doLoad('HU', 'Hungary');
  doLoad('ID', 'Indonesia');
  doLoad('IE', 'Ireland');
  doLoad('IL', 'Israel');
  doLoad('IM', 'Isle of Man');
  doLoad('IN', 'India');
  doLoad('IO', 'British Indian Ocean Territory');
  doLoad('IQ', 'Iraq');
  doLoad('IR', 'Iran, Islamic Republic of');
  doLoad('IS', 'Iceland');
  doLoad('IT', 'Italy');
  doLoad('JE', 'Jersey');
  doLoad('JM', 'Jamaica');
  doLoad('JO', 'Jordan');
  doLoad('JP', 'Japan');
  doLoad('KE', 'Kenya');
  doLoad('KG', 'Kyrgyzstan');
  doLoad('KH', 'Cambodia');
  doLoad('KI', 'Kiribati');
  doLoad('KM', 'Comoros');
  doLoad('KN', 'Saint Kitts and Nevis');
  doLoad('KP', 'Korea, Democratic People''s Republic of');
  doLoad('KR', 'Korea, Republic of');
  doLoad('KW', 'Kuwait');
  doLoad('KY', 'Cayman Islands');
  doLoad('KZ', 'Kazakhstan');
  doLoad('LA', 'Lao People''s Democratic Republic');
  doLoad('LB', 'Lebanon');
  doLoad('LC', 'Saint Lucia');
  doLoad('LI', 'Liechtenstein');
  doLoad('LK', 'Sri Lanka');
  doLoad('LR', 'Liberia');
  doLoad('LS', 'Lesotho');
  doLoad('LT', 'Lithuania');
  doLoad('LU', 'Luxembourg');
  doLoad('LV', 'Latvia');
  doLoad('LY', 'Libya');
  doLoad('MA', 'Morocco');
  doLoad('MC', 'Monaco');
  doLoad('MD', 'Moldova, Republic of');
  doLoad('ME', 'Montenegro');
  doLoad('MF', 'Saint Martin (French part)');
  doLoad('MG', 'Madagascar');
  doLoad('MH', 'Marshall Islands');
  doLoad('MK', 'Macedonia, the former Yugoslav Republic of');
  doLoad('ML', 'Mali');
  doLoad('MM', 'Myanmar');
  doLoad('MN', 'Mongolia');
  doLoad('MO', 'Macao');
  doLoad('MP', 'Northern Mariana Islands');
  doLoad('MQ', 'Martinique');
  doLoad('MR', 'Mauritania');
  doLoad('MS', 'Montserrat');
  doLoad('MT', 'Malta');
  doLoad('MU', 'Mauritius');
  doLoad('MV', 'Maldives');
  doLoad('MW', 'Malawi');
  doLoad('MX', 'Mexico');
  doLoad('MY', 'Malaysia');
  doLoad('MZ', 'Mozambique');
  doLoad('NA', 'Namibia');
  doLoad('NC', 'New Caledonia');
  doLoad('NE', 'Niger');
  doLoad('NF', 'Norfolk Island');
  doLoad('NG', 'Nigeria');
  doLoad('NI', 'Nicaragua');
  doLoad('NL', 'Netherlands');
  doLoad('NO', 'Norway');
  doLoad('NP', 'Nepal');
  doLoad('NR', 'Nauru');
  doLoad('NU', 'Niue');
  doLoad('NZ', 'New Zealand');
  doLoad('OM', 'Oman');
  doLoad('PA', 'Panama');
  doLoad('PE', 'Peru');
  doLoad('PF', 'French Polynesia');
  doLoad('PG', 'Papua New Guinea');
  doLoad('PH', 'Philippines');
  doLoad('PK', 'Pakistan');
  doLoad('PL', 'Poland');
  doLoad('PM', 'Saint Pierre and Miquelon');
  doLoad('PN', 'Pitcairn');
  doLoad('PR', 'Puerto Rico');
  doLoad('PS', 'Palestine, State of');
  doLoad('PT', 'Portugal');
  doLoad('PW', 'Palau');
  doLoad('PY', 'Paraguay');
  doLoad('QA', 'Qatar');
  doLoad('RE', 'Riunion');
  doLoad('RO', 'Romania');
  doLoad('RS', 'Serbia');
  doLoad('RU', 'Russian Federation');
  doLoad('RW', 'Rwanda');
  doLoad('SA', 'Saudi Arabia');
  doLoad('SB', 'Solomon Islands');
  doLoad('SC', 'Seychelles');
  doLoad('SD', 'Sudan');
  doLoad('SE', 'Sweden');
  doLoad('SG', 'Singapore');
  doLoad('SH', 'Saint Helena, Ascension and Tristan da Cunha');
  doLoad('SI', 'Slovenia');
  doLoad('SJ', 'Svalbard and Jan Mayen');
  doLoad('SK', 'Slovakia');
  doLoad('SL', 'Sierra Leone');
  doLoad('SM', 'San Marino');
  doLoad('SN', 'Senegal');
  doLoad('SO', 'Somalia');
  doLoad('SR', 'Suriname');
  doLoad('SS', 'South Sudan');
  doLoad('ST', 'Sao Tome and Principe');
  doLoad('SV', 'El Salvador');
  doLoad('SX', 'Sint Maarten (Dutch part)');
  doLoad('SY', 'Syrian Arab Republic');
  doLoad('SZ', 'Swaziland');
  doLoad('TC', 'Turks and Caicos Islands');
  doLoad('TD', 'Chad');
  doLoad('TF', 'French Southern Territories');
  doLoad('TG', 'Togo');
  doLoad('TH', 'Thailand');
  doLoad('TJ', 'Tajikistan');
  doLoad('TK', 'Tokelau');
  doLoad('TL', 'Timor-Leste');
  doLoad('TM', 'Turkmenistan');
  doLoad('TN', 'Tunisia');
  doLoad('TO', 'Tonga');
  doLoad('TR', 'Turkey');
  doLoad('TT', 'Trinidad and Tobago');
  doLoad('TV', 'Tuvalu');
  doLoad('TW', 'Taiwan, Province of China');
  doLoad('TZ', 'Tanzania, United Republic of');
  doLoad('UA', 'Ukraine');
  doLoad('UG', 'Uganda');
  doLoad('UM', 'United States Minor Outlying Islands');
  doLoad('US', 'United States of America');
  doLoad('UY', 'Uruguay');
  doLoad('UZ', 'Uzbekistan');
  doLoad('VA', 'Holy See');
  doLoad('VC', 'Saint Vincent and the Grenadines');
  doLoad('VE', 'Venezuela, Bolivarian Republic of');
  doLoad('VG', 'Virgin Islands, British');
  doLoad('VI', 'Virgin Islands,');
  doLoad('VN', 'Viet Nam');
  doLoad('VU', 'Vanuatu');
  doLoad('WF', 'Wallis and Futuna');
  doLoad('WS', 'Samoa');
  doLoad('YE', 'Yemen');
  doLoad('YT', 'Mayotte');
  doLoad('ZA', 'South Africa');
  doLoad('ZM', 'Zambia');
  doLoad('ZW', 'Zimbabwe');

  doLoad('ABW', 'Aruba');
  doLoad('AFG', 'Afghanistan');
  doLoad('AGO', 'Angola');
  doLoad('AIA', 'Anguilla');
  doLoad('ALA', 'Eland Islands');
  doLoad('ALB', 'Albania');
  doLoad('AND', 'Andorra');
  doLoad('ARE', 'United Arab Emirates');
  doLoad('ARG', 'Argentina');
  doLoad('ARM', 'Armenia');
  doLoad('ASM', 'American Samoa');
  doLoad('ATA', 'Antarctica');
  doLoad('ATF', 'French Southern Territories');
  doLoad('ATG', 'Antigua and Barbuda');
  doLoad('AUS', 'Australia');
  doLoad('AUT', 'Austria');
  doLoad('AZE', 'Azerbaijan');
  doLoad('BDI', 'Burundi');
  doLoad('BEL', 'Belgium');
  doLoad('BEN', 'Benin');
  doLoad('BES', 'Bonaire, Sint Eustatius and Saba');
  doLoad('BFA', 'Burkina Faso');
  doLoad('BGD', 'Bangladesh');
  doLoad('BGR', 'Bulgaria');
  doLoad('BHR', 'Bahrain');
  doLoad('BHS', 'Bahamas');
  doLoad('BIH', 'Bosnia and Herzegovina');
  doLoad('BLM', 'Saint Barthilemy');
  doLoad('BLR', 'Belarus');
  doLoad('BLZ', 'Belize');
  doLoad('BMU', 'Bermuda');
  doLoad('BOL', 'Bolivia, Plurinational State of');
  doLoad('BRA', 'Brazil');
  doLoad('BRB', 'Barbados');
  doLoad('BRN', 'Brunei Darussalam');
  doLoad('BTN', 'Bhutan');
  doLoad('BVT', 'Bouvet Island');
  doLoad('BWA', 'Botswana');
  doLoad('CAF', 'Central African Republic');
  doLoad('CAN', 'Canada');
  doLoad('CCK', 'Cocos (Keeling) Islands');
  doLoad('CHE', 'Switzerland');
  doLoad('CHL', 'Chile');
  doLoad('CHN', 'China');
  doLoad('CIV', 'Ctte d''Ivoire');
  doLoad('CMR', 'Cameroon');
  doLoad('COD', 'Congo, the Democratic Republic of the');
  doLoad('COG', 'Congo');
  doLoad('COK', 'Cook Islands');
  doLoad('COL', 'Colombia');
  doLoad('COM', 'Comoros');
  doLoad('CPV', 'Cabo Verde');
  doLoad('CRI', 'Costa Rica');
  doLoad('CUB', 'Cuba');
  doLoad('CUW', 'Curagao');
  doLoad('CXR', 'Christmas Island');
  doLoad('CYM', 'Cayman Islands');
  doLoad('CYP', 'Cyprus');
  doLoad('CZE', 'Czechia');
  doLoad('DEU', 'Germany');
  doLoad('DJI', 'Djibouti');
  doLoad('DMA', 'Dominica');
  doLoad('DNK', 'Denmark');
  doLoad('DOM', 'Dominican Republic');
  doLoad('DZA', 'Algeria');
  doLoad('ECU', 'Ecuador');
  doLoad('EGY', 'Egypt');
  doLoad('ERI', 'Eritrea');
  doLoad('ESH', 'Western Sahara');
  doLoad('ESP', 'Spain');
  doLoad('EST', 'Estonia');
  doLoad('ETH', 'Ethiopia');
  doLoad('FIN', 'Finland');
  doLoad('FJI', 'Fiji');
  doLoad('FLK', 'Falkland Islands (Malvinas)');
  doLoad('FRA', 'France');
  doLoad('FRO', 'Faroe Islands');
  doLoad('FSM', 'Micronesia, Federated States of');
  doLoad('GAB', 'Gabon');
  doLoad('GBR', 'United Kingdom');
  doLoad('GEO', 'Georgia');
  doLoad('GGY', 'Guernsey');
  doLoad('GHA', 'Ghana');
  doLoad('GIB', 'Gibraltar');
  doLoad('GIN', 'Guinea');
  doLoad('GLP', 'Guadeloupe');
  doLoad('GMB', 'Gambia');
  doLoad('GNB', 'Guinea-Bissau');
  doLoad('GNQ', 'Equatorial Guinea');
  doLoad('GRC', 'Greece');
  doLoad('GRD', 'Grenada');
  doLoad('GRL', 'Greenland');
  doLoad('GTM', 'Guatemala');
  doLoad('GUF', 'French Guiana');
  doLoad('GUM', 'Guam');
  doLoad('GUY', 'Guyana');
  doLoad('HKG', 'Hong Kong');
  doLoad('HMD', 'Heard Island and McDonald Islands');
  doLoad('HND', 'Honduras');
  doLoad('HRV', 'Croatia');
  doLoad('HTI', 'Haiti');
  doLoad('HUN', 'Hungary');
  doLoad('IDN', 'Indonesia');
  doLoad('IMN', 'Isle of Man');
  doLoad('IND', 'India');
  doLoad('IOT', 'British Indian Ocean Territory');
  doLoad('IRL', 'Ireland');
  doLoad('IRN', 'Iran, Islamic Republic of');
  doLoad('IRQ', 'Iraq');
  doLoad('ISL', 'Iceland');
  doLoad('ISR', 'Israel');
  doLoad('ITA', 'Italy');
  doLoad('JAM', 'Jamaica');
  doLoad('JEY', 'Jersey');
  doLoad('JOR', 'Jordan');
  doLoad('JPN', 'Japan');
  doLoad('KAZ', 'Kazakhstan');
  doLoad('KEN', 'Kenya');
  doLoad('KGZ', 'Kyrgyzstan');
  doLoad('KHM', 'Cambodia');
  doLoad('KIR', 'Kiribati');
  doLoad('KNA', 'Saint Kitts and Nevis');
  doLoad('KOR', 'Korea, Republic of');
  doLoad('KWT', 'Kuwait');
  doLoad('LAO', 'Lao People''s Democratic Republic');
  doLoad('LBN', 'Lebanon');
  doLoad('LBR', 'Liberia');
  doLoad('LBY', 'Libya');
  doLoad('LCA', 'Saint Lucia');
  doLoad('LIE', 'Liechtenstein');
  doLoad('LKA', 'Sri Lanka');
  doLoad('LSO', 'Lesotho');
  doLoad('LTU', 'Lithuania');
  doLoad('LUX', 'Luxembourg');
  doLoad('LVA', 'Latvia');
  doLoad('MAC', 'Macao');
  doLoad('MAF', 'Saint Martin (French part)');
  doLoad('MAR', 'Morocco');
  doLoad('MCO', 'Monaco');
  doLoad('MDA', 'Moldova, Republic of');
  doLoad('MDG', 'Madagascar');
  doLoad('MDV', 'Maldives');
  doLoad('MEX', 'Mexico');
  doLoad('MHL', 'Marshall Islands');
  doLoad('MKD', 'Macedonia, the former Yugoslav Republic of');
  doLoad('MLI', 'Mali');
  doLoad('MLT', 'Malta');
  doLoad('MMR', 'Myanmar');
  doLoad('MNE', 'Montenegro');
  doLoad('MNG', 'Mongolia');
  doLoad('MNP', 'Northern Mariana Islands');
  doLoad('MOZ', 'Mozambique');
  doLoad('MRT', 'Mauritania');
  doLoad('MSR', 'Montserrat');
  doLoad('MTQ', 'Martinique');
  doLoad('MUS', 'Mauritius');
  doLoad('MWI', 'Malawi');
  doLoad('MYS', 'Malaysia');
  doLoad('MYT', 'Mayotte');
  doLoad('NAM', 'Namibia');
  doLoad('NCL', 'New Caledonia');
  doLoad('NER', 'Niger');
  doLoad('NFK', 'Norfolk Island');
  doLoad('NGA', 'Nigeria');
  doLoad('NIC', 'Nicaragua');
  doLoad('NIU', 'Niue');
  doLoad('NLD', 'Netherlands');
  doLoad('NOR', 'Norway');
  doLoad('NPL', 'Nepal');
  doLoad('NRU', 'Nauru');
  doLoad('NZL', 'New Zealand');
  doLoad('OMN', 'Oman');
  doLoad('PAK', 'Pakistan');
  doLoad('PAN', 'Panama');
  doLoad('PCN', 'Pitcairn');
  doLoad('PER', 'Peru');
  doLoad('PHL', 'Philippines');
  doLoad('PLW', 'Palau');
  doLoad('PNG', 'Papua New Guinea');
  doLoad('POL', 'Poland');
  doLoad('PRI', 'Puerto Rico');
  doLoad('PRK', 'Korea, Democratic People''s Republic of');
  doLoad('PRT', 'Portugal');
  doLoad('PRY', 'Paraguay');
  doLoad('PSE', 'Palestine, State of');
  doLoad('PYF', 'French Polynesia');
  doLoad('QAT', 'Qatar');
  doLoad('REU', 'Riunion');
  doLoad('ROU', 'Romania');
  doLoad('RUS', 'Russian Federation');
  doLoad('RWA', 'Rwanda');
  doLoad('SAU', 'Saudi Arabia');
  doLoad('SDN', 'Sudan');
  doLoad('SEN', 'Senegal');
  doLoad('SGP', 'Singapore');
  doLoad('SGS', 'South Georgia and the South Sandwich Islands');
  doLoad('SHN', 'Saint Helena, Ascension and Tristan da Cunha');
  doLoad('SJM', 'Svalbard and Jan Mayen');
  doLoad('SLB', 'Solomon Islands');
  doLoad('SLE', 'Sierra Leone');
  doLoad('SLV', 'El Salvador');
  doLoad('SMR', 'San Marino');
  doLoad('SOM', 'Somalia');
  doLoad('SPM', 'Saint Pierre and Miquelon');
  doLoad('SRB', 'Serbia');
  doLoad('SSD', 'South Sudan');
  doLoad('STP', 'Sao Tome and Principe');
  doLoad('SUR', 'Suriname');
  doLoad('SVK', 'Slovakia');
  doLoad('SVN', 'Slovenia');
  doLoad('SWE', 'Sweden');
  doLoad('SWZ', 'Swaziland');
  doLoad('SXM', 'Sint Maarten (Dutch part)');
  doLoad('SYC', 'Seychelles');
  doLoad('SYR', 'Syrian Arab Republic');
  doLoad('TCA', 'Turks and Caicos Islands');
  doLoad('TCD', 'Chad');
  doLoad('TGO', 'Togo');
  doLoad('THA', 'Thailand');
  doLoad('TJK', 'Tajikistan');
  doLoad('TKL', 'Tokelau');
  doLoad('TKM', 'Turkmenistan');
  doLoad('TLS', 'Timor-Leste');
  doLoad('TON', 'Tonga');
  doLoad('TTO', 'Trinidad and Tobago');
  doLoad('TUN', 'Tunisia');
  doLoad('TUR', 'Turkey');
  doLoad('TUV', 'Tuvalu');
  doLoad('TWN', 'Taiwan, Province of China');
  doLoad('TZA', 'Tanzania, United Republic of');
  doLoad('UGA', 'Uganda');
  doLoad('UKR', 'Ukraine');
  doLoad('UMI', 'United States Minor Outlying Islands');
  doLoad('URY', 'Uruguay');
  doLoad('USA', 'United States of America');
  doLoad('UZB', 'Uzbekistan');
  doLoad('VAT', 'Holy See');
  doLoad('VCT', 'Saint Vincent and the Grenadines');
  doLoad('VEN', 'Venezuela, Bolivarian Republic of');
  doLoad('VGB', 'Virgin Islands, British');
  doLoad('VIR', 'Virgin Islands, U.S.');
  doLoad('VNM', 'Viet Nam');
  doLoad('VUT', 'Vanuatu');
  doLoad('WLF', 'Wallis and Futuna');
  doLoad('WSM', 'Samoa');
  doLoad('YEM', 'Yemen');
  doLoad('ZAF', 'South Africa');
  doLoad('ZMB', 'Zambia');
  doLoad('ZWE', 'Zimbabwe');

  doLoad('004', 'Afghanistan');
  doLoad('008', 'Albania');
  doLoad('010', 'Antarctica');
  doLoad('012', 'Algeria');
  doLoad('016', 'American Samoa');
  doLoad('020', 'Andorra');
  doLoad('024', 'Angola');
  doLoad('028', 'Antigua and Barbuda');
  doLoad('031', 'Azerbaijan');
  doLoad('032', 'Argentina');
  doLoad('036', 'Australia');
  doLoad('040', 'Austria');
  doLoad('044', 'Bahamas');
  doLoad('048', 'Bahrain');
  doLoad('050', 'Bangladesh');
  doLoad('051', 'Armenia');
  doLoad('052', 'Barbados');
  doLoad('056', 'Belgium');
  doLoad('060', 'Bermuda');
  doLoad('064', 'Bhutan');
  doLoad('068', 'Bolivia, Plurinational State of');
  doLoad('070', 'Bosnia and Herzegovina');
  doLoad('072', 'Botswana');
  doLoad('074', 'Bouvet Island');
  doLoad('076', 'Brazil');
  doLoad('084', 'Belize');
  doLoad('086', 'British Indian Ocean Territory');
  doLoad('090', 'Solomon Islands');
  doLoad('092', 'Virgin Islands, British');
  doLoad('096', 'Brunei Darussalam');
  doLoad('100', 'Bulgaria');
  doLoad('104', 'Myanmar');
  doLoad('108', 'Burundi');
  doLoad('112', 'Belarus');
  doLoad('116', 'Cambodia');
  doLoad('120', 'Cameroon');
  doLoad('124', 'Canada');
  doLoad('132', 'Cabo Verde');
  doLoad('136', 'Cayman Islands');
  doLoad('140', 'Central African Republic');
  doLoad('144', 'Sri Lanka');
  doLoad('148', 'Chad');
  doLoad('152', 'Chile');
  doLoad('156', 'China');
  doLoad('158', 'Taiwan, Province of China');
  doLoad('162', 'Christmas Island');
  doLoad('166', 'Cocos (Keeling) Islands');
  doLoad('170', 'Colombia');
  doLoad('174', 'Comoros');
  doLoad('175', 'Mayotte');
  doLoad('178', 'Congo');
  doLoad('180', 'Congo, the Democratic Republic of the');
  doLoad('184', 'Cook Islands');
  doLoad('188', 'Costa Rica');
  doLoad('191', 'Croatia');
  doLoad('192', 'Cuba');
  doLoad('196', 'Cyprus');
  doLoad('203', 'Czechia');
  doLoad('204', 'Benin');
  doLoad('208', 'Denmark');
  doLoad('212', 'Dominica');
  doLoad('214', 'Dominican Republic');
  doLoad('218', 'Ecuador');
  doLoad('222', 'El Salvador');
  doLoad('226', 'Equatorial Guinea');
  doLoad('231', 'Ethiopia');
  doLoad('232', 'Eritrea');
  doLoad('233', 'Estonia');
  doLoad('234', 'Faroe Islands');
  doLoad('238', 'Falkland Islands (Malvinas)');
  doLoad('239', 'South Georgia and the South Sandwich Islands');
  doLoad('242', 'Fiji');
  doLoad('246', 'Finland');
  doLoad('248', 'Eland Islands');
  doLoad('250', 'France');
  doLoad('254', 'French Guiana');
  doLoad('258', 'French Polynesia');
  doLoad('260', 'French Southern Territories');
  doLoad('262', 'Djibouti');
  doLoad('266', 'Gabon');
  doLoad('268', 'Georgia');
  doLoad('270', 'Gambia');
  doLoad('275', 'Palestine, State of');
  doLoad('276', 'Germany');
  doLoad('288', 'Ghana');
  doLoad('292', 'Gibraltar');
  doLoad('296', 'Kiribati');
  doLoad('300', 'Greece');
  doLoad('304', 'Greenland');
  doLoad('308', 'Grenada');
  doLoad('312', 'Guadeloupe');
  doLoad('316', 'Guam');
  doLoad('320', 'Guatemala');
  doLoad('324', 'Guinea');
  doLoad('328', 'Guyana');
  doLoad('332', 'Haiti');
  doLoad('334', 'Heard Island and McDonald Islands');
  doLoad('336', 'Holy See');
  doLoad('340', 'Honduras');
  doLoad('344', 'Hong Kong');
  doLoad('348', 'Hungary');
  doLoad('352', 'Iceland');
  doLoad('356', 'India');
  doLoad('360', 'Indonesia');
  doLoad('364', 'Iran, Islamic Republic of');
  doLoad('368', 'Iraq');
  doLoad('372', 'Ireland');
  doLoad('376', 'Israel');
  doLoad('380', 'Italy');
  doLoad('384', 'Ctte d''Ivoire');
  doLoad('388', 'Jamaica');
  doLoad('392', 'Japan');
  doLoad('398', 'Kazakhstan');
  doLoad('400', 'Jordan');
  doLoad('404', 'Kenya');
  doLoad('408', 'Korea, Democratic People''s Republic of');
  doLoad('410', 'Korea, Republic of');
  doLoad('414', 'Kuwait');
  doLoad('417', 'Kyrgyzstan');
  doLoad('418', 'Lao People''s Democratic Republic');
  doLoad('422', 'Lebanon');
  doLoad('426', 'Lesotho');
  doLoad('428', 'Latvia');
  doLoad('430', 'Liberia');
  doLoad('434', 'Libya');
  doLoad('438', 'Liechtenstein');
  doLoad('440', 'Lithuania');
  doLoad('442', 'Luxembourg');
  doLoad('446', 'Macao');
  doLoad('450', 'Madagascar');
  doLoad('454', 'Malawi');
  doLoad('458', 'Malaysia');
  doLoad('462', 'Maldives');
  doLoad('466', 'Mali');
  doLoad('470', 'Malta');
  doLoad('474', 'Martinique');
  doLoad('478', 'Mauritania');
  doLoad('480', 'Mauritius');
  doLoad('484', 'Mexico');
  doLoad('492', 'Monaco');
  doLoad('496', 'Mongolia');
  doLoad('498', 'Moldova, Republic of');
  doLoad('499', 'Montenegro');
  doLoad('500', 'Montserrat');
  doLoad('504', 'Morocco');
  doLoad('508', 'Mozambique');
  doLoad('512', 'Oman');
  doLoad('516', 'Namibia');
  doLoad('520', 'Nauru');
  doLoad('524', 'Nepal');
  doLoad('528', 'Netherlands');
  doLoad('531', 'Curagao');
  doLoad('533', 'Aruba');
  doLoad('534', 'Sint Maarten (Dutch part)');
  doLoad('535', 'Bonaire, Sint Eustatius and Saba');
  doLoad('540', 'New Caledonia');
  doLoad('548', 'Vanuatu');
  doLoad('554', 'New Zealand');
  doLoad('558', 'Nicaragua');
  doLoad('562', 'Niger');
  doLoad('566', 'Nigeria');
  doLoad('570', 'Niue');
  doLoad('574', 'Norfolk Island');
  doLoad('578', 'Norway');
  doLoad('580', 'Northern Mariana Islands');
  doLoad('581', 'United States Minor Outlying Islands');
  doLoad('583', 'Micronesia, Federated States of');
  doLoad('584', 'Marshall Islands');
  doLoad('585', 'Palau');
  doLoad('586', 'Pakistan');
  doLoad('591', 'Panama');
  doLoad('598', 'Papua New Guinea');
  doLoad('600', 'Paraguay');
  doLoad('604', 'Peru');
  doLoad('608', 'Philippines');
  doLoad('612', 'Pitcairn');
  doLoad('616', 'Poland');
  doLoad('620', 'Portugal');
  doLoad('624', 'Guinea-Bissau');
  doLoad('626', 'Timor-Leste');
  doLoad('630', 'Puerto Rico');
  doLoad('634', 'Qatar');
  doLoad('638', 'Riunion');
  doLoad('642', 'Romania');
  doLoad('643', 'Russian Federation');
  doLoad('646', 'Rwanda');
  doLoad('652', 'Saint Barthilemy');
  doLoad('654', 'Saint Helena, Ascension and Tristan da Cunha');
  doLoad('659', 'Saint Kitts and Nevis');
  doLoad('660', 'Anguilla');
  doLoad('662', 'Saint Lucia');
  doLoad('663', 'Saint Martin (French part)');
  doLoad('666', 'Saint Pierre and Miquelon');
  doLoad('670', 'Saint Vincent and the Grenadines');
  doLoad('674', 'San Marino');
  doLoad('678', 'Sao Tome and Principe');
  doLoad('682', 'Saudi Arabia');
  doLoad('686', 'Senegal');
  doLoad('688', 'Serbia');
  doLoad('690', 'Seychelles');
  doLoad('694', 'Sierra Leone');
  doLoad('702', 'Singapore');
  doLoad('703', 'Slovakia');
  doLoad('704', 'Viet Nam');
  doLoad('705', 'Slovenia');
  doLoad('706', 'Somalia');
  doLoad('710', 'South Africa');
  doLoad('716', 'Zimbabwe');
  doLoad('724', 'Spain');
  doLoad('728', 'South Sudan');
  doLoad('729', 'Sudan');
  doLoad('732', 'Western Sahara');
  doLoad('740', 'Suriname');
  doLoad('744', 'Svalbard and Jan Mayen');
  doLoad('748', 'Swaziland');
  doLoad('752', 'Sweden');
  doLoad('756', 'Switzerland');
  doLoad('760', 'Syrian Arab Republic');
  doLoad('762', 'Tajikistan');
  doLoad('764', 'Thailand');
  doLoad('768', 'Togo');
  doLoad('772', 'Tokelau');
  doLoad('776', 'Tonga');
  doLoad('780', 'Trinidad and Tobago');
  doLoad('784', 'United Arab Emirates');
  doLoad('788', 'Tunisia');
  doLoad('792', 'Turkey');
  doLoad('795', 'Turkmenistan');
  doLoad('796', 'Turks and Caicos Islands');
  doLoad('798', 'Tuvalu');
  doLoad('800', 'Uganda');
  doLoad('804', 'Ukraine');
  doLoad('807', 'Macedonia, the former Yugoslav Republic of');
  doLoad('818', 'Egypt');
  doLoad('826', 'United Kingdom');
  doLoad('831', 'Guernsey');
  doLoad('832', 'Jersey');
  doLoad('833', 'Isle of Man');
  doLoad('834', 'Tanzania, United Republic of');
  doLoad('840', 'United States of America');
  doLoad('850', 'Virgin Islands, U.S.');
  doLoad('854', 'Burkina Faso');
  doLoad('858', 'Uruguay');
  doLoad('860', 'Uzbekistan');
  doLoad('862', 'Venezuela, Bolivarian Republic of');
  doLoad('876', 'Wallis and Futuna');
  doLoad('882', 'Samoa');
  doLoad('887', 'Yemen');
  doLoad('894', 'Zambia');

end;

function TCountryCodeServices.locate(code : String; var message : String) : TCodeSystemProviderContext;
begin
  result := FMap[code];
end;


function TCountryCodeServices.Code(context : TCodeSystemProviderContext) : string;
begin
  result := TCountryCodeConcept(context).code;
end;

function TCountryCodeServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TCountryCodeServices.description: String;
begin
  result := 'ISO Country Codes';
end;

destructor TCountryCodeServices.Destroy;
begin
  FMap.free;
  FCodes.Free;
  inherited;
end;

function TCountryCodeServices.Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string;
begin
  result := TCountryCodeConcept(context).display.Trim;
end;

procedure TCountryCodeServices.Displays(context: TCodeSystemProviderContext; list: TStringList; const lang : THTTPLanguages);
begin
  list.Add(Display(context, lang));
end;

function TCountryCodeServices.IsAbstract(context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // CountryCode doesn't do abstract
end;

function TCountryCodeServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TCountryCodeServices.Link: TCountryCodeServices;
begin
  result := TCountryCodeServices(Inherited Link);
end;

function TCountryCodeServices.ChildCount(context : TCodeSystemProviderContext) : integer;
begin
  if (context = nil) then
    result := TotalCount
  else
    result := 0; // no children
end;

function TCountryCodeServices.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  result := FCodes[ndx];
end;

function TCountryCodeServices.locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  result := nil; // no subsumption
end;


function TCountryCodeServices.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  // nothing
  result := true;
end;

function TCountryCodeServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.create('TCountryCodeServices.searchFilter');
end;

function TCountryCodeServices.subsumesTest(codeA, codeB: String): String;
begin
  result := 'not-subsumed';
end;

function TCountryCodeServices.filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
var
  regex : TRegex;
  list : TCountryCodeConceptFilter;
  concept : TCountryCodeConcept;
begin
  if (op = foRegex) and (prop = 'code') then
  begin
    list := TCountryCodeConceptFilter.Create;
    try
      regex := TRegEx.Create(value);
      for concept in FCodes do
        if regex.IsMatch(concept.code) then
          list.FList.Add(concept.link);
      result := list.link;
    finally
      list.Free;
    end;
  end
  else
    raise ETerminologyError.create('the filter '+prop+' '+CODES_TFhirFilterOperator[op]+' = '+value+' is not support for '+systemUri(nil));
end;

function TCountryCodeServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.create('TCountryCodeServices.filterLocate');
end;

function TCountryCodeServices.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  TCountryCodeConceptFilter(ctxt).FCursor := TCountryCodeConceptFilter(ctxt).FCursor + 1;
  result := TCountryCodeConceptFilter(ctxt).FCursor < TCountryCodeConceptFilter(ctxt).FList.Count;
end;

function TCountryCodeServices.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  result := TCountryCodeConceptFilter(ctxt).FList[TCountryCodeConceptFilter(ctxt).FCursor];
end;

function TCountryCodeServices.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyTodo.create('TCountryCodeServices.InFilter');
end;

procedure TCountryCodeServices.Close(ctxt: TCodeSystemProviderContext);
begin
//  ctxt.free;
end;

procedure TCountryCodeServices.Close(ctxt : TCodeSystemProviderFilterContext);
begin
  ctxt.free;
end;

procedure TCountryCodeServices.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  raise ETerminologyTodo.create('TCountryCodeServices.Close');
end;


{ TCountryCodeConcept }

function TCountryCodeConcept.link: TCountryCodeConcept;
begin
  result := TCountryCodeConcept(inherited Link);
end;

{ TCountryCodeConceptFilter }

constructor TCountryCodeConceptFilter.Create;
begin
  inherited;
  FList := TFslList<TCountryCodeConcept>.Create;
  FCursor := -1;
end;

destructor TCountryCodeConceptFilter.Destroy;
begin
  FList.Free;
  inherited;
end;

function TCountryCodeConceptFilter.link: TCountryCodeConceptFilter;
begin
  result := TCountryCodeConceptFilter(inherited Link);
end;

end.

