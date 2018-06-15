unit FHIR.Tx.Iso4217;

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
  SysUtils, Classes,
  FHIR.Support.Utilities,
  FHIR.Support.Base, FHIR.Support.Stream, 
  FHIR.Base.Common,
  FHIR.Version.Types, FHIR.Version.Resources, FHIR.Tx.Service;

type
  TIso4217Concept = class (TCodeSystemProviderContext)
  private
    FDisplay: String;
    FCode: String;
    FDecimals: integer;
  public
    function link : TIso4217Concept; overload;

    property code : String read FCode write FCode;
    property display : String read FDisplay write FDisplay;
    property decimals : integer read FDecimals write FDecimals;
  end;

  TIso4217ConceptFilter = class (TCodeSystemProviderFilterContext)
  private
    FList : TFslList<TIso4217Concept>;
    FCursor : integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TIso4217ConceptFilter; overload;
  end;

  TIso4217Services = class (TCodeSystemProvider)
  private
    FCodes : TFslList<TIso4217Concept>;
    FMap : TFslMap<TIso4217Concept>;

    procedure load;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Function Link : TIso4217Services; overload;

    function TotalCount : integer;  override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function system(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; lang : String):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; lang : String) : string; override;
    procedure Displays(code : String; list : TStringList; lang : String); override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList; lang : String); override;
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

{ TIso4217Services }

Constructor TIso4217Services.create();
begin
  inherited Create;
  FCodes := TFslList<TIso4217Concept>.create;
  FMap := TFslMap<TIso4217Concept>.create;
  Load;
end;


function TIso4217Services.TotalCount : integer;
begin
  result := FCodes.Count;
end;


function TIso4217Services.system(context : TCodeSystemProviderContext) : String;
begin
  result := 'urn:iso:std:iso:4217';
end;

function TIso4217Services.getDefinition(code: String): String;
begin
  result := '';
end;

function TIso4217Services.getDisplay(code : String; lang : String):String;
begin
  result := FMap[code].display;
end;

function TIso4217Services.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

procedure TIso4217Services.Displays(code : String; list : TStringList; lang : String);
begin
  list.Add(getDisplay(code, lang));
end;


procedure TIso4217Services.load;
  procedure doLoad(code : string; decimals : integer; display, countries : String);
  var
    c : TIso4217Concept;
  begin
    c := TIso4217Concept.Create;
    try
      c.code := code;
      c.display := display;
      c.decimals := decimals;
      FCodes.Add(c.Link);
      FMap.Add(code, c.Link);
    finally
      c.Free;
    end;
  end;
begin
  doLoad('AED', 2, 'United Arab Emirates dirham', 'United Arab Emirates');
  doLoad('AFN', 2, 'Afghan afghani', 'Afghanistan');
  doLoad('ALL', 2, 'Albanian lek', 'Albania');
  doLoad('AMD', 2, 'Armenian dram', 'Armenia');
  doLoad('ANG', 2, 'Netherlands Antillean guilder', 'Curaçao (CW),  Sint Maarten (SX)');
  doLoad('AOA', 2, 'Angolan kwanza', 'Angola');
  doLoad('ARS', 2, 'Argentine peso', 'Argentina');
  doLoad('AUD', 2, 'Australian dollar', 'Australia,  Christmas Island (CX),  Cocos (Keeling) Islands (CC),  Heard Island and McDonald Islands (HM),  Kiribati (KI),  Nauru (NR),  Norfolk Island (NF),  Tuvalu (TV)');
  doLoad('AWG', 2, 'Aruban florin', 'Aruba');
  doLoad('AZN', 2, 'Azerbaijani manat', 'Azerbaijan');
  doLoad('BAM', 2, 'Bosnia and Herzegovina convertible mark', 'Bosnia and Herzegovina');
  doLoad('BBD', 2, 'Barbados dollar', 'Barbados');
  doLoad('BDT', 2, 'Bangladeshi taka', 'Bangladesh');
  doLoad('BGN', 2, 'Bulgarian lev', 'Bulgaria');
  doLoad('BHD', 3, 'Bahraini dinar', 'Bahrain');
  doLoad('BIF', 0, 'Burundian franc', 'Burundi');
  doLoad('BMD', 2, 'Bermudian dollar', 'Bermuda');
  doLoad('BND', 2, 'Brunei dollar', 'Brunei');
  doLoad('BOB', 2, 'Boliviano', 'Bolivia');
  doLoad('BOV', 2, 'Bolivian Mvdol (funds code)', 'Bolivia');
  doLoad('BRL', 2, 'Brazilian real', 'Brazil');
  doLoad('BSD', 2, 'Bahamian dollar', 'Bahamas');
  doLoad('BTN', 2, 'Bhutanese ngultrum', 'Bhutan');
  doLoad('BWP', 2, 'Botswana pula', 'Botswana');
  doLoad('BYN', 2, 'Belarusian ruble', 'Belarus');
  doLoad('BZD', 2, 'Belize dollar', 'Belize');
  doLoad('CAD', 2, 'Canadian dollar', 'Canada');
  doLoad('CDF', 2, 'Congolese franc', 'Democratic Republic of the Congo');
  doLoad('CHE', 2, 'WIR Euro (complementary currency)', 'Switzerland');
  doLoad('CHF', 2, 'Swiss franc', 'Switzerland,  Liechtenstein (LI)');
  doLoad('CHW', 2, 'WIR Franc (complementary currency)', 'Switzerland');
  doLoad('CLF', 4, 'Unidad de Fomento (funds code)', 'Chile');
  doLoad('CLP', 0, 'Chilean peso', 'Chile');
  doLoad('CNY', 2, 'Renminbi (Chinese) yuan[8]', 'China');
  doLoad('COP', 2, 'Colombian peso', 'Colombia');
  doLoad('COU', 2, 'Unidad de Valor Real (UVR) (funds code)[9]', 'Colombia');
  doLoad('CRC', 2, 'Costa Rican colon', 'Costa Rica');
  doLoad('CUC', 2, 'Cuban convertible peso', 'Cuba');
  doLoad('CUP', 2, 'Cuban peso', 'Cuba');
  doLoad('CVE', 0, 'Cape Verde escudo', 'Cape Verde');
  doLoad('CZK', 2, 'Czech koruna', 'Czechia [10]');
  doLoad('DJF', 0, 'Djiboutian franc', 'Djibouti');
  doLoad('DKK', 2, 'Danish krone', 'Denmark,  Faroe Islands (FO),  Greenland (GL)');
  doLoad('DOP', 2, 'Dominican peso', 'Dominican Republic');
  doLoad('DZD', 2, 'Algerian dinar', 'Algeria');
  doLoad('EGP', 2, 'Egyptian pound', 'Egypt');
  doLoad('ERN', 2, 'Eritrean nakfa', 'Eritrea');
  doLoad('ETB', 2, 'Ethiopian birr', 'Ethiopia');
  doLoad('EUR', 2, 'Euro', 'Andorra (AD),  Austria (AT),  Belgium (BE),  Cyprus (CY),  Estonia (EE),  Finland (FI),  France (FR),  Germany (DE),  Greece (GR),  Guadeloupe (GP),  Ireland (IE),  Italy (IT),  '+'Latvia (LV),  Lithuania (LT),  Luxembourg (LU),  Malta (MT),  Martinique (MQ),  Mayotte (YT),  Monaco (MC),  '+'Montenegro (ME),  Netherlands (NL),  Portugal (PT),  Réunion (RE),  Saint Barthélemy (BL),  Saint Pierre and Miquelon (PM),  San Marino (SM),  Slovakia (SK),  Slovenia (SI),  Spain (ES)');
  doLoad('FJD', 2, 'Fiji dollar', 'Fiji');
  doLoad('FKP', 2, 'Falkland Islands pound', 'Falkland Islands (pegged to GBP 1:1)');
  doLoad('GBP', 2, 'Pound sterling', 'United Kingdom, the  Isle of Man (IM, see Manx pound),  Jersey (JE, see Jersey pound), and  Guernsey (GG, see Guernsey pound)');
  doLoad('GEL', 2, 'Georgian lari', 'Georgia');
  doLoad('GHS', 2, 'Ghanaian cedi', 'Ghana');
  doLoad('GIP', 2, 'Gibraltar pound', 'Gibraltar (pegged to GBP 1:1)');
  doLoad('GMD', 2, 'Gambian dalasi', 'Gambia');
  doLoad('GNF', 0, 'Guinean franc', 'Guinea');
  doLoad('GTQ', 2, 'Guatemalan quetzal', 'Guatemala');
  doLoad('GYD', 2, 'Guyanese dollar', 'Guyana');
  doLoad('HKD', 2, 'Hong Kong dollar', 'Hong Kong');
  doLoad('HNL', 2, 'Honduran lempira', 'Honduras');
  doLoad('HRK', 2, 'Croatian kuna', 'Croatia');
  doLoad('HTG', 2, 'Haitian gourde', 'Haiti');
  doLoad('HUF', 2, 'Hungarian forint', 'Hungary');
  doLoad('IDR', 2, 'Indonesian rupiah', 'Indonesia');
  doLoad('ILS', 2, 'Israeli new shekel', 'Israel');
  doLoad('INR', 2, 'Indian rupee', 'India,  Bhutan');
  doLoad('IQD', 3, 'Iraqi dinar', 'Iraq');
  doLoad('IRR', 2, 'Iranian rial', 'Iran');
  doLoad('ISK', 0, 'Icelandic króna', 'Iceland');
  doLoad('JMD', 2, 'Jamaican dollar', 'Jamaica');
  doLoad('JOD', 3, 'Jordanian dinar', 'Jordan');
  doLoad('JPY', 0, 'Japanese yen', 'Japan');
  doLoad('KES', 2, 'Kenyan shilling', 'Kenya');
  doLoad('KGS', 2, 'Kyrgyzstani som', 'Kyrgyzstan');
  doLoad('KHR', 2, 'Cambodian riel', 'Cambodia');
  doLoad('KMF', 0, 'Comoro franc', 'Comoros');
  doLoad('KPW', 2, 'North Korean won', 'North Korea');
  doLoad('KRW', 0, 'South Korean won', 'South Korea');
  doLoad('KWD', 3, 'Kuwaiti dinar', 'Kuwait');
  doLoad('KYD', 2, 'Cayman Islands dollar', 'Cayman Islands');
  doLoad('KZT', 2, 'Kazakhstani tenge', 'Kazakhstan');
  doLoad('LAK', 2, 'Lao kip', 'Laos');
  doLoad('LBP', 2, 'Lebanese pound', 'Lebanon');
  doLoad('LKR', 2, 'Sri Lankan rupee', 'Sri Lanka');
  doLoad('LRD', 2, 'Liberian dollar', 'Liberia');
  doLoad('LSL', 2, 'Lesotho loti', 'Lesotho');
  doLoad('LYD', 3, 'Libyan dinar', 'Libya');
  doLoad('MAD', 2, 'Moroccan dirham', 'Morocco');
  doLoad('MDL', 2, 'Moldovan leu', 'Moldova');
  doLoad('MGA', 1, 'Malagasy ariary', 'Madagascar');
  doLoad('MKD', 2, 'Macedonian denar', 'Macedonia');
  doLoad('MMK', 2, 'Myanmar kyat', 'Myanmar');
  doLoad('MNT', 2, 'Mongolian tögrög', 'Mongolia');
  doLoad('MOP', 2, 'Macanese pataca', 'Macao');
  doLoad('MRU', 1, 'Mauritanian ouguiya', 'Mauritania');
  doLoad('MUR', 2, 'Mauritian rupee', 'Mauritius');
  doLoad('MVR', 2, 'Maldivian rufiyaa', 'Maldives');
  doLoad('MWK', 2, 'Malawian kwacha', 'Malawi');
  doLoad('MXN', 2, 'Mexican peso', 'Mexico');
  doLoad('MXV', 2, 'Mexican Unidad de Inversion (UDI) (funds code)', 'Mexico');
  doLoad('MYR', 2, 'Malaysian ringgit', 'Malaysia');
  doLoad('MZN', 2, 'Mozambican metical', 'Mozambique');
  doLoad('NAD', 2, 'Namibian dollar', 'Namibia');
  doLoad('NGN', 2, 'Nigerian naira', 'Nigeria');
  doLoad('NIO', 2, 'Nicaraguan córdoba', 'Nicaragua');
  doLoad('NOK', 2, 'Norwegian krone', 'Norway,  Svalbard and  Jan Mayen (SJ),  Bouvet Island (BV)');
  doLoad('NPR', 2, 'Nepalese rupee', 'Nepal');
  doLoad('NZD', 2, 'New Zealand dollar', 'New Zealand,  Cook Islands (CK),  Niue (NU),  Pitcairn Islands (PN); see also Pitcairn Islands dollar),  Tokelau (TK)');
  doLoad('OMR', 3, 'Omani rial', 'Oman');
  doLoad('PAB', 2, 'Panamanian balboa', 'Panama');
  doLoad('PEN', 2, 'Peruvian Sol', 'Peru');
  doLoad('PGK', 2, 'Papua New Guinean kina', 'Papua New Guinea');
  doLoad('PHP', 2, 'Philippine piso[13]', 'Philippines');
  doLoad('PKR', 2, 'Pakistani rupee', 'Pakistan');
  doLoad('PLN', 2, 'Polish złoty', 'Poland');
  doLoad('PYG', 0, 'Paraguayan guaraní', 'Paraguay');
  doLoad('QAR', 2, 'Qatari riyal', 'Qatar');
  doLoad('RON', 2, 'Romanian leu', 'Romania');
  doLoad('RSD', 2, 'Serbian dinar', 'Serbia');
  doLoad('RUB', 2, 'Russian ruble', 'Russia');
  doLoad('RWF', 0, 'Rwandan franc', 'Rwanda');
  doLoad('SAR', 2, 'Saudi riyal', 'Saudi Arabia');
  doLoad('SBD', 2, 'Solomon Islands dollar', 'Solomon Islands');
  doLoad('SCR', 2, 'Seychelles rupee', 'Seychelles');
  doLoad('SDG', 2, 'Sudanese pound', 'Sudan');
  doLoad('SEK', 2, 'Swedish krona/kronor', 'Sweden');
  doLoad('SGD', 2, 'Singapore dollar', 'Singapore');
  doLoad('SHP', 2, 'Saint Helena pound', 'Saint Helena (SH-SH),  Ascension Island (SH-AC),  Tristan da Cunha');
  doLoad('SLL', 2, 'Sierra Leonean leone', 'Sierra Leone');
  doLoad('SOS', 2, 'Somali shilling', 'Somalia');
  doLoad('SRD', 2, 'Surinamese dollar', 'Suriname');
  doLoad('SSP', 2, 'South Sudanese pound', 'South Sudan');
  doLoad('STN', 2, 'São Tomé and Príncipe dobra', 'São Tomé and Príncipe');
  doLoad('SVC', 2, 'Salvadoran colón', 'El Salvador');
  doLoad('SYP', 2, 'Syrian pound', 'Syria');
  doLoad('SZL', 2, 'Swazi lilangeni', 'Swaziland');
  doLoad('THB', 2, 'Thai baht', 'Thailand');
  doLoad('TJS', 2, 'Tajikistani somoni', 'Tajikistan');
  doLoad('TMT', 2, 'Turkmenistan manat', 'Turkmenistan');
  doLoad('TND', 3, 'Tunisian dinar', 'Tunisia');
  doLoad('TOP', 2, 'Tongan paʻanga', 'Tonga');
  doLoad('TRY', 2, 'Turkish lira', 'Turkey');
  doLoad('TTD', 2, 'Trinidad and Tobago dollar', 'Trinidad and Tobago');
  doLoad('TWD', 2, 'New Taiwan dollar', 'Taiwan');
  doLoad('TZS', 2, 'Tanzanian shilling', 'Tanzania');
  doLoad('UAH', 2, 'Ukrainian hryvnia', 'Ukraine');
  doLoad('UGX', 0, 'Ugandan shilling', 'Uganda');
  doLoad('USD', 2, 'United States dollar', 'United States,  American Samoa (AS),  Barbados (BB) (as well as Barbados Dollar),  Bermuda (BM) (as well as Bermudian Dollar),  British Indian Ocean Territory (IO) (also uses GBP),  British Virgin Islands (VG),  '+'Caribbean Netherlands (BQ - Bonaire, Sint Eustatius and Saba),  Ecuador (EC),  El Salvador (SV),  Guam (GU),  Haiti (HT),  Marshall Islands (MH),  Federated States of Micronesia (FM),  '+'Northern Mariana Islands (MP),  Palau (PW),  Panama (PA) (as well as Panamanian Balboa),  Puerto Rico (PR),  Timor-Leste (TL),  Turks and Caicos Islands (TC),  U.S. Virgin Islands (VI),  United States Minor Outlying Islands');
  doLoad('USN', 2, 'United States dollar (next day) (funds code)', 'United States');
  doLoad('UYI', 0, 'Uruguay Peso en Unidades Indexadas (URUIURUI) (funds code)', 'Uruguay');
  doLoad('UYU', 2, 'Uruguayan peso', 'Uruguay');
  doLoad('UZS', 2, 'Uzbekistan som', 'Uzbekistan');
  doLoad('VEF', 2, 'Venezuelan bolívar', 'Venezuela');
  doLoad('VND', 0, 'Vietnamese đồng', 'Vietnam');
  doLoad('VUV', 0, 'Vanuatu vatu', 'Vanuatu');
  doLoad('WST', 2, 'Samoan tala', 'Samoa');
  doLoad('XAF', 0, 'CFA franc BEAC', 'Cameroon (CM),  Central African Republic (CF),  Republic of the Congo (CG),  Chad (TD),  Equatorial Guinea (GQ),  Gabon (GA)');
  doLoad('XAG', -1, 'Silver (one troy ounce)', '');
  doLoad('XAU', -1, 'Gold (one troy ounce)', '');
  doLoad('XBA', -1, 'European Composite Unit (EURCO) (bond market unit)', '');
  doLoad('XBB', -1, 'European Monetary Unit (E.M.U.-6) (bond market unit)', '');
  doLoad('XBC', -1, 'European Unit of Account 9 (E.U.A.-9) (bond market unit)', '');
  doLoad('XBD', -1, 'European Unit of Account 17 (E.U.A.-17) (bond market unit)', '');
  doLoad('XCD', 2, 'East Caribbean dollar', 'Anguilla (AI),  Antigua and Barbuda (AG),  Dominica (DM),  Grenada (GD),  Montserrat (MS),  Saint Kitts and Nevis (KN),  Saint Lucia (LC),  Saint Vincent and the Grenadines (VC)');
  doLoad('XDR', -1, 'Special drawing rights', 'International Monetary Fund');
  doLoad('XOF', 0, 'CFA franc BCEAO', 'Benin (BJ),  Burkina Faso (BF),  Côte d''Ivoire (CI),  Guinea-Bissau (GW),  Mali (ML),  Niger (NE),  Senegal (SN),  Togo (TG)');
  doLoad('XPD', -1, 'Palladium (one troy ounce)', '');
  doLoad('XPF', 0, 'CFP franc (franc Pacifique)', 'French territories of the Pacific Ocean:  French Polynesia (PF),  New Caledonia (NC),  Wallis and Futuna (WF)');
  doLoad('XPT', -1, 'Platinum (one troy ounce)', '');
  doLoad('XSU', -1, 'SUCRE', 'Unified System for Regional Compensation (SUCRE)[15]');
  doLoad('XTS', -1, 'Code reserved for testing purposes', '');
  doLoad('XUA', -1, 'ADB Unit of Account', 'African Development Bank[16]');
  doLoad('XXX', -1, 'No currency', '');
  doLoad('YER', 2, 'Yemeni rial', 'Yemen');
  doLoad('ZAR', 2, 'South African rand', 'South Africa');
  doLoad('ZMW', 2, 'Zambian kwacha', 'Zambia');
  doLoad('ZWL', 2, 'Zimbabwean dollar A/10', 'Zimbabwe');
end;

function TIso4217Services.locate(code : String; var message : String) : TCodeSystemProviderContext;
begin
  result := FMap[code];
end;


function TIso4217Services.Code(context : TCodeSystemProviderContext) : string;
begin
  result := TIso4217Concept(context).code;
end;

function TIso4217Services.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

destructor TIso4217Services.Destroy;
begin
  FMap.free;
  FCodes.Free;
  inherited;
end;

function TIso4217Services.Display(context : TCodeSystemProviderContext; lang : String) : string;
begin
  result := TIso4217Concept(context).display;
end;

procedure TIso4217Services.Displays(context: TCodeSystemProviderContext; list: TStringList; lang : String);
begin
  list.Add(Display(context, lang));
end;

function TIso4217Services.IsAbstract(context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // 4217 doesn't do abstract
end;

function TIso4217Services.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TIso4217Services.Link: TIso4217Services;
begin
  result := TIso4217Services(Inherited Link);
end;

function TIso4217Services.ChildCount(context : TCodeSystemProviderContext) : integer;
begin
  if (context = nil) then
    result := TotalCount
  else
    result := 0; // no children
end;

function TIso4217Services.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  result := FCodes[ndx];
end;

function TIso4217Services.locateIsA(code, parent : String) : TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('locateIsA not supported by Iso4217'); // Iso4217 doesn't have formal subsumption property, so this is not used
end;


function TIso4217Services.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  // nothing
  result := true;
end;

function TIso4217Services.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.create('not done yet');
end;

function TIso4217Services.subsumesTest(codeA, codeB: String): String;
begin
  result := 'not-subsumed';
end;

function TIso4217Services.filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
var
  res : TIso4217ConceptFilter;
  c : TIso4217Concept;
begin
  if (prop = 'decimals') and (op = foEqual) then
  begin
    res := TIso4217ConceptFilter.create;
    try
      for c in FCodes do
        if inttostr(c.decimals) = value then
          res.flist.Add(c.link);
        result := res.link;
    finally
      res.Free;
    end;
  end
  else
    raise ETerminologyError.create('the filter '+prop+' '+CODES_TFhirFilterOperator[op]+' = '+value+' is not support for '+system(nil));
end;

function TIso4217Services.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('not done yet');
end;

function TIso4217Services.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  TIso4217ConceptFilter(ctxt).FCursor := TIso4217ConceptFilter(ctxt).FCursor + 1;
  result := TIso4217ConceptFilter(ctxt).FCursor < TIso4217ConceptFilter(ctxt).FList.Count;
end;

function TIso4217Services.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  result := TIso4217ConceptFilter(ctxt).FList[TIso4217ConceptFilter(ctxt).FCursor];
end;

function TIso4217Services.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyError.create('not done yet');
end;

procedure TIso4217Services.Close(ctxt: TCodeSystemProviderContext);
begin
//  ctxt.free;
end;

procedure TIso4217Services.Close(ctxt : TCodeSystemProviderFilterContext);
begin
  ctxt.free;
end;

procedure TIso4217Services.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  raise ETerminologyError.create('not done yet');
end;


{ TIso4217Concept }

function TIso4217Concept.link: TIso4217Concept;
begin
  result := TIso4217Concept(inherited Link);
end;

{ TIso4217ConceptFilter }

constructor TIso4217ConceptFilter.Create;
begin
  inherited;
  FList := TFslList<TIso4217Concept>.Create;
  FCursor := -1;
end;

destructor TIso4217ConceptFilter.Destroy;
begin
  FList.Free;
  inherited;
end;

function TIso4217ConceptFilter.link: TIso4217ConceptFilter;
begin
  result := TIso4217ConceptFilter(inherited Link);
end;

end.

