unit fsl_lang;

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
  SysUtils, Classes,
  fsl_utilities, fsl_base, fsl_stream;

type
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

  TIETFLangPartType = (lptNone, lptLanguage, lptExtLang, lptScript, lptRegion, lptVariant, lptExtension);

  { TIETFLang }

  TIETFLang = class (TFslObject)
  private
    FCode : String;
    FLanguage : string;
    FScript : string;
    FRegion : String;
    FVariant : String;
    FExtension : String;
    FExtLang : TArray<String>;
    FPrivateUse : TArray<String>;

    procedure addExtLang(s : String);
    procedure addPrivateUse(s : String);
    constructor Create(code : String);
  public
    function Link : TIETFLang; overload;

    property code : String read FCode;
    property language : string read Flanguage;
    property extLang : TArray<String> read FextLang;
    property script : string read Fscript;
    property region : String read Fregion;
    property variant : String read Fvariant;
    property extension : String read Fextension;
    property privateUse : TArray<String> read FPrivateUse;

    function matches(other : TIETFLang; depth : TIETFLangPartType) : boolean; overload;
    function matches(other : TIETFLang) : boolean; overload;

    function isLangRegion : boolean;
  end;

  { TIETFLanguageEntry }

  TIETFLanguageEntry = class (TFslObject)
  private
    FCode: String;
    FDisplays: TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;

    property code : String read FCode write FCode;
    property displays : TStringList read FDisplays;
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
    FParsed : TFslMap<TIETFLang>;
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
    procedure clear;

    function link : TIETFLanguageDefinitions; overload;

    class function checkSource(source : String) : String;
    function parse(code : String; var msg : String) : TIETFLang; overload;
    function parse(code : String) : TIETFLang; overload;
    function present(code : TIETFLang; i : integer = 0) : String; overload;
    function present(code : TIETFLang; i : integer; template : String) : String; overload;
    function displayCount(code : TIETFLang) : integer;

    function getDisplayForRegion(code : String):String;
    function getDisplayForLang(code : String):String;
  end;


  TIso4217Currency = class (TFslObject)
  private
    FDisplay: String;
    FCode: String;
    FDecimals: integer;
    FSymbol: String;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    function link : TIso4217Currency; overload;

    property code : String read FCode write FCode;
    property display : String read FDisplay write FDisplay;
    property decimals : integer read FDecimals write FDecimals;
    property symbol : String read FSymbol write FSymbol;
  end;

  TIso4217CurrencySet = class (TFslObject)
  private
    FCodes : TFslList<TIso4217Currency>;
    FMap : TFslMap<TIso4217Currency>;

    procedure load;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TIso4217CurrencySet; overload;

    property Codes : TFslList<TIso4217Currency> read FCodes;
    property Map : TFslMap<TIso4217Currency> read FMap;
  end;

function currencyForIso4217Code(code : String) : String;


implementation

function currencyForIso4217Code(code : String) : String;
var
  l : TIso4217CurrencySet;
  c : TIso4217Currency;
begin
  l := TIso4217CurrencySet.Create;
  try
    if l.FMap.TryGetValue(code, c) then
      result := c.symbol
    else
      result := '';
  finally
    l.free;
  end;
end;

{ TIso4217Currency }

function TIso4217Currency.link: TIso4217Currency;
begin
  result := TIso4217Currency(inherited link);
end;

function TIso4217Currency.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, (FDisplay.length * sizeof(char)) + 12);
  inc(result, (FCode.length * sizeof(char)) + 12);
  inc(result, (FSymbol.length * sizeof(char)) + 12);
end;

{ TIso4217CurrencySet }

constructor TIso4217CurrencySet.Create;
begin
  inherited;
  FCodes := TFslList<TIso4217Currency>.Create;
  FMap := TFslMap<TIso4217Currency>.create('tx.currency');
  FMap.defaultValue := nil;
  Load;
end;

destructor TIso4217CurrencySet.Destroy;
begin
  FMap.free;
  FCodes.free;
  inherited;
end;

function TIso4217CurrencySet.Link: TIso4217CurrencySet;
begin
  result := TIso4217CurrencySet(inherited link);
end;

procedure TIso4217CurrencySet.load;
  procedure doLoad(code : string; decimals : integer; symbol, display, countries : String);
  var
    c : TIso4217Currency;
  begin
    c := TIso4217Currency.Create;
    try
      c.code := code;
      c.display := display;
      c.decimals := decimals;
      c.symbol := symbol;
      FCodes.Add(c.Link);
      FMap.Add(code, c.Link);
    finally
      c.free;
    end;
  end;
begin
  doLoad('AED', 2, 'DH', 'United Arab Emirates dirham', 'United Arab Emirates');
  doLoad('AFN', 2, ''#$060B'', 'Afghan afghani', 'Afghanistan');
  doLoad('ALL', 2, 'Lek', 'Albanian lek', 'Albania');
  doLoad('AMD', 2, '', 'Armenian dram', 'Armenia');
  doLoad('ANG', 2, ''#$0192'', 'Netherlands Antillean guilder', 'Cura'#$00E7'ao (CW),  Sint Maarten (SX)');
  doLoad('AOA', 2, 'Kz', 'Angolan kwanza', 'Angola');
  doLoad('ARS', 2, '$', 'Argentine peso', 'Argentina');
  doLoad('AUD', 2, '$', 'Australian dollar', 'Australia,  Christmas Island (CX),  Cocos (Keeling) Islands (CC),  Heard Island and McDonald Islands (HM),  Kiribati (KI),  Nauru (NR),  Norfolk Island (NF),  Tuvalu (TV)');
  doLoad('AWG', 2, ''#$0192'', 'Aruban florin', 'Aruba');
  doLoad('AZN', 2, ''#$20BC'', 'Azerbaijani manat', 'Azerbaijan');
  doLoad('BAM', 2, 'KM', 'Bosnia and Herzegovina convertible mark', 'Bosnia and Herzegovina');
  doLoad('BBD', 2, '$', 'Barbados dollar', 'Barbados');
  doLoad('BDT', 2, '', 'Bangladeshi taka', 'Bangladesh');
  doLoad('BGN', 2, ''#$043B#$0432'', 'Bulgarian lev', 'Bulgaria');
  doLoad('BHD', 3, '', 'Bahraini dinar', 'Bahrain');
  doLoad('BIF', 0, '', 'Burundian franc', 'Burundi');
  doLoad('BMD', 2, '', 'Bermudian dollar', 'Bermuda');
  doLoad('BND', 2, '$', 'Brunei dollar', 'Brunei');
  doLoad('BOB', 2, '', 'Boliviano', 'Bolivia');
  doLoad('BOV', 2, '', 'Bolivian Mvdol (funds code)', 'Bolivia');
  doLoad('BRL', 2, 'R$', 'Brazilian real', 'Brazil');
  doLoad('BSD', 2, '', 'Bahamian dollar', 'Bahamas');
  doLoad('BTN', 2, '', 'Bhutanese ngultrum', 'Bhutan');
  doLoad('BWP', 2, '', 'Botswana pula', 'Botswana');
  doLoad('BYN', 2, '', 'Belarusian ruble', 'Belarus');
  doLoad('BZD', 2, '', 'Belize dollar', 'Belize');
  doLoad('CAD', 2, '$', 'Canadian dollar', 'Canada');
  doLoad('CDF', 2, '', 'Congolese franc', 'Democratic Republic of the Congo');
  doLoad('CHE', 2, '', 'WIR Euro (complementary currency)', 'Switzerland');
  doLoad('CHF', 2, 'CHF', 'Swiss franc', 'Switzerland,  Liechtenstein (LI)');
  doLoad('CHW', 2, '', 'WIR Franc (complementary currency)', 'Switzerland');
  doLoad('CLF', 4, '', 'Unidad de Fomento (funds code)', 'Chile');
  doLoad('CLP', 0, '$', 'Chilean peso', 'Chile');
  doLoad('CNY', 2, ''#$00A5'', 'Renminbi (Chinese) yuan[8]', 'China');
  doLoad('COP', 2, '$', 'Colombian peso', 'Colombia');
  doLoad('COU', 2, '', 'Unidad de Valor Real (UVR) (funds code)[9]', 'Colombia');
  doLoad('CRC', 2, ''#$20A1'', 'Costa Rican colon', 'Costa Rica');
  doLoad('CUC', 2, '', 'Cuban convertible peso', 'Cuba');
  doLoad('CUP', 2, ''#$20B1'', 'Cuban peso', 'Cuba');
  doLoad('CVE', 0, '', 'Cape Verde escudo', 'Cape Verde');
  doLoad('CZK', 2, 'K'#$010D'', 'Czech koruna', 'Czechia [10]');
  doLoad('DJF', 0, '', 'Djiboutian franc', 'Djibouti');
  doLoad('DKK', 2, 'kr', 'Danish krone', 'Denmark,  Faroe Islands (FO),  Greenland (GL)');
  doLoad('DOP', 2, 'RD$', 'Dominican peso', 'Dominican Republic');
  doLoad('DZD', 2, '', 'Algerian dinar', 'Algeria');
  doLoad('EGP', 2, ''#$00A3'', 'Egyptian pound', 'Egypt');
  doLoad('ERN', 2, '', 'Eritrean nakfa', 'Eritrea');
  doLoad('ETB', 2, '', 'Ethiopian birr', 'Ethiopia');
  doLoad('EUR', 2, ''#$20AC'', 'Euro', 'Andorra (AD),  Austria (AT),  Belgium (BE),  Cyprus (CY),  Estonia (EE),  Finland (FI),  France (FR),  Germany (DE),  Greece (GR),  Guadeloupe (GP),  Ireland (IE),  Italy (IT),  '+'Latvia (LV),  Lithuania (LT),  Luxembourg (LU),  Malta (MT),  Martinique (MQ),  Mayotte (YT),  Monaco (MC),  '+'Montenegro (ME),  Netherlands (NL),  Portugal (PT),  R'#$00E9'union (RE),  Saint Barth'#$00E9'lemy (BL),  Saint Pierre and Miquelon (PM),  San Marino (SM),  Slovakia (SK),  Slovenia (SI),  Spain (ES)');
  doLoad('FJD', 2, '$', 'Fiji dollar', 'Fiji');
  doLoad('FKP', 2, ''#$00A3'', 'Falkland Islands pound', 'Falkland Islands (pegged to GBP 1:1)');
  doLoad('GBP', 2, ''#$00A3'', 'Pound sterling', 'United Kingdom, the  Isle of Man (IM, see Manx pound),  Jersey (JE, see Jersey pound), and  Guernsey (GG, see Guernsey pound)');
  doLoad('GEL', 2, '', 'Georgian lari', 'Georgia');
  doLoad('GGP', 2, ''#$00A3'', 'Guernsey Pound', 'Guernsey');
  doLoad('GHS', 2, ''#$00A2'', 'Ghanaian cedi', 'Ghana');
  doLoad('GIP', 2, ''#$00A3'', 'Gibraltar pound', 'Gibraltar (pegged to GBP 1:1)');
  doLoad('GMD', 2, '', 'Gambian dalasi', 'Gambia');
  doLoad('GNF', 0, '', 'Guinean franc', 'Guinea');
  doLoad('GTQ', 2, 'Q', 'Guatemalan quetzal', 'Guatemala');
  doLoad('GYD', 2, '$', 'Guyanese dollar', 'Guyana');
  doLoad('HKD', 2, '$', 'Hong Kong dollar', 'Hong Kong');
  doLoad('HNL', 2, 'L', 'Honduran lempira', 'Honduras');
  doLoad('HRK', 2, 'kn', 'Croatian kuna', 'Croatia');
  doLoad('HTG', 2, '', 'Haitian gourde', 'Haiti');
  doLoad('HUF', 2, 'Ft', 'Hungarian forint', 'Hungary');
  doLoad('IDR', 2, 'Rp', 'Indonesian rupiah', 'Indonesia');
  doLoad('ILS', 2, ''#$20AA'', 'Israeli new shekel', 'Israel');
  doLoad('IMP', 2, ''#$00A3'', 'Isle of Man Pound', 'Isle of Man');
  doLoad('INR', 2, '', 'Indian rupee', 'India,  Bhutan');
  doLoad('IQD', 3, '', 'Iraqi dinar', 'Iraq');
  doLoad('IRR', 2, ''#$FDFC'', 'Iranian rial', 'Iran');
  doLoad('ISK', 0, 'kr', 'Icelandic kr'#$00F3'na', 'Iceland');
  doLoad('JEP', 2, ''#$00A3'', 'Jersey Pound', 'Jersey');
  doLoad('JMD', 2, 'J$', 'Jamaican dollar', 'Jamaica');
  doLoad('JOD', 3, '', 'Jordanian dinar', 'Jordan');
  doLoad('JPY', 0, ''#$00A5'', 'Japanese yen', 'Japan');
  doLoad('KES', 2, '', 'Kenyan shilling', 'Kenya');
  doLoad('KGS', 2, ''#$043B#$0432'', 'Kyrgyzstani som', 'Kyrgyzstan');
  doLoad('KHR', 2, ''#$17DB'', 'Cambodian riel', 'Cambodia');
  doLoad('KMF', 0, '', 'Comoro franc', 'Comoros');
  doLoad('KPW', 2, ''#$20A9'', 'North Korean won', 'North Korea');
  doLoad('KRW', 0, ''#$20A9'', 'South Korean won', 'South Korea');
  doLoad('KWD', 3, '', 'Kuwaiti dinar', 'Kuwait');
  doLoad('KYD', 2, '$', 'Cayman Islands dollar', 'Cayman Islands');
  doLoad('KZT', 2, ''#$043B#$0432'', 'Kazakhstani tenge', 'Kazakhstan');
  doLoad('LAK', 2, ''#$20AD'', 'Lao kip', 'Laos');
  doLoad('LBP', 2, ''#$00A3'', 'Lebanese pound', 'Lebanon');
  doLoad('LKR', 2, ''#$20A8'', 'Sri Lankan rupee', 'Sri Lanka');
  doLoad('LRD', 2, '$', 'Liberian dollar', 'Liberia');
  doLoad('LSL', 2, '', 'Lesotho loti', 'Lesotho');
  doLoad('LYD', 3, '', 'Libyan dinar', 'Libya');
  doLoad('MAD', 2, '', 'Moroccan dirham', 'Morocco');
  doLoad('MDL', 2, '', 'Moldovan leu', 'Moldova');
  doLoad('MGA', 1, '', 'Malagasy ariary', 'Madagascar');
  doLoad('MKD', 2, ''#$0434#$0435#$043D'', 'Macedonian denar', 'Macedonia');
  doLoad('MMK', 2, '', 'Myanmar kyat', 'Myanmar');
  doLoad('MNT', 2, ''#$20AE'', 'Mongolian t'#$00F6'gr'#$00F6'g', 'Mongolia');
  doLoad('MOP', 2, '', 'Macanese pataca', 'Macao');
  doLoad('MRU', 1, '', 'Mauritanian ouguiya', 'Mauritania');
  doLoad('MUR', 2, ''#$20A8'', 'Mauritian rupee', 'Mauritius');
  doLoad('MVR', 2, '', 'Maldivian rufiyaa', 'Maldives');
  doLoad('MWK', 2, '', 'Malawian kwacha', 'Malawi');
  doLoad('MXN', 2, '$', 'Mexican peso', 'Mexico');
  doLoad('MXV', 2, '', 'Mexican Unidad de Inversion (UDI) (funds code)', 'Mexico');
  doLoad('MYR', 2, 'RM', 'Malaysian ringgit', 'Malaysia');
  doLoad('MZN', 2, 'MT', 'Mozambican metical', 'Mozambique');
  doLoad('NAD', 2, '$', 'Namibian dollar', 'Namibia');
  doLoad('NGN', 2, ''#$20A6'', 'Nigerian naira', 'Nigeria');
  doLoad('NIO', 2, 'C$', 'Nicaraguan c'#$00F3'rdoba', 'Nicaragua');
  doLoad('NOK', 2, 'kr', 'Norwegian krone', 'Norway,  Svalbard and  Jan Mayen (SJ),  Bouvet Island (BV)');
  doLoad('NPR', 2, ''#$20A8'', 'Nepalese rupee', 'Nepal');
  doLoad('NZD', 2, '$', 'New Zealand dollar', 'New Zealand,  Cook Islands (CK),  Niue (NU),  Pitcairn Islands (PN); see also Pitcairn Islands dollar),  Tokelau (TK)');
  doLoad('OMR', 3, ''#$FDFC'', 'Omani rial', 'Oman');
  doLoad('PAB', 2, 'B/.', 'Panamanian balboa', 'Panama');
  doLoad('PEN', 2, 'S/.', 'Peruvian Sol', 'Peru');
  doLoad('PGK', 2, '', 'Papua New Guinean kina', 'Papua New Guinea');
  doLoad('PHP', 2, ''#$20B1'', 'Philippine piso[13]', 'Philippines');
  doLoad('PKR', 2, ''#$20A8'', 'Pakistani rupee', 'Pakistan');
  doLoad('PLN', 2, 'z'#$0142'', 'Polish z'#$0142'oty', 'Poland');
  doLoad('PYG', 0, 'Gs', 'Paraguayan guaran'#$00ED'', 'Paraguay');
  doLoad('QAR', 2, ''#$FDFC'', 'Qatari riyal', 'Qatar');
  doLoad('RON', 2, 'lei', 'Romanian leu', 'Romania');
  doLoad('RSD', 2, ''#$0414#$0438#$043D'.', 'Serbian dinar', 'Serbia');
  doLoad('RUB', 2, ''#$20BD'', 'Russian ruble', 'Russia');
  doLoad('RWF', 0, '', 'Rwandan franc', 'Rwanda');
  doLoad('SAR', 2, ''#$FDFC'', 'Saudi riyal', 'Saudi Arabia');
  doLoad('SBD', 2, '$', 'Solomon Islands dollar', 'Solomon Islands');
  doLoad('SCR', 2, ''#$20A8'', 'Seychelles rupee', 'Seychelles');
  doLoad('SDG', 2, '', 'Sudanese pound', 'Sudan');
  doLoad('SEK', 2, 'kr', 'Swedish krona/kronor', 'Sweden');
  doLoad('SGD', 2, '$', 'Singapore dollar', 'Singapore');
  doLoad('SHP', 2, ''#$00A3'', 'Saint Helena pound', 'Saint Helena (SH-SH),  Ascension Island (SH-AC),  Tristan da Cunha');
  doLoad('SLL', 2, '', 'Sierra Leonean leone', 'Sierra Leone');
  doLoad('SOS', 2, 'S', 'Somali shilling', 'Somalia');
  doLoad('SRD', 2, '$', 'Surinamese dollar', 'Suriname');
  doLoad('SSP', 2, '', 'South Sudanese pound', 'South Sudan');
  doLoad('STN', 2, '', 'S'#$00E3'o Tom'#$00E9' and Pr'#$00ED'ncipe dobra', 'S'#$00E3'o Tom'#$00E9' and Pr'#$00ED'ncipe');
  doLoad('SVC', 2, '$', 'Salvadoran col'#$00F3'n', 'El Salvador');
  doLoad('SYP', 2, ''#$00A3'', 'Syrian pound', 'Syria');
  doLoad('SZL', 2, '', 'Swazi lilangeni', 'Swaziland');
  doLoad('THB', 2, ''#$0E3F'', 'Thai baht', 'Thailand');
  doLoad('TJS', 2, '', 'Tajikistani somoni', 'Tajikistan');
  doLoad('TMT', 2, '', 'Turkmenistan manat', 'Turkmenistan');
  doLoad('TND', 3, '', 'Tunisian dinar', 'Tunisia');
  doLoad('TOP', 2, '', 'Tongan pa'#$02BB'anga', 'Tonga');
  doLoad('TRY', 2, '', 'Turkish lira', 'Turkey');
  doLoad('TTD', 2, 'TT$', 'Trinidad and Tobago dollar', 'Trinidad and Tobago');
  doLoad('TVD', 2, '$', 'Tuvalu Dollar', 'Tuvalu');
  doLoad('TWD', 2, 'NT$', 'New Taiwan dollar', 'Taiwan');
  doLoad('TZS', 2, '', 'Tanzanian shilling', 'Tanzania');
  doLoad('UAH', 2, ''#$20B4'', 'Ukrainian hryvnia', 'Ukraine');
  doLoad('UGX', 0, '', 'Ugandan shilling', 'Uganda');
  doLoad('USD', 2, '$', 'United States dollar', 'United States,  American Samoa (AS),  Barbados (BB) (as well as Barbados Dollar),  Bermuda (BM) (as well as Bermudian Dollar),  British Indian Ocean Territory (IO) (also uses GBP),  British Virgin Islands (VG),  '+'Caribbean Netherlands (BQ - Bonaire, Sint Eustatius and Saba),  Ecuador (EC),  El Salvador (SV),  Guam (GU),  Haiti (HT),  Marshall Islands (MH),  Federated States of Micronesia (FM),  '+'Northern Mariana Islands (MP),  Palau (PW),  Panama (PA) (as well as Panamanian Balboa),  Puerto Rico (PR),  Timor-Leste (TL),  Turks and Caicos Islands (TC),  U.S. Virgin Islands (VI),  United States Minor Outlying Islands');
  doLoad('USN', 2, '', 'United States dollar (next day) (funds code)', 'United States');
  doLoad('UYI', 0, '$U', 'Uruguay Peso en Unidades Indexadas (URUIURUI) (funds code)', 'Uruguay');
  doLoad('UYU', 2, '', 'Uruguayan peso', 'Uruguay');
  doLoad('UZS', 2, ''#$043B#$0432'', 'Uzbekistan som', 'Uzbekistan');
  doLoad('VEF', 2, 'Bs', 'Venezuelan bol'#$00ED'var', 'Venezuela');
  doLoad('VND', 0, ''#$20AB'', 'Vietnamese '#$0111#$1ED3'ng', 'Vietnam');
  doLoad('VUV', 0, '', 'Vanuatu vatu', 'Vanuatu');
  doLoad('WST', 2, '', 'Samoan tala', 'Samoa');
  doLoad('XAF', 0, '', 'CFA franc BEAC', 'Cameroon (CM),  Central African Republic (CF),  Republic of the Congo (CG),  Chad (TD),  Equatorial Guinea (GQ),  Gabon (GA)');
  doLoad('XAG', -1, '', 'Silver (one troy ounce)', '');
  doLoad('XAU', -1, '', 'Gold (one troy ounce)', '');
  doLoad('XBA', -1, '', 'European Composite Unit (EURCO) (bond market unit)', '');
  doLoad('XBB', -1, '', 'European Monetary Unit (E.M.U.-6) (bond market unit)', '');
  doLoad('XBC', -1, '', 'European Unit of Account 9 (E.U.A.-9) (bond market unit)', '');
  doLoad('XBD', -1, '', 'European Unit of Account 17 (E.U.A.-17) (bond market unit)', '');
  doLoad('XCD', 2, '$', 'East Caribbean dollar', 'Anguilla (AI),  Antigua and Barbuda (AG),  Dominica (DM),  Grenada (GD),  Montserrat (MS),  Saint Kitts and Nevis (KN),  Saint Lucia (LC),  Saint Vincent and the Grenadines (VC)');
  doLoad('XDR', -1, '', 'Special drawing rights', 'International Monetary Fund');
  doLoad('XOF', 0, '', 'CFA franc BCEAO', 'Benin (BJ),  Burkina Faso (BF),  C'#$00F4'te d''Ivoire (CI),  Guinea-Bissau (GW),  Mali (ML),  Niger (NE),  Senegal (SN),  Togo (TG)');
  doLoad('XPD', -1, '', 'Palladium (one troy ounce)', '');
  doLoad('XPF', 0, '', 'CFP franc (franc Pacifique)', 'French territories of the Pacific Ocean:  French Polynesia (PF),  New Caledonia (NC),  Wallis and Futuna (WF)');
  doLoad('XPT', -1, '', 'Platinum (one troy ounce)', '');
  doLoad('XSU', -1, '', 'SUCRE', 'Unified System for Regional Compensation (SUCRE)[15]');
  doLoad('XTS', -1, '', 'Code reserved for testing purposes', '');
  doLoad('XUA', -1, '', 'ADB Unit of Account', 'African Development Bank[16]');
  doLoad('XXX', -1, '', 'No currency', '');
  doLoad('YER', 2, ''#$FDFC'', 'Yemeni rial', 'Yemen');
  doLoad('ZAR', 2, 'R', 'South African rand', 'South Africa');
  doLoad('ZMW', 2, '', 'Zambian kwacha', 'Zambia');
  doLoad('ZWL', 2, 'Z$', 'Zimbabwean dollar A/10', 'Zimbabwe');

end;

function TIso4217CurrencySet.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FCodes.sizeInBytes(magic));
  inc(result, FMap.sizeInBytes(magic));
end;

{ TIETFLang }

constructor TIETFLang.Create(code: String);
begin
  inherited Create;
  self.FCode := code;
end;

function TIETFLang.matches(other: TIETFLang; depth: TIETFLangPartType): boolean;
begin
  if depth >= lptExtension then
    if (FExtension <> other.FExtension) then
      exit(false);

  if depth >= lptVariant then
    if (FVariant <> other.FVariant) then
      exit(false);

  if depth >= lptRegion then
    if (FRegion <> other.FRegion) then
      exit(false);

  if depth >= lptScript then
    if (FScript <> other.FScript) then
      exit(false);
//
//  if depth >= lptExtLang then
//    if (FExtLang <> '') and (FExtLang <> other.FExtLang) then
//      exit(false);

  if depth >= lptLanguage then
    exit(FLanguage = other.FLanguage);
  result := true;
end;

procedure TIETFLang.addExtLang(s: String);
begin
  SetLength(FExtLang, length(FExtLang)+1);
  FExtLang[length(FExtLang)-1] := s;
end;

procedure TIETFLang.addPrivateUse(s: String);
begin
  SetLength(FPrivateUse, length(FPrivateUse)+1);
  FPrivateUse[length(FPrivateUse)-1] := s;
end;


function TIETFLang.isLangRegion: boolean;
begin
  result := (language <> '') and (region <> '') and
    (length(extLang) = 0) and (script = '') and (variant = '') and (extension = '') and (length(FPrivateUse) = 0);

end;

function TIETFLang.Link: TIETFLang;
begin
  result := TIETFLang(inherited link);
end;

function TIETFLang.matches(other: TIETFLang): boolean;
begin
  if other = nil then
      exit(false);

  if FExtension <> '' then
    if FExtension <> other.FExtension then
      exit(false);

  if FVariant <> '' then
    if FVariant <> other.FVariant then
      exit(false);

  if FRegion <> '' then
    if FRegion <> other.FRegion then
      exit(false);

  if FScript <> '' then
    if FScript <> other.FScript then
      exit(false);

  if length(FExtLang) > 0 then
    if FExtLang <> other.FExtLang then
      exit(false);

  exit(FLanguage = other.FLanguage);
end;

{ TIETFLanguageEntry }

constructor TIETFLanguageEntry.Create;
begin
  inherited Create;
  FDisplays := TStringList.create;
end;

destructor TIETFLanguageEntry.Destroy;
begin
  FDisplays.free;
  inherited Destroy;
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
  FParsed := TFslMap<TIETFLang>.create('tx.lang.parsed');
  Load(source);
end;

destructor TIETFLanguageDefinitions.Destroy;
begin
  FParsed.free;
  FVariants.free;
  FScripts.free;
  FExtLanguages.free;
  FRegions.free;
  FLanguages.free;
  inherited;
end;

procedure TIETFLanguageDefinitions.clear;
begin
  FLanguages.Clear;
  FExtLanguages.Clear;
  FScripts.Clear;
  FRegions.Clear;
  FVariants.Clear;
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

function TIETFLanguageDefinitions.link: TIETFLanguageDefinitions;
begin
  result := TIETFLanguageDefinitions(inherited link);
end;

function TIETFLanguageDefinitions.getDisplayForLang(code: String): String;
begin
//  if not FLanguages.TryGetValue(code, result) then
//    result := '??';
end;


function TIETFLanguageDefinitions.parse(code : String; var msg : String) : TIETFLang;
var
  parts : TArray<String>;
  res : TIETFLang;
  c, i, t : integer;
begin
  if (code = '') then
    exit(nil);

  if (FParsed.TryGetValue(code, result)) then
    result.link
  else
  begin
    msg := '';
    res := TIETFLang.create(code);
    try
      if code <> '' then
      begin
        parts := code.Split(['-']);
        c := 0;
        t := length(parts);
        if not FLanguages.ContainsKey(parts[c]) and (parts[c] <> '*') then
          msg := 'Invalid Language code "'+parts[c]+'"'
        else
        begin
          res.FLanguage := parts[c];
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
            res.FScript := parts[c];
            inc(c);
          end;
          if (c < t) and FRegions.ContainsKey(parts[c]) then
          begin
            res.FRegion := parts[c];
            inc(c);
          end;
          if (c < t) and FVariants.ContainsKey(parts[c]) then
          begin
            res.FVariant := parts[c];
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
      end;
      if msg = '' then
      begin
        result := res.Link;
        FParsed.AddOrSetValue(code, result.link);
      end
      else
        result := nil;
    finally
      res.free;
    end;
  end;
end;


function TIETFLanguageDefinitions.parse(code: String): TIETFLang;
var
  m : String;
begin
  result := parse(code, m);
end;

function TIETFLanguageDefinitions.present(code: TIETFLang; i : integer; template: String): String;
begin
  result := template.Replace('{{lang}}', FLanguages[code.language].displays[i]).Replace('{{region}}', FRegions[code.region].displays[0]);
end;

function TIETFLanguageDefinitions.displayCount(code: TIETFLang): integer;
begin
  result := FLanguages[code.language].displays.Count;
end;

function TIETFLanguageDefinitions.present(code: TIETFLang; i : integer = 0): String;
var
  b : TFslStringBuilder;
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
  if (code = nil) then
    result := ''
  else
  begin
    b := TFslStringBuilder.Create;
    try
      b.append(FLanguages[code.language].displays[i]);
      if (code.region <> '') or (code.script <> '') or (code.variant <> '') then
      begin
        b.Append(' (');
        first := true;
        if (code.script <> '') then
          note('Script', FScripts[code.script].displays[0]);
        if (code.region <> '') then
          note('Region', FRegions[code.region].displays[0]);
        if (code.variant <> '') then
          note('Variant', FVariants[code.variant].displays[0]);
        b.Append(')');
      end;

      result := b.ToString;
    finally
      b.free;
    end;
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
      l := l.trim;
      r := r.trim;
      if not vars.ContainsKey(l) then
        vars.Add(l, r)
      else
        vars.Values[l] := vars.Values[l]+'|'+r;
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
    vars := TFslStringDictionary.Create;
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
           raise EFSLException.create('IETFLang: Unable to parse definitions expecting Type: found '+vars['Type']+' at line '+inttostr(i+1))
      end;
    finally
      vars.free;
    end;
    if i < st.count then
      raise EFSLException.create('IETFLang: Unable to parse definitions - premature end at line '+inttostr(i+1))
  finally
    st.free;
  end;
end;

function TIETFLanguageDefinitions.loadExtLang(vars: TFslStringDictionary; i: integer): integer;
var
  cc : TIETFLanguageExtLang;
  s : String;
begin
  cc := TIETFLanguageExtLang.Create;
  try
    cc.code := vars['Subtag'];
    for s in vars['Description'].split(['|']) do
      cc.displays.add(s);
    if FExtLanguages.ContainsKey(cc.code) then
      raise EFSLException.create('IETFLang: Unable to parse definitions expecting Type: duplicate extlang code '+cc.code+' at line '+inttostr(i+1));
    FExtLanguages.Add(cc.code, cc.Link);
    result := i;
  finally
    cc.free;
  end;
end;

function TIETFLanguageDefinitions.loadLanguage(vars : TFslStringDictionary; i: integer): integer;
var
  cc : TIETFLanguageLanguage;
  s : string;
begin
  cc := TIETFLanguageLanguage.Create;
  try
    cc.code := vars['Subtag'];
    for s in vars['Description'].split(['|']) do
      cc.displays.add(s);
    if (vars.ContainsKey('Suppress-Script')) then
      cc.sscript := vars['Suppress-Script'];
    if (vars.ContainsKey('Scope')) then
      cc.scope := vars['Scope'];
    if FLanguages.ContainsKey(cc.code) then
      raise EFSLException.create('IETFLang: Unable to parse definitions expecting Type: duplicate language code '+cc.code+' at line '+inttostr(i+1));
    FLanguages.Add(cc.code, cc.Link);
    result := i;
  finally
    cc.free;
  end;
end;

function TIETFLanguageDefinitions.loadRegion(vars: TFslStringDictionary; i: integer): integer;
var
  cc : TIETFLanguageRegion;
  s : String;
begin
  cc := TIETFLanguageRegion.Create;
  try
    cc.code := vars['Subtag'];
    for s in vars['Description'].split(['|']) do
      cc.displays.add(s);
    if FRegions.ContainsKey(cc.code) then
      raise EFSLException.create('IETFLang: Unable to parse definitions expecting Type: duplicate region code '+cc.code+' at line '+inttostr(i+1));
    FRegions.Add(cc.code, cc.Link);
    result := i;
  finally
    cc.free;
  end;
end;

function TIETFLanguageDefinitions.loadScript(vars: TFslStringDictionary; i: integer): integer;
var
  cc : TIETFLanguageScript;
  s : String;
begin
  cc := TIETFLanguageScript.Create;
  try
    cc.code := vars['Subtag'];
    for s in vars['Description'].split(['|']) do
      cc.displays.add(s);
    if FScripts.ContainsKey(cc.code) then
      raise EFSLException.create('IETFLang: Unable to parse definitions expecting Type: duplicate script code '+cc.code+' at line '+inttostr(i+1));
    FScripts.Add(cc.code, cc.Link);
    result := i;
  finally
    cc.free;
  end;
end;

function TIETFLanguageDefinitions.loadVariant(vars: TFslStringDictionary; i: integer): integer;
var
  cc : TIETFLanguageVariant;
  s : String;
begin
  cc := TIETFLanguageVariant.Create;
  try
    cc.code := vars['Subtag'];
    for s in vars['Description'].split(['|']) do
      cc.displays.add(s);
    if FVariants.ContainsKey(cc.code) then
      raise EFSLException.create('IETFLang: Unable to parse definitions expecting Type: duplicate region code '+cc.code+' at line '+inttostr(i+1));
    FVariants.Add(cc.code, cc.Link);
    result := i;
  finally
    cc.free;
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

end.
