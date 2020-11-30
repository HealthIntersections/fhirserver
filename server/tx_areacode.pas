unit tx_areacode;

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
  fsl_utilities, fsl_http,
  fsl_base, fsl_stream, 
  fhir_common,
  ftx_service;

type
  TAreaCodeConcept = class (TCodeSystemProviderContext)
  private
    FDisplay: String;
    FCode: String;
    FClass: String;
  public
    function link : TAreaCodeConcept; overload;

    property code : String read FCode write FCode;
    property display : String read FDisplay write FDisplay;
    property class_ : String read FClass write FClass;
  end;

  TAreaCodeConceptFilter = class (TCodeSystemProviderFilterContext)
  private
    FList : TFslList<TAreaCodeConcept>;
    FCursor : integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TAreaCodeConceptFilter; overload;
  end;

  TAreaCodeServices = class (TCodeSystemProvider)
  private
    FCodes : TFslList<TAreaCodeConcept>;
    FMap : TFslMap<TAreaCodeConcept>;

    procedure load;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TAreaCodeServices; overload;

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

{ TAreaCodeServices }

Constructor TAreaCodeServices.create();
begin
  inherited Create;
  FCodes := TFslList<TAreaCodeConcept>.create;
  FMap := TFslMap<TAreaCodeConcept>.create('tx.areacode');
  Load;
end;


function TAreaCodeServices.TotalCount : integer;
begin
  result := FCodes.Count;
end;


function TAreaCodeServices.systemUri(context : TCodeSystemProviderContext) : String;
begin
  result := 'http://unstats.un.org/unsd/methods/m49/m49.htm';
end;

function TAreaCodeServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TAreaCodeServices.getDisplay(code : String; const lang : THTTPLanguages):String;
begin
  result := FMap[code].display;
end;

function TAreaCodeServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

procedure TAreaCodeServices.Displays(code : String; list : TStringList; const lang : THTTPLanguages);
begin
  list.Add(getDisplay(code, lang));
end;


procedure TAreaCodeServices.load;
  procedure doLoad(code, display, abbrev, class_ : String);
  var
    c : TAreaCodeConcept;
  begin
    c := TAreaCodeConcept.Create;
    try
      c.code := code;
      if abbrev <> '' then
        c.display := display+' ('+abbrev+')'
      else
        c.display := display;
      c.class_ := class_;
      FCodes.Add(c.Link);
      FMap.Add(code, c.Link);
    finally
      c.Free;
    end;
  end;
begin
  doload('004', 'Afghanistan', 'AFG', 'country');
  doload('248', 'Eland Islands', 'ALA', 'country');
  doload('008', 'Albania', 'ALB', 'country');
  doload('012', 'Algeria', 'DZA', 'country');
  doload('016', 'American Samoa', 'ASM', 'country');
  doload('020', 'Andorra', 'AND', 'country');
  doload('024', 'Angola', 'AGO', 'country');
  doload('660', 'Anguilla', 'AIA', 'country');
  doload('028', 'Antigua and Barbuda', 'ATG', 'country');
  doload('032', 'Argentina', 'ARG', 'country');
  doload('051', 'Armenia', 'ARM', 'country');
  doload('533', 'Aruba', 'ABW', 'country');
  doload('036', 'Australia', 'AUS', 'country');
  doload('040', 'Austria', 'AUT', 'country');
  doload('031', 'Azerbaijan', 'AZE', 'country');
  doload('044', 'Bahamas', 'BHS', 'country');
  doload('048', 'Bahrain', 'BHR', 'country');
  doload('050', 'Bangladesh', 'BGD', 'country');
  doload('052', 'Barbados', 'BRB', 'country');
  doload('112', 'Belarus', 'BLR', 'country');
  doload('056', 'Belgium', 'BEL', 'country');
  doload('084', 'Belize', 'BLZ', 'country');
  doload('204', 'Benin', 'BEN', 'country');
  doload('060', 'Bermuda', 'BMU', 'country');
  doload('064', 'Bhutan', 'BTN', 'country');
  doload('068', 'Bolivia (Plurinational State of)', 'BOL', 'country');
  doload('535', 'Bonaire, Sint Eustatius and Saba', 'BES', 'country');
  doload('070', 'Bosnia and Herzegovina', 'BIH', 'country');
  doload('072', 'Botswana', 'BWA', 'country');
  doload('076', 'Brazil', 'BRA', 'country');
  doload('092', 'British Virgin Islands', 'VGB', 'country');
  doload('096', 'Brunei Darussalam', 'BRN', 'country');
  doload('100', 'Bulgaria', 'BGR', 'country');
  doload('854', 'Burkina Faso', 'BFA', 'country');
  doload('108', 'Burundi', 'BDI', 'country');
  doload('132', 'Cabo Verde', 'CPV', 'country');
  doload('116', 'Cambodia', 'KHM', 'country');
  doload('120', 'Cameroon', 'CMR', 'country');
  doload('124', 'Canada', 'CAN', 'country');
  doload('136', 'Cayman Islands', 'CYM', 'country');
  doload('140', 'Central African Republic', 'CAF', 'country');
  doload('148', 'Chad', 'TCD', 'country');
  doload('830', 'Channel Islands  ', '', 'country');
  doload('152', 'Chile', 'CHL', 'country');
  doload('156', 'China', 'CHN', 'country');
  doload('344', 'China, Hong Kong Special Administrative Region', 'HKG', 'country');
  doload('446', 'China, Macao Special Administrative Region', 'MAC', 'country');
  doload('170', 'Colombia', 'COL', 'country');
  doload('174', 'Comoros', 'COM', 'country');
  doload('178', 'Congo', 'COG', 'country');
  doload('184', 'Cook Islands', 'COK', 'country');
  doload('188', 'Costa Rica', 'CRI', 'country');
  doload('384', 'Ctte d''Ivoire', 'CIV', 'country');
  doload('191', 'Croatia', 'HRV', 'country');
  doload('192', 'Cuba', 'CUB', 'country');
  doload('531', 'Curagao', 'CUW', 'country');
  doload('196', 'Cyprus', 'CYP', 'country');
  doload('203', 'Czech Republic', 'CZE', 'country');
  doload('408', 'Democratic People''s Republic of Korea', 'PRK', 'country');
  doload('180', 'Democratic Republic of the Congo', 'COD', 'country');
  doload('208', 'Denmark', 'DNK', 'country');
  doload('262', 'Djibouti', 'DJI', 'country');
  doload('212', 'Dominica', 'DMA', 'country');
  doload('214', 'Dominican Republic', 'DOM', 'country');
  doload('218', 'Ecuador', 'ECU', 'country');
  doload('818', 'Egypt', 'EGY', 'country');
  doload('222', 'El Salvador', 'SLV', 'country');
  doload('226', 'Equatorial Guinea', 'GNQ', 'country');
  doload('232', 'Eritrea', 'ERI', 'country');
  doload('233', 'Estonia', 'EST', 'country');
  doload('231', 'Ethiopia', 'ETH', 'country');
  doload('234', 'Faeroe Islands', 'FRO', 'country');
  doload('238', 'Falkland Islands (Malvinas)', 'FLK', 'country');
  doload('242', 'Fiji', 'FJI', 'country');
  doload('246', 'Finland', 'FIN', 'country');
  doload('250', 'France', 'FRA', 'country');
  doload('254', 'French Guiana', 'GUF', 'country');
  doload('258', 'French Polynesia', 'PYF', 'country');
  doload('266', 'Gabon', 'GAB', 'country');
  doload('270', 'Gambia', 'GMB', 'country');
  doload('268', 'Georgia', 'GEO', 'country');
  doload('276', 'Germany', 'DEU', 'country');
  doload('288', 'Ghana', 'GHA', 'country');
  doload('292', 'Gibraltar', 'GIB', 'country');
  doload('300', 'Greece', 'GRC', 'country');
  doload('304', 'Greenland', 'GRL', 'country');
  doload('308', 'Grenada', 'GRD', 'country');
  doload('312', 'Guadeloupe', 'GLP', 'country');
  doload('316', 'Guam', 'GUM', 'country');
  doload('320', 'Guatemala', 'GTM', 'country');
  doload('831', 'Guernsey', 'GGY', 'country');
  doload('324', 'Guinea', 'GIN', 'country');
  doload('624', 'Guinea-Bissau', 'GNB', 'country');
  doload('328', 'Guyana', 'GUY', 'country');
  doload('332', 'Haiti', 'HTI', 'country');
  doload('336', 'Holy See', 'VAT', 'country');
  doload('340', 'Honduras', 'HND', 'country');
  doload('348', 'Hungary', 'HUN', 'country');
  doload('352', 'Iceland', 'ISL', 'country');
  doload('356', 'India', 'IND', 'country');
  doload('360', 'Indonesia', 'IDN', 'country');
  doload('364', 'Iran (Islamic Republic of)', 'IRN', 'country');
  doload('368', 'Iraq', 'IRQ', 'country');
  doload('372', 'Ireland', 'IRL', 'country');
  doload('833', 'Isle of Man', 'IMN', 'country');
  doload('376', 'Israel', 'ISR', 'country');
  doload('380', 'Italy', 'ITA', 'country');
  doload('388', 'Jamaica', 'JAM', 'country');
  doload('392', 'Japan', 'JPN', 'country');
  doload('832', 'Jersey  JEY', '', 'country');
  doload('400', 'Jordan', 'JOR', 'country');
  doload('398', 'Kazakhstan', 'KAZ', 'country');
  doload('404', 'Kenya', 'KEN', 'country');
  doload('296', 'Kiribati', 'KIR', 'country');
  doload('414', 'Kuwait', 'KWT', 'country');
  doload('417', 'Kyrgyzstan', 'KGZ', 'country');
  doload('418', 'Lao People''s Democratic Republic', 'LAO', 'country');
  doload('428', 'Latvia', 'LVA', 'country');
  doload('422', 'Lebanon', 'LBN', 'country');
  doload('426', 'Lesotho', 'LSO', 'country');
  doload('430', 'Liberia', 'LBR', 'country');
  doload('434', 'Libya', 'LBY', 'country');
  doload('438', 'Liechtenstein', 'LIE', 'country');
  doload('440', 'Lithuania', 'LTU', 'country');
  doload('442', 'Luxembourg', 'LUX', 'country');
  doload('450', 'Madagascar', 'MDG', 'country');
  doload('454', 'Malawi', 'MWI', 'country');
  doload('458', 'Malaysia', 'MYS', 'country');
  doload('462', 'Maldives', 'MDV', 'country');
  doload('466', 'Mali', 'MLI', 'country');
  doload('470', 'Malta', 'MLT', 'country');
  doload('584', 'Marshall Islands', 'MHL', 'country');
  doload('474', 'Martinique', 'MTQ', 'country');
  doload('478', 'Mauritania', 'MRT', 'country');
  doload('480', 'Mauritius', 'MUS', 'country');
  doload('175', 'Mayotte  MYT', '', 'country');
  doload('484', 'Mexico', 'MEX', 'country');
  doload('583', 'Micronesia (Federated States of)', 'FSM', 'country');
  doload('492', 'Monaco', 'MCO', 'country');
  doload('496', 'Mongolia', 'MNG', 'country');
  doload('499', 'Montenegro', 'MNE', 'country');
  doload('500', 'Montserrat', 'MSR', 'country');
  doload('504', 'Morocco', 'MAR', 'country');
  doload('508', 'Mozambique', 'MOZ', 'country');
  doload('104', 'Myanmar', 'MMR', 'country');
  doload('516', 'Namibia', 'NAM', 'country');
  doload('520', 'Nauru', 'NRU', 'country');
  doload('524', 'Nepal', 'NPL', 'country');
  doload('528', 'Netherlands', 'NLD', 'country');
  doload('540', 'New Caledonia', 'NCL', 'country');
  doload('554', 'New Zealand', 'NZL', 'country');
  doload('558', 'Nicaragua', 'NIC', 'country');
  doload('562', 'Niger', 'NER', 'country');
  doload('566', 'Nigeria', 'NGA', 'country');
  doload('570', 'Niue', 'NIU', 'country');
  doload('574', 'Norfolk Island', 'NFK', 'country');
  doload('580', 'Northern Mariana Islands', 'MNP', 'country');
  doload('578', 'Norway', 'NOR', 'country');
  doload('512', 'Oman', 'OMN', 'country');
  doload('586', 'Pakistan', 'PAK', 'country');
  doload('585', 'Palau', 'PLW', 'country');
  doload('591', 'Panama', 'PAN', 'country');
  doload('598', 'Papua New Guinea', 'PNG', 'country');
  doload('600', 'Paraguay', 'PRY', 'country');
  doload('604', 'Peru', 'PER', 'country');
  doload('608', 'Philippines', 'PHL', 'country');
  doload('612', 'Pitcairn', 'PCN', 'country');
  doload('616', 'Poland', 'POL', 'country');
  doload('620', 'Portugal', 'PRT', 'country');
  doload('630', 'Puerto Rico', 'PRI', 'country');
  doload('634', 'Qatar', 'QAT', 'country');
  doload('410', 'Republic of Korea', 'KOR', 'country');
  doload('498', 'Republic of Moldova', 'MDA', 'country');
  doload('638', 'Riunion', 'REU', 'country');
  doload('642', 'Romania', 'ROU', 'country');
  doload('643', 'Russian Federation', 'RUS', 'country');
  doload('646', 'Rwanda', 'RWA', 'country');
  doload('652', 'Saint Barthilemy', 'BLM', 'country');
  doload('654', 'Saint Helena', 'SHN', 'country');
  doload('659', 'Saint Kitts and Nevis', 'KNA', 'country');
  doload('662', 'Saint Lucia', 'LCA', 'country');
  doload('663', 'Saint Martin (French part)  MAF', '', 'country');
  doload('666', 'Saint Pierre and Miquelon', 'SPM', 'country');
  doload('670', 'Saint Vincent and the Grenadines', 'VCT', 'country');
  doload('882', 'Samoa', 'WSM', 'country');
  doload('674', 'San Marino', 'SMR', 'country');
  doload('678', 'Sao Tome and Principe', 'STP', 'country');
  doload('680', 'Sark', ' ', 'country');
  doload('682', 'Saudi Arabia', 'SAU', 'country');
  doload('686', 'Senegal', 'SEN', 'country');
  doload('688', 'Serbia', 'SRB', 'country');
  doload('690', 'Seychelles', 'SYC', 'country');
  doload('694', 'Sierra Leone', 'SLE', 'country');
  doload('702', 'Singapore', 'SGP', 'country');
  doload('534', 'Sint Maarten (Dutch part)  SXM', '', 'country');
  doload('703', 'Slovakia', 'SVK', 'country');
  doload('705', 'Slovenia', 'SVN', 'country');
  doload('090', 'Solomon Islands', 'SLB', 'country');
  doload('706', 'Somalia', 'SOM', 'country');
  doload('710', 'South Africa', 'ZAF', 'country');
  doload('728', 'South Sudan', 'SSD', 'country');
  doload('724', 'Spain', 'ESP', 'country');
  doload('144', 'Sri Lanka', 'LKA', 'country');
  doload('275', 'State of Palestine', 'PSE', 'country');
  doload('729', 'Sudan', 'SDN', 'country');
  doload('740', 'Suriname', 'SUR', 'country');
  doload('744', 'Svalbard and Jan Mayen Islands', 'SJM', 'country');
  doload('748', 'Swaziland', 'SWZ', 'country');
  doload('752', 'Sweden', 'SWE', 'country');
  doload('756', 'Switzerland', 'CHE', 'country');
  doload('760', 'Syrian Arab Republic', 'SYR', 'country');
  doload('762', 'Tajikistan', 'TJK', 'country');
  doload('764', 'Thailand', 'THA', 'country');
  doload('807', 'The former Yugoslav Republic of Macedonia', 'MKD', 'country');
  doload('626', 'Timor-Leste', 'TLS', 'country');
  doload('768', 'Togo', 'TGO', 'country');
  doload('772', 'Tokelau', 'TKL', 'country');
  doload('776', 'Tonga', 'TON', 'country');
  doload('780', 'Trinidad and Tobago', 'TTO', 'country');
  doload('788', 'Tunisia', 'TUN', 'country');
  doload('792', 'Turkey', 'TUR', 'country');
  doload('795', 'Turkmenistan', 'TKM', 'country');
  doload('796', 'Turks and Caicos Islands', 'TCA', 'country');
  doload('798', 'Tuvalu', 'TUV', 'country');
  doload('800', 'Uganda', 'UGA', 'country');
  doload('804', 'Ukraine', 'UKR', 'country');
  doload('784', 'United Arab Emirates', 'ARE', 'country');
  doload('826', 'United Kingdom of Great Britain and Northern Ireland', 'GBR', 'country');
  doload('834', 'United Republic of Tanzania', 'TZA', 'country');
  doload('840', 'United States of America', 'USA', 'country');
  doload('850', 'United States Virgin Islands', 'VIR', 'country');
  doload('858', 'Uruguay', 'URY', 'country');
  doload('860', 'Uzbekistan', 'UZB', 'country');
  doload('548', 'Vanuatu', 'VUT', 'country');
  doload('862', 'Venezuela (Bolivarian Republic of)', 'VEN', 'country');
  doload('704', 'Viet Nam', 'VNM', 'country');
  doload('876', 'Wallis and Futuna Islands', 'WLF', 'country');
  doload('732', 'Western Sahara', 'ESH', 'country');
  doload('887', 'Yemen', 'YEM', 'country');
  doload('894', 'Zambia', 'ZMB', 'country');
  doload('716', 'Zimbabwe', 'ZWE', 'country');
  doload('001', 'World', '', 'region');
  doload('002', 'Africa', '', 'region');
  doload('014', 'Eastern Africa', '', 'region');
  doload('017', 'Middle Africa', '', 'region');
  doload('015', 'Northern Africa', '', 'region');
  doload('018', 'Southern Africa', '', 'region');
  doload('011', 'Western Africa', '', 'region');
  doload('019', 'Americas', '', 'region');
  doload('419', 'Latin America and the Caribbean', '', 'region');
  doload('029', 'Caribbean', '', 'region');
  doload('013', 'Central America', '', 'region');
  doload('005', 'South America', '', 'region');
  doload('021', 'Northern America a/', '', 'region');
  doload('142', 'Asia', '', 'region');
  doload('143', 'Central Asia', '', 'region');
  doload('030', 'Eastern Asia', '', 'region');
  doload('034', 'Southern Asia', '', 'region');
  doload('035', 'South-Eastern Asia', '', 'region');
  doload('145', 'Western Asia', '', 'region');
  doload('150', 'Europe', '', 'region');
  doload('151', 'Eastern Europe', '', 'region');
  doload('154', 'Northern Europe', '', 'region');
  doload('039', 'Southern Europe', '', 'region');
  doload('155', 'Western Europe', '', 'region');
  doload('009', 'Oceania', '', 'region');
  doload('053', 'Australia and New Zealand', '', 'region');
  doload('054', 'Melanesia', '', 'region');
  doload('057', 'Micronesia', '', 'region');
  doload('061', 'Polynesia', '', 'region');
end;

function TAreaCodeServices.locate(code : String; var message : String) : TCodeSystemProviderContext;
begin
  result := FMap[code];
end;


function TAreaCodeServices.Code(context : TCodeSystemProviderContext) : string;
begin
  result := TAreaCodeConcept(context).code;
end;

function TAreaCodeServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TAreaCodeServices.description: String;
begin
  result := 'International area/region Codes';
end;

destructor TAreaCodeServices.Destroy;
begin
  FMap.free;
  FCodes.Free;
  inherited;
end;

function TAreaCodeServices.Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string;
begin
  result := TAreaCodeConcept(context).display;
end;

procedure TAreaCodeServices.Displays(context: TCodeSystemProviderContext; list: TStringList; const lang : THTTPLanguages);
begin
  list.Add(Display(context, lang));
end;

function TAreaCodeServices.IsAbstract(context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // AreaCode doesn't do abstract
end;

function TAreaCodeServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TAreaCodeServices.Link: TAreaCodeServices;
begin
  result := TAreaCodeServices(Inherited Link);
end;

function TAreaCodeServices.ChildCount(context : TCodeSystemProviderContext) : integer;
begin
  if (context = nil) then
    result := TotalCount
  else
    result := 0; // no children
end;

function TAreaCodeServices.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  result := FCodes[ndx];
end;

function TAreaCodeServices.locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('locateIsA not supported by AreaCode'); // AreaCode doesn't have formal subsumption property, so this is not used
end;


function TAreaCodeServices.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  // nothing
  result := true;
end;

function TAreaCodeServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.create('TAreaCodeServices.searchFilter');
end;

function TAreaCodeServices.subsumesTest(codeA, codeB: String): String;
begin
  result := 'not-subsumed';
end;

function TAreaCodeServices.filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
var
  res : TAreaCodeConceptFilter;
  c : TAreaCodeConcept;
begin
  if ((prop = 'type') or (prop = 'class')) and (op = foEqual) then
  begin
    res := TAreaCodeConceptFilter.create;
    try
      for c in FCodes do
        if c.class_ = value then
          res.flist.Add(c.link);
        result := res.link;
    finally
      res.Free;
    end;
  end
  else
    raise ETerminologyError.create('the filter '+prop+' '+CODES_TFhirFilterOperator[op]+' = '+value+' is not support for '+systemUri(nil));
end;

function TAreaCodeServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
var
  filter : TAreaCodeConceptFilter;
  c : TAreaCodeConcept;
begin
  filter := ctxt as TAreaCodeConceptFilter;
  result := nil;
  for c in filter.FList do
    if c.code = code then
      exit(c.link);
end;

function TAreaCodeServices.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  TAreaCodeConceptFilter(ctxt).FCursor := TAreaCodeConceptFilter(ctxt).FCursor + 1;
  result := TAreaCodeConceptFilter(ctxt).FCursor < TAreaCodeConceptFilter(ctxt).FList.Count;
end;

function TAreaCodeServices.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  result := TAreaCodeConceptFilter(ctxt).FList[TAreaCodeConceptFilter(ctxt).FCursor];
end;

function TAreaCodeServices.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyTodo.create('TAreaCodeServices.InFilter');
end;

procedure TAreaCodeServices.Close(ctxt: TCodeSystemProviderContext);
begin
//  ctxt.free;
end;

procedure TAreaCodeServices.Close(ctxt : TCodeSystemProviderFilterContext);
begin
  ctxt.free;
end;

procedure TAreaCodeServices.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  // nothing
end;

{ TAreaCodeConcept }

function TAreaCodeConcept.link: TAreaCodeConcept;
begin
  result := TAreaCodeConcept(inherited Link);
end;

{ TAreaCodeConceptFilter }

constructor TAreaCodeConceptFilter.Create;
begin
  inherited;
  FList := TFslList<TAreaCodeConcept>.Create;
  FCursor := -1;
end;

destructor TAreaCodeConceptFilter.Destroy;
begin
  FList.Free;
  inherited;
end;

function TAreaCodeConceptFilter.link: TAreaCodeConceptFilter;
begin
  result := TAreaCodeConceptFilter(inherited Link);
end;

end.

