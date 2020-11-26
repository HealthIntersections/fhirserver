unit tx_acir;

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

// based on a table by importing the excel spreadsheet directly
uses
  SysUtils, Classes,
  fsl_utilities, fsl_base, fsl_http,
  ftx_service;

type
  TACIRConcept = class (TCodeSystemProviderContext)
  private
    FCode : string;
    FDisplay : String;
    FDefinition : String;
  public
    constructor Create(code, display, definition, diesases : String);

    function link : TACIRConcept; overload;
    property code : String read FCode;
    property display : String read FDisplay;
    property definition : String read FDefinition;
  end;

  TACIRFilter = class (TCodeSystemProviderFilterContext)
  private
  public
    destructor Destroy; Override;
  end;

  TACIRPrep = class (TCodeSystemProviderFilterPreparationContext)
  public
    constructor Create; Override;
    destructor Destroy; Override;
  end;

  TACIRServices = class (TCodeSystemProvider)
  private
    FList : TFslList<TACIRConcept>;
    FMap : TFslMap<TACIRConcept>;

    procedure load;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TACIRServices; overload;

    function description : String; override;
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

    function subsumesTest(codeA, codeB : String) : String; override;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
  end;

implementation

{ TACIRServices }

Constructor TACIRServices.create();
begin
  inherited Create;
  FList := TFslList<TACIRConcept>.create;
  FMap := TFslMap<TACIRConcept>.create('tx.acir');

  Load;
end;


function TACIRServices.TotalCount : integer;
begin
  result := FList.count;
end;


function TACIRServices.version(context: TCodeSystemProviderContext): String;
begin
  result := '';
end;

function TACIRServices.systemUri(context : TCodeSystemProviderContext) : String;
begin
  result := 'urn:oid:1.2.36.1.2001.1005.17';
end;

function TACIRServices.getDefinition(code: String): String;
begin
  result := FMap[code].definition;
end;

function TACIRServices.getDisplay(code : String; const lang : THTTPLanguages):String;
begin
  result := FMap[code].display.Trim;
end;

function TACIRServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

procedure TACIRServices.Displays(code : String; list : TStringList; const lang : THTTPLanguages);
begin
  list.Add(getDisplay(code, lang));
end;

procedure TACIRServices.load;
var
  c : TACIRConcept;
begin
  FList.add(TACIRConcept.create('AGRPAL', 'Agrippal', 'Influenza', 'Influenza'));
  FList.add(TACIRConcept.create('AVAXM', 'Avaxim', 'Hepatitis A', 'Hepatitis A'));
  FList.add(TACIRConcept.create('BCG', 'BCG', 'Tuberculosis', 'Tuberculosis'));
  FList.add(TACIRConcept.create('CDT', 'CDT', 'Diphtheria, tetanus', 'Diphtheria, Tetanus'));
  FList.add(TACIRConcept.create('CMX', 'COMVAX', 'Hib, hepatitis B', 'Hib Hepatitis B'));
  FList.add(TACIRConcept.create('DTP', 'Triple Antigen', 'Triple Antigen', 'Diphtheria, Tetanus, Pertussis'));
  FList.add(TACIRConcept.create('DTPA', 'DTPa', 'DTPa', 'Diphtheria, Tetanus, Pertussis'));
  FList.add(TACIRConcept.create('ENGP', 'Engerix B', 'B Hepatitis B (paediatric)', 'Hepatitis B'));
  FList.add(TACIRConcept.create('FLRIX', 'Fluarix', 'Influenza', 'Influenza'));
  FList.add(TACIRConcept.create('FLUVAX', 'Fluvax', 'Influenza', 'Influenza'));
  FList.add(TACIRConcept.create('FLVRN', 'Fluvirin', 'Influenza', 'Influenza'));
  FList.add(TACIRConcept.create('FVXJNR', 'Fluvax Junior', 'Junior Influenza', 'Influenza'));
  FList.add(TACIRConcept.create('GNDIP', 'Diphtheria', 'Diphtheria', 'Diphtheria'));
  FList.add(TACIRConcept.create('GNFLU', 'Influenza', 'Influenza', 'Influenza'));
  FList.add(TACIRConcept.create('GNHEP', 'Hepatitis B', 'Hepatitis B', 'Hepatitis B'));
  FList.add(TACIRConcept.create('GNHIB', 'HIB', 'HIB', 'Hib'));
  FList.add(TACIRConcept.create('GNHPA', 'Hepatitis A', 'Hepatitis A', 'Hepatitis A'));
  FList.add(TACIRConcept.create('GNJEN', 'Japanese Encephalitis', 'Japanese Encephalitis', 'Japanese Encephalitis'));
  FList.add(TACIRConcept.create('GNMEA', 'Measles', 'Measles', 'Measles'));
  FList.add(TACIRConcept.create('GNMEN', 'Meningococcal C', 'Meningococcal C', 'Meningococcal C'));
  FList.add(TACIRConcept.create('GNMUM', 'Mumps', 'Mumps', 'Mumps'));
  FList.add(TACIRConcept.create('GNPNE', 'Pneumococcal', 'Pneumococcal', 'Pneumococcal'));
  FList.add(TACIRConcept.create('GNPOL', 'Polio', 'Polio', 'Polio'));
  FList.add(TACIRConcept.create('GNROX', 'Rotavirus', 'Rotavirus', 'Rotavirus'));
  FList.add(TACIRConcept.create('GNRUB', 'Rubella', 'Rubella', 'Rubella'));
  FList.add(TACIRConcept.create('GNTET', 'Tetanus', 'Tetanus', 'Tetanus'));
  FList.add(TACIRConcept.create('GNVAR', 'Varicella', 'Varicella', 'Varicella'));
  FList.add(TACIRConcept.create('HATWNJ', 'Twinrix Junior', 'Junior Hepatitis A &amp; B', 'Hepatitis A'));
  FList.add(TACIRConcept.create('HAVAQ', 'Vaqta Paed/Adol', 'Paediatric/Adolescent formulation Hepatitis A', 'Hepatitis A'));
  FList.add(TACIRConcept.create('HAVJ', 'Havrix Junior', 'Junior Hepatitis A', 'Hepatitis A'));
  FList.add(TACIRConcept.create('HBOC', 'HibTITER', 'Hib', 'Hib'));
  FList.add(TACIRConcept.create('HBV', 'HBV', 'HBV', 'Hepatitis B'));
  FList.add(TACIRConcept.create('HBVP', 'HBVAX II', 'II Hepatitis B (paediatric)', 'Hepatitis B'));
  FList.add(TACIRConcept.create('HBX', 'Hiberix', 'Hib', 'Hib'));
  FList.add(TACIRConcept.create('IFHX', 'Infanrix Hexa', 'Hexa Diphtheria, tetanus, pertussis, inactivated polio, Hib, hepatitis B', 'Diphtheria, Tetanus, Pertussis, Hepatitis B, Polio, Hib'));
  FList.add(TACIRConcept.create('IFIP', 'Infanrix-IPV', 'IPV Diphtheria, tetanus, pertussis, inactivated polio', 'Diphtheria, Tetanus, Pertussis, Polio'));
  FList.add(TACIRConcept.create('IFPA', 'Infanrix Penta', 'Penta Diphtheria, tetanus, pertussis, hepatitis B, inactivated polio', 'Diphtheria, Tetanus, Pertussis, Hepatitis B, Polio'));
  FList.add(TACIRConcept.create('IFX', 'Infanrix', 'Diphtheria, tetanus, pertussis', 'Diphtheria, Tetanus, Pertussis'));
  FList.add(TACIRConcept.create('IFXB', 'InfanrixHepB', '-Hep B Diphtheria, tetanus, pertussis, hepatitis B', 'Diphtheria, Tetanus, Pertussis, Hepatitis B'));
  FList.add(TACIRConcept.create('INFLUV', 'Influvac', 'Influenza', 'Influenza'));
  FList.add(TACIRConcept.create('IPV', 'IPOL', 'Inactivated polio (injection)', 'Polio'));
  FList.add(TACIRConcept.create('JEVAX', 'JE-VAX', '-VAX Japanese encephalitis', 'Japanese Encephalitis'));
  FList.add(TACIRConcept.create('MENJUG', 'Menjugate', 'Meningococcal C (conjugate)', 'Meningococcal C'));
  FList.add(TACIRConcept.create('MENTEC', 'Meningitec', 'Meningococcal C (conjugate)', 'Meningococcal C'));
  FList.add(TACIRConcept.create('MENUME', 'Menomune', 'Meningococcal A,C,W135 &amp; Y (polysaccharide)', 'Meningococcal'));
  FList.add(TACIRConcept.create('MENVAX', 'Mencevax ACWY', 'ACWY Meningococcal A,C,W135 &amp; Y (polysaccharide)', 'Meningococcal'));
  FList.add(TACIRConcept.create('MMR', 'MMR', 'MMR', 'Measles, Mumps, Rubella'));
  FList.add(TACIRConcept.create('MMRCSL', 'MMR II', 'II Measles, mumps, rubella', 'Measles, Mumps, Rubella'));
  FList.add(TACIRConcept.create('MMRSKB', 'Priorix', 'Measles, mumps, rubella', 'Measles, Mumps, Rubella'));
  FList.add(TACIRConcept.create('MNTRX', 'Menitorix', 'Meningococcal C (conjugate), Hib', 'Meningococcal C, Hib'));
  FList.add(TACIRConcept.create('NEISVC', 'NeisVac-C', '-C Meningococcal C (conjugate)', 'Meningococcal C'));
  FList.add(TACIRConcept.create('OPV', 'Polio', 'Sabin (Oral) Oral polio', 'Polio'));
  FList.add(TACIRConcept.create('P', 'Pertussis', 'Pertussis', 'Pertussis'));
  FList.add(TACIRConcept.create('PANVAX', 'Panvax', 'Influenza', 'Influenza'));
  FList.add(TACIRConcept.create('PDCL', 'Pediacel', 'Diphtheria, tetanus, pertussis, inactivated polio, Hib', 'Diphtheria, Tetanus, Pertussis, Polio, Hib'));
  FList.add(TACIRConcept.create('PLCL', 'Poliacel', 'Diphtheria, tetanus, pertussis, inactivated polio, Hib', 'Diphtheria, Tetanus, Pertussis, Polio, Hib'));
  FList.add(TACIRConcept.create('PNEUMO', 'Pneumovax', '23 Pneumococcal (23vPCV)', '23 Pneumococcal'));
  FList.add(TACIRConcept.create('PRPD', 'ProHIBit', 'ProHIBit', 'Hib'));
  FList.add(TACIRConcept.create('PROQAD', 'ProQuad', 'Measles, mumps, rubella, varicella', 'Measles, Mumps, Rubella, Varicella'));
  FList.add(TACIRConcept.create('PRPOMP', 'PedvaxHIB', 'Hib', 'Hib'));
  FList.add(TACIRConcept.create('PRPT', 'ActHIB', 'Hib', 'Hib'));
  FList.add(TACIRConcept.create('PRVNR', 'Prevenar 7', '7 Pneumococcal (7vPCV)', 'Pneumococcal'));
  FList.add(TACIRConcept.create('PRVTH', 'Prevenar 13', '13 Pneumococcal (13vPCV)', 'Pneumococcal'));
  FList.add(TACIRConcept.create('PRXTEX', 'Priorix-Tetra', '-Tetra Measles, mumps, rubella, varicella', 'Measles, Mumps, Rubella, Varicella'));
  FList.add(TACIRConcept.create('QDCL', 'Quadracel', 'Diphtheria, tetanus, pertussis, inactivated polio', 'Diphtheria, Tetanus, Pertussis, Polio'));
  FList.add(TACIRConcept.create('ROTRIX', 'Rotarix', 'Rotavirus', 'Rotavirus'));
  FList.add(TACIRConcept.create('ROTTEQ', 'Rotateq', 'Rotavirus', 'Rotavirus'));
  FList.add(TACIRConcept.create('SYNFLX', 'Synflorix', 'Pneumococcal (11vPCV)', 'Pneumococcal'));
  FList.add(TACIRConcept.create('TCL', 'Tripacel', 'Diphtheria, tetanus, pertussis', 'Diphtheria, Tetanus, Pertussis'));
  FList.add(TACIRConcept.create('VAXGRP', 'Vaxigrip', 'Influenza', 'Influenza'));
  FList.add(TACIRConcept.create('VGRJNR', 'Vaxigrip Junior', 'Junior Influenza', 'Influenza'));
  FList.add(TACIRConcept.create('VLRIX', 'Varilrix', 'Varicella Zoster', 'Varicella'));
  FList.add(TACIRConcept.create('VRVAX', 'Varivax', 'Varicella Zoster', 'Varicella'));

  for c in FList do
    FMap.AddOrSetValue(c.code, c.link);
end;

function TACIRServices.locate(code : String; var message : String) : TCodeSystemProviderContext;
begin
  result := FMap[code];
end;


function TACIRServices.Code(context : TCodeSystemProviderContext) : string;
begin
  result := TACIRConcept(context).FCode;
end;

function TACIRServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TACIRServices.description: String;
begin
  result := 'ACIR';
end;

destructor TACIRServices.Destroy;
begin
  FList.free;
  FMap.free;
  inherited;
end;

function TACIRServices.Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string;
begin
  result := TACIRConcept(context).Display.Trim;
end;

procedure TACIRServices.Displays(context: TCodeSystemProviderContext; list: TStringList; const lang : THTTPLanguages);
begin
  list.Add(Display(context, lang));
end;

function TACIRServices.IsAbstract(context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // ACIR doesn't do abstract
end;

function TACIRServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TACIRServices.Link: TACIRServices;
begin
  result := TACIRServices(Inherited Link);
end;

function TACIRServices.ChildCount(context : TCodeSystemProviderContext) : integer;
begin
  if context = nil then
    result := totalcount
  else
    result := 0; // no children
end;

function TACIRServices.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  result := FList[ndx];
end;

function TACIRServices.locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  raise ETerminologySetup.Create('locateIsA not supported by ACIR'); // ACIR doesn't have formal subsumption property, so this is not used
end;


function TACIRServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'ACIR';
end;

function TACIRServices.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  raise ETerminologyTodo.create('TACIRServices.prepare');
end;

function TACIRServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.create('TACIRServices.searchFilter');
end;

function TACIRServices.subsumesTest(codeA, codeB: String): String;
begin
  result := 'not-subsumed';
end;

function TACIRServices.filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.create('TACIRServices.filter');
end;

function TACIRServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.create('TACIRServices.filterLocate');
end;

function TACIRServices.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  raise ETerminologyTodo.create('TACIRServices.FilterMore');
end;

function TACIRServices.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.create('TACIRServices.FilterConcept');
end;

function TACIRServices.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyTodo.create('TACIRServices.InFilter');
end;

procedure TACIRServices.Close(ctxt: TCodeSystemProviderContext);
begin
end;

procedure TACIRServices.Close(ctxt : TCodeSystemProviderFilterContext);
begin
end;

procedure TACIRServices.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  raise ETerminologyTodo.create('TACIRServices.Close');
end;

{ TACIRPrep }

constructor TACIRPrep.Create;
begin
  inherited;
end;

destructor TACIRPrep.Destroy;
begin
  inherited;
end;

{ TACIRFilter }

destructor TACIRFilter.Destroy;
begin
  inherited;
end;


{ TACIRConcept }

constructor TACIRConcept.Create(code, display, definition, diesases: String);
begin
  inherited Create;
  FCode := code;
  FDisplay := display;
  FDefinition := definition;
end;

function TACIRConcept.link: TACIRConcept;
begin
  result := TACIRConcept(inherited Link);
end;

end.
