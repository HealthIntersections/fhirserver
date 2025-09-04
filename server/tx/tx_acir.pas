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
  fsl_utilities, fsl_base, fsl_http, fsl_lang, fsl_i18n,
  fhir_features,
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

  { TACIRServices }

  TACIRServices = class (TCodeSystemProvider)
  private
    FList : TFslList<TACIRConcept>;
    FMap : TFslMap<TACIRConcept>;

    procedure load;
  public
    constructor Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport);
    destructor Destroy; Override;
    Function Link : TACIRServices; overload;

    function description : String; override;
    function TotalCount : integer;  override;
    function getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    function systemUri : String; override;
    function version : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList):String; override;
    function getDefinition(opContext : TTxOperationContext; code : String):String; override;
    function locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(opContext : TTxOperationContext; code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function IsAbstract(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean; override;
    function Code(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;
    function Display(opContext : TTxOperationContext; context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string; override;
    procedure Designations(opContext : TTxOperationContext; context : TCodeSystemProviderContext; list : TConceptDesignations); override;
    function Definition(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;

    function getPrepContext(opContext : TTxOperationContext) : TCodeSystemProviderFilterPreparationContext; override;
    function prepare(opContext : TTxOperationContext; prep : TCodeSystemProviderFilterPreparationContext) : boolean; override;

    function searchFilter(opContext : TTxOperationContext; filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function FilterMore(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function filterSize(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : integer; override;
    function FilterConcept(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(opContext : TTxOperationContext; textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;

    function subsumesTest(opContext : TTxOperationContext; codeA, codeB : String) : String; override;

    procedure defineFeatures(opContext : TTxOperationContext; features : TFslList<TFHIRFeature>); override;
  end;

implementation

{ TACIRServices }

constructor TACIRServices.Create(languages: TIETFLanguageDefinitions; i18n : TI18nSupport);
begin
  inherited Create(languages, i18n);
  FList := TFslList<TACIRConcept>.Create;
  FMap := TFslMap<TACIRConcept>.Create('tx.acir');
  FMap.defaultValue := nil;

  Load;
end;


procedure TACIRServices.defineFeatures(opContext : TTxOperationContext; features: TFslList<TFHIRFeature>);
begin
end;

function TACIRServices.TotalCount : integer;
begin
  result := FList.count;
end;


function TACIRServices.version: String;
begin
  result := '';
end;

function TACIRServices.systemUri : String;
begin
  result := 'urn:oid:1.2.36.1.2001.1005.17';
end;

function TACIRServices.getDefinition(opContext : TTxOperationContext; code: String): String;
begin
  result := FMap[code].definition;
end;

function TACIRServices.getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList):String;
begin
  result := FMap[code].display.Trim;
end;

function TACIRServices.getPrepContext(opContext : TTxOperationContext): TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

procedure TACIRServices.load;
var
  c : TACIRConcept;
begin
  FList.add(TACIRConcept.Create('AGRPAL', 'Agrippal', 'Influenza', 'Influenza'));
  FList.add(TACIRConcept.Create('AVAXM', 'Avaxim', 'Hepatitis A', 'Hepatitis A'));
  FList.add(TACIRConcept.Create('BCG', 'BCG', 'Tuberculosis', 'Tuberculosis'));
  FList.add(TACIRConcept.Create('CDT', 'CDT', 'Diphtheria, tetanus', 'Diphtheria, Tetanus'));
  FList.add(TACIRConcept.Create('CMX', 'COMVAX', 'Hib, hepatitis B', 'Hib Hepatitis B'));
  FList.add(TACIRConcept.Create('DTP', 'Triple Antigen', 'Triple Antigen', 'Diphtheria, Tetanus, Pertussis'));
  FList.add(TACIRConcept.Create('DTPA', 'DTPa', 'DTPa', 'Diphtheria, Tetanus, Pertussis'));
  FList.add(TACIRConcept.Create('ENGP', 'Engerix B', 'B Hepatitis B (paediatric)', 'Hepatitis B'));
  FList.add(TACIRConcept.Create('FLRIX', 'Fluarix', 'Influenza', 'Influenza'));
  FList.add(TACIRConcept.Create('FLUVAX', 'Fluvax', 'Influenza', 'Influenza'));
  FList.add(TACIRConcept.Create('FLVRN', 'Fluvirin', 'Influenza', 'Influenza'));
  FList.add(TACIRConcept.Create('FVXJNR', 'Fluvax Junior', 'Junior Influenza', 'Influenza'));
  FList.add(TACIRConcept.Create('GNDIP', 'Diphtheria', 'Diphtheria', 'Diphtheria'));
  FList.add(TACIRConcept.Create('GNFLU', 'Influenza', 'Influenza', 'Influenza'));
  FList.add(TACIRConcept.Create('GNHEP', 'Hepatitis B', 'Hepatitis B', 'Hepatitis B'));
  FList.add(TACIRConcept.Create('GNHIB', 'HIB', 'HIB', 'Hib'));
  FList.add(TACIRConcept.Create('GNHPA', 'Hepatitis A', 'Hepatitis A', 'Hepatitis A'));
  FList.add(TACIRConcept.Create('GNJEN', 'Japanese Encephalitis', 'Japanese Encephalitis', 'Japanese Encephalitis'));
  FList.add(TACIRConcept.Create('GNMEA', 'Measles', 'Measles', 'Measles'));
  FList.add(TACIRConcept.Create('GNMEN', 'Meningococcal C', 'Meningococcal C', 'Meningococcal C'));
  FList.add(TACIRConcept.Create('GNMUM', 'Mumps', 'Mumps', 'Mumps'));
  FList.add(TACIRConcept.Create('GNPNE', 'Pneumococcal', 'Pneumococcal', 'Pneumococcal'));
  FList.add(TACIRConcept.Create('GNPOL', 'Polio', 'Polio', 'Polio'));
  FList.add(TACIRConcept.Create('GNROX', 'Rotavirus', 'Rotavirus', 'Rotavirus'));
  FList.add(TACIRConcept.Create('GNRUB', 'Rubella', 'Rubella', 'Rubella'));
  FList.add(TACIRConcept.Create('GNTET', 'Tetanus', 'Tetanus', 'Tetanus'));
  FList.add(TACIRConcept.Create('GNVAR', 'Varicella', 'Varicella', 'Varicella'));
  FList.add(TACIRConcept.Create('HATWNJ', 'Twinrix Junior', 'Junior Hepatitis A &amp; B', 'Hepatitis A'));
  FList.add(TACIRConcept.Create('HAVAQ', 'Vaqta Paed/Adol', 'Paediatric/Adolescent formulation Hepatitis A', 'Hepatitis A'));
  FList.add(TACIRConcept.Create('HAVJ', 'Havrix Junior', 'Junior Hepatitis A', 'Hepatitis A'));
  FList.add(TACIRConcept.Create('HBOC', 'HibTITER', 'Hib', 'Hib'));
  FList.add(TACIRConcept.Create('HBV', 'HBV', 'HBV', 'Hepatitis B'));
  FList.add(TACIRConcept.Create('HBVP', 'HBVAX II', 'II Hepatitis B (paediatric)', 'Hepatitis B'));
  FList.add(TACIRConcept.Create('HBX', 'Hiberix', 'Hib', 'Hib'));
  FList.add(TACIRConcept.Create('IFHX', 'Infanrix Hexa', 'Hexa Diphtheria, tetanus, pertussis, inactivated polio, Hib, hepatitis B', 'Diphtheria, Tetanus, Pertussis, Hepatitis B, Polio, Hib'));
  FList.add(TACIRConcept.Create('IFIP', 'Infanrix-IPV', 'IPV Diphtheria, tetanus, pertussis, inactivated polio', 'Diphtheria, Tetanus, Pertussis, Polio'));
  FList.add(TACIRConcept.Create('IFPA', 'Infanrix Penta', 'Penta Diphtheria, tetanus, pertussis, hepatitis B, inactivated polio', 'Diphtheria, Tetanus, Pertussis, Hepatitis B, Polio'));
  FList.add(TACIRConcept.Create('IFX', 'Infanrix', 'Diphtheria, tetanus, pertussis', 'Diphtheria, Tetanus, Pertussis'));
  FList.add(TACIRConcept.Create('IFXB', 'InfanrixHepB', '-Hep B Diphtheria, tetanus, pertussis, hepatitis B', 'Diphtheria, Tetanus, Pertussis, Hepatitis B'));
  FList.add(TACIRConcept.Create('INFLUV', 'Influvac', 'Influenza', 'Influenza'));
  FList.add(TACIRConcept.Create('IPV', 'IPOL', 'Inactivated polio (injection)', 'Polio'));
  FList.add(TACIRConcept.Create('JEVAX', 'JE-VAX', '-VAX Japanese encephalitis', 'Japanese Encephalitis'));
  FList.add(TACIRConcept.Create('MENJUG', 'Menjugate', 'Meningococcal C (conjugate)', 'Meningococcal C'));
  FList.add(TACIRConcept.Create('MENTEC', 'Meningitec', 'Meningococcal C (conjugate)', 'Meningococcal C'));
  FList.add(TACIRConcept.Create('MENUME', 'Menomune', 'Meningococcal A,C,W135 &amp; Y (polysaccharide)', 'Meningococcal'));
  FList.add(TACIRConcept.Create('MENVAX', 'Mencevax ACWY', 'ACWY Meningococcal A,C,W135 &amp; Y (polysaccharide)', 'Meningococcal'));
  FList.add(TACIRConcept.Create('MMR', 'MMR', 'MMR', 'Measles, Mumps, Rubella'));
  FList.add(TACIRConcept.Create('MMRCSL', 'MMR II', 'II Measles, mumps, rubella', 'Measles, Mumps, Rubella'));
  FList.add(TACIRConcept.Create('MMRSKB', 'Priorix', 'Measles, mumps, rubella', 'Measles, Mumps, Rubella'));
  FList.add(TACIRConcept.Create('MNTRX', 'Menitorix', 'Meningococcal C (conjugate), Hib', 'Meningococcal C, Hib'));
  FList.add(TACIRConcept.Create('NEISVC', 'NeisVac-C', '-C Meningococcal C (conjugate)', 'Meningococcal C'));
  FList.add(TACIRConcept.Create('OPV', 'Polio', 'Sabin (Oral) Oral polio', 'Polio'));
  FList.add(TACIRConcept.Create('P', 'Pertussis', 'Pertussis', 'Pertussis'));
  FList.add(TACIRConcept.Create('PANVAX', 'Panvax', 'Influenza', 'Influenza'));
  FList.add(TACIRConcept.Create('PDCL', 'Pediacel', 'Diphtheria, tetanus, pertussis, inactivated polio, Hib', 'Diphtheria, Tetanus, Pertussis, Polio, Hib'));
  FList.add(TACIRConcept.Create('PLCL', 'Poliacel', 'Diphtheria, tetanus, pertussis, inactivated polio, Hib', 'Diphtheria, Tetanus, Pertussis, Polio, Hib'));
  FList.add(TACIRConcept.Create('PNEUMO', 'Pneumovax', '23 Pneumococcal (23vPCV)', '23 Pneumococcal'));
  FList.add(TACIRConcept.Create('PRPD', 'ProHIBit', 'ProHIBit', 'Hib'));
  FList.add(TACIRConcept.Create('PROQAD', 'ProQuad', 'Measles, mumps, rubella, varicella', 'Measles, Mumps, Rubella, Varicella'));
  FList.add(TACIRConcept.Create('PRPOMP', 'PedvaxHIB', 'Hib', 'Hib'));
  FList.add(TACIRConcept.Create('PRPT', 'ActHIB', 'Hib', 'Hib'));
  FList.add(TACIRConcept.Create('PRVNR', 'Prevenar 7', '7 Pneumococcal (7vPCV)', 'Pneumococcal'));
  FList.add(TACIRConcept.Create('PRVTH', 'Prevenar 13', '13 Pneumococcal (13vPCV)', 'Pneumococcal'));
  FList.add(TACIRConcept.Create('PRXTEX', 'Priorix-Tetra', '-Tetra Measles, mumps, rubella, varicella', 'Measles, Mumps, Rubella, Varicella'));
  FList.add(TACIRConcept.Create('QDCL', 'Quadracel', 'Diphtheria, tetanus, pertussis, inactivated polio', 'Diphtheria, Tetanus, Pertussis, Polio'));
  FList.add(TACIRConcept.Create('ROTRIX', 'Rotarix', 'Rotavirus', 'Rotavirus'));
  FList.add(TACIRConcept.Create('ROTTEQ', 'Rotateq', 'Rotavirus', 'Rotavirus'));
  FList.add(TACIRConcept.Create('SYNFLX', 'Synflorix', 'Pneumococcal (11vPCV)', 'Pneumococcal'));
  FList.add(TACIRConcept.Create('TCL', 'Tripacel', 'Diphtheria, tetanus, pertussis', 'Diphtheria, Tetanus, Pertussis'));
  FList.add(TACIRConcept.Create('VAXGRP', 'Vaxigrip', 'Influenza', 'Influenza'));
  FList.add(TACIRConcept.Create('VGRJNR', 'Vaxigrip Junior', 'Junior Influenza', 'Influenza'));
  FList.add(TACIRConcept.Create('VLRIX', 'Varilrix', 'Varicella Zoster', 'Varicella'));
  FList.add(TACIRConcept.Create('VRVAX', 'Varivax', 'Varicella Zoster', 'Varicella'));

  for c in FList do
    FMap.AddOrSetValue(c.code, c.link);
end;

function TACIRServices.locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext;
begin
  result := FMap[code].link;
end;


function TACIRServices.Code(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string;
begin
  result := TACIRConcept(context).FCode;
end;

function TACIRServices.Definition(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
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

function TACIRServices.Display(opContext : TTxOperationContext; context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string;
begin
  result := TACIRConcept(context).Display.Trim;
end;

procedure TACIRServices.Designations(opContext : TTxOperationContext; context: TCodeSystemProviderContext; list: TConceptDesignations);
begin
  list.addDesignation(true, true, '', '', Display(opContext, context, nil));
end;

function TACIRServices.IsAbstract(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // ACIR doesn't do abstract
end;

function TACIRServices.isNotClosed(opContext : TTxOperationContext; textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TACIRServices.Link: TACIRServices;
begin
  result := TACIRServices(Inherited Link);
end;

function TACIRServices.getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext;
begin
  if context = nil then
    result := TCodeSystemIteratorContext.Create(nil, totalcount)
  else
    result := TCodeSystemIteratorContext.Create(nil, 0); // no children
end;

function TACIRServices.getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext;
begin
  result := FList[context.current].link;
  context.next;
end;

function TACIRServices.locateIsA(opContext : TTxOperationContext; code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  raise ETerminologySetup.Create('locateIsA not supported by ACIR'); // ACIR doesn't have formal subsumption property, so this is not used
end;


function TACIRServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'ACIR';
end;

function TACIRServices.prepare(opContext : TTxOperationContext; prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  raise ETerminologyTodo.Create('TACIRServices.prepare');
end;

function TACIRServices.searchFilter(opContext : TTxOperationContext; filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.Create('TACIRServices.searchFilter');
end;

function TACIRServices.subsumesTest(opContext : TTxOperationContext; codeA, codeB: String): String;
begin
  result := 'not-subsumed';
end;

function TACIRServices.filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.Create('TACIRServices.filter');
end;

function TACIRServices.filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.Create('TACIRServices.filterLocate');
end;

function TACIRServices.FilterMore(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  raise ETerminologyTodo.Create('TACIRServices.FilterMore');
end;

function TACIRServices.filterSize(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): integer;
begin
  raise ETerminologyTodo.Create('TACIRServices.FilterMore');
end;

function TACIRServices.FilterConcept(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.Create('TACIRServices.FilterConcept');
end;

function TACIRServices.InFilter(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyTodo.Create('TACIRServices.InFilter');
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
