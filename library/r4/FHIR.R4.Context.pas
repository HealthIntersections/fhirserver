unit FHIR.R4.Context;

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
  SysUtils,
  FHIR.Support.Base, 
  FHIR.Base.Objects, FHIR.Base.Factory, FHIR.Base.Common, FHIR.Base.Lang,
  FHIR.R4.Types, FHIR.R4.Resources;

type
  TFHIRCustomResourceInformation = class (TFslObject)
  private
    FName: String;
    FSearchParameters: TFslList<TFHIRSearchParameter>;
    FDefinition: TFHIRStructureDefinition;
  public
    constructor Create(definition : TFHIRStructureDefinition);
    destructor Destroy; override;
    function Link : TFHIRCustomResourceInformation; overload;
    property Name : String read FName;
    property Definition : TFHIRStructureDefinition read FDefinition;
    property SearchParameters : TFslList<TFHIRSearchParameter> read FSearchParameters;
  end;


  TFHIRWorkerContext = class abstract (TFHIRWorkerContextWithFactory)
  protected
    function GetVersion: TFHIRVersion; override;
  public
    function link : TFHIRWorkerContext; overload;

    procedure listStructures(list : TFslList<TFHIRStructureDefinition>); overload; virtual; abstract;
    function fetchResource(t : TFhirResourceType; url : String) : TFhirResource; overload; virtual; abstract;
    function expand(vs : TFhirValueSet; options : TExpansionOperationOptionSet = []) : TFHIRValueSet; overload; virtual; abstract;
    function validateCode(system, version, code : String; vs : TFhirValueSet) : TValidationResult; overload; virtual; abstract;
    function validateCode(system, version, code : String) : TValidationResult; overload; virtual; abstract;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; overload; virtual; abstract;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; overload; virtual; abstract;
    function getChildMap(profile : TFHIRStructureDefinition; element : TFhirElementDefinition) : TFHIRElementDefinitionList; virtual;  abstract;
    function getStructure(url : String) : TFHIRStructureDefinition; overload; virtual; abstract;
    function getStructure(ns, name : String) : TFHIRStructureDefinition; overload; virtual; abstract;
    function getCustomResource(name : String) : TFHIRCustomResourceInformation; virtual; abstract;
    function hasCustomResource(name : String) : boolean; virtual; abstract;

    // override version independent variants:
    function fetchResource(rType : String; url : String) : TFhirResourceV; overload; override;
    function expand(vs : TFhirValueSetW; options : TExpansionOperationOptionSet = []) : TFHIRValueSetW; overload; override;
    function validateCode(system, version, code : String; vs : TFhirValueSetW) : TValidationResult; overload; override;
    procedure listStructures(list : TFslList<TFhirStructureDefinitionW>); overload; override;
  end;

implementation

uses
  FHIR.R4.Utilities;

{ TFHIRWorkerContext }

function TFHIRWorkerContext.GetVersion: TFHIRVersion;
begin
  result := fhirVersionRelease4;
end;

function TFHIRWorkerContext.link: TFHIRWorkerContext;
begin
  result := TFHIRWorkerContext(inherited link);
end;

function TFHIRWorkerContext.expand(vs: TFhirValueSetW; options : TExpansionOperationOptionSet = []): TFHIRValueSetW;
begin
  result := Factory.wrapValueSet(expand(vs.Resource as TFhirValueSet, options));
end;

function TFHIRWorkerContext.validateCode(system, version, code: String; vs: TFhirValueSetW): TValidationResult;
begin
  result := validateCode(system, version, code, vs.Resource as TFHIRValueSet);
end;

function TFHIRWorkerContext.fetchResource(rType, url: String): TFhirResourceV;
var
  t : TFhirResourceType;
begin
  if RecogniseFHIRResourceName(rType, t) then
    result := fetchResource(t, url)
  else
    raise EFHIRException.create('Unknown type '+rType+' in '+versionString);
end;

procedure TFHIRWorkerContext.listStructures(list: TFslList<TFhirStructureDefinitionW>);
var
  l : TFslList<TFHIRStructureDefinition>;
  sd : TFHIRStructureDefinition;
begin
  l := TFslList<TFHIRStructureDefinition>.create;
  try
    listStructures(l);
    for sd in l do
      list.add(factory.wrapStructureDefinition(sd.link));
  finally
    l.Free;
  end;
end;


{ TFHIRCustomResourceInformation }

constructor TFHIRCustomResourceInformation.Create(definition: TFHIRStructureDefinition);
begin
  inherited Create;
  FDefinition := definition;
  FName := definition.snapshot.elementList[0].path;
  FSearchParameters := TFslList<TFHIRSearchParameter>.create;
end;

destructor TFHIRCustomResourceInformation.Destroy;
begin
  FSearchParameters.Free;
  FDefinition.Free;
  inherited;
end;

function TFHIRCustomResourceInformation.Link: TFHIRCustomResourceInformation;
begin
  result := TFHIRCustomResourceInformation(inherited Link);
end;


end.
