unit fhir5_tests_worker;

{.$.DEFINE DIFF}

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

{$i fhir.inc}

interface

uses
  {$IFDEF WINDOWS}Windows, ShellAPI, {$ENDIF}
  SysUtils, Classes,
  fsl_testing,
  fsl_utilities, fsl_json,
  fsl_npm_cache,
  fhir_objects,  fhir_common, fhir_factory,
  fhir5_parser,
  fsl_tests,
  fhir5_types, fhir5_resources, fhir5_constants, fhir5_context, fhir5_profiles, fhir5_pathengine;

Type
  TTestingWorkerContext5 = class (TBaseWorkerContext)
  public
    function expand(vs : TFhirValueSet; options : TExpansionOperationOptionSet = []) : TFHIRValueSet; override;
    function supportsSystem(system, version : string) : boolean; override;
    function validateCode(system, version, code, display : String) : TValidationResult; overload; override;
    function validateCode(system, version, code : String; vs : TFhirValueSet) : TValidationResult; overload; override;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; overload; override;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; overload; override;

    function getSearchParameter(resourceType, name : String) : TFHIRResourceV; override;
    class function Use : TFHIRWorkerContext;
    class procedure closeUp;
  end;

implementation

uses
  IdGlobalProtocols, fhir5_factory;


{ TTestingWorkerContext5 }
var
  GWorkerContext : TBaseWorkerContext;

class procedure TTestingWorkerContext5.closeUp;
begin
  GWorkerContext.Free;
  GWorkerContext := nil;
end;

function TTestingWorkerContext5.expand(vs: TFhirValueSet; options : TExpansionOperationOptionSet = []): TFHIRValueSet;
begin
  raise EFHIRPathTodo.create('TTestingWorkerContext5.expand');
end;

function TTestingWorkerContext5.getSearchParameter(resourceType, name: String): TFHIRResourceV;
begin
  result := nil;
end;

function TTestingWorkerContext5.supportsSystem(system, version: string): boolean;
begin
  raise EFHIRPathTodo.create('TTestingWorkerContext5.supportsSystem');
end;

class function TTestingWorkerContext5.Use: TFHIRWorkerContext;
var
  pcm : TFHIRPackageManager;
  li : TPackageLoadingInformation;
begin
  if GWorkerContext = nil then
  begin
    GWorkerContext := TTestingWorkerContext5.create(TFHIRFactoryR5.create, TFHIRPackageManager.Create(npmModeUser));
    pcm := TFHIRPackageManager.Create(npmModeSystem);
    li := TPackageLoadingInformation.create(fhir5_constants.FHIR_GENERATED_VERSION);
    try
      li.OnLoadEvent := GWorkerContext.loadResourceJson;
      pcm.loadPackage('hl7.fhir.r5.core', fhir5_constants.FHIR_GENERATED_VERSION, ['CodeSystem', 'ValueSet', 'StructureDefinition', 'StructureMap', 'ConceptMap'],
        li);
    finally
      li.Free;
      pcm.Free;
    end;
  end;
  result := GWorkerContext.link;
end;

function TTestingWorkerContext5.validateCode(system, version, code: String; vs: TFhirValueSet): TValidationResult;
begin
  raise EFHIRPathTodo.create('TTestingWorkerContext5.validateCode');
end;

function TTestingWorkerContext5.validateCode(system, version, code, display: String): TValidationResult;
begin
  raise EFHIRPathTodo.create('TTestingWorkerContext5.validateCode');
end;

function TTestingWorkerContext5.validateCode(code: TFHIRCodeableConcept; vs: TFhirValueSet): TValidationResult;
begin
  raise EFHIRPathTodo.create('TTestingWorkerContext5.validateCode');
end;

function TTestingWorkerContext5.validateCode(code: TFHIRCoding; vs: TFhirValueSet): TValidationResult;
begin
  raise EFHIRPathTodo.create('TTestingWorkerContext5.validateCode');
end;

initialization
finalization
  TTestingWorkerContext5.closeUp;
end.

