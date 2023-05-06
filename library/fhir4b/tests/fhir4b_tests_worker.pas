unit fhir4b_tests_worker;

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
  fhir4b_parser,
  fsl_tests,
  fhir4b_types, fhir4b_resources, fhir4b_constants, fhir4b_context, fhir4b_profiles, fhir4b_pathengine;

Type
  TTestingWorkerContext4B = class (TBaseWorkerContext)
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
  IdGlobalProtocols, fhir4b_factory;


{ TTestingWorkerContext4B }
var
  GWorkerContext : TBaseWorkerContext;

class procedure TTestingWorkerContext4B.closeUp;
begin
  GWorkerContext.Free;
  GWorkerContext := nil;
end;

function TTestingWorkerContext4B.expand(vs: TFhirValueSet; options : TExpansionOperationOptionSet = []): TFHIRValueSet;
begin
  raise EFHIRPathTodo.create('TTestingWorkerContext4B.expand');
end;

function TTestingWorkerContext4B.getSearchParameter(resourceType, name: String): TFHIRResourceV;
begin
  result := nil;
end;

function TTestingWorkerContext4B.supportsSystem(system, version: string): boolean;
begin
  raise EFHIRPathTodo.create('TTestingWorkerContext4B.supportsSystem');
end;


class function TTestingWorkerContext4B.Use: TFHIRWorkerContext;
var
  pcm : TFHIRPackageManager;
  li : TPackageLoadingInformation;
begin
  if GWorkerContext = nil then
  begin
    GWorkerContext := TTestingWorkerContext4B.create(TFHIRFactoryR4B.create, TFHIRPackageManager.Create(npmModeUser));
    pcm := TFHIRPackageManager.Create(npmModeSystem);
    li := TPackageLoadingInformation.create(fhir4b_constants.FHIR_GENERATED_VERSION);
    try
      li.OnLoadEvent := GWorkerContext.loadResourceJson;
      pcm.loadPackage('hl7.fhir.r4b.core', fhir4b_constants.FHIR_GENERATED_VERSION, ['CodeSystem', 'ValueSet', 'StructureDefinition', 'StructureMap', 'ConceptMap'],
        li);
    finally
      li.Free;
      pcm.Free;
    end;
  end;
  result := GWorkerContext.link;
end;

function TTestingWorkerContext4B.validateCode(system, version, code: String; vs: TFhirValueSet): TValidationResult;
begin
  raise EFHIRPathTodo.create('TTestingWorkerContext4B.validateCode');
end;

function TTestingWorkerContext4B.validateCode(system, version, code, display: String): TValidationResult;
begin
  raise EFHIRPathTodo.create('TTestingWorkerContext4B.validateCode');
end;

function TTestingWorkerContext4B.validateCode(code: TFHIRCodeableConcept; vs: TFhirValueSet): TValidationResult;
begin
  raise EFHIRPathTodo.create('TTestingWorkerContext4B.validateCode');
end;

function TTestingWorkerContext4B.validateCode(code: TFHIRCoding; vs: TFhirValueSet): TValidationResult;
begin
  raise EFHIRPathTodo.create('TTestingWorkerContext4B.validateCode');
end;

initialization
finalization
  TTestingWorkerContext4B.closeUp;
end.

