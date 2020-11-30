unit FHIR.R3.Tests.Worker;

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


interface

uses
  SysUtils, Classes, Windows, WinAPI.ShellAPI, Soap.EncdDecd,
   fsl_utilities,
  fhir_objects,  fhir_factory,
  FHIR.Version.Parser,
  fhir3_types, fhir3_resources, fhir3_constants, fhir3_context, fhir3_profiles, fhir3_pathengine,
  fsl_json, DUnitX.TestFramework;

var
  PUB_HOME : String;

Type
  FHIRFolderBasedTestCaseAttribute = class (CustomTestCaseSourceAttribute)
  private
    FFolder : String;
    FFilter : String;
    FCount : integer;
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  public
    constructor Create(folder, filter : String; count : integer);
  end;

  TTestingWorkerContext = class (TBaseWorkerContext)
  public
    function expand(vs : TFhirValueSet; options : TExpansionOperationOptionSet = []) : TFHIRValueSet; override;
    function supportsSystem(system, version : string) : boolean; override;
    function validateCode(system, version, code, display : String) : TValidationResult; overload; override;
    function validateCode(system, version, code : String; vs : TFhirValueSet) : TValidationResult; overload; override;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; overload; override;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; overload; override;

    class function Use : TFHIRWorkerContext;
    class procedure closeUp;
  protected
    function sizeInBytesV : cardinal; override;
  end;


implementation

uses
  fsl_stream, IdGlobalProtocols;



{ TTestingWorkerContext }
var
  GWorkerContext : TBaseWorkerContext;

function TTestingWorkerContext.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, class function Use.sizeInBytes);
end;

class procedure TTestingWorkerContext.closeUp;
begin
  GWorkerContext.Free;
  GWorkerContext := nil;
end;

function TTestingWorkerContext.expand(vs: TFhirValueSet; options : TExpansionOperationOptionSet = []): TFHIRValueSet;
begin
  raise EFHIRTodo.create('Not done for testing');;
end;

function TTestingWorkerContext.supportsSystem(system, version: string): boolean;
begin
  raise EFHIRTodo.create('Not done for testing');;
end;


class function TTestingWorkerContext.Use: TFHIRWorkerContext;
begin
  if GWorkerContext = nil then
  begin
    GWorkerContext := TTestingWorkerContext.create;
//    GWorkerContext.LoadFromDefinitions(IncludeTrailingBackslash(PUB_HOME)+'build\\publish\\validation-min.xml.zip');
    GWorkerContext.LoadFromFile(IncludeTrailingBackslash(PUB_HOME)+'build\\publish\\profiles-types.xml');
    GWorkerContext.LoadFromFile(IncludeTrailingBackslash(PUB_HOME)+'build\\publish\\profiles-resources.xml');
  end;
  result := GWorkerContext.link;
end;

function TTestingWorkerContext.validateCode(system, version, code: String; vs: TFhirValueSet): TValidationResult;
begin
  raise EFHIRTodo.create('Not done for testing');;
end;

function TTestingWorkerContext.validateCode(system, version, code, display: String): TValidationResult;
begin
  raise EFHIRTodo.create('Not done for testing');;
end;

function TTestingWorkerContext.validateCode(code: TFHIRCodeableConcept; vs: TFhirValueSet): TValidationResult;
begin
  raise EFHIRTodo.create('Not done for testing');;
end;

function TTestingWorkerContext.validateCode(code: TFHIRCoding; vs: TFhirValueSet): TValidationResult;
begin
  raise EFHIRTodo.create('Not done for testing');;
end;

{ FHIRFolderBasedTestCaseAttribute }

constructor FHIRFolderBasedTestCaseAttribute.Create(folder, filter: String; count : integer);
begin
  inherited Create;
  FFolder := folder;
  FFilter := filter;
  FCount := count;
end;

function FHIRFolderBasedTestCaseAttribute.GetCaseInfoArray: TestCaseInfoArray;
var
  sl : TStringlist;
  sr : TSearchRec;
  s : String;
  i : integer;
begin
  sl := TStringList.create;
  try
    if FindFirst(FFolder+'\*.*', faAnyFile, SR) = 0 then
    repeat
      s := sr.Name;
      if ((FFilter = '') or s.endsWith(FFilter)) and ((FCount = 0) or (sl.count < FCount)) then
        sl.Add(sr.Name);
    until FindNext(SR) <> 0;
    setLength(result, sl.Count);
    for i := 0 to sl.Count - 1 do
    begin
      result[i].Name := sl[i];
      SetLength(result[i].Values, 1);
      result[i].Values[0] := IncludeTrailingPathDelimiter(FFolder) + sl[i];
    end;
  finally
    sl.Free;
  end;
end;

initialization
finalization
  TTestingWorkerContext.closeUp;
end.

