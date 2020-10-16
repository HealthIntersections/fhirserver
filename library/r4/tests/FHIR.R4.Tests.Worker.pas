unit FHIR.R4.Tests.Worker;

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
  {$IFDEF WINDOWS}Windows, ShellAPI, {$ENDIF}
  SysUtils, Classes,
  {$IFDEF FPC} FPCUnit, TestRegistry, {$ELSE} DUnitX.TestFramework, {$ENDIF}
  FHIR.Support.Utilities, FHIR.Support.Json,
  FHIR.Npm.Cache,
  FHIR.Base.Objects, FHIR.Base.Lang, FHIR.Base.Common, FHIR.Base.Factory,
  FHIR.Version.Parser,
  FHIR.Support.Tests,
  FHIR.R4.Types, FHIR.R4.Resources, FHIR.R4.Constants, FHIR.R4.Context, FHIR.R4.Profiles, FHIR.R4.PathEngine;

{$IFNDEF FPC}
Type
  FHIRFolderBasedTestCase4Attribute = class (CustomTestCaseSourceAttribute)
  private
    FFolder : String;
    FFilter : String;
    FCount : integer;
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  public
    constructor Create(folder, filter : String; count : integer);
  end;

  TTestingWorkerContext4 = class (TBaseWorkerContext)
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


{$ENDIF}

implementation

uses
  IdGlobalProtocols, FHIR.R4.Factory;

{$IFNDEF FPC}


{ TTestingWorkerContext4 }
var
  GWorkerContext : TBaseWorkerContext;

class procedure TTestingWorkerContext4.closeUp;
begin
  GWorkerContext.Free;
  GWorkerContext := nil;
end;

function TTestingWorkerContext4.expand(vs: TFhirValueSet; options : TExpansionOperationOptionSet = []): TFHIRValueSet;
begin
  raise EFHIRPathTodo.create('TTestingWorkerContext4.expand');
end;

function TTestingWorkerContext4.getSearchParameter(resourceType, name: String): TFHIRResourceV;
begin
  result := nil;
end;

function TTestingWorkerContext4.supportsSystem(system, version: string): boolean;
begin
  raise EFHIRPathTodo.create('TTestingWorkerContext4.supportsSystem');
end;


class function TTestingWorkerContext4.Use: TFHIRWorkerContext;
var
  pcm : TFHIRPackageManager;
  li : TPackageLoadingInformation;
begin
  if GWorkerContext = nil then
  begin
    GWorkerContext := TTestingWorkerContext4.create(TFHIRFactoryR4.create);
    pcm := TFHIRPackageManager.Create(false);
    li := TPackageLoadingInformation.create(FHIR.R4.Constants.FHIR_GENERATED_VERSION);
    try
      li.OnLoadEvent := GWorkerContext.loadResourceJson;
      pcm.loadPackage('hl7.fhir.r4.core', FHIR.R4.Constants.FHIR_GENERATED_VERSION, ['CodeSystem', 'ValueSet', 'StructureDefinition', 'StructureMap', 'ConceptMap'],
        li);
    finally
      li.Free;
      pcm.Free;
    end;
  end;
  result := GWorkerContext.link;
end;

function TTestingWorkerContext4.validateCode(system, version, code: String; vs: TFhirValueSet): TValidationResult;
begin
  raise EFHIRPathTodo.create('TTestingWorkerContext4.validateCode');
end;

function TTestingWorkerContext4.validateCode(system, version, code, display: String): TValidationResult;
begin
  raise EFHIRPathTodo.create('TTestingWorkerContext4.validateCode');
end;

function TTestingWorkerContext4.validateCode(code: TFHIRCodeableConcept; vs: TFhirValueSet): TValidationResult;
begin
  raise EFHIRPathTodo.create('TTestingWorkerContext4.validateCode');
end;

function TTestingWorkerContext4.validateCode(code: TFHIRCoding; vs: TFhirValueSet): TValidationResult;
begin
  raise EFHIRPathTodo.create('TTestingWorkerContext4.validateCode');
end;

{ FHIRFolderBasedTestCase4Attribute }

constructor FHIRFolderBasedTestCase4Attribute.Create(folder, filter: String; count : integer);
begin
  inherited Create;
  FFolder := folder;
  FFilter := filter;
  FCount := count;
end;

function FHIRFolderBasedTestCase4Attribute.GetCaseInfoArray: TestCaseInfoArray;
var
  sl : TStringlist;
  sr : TSearchRec;
  s : String;
  i : integer;
begin
  sl := TStringList.create;
  try
    if FindFirst(FHIR_TESTING_FILE(FFolder, '\*.*'), faAnyFile, SR) = 0 then
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

{ TTestObjectThread4 }

constructor TTestObjectThread4.Create(proc: TThreadProcedure);
begin
  FProc := proc;
  FreeOnTerminate := true;
  inherited Create(false);
end;

procedure TTestObjectThread4.execute;
begin
  Fproc;
end;

{ TTestObject4 }

procedure TTestObject4.thread(proc: TThreadProcedure);
begin
  TTestObjectThread4.Create(proc);
end;

initialization
finalization
  TTestingWorkerContext4.closeUp;
{$ENDIF}
end.

