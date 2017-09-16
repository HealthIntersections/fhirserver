unit FHIRTestWorker;

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

{$IFNDEF FHIR4}
This is the dstu3 version of the FHIR code
{$ENDIF}


interface

uses
  {$IFDEF WINDOWS}Windows, WinAPI.ShellAPI, {$ENDIF}
  SysUtils, Classes, Soap.EncdDecd,
  StringSupport, FileSupport,
  FHIRBase, FHIRTypes, FHIRResources, FHIRConstants, FHIRParser, FHIRContext,
  FHIRSupport, FHIRProfileUtilities, FHIRPath,
  AdvJson, DUnitX.TestFramework;

var
  GBasePath : String;

Type
  FHIRFolderBasedTestCaseAttribute = class (CustomTestCaseSourceAttribute)
  private
    FFolder : String;
    FFilter : String;
    FCount : integer;
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  public
    Constructor Create(folder, filter : String; count : integer);
  end;

  TTestingWorkerContext = class (TBaseWorkerContext)
  public
    function expand(vs : TFhirValueSet) : TFHIRValueSet; override;
    function supportsSystem(system, version : string) : boolean; override;
    function validateCode(system, version, code, display : String) : TValidationResult; overload; override;
    function validateCode(system, version, code : String; vs : TFhirValueSet) : TValidationResult; overload; override;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; overload; override;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; overload; override;

    class function Use : TFHIRWorkerContext;
    class procedure closeUp;
  end;


  TTestObjectThread = class (TThread)
  private
    FProc : TThreadProcedure;
  protected
    procedure execute; override;
  public
    Constructor Create(proc : TThreadProcedure);
  end;
  TTestObject = class (TObject)
  private
  protected
    procedure thread(proc : TThreadProcedure);
  public
  end;

implementation

uses
  TextUtilities, IdGlobalProtocols, BytesSupport;



{ TTestingWorkerContext }
var
  GWorkerContext : TBaseWorkerContext;

class procedure TTestingWorkerContext.closeUp;
begin
  GWorkerContext.Free;
  GWorkerContext := nil;
end;

function TTestingWorkerContext.expand(vs: TFhirValueSet): TFHIRValueSet;
begin
  raise EFHIRPath.create('Not done yet');
end;

function TTestingWorkerContext.supportsSystem(system, version: string): boolean;
begin
  raise EFHIRPath.create('Not done yet');
end;


class function TTestingWorkerContext.Use: TFHIRWorkerContext;
begin
  if GWorkerContext = nil then
  begin
    GWorkerContext := TTestingWorkerContext.create;
//    GWorkerContext.LoadFromDefinitions(path([GBasePath, 'build', 'publish', 'validation-min.xml.zip']));
    GWorkerContext.LoadFromFile(path([GBasePath, 'build', 'publish', 'profiles-types.xml']));
    GWorkerContext.LoadFromFile(path([GBasePath, 'build', 'publish', 'profiles-resources.xml']));
  end;
  result := GWorkerContext.link;
end;

function TTestingWorkerContext.validateCode(system, version, code: String; vs: TFhirValueSet): TValidationResult;
begin
  raise EFHIRPath.create('Not done yet');
end;

function TTestingWorkerContext.validateCode(system, version, code, display: String): TValidationResult;
begin
  raise EFHIRPath.create('Not done yet');
end;

function TTestingWorkerContext.validateCode(code: TFHIRCodeableConcept; vs: TFhirValueSet): TValidationResult;
begin
  raise EFHIRPath.create('Not done yet');
end;

function TTestingWorkerContext.validateCode(code: TFHIRCoding; vs: TFhirValueSet): TValidationResult;
begin
  raise EFHIRPath.create('Not done yet');
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

{ TTestObjectThread }

constructor TTestObjectThread.Create(proc: TThreadProcedure);
begin
  FProc := proc;
  FreeOnTerminate := true;
  inherited Create(false);
end;

procedure TTestObjectThread.execute;
begin
  Fproc;
end;

{ TTestObject }

procedure TTestObject.thread(proc: TThreadProcedure);
begin
  TTestObjectThread.Create(proc);
end;

initialization
finalization
  TTestingWorkerContext.closeUp;
end.

