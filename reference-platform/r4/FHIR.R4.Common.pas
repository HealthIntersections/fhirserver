unit FHIR.R4.Common;

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
  FHIR.Base.Objects, FHIR.XVersion.Resources, FHIR.R4.Types, FHIR.R4.Resources;

const
  ExceptionTypeTranslations : array [TExceptionType] of TFhirIssueTypeEnum = (IssueTypeNull, IssueTypeInvalid, IssueTypeStructure, IssueTypeRequired, IssueTypeValue,
    IssueTypeInvariant, IssueTypeSecurity, IssueTypeLogin, IssueTypeUnknown, IssueTypeExpired, IssueTypeForbidden, IssueTypeSuppressed, IssueTypeProcessing,
    IssueTypeNotSupported, IssueTypeDuplicate, IssueTypeNotFound, IssueTypeTooLong, IssueTypeCodeInvalid, IssueTypeExtension, IssueTypeTooCostly, IssueTypeBusinessRule,
    IssueTypeConflict, IssueTypeIncomplete, IssueTypeTransient, IssueTypeLockError, IssueTypeNoStore, IssueTypeException, IssueTypeTimeout, IssueTypeThrottled, IssueTypeInformational);

  ISSUE_SEVERITY_MAP : array [TFhirIssueSeverityEnum] of TIssueSeverity = (isNull, isFatal, isError, isWarning, isInformation);

type
  TFhirOperationOutcome4 = class (TFhirOperationOutcomeW)
  public
    function hasText : boolean; override;
    function text : String; override;
    function code : TExceptionType; override;
  end;

  TBundleHandler4 = class (TBundleHandler)
  public
    function next(bnd : TFHIRResourceV) : String; overload; override;
    procedure addEntries(bnd : TFHIRResourceV); override;
    procedure clearLinks; override;
  end;

  TFHIROperationOutcomeIssue4 = class (TFHIROperationOutcomeIssueW)
  private
    function issue : TFHIROperationOutcomeIssue;
  public
    function display : String; override;
    function severity : TIssueSeverity; override;
  end;

  TFHIRCapabilityStatement4 = class (TFHIRCapabilityStatementW)
  public
    function hasRest : boolean; override;
    function hasSecurity(system, code : String) : boolean; override;

    procedure readSmartExtension(var authorize, token, register: String); override;
    function hasFormat(fmt : String) : boolean; override;
  end;

  TFhirParametersParameter4 = class (TFhirParametersParameterW)
  private
    function parameter : TFhirParametersParameter;
  protected
    function GetValue: TFHIRObject; override;
    procedure SetValue(const Value: TFHIRObject); override;
    procedure populateList; override;
    function GetParameterParameter(name: String): TFhirParametersParameterW; override;
    function GetResourceParameter(name: String): TFHIRResourceV; override;
    function GetStringParameter(name: String): String; override;
  public
    function name : String; override;
    function hasValue : boolean; override;
    property value : TFHIRObject read GetValue write SetValue;
    function appendPart(name : String) : TFhirParametersParameterW; override;
  end;

  TFHIRParameters4 = class (TFHIRParametersW)
  private
    function parameter : TFhirParameters;
  protected
    procedure populateList; override;
  public
    function appendParameter(name : String) : TFhirParametersParameterW; override;
  end;


implementation

uses
  FHIR.R4.Utilities;

{ TFhirOperationOutcome4 }

function TFhirOperationOutcome4.code: TExceptionType;
var
  a : TExceptionType;
  op : TFhirOperationOutcome;
begin
  op := Fres as TFhirOperationOutcome;
  result := etNull;
  if not op.issueList.IsEmpty then
    for a := low(TExceptionType) to High(TExceptionType) do
      if ExceptionTypeTranslations[a] = op.issueList[0].code then
       exit(a);
end;

function TFhirOperationOutcome4.hasText: boolean;
var
  op : TFhirOperationOutcome;
begin
  op := Fres as TFhirOperationOutcome;
  if (op.text <> nil) and (op.text.div_ <> nil) then
    result := true
  else if (op.issueList.Count > 0) and (op.issueList[0].diagnostics <> '') then
    result := true
  else
    result := false;
end;

function TFhirOperationOutcome4.text: String;
var
  op : TFhirOperationOutcome;
begin
  op := Fres as TFhirOperationOutcome;
  if (op.text <> nil) and (op.text.div_ <> nil) then
    result := op.text.div_.AsPlainText
  else if (op.issueList.Count > 0) and (op.issueList[0].diagnostics <> '') then
    result := op.issueList[0].diagnostics
  else
    result := '';
end;

{ TBundleHandler4 }

procedure TBundleHandler4.addEntries(bnd: TFHIRResourceV);
var
  b : TFHIRBundle;
begin
  b := bnd as TFHIRBundle;
  TFhirBundle(resource).entryList.AddAll(b.entryList);
end;

procedure TBundleHandler4.clearLinks;
var
  b : TFHIRBundle;
begin
  b := resource as TFHIRBundle;
  b.link_List.Clear;
end;

function TBundleHandler4.next(bnd: TFHIRResourceV): String;
var
  b : TFHIRBundle;
begin
  b := bnd as TFHIRBundle;
  result := b.Links['next'];
end;

{ TFHIROperationOutcomeIssue4 }

function TFHIROperationOutcomeIssue4.display: String;
var
  i : TFHIROperationOutcomeIssue;
begin
  i := issue;
  result := i.diagnostics;
  if (i.details <> nil) and (i.details.text <> '') then
    result := i.details.text;
end;

function TFHIROperationOutcomeIssue4.issue: TFHIROperationOutcomeIssue;
begin
  result := FElement as TFHIROperationOutcomeIssue;
end;

function TFHIROperationOutcomeIssue4.severity: TIssueSeverity;
begin
  result := ISSUE_SEVERITY_MAP[issue.severity];
end;


{ TFHIRCapabilityStatement4 }

function TFHIRCapabilityStatement4.hasFormat(fmt: String): boolean;
var
  cs : TFHIRCapabilityStatement;
begin
  cs := FRes as TFHIRCapabilityStatement;
  result := cs.formatList.hasCode(fmt);
end;

function TFHIRCapabilityStatement4.hasRest: boolean;
var
  cs : TFHIRCapabilityStatement;
begin
  cs := FRes as TFHIRCapabilityStatement;
  result :=  cs.restList.Count > 0;
end;

function TFHIRCapabilityStatement4.hasSecurity(system, code: String): boolean;
var
  cs : TFHIRCapabilityStatement;
  cc : TFhirCodeableConcept;
begin
  cs := FRes as TFHIRCapabilityStatement;
  result := false;
  if (cs.restList[0].security <> nil) then
    for cc in cs.restList[0].security.serviceList do
      if cc.hasCode(system, code) then
        exit(true);
end;

procedure TFHIRCapabilityStatement4.readSmartExtension(var authorize, token, register: String);
var
  cs : TFHIRCapabilityStatement;
  ex1, ex2 : TFhirExtension;
begin
  cs := FRes as TFHIRCapabilityStatement;
  for ex1 in cs.restList[0].security.extensionList do
    if ex1.url = 'http://fhir-registry.smarthealthit.org/StructureDefinition/oauth-uris' then
      for ex2 in ex1.extensionList do
        if ex2.url = 'authorize' then
          authorize := TFHIRUri(ex2.value).value
        else if ex2.url = 'token' then
          token := TFHIRUri(ex2.value).value
        else if ex2.url = 'register' then
          register := TFHIRUri(ex2.value).value;
end;

{ TFhirParametersParameter4 }

function TFhirParametersParameter4.appendPart(name: String): TFhirParametersParameterW;
begin
  result := TFhirParametersParameter4.Create(parameter.partList.Append);
  TFhirParametersParameter4(result).parameter.name := name;
  FList.Add(result);
end;

function TFhirParametersParameter4.GetParameterParameter(name: String): TFhirParametersParameterW;
var
  t : TFhirParametersParameterW;
begin
  populateList;
  result := nil;
  for t in FList do
    if t.name = name then
      exit(t);
end;

function TFhirParametersParameter4.GetResourceParameter(name: String): TFHIRResourceV;
var
  t : TFhirParametersParameterW;
begin
  populateList;
  result := nil;
  for t in FList do
    if t.name = name then
      exit(TFhirParametersParameter4(t).parameter.resource);
end;

function TFhirParametersParameter4.GetStringParameter(name: String): String;
var
  t : TFhirParametersParameterW;
begin
  populateList;
  result := '';
  for t in FList do
    if t.name = name then
      exit(TFhirParametersParameter4(t).parameter.value.primitiveValue);
end;

function TFhirParametersParameter4.GetValue: TFHIRObject;
begin
  result := parameter.value;
end;

function TFhirParametersParameter4.hasValue: boolean;
begin
  result := parameter.value <> nil;
end;

function TFhirParametersParameter4.name: String;
begin
  result := parameter.name;
end;

function TFhirParametersParameter4.parameter: TFhirParametersParameter;
begin
  result := Element as TFhirParametersParameter;
end;

procedure TFhirParametersParameter4.populateList;
var
  t : TFhirParametersParameter;
begin
  for t in parameter.partList do
    FList.Add(TFhirParametersParameter4.Create(t.Link));
end;

procedure TFhirParametersParameter4.SetValue(const Value: TFHIRObject);
begin
  parameter.value := value as TFHIRType;
end;

{ TFHIRParameters4 }

function TFHIRParameters4.appendParameter(name: String): TFhirParametersParameterW;
begin
  result := TFhirParametersParameter4.Create(parameter.parameterList.Append);
  TFhirParametersParameter4(result).parameter.name := name;
  FList.Add(result);
end;

function TFHIRParameters4.parameter: TFhirParameters;
begin
  result := Resource as TFhirParameters;
end;

procedure TFHIRParameters4.populateList;
var
  t : TFhirParametersParameter;
begin
  for t in parameter.parameterList do
    FList.Add(TFhirParametersParameter4.Create(t.Link));
end;

end.
