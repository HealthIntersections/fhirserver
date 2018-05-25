unit FHIR.R3.Common;

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
  FHIR.Base.Objects, FHIR.XVersion.Resources, FHIR.R3.Types, FHIR.R3.Resources;

const
  ExceptionTypeTranslations : array [TExceptionType] of TFhirIssueTypeEnum = (IssueTypeNull, IssueTypeInvalid, IssueTypeStructure, IssueTypeRequired, IssueTypeValue,
    IssueTypeInvariant, IssueTypeSecurity, IssueTypeLogin, IssueTypeUnknown, IssueTypeExpired, IssueTypeForbidden, IssueTypeSuppressed, IssueTypeProcessing,
    IssueTypeNotSupported, IssueTypeDuplicate, IssueTypeNotFound, IssueTypeTooLong, IssueTypeCodeInvalid, IssueTypeExtension, IssueTypeTooCostly, IssueTypeBusinessRule,
    IssueTypeConflict, IssueTypeIncomplete, IssueTypeTransient, IssueTypeLockError, IssueTypeNoStore, IssueTypeException, IssueTypeTimeout, IssueTypeThrottled, IssueTypeInformational);

  ISSUE_SEVERITY_MAP : array [TFhirIssueSeverityEnum] of TIssueSeverity = (isNull, isFatal, isError, isWarning, isInformation);
  ISSUE_SEVERITY_MAP2 : array [TIssueSeverity] of TFhirIssueSeverityEnum = (IssueSeverityNull, IssueSeverityFatal, IssueSeverityError, IssueSeverityWarning, IssueSeverityInformation);

type
  TFhirOperationOutcome3 = class (TFhirOperationOutcomeW)
  public
    function hasText : boolean; override;
    function text : String; override;
    function code : TExceptionType; override;
  end;

  TBundleHandler3 = class (TBundleHandler)
  public
    function next(bnd : TFHIRResourceV) : String; overload; override;
    procedure addEntries(bnd : TFHIRResourceV); override;
    procedure clearLinks; override;
  end;

  TFHIROperationOutcomeIssue3 = class (TFHIROperationOutcomeIssueW)
  private
    function issue : TFHIROperationOutcomeIssue;
  public
    function display : String; override;
    function severity : TIssueSeverity; override;
  end;

  TFHIRCapabilityStatement3 = class (TFHIRCapabilityStatementW)
  public
    function hasRest : boolean; override;
    function hasSecurity(system, code : String) : boolean; override;

    procedure readSmartExtension(var authorize, token, register: String); override;
    function hasFormat(fmt : String) : boolean; override;
  end;

  TFHIRStructureDefinition3 = class (TFhirStructureDefinitionW)
  public
    function kind : TStructureDefinitionKind; override;
    function name : String; override;
    function url : String; override;
  end;

  TFhirParametersParameter3 = class (TFhirParametersParameterW)
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

  TFHIRParameters3 = class (TFHIRParametersW)
  private
    function parameter : TFhirParameters;
  protected
    procedure populateList; override;
  public
    function appendParameter(name : String) : TFhirParametersParameterW; override;
  end;

implementation

uses
  FHIR.R3.Utilities;

{ TFhirOperationOutcome3 }

function TFhirOperationOutcome3.code: TExceptionType;
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

function TFhirOperationOutcome3.hasText: boolean;
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

function TFhirOperationOutcome3.text: String;
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

{ TBundleHandler3 }

procedure TBundleHandler3.addEntries(bnd: TFHIRResourceV);
var
  b : TFHIRBundle;
begin
  b := bnd as TFHIRBundle;
  TFhirBundle(resource).entryList.AddAll(b.entryList);
end;

procedure TBundleHandler3.clearLinks;
var
  b : TFHIRBundle;
begin
  b := resource as TFHIRBundle;
  b.link_List.Clear;
end;

function TBundleHandler3.next(bnd: TFHIRResourceV): String;
var
  b : TFHIRBundle;
begin
  b := bnd as TFHIRBundle;
  result := b.Links['next'];
end;

{ TFHIROperationOutcomeIssue3 }

function TFHIROperationOutcomeIssue3.display: String;
var
  i : TFHIROperationOutcomeIssue;
begin
  i := issue;
  result := i.diagnostics;
  if (i.details <> nil) and (i.details.text <> '') then
    result := i.details.text;
end;

function TFHIROperationOutcomeIssue3.issue: TFHIROperationOutcomeIssue;
begin
  result := Element as TFHIROperationOutcomeIssue;
end;

function TFHIROperationOutcomeIssue3.severity: TIssueSeverity;
begin
  result := ISSUE_SEVERITY_MAP[issue.severity];
end;


{ TFHIRCapabilityStatement3 }

function TFHIRCapabilityStatement3.hasFormat(fmt: String): boolean;
var
  cs : TFHIRCapabilityStatement;
begin
  cs := FRes as TFHIRCapabilityStatement;
  result := cs.formatList.hasCode(fmt);
end;

function TFHIRCapabilityStatement3.hasRest: boolean;
var
  cs : TFHIRCapabilityStatement;
begin
  cs := FRes as TFHIRCapabilityStatement;
  result :=  cs.restList.Count > 0;
end;

function TFHIRCapabilityStatement3.hasSecurity(system, code: String): boolean;
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

procedure TFHIRCapabilityStatement3.readSmartExtension(var authorize, token, register: String);
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

{ TFhirParametersParameter3 }

function TFhirParametersParameter3.appendPart(name: String): TFhirParametersParameterW;
begin
  result := TFhirParametersParameter3.Create(parameter.partList.Append.Link);
  TFhirParametersParameter3(result).parameter.name := name;
  PartList.Add(result);
end;

function TFhirParametersParameter3.GetParameterParameter(name: String): TFhirParametersParameterW;
var
  t : TFhirParametersParameterW;
begin
  populateList;
  result := nil;
  for t in FList do
    if t.name = name then
      exit(t);
end;

function TFhirParametersParameter3.GetResourceParameter(name: String): TFHIRResourceV;
var
  t : TFhirParametersParameterW;
begin
  populateList;
  result := nil;
  for t in FList do
    if t.name = name then
      exit(TFhirParametersParameter3(t).parameter.resource);
end;

function TFhirParametersParameter3.GetStringParameter(name: String): String;
var
  t : TFhirParametersParameterW;
begin
  populateList;
  result := '';
  for t in FList do
    if t.name = name then
      exit(TFhirParametersParameter3(t).parameter.value.primitiveValue);
end;

function TFhirParametersParameter3.GetValue: TFHIRObject;
begin
  result := parameter.value;
end;

function TFhirParametersParameter3.hasValue: boolean;
begin
  result := parameter.value <> nil;
end;

function TFhirParametersParameter3.name: String;
begin
  result := parameter.name;
end;

function TFhirParametersParameter3.parameter: TFhirParametersParameter;
begin
  result := Element as TFhirParametersParameter;
end;

procedure TFhirParametersParameter3.populateList;
var
  t : TFhirParametersParameter;
begin
  inherited;
  for t in parameter.partList do
    FList.Add(TFhirParametersParameter3.Create(t.Link));
end;

procedure TFhirParametersParameter3.SetValue(const Value: TFHIRObject);
begin
  parameter.value := value as TFHIRType;
end;

{ TFHIRParameters3 }

function TFHIRParameters3.appendParameter(name: String): TFhirParametersParameterW;
begin
  result := TFhirParametersParameter3.Create(parameter.parameterList.Append.link);
  TFhirParametersParameter3(result).parameter.name := name;
  ParameterList.Add(result);
end;

function TFHIRParameters3.parameter: TFhirParameters;
begin
  result := Resource as TFhirParameters;
end;

procedure TFHIRParameters3.populateList;
var
  t : TFhirParametersParameter;
begin
  inherited;
  for t in parameter.parameterList do
    ParameterList.Add(TFhirParametersParameter3.Create(t.Link));
end;

{ TFHIRStructureDefinition3 }

function TFHIRStructureDefinition3.kind: TStructureDefinitionKind;
begin
  case TFHIRStructureDefinition(resource).kind of
    StructureDefinitionKindPrimitiveType : result := sdkPrimitive;
    StructureDefinitionKindComplexType :
      if TFHIRStructureDefinition(resource).type_ = 'Extension' then
          result := sdkExtension
        else
          result := sdkDataType;
    StructureDefinitionKindResource :result := sdkResource;
    StructureDefinitionKindLogical : result := sdkResource;
  end;
end;

function TFHIRStructureDefinition3.name: String;
begin
  result := TFHIRStructureDefinition(resource).name;
end;

function TFHIRStructureDefinition3.url: String;
begin
  result := TFHIRStructureDefinition(resource).url;
end;

end.
