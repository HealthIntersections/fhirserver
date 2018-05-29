unit FHIR.Base.Validator;

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
  FHIR.Support.Objects, FHIR.Support.Generics, FHIR.Support.Stream,
  FHIR.Support.Json, FHIR.Support.MXml,
  FHIR.Base.Objects, FHIR.Base.Common;

type
  TBestPracticeWarningLevel = (bpwlIgnore, bpwlHint, bpwlWarning, bpwlError);
  TCheckDisplayOption = (cdoIgnore, cdopCheck, cdoCheckCaseAndSpace, cdoCheckCase, cdoCheckSpace);
  TResourceIdStatus = (risOptional, risRequired, risProhibited);

  TFHIRValidatorContext = class (TFslObject)
  private
    FCheckDisplay: TCheckDisplayOption;
    FBPWarnings: TBestPracticeWarningLevel;
    FSuppressLoincSnomedMessages: boolean;
    FResourceIdRule: TResourceIdStatus;
    FIsAnyExtensionsAllowed: boolean;
    FIssues : TFslList<TFhirOperationOutcomeIssueW>;
    Fowned : TFslList<TFslObject>;
    FOperationDescription : String;
    procedure SetIssues(const Value: TFslList<TFhirOperationOutcomeIssueW>);
  public
    constructor Create; override;
    destructor Destroy; override;

    property CheckDisplay : TCheckDisplayOption read FCheckDisplay write FCheckDisplay;
    property BPWarnings: TBestPracticeWarningLevel read FBPWarnings write FBPWarnings;
    property SuppressLoincSnomedMessages: boolean read FSuppressLoincSnomedMessages write FSuppressLoincSnomedMessages;
    property ResourceIdRule: TResourceIdStatus read FResourceIdRule write FResourceIdRule;
    property IsAnyExtensionsAllowed: boolean read FIsAnyExtensionsAllowed write FIsAnyExtensionsAllowed;
    property OperationDescription : String read FOperationDescription write FOperationDescription;

    property owned : TFslList<TFslObject> read FOwned;
    property Issues : TFslList<TFhirOperationOutcomeIssueW> read FIssues write SetIssues;
  end;

  TFHIRValidatorV = class abstract(TFslObject)
  private
  public
    Constructor Create(context: TFHIRWorkerContextV); virtual;

    procedure validate(ctxt : TFHIRValidatorContext; obj: TJsonObject); overload; virtual; abstract;
    procedure validate(ctxt : TFHIRValidatorContext; obj: TJsonObject; profile: String); overload; virtual; abstract;

    procedure validate(ctxt : TFHIRValidatorContext; element: TMXmlElement); overload; virtual; abstract;
    procedure validate(ctxt : TFHIRValidatorContext; element: TMXmlElement; profile: String); overload; virtual; abstract;

    procedure validate(ctxt : TFHIRValidatorContext; document: TMXmlDocument); overload; virtual; abstract;
    procedure validate(ctxt : TFHIRValidatorContext; document: TMXmlDocument; profile: String); overload; virtual; abstract;

    procedure validate(ctxt : TFHIRValidatorContext; source : TFslBuffer; format : TFHIRFormat); overload; virtual; abstract;
    procedure validate(ctxt : TFHIRValidatorContext; source : TFslBuffer; format : TFHIRFormat; profile : String); overload; virtual; abstract;

    procedure validate(ctxt : TFHIRValidatorContext; resource : TFhirResourceV); overload; virtual; abstract;
    procedure validate(ctxt : TFHIRValidatorContext; resource : TFhirResourceV; profile : string); overload; virtual; abstract;
  end;

  TFHIRValidatorClass = class of TFHIRValidatorV;

implementation

{ TFHIRValidatorContext }

constructor TFHIRValidatorContext.create;
begin
  inherited;
  FOwned := TFslList<TFslObject>.create;
  FIssues := TFslList<TFhirOperationOutcomeIssueW>.create;
end;

destructor TFHIRValidatorContext.destroy;
  begin
  FOwned.Free;
  FIssues.Free;
  inherited;
end;

procedure TFHIRValidatorContext.SetIssues(const Value: TFslList<TFhirOperationOutcomeIssueW>);
begin
  FIssues.Free;
  FIssues := Value;
end;


{ TFHIRValidatorV }

constructor TFHIRValidatorV.Create(context: TFHIRWorkerContextV);
begin
  inherited create;
end;

end.
