Unit FHIRValidator4;

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


Uses
  SysUtils, Classes, System.Character, RegularExpressions,

  StringSupport, MathSupport, TextUtilities,
  AdvObjects, AdvGenerics, AdvObjectLists, Advbuffers, AdvMemories, AdvVCLStreams, AdvNameBuffers,

  ParserSupport, MXML, AdvXmlEntities, AdvJSON,

  FHIRBase, FHIRParser, FHIRSupport, FHIRXhtml,
  FHIRContext4, FHIRResources4, FHIRTypes4, FHIRPathNode4, FHIRPath4, FHIRMetaModel4;

Type
{
Maintaining the pascal validator is proving too expensive.
this validator is being retired in favour of using Java
validator via JNI (see JavaBridge)
}

  TBestPracticeWarningLevel = (bpwlIgnore, bpwlHint, bpwlWarning, bpwlError);
  TCheckDisplayOption = (cdoIgnore, cdopCheck, cdoCheckCaseAndSpace, cdoCheckCase, cdoCheckSpace);
  TResourceIdStatus = (risOptional, risRequired, risProhibited);

  TFHIRValidatorContext = class (TAdvObject)
  private
    FCheckDisplay: TCheckDisplayOption;
    FBPWarnings: TBestPracticeWarningLevel;
    FSuppressLoincSnomedMessages: boolean;
    FResourceIdRule: TResourceIdStatus;
    FIsAnyExtensionsAllowed: boolean;
    FErrors: TFhirOperationOutcomeIssueList;
    Fowned : TAdvObjectList;
    FOperationDescription : String;
    procedure SetErrors(const Value: TFhirOperationOutcomeIssueList);
  public
    constructor Create; override;
    destructor Destroy; override;

    property CheckDisplay : TCheckDisplayOption read FCheckDisplay write FCheckDisplay;
    property BPWarnings: TBestPracticeWarningLevel read FBPWarnings write FBPWarnings;
    property SuppressLoincSnomedMessages: boolean read FSuppressLoincSnomedMessages write FSuppressLoincSnomedMessages;
    property ResourceIdRule: TResourceIdStatus read FResourceIdRule write FResourceIdRule;
    property IsAnyExtensionsAllowed: boolean read FIsAnyExtensionsAllowed write FIsAnyExtensionsAllowed;
    property OperationDescription : String read FOperationDescription write FOperationDescription;

    property Errors : TFhirOperationOutcomeIssueList read FErrors write SetErrors;
  end;

  TValidationProfileSet = class (TAdvObject)
  private
    FCanonical : TStringList;
    FDefinitions : TFHIRStructureDefinitionList;
  public
    constructor Create; overload; override;
    constructor Create(profile : String); overload;
    constructor Create(profile : TFHIRStructureDefinition); overload;
    destructor Destroy; override;
  end;

implementation

uses
  FHIRParserBase, 
  FHIRUtilities4, FHIRProfileUtilities4;


{ TFHIRValidatorContext }

constructor TFHIRValidatorContext.create;
begin
  inherited;
  FOwned := TAdvObjectList.create;
  FErrors := TFHIROperationOutcomeIssueList.create;
end;

destructor TFHIRValidatorContext.destroy;
  begin
  FOwned.Free;
  FErrors.Free;
  inherited;
end;

procedure TFHIRValidatorContext.SetErrors(const Value: TFhirOperationOutcomeIssueList);
begin
  FErrors.Free;
  FErrors := Value;
end;


{ TValidationProfileSet }

constructor TValidationProfileSet.create;
begin
  inherited;
  FCanonical := TStringList.create;
  FDefinitions := TFHIRStructureDefinitionList.create;

end;

constructor TValidationProfileSet.create(profile: String);
begin
  Create;
  FCanonical.add(profile);
end;

constructor TValidationProfileSet.create(profile: TFHIRStructureDefinition);
begin
  Create;
  FDefinitions.add(profile.link);
end;

destructor TValidationProfileSet.Destroy;
begin
  FCanonical.Free;
  FDefinitions.Free;
  inherited;
end;


end.
