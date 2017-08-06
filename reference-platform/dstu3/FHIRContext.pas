unit FHIRContext;

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
  AdvObjects, AdvGenerics,
  FHIRTypes, FHIRResources;

type
  TValidationResult = class (TAdvObject)
  private
    FSeverity : TFhirIssueSeverityEnum;
    FMessage  : String;
    FDisplay: String;
  public
    constructor Create; overload; override;
    constructor Create(Severity : TFhirIssueSeverityEnum; Message : String); overload; virtual;
    constructor Create(display : String); overload; virtual;
    Property Severity : TFhirIssueSeverityEnum read FSeverity write FSeverity;
    Property Message : String read FMessage write FMessage;
    Property Display : String read FDisplay write FDisplay;
    function isOk : boolean;
  end;

  TFHIRCustomResourceInformation = class (TAdvObject)
  private
    FName: String;
    FSearchParameters: TAdvList<TFHIRSearchParameter>;
    FDefinition: TFHIRStructureDefinition;
  public
    Constructor Create(definition : TFHIRStructureDefinition);
    Destructor Destroy; override;
    function Link : TFHIRCustomResourceInformation; overload;
    property Name : String read FName;
    property Definition : TFHIRStructureDefinition read FDefinition;
    property SearchParameters : TAdvList<TFHIRSearchParameter> read FSearchParameters;
  end;


  TFHIRWorkerContext = class abstract (TAdvObject)
  public
    function link : TFHIRWorkerContext; overload;

    function allStructures : TAdvMap<TFHIRStructureDefinition>.TValueCollection; virtual; abstract;
    function getResourceNames : TAdvStringSet; virtual; abstract;
    function fetchResource(t : TFhirResourceType; url : String) : TFhirResource; virtual; abstract;
    function expand(vs : TFhirValueSet) : TFHIRValueSet; virtual; abstract;
    function supportsSystem(system, version : string) : boolean; virtual; abstract;
    function validateCode(system, version, code, display : String) : TValidationResult; overload; virtual; abstract;
    function validateCode(system, version, code : String; vs : TFhirValueSet) : TValidationResult; overload; virtual; abstract;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; overload; virtual; abstract;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; overload; virtual; abstract;
    function getChildMap(profile : TFHIRStructureDefinition; element : TFhirElementDefinition) : TFHIRElementDefinitionList; virtual;  abstract;
    function getStructure(url : String) : TFHIRStructureDefinition; overload; virtual; abstract;
    function getStructure(ns, name : String) : TFHIRStructureDefinition; overload; virtual; abstract;
    function getCustomResource(name : String) : TFHIRCustomResourceInformation; virtual; abstract;
    function hasCustomResource(name : String) : boolean; virtual; abstract;
    function allResourceNames : TArray<String>; virtual; abstract;
    function nonSecureResourceNames : TArray<String>; virtual; abstract;
  end;

implementation

{ TValidationResult }

constructor TValidationResult.Create(Severity: TFhirIssueSeverityEnum; Message: String);
begin
  inherited create;
  FSeverity := Severity;
  FMessage := Message;
end;

constructor TValidationResult.Create;
begin
  Inherited Create;
end;

constructor TValidationResult.Create(display: String);
begin
  inherited Create;
  FDisplay := display;
end;

function TValidationResult.isOk: boolean;
begin
  result := not (Severity in [IssueSeverityError, IssueSeverityFatal]);
end;

{ TFHIRWorkerContext }

function TFHIRWorkerContext.link: TFHIRWorkerContext;
begin
  result := TFHIRWorkerContext(inherited link);
end;

{ TFHIRCustomResourceInformation }

constructor TFHIRCustomResourceInformation.Create(definition: TFHIRStructureDefinition);
begin
  inherited Create;
  FDefinition := definition;
  FName := definition.snapshot.elementList[0].path;
  FSearchParameters := TAdvList<TFHIRSearchParameter>.create;
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
