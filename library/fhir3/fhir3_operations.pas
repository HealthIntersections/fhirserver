unit fhir3_operations;

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

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

{$I fhir.inc}
{$I fhir3.inc}

interface

// FHIR v3.0.1 generated 2018-06-12T19:15:59+10:00

uses
  SysUtils, Classes, Generics.Collections, 
  fsl_base, fsl_utilities, fsl_stream, fsl_http, 
  fhir3_base, fhir3_types, fhir3_resources, fhir3_opbase;

Type

  //Operation apply (Apply)
  TFHIRApplyOpRequest = class (TFHIROperationRequest)
  private
    FPatient : TFhirReference;
    FEncounter : TFhirReference;
    FPractitioner : TFhirReference;
    FOrganization : TFhirReference;
    FUserType : TFhirCodeableConcept;
    FUserLanguage : TFhirCodeableConcept;
    FUserTaskContext : TFhirCodeableConcept;
    FSetting : TFhirCodeableConcept;
    FSettingContext : TFhirCodeableConcept;
    procedure SetPatient(value : TFhirReference);
    procedure SetEncounter(value : TFhirReference);
    procedure SetPractitioner(value : TFhirReference);
    procedure SetOrganization(value : TFhirReference);
    procedure SetUserType(value : TFhirCodeableConcept);
    procedure SetUserLanguage(value : TFhirCodeableConcept);
    procedure SetUserTaskContext(value : TFhirCodeableConcept);
    procedure SetSetting(value : TFhirCodeableConcept);
    procedure SetSettingContext(value : TFhirCodeableConcept);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property patient : TFhirReference read FPatient write SetPatient;
    property encounter : TFhirReference read FEncounter write SetEncounter;
    property practitioner : TFhirReference read FPractitioner write SetPractitioner;
    property organization : TFhirReference read FOrganization write SetOrganization;
    property userType : TFhirCodeableConcept read FUserType write SetUserType;
    property userLanguage : TFhirCodeableConcept read FUserLanguage write SetUserLanguage;
    property userTaskContext : TFhirCodeableConcept read FUserTaskContext write SetUserTaskContext;
    property setting : TFhirCodeableConcept read FSetting write SetSetting;
    property settingContext : TFhirCodeableConcept read FSettingContext write SetSettingContext;
  end;

  TFHIRApplyOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirResource;
    procedure SetReturn(value : TFhirResource);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirResource read FReturn write SetReturn;
  end;

  //Operation data-requirements (Data Requirements)
  TFHIRDataRequirementsOpRequest = class (TFHIROperationRequest)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  TFHIRDataRequirementsOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirLibrary;
    procedure SetReturn(value : TFhirLibrary);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirLibrary read FReturn write SetReturn;
  end;

  //Operation conforms (Test if a server implements a client's required operations)
  TFHIRConformsOpRequest = class (TFHIROperationRequest)
  private
    FLeft : String;
    FRight : String;
    FMode : String;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property left : String read FLeft write FLeft;
    property right : String read FRight write FRight;
    property mode : String read FMode write FMode;
  end;

  TFHIRConformsOpResponse = class (TFHIROperationResponse)
  private
    FIssues : TFhirOperationOutcome;
    FUnion : TFhirCapabilityStatement;
    FIntersection : TFhirCapabilityStatement;
    procedure SetIssues(value : TFhirOperationOutcome);
    procedure SetUnion(value : TFhirCapabilityStatement);
    procedure SetIntersection(value : TFhirCapabilityStatement);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property issues : TFhirOperationOutcome read FIssues write SetIssues;
    property union : TFhirCapabilityStatement read FUnion write SetUnion;
    property intersection : TFhirCapabilityStatement read FIntersection write SetIntersection;
  end;

  //Operation implements (Test if a server implements a client's required operations)
  TFHIRImplementsOpRequest = class (TFHIROperationRequest)
  private
    FServer : String;
    FClient : String;
    FResource : TFhirCapabilityStatement;
    procedure SetResource(value : TFhirCapabilityStatement);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property server : String read FServer write FServer;
    property client : String read FClient write FClient;
    property resource : TFhirCapabilityStatement read FResource write SetResource;
  end;

  TFHIRImplementsOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirOperationOutcome;
    procedure SetReturn(value : TFhirOperationOutcome);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirOperationOutcome read FReturn write SetReturn;
  end;

  //Operation subset (Fetch a subset of the CapabilityStatement resource)
  TFHIRSubsetOpRequest = class (TFHIROperationRequest)
  private
    FServer : String;
    FResourceList : TStringList;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property server : String read FServer write FServer;
    property resourceList : TStringList read FResourceList;
  end;

  TFHIRSubsetOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirCapabilityStatement;
    procedure SetReturn(value : TFhirCapabilityStatement);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirCapabilityStatement read FReturn write SetReturn;
  end;

  //Operation compose (Code Composition based on supplied properties)
  TFHIRComposeOpReqSubproperty = class (TFHIROperationObject)
  private
    FCode : String;
    FValue : String;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    constructor Create(params : TFhirParametersParameter); overload; override;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter; override;
    property code : String read FCode write FCode;
    property value : String read FValue write FValue;
  end;

  TFHIRComposeOpReqProperty_ = class (TFHIROperationObject)
  private
    FCode : String;
    FValue : String;
    FSubpropertyList : TFslList<TFHIRComposeOpReqSubproperty>;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    constructor Create(params : TFhirParametersParameter); overload; override;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter; override;
    property code : String read FCode write FCode;
    property value : String read FValue write FValue;
    property subpropertyList : TFslList<TFHIRComposeOpReqSubproperty> read FSubpropertyList;
  end;

  TFHIRComposeOpRequest = class (TFHIROperationRequest)
  private
    FSystem : String;
    FVersion : String;
    FProperty_List : TFslList<TFHIRComposeOpReqProperty_>;
    FExact : Boolean;
    FCompositional : Boolean;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property property_List : TFslList<TFHIRComposeOpReqProperty_> read FProperty_List;
    property exact : Boolean read FExact write FExact;
    property compositional : Boolean read FCompositional write FCompositional;
  end;

  TFHIRComposeOpRespProperty_ = class (TFHIROperationObject)
  private
    FCode : String;
    FValue : String;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    constructor Create(params : TFhirParametersParameter); overload; override;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter; override;
    property code : String read FCode write FCode;
    property value : String read FValue write FValue;
  end;

  TFHIRComposeOpRespUnmatched = class (TFHIROperationObject)
  private
    FCode : String;
    FValue : String;
    FProperty_List : TFslList<TFHIRComposeOpRespProperty_>;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    constructor Create(params : TFhirParametersParameter); overload; override;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter; override;
    property code : String read FCode write FCode;
    property value : String read FValue write FValue;
    property property_List : TFslList<TFHIRComposeOpRespProperty_> read FProperty_List;
  end;

  TFHIRComposeOpRespMatch = class (TFHIROperationObject)
  private
    FCode : TFhirCoding;
    FUnmatchedList : TFslList<TFHIRComposeOpRespUnmatched>;
    FComment : String;
    procedure SetCode(value : TFhirCoding);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    constructor Create(params : TFhirParametersParameter); overload; override;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter; override;
    property code : TFhirCoding read FCode write SetCode;
    property unmatchedList : TFslList<TFHIRComposeOpRespUnmatched> read FUnmatchedList;
    property comment : String read FComment write FComment;
  end;

  TFHIRComposeOpResponse = class (TFHIROperationResponse)
  private
    FMatchList : TFslList<TFHIRComposeOpRespMatch>;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property matchList : TFslList<TFHIRComposeOpRespMatch> read FMatchList;
  end;

  //Operation lookup (Concept Look Up & Decomposition)
  TFHIRLookupOpRequest = class (TFHIROperationRequest)
  private
    FCode : String;
    FSystem : String;
    FVersion : String;
    FCoding : TFhirCoding;
    FDate : TFslDateTime;
    FDisplayLanguage : String;
    FProperty_List : TStringList;
    procedure SetCoding(value : TFhirCoding);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property code : String read FCode write FCode;
    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property coding : TFhirCoding read FCoding write SetCoding;
    property date : TFslDateTime read FDate write FDate;
    property displayLanguage : String read FDisplayLanguage write FDisplayLanguage;
    property property_List : TStringList read FProperty_List;
  end;

  TFHIRLookupOpRespDesignation = class (TFHIROperationObject)
  private
    FLanguage : String;
    FUse : TFhirCoding;
    FValue : String;
    procedure SetUse(value : TFhirCoding);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    constructor Create(params : TFhirParametersParameter); overload; override;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter; override;
    property language : String read FLanguage write FLanguage;
    property use : TFhirCoding read FUse write SetUse;
    property value : String read FValue write FValue;
  end;

  TFHIRLookupOpRespSubproperty = class (TFHIROperationObject)
  private
    FCode : String;
    FValue : String;
    FDescription : String;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    constructor Create(params : TFhirParametersParameter); overload; override;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter; override;
    property code : String read FCode write FCode;
    property value : String read FValue write FValue;
    property description : String read FDescription write FDescription;
  end;

  TFHIRLookupOpRespProperty_ = class (TFHIROperationObject)
  private
    FCode : String;
    FValue : TFHIRType;
    FDescription : String;
    FSubpropertyList : TFslList<TFHIRLookupOpRespSubproperty>;
    procedure SetValue(const Value: TFHIRType);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    constructor Create(params : TFhirParametersParameter); overload; override;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter; override;
    property code : String read FCode write FCode;
    property value : TFHIRType read FValue write SetValue;
    property description : String read FDescription write FDescription;
    property subpropertyList : TFslList<TFHIRLookupOpRespSubproperty> read FSubpropertyList;
  end;

  TFHIRLookupOpResponse = class (TFHIROperationResponse)
  private
    FName : String;
    FVersion : String;
    FDisplay : String;
    FDesignationList : TFslList<TFHIRLookupOpRespDesignation>;
    FProperty_List : TFslList<TFHIRLookupOpRespProperty_>;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property name : String read FName write FName;
    property version : String read FVersion write FVersion;
    property display : String read FDisplay write FDisplay;
    property designationList : TFslList<TFHIRLookupOpRespDesignation> read FDesignationList;
    property property_List : TFslList<TFHIRLookupOpRespProperty_> read FProperty_List;
  end;

  //Operation subsumes (Subsumption Testing)
  TFHIRSubsumesOpRequest = class (TFHIROperationRequest)
  private
    FCodeA : String;
    FCodeB : String;
    FSystem : String;
    FVersion : String;
    FCodingA : TFhirCoding;
    FCodingB : TFhirCoding;
    procedure SetCodingA(value : TFhirCoding);
    procedure SetCodingB(value : TFhirCoding);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property codeA : String read FCodeA write FCodeA;
    property codeB : String read FCodeB write FCodeB;
    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property codingA : TFhirCoding read FCodingA write SetCodingA;
    property codingB : TFhirCoding read FCodingB write SetCodingB;
  end;

  TFHIRSubsumesOpResponse = class (TFHIROperationResponse)
  private
    FOutcome : String;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property outcome : String read FOutcome write FOutcome;
  end;

  //Operation document (Generate a Document)
  TFHIRDocumentOpRequest = class (TFHIROperationRequest)
  private
    FPersist : Boolean;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property persist : Boolean read FPersist write FPersist;
  end;

  TFHIRDocumentOpResponse = class (TFHIROperationResponse)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  //Operation closure (Closure Table Maintenance)
  TFHIRClosureOpRequest = class (TFHIROperationRequest)
  private
    FName : String;
    FConceptList : TFslList<TFhirCoding>;
    FVersion : String;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property name : String read FName write FName;
    property conceptList : TFslList<TFhirCoding> read FConceptList;
    property version : String read FVersion write FVersion;
  end;

  TFHIRClosureOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirConceptMap;
    procedure SetReturn(value : TFhirConceptMap);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirConceptMap read FReturn write SetReturn;
  end;

  //Operation translate (Concept Translation)
  TFHIRTranslateOpReqDependency = class (TFHIROperationObject)
  private
    FElement : String;
    FConcept : TFhirCodeableConcept;
    procedure SetConcept(value : TFhirCodeableConcept);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    constructor Create(params : TFhirParametersParameter); overload; override;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter; override;
    property element : String read FElement write FElement;
    property concept : TFhirCodeableConcept read FConcept write SetConcept;
  end;

  TFHIRTranslateOpRequest = class (TFHIROperationRequest)
  private
    FCode : String;
    FSystem : String;
    FVersion : String;
    FSource : String;
    FCoding : TFhirCoding;
    FCodeableConcept : TFhirCodeableConcept;
    FTarget : String;
    FTargetsystem : String;
    FDependencyList : TFslList<TFHIRTranslateOpReqDependency>;
    FReverse : Boolean;
    procedure SetCoding(value : TFhirCoding);
    procedure SetCodeableConcept(value : TFhirCodeableConcept);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property code : String read FCode write FCode;
    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property source : String read FSource write FSource;
    property coding : TFhirCoding read FCoding write SetCoding;
    property codeableConcept : TFhirCodeableConcept read FCodeableConcept write SetCodeableConcept;
    property target : String read FTarget write FTarget;
    property targetsystem : String read FTargetsystem write FTargetsystem;
    property dependencyList : TFslList<TFHIRTranslateOpReqDependency> read FDependencyList;
    property reverse : Boolean read FReverse write FReverse;
  end;

  TFHIRTranslateOpRespProduct = class (TFHIROperationObject)
  private
    FElement : String;
    FConcept : TFhirCoding;
    procedure SetConcept(value : TFhirCoding);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    constructor Create(params : TFhirParametersParameter); overload; override;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter; override;
    property element : String read FElement write FElement;
    property concept : TFhirCoding read FConcept write SetConcept;
  end;

  TFHIRTranslateOpRespMatch = class (TFHIROperationObject)
  private
    FEquivalence : String;
    FConcept : TFhirCoding;
    FProductList : TFslList<TFHIRTranslateOpRespProduct>;
    FSource : String;
    procedure SetConcept(value : TFhirCoding);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    constructor Create(params : TFhirParametersParameter); overload; override;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter; override;
    property equivalence : String read FEquivalence write FEquivalence;
    property concept : TFhirCoding read FConcept write SetConcept;
    property productList : TFslList<TFHIRTranslateOpRespProduct> read FProductList;
    property source : String read FSource write FSource;
  end;

  TFHIRTranslateOpResponse = class (TFHIROperationResponse)
  private
    FResult : Boolean;
    FMessage : String;
    FMatchList : TFslList<TFHIRTranslateOpRespMatch>;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property result : Boolean read FResult write FResult;
    property message : String read FMessage write FMessage;
    property matchList : TFslList<TFHIRTranslateOpRespMatch> read FMatchList;
  end;

  //Operation everything (Fetch Encounter Record)
  TFHIREverythingOpRequest = class (TFHIROperationRequest)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  TFHIREverythingOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirBundle;
    procedure SetReturn(value : TFhirBundle);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirBundle read FReturn write SetReturn;
  end;

  //Operation find (Find a functional list)
  TFHIRFindOpRequest = class (TFHIROperationRequest)
  private
    FPatient : String;
    FName : String;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property patient : String read FPatient write FPatient;
    property name : String read FName write FName;
  end;

  TFHIRFindOpResponse = class (TFHIROperationResponse)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  //Operation evaluate-measure (Evaluate Measure)
  TFHIREvaluateMeasureOpRequest = class (TFHIROperationRequest)
  private
    FPeriodStart : TFslDateTime;
    FPeriodEnd : TFslDateTime;
    FMeasure : TFhirReference;
    FReportType : String;
    FPatient : TFhirReference;
    FPractitioner : TFhirReference;
    FLastReceivedOn : TFslDateTime;
    procedure SetMeasure(value : TFhirReference);
    procedure SetPatient(value : TFhirReference);
    procedure SetPractitioner(value : TFhirReference);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property periodStart : TFslDateTime read FPeriodStart write FPeriodStart;
    property periodEnd : TFslDateTime read FPeriodEnd write FPeriodEnd;
    property measure : TFhirReference read FMeasure write SetMeasure;
    property reportType : String read FReportType write FReportType;
    property patient : TFhirReference read FPatient write SetPatient;
    property practitioner : TFhirReference read FPractitioner write SetPractitioner;
    property lastReceivedOn : TFslDateTime read FLastReceivedOn write FLastReceivedOn;
  end;

  TFHIREvaluateMeasureOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirMeasureReport;
    procedure SetReturn(value : TFhirMeasureReport);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirMeasureReport read FReturn write SetReturn;
  end;

  //Operation process-message (Process Message)
  TFHIRProcessMessageOpRequest = class (TFHIROperationRequest)
  private
    FContent : TFhirBundle;
    FAsync : Boolean;
    FResponseUrl : String;
    procedure SetContent(value : TFhirBundle);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property content : TFhirBundle read FContent write SetContent;
    property async : Boolean read FAsync write FAsync;
    property responseUrl : String read FResponseUrl write FResponseUrl;
  end;

  TFHIRProcessMessageOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirBundle;
    procedure SetReturn(value : TFhirBundle);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirBundle read FReturn write SetReturn;
  end;

  //Operation lastn (Last N Observations Query)
  TFHIRLastnOpRequest = class (TFHIROperationRequest)
  private
    FMax : String;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property max : String read FMax write FMax;
  end;

  TFHIRLastnOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirBundle;
    procedure SetReturn(value : TFhirBundle);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirBundle read FReturn write SetReturn;
  end;

  //Operation stats (Observation Statistics)
  TFHIRStatsOpRequest = class (TFHIROperationRequest)
  private
    FSubject : String;
    FCodeList : TStringList;
    FSystem : String;
    FCodingList : TFslList<TFhirCoding>;
    FDuration : String;
    FPeriod : TFhirPeriod;
    FStatisticList : TStringList;
    FInclude : Boolean;
    FLimit : String;
    procedure SetPeriod(value : TFhirPeriod);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property subject : String read FSubject write FSubject;
    property codeList : TStringList read FCodeList;
    property system : String read FSystem write FSystem;
    property codingList : TFslList<TFhirCoding> read FCodingList;
    property duration : String read FDuration write FDuration;
    property period : TFhirPeriod read FPeriod write SetPeriod;
    property statisticList : TStringList read FStatisticList;
    property include : Boolean read FInclude write FInclude;
    property limit : String read FLimit write FLimit;
  end;

  TFHIRStatsOpResponse = class (TFHIROperationResponse)
  private
    FReturnList : TFslList<TFhirObservation>;
    FSourceList : TFslList<TFhirObservation>;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property returnList : TFslList<TFhirObservation> read FReturnList;
    property sourceList : TFslList<TFhirObservation> read FSourceList;
  end;

  //Operation match (Find patient matches using MPI based logic)
  TFHIRMatchOpRequest = class (TFHIROperationRequest)
  private
    FResource : TFhirResource;
    FOnlyCertainMatches : Boolean;
    FCount : String;
    procedure SetResource(value : TFhirResource);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property resource : TFhirResource read FResource write SetResource;
    property onlyCertainMatches : Boolean read FOnlyCertainMatches write FOnlyCertainMatches;
    property count : String read FCount write FCount;
  end;

  TFHIRMatchOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirBundle;
    procedure SetReturn(value : TFhirBundle);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirBundle read FReturn write SetReturn;
  end;

  //Operation populate (Populate Questionnaire)
  TFHIRPopulateOpRequest = class (TFHIROperationRequest)
  private
    FIdentifier : String;
    FQuestionnaire : TFhirQuestionnaire;
    FQuestionnaireRef : TFhirReference;
    FSubject : TFhirReference;
    FContentList : TFslList<TFhirReference>;
    FLocal : Boolean;
    procedure SetQuestionnaire(value : TFhirQuestionnaire);
    procedure SetQuestionnaireRef(value : TFhirReference);
    procedure SetSubject(value : TFhirReference);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property identifier : String read FIdentifier write FIdentifier;
    property questionnaire : TFhirQuestionnaire read FQuestionnaire write SetQuestionnaire;
    property questionnaireRef : TFhirReference read FQuestionnaireRef write SetQuestionnaireRef;
    property subject : TFhirReference read FSubject write SetSubject;
    property contentList : TFslList<TFhirReference> read FContentList;
    property local : Boolean read FLocal write FLocal;
  end;

  TFHIRPopulateOpResponse = class (TFHIROperationResponse)
  private
    FQuestionnaire : TFhirQuestionnaireResponse;
    FIssues : TFhirOperationOutcome;
    procedure SetQuestionnaire(value : TFhirQuestionnaireResponse);
    procedure SetIssues(value : TFhirOperationOutcome);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property questionnaire : TFhirQuestionnaireResponse read FQuestionnaire write SetQuestionnaire;
    property issues : TFhirOperationOutcome read FIssues write SetIssues;
  end;

  //Operation populatehtml (Generate HTML for Questionnaire)
  TFHIRPopulatehtmlOpRequest = class (TFHIROperationRequest)
  private
    FIdentifier : String;
    FQuestionnaire : TFhirQuestionnaire;
    FQuestionnaireRef : TFhirReference;
    FContentList : TFslList<TFhirReference>;
    FLocal : Boolean;
    procedure SetQuestionnaire(value : TFhirQuestionnaire);
    procedure SetQuestionnaireRef(value : TFhirReference);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property identifier : String read FIdentifier write FIdentifier;
    property questionnaire : TFhirQuestionnaire read FQuestionnaire write SetQuestionnaire;
    property questionnaireRef : TFhirReference read FQuestionnaireRef write SetQuestionnaireRef;
    property contentList : TFslList<TFhirReference> read FContentList;
    property local : Boolean read FLocal write FLocal;
  end;

  TFHIRPopulatehtmlOpResponse = class (TFHIROperationResponse)
  private
    FForm : TFhirBinary;
    FIssues : TFhirOperationOutcome;
    procedure SetForm(value : TFhirBinary);
    procedure SetIssues(value : TFhirOperationOutcome);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property form : TFhirBinary read FForm write SetForm;
    property issues : TFhirOperationOutcome read FIssues write SetIssues;
  end;

  //Operation populatelink (Generate a link to a Questionnaire completion webpage)
  TFHIRPopulatelinkOpRequest = class (TFHIROperationRequest)
  private
    FIdentifier : String;
    FQuestionnaire : TFhirQuestionnaire;
    FQuestionnaireRef : TFhirReference;
    FContentList : TFslList<TFhirReference>;
    FLocal : Boolean;
    procedure SetQuestionnaire(value : TFhirQuestionnaire);
    procedure SetQuestionnaireRef(value : TFhirReference);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property identifier : String read FIdentifier write FIdentifier;
    property questionnaire : TFhirQuestionnaire read FQuestionnaire write SetQuestionnaire;
    property questionnaireRef : TFhirReference read FQuestionnaireRef write SetQuestionnaireRef;
    property contentList : TFslList<TFhirReference> read FContentList;
    property local : Boolean read FLocal write FLocal;
  end;

  TFHIRPopulatelinkOpResponse = class (TFHIROperationResponse)
  private
    FLink_ : String;
    FIssues : TFhirOperationOutcome;
    procedure SetIssues(value : TFhirOperationOutcome);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property link_ : String read FLink_ write FLink_;
    property issues : TFhirOperationOutcome read FIssues write SetIssues;
  end;

  //Operation meta (Access a list of profiles, tags, and security labels)
  TFHIRMetaOpRequest = class (TFHIROperationRequest)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  TFHIRMetaOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirMeta;
    procedure SetReturn(value : TFhirMeta);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirMeta read FReturn write SetReturn;
  end;

  //Operation meta-add (Add profiles, tags, and security labels to a resource)
  TFHIRMetaAddOpRequest = class (TFHIROperationRequest)
  private
    FMeta : TFhirMeta;
    procedure SetMeta(value : TFhirMeta);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property meta : TFhirMeta read FMeta write SetMeta;
  end;

  TFHIRMetaAddOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirMeta;
    procedure SetReturn(value : TFhirMeta);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirMeta read FReturn write SetReturn;
  end;

  //Operation meta-delete (Delete profiles, tags, and security labels for a resource)
  TFHIRMetaDeleteOpRequest = class (TFHIROperationRequest)
  private
    FMeta : TFhirMeta;
    procedure SetMeta(value : TFhirMeta);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property meta : TFhirMeta read FMeta write SetMeta;
  end;

  TFHIRMetaDeleteOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirMeta;
    procedure SetReturn(value : TFhirMeta);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirMeta read FReturn write SetReturn;
  end;

  //Operation validate (Validate a resource)
  TFHIRValidateOpRequest = class (TFHIROperationRequest)
  private
    FResource : TFhirResource;
    FMode : String;
    FProfile : String;
    procedure SetResource(value : TFhirResource);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property resource : TFhirResource read FResource write SetResource;
    property mode : String read FMode write FMode;
    property profile : String read FProfile write FProfile;
  end;

  TFHIRValidateOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirOperationOutcome;
    procedure SetReturn(value : TFhirOperationOutcome);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirOperationOutcome read FReturn write SetReturn;
  end;

  //Operation evaluate (Evaluate)
  TFHIREvaluateOpRequest = class (TFHIROperationRequest)
  private
    FRequestId : String;
    FEvaluateAtDateTime : TFslDateTime;
    FInputParameters : TFhirParameters;
    FInputDataList : TFslList<TFhirResource>;
    FPatient : TFhirReference;
    FEncounter : TFhirReference;
    FInitiatingOrganization : TFhirReference;
    FInitiatingPerson : TFhirReference;
    FUserType : TFhirCodeableConcept;
    FUserLanguage : TFhirCodeableConcept;
    FUserTaskContext : TFhirCodeableConcept;
    FReceivingOrganization : TFhirReference;
    FReceivingPerson : TFhirReference;
    FRecipientType : TFhirCodeableConcept;
    FRecipientLanguage : TFhirCodeableConcept;
    FSetting : TFhirCodeableConcept;
    FSettingContext : TFhirCodeableConcept;
    procedure SetInputParameters(value : TFhirParameters);
    procedure SetPatient(value : TFhirReference);
    procedure SetEncounter(value : TFhirReference);
    procedure SetInitiatingOrganization(value : TFhirReference);
    procedure SetInitiatingPerson(value : TFhirReference);
    procedure SetUserType(value : TFhirCodeableConcept);
    procedure SetUserLanguage(value : TFhirCodeableConcept);
    procedure SetUserTaskContext(value : TFhirCodeableConcept);
    procedure SetReceivingOrganization(value : TFhirReference);
    procedure SetReceivingPerson(value : TFhirReference);
    procedure SetRecipientType(value : TFhirCodeableConcept);
    procedure SetRecipientLanguage(value : TFhirCodeableConcept);
    procedure SetSetting(value : TFhirCodeableConcept);
    procedure SetSettingContext(value : TFhirCodeableConcept);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property requestId : String read FRequestId write FRequestId;
    property evaluateAtDateTime : TFslDateTime read FEvaluateAtDateTime write FEvaluateAtDateTime;
    property inputParameters : TFhirParameters read FInputParameters write SetInputParameters;
    property inputDataList : TFslList<TFhirResource> read FInputDataList;
    property patient : TFhirReference read FPatient write SetPatient;
    property encounter : TFhirReference read FEncounter write SetEncounter;
    property initiatingOrganization : TFhirReference read FInitiatingOrganization write SetInitiatingOrganization;
    property initiatingPerson : TFhirReference read FInitiatingPerson write SetInitiatingPerson;
    property userType : TFhirCodeableConcept read FUserType write SetUserType;
    property userLanguage : TFhirCodeableConcept read FUserLanguage write SetUserLanguage;
    property userTaskContext : TFhirCodeableConcept read FUserTaskContext write SetUserTaskContext;
    property receivingOrganization : TFhirReference read FReceivingOrganization write SetReceivingOrganization;
    property receivingPerson : TFhirReference read FReceivingPerson write SetReceivingPerson;
    property recipientType : TFhirCodeableConcept read FRecipientType write SetRecipientType;
    property recipientLanguage : TFhirCodeableConcept read FRecipientLanguage write SetRecipientLanguage;
    property setting : TFhirCodeableConcept read FSetting write SetSetting;
    property settingContext : TFhirCodeableConcept read FSettingContext write SetSettingContext;
  end;

  TFHIREvaluateOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirGuidanceResponse;
    procedure SetReturn(value : TFhirGuidanceResponse);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirGuidanceResponse read FReturn write SetReturn;
  end;

  //Operation questionnaire (Build Questionnaire)
  TFHIRQuestionnaireOpRequest = class (TFHIROperationRequest)
  private
    FIdentifier : String;
    FProfile : String;
    FUrl : String;
    FSupportedOnly : Boolean;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property identifier : String read FIdentifier write FIdentifier;
    property profile : String read FProfile write FProfile;
    property url : String read FUrl write FUrl;
    property supportedOnly : Boolean read FSupportedOnly write FSupportedOnly;
  end;

  TFHIRQuestionnaireOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirQuestionnaire;
    procedure SetReturn(value : TFhirQuestionnaire);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirQuestionnaire read FReturn write SetReturn;
  end;

  //Operation transform (Model Instance Transformation)
  TFHIRTransformOpRequest = class (TFHIROperationRequest)
  private
    FSource : String;
    FContent : TFhirResource;
    procedure SetContent(value : TFhirResource);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property source : String read FSource write FSource;
    property content : TFhirResource read FContent write SetContent;
  end;

  TFHIRTransformOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirResource;
    procedure SetReturn(value : TFhirResource);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirResource read FReturn write SetReturn;
  end;

  //Operation expand (Value Set Expansion)
  TFHIRExpandOpRequest = class (TFHIROperationRequest)
  private
    FUrl : String;
    FValueSet : TFhirValueSet;
    FContext : String;
    FFilter : String;
    FProfile : String;
    FDate : TFslDateTime;
    FOffset : String;
    FCount : String;
    FIncludeDesignations : Boolean;
    FIncludeDefinition : Boolean;
    FActiveOnly : Boolean;
    FExcludeNested : Boolean;
    FExcludeNotForUI : Boolean;
    FExcludePostCoordinated : Boolean;
    FDisplayLanguage : String;
    FLimitedExpansion : Boolean;
    procedure SetValueSet(value : TFhirValueSet);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property url : String read FUrl write FUrl;
    property valueSet : TFhirValueSet read FValueSet write SetValueSet;
    property context : String read FContext write FContext;
    property filter : String read FFilter write FFilter;
    property profile : String read FProfile write FProfile;
    property date : TFslDateTime read FDate write FDate;
    property offset : String read FOffset write FOffset;
    property count : String read FCount write FCount;
    property includeDesignations : Boolean read FIncludeDesignations write FIncludeDesignations;
    property includeDefinition : Boolean read FIncludeDefinition write FIncludeDefinition;
    property activeOnly : Boolean read FActiveOnly write FActiveOnly;
    property excludeNested : Boolean read FExcludeNested write FExcludeNested;
    property excludeNotForUI : Boolean read FExcludeNotForUI write FExcludeNotForUI;
    property excludePostCoordinated : Boolean read FExcludePostCoordinated write FExcludePostCoordinated;
    property displayLanguage : String read FDisplayLanguage write FDisplayLanguage;
    property limitedExpansion : Boolean read FLimitedExpansion write FLimitedExpansion;
  end;

  TFHIRExpandOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirValueSet;
    procedure SetReturn(value : TFhirValueSet);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirValueSet read FReturn write SetReturn;
  end;

  //Operation validate-code (Value Set based Validation)
  TFHIRValidateCodeOpRequest = class (TFHIROperationRequest)
  private
    FUrl : String;
    FContext : String;
    FValueSet : TFhirValueSet;
    FCode : String;
    FSystem : String;
    FVersion : String;
    FDisplay : String;
    FCoding : TFhirCoding;
    FCodeableConcept : TFhirCodeableConcept;
    FDate : TFslDateTime;
    FAbstract : Boolean;
    FDisplayLanguage : String;
    procedure SetValueSet(value : TFhirValueSet);
    procedure SetCoding(value : TFhirCoding);
    procedure SetCodeableConcept(value : TFhirCodeableConcept);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property url : String read FUrl write FUrl;
    property context : String read FContext write FContext;
    property valueSet : TFhirValueSet read FValueSet write SetValueSet;
    property code : String read FCode write FCode;
    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property display : String read FDisplay write FDisplay;
    property coding : TFhirCoding read FCoding write SetCoding;
    property codeableConcept : TFhirCodeableConcept read FCodeableConcept write SetCodeableConcept;
    property date : TFslDateTime read FDate write FDate;
    property abstract : Boolean read FAbstract write FAbstract;
    property displayLanguage : String read FDisplayLanguage write FDisplayLanguage;
  end;

  TFHIRValidateCodeOpResponse = class (TFHIROperationResponse)
  private
    FResult : Boolean;
    FMessage : String;
    FDisplay : String;
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
    function asParams : TFHIRParameters; override;
    property result : Boolean read FResult write FResult;
    property message : String read FMessage write FMessage;
    property display : String read FDisplay write FDisplay;
  end;

implementation

uses
  fhir3_utilities;

procedure TFHIRApplyOpRequest.SetPatient(value : TFhirReference);
begin
  FPatient.free;
  FPatient := value;
end;

procedure TFHIRApplyOpRequest.SetEncounter(value : TFhirReference);
begin
  FEncounter.free;
  FEncounter := value;
end;

procedure TFHIRApplyOpRequest.SetPractitioner(value : TFhirReference);
begin
  FPractitioner.free;
  FPractitioner := value;
end;

procedure TFHIRApplyOpRequest.SetOrganization(value : TFhirReference);
begin
  FOrganization.free;
  FOrganization := value;
end;

procedure TFHIRApplyOpRequest.SetUserType(value : TFhirCodeableConcept);
begin
  FUserType.free;
  FUserType := value;
end;

procedure TFHIRApplyOpRequest.SetUserLanguage(value : TFhirCodeableConcept);
begin
  FUserLanguage.free;
  FUserLanguage := value;
end;

procedure TFHIRApplyOpRequest.SetUserTaskContext(value : TFhirCodeableConcept);
begin
  FUserTaskContext.free;
  FUserTaskContext := value;
end;

procedure TFHIRApplyOpRequest.SetSetting(value : TFhirCodeableConcept);
begin
  FSetting.free;
  FSetting := value;
end;

procedure TFHIRApplyOpRequest.SetSettingContext(value : TFhirCodeableConcept);
begin
  FSettingContext.free;
  FSettingContext := value;
end;

constructor TFHIRApplyOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRApplyOpRequest.load(params : TFHIRParameters);
begin
  if params.param['patient'] <> nil then
    FPatient := (params.param['patient'].value as TFhirReference).Link; {ob.5d}
  if params.param['encounter'] <> nil then
    FEncounter := (params.param['encounter'].value as TFhirReference).Link; {ob.5d}
  if params.param['practitioner'] <> nil then
    FPractitioner := (params.param['practitioner'].value as TFhirReference).Link; {ob.5d}
  if params.param['organization'] <> nil then
    FOrganization := (params.param['organization'].value as TFhirReference).Link; {ob.5d}
  if params.param['userType'] <> nil then
    FUserType := (params.param['userType'].value as TFhirCodeableConcept).Link; {ob.5d}
  if params.param['userLanguage'] <> nil then
    FUserLanguage := (params.param['userLanguage'].value as TFhirCodeableConcept).Link; {ob.5d}
  if params.param['userTaskContext'] <> nil then
    FUserTaskContext := (params.param['userTaskContext'].value as TFhirCodeableConcept).Link; {ob.5d}
  if params.param['setting'] <> nil then
    FSetting := (params.param['setting'].value as TFhirCodeableConcept).Link; {ob.5d}
  if params.param['settingContext'] <> nil then
    FSettingContext := (params.param['settingContext'].value as TFhirCodeableConcept).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIRApplyOpRequest.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRApplyOpRequest.Destroy;
begin
  FPatient.free;
  FEncounter.free;
  FPractitioner.free;
  FOrganization.free;
  FUserType.free;
  FUserLanguage.free;
  FUserTaskContext.free;
  FSetting.free;
  FSettingContext.free;
  inherited;
end;

function TFHIRApplyOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FPatient <> nil) then
      result.addParameter('patient', FPatient.Link);{oz.5d}
    if (FEncounter <> nil) then
      result.addParameter('encounter', FEncounter.Link);{oz.5d}
    if (FPractitioner <> nil) then
      result.addParameter('practitioner', FPractitioner.Link);{oz.5d}
    if (FOrganization <> nil) then
      result.addParameter('organization', FOrganization.Link);{oz.5d}
    if (FUserType <> nil) then
      result.addParameter('userType', FUserType.Link);{oz.5d}
    if (FUserLanguage <> nil) then
      result.addParameter('userLanguage', FUserLanguage.Link);{oz.5d}
    if (FUserTaskContext <> nil) then
      result.addParameter('userTaskContext', FUserTaskContext.Link);{oz.5d}
    if (FSetting <> nil) then
      result.addParameter('setting', FSetting.Link);{oz.5d}
    if (FSettingContext <> nil) then
      result.addParameter('settingContext', FSettingContext.Link);{oz.5d}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRApplyOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['patient', 'encounter', 'practitioner', 'organization', 'userType', 'userLanguage', 'userTaskContext', 'setting', 'settingContext'], name);
end;

function TFHIRApplyOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPatient.sizeInBytes);
  inc(result, FEncounter.sizeInBytes);
  inc(result, FPractitioner.sizeInBytes);
  inc(result, FOrganization.sizeInBytes);
  inc(result, FUserType.sizeInBytes);
  inc(result, FUserLanguage.sizeInBytes);
  inc(result, FUserTaskContext.sizeInBytes);
  inc(result, FSetting.sizeInBytes);
  inc(result, FSettingContext.sizeInBytes);
end;

procedure TFHIRApplyOpResponse.SetReturn(value : TFhirResource);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIRApplyOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRApplyOpResponse.load(params : TFHIRParameters);
begin
  FReturn := (params.res['return'] as TFhirResource).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRApplyOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRApplyOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRApplyOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRApplyOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
end;

function TFHIRApplyOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturn.sizeInBytes);
end;

constructor TFHIRDataRequirementsOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRDataRequirementsOpRequest.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRDataRequirementsOpRequest.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRDataRequirementsOpRequest.Destroy;
begin
  inherited;
end;

function TFHIRDataRequirementsOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRDataRequirementsOpRequest.isKnownName(name : String) : boolean;
begin
  result := false;
end;

procedure TFHIRDataRequirementsOpResponse.SetReturn(value : TFhirLibrary);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIRDataRequirementsOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRDataRequirementsOpResponse.load(params : TFHIRParameters);
begin
  FReturn := (params.res['return'] as TFhirLibrary).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRDataRequirementsOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRDataRequirementsOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRDataRequirementsOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRDataRequirementsOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
end;

function TFHIRDataRequirementsOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturn.sizeInBytes);
end;

constructor TFHIRConformsOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRConformsOpRequest.load(params : TFHIRParameters);
begin
  FLeft := params.str['left'];
  FRight := params.str['right'];
  FMode := params.str['mode'];
  loadExtensions(params);
end;

procedure TFHIRConformsOpRequest.load(params : THTTPParameters);
begin
  FLeft := params['left'];
  FRight := params['right'];
  FMode := params['mode'];
  loadExtensions(params);
end;

destructor TFHIRConformsOpRequest.Destroy;
begin
  inherited;
end;

function TFHIRConformsOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FLeft <> '') then
      result.addParameter('left', TFHIRUri.create(FLeft));{oz.5f}
    if (FRight <> '') then
      result.addParameter('right', TFHIRUri.create(FRight));{oz.5f}
    if (FMode <> '') then
      result.addParameter('mode', TFHIRCode.create(FMode));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRConformsOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['left', 'right', 'mode'], name);
end;

function TFHIRConformsOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FLeft.length * sizeof(char)) + 12);
  inc(result, (FRight.length * sizeof(char)) + 12);
  inc(result, (FMode.length * sizeof(char)) + 12);
end;

procedure TFHIRConformsOpResponse.SetIssues(value : TFhirOperationOutcome);
begin
  FIssues.free;
  FIssues := value;
end;

procedure TFHIRConformsOpResponse.SetUnion(value : TFhirCapabilityStatement);
begin
  FUnion.free;
  FUnion := value;
end;

procedure TFHIRConformsOpResponse.SetIntersection(value : TFhirCapabilityStatement);
begin
  FIntersection.free;
  FIntersection := value;
end;

constructor TFHIRConformsOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRConformsOpResponse.load(params : TFHIRParameters);
begin
  FIssues := (params.res['issues'] as TFhirOperationOutcome).Link;{ob.5a}
  FUnion := (params.res['union'] as TFhirCapabilityStatement).Link;{ob.5a}
  FIntersection := (params.res['intersection'] as TFhirCapabilityStatement).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRConformsOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRConformsOpResponse.Destroy;
begin
  FIssues.free;
  FUnion.free;
  FIntersection.free;
  inherited;
end;

function TFHIRConformsOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FIssues <> nil) then
      result.addParameter('issues', FIssues.Link);{oz.5a}
    if (FUnion <> nil) then
      result.addParameter('union', FUnion.Link);{oz.5a}
    if (FIntersection <> nil) then
      result.addParameter('intersection', FIntersection.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRConformsOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['issues', 'union', 'intersection'], name);
end;

function TFHIRConformsOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FIssues.sizeInBytes);
  inc(result, FUnion.sizeInBytes);
  inc(result, FIntersection.sizeInBytes);
end;

procedure TFHIRImplementsOpRequest.SetResource(value : TFhirCapabilityStatement);
begin
  FResource.free;
  FResource := value;
end;

constructor TFHIRImplementsOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRImplementsOpRequest.load(params : TFHIRParameters);
begin
  FServer := params.str['server'];
  FClient := params.str['client'];
  FResource := (params.res['resource'] as TFhirCapabilityStatement).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRImplementsOpRequest.load(params : THTTPParameters);
begin
  FServer := params['server'];
  FClient := params['client'];
  loadExtensions(params);
end;

destructor TFHIRImplementsOpRequest.Destroy;
begin
  FResource.free;
  inherited;
end;

function TFHIRImplementsOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FServer <> '') then
      result.addParameter('server', TFHIRUri.create(FServer));{oz.5f}
    if (FClient <> '') then
      result.addParameter('client', TFHIRUri.create(FClient));{oz.5f}
    if (FResource <> nil) then
      result.addParameter('resource', FResource.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRImplementsOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['server', 'client', 'resource'], name);
end;

function TFHIRImplementsOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FServer.length * sizeof(char)) + 12);
  inc(result, (FClient.length * sizeof(char)) + 12);
  inc(result, FResource.sizeInBytes);
end;

procedure TFHIRImplementsOpResponse.SetReturn(value : TFhirOperationOutcome);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIRImplementsOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRImplementsOpResponse.load(params : TFHIRParameters);
begin
  FReturn := (params.res['return'] as TFhirOperationOutcome).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRImplementsOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRImplementsOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRImplementsOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRImplementsOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
end;

function TFHIRImplementsOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturn.sizeInBytes);
end;

constructor TFHIRSubsetOpRequest.create;
begin
  inherited create();
  FResourceList := TStringList.create;
end;

procedure TFHIRSubsetOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FServer := params.str['server'];
  for p in params.parameterList do
    if p.name = 'resource' then
      FResourceList.Add((p.value as TFhirCode).value);{ob.1}
  loadExtensions(params);
end;

procedure TFHIRSubsetOpRequest.load(params : THTTPParameters);
var
  s : String;
begin
  FServer := params['server'];
  for s in params['resource'].Split([';']) do
    FResourceList.add(s);
  loadExtensions(params);
end;

destructor TFHIRSubsetOpRequest.Destroy;
begin
  FResourceList.free;
  inherited;
end;

function TFHIRSubsetOpRequest.asParams : TFhirParameters;
var
  v1 : String;
begin
  result := TFHIRParameters.create;
  try
    if (FServer <> '') then
      result.addParameter('server', TFHIRUri.create(FServer));{oz.5f}
    for v1 in FResourceList do
      result.AddParameter('resource', TFhirCode.create(v1));
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRSubsetOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['server', 'resource'], name);
end;

function TFHIRSubsetOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FServer.length * sizeof(char)) + 12);
  inc(result, FResourceList.sizeInBytes);
end;

procedure TFHIRSubsetOpResponse.SetReturn(value : TFhirCapabilityStatement);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIRSubsetOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRSubsetOpResponse.load(params : TFHIRParameters);
begin
  FReturn := (params.res['return'] as TFhirCapabilityStatement).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRSubsetOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRSubsetOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRSubsetOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRSubsetOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
end;

function TFHIRSubsetOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturn.sizeInBytes);
end;

constructor TFHIRComposeOpReqSubproperty.create;
begin
  inherited create();
end;

constructor TFHIRComposeOpReqSubproperty.create(params : TFhirParametersParameter);
begin
  inherited create();
  FCode := params.str['code'];
  FValue := params.str['value'];
  loadExtensions(params);
end;

destructor TFHIRComposeOpReqSubproperty.Destroy;
begin
  inherited;
end;

function TFHIRComposeOpReqSubproperty.asParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FCode <> '') then
      result.addParameter('code', TFHIRCode.create(FCode));{oz.5f}
    if (FValue <> '') then
      result.addParameter('value', TFHIRCode.create(FValue));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRComposeOpReqSubproperty.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['code', 'value'], name);
end;

function TFHIRComposeOpReqSubproperty.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCode.length * sizeof(char)) + 12);
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

constructor TFHIRComposeOpReqProperty_.create;
begin
  inherited create();
  FSubpropertyList := TFslList<TFHIRComposeOpReqSubproperty>.create;
end;

constructor TFHIRComposeOpReqProperty_.create(params : TFhirParametersParameter);
var
  p : TFhirParametersParameter;
begin
  inherited create();
  FSubpropertyList := TFslList<TFHIRComposeOpReqSubproperty>.create;
  FCode := params.str['code'];
  FValue := params.str['value'];
  for p in params.partList do
    if p.name = 'subproperty' then
      FSubpropertyList.Add(TFHIRComposeOpReqSubproperty.create(p));{a}
  loadExtensions(params);
end;

destructor TFHIRComposeOpReqProperty_.Destroy;
begin
  FSubpropertyList.free;
  inherited;
end;

function TFHIRComposeOpReqProperty_.asParams(name : String) : TFhirParametersParameter;
var
  v1 : TFHIRComposeOpReqSubproperty;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FCode <> '') then
      result.addParameter('code', TFHIRCode.create(FCode));{oz.5f}
    if (FValue <> '') then
      result.addParameter('value', TFHIRCode.create(FValue));{oz.5f}
    for v1 in FSubpropertyList do
      result.AddParameter(v1.asParams('subproperty'));
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRComposeOpReqProperty_.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['code', 'value', 'subproperty'], name);
end;

function TFHIRComposeOpReqProperty_.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCode.length * sizeof(char)) + 12);
  inc(result, (FValue.length * sizeof(char)) + 12);
  inc(result, FSubpropertyList.sizeInBytes);
end;

constructor TFHIRComposeOpRequest.create;
begin
  inherited create();
  FProperty_List := TFslList<TFHIRComposeOpReqProperty_>.create;
end;

procedure TFHIRComposeOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FSystem := params.str['system'];
  FVersion := params.str['version'];
  for p in params.parameterList do
    if p.name = 'property' then
      FProperty_List.Add(TFHIRComposeOpReqProperty_.create(p));{a}
  FExact := params.bool['exact'];
  FCompositional := params.bool['compositional'];
  loadExtensions(params);
end;

procedure TFHIRComposeOpRequest.load(params : THTTPParameters);
begin
  FSystem := params['system'];
  FVersion := params['version'];
  FExact := StrToBoolDef(params['exact'], false);
  FCompositional := StrToBoolDef(params['compositional'], false);
  loadExtensions(params);
end;

destructor TFHIRComposeOpRequest.Destroy;
begin
  FProperty_List.free;
  inherited;
end;

function TFHIRComposeOpRequest.asParams : TFhirParameters;
var
  v1 : TFHIRComposeOpReqProperty_;
begin
  result := TFHIRParameters.create;
  try
    if (FSystem <> '') then
      result.addParameter('system', TFHIRUri.create(FSystem));{oz.5f}
    if (FVersion <> '') then
      result.addParameter('version', TFHIRString.create(FVersion));{oz.5f}
    for v1 in FProperty_List do
      result.AddParameter(v1.asParams('property'));
      result.addParameter('exact', TFHIRBoolean.create(FExact));{oz.5f}
      result.addParameter('compositional', TFHIRBoolean.create(FCompositional));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRComposeOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['system', 'version', 'property', 'exact', 'compositional'], name);
end;

function TFHIRComposeOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FSystem.length * sizeof(char)) + 12);
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, FProperty_List.sizeInBytes);
end;

procedure TFHIRComposeOpRespMatch.SetCode(value : TFhirCoding);
begin
  FCode.free;
  FCode := value;
end;

function TFHIRComposeOpRespMatch.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCode.sizeInBytes);
  inc(result, FUnmatchedList.sizeInBytes);
  inc(result, (FComment.length * sizeof(char)) + 12);
end;

constructor TFHIRComposeOpRespProperty_.create;
begin
  inherited create();
end;

constructor TFHIRComposeOpRespProperty_.create(params : TFhirParametersParameter);
begin
  inherited create();
  FCode := params.str['code'];
  FValue := params.str['value'];
  loadExtensions(params);
end;

destructor TFHIRComposeOpRespProperty_.Destroy;
begin
  inherited;
end;

function TFHIRComposeOpRespProperty_.asParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FCode <> '') then
      result.addParameter('code', TFHIRCode.create(FCode));{oz.5f}
    if (FValue <> '') then
      result.addParameter('value', TFHIRCode.create(FValue));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRComposeOpRespProperty_.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['code', 'value'], name);
end;

function TFHIRComposeOpRespProperty_.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCode.length * sizeof(char)) + 12);
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

constructor TFHIRComposeOpRespUnmatched.create;
begin
  inherited create();
  FProperty_List := TFslList<TFHIRComposeOpRespProperty_>.create;
end;

constructor TFHIRComposeOpRespUnmatched.create(params : TFhirParametersParameter);
var
  p : TFhirParametersParameter;
begin
  inherited create();
  FProperty_List := TFslList<TFHIRComposeOpRespProperty_>.create;
  FCode := params.str['code'];
  FValue := params.str['value'];
  for p in params.partList do
    if p.name = 'property' then
      FProperty_List.Add(TFHIRComposeOpRespProperty_.create(p));{a}
  loadExtensions(params);
end;

destructor TFHIRComposeOpRespUnmatched.Destroy;
begin
  FProperty_List.free;
  inherited;
end;

function TFHIRComposeOpRespUnmatched.asParams(name : String) : TFhirParametersParameter;
var
  v1 : TFHIRComposeOpRespProperty_;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FCode <> '') then
      result.addParameter('code', TFHIRCode.create(FCode));{oz.5f}
    if (FValue <> '') then
      result.addParameter('value', TFHIRCode.create(FValue));{oz.5f}
    for v1 in FProperty_List do
      result.AddParameter(v1.asParams('property'));
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRComposeOpRespUnmatched.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['code', 'value', 'property'], name);
end;

function TFHIRComposeOpRespUnmatched.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCode.length * sizeof(char)) + 12);
  inc(result, (FValue.length * sizeof(char)) + 12);
  inc(result, FProperty_List.sizeInBytes);
end;

constructor TFHIRComposeOpRespMatch.create;
begin
  inherited create();
  FUnmatchedList := TFslList<TFHIRComposeOpRespUnmatched>.create;
end;

constructor TFHIRComposeOpRespMatch.create(params : TFhirParametersParameter);
var
  p : TFhirParametersParameter;
begin
  inherited create();
  FUnmatchedList := TFslList<TFHIRComposeOpRespUnmatched>.create;
  if params.param['code'] <> nil then
    FCode := (params.param['code'].value as TFhirCoding).Link; {ob.5d}
  for p in params.partList do
    if p.name = 'unmatched' then
      FUnmatchedList.Add(TFHIRComposeOpRespUnmatched.create(p));{a}
  FComment := params.str['comment'];
  loadExtensions(params);
end;

destructor TFHIRComposeOpRespMatch.Destroy;
begin
  FCode.free;
  FUnmatchedList.free;
  inherited;
end;

function TFHIRComposeOpRespMatch.asParams(name : String) : TFhirParametersParameter;
var
  v1 : TFHIRComposeOpRespUnmatched;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FCode <> nil) then
      result.addParameter('code', FCode.Link);{oz.5d}
    for v1 in FUnmatchedList do
      result.AddParameter(v1.asParams('unmatched'));
    if (FComment <> '') then
      result.addParameter('comment', TFHIRString.create(FComment));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRComposeOpRespMatch.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['code', 'unmatched', 'comment'], name);
end;

constructor TFHIRComposeOpResponse.create;
begin
  inherited create();
  FMatchList := TFslList<TFHIRComposeOpRespMatch>.create;
end;

procedure TFHIRComposeOpResponse.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  for p in params.parameterList do
    if p.name = 'match' then
      FMatchList.Add(TFHIRComposeOpRespMatch.create(p));{a}
  loadExtensions(params);
end;

procedure TFHIRComposeOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRComposeOpResponse.Destroy;
begin
  FMatchList.free;
  inherited;
end;

function TFHIRComposeOpResponse.asParams : TFhirParameters;
var
  v1 : TFHIRComposeOpRespMatch;
begin
  result := TFHIRParameters.create;
  try
    for v1 in FMatchList do
      result.AddParameter(v1.asParams('match'));
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRComposeOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['match'], name);
end;

function TFHIRComposeOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FMatchList.sizeInBytes);
end;

procedure TFHIRLookupOpRequest.SetCoding(value : TFhirCoding);
begin
  FCoding.free;
  FCoding := value;
end;

constructor TFHIRLookupOpRequest.create;
begin
  inherited create();
  FProperty_List := TStringList.create;
end;

procedure TFHIRLookupOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FCode := params.str['code'];
  FSystem := params.str['system'];
  FVersion := params.str['version'];
  if params.param['coding'] <> nil then
    FCoding := (params.param['coding'].value as TFhirCoding).Link; {ob.5d}
  FDate := TFslDateTime.fromXml(params.str['date']);
  FDisplayLanguage := params.str['displayLanguage'];
  for p in params.parameterList do
    if p.name = 'property' then
      FProperty_List.Add((p.value as TFhirCode).value);{ob.1}
  loadExtensions(params);
end;

procedure TFHIRLookupOpRequest.load(params : THTTPParameters);
var
  s : String;
begin
  FCode := params['code'];
  FSystem := params['system'];
  FVersion := params['version'];
  FDate := TFslDateTime.fromXml(params['date']);
  FDisplayLanguage := params['displayLanguage'];
  for s in params['property'].Split([';']) do
    FProperty_List.add(s); 
  loadExtensions(params);
end;

destructor TFHIRLookupOpRequest.Destroy;
begin
  FCoding.free;
  FProperty_List.free;
  inherited;
end;

function TFHIRLookupOpRequest.asParams : TFhirParameters;
var
  v1 : String;
begin
  result := TFHIRParameters.create;
  try
    if (FCode <> '') then
      result.addParameter('code', TFHIRCode.create(FCode));{oz.5f}
    if (FSystem <> '') then
      result.addParameter('system', TFHIRUri.create(FSystem));{oz.5f}
    if (FVersion <> '') then
      result.addParameter('version', TFHIRString.create(FVersion));{oz.5f}
    if (FCoding <> nil) then
      result.addParameter('coding', FCoding.Link);{oz.5d}
    if (FDate.notNull) then
      result.addParameter('date', TFHIRDateTime.create(FDate));{oz.5f}
    if (FDisplayLanguage <> '') then
      result.addParameter('displayLanguage', TFHIRCode.create(FDisplayLanguage));{oz.5f}
    for v1 in FProperty_List do
      result.AddParameter('property', TFhirCode.create(v1));
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRLookupOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['code', 'system', 'version', 'coding', 'date', 'displayLanguage', 'property'], name);
end;

function TFHIRLookupOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCode.length * sizeof(char)) + 12);
  inc(result, (FSystem.length * sizeof(char)) + 12);
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, FCoding.sizeInBytes);
  inc(result, (FDisplayLanguage.length * sizeof(char)) + 12);
  inc(result, FProperty_List.sizeInBytes);
end;

procedure TFHIRLookupOpRespDesignation.SetUse(value : TFhirCoding);
begin
  FUse.free;
  FUse := value;
end;

constructor TFHIRLookupOpRespDesignation.create;
begin
  inherited create();
end;

constructor TFHIRLookupOpRespDesignation.create(params : TFhirParametersParameter);
begin
  inherited create();
  FLanguage := params.str['language'];
  if params.param['use'] <> nil then
    FUse := (params.param['use'].value as TFhirCoding).Link; {ob.5d}
  FValue := params.str['value'];
  loadExtensions(params);
end;

destructor TFHIRLookupOpRespDesignation.Destroy;
begin
  FUse.free;
  inherited;
end;

function TFHIRLookupOpRespDesignation.asParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FLanguage <> '') then
      result.addParameter('language', TFHIRCode.create(FLanguage));{oz.5f}
    if (FUse <> nil) then
      result.addParameter('use', FUse.Link);{oz.5d}
    if (FValue <> '') then
      result.addParameter('value', TFHIRString.create(FValue));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRLookupOpRespDesignation.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['language', 'use', 'value'], name);
end;

function TFHIRLookupOpRespDesignation.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FLanguage.length * sizeof(char)) + 12);
  inc(result, FUse.sizeInBytes);
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

constructor TFHIRLookupOpRespSubproperty.create;
begin
  inherited create();
end;

constructor TFHIRLookupOpRespSubproperty.create(params : TFhirParametersParameter);
begin
  inherited create();
  FCode := params.str['code'];
  FValue := params.str['value'];
  FDescription := params.str['description'];
  loadExtensions(params);
end;

destructor TFHIRLookupOpRespSubproperty.Destroy;
begin
  inherited;
end;

function TFHIRLookupOpRespSubproperty.asParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FCode <> '') then
      result.addParameter('code', TFHIRCode.create(FCode));{oz.5f}
    if (FValue <> '') then
      result.addParameter('value', TFHIRCode.create(FValue));{oz.5f}
    if (FDescription <> '') then
      result.addParameter('description', TFHIRString.create(FDescription));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRLookupOpRespSubproperty.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['code', 'value', 'description'], name);
end;

function TFHIRLookupOpRespSubproperty.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCode.length * sizeof(char)) + 12);
  inc(result, (FValue.length * sizeof(char)) + 12);
  inc(result, (FDescription.length * sizeof(char)) + 12);
end;

constructor TFHIRLookupOpRespProperty_.create;
begin
  inherited create();
  FSubpropertyList := TFslList<TFHIRLookupOpRespSubproperty>.create;
end;

constructor TFHIRLookupOpRespProperty_.create(params : TFhirParametersParameter);
var
  p : TFhirParametersParameter;
begin
  inherited create();
  FSubpropertyList := TFslList<TFHIRLookupOpRespSubproperty>.create;
  FCode := params.str['code'];
  if params.hasParameter('value') then
    FValue := params.param['value'].value.Link;
  FDescription := params.str['description'];
  for p in params.partList do
    if p.name = 'subproperty' then
      FSubpropertyList.Add(TFHIRLookupOpRespSubproperty.create(p));{a}
  loadExtensions(params);
end;

destructor TFHIRLookupOpRespProperty_.Destroy;
begin
  FValue.Free;
  FSubpropertyList.free;
  inherited;
end;

function TFHIRLookupOpRespProperty_.asParams(name : String) : TFhirParametersParameter;
var
  v1 : TFHIRLookupOpRespSubproperty;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FCode <> '') then
      result.addParameter('code', TFHIRCode.create(FCode));{oz.5f}
    if (FValue <> nil) then
      result.addParameter('value', FValue.Link);{oz.5f}
    if (FDescription <> '') then
      result.addParameter('description', TFHIRString.create(FDescription));{oz.5f}
    for v1 in FSubpropertyList do
      result.AddParameter(v1.asParams('subproperty'));
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRLookupOpRespProperty_.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['code', 'value', 'description', 'subproperty'], name);
end;

procedure TFHIRLookupOpRespProperty_.SetValue(const Value: TFHIRType);
begin
  FValue.Free;
  FValue := Value;
end;

function TFHIRLookupOpRespProperty_.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCode.length * sizeof(char)) + 12);
  inc(result, FValue.sizeInBytes);
  inc(result, (FDescription.length * sizeof(char)) + 12);
  inc(result, FSubpropertyList.sizeInBytes);
end;

constructor TFHIRLookupOpResponse.create;
begin
  inherited create();
  FDesignationList := TFslList<TFHIRLookupOpRespDesignation>.create;
  FProperty_List := TFslList<TFHIRLookupOpRespProperty_>.create;
end;

procedure TFHIRLookupOpResponse.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FName := params.str['name'];
  FVersion := params.str['version'];
  FDisplay := params.str['display'];
  for p in params.parameterList do
    if p.name = 'designation' then
      FDesignationList.Add(TFHIRLookupOpRespDesignation.create(p));{a}
  for p in params.parameterList do
    if p.name = 'property' then
      FProperty_List.Add(TFHIRLookupOpRespProperty_.create(p));{a}
  loadExtensions(params);
end;

procedure TFHIRLookupOpResponse.load(params : THTTPParameters);
begin
  FName := params['name'];
  FVersion := params['version'];
  FDisplay := params['display'];
  loadExtensions(params);
end;

destructor TFHIRLookupOpResponse.Destroy;
begin
  FDesignationList.free;
  FProperty_List.free;
  inherited;
end;

function TFHIRLookupOpResponse.asParams : TFhirParameters;
var
  v1 : TFHIRLookupOpRespDesignation;
  v2 : TFHIRLookupOpRespProperty_;
begin
  result := TFHIRParameters.create;
  try
    if (FName <> '') then
      result.addParameter('name', TFHIRString.create(FName));{oz.5f}
    if (FVersion <> '') then
      result.addParameter('version', TFHIRString.create(FVersion));{oz.5f}
    if (FDisplay <> '') then
      result.addParameter('display', TFHIRString.create(FDisplay));{oz.5f}
    for v1 in FDesignationList do
      result.AddParameter(v1.asParams('designation'));
    for v2 in FProperty_List do
      result.AddParameter(v2.asParams('property'));
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRLookupOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['name', 'version', 'display', 'designation', 'property'], name);
end;

function TFHIRLookupOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, (FDisplay.length * sizeof(char)) + 12);
  inc(result, FDesignationList.sizeInBytes);
  inc(result, FProperty_List.sizeInBytes);
end;

procedure TFHIRSubsumesOpRequest.SetCodingA(value : TFhirCoding);
begin
  FCodingA.free;
  FCodingA := value;
end;

procedure TFHIRSubsumesOpRequest.SetCodingB(value : TFhirCoding);
begin
  FCodingB.free;
  FCodingB := value;
end;

constructor TFHIRSubsumesOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRSubsumesOpRequest.load(params : TFHIRParameters);
begin
  FCodeA := params.str['codeA'];
  FCodeB := params.str['codeB'];
  FSystem := params.str['system'];
  FVersion := params.str['version'];
  if params.param['codingA'] <> nil then
    FCodingA := (params.param['codingA'].value as TFhirCoding).Link; {ob.5d}
  if params.param['codingB'] <> nil then
    FCodingB := (params.param['codingB'].value as TFhirCoding).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIRSubsumesOpRequest.load(params : THTTPParameters);
begin
  FCodeA := params['codeA'];
  FCodeB := params['codeB'];
  FSystem := params['system'];
  FVersion := params['version'];
  loadExtensions(params);
end;

destructor TFHIRSubsumesOpRequest.Destroy;
begin
  FCodingA.free;
  FCodingB.free;
  inherited;
end;

function TFHIRSubsumesOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FCodeA <> '') then
      result.addParameter('codeA', TFHIRCode.create(FCodeA));{oz.5f}
    if (FCodeB <> '') then
      result.addParameter('codeB', TFHIRCode.create(FCodeB));{oz.5f}
    if (FSystem <> '') then
      result.addParameter('system', TFHIRUri.create(FSystem));{oz.5f}
    if (FVersion <> '') then
      result.addParameter('version', TFHIRString.create(FVersion));{oz.5f}
    if (FCodingA <> nil) then
      result.addParameter('codingA', FCodingA.Link);{oz.5d}
    if (FCodingB <> nil) then
      result.addParameter('codingB', FCodingB.Link);{oz.5d}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRSubsumesOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['codeA', 'codeB', 'system', 'version', 'codingA', 'codingB'], name);
end;

function TFHIRSubsumesOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCodeA.length * sizeof(char)) + 12);
  inc(result, (FCodeB.length * sizeof(char)) + 12);
  inc(result, (FSystem.length * sizeof(char)) + 12);
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, FCodingA.sizeInBytes);
  inc(result, FCodingB.sizeInBytes);
end;

constructor TFHIRSubsumesOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRSubsumesOpResponse.load(params : TFHIRParameters);
begin
  FOutcome := params.str['outcome'];
  loadExtensions(params);
end;

procedure TFHIRSubsumesOpResponse.load(params : THTTPParameters);
begin
  FOutcome := params['outcome'];
  loadExtensions(params);
end;

destructor TFHIRSubsumesOpResponse.Destroy;
begin
  inherited;
end;

function TFHIRSubsumesOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FOutcome <> '') then
      result.addParameter('outcome', TFHIRCode.create(FOutcome));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRSubsumesOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['outcome'], name);
end;

function TFHIRSubsumesOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FOutcome.length * sizeof(char)) + 12);
end;

constructor TFHIRDocumentOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRDocumentOpRequest.load(params : TFHIRParameters);
begin
  FPersist := params.bool['persist'];
  loadExtensions(params);
end;

procedure TFHIRDocumentOpRequest.load(params : THTTPParameters);
begin
  FPersist := StrToBoolDef(params['persist'], false);
  loadExtensions(params);
end;

destructor TFHIRDocumentOpRequest.Destroy;
begin
  inherited;
end;

function TFHIRDocumentOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
      result.addParameter('persist', TFHIRBoolean.create(FPersist));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRDocumentOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['persist'], name);
end;

function TFHIRDocumentOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

constructor TFHIRDocumentOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRDocumentOpResponse.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRDocumentOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRDocumentOpResponse.Destroy;
begin
  inherited;
end;

function TFHIRDocumentOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRDocumentOpResponse.isKnownName(name : String) : boolean;
begin
  result := false;
end;

constructor TFHIRClosureOpRequest.create;
begin
  inherited create();
  FConceptList := TFslList<TFhirCoding>.create;
end;

procedure TFHIRClosureOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FName := params.str['name'];
  for p in params.parameterList do
    if p.name = 'concept' then
      FConceptList.Add((p.value as TFhirCoding).Link);{a}
  FVersion := params.str['version'];
  loadExtensions(params);
end;

procedure TFHIRClosureOpRequest.load(params : THTTPParameters);
begin
  FName := params['name'];
  FVersion := params['version'];
  loadExtensions(params);
end;

destructor TFHIRClosureOpRequest.Destroy;
begin
  FConceptList.free;
  inherited;
end;

function TFHIRClosureOpRequest.asParams : TFhirParameters;
var
  v1 : TFhirCoding;
begin
  result := TFHIRParameters.create;
  try
    if (FName <> '') then
      result.addParameter('name', TFHIRString.create(FName));{oz.5f}
    for v1 in FConceptList do
      result.AddParameter('concept', v1.Link);
    if (FVersion <> '') then
      result.addParameter('version', TFHIRId.create(FVersion));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRClosureOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['name', 'concept', 'version'], name);
end;

function TFHIRClosureOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FConceptList.sizeInBytes);
  inc(result, (FVersion.length * sizeof(char)) + 12);
end;

procedure TFHIRClosureOpResponse.SetReturn(value : TFhirConceptMap);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIRClosureOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRClosureOpResponse.load(params : TFHIRParameters);
begin
  FReturn := (params.res['return'] as TFhirConceptMap).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRClosureOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRClosureOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRClosureOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRClosureOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
end;

function TFHIRClosureOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturn.sizeInBytes);
end;

procedure TFHIRTranslateOpRequest.SetCoding(value : TFhirCoding);
begin
  FCoding.free;
  FCoding := value;
end;

procedure TFHIRTranslateOpRequest.SetCodeableConcept(value : TFhirCodeableConcept);
begin
  FCodeableConcept.free;
  FCodeableConcept := value;
end;

function TFHIRTranslateOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCode.length * sizeof(char)) + 12);
  inc(result, (FSystem.length * sizeof(char)) + 12);
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, (FSource.length * sizeof(char)) + 12);
  inc(result, FCoding.sizeInBytes);
  inc(result, FCodeableConcept.sizeInBytes);
  inc(result, (FTarget.length * sizeof(char)) + 12);
  inc(result, (FTargetsystem.length * sizeof(char)) + 12);
  inc(result, FDependencyList.sizeInBytes);
end;

procedure TFHIRTranslateOpReqDependency.SetConcept(value : TFhirCodeableConcept);
begin
  FConcept.free;
  FConcept := value;
end;

constructor TFHIRTranslateOpReqDependency.create;
begin
  inherited create();
end;

constructor TFHIRTranslateOpReqDependency.create(params : TFhirParametersParameter);
begin
  inherited create();
  FElement := params.str['element'];
  if params.param['concept'] <> nil then
    FConcept := (params.param['concept'].value as TFhirCodeableConcept).Link; {ob.5d}
  loadExtensions(params);
end;

destructor TFHIRTranslateOpReqDependency.Destroy;
begin
  FConcept.free;
  inherited;
end;

function TFHIRTranslateOpReqDependency.asParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FElement <> '') then
      result.addParameter('element', TFHIRUri.create(FElement));{oz.5f}
    if (FConcept <> nil) then
      result.addParameter('concept', FConcept.Link);{oz.5d}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRTranslateOpReqDependency.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['element', 'concept'], name);
end;

function TFHIRTranslateOpReqDependency.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FElement.length * sizeof(char)) + 12);
  inc(result, FConcept.sizeInBytes);
end;

constructor TFHIRTranslateOpRequest.create;
begin
  inherited create();
  FDependencyList := TFslList<TFHIRTranslateOpReqDependency>.create;
end;

procedure TFHIRTranslateOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FCode := params.str['code'];
  FSystem := params.str['system'];
  FVersion := params.str['version'];
  FSource := params.str['source'];
  if params.param['coding'] <> nil then
    FCoding := (params.param['coding'].value as TFhirCoding).Link; {ob.5d}
  if params.param['codeableConcept'] <> nil then
    FCodeableConcept := (params.param['codeableConcept'].value as TFhirCodeableConcept).Link; {ob.5d}
  FTarget := params.str['target'];
  FTargetsystem := params.str['targetsystem'];
  for p in params.parameterList do
    if p.name = 'dependency' then
      FDependencyList.Add(TFHIRTranslateOpReqDependency.create(p));{a}
  FReverse := params.bool['reverse'];
  loadExtensions(params);
end;

procedure TFHIRTranslateOpRequest.load(params : THTTPParameters);
begin
  FCode := params['code'];
  FSystem := params['system'];
  FVersion := params['version'];
  FSource := params['source'];
  FTarget := params['target'];
  FTargetsystem := params['targetsystem'];
  FReverse := StrToBoolDef(params['reverse'], false);
  loadExtensions(params);
end;

destructor TFHIRTranslateOpRequest.Destroy;
begin
  FCoding.free;
  FCodeableConcept.free;
  FDependencyList.free;
  inherited;
end;

function TFHIRTranslateOpRequest.asParams : TFhirParameters;
var
  v1 : TFHIRTranslateOpReqDependency;
begin
  result := TFHIRParameters.create;
  try
    if (FCode <> '') then
      result.addParameter('code', TFHIRCode.create(FCode));{oz.5f}
    if (FSystem <> '') then
      result.addParameter('system', TFHIRUri.create(FSystem));{oz.5f}
    if (FVersion <> '') then
      result.addParameter('version', TFHIRString.create(FVersion));{oz.5f}
    if (FSource <> '') then
      result.addParameter('source', TFHIRUri.create(FSource));{oz.5f}
    if (FCoding <> nil) then
      result.addParameter('coding', FCoding.Link);{oz.5d}
    if (FCodeableConcept <> nil) then
      result.addParameter('codeableConcept', FCodeableConcept.Link);{oz.5d}
    if (FTarget <> '') then
      result.addParameter('target', TFHIRUri.create(FTarget));{oz.5f}
    if (FTargetsystem <> '') then
      result.addParameter('targetsystem', TFHIRUri.create(FTargetsystem));{oz.5f}
    for v1 in FDependencyList do
      result.AddParameter(v1.asParams('dependency'));
      result.addParameter('reverse', TFHIRBoolean.create(FReverse));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRTranslateOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['code', 'system', 'version', 'source', 'coding', 'codeableConcept', 'target', 'targetsystem', 'dependency', 'reverse'], name);
end;

procedure TFHIRTranslateOpRespMatch.SetConcept(value : TFhirCoding);
begin
  FConcept.free;
  FConcept := value;
end;

function TFHIRTranslateOpRespMatch.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FEquivalence.length * sizeof(char)) + 12);
  inc(result, FConcept.sizeInBytes);
  inc(result, FProductList.sizeInBytes);
  inc(result, (FSource.length * sizeof(char)) + 12);
end;

procedure TFHIRTranslateOpRespProduct.SetConcept(value : TFhirCoding);
begin
  FConcept.free;
  FConcept := value;
end;

constructor TFHIRTranslateOpRespProduct.create;
begin
  inherited create();
end;

constructor TFHIRTranslateOpRespProduct.create(params : TFhirParametersParameter);
begin
  inherited create();
  FElement := params.str['element'];
  if params.param['concept'] <> nil then
    FConcept := (params.param['concept'].value as TFhirCoding).Link; {ob.5d}
  loadExtensions(params);
end;

destructor TFHIRTranslateOpRespProduct.Destroy;
begin
  FConcept.free;
  inherited;
end;

function TFHIRTranslateOpRespProduct.asParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FElement <> '') then
      result.addParameter('element', TFHIRUri.create(FElement));{oz.5f}
    if (FConcept <> nil) then
      result.addParameter('concept', FConcept.Link);{oz.5d}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRTranslateOpRespProduct.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['element', 'concept'], name);
end;

function TFHIRTranslateOpRespProduct.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FElement.length * sizeof(char)) + 12);
  inc(result, FConcept.sizeInBytes);
end;

constructor TFHIRTranslateOpRespMatch.create;
begin
  inherited create();
  FProductList := TFslList<TFHIRTranslateOpRespProduct>.create;
end;

constructor TFHIRTranslateOpRespMatch.create(params : TFhirParametersParameter);
var
  p : TFhirParametersParameter;
begin
  inherited create();
  FProductList := TFslList<TFHIRTranslateOpRespProduct>.create;
  FEquivalence := params.str['equivalence'];
  if params.param['concept'] <> nil then
    FConcept := (params.param['concept'].value as TFhirCoding).Link; {ob.5d}
  for p in params.partList do
    if p.name = 'product' then
      FProductList.Add(TFHIRTranslateOpRespProduct.create(p));{a}
  FSource := params.str['source'];
  loadExtensions(params);
end;

destructor TFHIRTranslateOpRespMatch.Destroy;
begin
  FConcept.free;
  FProductList.free;
  inherited;
end;

function TFHIRTranslateOpRespMatch.asParams(name : String) : TFhirParametersParameter;
var
  v1 : TFHIRTranslateOpRespProduct;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FEquivalence <> '') then
      result.addParameter('equivalence', TFHIRCode.create(FEquivalence));{oz.5f}
    if (FConcept <> nil) then
      result.addParameter('concept', FConcept.Link);{oz.5d}
    for v1 in FProductList do
      result.AddParameter(v1.asParams('product'));
    if (FSource <> '') then
      result.addParameter('source', TFHIRUri.create(FSource));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRTranslateOpRespMatch.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['equivalence', 'concept', 'product', 'source'], name);
end;

constructor TFHIRTranslateOpResponse.create;
begin
  inherited create();
  FMatchList := TFslList<TFHIRTranslateOpRespMatch>.create;
end;

procedure TFHIRTranslateOpResponse.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FResult := params.bool['result'];
  FMessage := params.str['message'];
  for p in params.parameterList do
    if p.name = 'match' then
      FMatchList.Add(TFHIRTranslateOpRespMatch.create(p));{a}
  loadExtensions(params);
end;

procedure TFHIRTranslateOpResponse.load(params : THTTPParameters);
begin
  FResult := StrToBoolDef(params['result'], false);
  FMessage := params['message'];
  loadExtensions(params);
end;

destructor TFHIRTranslateOpResponse.Destroy;
begin
  FMatchList.free;
  inherited;
end;

function TFHIRTranslateOpResponse.asParams : TFhirParameters;
var
  v1 : TFHIRTranslateOpRespMatch;
begin
  result := TFHIRParameters.create;
  try
      result.addParameter('result', TFHIRBoolean.create(FResult));{oz.5f}
    if (FMessage <> '') then
      result.addParameter('message', TFHIRString.create(FMessage));{oz.5f}
    for v1 in FMatchList do
      result.AddParameter(v1.asParams('match'));
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRTranslateOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['result', 'message', 'match'], name);
end;

function TFHIRTranslateOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FMessage.length * sizeof(char)) + 12);
  inc(result, FMatchList.sizeInBytes);
end;

constructor TFHIREverythingOpRequest.create;
begin
  inherited create();
end;

procedure TFHIREverythingOpRequest.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIREverythingOpRequest.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIREverythingOpRequest.Destroy;
begin
  inherited;
end;

function TFHIREverythingOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIREverythingOpRequest.isKnownName(name : String) : boolean;
begin
  result := false;
end;

procedure TFHIREverythingOpResponse.SetReturn(value : TFhirBundle);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIREverythingOpResponse.create;
begin
  inherited create();
end;

procedure TFHIREverythingOpResponse.load(params : TFHIRParameters);
begin
  FReturn := (params.res['return'] as TFhirBundle).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIREverythingOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIREverythingOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIREverythingOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIREverythingOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
end;

function TFHIREverythingOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturn.sizeInBytes);
end;

constructor TFHIRFindOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRFindOpRequest.load(params : TFHIRParameters);
begin
  FPatient := params.str['patient'];
  FName := params.str['name'];
  loadExtensions(params);
end;

procedure TFHIRFindOpRequest.load(params : THTTPParameters);
begin
  FPatient := params['patient'];
  FName := params['name'];
  loadExtensions(params);
end;

destructor TFHIRFindOpRequest.Destroy;
begin
  inherited;
end;

function TFHIRFindOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FPatient <> '') then
      result.addParameter('patient', TFHIRId.create(FPatient));{oz.5f}
    if (FName <> '') then
      result.addParameter('name', TFHIRCode.create(FName));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFindOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['patient', 'name'], name);
end;

function TFHIRFindOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FPatient.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
end;

constructor TFHIRFindOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRFindOpResponse.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRFindOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRFindOpResponse.Destroy;
begin
  inherited;
end;

function TFHIRFindOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFindOpResponse.isKnownName(name : String) : boolean;
begin
  result := false;
end;

procedure TFHIREvaluateMeasureOpRequest.SetMeasure(value : TFhirReference);
begin
  FMeasure.free;
  FMeasure := value;
end;

procedure TFHIREvaluateMeasureOpRequest.SetPatient(value : TFhirReference);
begin
  FPatient.free;
  FPatient := value;
end;

procedure TFHIREvaluateMeasureOpRequest.SetPractitioner(value : TFhirReference);
begin
  FPractitioner.free;
  FPractitioner := value;
end;

constructor TFHIREvaluateMeasureOpRequest.create;
begin
  inherited create();
end;

procedure TFHIREvaluateMeasureOpRequest.load(params : TFHIRParameters);
begin
  FPeriodStart := TFslDateTime.fromXml(params.str['periodStart']);
  FPeriodEnd := TFslDateTime.fromXml(params.str['periodEnd']);
  if params.param['measure'] <> nil then
    FMeasure := (params.param['measure'].value as TFhirReference).Link; {ob.5d}
  FReportType := params.str['reportType'];
  if params.param['patient'] <> nil then
    FPatient := (params.param['patient'].value as TFhirReference).Link; {ob.5d}
  if params.param['practitioner'] <> nil then
    FPractitioner := (params.param['practitioner'].value as TFhirReference).Link; {ob.5d}
  FLastReceivedOn := TFslDateTime.fromXml(params.str['lastReceivedOn']);
  loadExtensions(params);
end;

procedure TFHIREvaluateMeasureOpRequest.load(params : THTTPParameters);
begin
  FPeriodStart := TFslDateTime.fromXml(params['periodStart']);
  FPeriodEnd := TFslDateTime.fromXml(params['periodEnd']);
  FReportType := params['reportType'];
  FLastReceivedOn := TFslDateTime.fromXml(params['lastReceivedOn']);
  loadExtensions(params);
end;

destructor TFHIREvaluateMeasureOpRequest.Destroy;
begin
  FMeasure.free;
  FPatient.free;
  FPractitioner.free;
  inherited;
end;

function TFHIREvaluateMeasureOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FPeriodStart.notNull) then
      result.addParameter('periodStart', TFHIRDate.create(FPeriodStart));{oz.5f}
    if (FPeriodEnd.notNull) then
      result.addParameter('periodEnd', TFHIRDate.create(FPeriodEnd));{oz.5f}
    if (FMeasure <> nil) then
      result.addParameter('measure', FMeasure.Link);{oz.5d}
    if (FReportType <> '') then
      result.addParameter('reportType', TFHIRCode.create(FReportType));{oz.5f}
    if (FPatient <> nil) then
      result.addParameter('patient', FPatient.Link);{oz.5d}
    if (FPractitioner <> nil) then
      result.addParameter('practitioner', FPractitioner.Link);{oz.5d}
    if (FLastReceivedOn.notNull) then
      result.addParameter('lastReceivedOn', TFHIRDateTime.create(FLastReceivedOn));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIREvaluateMeasureOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['periodStart', 'periodEnd', 'measure', 'reportType', 'patient', 'practitioner', 'lastReceivedOn'], name);
end;

function TFHIREvaluateMeasureOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FMeasure.sizeInBytes);
  inc(result, (FReportType.length * sizeof(char)) + 12);
  inc(result, FPatient.sizeInBytes);
  inc(result, FPractitioner.sizeInBytes);
end;

procedure TFHIREvaluateMeasureOpResponse.SetReturn(value : TFhirMeasureReport);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIREvaluateMeasureOpResponse.create;
begin
  inherited create();
end;

procedure TFHIREvaluateMeasureOpResponse.load(params : TFHIRParameters);
begin
  FReturn := (params.res['return'] as TFhirMeasureReport).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIREvaluateMeasureOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIREvaluateMeasureOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIREvaluateMeasureOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIREvaluateMeasureOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
end;

function TFHIREvaluateMeasureOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturn.sizeInBytes);
end;

procedure TFHIRProcessMessageOpRequest.SetContent(value : TFhirBundle);
begin
  FContent.free;
  FContent := value;
end;

constructor TFHIRProcessMessageOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRProcessMessageOpRequest.load(params : TFHIRParameters);
begin
  FContent := (params.res['content'] as TFhirBundle).Link;{ob.5a}
  FAsync := params.bool['async'];
  FResponseUrl := params.str['response-url'];
  loadExtensions(params);
end;

procedure TFHIRProcessMessageOpRequest.load(params : THTTPParameters);
begin
  FAsync := StrToBoolDef(params['async'], false);
  FResponseUrl := params['response-url'];
  loadExtensions(params);
end;

destructor TFHIRProcessMessageOpRequest.Destroy;
begin
  FContent.free;
  inherited;
end;

function TFHIRProcessMessageOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FContent <> nil) then
      result.addParameter('content', FContent.Link);{oz.5a}
      result.addParameter('async', TFHIRBoolean.create(FAsync));{oz.5f}
    if (FResponseUrl <> '') then
      result.addParameter('response-url', TFHIRUri.create(FResponseUrl));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRProcessMessageOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['content', 'async', 'response-url'], name);
end;

function TFHIRProcessMessageOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FContent.sizeInBytes);
  inc(result, (FResponseUrl.length * sizeof(char)) + 12);
end;

procedure TFHIRProcessMessageOpResponse.SetReturn(value : TFhirBundle);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIRProcessMessageOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRProcessMessageOpResponse.load(params : TFHIRParameters);
begin
  FReturn := (params.res['return'] as TFhirBundle).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRProcessMessageOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRProcessMessageOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRProcessMessageOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRProcessMessageOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
end;

function TFHIRProcessMessageOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturn.sizeInBytes);
end;

constructor TFHIRLastnOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRLastnOpRequest.load(params : TFHIRParameters);
begin
  FMax := params.str['max'];
  loadExtensions(params);
end;

procedure TFHIRLastnOpRequest.load(params : THTTPParameters);
begin
  FMax := params['max'];
  loadExtensions(params);
end;

destructor TFHIRLastnOpRequest.Destroy;
begin
  inherited;
end;

function TFHIRLastnOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FMax <> '') then
      result.addParameter('max', TFHIRPositiveInt.create(FMax));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRLastnOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['max'], name);
end;

function TFHIRLastnOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FMax.length * sizeof(char)) + 12);
end;

procedure TFHIRLastnOpResponse.SetReturn(value : TFhirBundle);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIRLastnOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRLastnOpResponse.load(params : TFHIRParameters);
begin
  FReturn := (params.res['return'] as TFhirBundle).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRLastnOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRLastnOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRLastnOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRLastnOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
end;

function TFHIRLastnOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturn.sizeInBytes);
end;

procedure TFHIRStatsOpRequest.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

constructor TFHIRStatsOpRequest.create;
begin
  inherited create();
  FCodeList := TStringList.create;
  FCodingList := TFslList<TFhirCoding>.create;
  FStatisticList := TStringList.create;
end;

procedure TFHIRStatsOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FSubject := params.str['subject'];
  for p in params.parameterList do
    if p.name = 'code' then
      FCodeList.Add((p.value as TFhirString).value);{ob.1}
  FSystem := params.str['system'];
  for p in params.parameterList do
    if p.name = 'coding' then
      FCodingList.Add((p.value as TFhirCoding).Link);{a}
  FDuration := params.str['duration'];
  if params.param['period'] <> nil then
    FPeriod := (params.param['period'].value as TFhirPeriod).Link; {ob.5d}
  for p in params.parameterList do
    if p.name = 'statistic' then
      FStatisticList.Add((p.value as TFhirCode).value);{ob.1}
  FInclude := params.bool['include'];
  FLimit := params.str['limit'];
  loadExtensions(params);
end;

procedure TFHIRStatsOpRequest.load(params : THTTPParameters);
var
  s : String;
begin
  FSubject := params['subject'];
  for s in params['code'].Split([';']) do
    FCodeList.add(s); 
  FSystem := params['system'];
  FDuration := params['duration'];
  for s in params['statistic'].Split([';']) do
    FStatisticList.add(s); 
  FInclude := StrToBoolDef(params['include'], false);
  FLimit := params['limit'];
  loadExtensions(params);
end;

destructor TFHIRStatsOpRequest.Destroy;
begin
  FCodeList.free;
  FCodingList.free;
  FPeriod.free;
  FStatisticList.free;
  inherited;
end;

function TFHIRStatsOpRequest.asParams : TFhirParameters;
var
  v1 : String;
  v2 : TFhirCoding;
  v3 : String;
begin
  result := TFHIRParameters.create;
  try
    if (FSubject <> '') then
      result.addParameter('subject', TFHIRUri.create(FSubject));{oz.5f}
    for v1 in FCodeList do
      result.AddParameter('code', TFhirString.create(v1));
    if (FSystem <> '') then
      result.addParameter('system', TFHIRUri.create(FSystem));{oz.5f}
    for v2 in FCodingList do
      result.AddParameter('coding', v2.Link);
    if (FDuration <> '') then
      result.addParameter('duration', TFHIRDecimal.create(FDuration));{oz.5f}
    if (FPeriod <> nil) then
      result.addParameter('period', FPeriod.Link);{oz.5d}
    for v3 in FStatisticList do
      result.AddParameter('statistic', TFhirCode.create(v3));
      result.addParameter('include', TFHIRBoolean.create(FInclude));{oz.5f}
    if (FLimit <> '') then
      result.addParameter('limit', TFHIRPositiveInt.create(FLimit));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRStatsOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['subject', 'code', 'system', 'coding', 'duration', 'period', 'statistic', 'include', 'limit'], name);
end;

function TFHIRStatsOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FSubject.length * sizeof(char)) + 12);
  inc(result, FCodeList.sizeInBytes);
  inc(result, (FSystem.length * sizeof(char)) + 12);
  inc(result, FCodingList.sizeInBytes);
  inc(result, (FDuration.length * sizeof(char)) + 12);
  inc(result, FPeriod.sizeInBytes);
  inc(result, FStatisticList.sizeInBytes);
  inc(result, (FLimit.length * sizeof(char)) + 12);
end;

constructor TFHIRStatsOpResponse.create;
begin
  inherited create();
  FReturnList := TFslList<TFhirObservation>.create;
  FSourceList := TFslList<TFhirObservation>.create;
end;

procedure TFHIRStatsOpResponse.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  for p in params.parameterList do
    if p.name = 'return' then
      FReturnList.Add((p.resource as TFhirObservation).Link);{ob.2}
  for p in params.parameterList do
    if p.name = 'source' then
      FSourceList.Add((p.resource as TFhirObservation).Link);{ob.2}
  loadExtensions(params);
end;

procedure TFHIRStatsOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRStatsOpResponse.Destroy;
begin
  FReturnList.free;
  FSourceList.free;
  inherited;
end;

function TFHIRStatsOpResponse.asParams : TFhirParameters;
var
  v1 : TFhirObservation;
  v2 : TFhirObservation;
begin
  result := TFHIRParameters.create;
  try
    for v1 in FReturnList do
      result.AddParameter('return', v1.Link);
    for v2 in FSourceList do
      result.AddParameter('source', v2.Link);
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRStatsOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return', 'source'], name);
end;

function TFHIRStatsOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturnList.sizeInBytes);
  inc(result, FSourceList.sizeInBytes);
end;

procedure TFHIRMatchOpRequest.SetResource(value : TFhirResource);
begin
  FResource.free;
  FResource := value;
end;

constructor TFHIRMatchOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRMatchOpRequest.load(params : TFHIRParameters);
begin
  FResource := (params.res['resource'] as TFhirResource).Link;{ob.5a}
  FOnlyCertainMatches := params.bool['onlyCertainMatches'];
  FCount := params.str['count'];
  loadExtensions(params);
end;

procedure TFHIRMatchOpRequest.load(params : THTTPParameters);
begin
  FOnlyCertainMatches := StrToBoolDef(params['onlyCertainMatches'], false);
  FCount := params['count'];
  loadExtensions(params);
end;

destructor TFHIRMatchOpRequest.Destroy;
begin
  FResource.free;
  inherited;
end;

function TFHIRMatchOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FResource <> nil) then
      result.addParameter('resource', FResource.Link);{oz.5a}
      result.addParameter('onlyCertainMatches', TFHIRBoolean.create(FOnlyCertainMatches));{oz.5f}
    if (FCount <> '') then
      result.addParameter('count', TFHIRInteger.create(FCount));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRMatchOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['resource', 'onlyCertainMatches', 'count'], name);
end;

function TFHIRMatchOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FResource.sizeInBytes);
  inc(result, (FCount.length * sizeof(char)) + 12);
end;

procedure TFHIRMatchOpResponse.SetReturn(value : TFhirBundle);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIRMatchOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRMatchOpResponse.load(params : TFHIRParameters);
begin
  FReturn := (params.res['return'] as TFhirBundle).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRMatchOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRMatchOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRMatchOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRMatchOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
end;

function TFHIRMatchOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturn.sizeInBytes);
end;

procedure TFHIRPopulateOpRequest.SetQuestionnaire(value : TFhirQuestionnaire);
begin
  FQuestionnaire.free;
  FQuestionnaire := value;
end;

procedure TFHIRPopulateOpRequest.SetQuestionnaireRef(value : TFhirReference);
begin
  FQuestionnaireRef.free;
  FQuestionnaireRef := value;
end;

procedure TFHIRPopulateOpRequest.SetSubject(value : TFhirReference);
begin
  FSubject.free;
  FSubject := value;
end;

constructor TFHIRPopulateOpRequest.create;
begin
  inherited create();
  FContentList := TFslList<TFhirReference>.create;
end;

procedure TFHIRPopulateOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FIdentifier := params.str['identifier'];
  FQuestionnaire := (params.res['questionnaire'] as TFhirQuestionnaire).Link;{ob.5a}
  if params.param['questionnaireRef'] <> nil then
    FQuestionnaireRef := (params.param['questionnaireRef'].value as TFhirReference).Link; {ob.5d}
  if params.param['subject'] <> nil then
    FSubject := (params.param['subject'].value as TFhirReference).Link; {ob.5d}
  for p in params.parameterList do
    if p.name = 'content' then
      FContentList.Add((p.value as TFhirReference).Link);{a}
  FLocal := params.bool['local'];
  loadExtensions(params);
end;

procedure TFHIRPopulateOpRequest.load(params : THTTPParameters);
begin
  FIdentifier := params['identifier'];
  FLocal := StrToBoolDef(params['local'], false);
  loadExtensions(params);
end;

destructor TFHIRPopulateOpRequest.Destroy;
begin
  FQuestionnaire.free;
  FQuestionnaireRef.free;
  FSubject.free;
  FContentList.free;
  inherited;
end;

function TFHIRPopulateOpRequest.asParams : TFhirParameters;
var
  v1 : TFhirReference;
begin
  result := TFHIRParameters.create;
  try
    if (FIdentifier <> '') then
      result.addParameter('identifier', TFHIRUri.create(FIdentifier));{oz.5f}
    if (FQuestionnaire <> nil) then
      result.addParameter('questionnaire', FQuestionnaire.Link);{oz.5a}
    if (FQuestionnaireRef <> nil) then
      result.addParameter('questionnaireRef', FQuestionnaireRef.Link);{oz.5d}
    if (FSubject <> nil) then
      result.addParameter('subject', FSubject.Link);{oz.5d}
    for v1 in FContentList do
      result.AddParameter('content', v1.Link);
      result.addParameter('local', TFHIRBoolean.create(FLocal));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRPopulateOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['identifier', 'questionnaire', 'questionnaireRef', 'subject', 'content', 'local'], name);
end;

function TFHIRPopulateOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FIdentifier.length * sizeof(char)) + 12);
  inc(result, FQuestionnaire.sizeInBytes);
  inc(result, FQuestionnaireRef.sizeInBytes);
  inc(result, FSubject.sizeInBytes);
  inc(result, FContentList.sizeInBytes);
end;

procedure TFHIRPopulateOpResponse.SetQuestionnaire(value : TFhirQuestionnaireResponse);
begin
  FQuestionnaire.free;
  FQuestionnaire := value;
end;

procedure TFHIRPopulateOpResponse.SetIssues(value : TFhirOperationOutcome);
begin
  FIssues.free;
  FIssues := value;
end;

constructor TFHIRPopulateOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRPopulateOpResponse.load(params : TFHIRParameters);
begin
  FQuestionnaire := (params.res['questionnaire'] as TFhirQuestionnaireResponse).Link;{ob.5a}
  FIssues := (params.res['issues'] as TFhirOperationOutcome).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRPopulateOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRPopulateOpResponse.Destroy;
begin
  FQuestionnaire.free;
  FIssues.free;
  inherited;
end;

function TFHIRPopulateOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FQuestionnaire <> nil) then
      result.addParameter('questionnaire', FQuestionnaire.Link);{oz.5a}
    if (FIssues <> nil) then
      result.addParameter('issues', FIssues.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRPopulateOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['questionnaire', 'issues'], name);
end;

function TFHIRPopulateOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FQuestionnaire.sizeInBytes);
  inc(result, FIssues.sizeInBytes);
end;

procedure TFHIRPopulatehtmlOpRequest.SetQuestionnaire(value : TFhirQuestionnaire);
begin
  FQuestionnaire.free;
  FQuestionnaire := value;
end;

procedure TFHIRPopulatehtmlOpRequest.SetQuestionnaireRef(value : TFhirReference);
begin
  FQuestionnaireRef.free;
  FQuestionnaireRef := value;
end;

constructor TFHIRPopulatehtmlOpRequest.create;
begin
  inherited create();
  FContentList := TFslList<TFhirReference>.create;
end;

procedure TFHIRPopulatehtmlOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FIdentifier := params.str['identifier'];
  FQuestionnaire := (params.res['questionnaire'] as TFhirQuestionnaire).Link;{ob.5a}
  if params.param['questionnaireRef'] <> nil then
    FQuestionnaireRef := (params.param['questionnaireRef'].value as TFhirReference).Link; {ob.5d}
  for p in params.parameterList do
    if p.name = 'content' then
      FContentList.Add((p.value as TFhirReference).Link);{a}
  FLocal := params.bool['local'];
  loadExtensions(params);
end;

procedure TFHIRPopulatehtmlOpRequest.load(params : THTTPParameters);
begin
  FIdentifier := params['identifier'];
  FLocal := StrToBoolDef(params['local'], false);
  loadExtensions(params);
end;

destructor TFHIRPopulatehtmlOpRequest.Destroy;
begin
  FQuestionnaire.free;
  FQuestionnaireRef.free;
  FContentList.free;
  inherited;
end;

function TFHIRPopulatehtmlOpRequest.asParams : TFhirParameters;
var
  v1 : TFhirReference;
begin
  result := TFHIRParameters.create;
  try
    if (FIdentifier <> '') then
      result.addParameter('identifier', TFHIRUri.create(FIdentifier));{oz.5f}
    if (FQuestionnaire <> nil) then
      result.addParameter('questionnaire', FQuestionnaire.Link);{oz.5a}
    if (FQuestionnaireRef <> nil) then
      result.addParameter('questionnaireRef', FQuestionnaireRef.Link);{oz.5d}
    for v1 in FContentList do
      result.AddParameter('content', v1.Link);
      result.addParameter('local', TFHIRBoolean.create(FLocal));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRPopulatehtmlOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['identifier', 'questionnaire', 'questionnaireRef', 'content', 'local'], name);
end;

function TFHIRPopulatehtmlOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FIdentifier.length * sizeof(char)) + 12);
  inc(result, FQuestionnaire.sizeInBytes);
  inc(result, FQuestionnaireRef.sizeInBytes);
  inc(result, FContentList.sizeInBytes);
end;

procedure TFHIRPopulatehtmlOpResponse.SetForm(value : TFhirBinary);
begin
  FForm.free;
  FForm := value;
end;

procedure TFHIRPopulatehtmlOpResponse.SetIssues(value : TFhirOperationOutcome);
begin
  FIssues.free;
  FIssues := value;
end;

constructor TFHIRPopulatehtmlOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRPopulatehtmlOpResponse.load(params : TFHIRParameters);
begin
  FForm := (params.res['form'] as TFhirBinary).Link;{ob.5a}
  FIssues := (params.res['issues'] as TFhirOperationOutcome).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRPopulatehtmlOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRPopulatehtmlOpResponse.Destroy;
begin
  FForm.free;
  FIssues.free;
  inherited;
end;

function TFHIRPopulatehtmlOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FForm <> nil) then
      result.addParameter('form', FForm.Link);{oz.5a}
    if (FIssues <> nil) then
      result.addParameter('issues', FIssues.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRPopulatehtmlOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['form', 'issues'], name);
end;

function TFHIRPopulatehtmlOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FForm.sizeInBytes);
  inc(result, FIssues.sizeInBytes);
end;

procedure TFHIRPopulatelinkOpRequest.SetQuestionnaire(value : TFhirQuestionnaire);
begin
  FQuestionnaire.free;
  FQuestionnaire := value;
end;

procedure TFHIRPopulatelinkOpRequest.SetQuestionnaireRef(value : TFhirReference);
begin
  FQuestionnaireRef.free;
  FQuestionnaireRef := value;
end;

constructor TFHIRPopulatelinkOpRequest.create;
begin
  inherited create();
  FContentList := TFslList<TFhirReference>.create;
end;

procedure TFHIRPopulatelinkOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FIdentifier := params.str['identifier'];
  FQuestionnaire := (params.res['questionnaire'] as TFhirQuestionnaire).Link;{ob.5a}
  if params.param['questionnaireRef'] <> nil then
    FQuestionnaireRef := (params.param['questionnaireRef'].value as TFhirReference).Link; {ob.5d}
  for p in params.parameterList do
    if p.name = 'content' then
      FContentList.Add((p.value as TFhirReference).Link);{a}
  FLocal := params.bool['local'];
  loadExtensions(params);
end;

procedure TFHIRPopulatelinkOpRequest.load(params : THTTPParameters);
begin
  FIdentifier := params['identifier'];
  FLocal := StrToBoolDef(params['local'], false);
  loadExtensions(params);
end;

destructor TFHIRPopulatelinkOpRequest.Destroy;
begin
  FQuestionnaire.free;
  FQuestionnaireRef.free;
  FContentList.free;
  inherited;
end;

function TFHIRPopulatelinkOpRequest.asParams : TFhirParameters;
var
  v1 : TFhirReference;
begin
  result := TFHIRParameters.create;
  try
    if (FIdentifier <> '') then
      result.addParameter('identifier', TFHIRUri.create(FIdentifier));{oz.5f}
    if (FQuestionnaire <> nil) then
      result.addParameter('questionnaire', FQuestionnaire.Link);{oz.5a}
    if (FQuestionnaireRef <> nil) then
      result.addParameter('questionnaireRef', FQuestionnaireRef.Link);{oz.5d}
    for v1 in FContentList do
      result.AddParameter('content', v1.Link);
      result.addParameter('local', TFHIRBoolean.create(FLocal));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRPopulatelinkOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['identifier', 'questionnaire', 'questionnaireRef', 'content', 'local'], name);
end;

function TFHIRPopulatelinkOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FIdentifier.length * sizeof(char)) + 12);
  inc(result, FQuestionnaire.sizeInBytes);
  inc(result, FQuestionnaireRef.sizeInBytes);
  inc(result, FContentList.sizeInBytes);
end;

procedure TFHIRPopulatelinkOpResponse.SetIssues(value : TFhirOperationOutcome);
begin
  FIssues.free;
  FIssues := value;
end;

constructor TFHIRPopulatelinkOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRPopulatelinkOpResponse.load(params : TFHIRParameters);
begin
  FLink_ := params.str['link'];
  FIssues := (params.res['issues'] as TFhirOperationOutcome).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRPopulatelinkOpResponse.load(params : THTTPParameters);
begin
  FLink_ := params['link'];
  loadExtensions(params);
end;

destructor TFHIRPopulatelinkOpResponse.Destroy;
begin
  FIssues.free;
  inherited;
end;

function TFHIRPopulatelinkOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FLink_ <> '') then
      result.addParameter('link', TFHIRUri.create(FLink_));{oz.5f}
    if (FIssues <> nil) then
      result.addParameter('issues', FIssues.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRPopulatelinkOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['link', 'issues'], name);
end;

function TFHIRPopulatelinkOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FLink_.length * sizeof(char)) + 12);
  inc(result, FIssues.sizeInBytes);
end;

constructor TFHIRMetaOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRMetaOpRequest.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRMetaOpRequest.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRMetaOpRequest.Destroy;
begin
  inherited;
end;

function TFHIRMetaOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRMetaOpRequest.isKnownName(name : String) : boolean;
begin
  result := false;
end;

procedure TFHIRMetaOpResponse.SetReturn(value : TFhirMeta);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIRMetaOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRMetaOpResponse.load(params : TFHIRParameters);
begin
  if params.param['return'] <> nil then
    FReturn := (params.param['return'].value as TFhirMeta).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIRMetaOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRMetaOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRMetaOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5d}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRMetaOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
end;

function TFHIRMetaOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturn.sizeInBytes);
end;

procedure TFHIRMetaAddOpRequest.SetMeta(value : TFhirMeta);
begin
  FMeta.free;
  FMeta := value;
end;

constructor TFHIRMetaAddOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRMetaAddOpRequest.load(params : TFHIRParameters);
begin
  if params.param['meta'] <> nil then
    FMeta := (params.param['meta'].value as TFhirMeta).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIRMetaAddOpRequest.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRMetaAddOpRequest.Destroy;
begin
  FMeta.free;
  inherited;
end;

function TFHIRMetaAddOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FMeta <> nil) then
      result.addParameter('meta', FMeta.Link);{oz.5d}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRMetaAddOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['meta'], name);
end;

function TFHIRMetaAddOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FMeta.sizeInBytes);
end;

procedure TFHIRMetaAddOpResponse.SetReturn(value : TFhirMeta);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIRMetaAddOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRMetaAddOpResponse.load(params : TFHIRParameters);
begin
  if params.param['return'] <> nil then
    FReturn := (params.param['return'].value as TFhirMeta).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIRMetaAddOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRMetaAddOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRMetaAddOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5d}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRMetaAddOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
end;

function TFHIRMetaAddOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturn.sizeInBytes);
end;

procedure TFHIRMetaDeleteOpRequest.SetMeta(value : TFhirMeta);
begin
  FMeta.free;
  FMeta := value;
end;

constructor TFHIRMetaDeleteOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRMetaDeleteOpRequest.load(params : TFHIRParameters);
begin
  if params.param['meta'] <> nil then
    FMeta := (params.param['meta'].value as TFhirMeta).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIRMetaDeleteOpRequest.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRMetaDeleteOpRequest.Destroy;
begin
  FMeta.free;
  inherited;
end;

function TFHIRMetaDeleteOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FMeta <> nil) then
      result.addParameter('meta', FMeta.Link);{oz.5d}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRMetaDeleteOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['meta'], name);
end;

function TFHIRMetaDeleteOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FMeta.sizeInBytes);
end;

procedure TFHIRMetaDeleteOpResponse.SetReturn(value : TFhirMeta);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIRMetaDeleteOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRMetaDeleteOpResponse.load(params : TFHIRParameters);
begin
  if params.param['return'] <> nil then
    FReturn := (params.param['return'].value as TFhirMeta).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIRMetaDeleteOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRMetaDeleteOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRMetaDeleteOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5d}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRMetaDeleteOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
end;

function TFHIRMetaDeleteOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturn.sizeInBytes);
end;

procedure TFHIRValidateOpRequest.SetResource(value : TFhirResource);
begin
  FResource.free;
  FResource := value;
end;

constructor TFHIRValidateOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRValidateOpRequest.load(params : TFHIRParameters);
begin
  FResource := (params.res['resource'] as TFhirResource).Link;{ob.5a}
  FMode := params.str['mode'];
  FProfile := params.str['profile'];
  loadExtensions(params);
end;

procedure TFHIRValidateOpRequest.load(params : THTTPParameters);
begin
  FMode := params['mode'];
  FProfile := params['profile'];
  loadExtensions(params);
end;

destructor TFHIRValidateOpRequest.Destroy;
begin
  FResource.free;
  inherited;
end;

function TFHIRValidateOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FResource <> nil) then
      result.addParameter('resource', FResource.Link);{oz.5a}
    if (FMode <> '') then
      result.addParameter('mode', TFHIRCode.create(FMode));{oz.5f}
    if (FProfile <> '') then
      result.addParameter('profile', TFHIRUri.create(FProfile));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRValidateOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['resource', 'mode', 'profile'], name);
end;

function TFHIRValidateOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FResource.sizeInBytes);
  inc(result, (FMode.length * sizeof(char)) + 12);
  inc(result, (FProfile.length * sizeof(char)) + 12);
end;

procedure TFHIRValidateOpResponse.SetReturn(value : TFhirOperationOutcome);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIRValidateOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRValidateOpResponse.load(params : TFHIRParameters);
begin
  FReturn := (params.res['return'] as TFhirOperationOutcome).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRValidateOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRValidateOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRValidateOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRValidateOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
end;

function TFHIRValidateOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturn.sizeInBytes);
end;

procedure TFHIREvaluateOpRequest.SetInputParameters(value : TFhirParameters);
begin
  FInputParameters.free;
  FInputParameters := value;
end;

procedure TFHIREvaluateOpRequest.SetPatient(value : TFhirReference);
begin
  FPatient.free;
  FPatient := value;
end;

procedure TFHIREvaluateOpRequest.SetEncounter(value : TFhirReference);
begin
  FEncounter.free;
  FEncounter := value;
end;

procedure TFHIREvaluateOpRequest.SetInitiatingOrganization(value : TFhirReference);
begin
  FInitiatingOrganization.free;
  FInitiatingOrganization := value;
end;

procedure TFHIREvaluateOpRequest.SetInitiatingPerson(value : TFhirReference);
begin
  FInitiatingPerson.free;
  FInitiatingPerson := value;
end;

procedure TFHIREvaluateOpRequest.SetUserType(value : TFhirCodeableConcept);
begin
  FUserType.free;
  FUserType := value;
end;

procedure TFHIREvaluateOpRequest.SetUserLanguage(value : TFhirCodeableConcept);
begin
  FUserLanguage.free;
  FUserLanguage := value;
end;

procedure TFHIREvaluateOpRequest.SetUserTaskContext(value : TFhirCodeableConcept);
begin
  FUserTaskContext.free;
  FUserTaskContext := value;
end;

procedure TFHIREvaluateOpRequest.SetReceivingOrganization(value : TFhirReference);
begin
  FReceivingOrganization.free;
  FReceivingOrganization := value;
end;

procedure TFHIREvaluateOpRequest.SetReceivingPerson(value : TFhirReference);
begin
  FReceivingPerson.free;
  FReceivingPerson := value;
end;

procedure TFHIREvaluateOpRequest.SetRecipientType(value : TFhirCodeableConcept);
begin
  FRecipientType.free;
  FRecipientType := value;
end;

procedure TFHIREvaluateOpRequest.SetRecipientLanguage(value : TFhirCodeableConcept);
begin
  FRecipientLanguage.free;
  FRecipientLanguage := value;
end;

procedure TFHIREvaluateOpRequest.SetSetting(value : TFhirCodeableConcept);
begin
  FSetting.free;
  FSetting := value;
end;

procedure TFHIREvaluateOpRequest.SetSettingContext(value : TFhirCodeableConcept);
begin
  FSettingContext.free;
  FSettingContext := value;
end;

constructor TFHIREvaluateOpRequest.create;
begin
  inherited create();
  FInputDataList := TFslList<TFhirResource>.create;
end;

procedure TFHIREvaluateOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FRequestId := params.str['requestId'];
  FEvaluateAtDateTime := TFslDateTime.fromXml(params.str['evaluateAtDateTime']);
  FInputParameters := (params.res['inputParameters'] as TFhirParameters).Link;{ob.5a}
  for p in params.parameterList do
    if p.name = 'inputData' then
      FInputDataList.Add((p.resource as TFhirResource).Link);{ob.2}
  if params.param['patient'] <> nil then
    FPatient := (params.param['patient'].value as TFhirReference).Link; {ob.5d}
  if params.param['encounter'] <> nil then
    FEncounter := (params.param['encounter'].value as TFhirReference).Link; {ob.5d}
  if params.param['initiatingOrganization'] <> nil then
    FInitiatingOrganization := (params.param['initiatingOrganization'].value as TFhirReference).Link; {ob.5d}
  if params.param['initiatingPerson'] <> nil then
    FInitiatingPerson := (params.param['initiatingPerson'].value as TFhirReference).Link; {ob.5d}
  if params.param['userType'] <> nil then
    FUserType := (params.param['userType'].value as TFhirCodeableConcept).Link; {ob.5d}
  if params.param['userLanguage'] <> nil then
    FUserLanguage := (params.param['userLanguage'].value as TFhirCodeableConcept).Link; {ob.5d}
  if params.param['userTaskContext'] <> nil then
    FUserTaskContext := (params.param['userTaskContext'].value as TFhirCodeableConcept).Link; {ob.5d}
  if params.param['receivingOrganization'] <> nil then
    FReceivingOrganization := (params.param['receivingOrganization'].value as TFhirReference).Link; {ob.5d}
  if params.param['receivingPerson'] <> nil then
    FReceivingPerson := (params.param['receivingPerson'].value as TFhirReference).Link; {ob.5d}
  if params.param['recipientType'] <> nil then
    FRecipientType := (params.param['recipientType'].value as TFhirCodeableConcept).Link; {ob.5d}
  if params.param['recipientLanguage'] <> nil then
    FRecipientLanguage := (params.param['recipientLanguage'].value as TFhirCodeableConcept).Link; {ob.5d}
  if params.param['setting'] <> nil then
    FSetting := (params.param['setting'].value as TFhirCodeableConcept).Link; {ob.5d}
  if params.param['settingContext'] <> nil then
    FSettingContext := (params.param['settingContext'].value as TFhirCodeableConcept).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIREvaluateOpRequest.load(params : THTTPParameters);
begin
  FRequestId := params['requestId'];
  FEvaluateAtDateTime := TFslDateTime.fromXml(params['evaluateAtDateTime']);
  loadExtensions(params);
end;

destructor TFHIREvaluateOpRequest.Destroy;
begin
  FInputParameters.free;
  FInputDataList.free;
  FPatient.free;
  FEncounter.free;
  FInitiatingOrganization.free;
  FInitiatingPerson.free;
  FUserType.free;
  FUserLanguage.free;
  FUserTaskContext.free;
  FReceivingOrganization.free;
  FReceivingPerson.free;
  FRecipientType.free;
  FRecipientLanguage.free;
  FSetting.free;
  FSettingContext.free;
  inherited;
end;

function TFHIREvaluateOpRequest.asParams : TFhirParameters;
var
  v1 : TFhirResource;
begin
  result := TFHIRParameters.create;
  try
    if (FRequestId <> '') then
      result.addParameter('requestId', TFHIRId.create(FRequestId));{oz.5f}
    if (FEvaluateAtDateTime.notNull) then
      result.addParameter('evaluateAtDateTime', TFHIRDateTime.create(FEvaluateAtDateTime));{oz.5f}
    if (FInputParameters <> nil) then
      result.addParameter('inputParameters', FInputParameters.Link);{oz.5a}
    for v1 in FInputDataList do
      result.AddParameter('inputData', v1.Link);
    if (FPatient <> nil) then
      result.addParameter('patient', FPatient.Link);{oz.5d}
    if (FEncounter <> nil) then
      result.addParameter('encounter', FEncounter.Link);{oz.5d}
    if (FInitiatingOrganization <> nil) then
      result.addParameter('initiatingOrganization', FInitiatingOrganization.Link);{oz.5d}
    if (FInitiatingPerson <> nil) then
      result.addParameter('initiatingPerson', FInitiatingPerson.Link);{oz.5d}
    if (FUserType <> nil) then
      result.addParameter('userType', FUserType.Link);{oz.5d}
    if (FUserLanguage <> nil) then
      result.addParameter('userLanguage', FUserLanguage.Link);{oz.5d}
    if (FUserTaskContext <> nil) then
      result.addParameter('userTaskContext', FUserTaskContext.Link);{oz.5d}
    if (FReceivingOrganization <> nil) then
      result.addParameter('receivingOrganization', FReceivingOrganization.Link);{oz.5d}
    if (FReceivingPerson <> nil) then
      result.addParameter('receivingPerson', FReceivingPerson.Link);{oz.5d}
    if (FRecipientType <> nil) then
      result.addParameter('recipientType', FRecipientType.Link);{oz.5d}
    if (FRecipientLanguage <> nil) then
      result.addParameter('recipientLanguage', FRecipientLanguage.Link);{oz.5d}
    if (FSetting <> nil) then
      result.addParameter('setting', FSetting.Link);{oz.5d}
    if (FSettingContext <> nil) then
      result.addParameter('settingContext', FSettingContext.Link);{oz.5d}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIREvaluateOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['requestId', 'evaluateAtDateTime', 'inputParameters', 'inputData', 'patient', 'encounter', 'initiatingOrganization', 'initiatingPerson', 'userType', 'userLanguage', 'userTaskContext', 'receivingOrganization', 'receivingPerson', 'recipientType', 'recipientLanguage', 'setting', 'settingContext'], name);
end;

function TFHIREvaluateOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FRequestId.length * sizeof(char)) + 12);
  inc(result, FInputParameters.sizeInBytes);
  inc(result, FInputDataList.sizeInBytes);
  inc(result, FPatient.sizeInBytes);
  inc(result, FEncounter.sizeInBytes);
  inc(result, FInitiatingOrganization.sizeInBytes);
  inc(result, FInitiatingPerson.sizeInBytes);
  inc(result, FUserType.sizeInBytes);
  inc(result, FUserLanguage.sizeInBytes);
  inc(result, FUserTaskContext.sizeInBytes);
  inc(result, FReceivingOrganization.sizeInBytes);
  inc(result, FReceivingPerson.sizeInBytes);
  inc(result, FRecipientType.sizeInBytes);
  inc(result, FRecipientLanguage.sizeInBytes);
  inc(result, FSetting.sizeInBytes);
  inc(result, FSettingContext.sizeInBytes);
end;

procedure TFHIREvaluateOpResponse.SetReturn(value : TFhirGuidanceResponse);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIREvaluateOpResponse.create;
begin
  inherited create();
end;

procedure TFHIREvaluateOpResponse.load(params : TFHIRParameters);
begin
  FReturn := (params.res['return'] as TFhirGuidanceResponse).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIREvaluateOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIREvaluateOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIREvaluateOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIREvaluateOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
end;

function TFHIREvaluateOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturn.sizeInBytes);
end;

constructor TFHIRQuestionnaireOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRQuestionnaireOpRequest.load(params : TFHIRParameters);
begin
  FIdentifier := params.str['identifier'];
  FProfile := params.str['profile'];
  FUrl := params.str['url'];
  FSupportedOnly := params.bool['supportedOnly'];
  loadExtensions(params);
end;

procedure TFHIRQuestionnaireOpRequest.load(params : THTTPParameters);
begin
  FIdentifier := params['identifier'];
  FProfile := params['profile'];
  FUrl := params['url'];
  FSupportedOnly := StrToBoolDef(params['supportedOnly'], false);
  loadExtensions(params);
end;

destructor TFHIRQuestionnaireOpRequest.Destroy;
begin
  inherited;
end;

function TFHIRQuestionnaireOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FIdentifier <> '') then
      result.addParameter('identifier', TFHIRUri.create(FIdentifier));{oz.5f}
    if (FProfile <> '') then
      result.addParameter('profile', TFHIRString.create(FProfile));{oz.5f}
    if (FUrl <> '') then
      result.addParameter('url', TFHIRUri.create(FUrl));{oz.5f}
      result.addParameter('supportedOnly', TFHIRBoolean.create(FSupportedOnly));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRQuestionnaireOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['identifier', 'profile', 'url', 'supportedOnly'], name);
end;

function TFHIRQuestionnaireOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FIdentifier.length * sizeof(char)) + 12);
  inc(result, (FProfile.length * sizeof(char)) + 12);
  inc(result, (FUrl.length * sizeof(char)) + 12);
end;

procedure TFHIRQuestionnaireOpResponse.SetReturn(value : TFhirQuestionnaire);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIRQuestionnaireOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRQuestionnaireOpResponse.load(params : TFHIRParameters);
begin
  FReturn := (params.res['return'] as TFhirQuestionnaire).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRQuestionnaireOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRQuestionnaireOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRQuestionnaireOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRQuestionnaireOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
end;

function TFHIRQuestionnaireOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturn.sizeInBytes);
end;

procedure TFHIRTransformOpRequest.SetContent(value : TFhirResource);
begin
  FContent.free;
  FContent := value;
end;

constructor TFHIRTransformOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRTransformOpRequest.load(params : TFHIRParameters);
begin
  FSource := params.str['source'];
  FContent := (params.res['content'] as TFhirResource).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRTransformOpRequest.load(params : THTTPParameters);
begin
  FSource := params['source'];
  loadExtensions(params);
end;

destructor TFHIRTransformOpRequest.Destroy;
begin
  FContent.free;
  inherited;
end;

function TFHIRTransformOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FSource <> '') then
      result.addParameter('source', TFHIRUri.create(FSource));{oz.5f}
    if (FContent <> nil) then
      result.addParameter('content', FContent.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRTransformOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['source', 'content'], name);
end;

function TFHIRTransformOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FSource.length * sizeof(char)) + 12);
  inc(result, FContent.sizeInBytes);
end;

procedure TFHIRTransformOpResponse.SetReturn(value : TFhirResource);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIRTransformOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRTransformOpResponse.load(params : TFHIRParameters);
begin
  FReturn := (params.res['return'] as TFhirResource).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRTransformOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRTransformOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRTransformOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRTransformOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
end;

function TFHIRTransformOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturn.sizeInBytes);
end;

procedure TFHIRExpandOpRequest.SetValueSet(value : TFhirValueSet);
begin
  FValueSet.free;
  FValueSet := value;
end;

constructor TFHIRExpandOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRExpandOpRequest.load(params : TFHIRParameters);
begin
  FUrl := params.str['url'];
  FValueSet := (params.res['valueSet'] as TFhirValueSet).Link;{ob.5a}
  FContext := params.str['context'];
  FFilter := params.str['filter'];
  FProfile := params.str['profile'];
  FDate := TFslDateTime.fromXml(params.str['date']);
  FOffset := params.str['offset'];
  FCount := params.str['count'];
  FIncludeDesignations := params.bool['includeDesignations'];
  FIncludeDefinition := params.bool['includeDefinition'];
  FActiveOnly := params.bool['activeOnly'];
  FExcludeNested := params.bool['excludeNested'];
  FExcludeNotForUI := params.bool['excludeNotForUI'];
  FExcludePostCoordinated := params.bool['excludePostCoordinated'];
  FDisplayLanguage := params.str['displayLanguage'];
  FLimitedExpansion := params.bool['limitedExpansion'];
  loadExtensions(params);
end;

procedure TFHIRExpandOpRequest.load(params : THTTPParameters);
begin
  FUrl := params['url'];
  FContext := params['context'];
  FFilter := params['filter'];
  FProfile := params['profile'];
  FDate := TFslDateTime.fromXml(params['date']);
  FOffset := params['offset'];
  FCount := params['count'];
  FIncludeDesignations := StrToBoolDef(params['includeDesignations'], false);
  FIncludeDefinition := StrToBoolDef(params['includeDefinition'], false);
  FActiveOnly := StrToBoolDef(params['activeOnly'], false);
  FExcludeNested := StrToBoolDef(params['excludeNested'], false);
  FExcludeNotForUI := StrToBoolDef(params['excludeNotForUI'], false);
  FExcludePostCoordinated := StrToBoolDef(params['excludePostCoordinated'], false);
  FDisplayLanguage := params['displayLanguage'];
  FLimitedExpansion := StrToBoolDef(params['limitedExpansion'], false);
  loadExtensions(params);
end;

destructor TFHIRExpandOpRequest.Destroy;
begin
  FValueSet.free;
  inherited;
end;

function TFHIRExpandOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FUrl <> '') then
      result.addParameter('url', TFHIRUri.create(FUrl));{oz.5f}
    if (FValueSet <> nil) then
      result.addParameter('valueSet', FValueSet.Link);{oz.5a}
    if (FContext <> '') then
      result.addParameter('context', TFHIRUri.create(FContext));{oz.5f}
    if (FFilter <> '') then
      result.addParameter('filter', TFHIRString.create(FFilter));{oz.5f}
    if (FProfile <> '') then
      result.addParameter('profile', TFHIRUri.create(FProfile));{oz.5f}
    if (FDate.notNull) then
      result.addParameter('date', TFHIRDateTime.create(FDate));{oz.5f}
    if (FOffset <> '') then
      result.addParameter('offset', TFHIRInteger.create(FOffset));{oz.5f}
    if (FCount <> '') then
      result.addParameter('count', TFHIRInteger.create(FCount));{oz.5f}
      result.addParameter('includeDesignations', TFHIRBoolean.create(FIncludeDesignations));{oz.5f}
      result.addParameter('includeDefinition', TFHIRBoolean.create(FIncludeDefinition));{oz.5f}
      result.addParameter('activeOnly', TFHIRBoolean.create(FActiveOnly));{oz.5f}
      result.addParameter('excludeNested', TFHIRBoolean.create(FExcludeNested));{oz.5f}
      result.addParameter('excludeNotForUI', TFHIRBoolean.create(FExcludeNotForUI));{oz.5f}
      result.addParameter('excludePostCoordinated', TFHIRBoolean.create(FExcludePostCoordinated));{oz.5f}
    if (FDisplayLanguage <> '') then
      result.addParameter('displayLanguage', TFHIRCode.create(FDisplayLanguage));{oz.5f}
      result.addParameter('limitedExpansion', TFHIRBoolean.create(FLimitedExpansion));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRExpandOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['url', 'valueSet', 'context', 'filter', 'profile', 'date', 'offset', 'count', 'includeDesignations', 'includeDefinition', 'activeOnly', 'excludeNested', 'excludeNotForUI', 'excludePostCoordinated', 'displayLanguage', 'limitedExpansion'], name);
end;

function TFHIRExpandOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FUrl.length * sizeof(char)) + 12);
  inc(result, FValueSet.sizeInBytes);
  inc(result, (FContext.length * sizeof(char)) + 12);
  inc(result, (FFilter.length * sizeof(char)) + 12);
  inc(result, (FProfile.length * sizeof(char)) + 12);
  inc(result, (FOffset.length * sizeof(char)) + 12);
  inc(result, (FCount.length * sizeof(char)) + 12);
  inc(result, (FDisplayLanguage.length * sizeof(char)) + 12);
end;

procedure TFHIRExpandOpResponse.SetReturn(value : TFhirValueSet);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIRExpandOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRExpandOpResponse.load(params : TFHIRParameters);
begin
  FReturn := (params.res['return'] as TFhirValueSet).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRExpandOpResponse.load(params : THTTPParameters);
begin
  loadExtensions(params);
end;

destructor TFHIRExpandOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRExpandOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRExpandOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
end;

function TFHIRExpandOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FReturn.sizeInBytes);
end;

procedure TFHIRValidateCodeOpRequest.SetValueSet(value : TFhirValueSet);
begin
  FValueSet.free;
  FValueSet := value;
end;

procedure TFHIRValidateCodeOpRequest.SetCoding(value : TFhirCoding);
begin
  FCoding.free;
  FCoding := value;
end;

procedure TFHIRValidateCodeOpRequest.SetCodeableConcept(value : TFhirCodeableConcept);
begin
  FCodeableConcept.free;
  FCodeableConcept := value;
end;

constructor TFHIRValidateCodeOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRValidateCodeOpRequest.load(params : TFHIRParameters);
begin
  FUrl := params.str['url'];
  FContext := params.str['context'];
  FValueSet := (params.res['valueSet'] as TFhirValueSet).Link;{ob.5a}
  FCode := params.str['code'];
  FSystem := params.str['system'];
  FVersion := params.str['version'];
  FDisplay := params.str['display'];
  if params.param['coding'] <> nil then
    FCoding := (params.param['coding'].value as TFhirCoding).Link; {ob.5d}
  if params.param['codeableConcept'] <> nil then
    FCodeableConcept := (params.param['codeableConcept'].value as TFhirCodeableConcept).Link; {ob.5d}
  FDate := TFslDateTime.fromXml(params.str['date']);
  FAbstract := params.bool['abstract'];
  FDisplayLanguage := params.str['displayLanguage'];
  loadExtensions(params);
end;

procedure TFHIRValidateCodeOpRequest.load(params : THTTPParameters);
begin
  FUrl := params['url'];
  FContext := params['context'];
  FCode := params['code'];
  FSystem := params['system'];
  FVersion := params['version'];
  FDisplay := params['display'];
  FDate := TFslDateTime.fromXml(params['date']);
  FAbstract := StrToBoolDef(params['abstract'], false);
  FDisplayLanguage := params['displayLanguage'];
  loadExtensions(params);
end;

destructor TFHIRValidateCodeOpRequest.Destroy;
begin
  FValueSet.free;
  FCoding.free;
  FCodeableConcept.free;
  inherited;
end;

function TFHIRValidateCodeOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FUrl <> '') then
      result.addParameter('url', TFHIRUri.create(FUrl));{oz.5f}
    if (FContext <> '') then
      result.addParameter('context', TFHIRUri.create(FContext));{oz.5f}
    if (FValueSet <> nil) then
      result.addParameter('valueSet', FValueSet.Link);{oz.5a}
    if (FCode <> '') then
      result.addParameter('code', TFHIRCode.create(FCode));{oz.5f}
    if (FSystem <> '') then
      result.addParameter('system', TFHIRUri.create(FSystem));{oz.5f}
    if (FVersion <> '') then
      result.addParameter('version', TFHIRString.create(FVersion));{oz.5f}
    if (FDisplay <> '') then
      result.addParameter('display', TFHIRString.create(FDisplay));{oz.5f}
    if (FCoding <> nil) then
      result.addParameter('coding', FCoding.Link);{oz.5d}
    if (FCodeableConcept <> nil) then
      result.addParameter('codeableConcept', FCodeableConcept.Link);{oz.5d}
    if (FDate.notNull) then
      result.addParameter('date', TFHIRDateTime.create(FDate));{oz.5f}
      result.addParameter('abstract', TFHIRBoolean.create(FAbstract));{oz.5f}
    if (FDisplayLanguage <> '') then
      result.addParameter('displayLanguage', TFHIRCode.create(FDisplayLanguage));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRValidateCodeOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['url', 'context', 'valueSet', 'code', 'system', 'version', 'display', 'coding', 'codeableConcept', 'date', 'abstract', 'displayLanguage'], name);
end;

function TFHIRValidateCodeOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FUrl.length * sizeof(char)) + 12);
  inc(result, (FContext.length * sizeof(char)) + 12);
  inc(result, FValueSet.sizeInBytes);
  inc(result, (FCode.length * sizeof(char)) + 12);
  inc(result, (FSystem.length * sizeof(char)) + 12);
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, (FDisplay.length * sizeof(char)) + 12);
  inc(result, FCoding.sizeInBytes);
  inc(result, FCodeableConcept.sizeInBytes);
  inc(result, (FDisplayLanguage.length * sizeof(char)) + 12);
end;

constructor TFHIRValidateCodeOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRValidateCodeOpResponse.load(params : TFHIRParameters);
begin
  FResult := params.bool['result'];
  FMessage := params.str['message'];
  FDisplay := params.str['display'];
  loadExtensions(params);
end;

procedure TFHIRValidateCodeOpResponse.load(params : THTTPParameters);
begin
  FResult := StrToBoolDef(params['result'], false);
  FMessage := params['message'];
  FDisplay := params['display'];
  loadExtensions(params);
end;

destructor TFHIRValidateCodeOpResponse.Destroy;
begin
  inherited;
end;

function TFHIRValidateCodeOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
      result.addParameter('result', TFHIRBoolean.create(FResult));{oz.5f}
    if (FMessage <> '') then
      result.addParameter('message', TFHIRString.create(FMessage));{oz.5f}
    if (FDisplay <> '') then
      result.addParameter('display', TFHIRString.create(FDisplay));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRValidateCodeOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['result', 'message', 'display'], name);
end;

function TFHIRValidateCodeOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FMessage.length * sizeof(char)) + 12);
  inc(result, (FDisplay.length * sizeof(char)) + 12);
end;

end.

