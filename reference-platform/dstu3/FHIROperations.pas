{!Wrapper uses FHIRBase, FHIRBase_Wrapper, FHIRTypes, FHIRTypes_Wrapper, DateAndTime, DateAndTime_Wrapper}

unit FHIROperations;

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

{$IFNDEF FHIR3}
This is the dstu3 version of the FHIR code
{$ENDIF}


interface

// FHIR v1.5.0 generated 2016-07-19T06:18:28+10:00

uses
  SysUtils, Classes, Generics.Collections, StringSupport, DecimalSupport, AdvBuffers, AdvGenerics, ParseMap, DateAndTime, FHIRBase, FHIRTypes, FHIRResources, FHIROpBase;

Type

  //Operation lookup (Concept Look Up)
  TFHIRLookupOpRequest = class (TFHIROperationRequest)
  private
    FCode : String;
    FSystem : String;
    FVersion : String;
    FCoding : TFhirCoding;
    FDate : TDateAndTime;
    FProperty_List : TList<String>;
    procedure SetCoding(value : TFhirCoding);
    procedure SetDate(value : TDateAndTime);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property code : String read FCode write FCode;
    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property coding : TFhirCoding read FCoding write SetCoding;
    property date : TDateAndTime read FDate write SetDate;
    property property_List : TList<String> read FProperty_List;
  end;

  TFHIRLookupOpProperty_ = class (TFHIROperationObject)
  private
    FCode : String;
    FValue : String;
    FDescription : String;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload;
    constructor Create(params : TFhirParametersParameter); overload;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter;
    property code : String read FCode write FCode;
    property value : String read FValue write FValue;
    property description : String read FDescription write FDescription;
  end;

  TFHIRLookupOpDesignation = class (TFHIROperationObject)
  private
    FLanguage : String;
    FUse : TFhirCoding;
    FValue : String;
    procedure SetUse(value : TFhirCoding);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload;
    constructor Create(params : TFhirParametersParameter); overload;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter;
    property language : String read FLanguage write FLanguage;
    property use : TFhirCoding read FUse write SetUse;
    property value : String read FValue write FValue;
  end;

  TFHIRLookupOpResponse = class (TFHIROperationResponse)
  private
    FName : String;
    FVersion : String;
    FDisplay : String;
    FProperty_List : TAdvList<TFHIRLookupOpProperty_>;
    FDesignationList : TAdvList<TFHIRLookupOpDesignation>;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property name : String read FName write FName;
    property version : String read FVersion write FVersion;
    property display : String read FDisplay write FDisplay;
    property property_List : TAdvList<TFHIRLookupOpProperty_> read FProperty_List;
    property designationList : TAdvList<TFHIRLookupOpDesignation> read FDesignationList;
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
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
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
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property outcome : String read FOutcome write FOutcome;
  end;

  //Operation infer (Performing inferencing based on a set of codes - e.g. a mix of composition and decomposition)
  TFHIRInferOpConcept = class (TFHIROperationObject)
  private
    FProperty_ : String;
    FCode : String;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload;
    constructor Create(params : TFhirParametersParameter); overload;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter;
    property property_ : String read FProperty_ write FProperty_;
    property code : String read FCode write FCode;
  end;

  TFHIRInferOpRequest = class (TFHIROperationRequest)
  private
    FSystem : String;
    FVersion : String;
    FConceptList : TAdvList<TFHIRInferOpConcept>;
    FProperty_List : TList<String>;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property conceptList : TAdvList<TFHIRInferOpConcept> read FConceptList;
    property property_List : TList<String> read FProperty_List;
  end;

  TFHIRInferOpOutput = class (TFHIROperationObject)
  private
    FProperty_ : String;
    FCode : String;
    FDisplay : String;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload;
    constructor Create(params : TFhirParametersParameter); overload;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter;
    property property_ : String read FProperty_ write FProperty_;
    property code : String read FCode write FCode;
    property display : String read FDisplay write FDisplay;
  end;

  TFHIRInferOpResponse = class (TFHIROperationResponse)
  private
    FMessage : String;
    FOutputList : TAdvList<TFHIRInferOpOutput>;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property message : String read FMessage write FMessage;
    property outputList : TAdvList<TFHIRInferOpOutput> read FOutputList;
  end;

  //Operation expand (Value Set Expansion)
  TFHIRExpandOpRequest = class (TFHIROperationRequest)
  private
    FIdentifier : String;
    FValueSet : TFhirValueSet;
    FContext : String;
    FFilter : String;
    FProfile : String;
    FDate : TDateAndTime;
    FOffset : String;
    FCount : String;
    procedure SetValueSet(value : TFhirValueSet);
    procedure SetDate(value : TDateAndTime);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property identifier : String read FIdentifier write FIdentifier;
    property valueSet : TFhirValueSet read FValueSet write SetValueSet;
    property context : String read FContext write FContext;
    property filter : String read FFilter write FFilter;
    property profile : String read FProfile write FProfile;
    property date : TDateAndTime read FDate write SetDate;
    property offset : String read FOffset write FOffset;
    property count : String read FCount write FCount;
  end;

  TFHIRExpandOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirValueSet;
    procedure SetReturn(value : TFhirValueSet);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirValueSet read FReturn write SetReturn;
  end;

  //Operation validate-code (Value Set based Validation)
  TFHIRValidateCodeOpRequest = class (TFHIROperationRequest)
  private
    FIdentifier : String;
    FContext : String;
    FValueSet : TFhirValueSet;
    FCode : String;
    FSystem : String;
    FVersion : String;
    FDisplay : String;
    FCoding : TFhirCoding;
    FCodeableConcept : TFhirCodeableConcept;
    FDate : TDateAndTime;
    FAbstract : Boolean;
    procedure SetValueSet(value : TFhirValueSet);
    procedure SetCoding(value : TFhirCoding);
    procedure SetCodeableConcept(value : TFhirCodeableConcept);
    procedure SetDate(value : TDateAndTime);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property identifier : String read FIdentifier write FIdentifier;
    property context : String read FContext write FContext;
    property valueSet : TFhirValueSet read FValueSet write SetValueSet;
    property code : String read FCode write FCode;
    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property display : String read FDisplay write FDisplay;
    property coding : TFhirCoding read FCoding write SetCoding;
    property codeableConcept : TFhirCodeableConcept read FCodeableConcept write SetCodeableConcept;
    property date : TDateAndTime read FDate write SetDate;
    property abstract : Boolean read FAbstract write FAbstract;
  end;

  TFHIRValidateCodeOpResponse = class (TFHIROperationResponse)
  private
    FResult : Boolean;
    FMessage : String;
    FDisplay : String;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property result : Boolean read FResult write FResult;
    property message : String read FMessage write FMessage;
    property display : String read FDisplay write FDisplay;
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
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
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
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirOperationOutcome read FReturn write SetReturn;
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
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  TFHIRMetaOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirMeta;
    procedure SetReturn(value : TFhirMeta);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
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
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property meta : TFhirMeta read FMeta write SetMeta;
  end;

  TFHIRMetaAddOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirMeta;
    procedure SetReturn(value : TFhirMeta);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
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
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property meta : TFhirMeta read FMeta write SetMeta;
  end;

  TFHIRMetaDeleteOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirMeta;
    procedure SetReturn(value : TFhirMeta);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirMeta read FReturn write SetReturn;
  end;

  //Operation cds-hook (The CDS Hook operation is the core API request for CDS Hooks)
  TFHIRCdsHookOpOauth = class (TFHIROperationObject)
  private
    FToken : String;
    FScope : String;
    FExpires : String;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload;
    constructor Create(params : TFhirParametersParameter); overload;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter;
    property token : String read FToken write FToken;
    property scope : String read FScope write FScope;
    property expires : String read FExpires write FExpires;
  end;

  TFHIRCdsHookOpSource = class (TFHIROperationObject)
  private
    FLabel_ : String;
    FUrl : String;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload;
    constructor Create(params : TFhirParametersParameter); overload;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter;
    property label_ : String read FLabel_ write FLabel_;
    property url : String read FUrl write FUrl;
  end;

  TFHIRCdsHookOpSuggestion = class (TFHIROperationObject)
  private
    FLabel_ : String;
    FCreate_List : TAdvList<TFhirResource>;
    FDeleteList : TList<String>;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload;
    constructor Create(params : TFhirParametersParameter); overload;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter;
    property label_ : String read FLabel_ write FLabel_;
    property create_List : TAdvList<TFhirResource> read FCreate_List;
    property deleteList : TList<String> read FDeleteList;
  end;

  TFHIRCdsHookOpLink_ = class (TFHIROperationObject)
  private
    FLabel_ : String;
    FUrl : String;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload;
    constructor Create(params : TFhirParametersParameter); overload;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter;
    property label_ : String read FLabel_ write FLabel_;
    property url : String read FUrl write FUrl;
  end;

  TFHIRCdsHookOpCard = class (TFHIROperationObject)
  private
    FSummary : String;
    FDetail : String;
    FIndicator : String;
    FSource : TFHIRCdsHookOpSource;
    FSuggestionList : TAdvList<TFHIRCdsHookOpSuggestion>;
    FLink_List : TAdvList<TFHIRCdsHookOpLink_>;
    procedure SetSource(value : TFHIRCdsHookOpSource);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload;
    constructor Create(params : TFhirParametersParameter); overload;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter;
    property summary : String read FSummary write FSummary;
    property detail : String read FDetail write FDetail;
    property indicator : String read FIndicator write FIndicator;
    property source : TFHIRCdsHookOpSource read FSource write SetSource;
    property suggestionList : TAdvList<TFHIRCdsHookOpSuggestion> read FSuggestionList;
    property link_List : TAdvList<TFHIRCdsHookOpLink_> read FLink_List;
  end;

  TFHIRCdsHookOpRequest = class (TFHIROperationRequest)
  private
    FActivity : TFhirCoding;
    FActivityInstance : String;
    FFhirServer : String;
    FOauth : TFHIRCdsHookOpOauth;
    FRedirect : String;
    FUser : String;
    FPatient : String;
    FEncounter : String;
    FContextList : TAdvList<TFhirResource>;
    FPreFetchData : TFhirBundle;
    FCard : TFHIRCdsHookOpCard;
    procedure SetActivity(value : TFhirCoding);
    procedure SetOauth(value : TFHIRCdsHookOpOauth);
    procedure SetPreFetchData(value : TFhirBundle);
    procedure SetCard(value : TFHIRCdsHookOpCard);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property activity : TFhirCoding read FActivity write SetActivity;
    property activityInstance : String read FActivityInstance write FActivityInstance;
    property fhirServer : String read FFhirServer write FFhirServer;
    property oauth : TFHIRCdsHookOpOauth read FOauth write SetOauth;
    property redirect : String read FRedirect write FRedirect;
    property user : String read FUser write FUser;
    property patient : String read FPatient write FPatient;
    property encounter : String read FEncounter write FEncounter;
    property contextList : TAdvList<TFhirResource> read FContextList;
    property preFetchData : TFhirBundle read FPreFetchData write SetPreFetchData;
    property card : TFHIRCdsHookOpCard read FCard write SetCard;
  end;

  TFHIRCdsHookOpDecision = class (TFHIROperationObject)
  private
    FCreate_List : TAdvList<TFhirResource>;
    FDeleteList : TList<String>;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload;
    constructor Create(params : TFhirParametersParameter); overload;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter;
    property create_List : TAdvList<TFhirResource> read FCreate_List;
    property deleteList : TList<String> read FDeleteList;
  end;

  TFHIRCdsHookOpResponse = class (TFHIROperationResponse)
  private
    FDecisionList : TAdvList<TFHIRCdsHookOpDecision>;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property decisionList : TAdvList<TFHIRCdsHookOpDecision> read FDecisionList;
  end;

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
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
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
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
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
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  TFHIRDataRequirementsOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirLibrary;
    procedure SetReturn(value : TFhirLibrary);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirLibrary read FReturn write SetReturn;
  end;

  //Operation document (Generate a Document)
  TFHIRDocumentOpRequest = class (TFHIROperationRequest)
  private
    FPersist : Boolean;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
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
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  //Operation translate (Concept Translation)
  TFHIRTranslateOpDependency = class (TFHIROperationObject)
  private
    FElement : String;
    FConcept : TFhirCodeableConcept;
    procedure SetConcept(value : TFhirCodeableConcept);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload;
    constructor Create(params : TFhirParametersParameter); overload;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter;
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
    FDependencyList : TAdvList<TFHIRTranslateOpDependency>;
    FReverse : Boolean;
    procedure SetCoding(value : TFhirCoding);
    procedure SetCodeableConcept(value : TFhirCodeableConcept);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property code : String read FCode write FCode;
    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property source : String read FSource write FSource;
    property coding : TFhirCoding read FCoding write SetCoding;
    property codeableConcept : TFhirCodeableConcept read FCodeableConcept write SetCodeableConcept;
    property target : String read FTarget write FTarget;
    property targetsystem : String read FTargetsystem write FTargetsystem;
    property dependencyList : TAdvList<TFHIRTranslateOpDependency> read FDependencyList;
    property reverse : Boolean read FReverse write FReverse;
  end;

  TFHIRTranslateOpProduct = class (TFHIROperationObject)
  private
    FElement : String;
    FConcept : TFhirCoding;
    procedure SetConcept(value : TFhirCoding);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload;
    constructor Create(params : TFhirParametersParameter); overload;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter;
    property element : String read FElement write FElement;
    property concept : TFhirCoding read FConcept write SetConcept;
  end;

  TFHIRTranslateOpMatch = class (TFHIROperationObject)
  private
    FEquivalence : String;
    FConcept : TFhirCoding;
    FProductList : TAdvList<TFHIRTranslateOpProduct>;
    FSource : String;
    procedure SetConcept(value : TFhirCoding);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload;
    constructor Create(params : TFhirParametersParameter); overload;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter;
    property equivalence : String read FEquivalence write FEquivalence;
    property concept : TFhirCoding read FConcept write SetConcept;
    property productList : TAdvList<TFHIRTranslateOpProduct> read FProductList;
    property source : String read FSource write FSource;
  end;

  TFHIRTranslateOpResponse = class (TFHIROperationResponse)
  private
    FResult : Boolean;
    FMessage : String;
    FMatchList : TAdvList<TFHIRTranslateOpMatch>;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property result : Boolean read FResult write FResult;
    property message : String read FMessage write FMessage;
    property matchList : TAdvList<TFHIRTranslateOpMatch> read FMatchList;
  end;

  //Operation closure (Closure Table Maintenance)
  TFHIRClosureOpRequest = class (TFHIROperationRequest)
  private
    FName : String;
    FConceptList : TAdvList<TFhirCoding>;
    FVersion : String;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property name : String read FName write FName;
    property conceptList : TAdvList<TFhirCoding> read FConceptList;
    property version : String read FVersion write FVersion;
  end;

  TFHIRClosureOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirConceptMap;
    procedure SetReturn(value : TFhirConceptMap);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirConceptMap read FReturn write SetReturn;
  end;

  //Operation subset (Fetch a subset of the conformance resource)
  TFHIRSubsetOpRequest = class (TFHIROperationRequest)
  private
    FServer : String;
    FResourceList : TList<String>;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property server : String read FServer write FServer;
    property resourceList : TList<String> read FResourceList;
  end;

  TFHIRSubsetOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirConformance;
    procedure SetReturn(value : TFhirConformance);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirConformance read FReturn write SetReturn;
  end;

  //Operation implements (Test if a server implements a client's required operations)
  TFHIRImplementsOpRequest = class (TFHIROperationRequest)
  private
    FServer : String;
    FClient : String;
    FResource : TFhirConformance;
    procedure SetResource(value : TFhirConformance);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property server : String read FServer write FServer;
    property client : String read FClient write FClient;
    property resource : TFhirConformance read FResource write SetResource;
  end;

  TFHIRImplementsOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirOperationOutcome;
    FIssues : TFhirOperationOutcome;
    FUnion : TFhirBundle;
    FIntersection : TFhirBundle;
    procedure SetReturn(value : TFhirOperationOutcome);
    procedure SetIssues(value : TFhirOperationOutcome);
    procedure SetUnion(value : TFhirBundle);
    procedure SetIntersection(value : TFhirBundle);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirOperationOutcome read FReturn write SetReturn;
    property issues : TFhirOperationOutcome read FIssues write SetIssues;
    property union : TFhirBundle read FUnion write SetUnion;
    property intersection : TFhirBundle read FIntersection write SetIntersection;
  end;

  //Operation conforms (Test if a server implements a client's required operations)
  TFHIRConformsOpRequest = class (TFHIROperationRequest)
  private
    FLeft : String;
    FRight : String;
    FMode : String;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property left : String read FLeft write FLeft;
    property right : String read FRight write FRight;
    property mode : String read FMode write FMode;
  end;

  TFHIRConformsOpResponse = class (TFHIROperationResponse)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  //Operation evaluate (Evaluate)
  TFHIREvaluateOpRequest = class (TFHIROperationRequest)
  private
    FRequestId : String;
    FEvaluateAtDateTime : TDateAndTime;
    FInputParameters : TFhirParameters;
    FInputDataList : TAdvList<TFhirResource>;
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
    procedure SetEvaluateAtDateTime(value : TDateAndTime);
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
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property requestId : String read FRequestId write FRequestId;
    property evaluateAtDateTime : TDateAndTime read FEvaluateAtDateTime write SetEvaluateAtDateTime;
    property inputParameters : TFhirParameters read FInputParameters write SetInputParameters;
    property inputDataList : TAdvList<TFhirResource> read FInputDataList;
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
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirGuidanceResponse read FReturn write SetReturn;
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
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  TFHIREverythingOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirBundle;
    procedure SetReturn(value : TFhirBundle);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
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
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
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
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  //Operation evaluate-measure (Evaluate Measure)
  TFHIREvaluateMeasureOpRequest = class (TFHIROperationRequest)
  private
    FPeriodStart : TDateAndTime;
    FPeriodEnd : TDateAndTime;
    FMeasure : TFhirReference;
    FReportType : String;
    FPatient : TFhirReference;
    FPractitioner : TFhirReference;
    FLastReceivedOn : TDateAndTime;
    procedure SetPeriodStart(value : TDateAndTime);
    procedure SetPeriodEnd(value : TDateAndTime);
    procedure SetMeasure(value : TFhirReference);
    procedure SetPatient(value : TFhirReference);
    procedure SetPractitioner(value : TFhirReference);
    procedure SetLastReceivedOn(value : TDateAndTime);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property periodStart : TDateAndTime read FPeriodStart write SetPeriodStart;
    property periodEnd : TDateAndTime read FPeriodEnd write SetPeriodEnd;
    property measure : TFhirReference read FMeasure write SetMeasure;
    property reportType : String read FReportType write FReportType;
    property patient : TFhirReference read FPatient write SetPatient;
    property practitioner : TFhirReference read FPractitioner write SetPractitioner;
    property lastReceivedOn : TDateAndTime read FLastReceivedOn write SetLastReceivedOn;
  end;

  TFHIREvaluateMeasureOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirMeasureReport;
    procedure SetReturn(value : TFhirMeasureReport);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
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
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
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
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirBundle read FReturn write SetReturn;
  end;

  //Operation stats (Observation Statistics)
  TFHIRStatsOpRequest = class (TFHIROperationRequest)
  private
    FPatient : String;
    FCode : String;
    FDuration : String;
    FParamsList : TList<String>;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property patient : String read FPatient write FPatient;
    property code : String read FCode write FCode;
    property duration : String read FDuration write FDuration;
    property paramsList : TList<String> read FParamsList;
  end;

  TFHIRStatsOpResponse = class (TFHIROperationResponse)
  private
    FObsList : TAdvList<TFhirObservation>;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property obsList : TAdvList<TFhirObservation> read FObsList;
  end;

  //Operation match (Find patient matches using MPI based logic)
  TFHIRMatchOpRequest = class (TFHIROperationRequest)
  private
    FPatient : TFhirPatient;
    FFamily : String;
    FGiven : String;
    FGender : String;
    FBirthdate : String;
    FIdentifier : String;
    FTelecom : String;
    FPhone : String;
    FEmail : String;
    FPostcode : String;
    FUserid : String;
    FCount : String;
    procedure SetPatient(value : TFhirPatient);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property patient : TFhirPatient read FPatient write SetPatient;
    property family : String read FFamily write FFamily;
    property given : String read FGiven write FGiven;
    property gender : String read FGender write FGender;
    property birthdate : String read FBirthdate write FBirthdate;
    property identifier : String read FIdentifier write FIdentifier;
    property telecom : String read FTelecom write FTelecom;
    property phone : String read FPhone write FPhone;
    property email : String read FEmail write FEmail;
    property postcode : String read FPostcode write FPostcode;
    property userid : String read FUserid write FUserid;
    property count : String read FCount write FCount;
  end;

  TFHIRMatchOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirBundle;
    procedure SetReturn(value : TFhirBundle);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
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
    FContentList : TAdvList<TFhirReference>;
    FLocal : Boolean;
    procedure SetQuestionnaire(value : TFhirQuestionnaire);
    procedure SetQuestionnaireRef(value : TFhirReference);
    procedure SetSubject(value : TFhirReference);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property identifier : String read FIdentifier write FIdentifier;
    property questionnaire : TFhirQuestionnaire read FQuestionnaire write SetQuestionnaire;
    property questionnaireRef : TFhirReference read FQuestionnaireRef write SetQuestionnaireRef;
    property subject : TFhirReference read FSubject write SetSubject;
    property contentList : TAdvList<TFhirReference> read FContentList;
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
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property questionnaire : TFhirQuestionnaireResponse read FQuestionnaire write SetQuestionnaire;
    property issues : TFhirOperationOutcome read FIssues write SetIssues;
  end;

  //Operation populatehtml (Geneate HTML for Questionnaire)
  TFHIRPopulatehtmlOpRequest = class (TFHIROperationRequest)
  private
    FIdentifier : String;
    FQuestionnaire : TFhirQuestionnaire;
    FQuestionnaireRef : TFhirReference;
    FContentList : TAdvList<TFhirReference>;
    FLocal : Boolean;
    procedure SetQuestionnaire(value : TFhirQuestionnaire);
    procedure SetQuestionnaireRef(value : TFhirReference);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property identifier : String read FIdentifier write FIdentifier;
    property questionnaire : TFhirQuestionnaire read FQuestionnaire write SetQuestionnaire;
    property questionnaireRef : TFhirReference read FQuestionnaireRef write SetQuestionnaireRef;
    property contentList : TAdvList<TFhirReference> read FContentList;
    property local : Boolean read FLocal write FLocal;
  end;

  TFHIRPopulatehtmlOpResponse = class (TFHIROperationResponse)
  private
    FForm : String;
    FIssues : TFhirOperationOutcome;
    procedure SetIssues(value : TFhirOperationOutcome);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property form : String read FForm write FForm;
    property issues : TFhirOperationOutcome read FIssues write SetIssues;
  end;

  //Operation populatelink (Generate a link to a Questionnaire completion webpage)
  TFHIRPopulatelinkOpRequest = class (TFHIROperationRequest)
  private
    FIdentifier : String;
    FQuestionnaire : TFhirQuestionnaire;
    FQuestionnaireRef : TFhirReference;
    FContentList : TAdvList<TFhirReference>;
    FLocal : Boolean;
    procedure SetQuestionnaire(value : TFhirQuestionnaire);
    procedure SetQuestionnaireRef(value : TFhirReference);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property identifier : String read FIdentifier write FIdentifier;
    property questionnaire : TFhirQuestionnaire read FQuestionnaire write SetQuestionnaire;
    property questionnaireRef : TFhirReference read FQuestionnaireRef write SetQuestionnaireRef;
    property contentList : TAdvList<TFhirReference> read FContentList;
    property local : Boolean read FLocal write FLocal;
  end;

  TFHIRPopulatelinkOpResponse = class (TFHIROperationResponse)
  private
    FLink_ : TFhirBinary;
    FIssues : TFhirOperationOutcome;
    procedure SetLink_(value : TFhirBinary);
    procedure SetIssues(value : TFhirOperationOutcome);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property link_ : TFhirBinary read FLink_ write SetLink_;
    property issues : TFhirOperationOutcome read FIssues write SetIssues;
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
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
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
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
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
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
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
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirResource read FReturn write SetReturn;
  end;

  //Operation reserve (Reserve Task)
  TFHIRReserveOpRequest = class (TFHIROperationRequest)
  private
    FOwner : TFhirReference;
    procedure SetOwner(value : TFhirReference);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property owner : TFhirReference read FOwner write SetOwner;
  end;

  TFHIRReserveOpResponse = class (TFHIROperationResponse)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  //Operation start (Start Task)
  TFHIRStartOpRequest = class (TFHIROperationRequest)
  private
    FOwner : TFhirReference;
    procedure SetOwner(value : TFhirReference);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property owner : TFhirReference read FOwner write SetOwner;
  end;

  TFHIRStartOpResponse = class (TFHIROperationResponse)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  //Operation finish (Finish Task)
  TFHIRFinishOpRequest = class (TFHIROperationRequest)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  TFHIRFinishOpResponse = class (TFHIROperationResponse)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  //Operation fail (Fail Task)
  TFHIRFailOpRequest = class (TFHIROperationRequest)
  private
    FReason : TFhirCodeableConcept;
    procedure SetReason(value : TFhirCodeableConcept);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property reason : TFhirCodeableConcept read FReason write SetReason;
  end;

  TFHIRFailOpResponse = class (TFHIROperationResponse)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  //Operation release (Release Task)
  TFHIRReleaseOpRequest = class (TFHIROperationRequest)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  TFHIRReleaseOpResponse = class (TFHIROperationResponse)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  //Operation suspend (Suspend Task)
  TFHIRSuspendOpRequest = class (TFHIROperationRequest)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  TFHIRSuspendOpResponse = class (TFHIROperationResponse)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  //Operation resume (Resume Task)
  TFHIRResumeOpRequest = class (TFHIROperationRequest)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  TFHIRResumeOpResponse = class (TFHIROperationResponse)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  //Operation cancel (Cancel Task)
  TFHIRCancelOpRequest = class (TFHIROperationRequest)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  TFHIRCancelOpResponse = class (TFHIROperationResponse)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  //Operation stop (Stop Task)
  TFHIRStopOpRequest = class (TFHIROperationRequest)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  TFHIRStopOpResponse = class (TFHIROperationResponse)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  //Operation set-input (Set Task Input)
  TFHIRSetInputOpInput = class (TFHIROperationObject)
  private
    FName : String;
    FValueX : TFhirType;
    procedure SetValueX(value : TFhirType);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload;
    constructor Create(params : TFhirParametersParameter); overload;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter;
    property name : String read FName write FName;
    property valueX : TFhirType read FValueX write SetValueX;
  end;

  TFHIRSetInputOpRequest = class (TFHIROperationRequest)
  private
    FInputList : TAdvList<TFHIRSetInputOpInput>;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property inputList : TAdvList<TFHIRSetInputOpInput> read FInputList;
  end;

  TFHIRSetInputOpResponse = class (TFHIROperationResponse)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  //Operation set-output (Set Task Output)
  TFHIRSetOutputOpRequest = class (TFHIROperationRequest)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
  end;

  TFHIRSetOutputOpOutput = class (TFHIROperationObject)
  private
    FName : String;
    FValueX : TFhirType;
    procedure SetValueX(value : TFhirType);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload;
    constructor Create(params : TFhirParametersParameter); overload;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter;
    property name : String read FName write FName;
    property valueX : TFhirType read FValueX write SetValueX;
  end;

  TFHIRSetOutputOpResponse = class (TFHIROperationResponse)
  private
    FOutputList : TAdvList<TFHIRSetOutputOpOutput>;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property outputList : TAdvList<TFHIRSetOutputOpOutput> read FOutputList;
  end;

implementation

uses
  FHIRUtilities;

procedure TFHIRLookupOpRequest.SetCoding(value : TFhirCoding);
begin
  FCoding.free;
  FCoding := value;
end;

procedure TFHIRLookupOpRequest.SetDate(value : TDateAndTime);
begin
  FDate.free;
  FDate := value;
end;

constructor TFHIRLookupOpRequest.create;
begin
  inherited create();
  FProperty_List := TList<String>.create;
end;

procedure TFHIRLookupOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FCode := params.str['code'];
  FSystem := params.str['system'];
  FVersion := params.str['version'];
  FCoding := (params.param['coding'].value as TFhirCoding).Link; {ob.5d}
  FDate := (params.param['date'].value as TFHIRDateTime).value;
  for p in params.parameterList do
    if p.name = 'property' then
      FProperty_List.Add((p.value as TFhirCode).value);{ob.1}
  loadExtensions(params);
end;

procedure TFHIRLookupOpRequest.load(params : TParseMap);
begin
  FCode := params.getVar('code');
  FSystem := params.getVar('system');
  FVersion := params.getVar('version');
  loadExtensions(params);
end;

destructor TFHIRLookupOpRequest.Destroy;
begin
  FCoding.free;
  FDate.free;
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
      result.addParameter('code', TFHIRString.create(FCode));{oz.5f}
    if (FSystem <> '') then
      result.addParameter('system', TFHIRString.create(FSystem));{oz.5f}
    if (FVersion <> '') then
      result.addParameter('version', TFHIRString.create(FVersion));{oz.5f}
    if (FCoding <> nil) then
      result.addParameter('coding', FCoding.Link);{oz.5d}
    if (FDate <> nil) then
      result.addParameter('date', TFHIRDateTime.create(FDate));{oz.5e}
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
  result := StringArrayExists(['code', 'system', 'version', 'coding', 'date', 'property'], name);
end;

constructor TFHIRLookupOpProperty_.create;
begin
  inherited create();
end;

constructor TFHIRLookupOpProperty_.create(params : TFhirParametersParameter);
begin
  inherited create();
  FCode := params.str['code'];
  FValue := params.str['value'];
  FDescription := params.str['description'];
  loadExtensions(params);
end;

destructor TFHIRLookupOpProperty_.Destroy;
begin
  inherited;
end;

function TFHIRLookupOpProperty_.asParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FCode <> '') then
      result.addParameter('code', TFHIRString.create(FCode));{oz.5f}
    if (FValue <> '') then
      result.addParameter('value', TFHIRString.create(FValue));{oz.5f}
    if (FDescription <> '') then
      result.addParameter('description', TFHIRString.create(FDescription));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRLookupOpProperty_.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['code', 'value', 'description'], name);
end;

procedure TFHIRLookupOpDesignation.SetUse(value : TFhirCoding);
begin
  FUse.free;
  FUse := value;
end;

constructor TFHIRLookupOpDesignation.create;
begin
  inherited create();
end;

constructor TFHIRLookupOpDesignation.create(params : TFhirParametersParameter);
begin
  inherited create();
  FLanguage := params.str['language'];
  FUse := (params.param['use'].value as TFhirCoding).Link; {ob.5d}
  FValue := params.str['value'];
  loadExtensions(params);
end;

destructor TFHIRLookupOpDesignation.Destroy;
begin
  FUse.free;
  inherited;
end;

function TFHIRLookupOpDesignation.asParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FLanguage <> '') then
      result.addParameter('language', TFHIRString.create(FLanguage));{oz.5f}
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

function TFHIRLookupOpDesignation.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['language', 'use', 'value'], name);
end;

constructor TFHIRLookupOpResponse.create;
begin
  inherited create();
  FProperty_List := TAdvList<TFHIRLookupOpProperty_>.create;
  FDesignationList := TAdvList<TFHIRLookupOpDesignation>.create;
end;

procedure TFHIRLookupOpResponse.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FName := params.str['name'];
  FVersion := params.str['version'];
  FDisplay := params.str['display'];
  for p in params.parameterList do
    if p.name = 'property' then
      FProperty_List.Add(TFHIRLookupOpProperty_.create(p));{a}
  for p in params.parameterList do
    if p.name = 'designation' then
      FDesignationList.Add(TFHIRLookupOpDesignation.create(p));{a}
  loadExtensions(params);
end;

procedure TFHIRLookupOpResponse.load(params : TParseMap);
begin
  FName := params.getVar('name');
  FVersion := params.getVar('version');
  FDisplay := params.getVar('display');
  loadExtensions(params);
end;

destructor TFHIRLookupOpResponse.Destroy;
begin
  FProperty_List.free;
  FDesignationList.free;
  inherited;
end;

function TFHIRLookupOpResponse.asParams : TFhirParameters;
var
  v1 : TFHIRLookupOpProperty_;
  v2 : TFHIRLookupOpDesignation;
begin
  result := TFHIRParameters.create;
  try
    if (FName <> '') then
      result.addParameter('name', TFHIRString.create(FName));{oz.5f}
    if (FVersion <> '') then
      result.addParameter('version', TFHIRString.create(FVersion));{oz.5f}
    if (FDisplay <> '') then
      result.addParameter('display', TFHIRString.create(FDisplay));{oz.5f}
    for v1 in FProperty_List do
      result.AddParameter(v1.asParams('property'));
    for v2 in FDesignationList do
      result.AddParameter(v2.asParams('designation'));
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRLookupOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['name', 'version', 'display', 'property', 'designation'], name);
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
  FCodingA := (params.param['codingA'].value as TFhirCoding).Link; {ob.5d}
  FCodingB := (params.param['codingB'].value as TFhirCoding).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIRSubsumesOpRequest.load(params : TParseMap);
begin
  FCodeA := params.getVar('codeA');
  FCodeB := params.getVar('codeB');
  FSystem := params.getVar('system');
  FVersion := params.getVar('version');
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
      result.addParameter('codeA', TFHIRString.create(FCodeA));{oz.5f}
    if (FCodeB <> '') then
      result.addParameter('codeB', TFHIRString.create(FCodeB));{oz.5f}
    if (FSystem <> '') then
      result.addParameter('system', TFHIRString.create(FSystem));{oz.5f}
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

constructor TFHIRSubsumesOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRSubsumesOpResponse.load(params : TFHIRParameters);
begin
  FOutcome := params.str['outcome'];
  loadExtensions(params);
end;

procedure TFHIRSubsumesOpResponse.load(params : TParseMap);
begin
  FOutcome := params.getVar('outcome');
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
      result.addParameter('outcome', TFHIRString.create(FOutcome));{oz.5f}
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

constructor TFHIRInferOpConcept.create;
begin
  inherited create();
end;

constructor TFHIRInferOpConcept.create(params : TFhirParametersParameter);
begin
  inherited create();
  FProperty_ := params.str['property'];
  FCode := params.str['code'];
  loadExtensions(params);
end;

destructor TFHIRInferOpConcept.Destroy;
begin
  inherited;
end;

function TFHIRInferOpConcept.asParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FProperty_ <> '') then
      result.addParameter('property', TFHIRString.create(FProperty_));{oz.5f}
    if (FCode <> '') then
      result.addParameter('code', TFHIRString.create(FCode));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRInferOpConcept.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['property', 'code'], name);
end;

constructor TFHIRInferOpRequest.create;
begin
  inherited create();
  FConceptList := TAdvList<TFHIRInferOpConcept>.create;
  FProperty_List := TList<String>.create;
end;

procedure TFHIRInferOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FSystem := params.str['system'];
  FVersion := params.str['version'];
  for p in params.parameterList do
    if p.name = 'concept' then
      FConceptList.Add(TFHIRInferOpConcept.create(p));{a}
  for p in params.parameterList do
    if p.name = 'property' then
      FProperty_List.Add((p.value as TFhirCode).value);{ob.1}
  loadExtensions(params);
end;

procedure TFHIRInferOpRequest.load(params : TParseMap);
begin
  FSystem := params.getVar('system');
  FVersion := params.getVar('version');
  loadExtensions(params);
end;

destructor TFHIRInferOpRequest.Destroy;
begin
  FConceptList.free;
  FProperty_List.free;
  inherited;
end;

function TFHIRInferOpRequest.asParams : TFhirParameters;
var
  v1 : TFHIRInferOpConcept;
  v2 : String;
begin
  result := TFHIRParameters.create;
  try
    if (FSystem <> '') then
      result.addParameter('system', TFHIRString.create(FSystem));{oz.5f}
    if (FVersion <> '') then
      result.addParameter('version', TFHIRString.create(FVersion));{oz.5f}
    for v1 in FConceptList do
      result.AddParameter(v1.asParams('concept'));
    for v2 in FProperty_List do
      result.AddParameter('property', TFhirCode.create(v2));
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRInferOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['system', 'version', 'concept', 'property'], name);
end;

constructor TFHIRInferOpOutput.create;
begin
  inherited create();
end;

constructor TFHIRInferOpOutput.create(params : TFhirParametersParameter);
begin
  inherited create();
  FProperty_ := params.str['property'];
  FCode := params.str['code'];
  FDisplay := params.str['display'];
  loadExtensions(params);
end;

destructor TFHIRInferOpOutput.Destroy;
begin
  inherited;
end;

function TFHIRInferOpOutput.asParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FProperty_ <> '') then
      result.addParameter('property', TFHIRString.create(FProperty_));{oz.5f}
    if (FCode <> '') then
      result.addParameter('code', TFHIRString.create(FCode));{oz.5f}
    if (FDisplay <> '') then
      result.addParameter('display', TFHIRString.create(FDisplay));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRInferOpOutput.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['property', 'code', 'display'], name);
end;

constructor TFHIRInferOpResponse.create;
begin
  inherited create();
  FOutputList := TAdvList<TFHIRInferOpOutput>.create;
end;

procedure TFHIRInferOpResponse.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FMessage := params.str['message'];
  for p in params.parameterList do
    if p.name = 'output' then
      FOutputList.Add(TFHIRInferOpOutput.create(p));{a}
  loadExtensions(params);
end;

procedure TFHIRInferOpResponse.load(params : TParseMap);
begin
  FMessage := params.getVar('message');
  loadExtensions(params);
end;

destructor TFHIRInferOpResponse.Destroy;
begin
  FOutputList.free;
  inherited;
end;

function TFHIRInferOpResponse.asParams : TFhirParameters;
var
  v1 : TFHIRInferOpOutput;
begin
  result := TFHIRParameters.create;
  try
    if (FMessage <> '') then
      result.addParameter('message', TFHIRString.create(FMessage));{oz.5f}
    for v1 in FOutputList do
      result.AddParameter(v1.asParams('output'));
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRInferOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['message', 'output'], name);
end;

procedure TFHIRExpandOpRequest.SetValueSet(value : TFhirValueSet);
begin
  FValueSet.free;
  FValueSet := value;
end;

procedure TFHIRExpandOpRequest.SetDate(value : TDateAndTime);
begin
  FDate.free;
  FDate := value;
end;

constructor TFHIRExpandOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRExpandOpRequest.load(params : TFHIRParameters);
begin
  FIdentifier := params.str['identifier'];
  FValueSet := (params.res['valueSet'] as TFhirValueSet).Link;{ob.5a}
  FContext := params.str['context'];
  FFilter := params.str['filter'];
  FProfile := params.str['profile'];
  FDate := (params.param['date'].value as TFHIRDateTime).value;
  FOffset := params.str['offset'];
  FCount := params.str['count'];
  loadExtensions(params);
end;

procedure TFHIRExpandOpRequest.load(params : TParseMap);
begin
  FIdentifier := params.getVar('identifier');
  FContext := params.getVar('context');
  FFilter := params.getVar('filter');
  FProfile := params.getVar('profile');
  FOffset := params.getVar('offset');
  FCount := params.getVar('count');
  loadExtensions(params);
end;

destructor TFHIRExpandOpRequest.Destroy;
begin
  FValueSet.free;
  FDate.free;
  inherited;
end;

function TFHIRExpandOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FIdentifier <> '') then
      result.addParameter('identifier', TFHIRString.create(FIdentifier));{oz.5f}
    if (FValueSet <> nil) then
      result.addParameter('valueSet', FValueSet.Link);{oz.5a}
    if (FContext <> '') then
      result.addParameter('context', TFHIRString.create(FContext));{oz.5f}
    if (FFilter <> '') then
      result.addParameter('filter', TFHIRString.create(FFilter));{oz.5f}
    if (FProfile <> '') then
      result.addParameter('profile', TFHIRString.create(FProfile));{oz.5f}
    if (FDate <> nil) then
      result.addParameter('date', TFHIRDateTime.create(FDate));{oz.5e}
    if (FOffset <> '') then
      result.addParameter('offset', TFHIRString.create(FOffset));{oz.5f}
    if (FCount <> '') then
      result.addParameter('count', TFHIRString.create(FCount));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRExpandOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['identifier', 'valueSet', 'context', 'filter', 'profile', 'date', 'offset', 'count'], name);
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

procedure TFHIRExpandOpResponse.load(params : TParseMap);
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

procedure TFHIRValidateCodeOpRequest.SetDate(value : TDateAndTime);
begin
  FDate.free;
  FDate := value;
end;

constructor TFHIRValidateCodeOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRValidateCodeOpRequest.load(params : TFHIRParameters);
begin
  FIdentifier := params.str['identifier'];
  FContext := params.str['context'];
  FValueSet := (params.res['valueSet'] as TFhirValueSet).Link;{ob.5a}
  FCode := params.str['code'];
  FSystem := params.str['system'];
  FVersion := params.str['version'];
  FDisplay := params.str['display'];
  FCoding := (params.param['coding'].value as TFhirCoding).Link; {ob.5d}
  FCodeableConcept := (params.param['codeableConcept'].value as TFhirCodeableConcept).Link; {ob.5d}
  FDate := (params.param['date'].value as TFHIRDateTime).value;
  FAbstract := params.bool['abstract'];
  loadExtensions(params);
end;

procedure TFHIRValidateCodeOpRequest.load(params : TParseMap);
begin
  FIdentifier := params.getVar('identifier');
  FContext := params.getVar('context');
  FCode := params.getVar('code');
  FSystem := params.getVar('system');
  FVersion := params.getVar('version');
  FDisplay := params.getVar('display');
  FAbstract := StrToBool(params.getVar('abstract'));
  loadExtensions(params);
end;

destructor TFHIRValidateCodeOpRequest.Destroy;
begin
  FValueSet.free;
  FCoding.free;
  FCodeableConcept.free;
  FDate.free;
  inherited;
end;

function TFHIRValidateCodeOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FIdentifier <> '') then
      result.addParameter('identifier', TFHIRString.create(FIdentifier));{oz.5f}
    if (FContext <> '') then
      result.addParameter('context', TFHIRString.create(FContext));{oz.5f}
    if (FValueSet <> nil) then
      result.addParameter('valueSet', FValueSet.Link);{oz.5a}
    if (FCode <> '') then
      result.addParameter('code', TFHIRString.create(FCode));{oz.5f}
    if (FSystem <> '') then
      result.addParameter('system', TFHIRString.create(FSystem));{oz.5f}
    if (FVersion <> '') then
      result.addParameter('version', TFHIRString.create(FVersion));{oz.5f}
    if (FDisplay <> '') then
      result.addParameter('display', TFHIRString.create(FDisplay));{oz.5f}
    if (FCoding <> nil) then
      result.addParameter('coding', FCoding.Link);{oz.5d}
    if (FCodeableConcept <> nil) then
      result.addParameter('codeableConcept', FCodeableConcept.Link);{oz.5d}
    if (FDate <> nil) then
      result.addParameter('date', TFHIRDateTime.create(FDate));{oz.5e}
      result.addParameter('abstract', TFHIRBoolean.create(FAbstract));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRValidateCodeOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['identifier', 'context', 'valueSet', 'code', 'system', 'version', 'display', 'coding', 'codeableConcept', 'date', 'abstract'], name);
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

procedure TFHIRValidateCodeOpResponse.load(params : TParseMap);
begin
  FResult := StrToBool(params.getVar('result'));
  FMessage := params.getVar('message');
  FDisplay := params.getVar('display');
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

procedure TFHIRValidateOpRequest.load(params : TParseMap);
begin
  FMode := params.getVar('mode');
  FProfile := params.getVar('profile');
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
      result.addParameter('mode', TFHIRString.create(FMode));{oz.5f}
    if (FProfile <> '') then
      result.addParameter('profile', TFHIRString.create(FProfile));{oz.5f}
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

procedure TFHIRValidateOpResponse.load(params : TParseMap);
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

constructor TFHIRMetaOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRMetaOpRequest.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRMetaOpRequest.load(params : TParseMap);
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
  FReturn := (params.param['return'].value as TFhirMeta).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIRMetaOpResponse.load(params : TParseMap);
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
  FMeta := (params.param['meta'].value as TFhirMeta).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIRMetaAddOpRequest.load(params : TParseMap);
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
  FReturn := (params.param['return'].value as TFhirMeta).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIRMetaAddOpResponse.load(params : TParseMap);
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
  FMeta := (params.param['meta'].value as TFhirMeta).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIRMetaDeleteOpRequest.load(params : TParseMap);
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
  FReturn := (params.param['return'].value as TFhirMeta).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIRMetaDeleteOpResponse.load(params : TParseMap);
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

procedure TFHIRCdsHookOpRequest.SetActivity(value : TFhirCoding);
begin
  FActivity.free;
  FActivity := value;
end;

constructor TFHIRCdsHookOpOauth.create;
begin
  inherited create();
end;

constructor TFHIRCdsHookOpOauth.create(params : TFhirParametersParameter);
begin
  inherited create();
  FToken := params.str['token'];
  FScope := params.str['scope'];
  FExpires := params.str['expires'];
  loadExtensions(params);
end;

destructor TFHIRCdsHookOpOauth.Destroy;
begin
  inherited;
end;

function TFHIRCdsHookOpOauth.asParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FToken <> '') then
      result.addParameter('token', TFHIRString.create(FToken));{oz.5f}
    if (FScope <> '') then
      result.addParameter('scope', TFHIRString.create(FScope));{oz.5f}
    if (FExpires <> '') then
      result.addParameter('expires', TFHIRString.create(FExpires));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRCdsHookOpOauth.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['token', 'scope', 'expires'], name);
end;

procedure TFHIRCdsHookOpRequest.SetOauth(value : TFHIRCdsHookOpOauth);
begin
  FOauth.free;
  FOauth := value;
end;

procedure TFHIRCdsHookOpRequest.SetPreFetchData(value : TFhirBundle);
begin
  FPreFetchData.free;
  FPreFetchData := value;
end;

constructor TFHIRCdsHookOpSource.create;
begin
  inherited create();
end;

constructor TFHIRCdsHookOpSource.create(params : TFhirParametersParameter);
begin
  inherited create();
  FLabel_ := params.str['label'];
  FUrl := params.str['url'];
  loadExtensions(params);
end;

destructor TFHIRCdsHookOpSource.Destroy;
begin
  inherited;
end;

function TFHIRCdsHookOpSource.asParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FLabel_ <> '') then
      result.addParameter('label', TFHIRString.create(FLabel_));{oz.5f}
    if (FUrl <> '') then
      result.addParameter('url', TFHIRString.create(FUrl));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRCdsHookOpSource.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['label', 'url'], name);
end;

procedure TFHIRCdsHookOpCard.SetSource(value : TFHIRCdsHookOpSource);
begin
  FSource.free;
  FSource := value;
end;

constructor TFHIRCdsHookOpSuggestion.create;
begin
  inherited create();
  FCreate_List := TAdvList<TFhirResource>.create;
  FDeleteList := TList<String>.create;
end;

constructor TFHIRCdsHookOpSuggestion.create(params : TFhirParametersParameter);
var
  p : TFhirParametersParameter;
begin
  inherited create();
  FCreate_List := TAdvList<TFhirResource>.create;
  FDeleteList := TList<String>.create;
  FLabel_ := params.str['label'];
  for p in params.partList do
    if p.name = 'create' then
      FCreate_List.Add((p.resource as TFhirResource).Link);{ob.2}
  for p in params.partList do
    if p.name = 'delete' then
      FDeleteList.Add((p.value as TFhirId).value);{ob.1}
  loadExtensions(params);
end;

destructor TFHIRCdsHookOpSuggestion.Destroy;
begin
  FCreate_List.free;
  FDeleteList.free;
  inherited;
end;

function TFHIRCdsHookOpSuggestion.asParams(name : String) : TFhirParametersParameter;
var
  v1 : TFhirResource;
  v2 : String;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FLabel_ <> '') then
      result.addParameter('label', TFHIRString.create(FLabel_));{oz.5f}
    for v1 in FCreate_List do
      result.AddParameter('create', v1.Link);
    for v2 in FDeleteList do
      result.AddParameter('delete', TFhirId.create(v2));
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRCdsHookOpSuggestion.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['label', 'create', 'delete'], name);
end;

constructor TFHIRCdsHookOpLink_.create;
begin
  inherited create();
end;

constructor TFHIRCdsHookOpLink_.create(params : TFhirParametersParameter);
begin
  inherited create();
  FLabel_ := params.str['label'];
  FUrl := params.str['url'];
  loadExtensions(params);
end;

destructor TFHIRCdsHookOpLink_.Destroy;
begin
  inherited;
end;

function TFHIRCdsHookOpLink_.asParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FLabel_ <> '') then
      result.addParameter('label', TFHIRString.create(FLabel_));{oz.5f}
    if (FUrl <> '') then
      result.addParameter('url', TFHIRString.create(FUrl));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRCdsHookOpLink_.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['label', 'url'], name);
end;

constructor TFHIRCdsHookOpCard.create;
begin
  inherited create();
  FSuggestionList := TAdvList<TFHIRCdsHookOpSuggestion>.create;
  FLink_List := TAdvList<TFHIRCdsHookOpLink_>.create;
end;

constructor TFHIRCdsHookOpCard.create(params : TFhirParametersParameter);
var
  p : TFhirParametersParameter;
begin
  inherited create();
  FSuggestionList := TAdvList<TFHIRCdsHookOpSuggestion>.create;
  FLink_List := TAdvList<TFHIRCdsHookOpLink_>.create;
  FSummary := params.str['summary'];
  FDetail := params.str['detail'];
  FIndicator := params.str['indicator'];
  FSource := TFHIRCdsHookOpSource.create(params.param['source']); {ob.5c}
  for p in params.partList do
    if p.name = 'suggestion' then
      FSuggestionList.Add(TFHIRCdsHookOpSuggestion.create(p));{a}
  for p in params.partList do
    if p.name = 'link' then
      FLink_List.Add(TFHIRCdsHookOpLink_.create(p));{a}
  loadExtensions(params);
end;

destructor TFHIRCdsHookOpCard.Destroy;
begin
  FSource.free;
  FSuggestionList.free;
  FLink_List.free;
  inherited;
end;

function TFHIRCdsHookOpCard.asParams(name : String) : TFhirParametersParameter;
var
  v1 : TFHIRCdsHookOpSuggestion;
  v2 : TFHIRCdsHookOpLink_;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FSummary <> '') then
      result.addParameter('summary', TFHIRString.create(FSummary));{oz.5f}
    if (FDetail <> '') then
      result.addParameter('detail', TFHIRString.create(FDetail));{oz.5f}
    if (FIndicator <> '') then
      result.addParameter('indicator', TFHIRString.create(FIndicator));{oz.5f}
    if (FSource <> nil) then
      result.addParameter(FSource.asParams('source'));{oz.5c}
    for v1 in FSuggestionList do
      result.AddParameter(v1.asParams('suggestion'));
    for v2 in FLink_List do
      result.AddParameter(v2.asParams('link'));
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRCdsHookOpCard.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['summary', 'detail', 'indicator', 'source', 'suggestion', 'link'], name);
end;

procedure TFHIRCdsHookOpRequest.SetCard(value : TFHIRCdsHookOpCard);
begin
  FCard.free;
  FCard := value;
end;

constructor TFHIRCdsHookOpRequest.create;
begin
  inherited create();
  FContextList := TAdvList<TFhirResource>.create;
end;

procedure TFHIRCdsHookOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FActivity := (params.param['activity'].value as TFhirCoding).Link; {ob.5d}
  FActivityInstance := params.str['activityInstance'];
  FFhirServer := params.str['fhirServer'];
  FOauth := TFHIRCdsHookOpOauth.create(params.param['oauth']); {ob.5c}
  FRedirect := params.str['redirect'];
  FUser := params.str['user'];
  FPatient := params.str['patient'];
  FEncounter := params.str['encounter'];
  for p in params.parameterList do
    if p.name = 'context' then
      FContextList.Add((p.resource as TFhirResource).Link);{ob.2}
  FPreFetchData := (params.res['preFetchData'] as TFhirBundle).Link;{ob.5a}
  FCard := TFHIRCdsHookOpCard.create(params.param['card']); {ob.5c}
  loadExtensions(params);
end;

procedure TFHIRCdsHookOpRequest.load(params : TParseMap);
begin
  FActivityInstance := params.getVar('activityInstance');
  FFhirServer := params.getVar('fhirServer');
  FRedirect := params.getVar('redirect');
  FUser := params.getVar('user');
  FPatient := params.getVar('patient');
  FEncounter := params.getVar('encounter');
  loadExtensions(params);
end;

destructor TFHIRCdsHookOpRequest.Destroy;
begin
  FActivity.free;
  FOauth.free;
  FContextList.free;
  FPreFetchData.free;
  FCard.free;
  inherited;
end;

function TFHIRCdsHookOpRequest.asParams : TFhirParameters;
var
  v1 : TFhirResource;
begin
  result := TFHIRParameters.create;
  try
    if (FActivity <> nil) then
      result.addParameter('activity', FActivity.Link);{oz.5d}
    if (FActivityInstance <> '') then
      result.addParameter('activityInstance', TFHIRString.create(FActivityInstance));{oz.5f}
    if (FFhirServer <> '') then
      result.addParameter('fhirServer', TFHIRString.create(FFhirServer));{oz.5f}
    if (FOauth <> nil) then
      result.addParameter(FOauth.asParams('oauth'));{oz.5c}
    if (FRedirect <> '') then
      result.addParameter('redirect', TFHIRString.create(FRedirect));{oz.5f}
    if (FUser <> '') then
      result.addParameter('user', TFHIRString.create(FUser));{oz.5f}
    if (FPatient <> '') then
      result.addParameter('patient', TFHIRString.create(FPatient));{oz.5f}
    if (FEncounter <> '') then
      result.addParameter('encounter', TFHIRString.create(FEncounter));{oz.5f}
    for v1 in FContextList do
      result.AddParameter('context', v1.Link);
    if (FPreFetchData <> nil) then
      result.addParameter('preFetchData', FPreFetchData.Link);{oz.5a}
    if (FCard <> nil) then
      result.addParameter(FCard.asParams('card'));{oz.5c}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRCdsHookOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['activity', 'activityInstance', 'fhirServer', 'oauth', 'redirect', 'user', 'patient', 'encounter', 'context', 'preFetchData', 'card'], name);
end;

constructor TFHIRCdsHookOpDecision.create;
begin
  inherited create();
  FCreate_List := TAdvList<TFhirResource>.create;
  FDeleteList := TList<String>.create;
end;

constructor TFHIRCdsHookOpDecision.create(params : TFhirParametersParameter);
var
  p : TFhirParametersParameter;
begin
  inherited create();
  FCreate_List := TAdvList<TFhirResource>.create;
  FDeleteList := TList<String>.create;
  for p in params.partList do
    if p.name = 'create' then
      FCreate_List.Add((p.resource as TFhirResource).Link);{ob.2}
  for p in params.partList do
    if p.name = 'delete' then
      FDeleteList.Add((p.value as TFhirId).value);{ob.1}
  loadExtensions(params);
end;

destructor TFHIRCdsHookOpDecision.Destroy;
begin
  FCreate_List.free;
  FDeleteList.free;
  inherited;
end;

function TFHIRCdsHookOpDecision.asParams(name : String) : TFhirParametersParameter;
var
  v1 : TFhirResource;
  v2 : String;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    for v1 in FCreate_List do
      result.AddParameter('create', v1.Link);
    for v2 in FDeleteList do
      result.AddParameter('delete', TFhirId.create(v2));
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRCdsHookOpDecision.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['create', 'delete'], name);
end;

constructor TFHIRCdsHookOpResponse.create;
begin
  inherited create();
  FDecisionList := TAdvList<TFHIRCdsHookOpDecision>.create;
end;

procedure TFHIRCdsHookOpResponse.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  for p in params.parameterList do
    if p.name = 'decision' then
      FDecisionList.Add(TFHIRCdsHookOpDecision.create(p));{a}
  loadExtensions(params);
end;

procedure TFHIRCdsHookOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRCdsHookOpResponse.Destroy;
begin
  FDecisionList.free;
  inherited;
end;

function TFHIRCdsHookOpResponse.asParams : TFhirParameters;
var
  v1 : TFHIRCdsHookOpDecision;
begin
  result := TFHIRParameters.create;
  try
    for v1 in FDecisionList do
      result.AddParameter(v1.asParams('decision'));
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRCdsHookOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['decision'], name);
end;

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
  FPatient := (params.param['patient'].value as TFhirReference).Link; {ob.5d}
  FEncounter := (params.param['encounter'].value as TFhirReference).Link; {ob.5d}
  FPractitioner := (params.param['practitioner'].value as TFhirReference).Link; {ob.5d}
  FOrganization := (params.param['organization'].value as TFhirReference).Link; {ob.5d}
  FUserType := (params.param['userType'].value as TFhirCodeableConcept).Link; {ob.5d}
  FUserLanguage := (params.param['userLanguage'].value as TFhirCodeableConcept).Link; {ob.5d}
  FUserTaskContext := (params.param['userTaskContext'].value as TFhirCodeableConcept).Link; {ob.5d}
  FSetting := (params.param['setting'].value as TFhirCodeableConcept).Link; {ob.5d}
  FSettingContext := (params.param['settingContext'].value as TFhirCodeableConcept).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIRApplyOpRequest.load(params : TParseMap);
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

procedure TFHIRApplyOpResponse.load(params : TParseMap);
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

constructor TFHIRDataRequirementsOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRDataRequirementsOpRequest.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRDataRequirementsOpRequest.load(params : TParseMap);
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

procedure TFHIRDataRequirementsOpResponse.load(params : TParseMap);
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

constructor TFHIRDocumentOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRDocumentOpRequest.load(params : TFHIRParameters);
begin
  FPersist := params.bool['persist'];
  loadExtensions(params);
end;

procedure TFHIRDocumentOpRequest.load(params : TParseMap);
begin
  FPersist := StrToBool(params.getVar('persist'));
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

constructor TFHIRDocumentOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRDocumentOpResponse.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRDocumentOpResponse.load(params : TParseMap);
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

procedure TFHIRTranslateOpDependency.SetConcept(value : TFhirCodeableConcept);
begin
  FConcept.free;
  FConcept := value;
end;

constructor TFHIRTranslateOpDependency.create;
begin
  inherited create();
end;

constructor TFHIRTranslateOpDependency.create(params : TFhirParametersParameter);
begin
  inherited create();
  FElement := params.str['element'];
  FConcept := (params.param['concept'].value as TFhirCodeableConcept).Link; {ob.5d}
  loadExtensions(params);
end;

destructor TFHIRTranslateOpDependency.Destroy;
begin
  FConcept.free;
  inherited;
end;

function TFHIRTranslateOpDependency.asParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FElement <> '') then
      result.addParameter('element', TFHIRString.create(FElement));{oz.5f}
    if (FConcept <> nil) then
      result.addParameter('concept', FConcept.Link);{oz.5d}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRTranslateOpDependency.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['element', 'concept'], name);
end;

constructor TFHIRTranslateOpRequest.create;
begin
  inherited create();
  FDependencyList := TAdvList<TFHIRTranslateOpDependency>.create;
end;

procedure TFHIRTranslateOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FCode := params.str['code'];
  FSystem := params.str['system'];
  FVersion := params.str['version'];
  FSource := params.str['source'];
  FCoding := (params.param['coding'].value as TFhirCoding).Link; {ob.5d}
  FCodeableConcept := (params.param['codeableConcept'].value as TFhirCodeableConcept).Link; {ob.5d}
  FTarget := params.str['target'];
  FTargetsystem := params.str['targetsystem'];
  for p in params.parameterList do
    if p.name = 'dependency' then
      FDependencyList.Add(TFHIRTranslateOpDependency.create(p));{a}
  FReverse := params.bool['reverse'];
  loadExtensions(params);
end;

procedure TFHIRTranslateOpRequest.load(params : TParseMap);
begin
  FCode := params.getVar('code');
  FSystem := params.getVar('system');
  FVersion := params.getVar('version');
  FSource := params.getVar('source');
  FTarget := params.getVar('target');
  FTargetsystem := params.getVar('targetsystem');
  FReverse := StrToBool(params.getVar('reverse'));
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
  v1 : TFHIRTranslateOpDependency;
begin
  result := TFHIRParameters.create;
  try
    if (FCode <> '') then
      result.addParameter('code', TFHIRString.create(FCode));{oz.5f}
    if (FSystem <> '') then
      result.addParameter('system', TFHIRString.create(FSystem));{oz.5f}
    if (FVersion <> '') then
      result.addParameter('version', TFHIRString.create(FVersion));{oz.5f}
    if (FSource <> '') then
      result.addParameter('source', TFHIRString.create(FSource));{oz.5f}
    if (FCoding <> nil) then
      result.addParameter('coding', FCoding.Link);{oz.5d}
    if (FCodeableConcept <> nil) then
      result.addParameter('codeableConcept', FCodeableConcept.Link);{oz.5d}
    if (FTarget <> '') then
      result.addParameter('target', TFHIRString.create(FTarget));{oz.5f}
    if (FTargetsystem <> '') then
      result.addParameter('targetsystem', TFHIRString.create(FTargetsystem));{oz.5f}
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

procedure TFHIRTranslateOpMatch.SetConcept(value : TFhirCoding);
begin
  FConcept.free;
  FConcept := value;
end;

procedure TFHIRTranslateOpProduct.SetConcept(value : TFhirCoding);
begin
  FConcept.free;
  FConcept := value;
end;

constructor TFHIRTranslateOpProduct.create;
begin
  inherited create();
end;

constructor TFHIRTranslateOpProduct.create(params : TFhirParametersParameter);
begin
  inherited create();
  FElement := params.str['element'];
  FConcept := (params.param['concept'].value as TFhirCoding).Link; {ob.5d}
  loadExtensions(params);
end;

destructor TFHIRTranslateOpProduct.Destroy;
begin
  FConcept.free;
  inherited;
end;

function TFHIRTranslateOpProduct.asParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FElement <> '') then
      result.addParameter('element', TFHIRString.create(FElement));{oz.5f}
    if (FConcept <> nil) then
      result.addParameter('concept', FConcept.Link);{oz.5d}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRTranslateOpProduct.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['element', 'concept'], name);
end;

constructor TFHIRTranslateOpMatch.create;
begin
  inherited create();
  FProductList := TAdvList<TFHIRTranslateOpProduct>.create;
end;

constructor TFHIRTranslateOpMatch.create(params : TFhirParametersParameter);
var
  p : TFhirParametersParameter;
begin
  inherited create();
  FProductList := TAdvList<TFHIRTranslateOpProduct>.create;
  FEquivalence := params.str['equivalence'];
  FConcept := (params.param['concept'].value as TFhirCoding).Link; {ob.5d}
  for p in params.partList do
    if p.name = 'product' then
      FProductList.Add(TFHIRTranslateOpProduct.create(p));{a}
  FSource := params.str['source'];
  loadExtensions(params);
end;

destructor TFHIRTranslateOpMatch.Destroy;
begin
  FConcept.free;
  FProductList.free;
  inherited;
end;

function TFHIRTranslateOpMatch.asParams(name : String) : TFhirParametersParameter;
var
  v1 : TFHIRTranslateOpProduct;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FEquivalence <> '') then
      result.addParameter('equivalence', TFHIRString.create(FEquivalence));{oz.5f}
    if (FConcept <> nil) then
      result.addParameter('concept', FConcept.Link);{oz.5d}
    for v1 in FProductList do
      result.AddParameter(v1.asParams('product'));
    if (FSource <> '') then
      result.addParameter('source', TFHIRString.create(FSource));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRTranslateOpMatch.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['equivalence', 'concept', 'product', 'source'], name);
end;

constructor TFHIRTranslateOpResponse.create;
begin
  inherited create();
  FMatchList := TAdvList<TFHIRTranslateOpMatch>.create;
end;

procedure TFHIRTranslateOpResponse.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FResult := params.bool['result'];
  FMessage := params.str['message'];
  for p in params.parameterList do
    if p.name = 'match' then
      FMatchList.Add(TFHIRTranslateOpMatch.create(p));{a}
  loadExtensions(params);
end;

procedure TFHIRTranslateOpResponse.load(params : TParseMap);
begin
  FResult := StrToBool(params.getVar('result'));
  FMessage := params.getVar('message');
  loadExtensions(params);
end;

destructor TFHIRTranslateOpResponse.Destroy;
begin
  FMatchList.free;
  inherited;
end;

function TFHIRTranslateOpResponse.asParams : TFhirParameters;
var
  v1 : TFHIRTranslateOpMatch;
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

constructor TFHIRClosureOpRequest.create;
begin
  inherited create();
  FConceptList := TAdvList<TFhirCoding>.create;
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

procedure TFHIRClosureOpRequest.load(params : TParseMap);
begin
  FName := params.getVar('name');
  FVersion := params.getVar('version');
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
      result.addParameter('version', TFHIRString.create(FVersion));{oz.5f}
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

procedure TFHIRClosureOpResponse.load(params : TParseMap);
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

constructor TFHIRSubsetOpRequest.create;
begin
  inherited create();
  FResourceList := TList<String>.create;
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

procedure TFHIRSubsetOpRequest.load(params : TParseMap);
begin
  FServer := params.getVar('server');
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
      result.addParameter('server', TFHIRString.create(FServer));{oz.5f}
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

procedure TFHIRSubsetOpResponse.SetReturn(value : TFhirConformance);
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
  FReturn := (params.res['return'] as TFhirConformance).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRSubsetOpResponse.load(params : TParseMap);
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

procedure TFHIRImplementsOpRequest.SetResource(value : TFhirConformance);
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
  FResource := (params.res['resource'] as TFhirConformance).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRImplementsOpRequest.load(params : TParseMap);
begin
  FServer := params.getVar('server');
  FClient := params.getVar('client');
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
      result.addParameter('server', TFHIRString.create(FServer));{oz.5f}
    if (FClient <> '') then
      result.addParameter('client', TFHIRString.create(FClient));{oz.5f}
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

procedure TFHIRImplementsOpResponse.SetReturn(value : TFhirOperationOutcome);
begin
  FReturn.free;
  FReturn := value;
end;

procedure TFHIRImplementsOpResponse.SetIssues(value : TFhirOperationOutcome);
begin
  FIssues.free;
  FIssues := value;
end;

procedure TFHIRImplementsOpResponse.SetUnion(value : TFhirBundle);
begin
  FUnion.free;
  FUnion := value;
end;

procedure TFHIRImplementsOpResponse.SetIntersection(value : TFhirBundle);
begin
  FIntersection.free;
  FIntersection := value;
end;

constructor TFHIRImplementsOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRImplementsOpResponse.load(params : TFHIRParameters);
begin
  FReturn := (params.res['return'] as TFhirOperationOutcome).Link;{ob.5a}
  FIssues := (params.res['issues'] as TFhirOperationOutcome).Link;{ob.5a}
  FUnion := (params.res['union'] as TFhirBundle).Link;{ob.5a}
  FIntersection := (params.res['intersection'] as TFhirBundle).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRImplementsOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRImplementsOpResponse.Destroy;
begin
  FReturn.free;
  FIssues.free;
  FUnion.free;
  FIntersection.free;
  inherited;
end;

function TFHIRImplementsOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
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

function TFHIRImplementsOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return', 'issues', 'union', 'intersection'], name);
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

procedure TFHIRConformsOpRequest.load(params : TParseMap);
begin
  FLeft := params.getVar('left');
  FRight := params.getVar('right');
  FMode := params.getVar('mode');
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
      result.addParameter('left', TFHIRString.create(FLeft));{oz.5f}
    if (FRight <> '') then
      result.addParameter('right', TFHIRString.create(FRight));{oz.5f}
    if (FMode <> '') then
      result.addParameter('mode', TFHIRString.create(FMode));{oz.5f}
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

constructor TFHIRConformsOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRConformsOpResponse.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRConformsOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRConformsOpResponse.Destroy;
begin
  inherited;
end;

function TFHIRConformsOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRConformsOpResponse.isKnownName(name : String) : boolean;
begin
  result := false;
end;

procedure TFHIREvaluateOpRequest.SetEvaluateAtDateTime(value : TDateAndTime);
begin
  FEvaluateAtDateTime.free;
  FEvaluateAtDateTime := value;
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
  FInputDataList := TAdvList<TFhirResource>.create;
end;

procedure TFHIREvaluateOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FRequestId := params.str['requestId'];
  FEvaluateAtDateTime := (params.param['evaluateAtDateTime'].value as TFHIRDateTime).value;
  FInputParameters := (params.res['inputParameters'] as TFhirParameters).Link;{ob.5a}
  for p in params.parameterList do
    if p.name = 'inputData' then
      FInputDataList.Add((p.resource as TFhirResource).Link);{ob.2}
  FPatient := (params.param['patient'].value as TFhirReference).Link; {ob.5d}
  FEncounter := (params.param['encounter'].value as TFhirReference).Link; {ob.5d}
  FInitiatingOrganization := (params.param['initiatingOrganization'].value as TFhirReference).Link; {ob.5d}
  FInitiatingPerson := (params.param['initiatingPerson'].value as TFhirReference).Link; {ob.5d}
  FUserType := (params.param['userType'].value as TFhirCodeableConcept).Link; {ob.5d}
  FUserLanguage := (params.param['userLanguage'].value as TFhirCodeableConcept).Link; {ob.5d}
  FUserTaskContext := (params.param['userTaskContext'].value as TFhirCodeableConcept).Link; {ob.5d}
  FReceivingOrganization := (params.param['receivingOrganization'].value as TFhirReference).Link; {ob.5d}
  FReceivingPerson := (params.param['receivingPerson'].value as TFhirReference).Link; {ob.5d}
  FRecipientType := (params.param['recipientType'].value as TFhirCodeableConcept).Link; {ob.5d}
  FRecipientLanguage := (params.param['recipientLanguage'].value as TFhirCodeableConcept).Link; {ob.5d}
  FSetting := (params.param['setting'].value as TFhirCodeableConcept).Link; {ob.5d}
  FSettingContext := (params.param['settingContext'].value as TFhirCodeableConcept).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIREvaluateOpRequest.load(params : TParseMap);
begin
  FRequestId := params.getVar('requestId');
  loadExtensions(params);
end;

destructor TFHIREvaluateOpRequest.Destroy;
begin
  FEvaluateAtDateTime.free;
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
      result.addParameter('requestId', TFHIRString.create(FRequestId));{oz.5f}
    if (FEvaluateAtDateTime <> nil) then
      result.addParameter('evaluateAtDateTime', TFHIRDateTime.create(FEvaluateAtDateTime));{oz.5e}
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

procedure TFHIREvaluateOpResponse.load(params : TParseMap);
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

constructor TFHIREverythingOpRequest.create;
begin
  inherited create();
end;

procedure TFHIREverythingOpRequest.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIREverythingOpRequest.load(params : TParseMap);
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

procedure TFHIREverythingOpResponse.load(params : TParseMap);
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

procedure TFHIRFindOpRequest.load(params : TParseMap);
begin
  FPatient := params.getVar('patient');
  FName := params.getVar('name');
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
      result.addParameter('patient', TFHIRString.create(FPatient));{oz.5f}
    if (FName <> '') then
      result.addParameter('name', TFHIRString.create(FName));{oz.5f}
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

constructor TFHIRFindOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRFindOpResponse.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRFindOpResponse.load(params : TParseMap);
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

procedure TFHIREvaluateMeasureOpRequest.SetPeriodStart(value : TDateAndTime);
begin
  FPeriodStart.free;
  FPeriodStart := value;
end;

procedure TFHIREvaluateMeasureOpRequest.SetPeriodEnd(value : TDateAndTime);
begin
  FPeriodEnd.free;
  FPeriodEnd := value;
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

procedure TFHIREvaluateMeasureOpRequest.SetLastReceivedOn(value : TDateAndTime);
begin
  FLastReceivedOn.free;
  FLastReceivedOn := value;
end;

constructor TFHIREvaluateMeasureOpRequest.create;
begin
  inherited create();
end;

procedure TFHIREvaluateMeasureOpRequest.load(params : TFHIRParameters);
begin
  FPeriodStart := (params.param['periodStart'].value as TFHIRDate).value;
  FPeriodEnd := (params.param['periodEnd'].value as TFHIRDate).value;
  FMeasure := (params.param['measure'].value as TFhirReference).Link; {ob.5d}
  FReportType := params.str['reportType'];
  FPatient := (params.param['patient'].value as TFhirReference).Link; {ob.5d}
  FPractitioner := (params.param['practitioner'].value as TFhirReference).Link; {ob.5d}
  FLastReceivedOn := (params.param['lastReceivedOn'].value as TFHIRDateTime).value;
  loadExtensions(params);
end;

procedure TFHIREvaluateMeasureOpRequest.load(params : TParseMap);
begin
  FReportType := params.getVar('reportType');
  loadExtensions(params);
end;

destructor TFHIREvaluateMeasureOpRequest.Destroy;
begin
  FPeriodStart.free;
  FPeriodEnd.free;
  FMeasure.free;
  FPatient.free;
  FPractitioner.free;
  FLastReceivedOn.free;
  inherited;
end;

function TFHIREvaluateMeasureOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FPeriodStart <> nil) then
      result.addParameter('periodStart', TFHIRDate.create(FPeriodStart));{oz.5e}
    if (FPeriodEnd <> nil) then
      result.addParameter('periodEnd', TFHIRDate.create(FPeriodEnd));{oz.5e}
    if (FMeasure <> nil) then
      result.addParameter('measure', FMeasure.Link);{oz.5d}
    if (FReportType <> '') then
      result.addParameter('reportType', TFHIRString.create(FReportType));{oz.5f}
    if (FPatient <> nil) then
      result.addParameter('patient', FPatient.Link);{oz.5d}
    if (FPractitioner <> nil) then
      result.addParameter('practitioner', FPractitioner.Link);{oz.5d}
    if (FLastReceivedOn <> nil) then
      result.addParameter('lastReceivedOn', TFHIRDateTime.create(FLastReceivedOn));{oz.5e}
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

procedure TFHIREvaluateMeasureOpResponse.load(params : TParseMap);
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

procedure TFHIRProcessMessageOpRequest.load(params : TParseMap);
begin
  FAsync := StrToBool(params.getVar('async'));
  FResponseUrl := params.getVar('response-url');
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
      result.addParameter('response-url', TFHIRString.create(FResponseUrl));{oz.5f}
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

procedure TFHIRProcessMessageOpResponse.load(params : TParseMap);
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

constructor TFHIRStatsOpRequest.create;
begin
  inherited create();
  FParamsList := TList<String>.create;
end;

procedure TFHIRStatsOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FPatient := params.str['patient'];
  FCode := params.str['code'];
  FDuration := params.str['duration'];
  for p in params.parameterList do
    if p.name = 'params' then
      FParamsList.Add((p.value as TFhirCode).value);{ob.1}
  loadExtensions(params);
end;

procedure TFHIRStatsOpRequest.load(params : TParseMap);
begin
  FPatient := params.getVar('patient');
  FCode := params.getVar('code');
  FDuration := params.getVar('duration');
  loadExtensions(params);
end;

destructor TFHIRStatsOpRequest.Destroy;
begin
  FParamsList.free;
  inherited;
end;

function TFHIRStatsOpRequest.asParams : TFhirParameters;
var
  v1 : String;
begin
  result := TFHIRParameters.create;
  try
    if (FPatient <> '') then
      result.addParameter('patient', TFHIRString.create(FPatient));{oz.5f}
    if (FCode <> '') then
      result.addParameter('code', TFHIRString.create(FCode));{oz.5f}
    if (FDuration <> '') then
      result.addParameter('duration', TFHIRString.create(FDuration));{oz.5f}
    for v1 in FParamsList do
      result.AddParameter('params', TFhirCode.create(v1));
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRStatsOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['patient', 'code', 'duration', 'params'], name);
end;

constructor TFHIRStatsOpResponse.create;
begin
  inherited create();
  FObsList := TAdvList<TFhirObservation>.create;
end;

procedure TFHIRStatsOpResponse.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  for p in params.parameterList do
    if p.name = 'obs' then
      FObsList.Add((p.resource as TFhirObservation).Link);{ob.2}
  loadExtensions(params);
end;

procedure TFHIRStatsOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRStatsOpResponse.Destroy;
begin
  FObsList.free;
  inherited;
end;

function TFHIRStatsOpResponse.asParams : TFhirParameters;
var
  v1 : TFhirObservation;
begin
  result := TFHIRParameters.create;
  try
    for v1 in FObsList do
      result.AddParameter('obs', v1.Link);
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRStatsOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['obs'], name);
end;

procedure TFHIRMatchOpRequest.SetPatient(value : TFhirPatient);
begin
  FPatient.free;
  FPatient := value;
end;

constructor TFHIRMatchOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRMatchOpRequest.load(params : TFHIRParameters);
begin
  FPatient := (params.res['patient'] as TFhirPatient).Link;{ob.5a}
  FFamily := params.str['family'];
  FGiven := params.str['given'];
  FGender := params.str['gender'];
  FBirthdate := params.str['birthdate'];
  FIdentifier := params.str['identifier'];
  FTelecom := params.str['telecom'];
  FPhone := params.str['phone'];
  FEmail := params.str['email'];
  FPostcode := params.str['postcode'];
  FUserid := params.str['userid'];
  FCount := params.str['count'];
  loadExtensions(params);
end;

procedure TFHIRMatchOpRequest.load(params : TParseMap);
begin
  FFamily := params.getVar('family');
  FGiven := params.getVar('given');
  FGender := params.getVar('gender');
  FBirthdate := params.getVar('birthdate');
  FIdentifier := params.getVar('identifier');
  FTelecom := params.getVar('telecom');
  FPhone := params.getVar('phone');
  FEmail := params.getVar('email');
  FPostcode := params.getVar('postcode');
  FUserid := params.getVar('userid');
  FCount := params.getVar('count');
  loadExtensions(params);
end;

destructor TFHIRMatchOpRequest.Destroy;
begin
  FPatient.free;
  inherited;
end;

function TFHIRMatchOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FPatient <> nil) then
      result.addParameter('patient', FPatient.Link);{oz.5a}
    if (FFamily <> '') then
      result.addParameter('family', TFHIRString.create(FFamily));{oz.5f}
    if (FGiven <> '') then
      result.addParameter('given', TFHIRString.create(FGiven));{oz.5f}
    if (FGender <> '') then
      result.addParameter('gender', TFHIRString.create(FGender));{oz.5f}
    if (FBirthdate <> '') then
      result.addParameter('birthdate', TFHIRString.create(FBirthdate));{oz.5f}
    if (FIdentifier <> '') then
      result.addParameter('identifier', TFHIRString.create(FIdentifier));{oz.5f}
    if (FTelecom <> '') then
      result.addParameter('telecom', TFHIRString.create(FTelecom));{oz.5f}
    if (FPhone <> '') then
      result.addParameter('phone', TFHIRString.create(FPhone));{oz.5f}
    if (FEmail <> '') then
      result.addParameter('email', TFHIRString.create(FEmail));{oz.5f}
    if (FPostcode <> '') then
      result.addParameter('postcode', TFHIRString.create(FPostcode));{oz.5f}
    if (FUserid <> '') then
      result.addParameter('userid', TFHIRString.create(FUserid));{oz.5f}
    if (FCount <> '') then
      result.addParameter('count', TFHIRString.create(FCount));{oz.5f}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRMatchOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['patient', 'family', 'given', 'gender', 'birthdate', 'identifier', 'telecom', 'phone', 'email', 'postcode', 'userid', 'count'], name);
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

procedure TFHIRMatchOpResponse.load(params : TParseMap);
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
  FContentList := TAdvList<TFhirReference>.create;
end;

procedure TFHIRPopulateOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FIdentifier := params.str['identifier'];
  FQuestionnaire := (params.res['questionnaire'] as TFhirQuestionnaire).Link;{ob.5a}
  FQuestionnaireRef := (params.param['questionnaireRef'].value as TFhirReference).Link; {ob.5d}
  FSubject := (params.param['subject'].value as TFhirReference).Link; {ob.5d}
  for p in params.parameterList do
    if p.name = 'content' then
      FContentList.Add((p.value as TFhirReference).Link);{a}
  FLocal := params.bool['local'];
  loadExtensions(params);
end;

procedure TFHIRPopulateOpRequest.load(params : TParseMap);
begin
  FIdentifier := params.getVar('identifier');
  FLocal := StrToBool(params.getVar('local'));
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
      result.addParameter('identifier', TFHIRString.create(FIdentifier));{oz.5f}
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

procedure TFHIRPopulateOpResponse.load(params : TParseMap);
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
  FContentList := TAdvList<TFhirReference>.create;
end;

procedure TFHIRPopulatehtmlOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FIdentifier := params.str['identifier'];
  FQuestionnaire := (params.res['questionnaire'] as TFhirQuestionnaire).Link;{ob.5a}
  FQuestionnaireRef := (params.param['questionnaireRef'].value as TFhirReference).Link; {ob.5d}
  for p in params.parameterList do
    if p.name = 'content' then
      FContentList.Add((p.value as TFhirReference).Link);{a}
  FLocal := params.bool['local'];
  loadExtensions(params);
end;

procedure TFHIRPopulatehtmlOpRequest.load(params : TParseMap);
begin
  FIdentifier := params.getVar('identifier');
  FLocal := StrToBool(params.getVar('local'));
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
      result.addParameter('identifier', TFHIRString.create(FIdentifier));{oz.5f}
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
  FForm := params.str['form'];
  FIssues := (params.res['issues'] as TFhirOperationOutcome).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRPopulatehtmlOpResponse.load(params : TParseMap);
begin
  FForm := params.getVar('form');
  loadExtensions(params);
end;

destructor TFHIRPopulatehtmlOpResponse.Destroy;
begin
  FIssues.free;
  inherited;
end;

function TFHIRPopulatehtmlOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FForm <> '') then
      result.addParameter('form', TFHIRString.create(FForm));{oz.5f}
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
  FContentList := TAdvList<TFhirReference>.create;
end;

procedure TFHIRPopulatelinkOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FIdentifier := params.str['identifier'];
  FQuestionnaire := (params.res['questionnaire'] as TFhirQuestionnaire).Link;{ob.5a}
  FQuestionnaireRef := (params.param['questionnaireRef'].value as TFhirReference).Link; {ob.5d}
  for p in params.parameterList do
    if p.name = 'content' then
      FContentList.Add((p.value as TFhirReference).Link);{a}
  FLocal := params.bool['local'];
  loadExtensions(params);
end;

procedure TFHIRPopulatelinkOpRequest.load(params : TParseMap);
begin
  FIdentifier := params.getVar('identifier');
  FLocal := StrToBool(params.getVar('local'));
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
      result.addParameter('identifier', TFHIRString.create(FIdentifier));{oz.5f}
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

procedure TFHIRPopulatelinkOpResponse.SetLink_(value : TFhirBinary);
begin
  FLink_.free;
  FLink_ := value;
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
  FLink_ := (params.res['link'] as TFhirBinary).Link;{ob.5a}
  FIssues := (params.res['issues'] as TFhirOperationOutcome).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRPopulatelinkOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRPopulatelinkOpResponse.Destroy;
begin
  FLink_.free;
  FIssues.free;
  inherited;
end;

function TFHIRPopulatelinkOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FLink_ <> nil) then
      result.addParameter('link', FLink_.Link);{oz.5a}
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

procedure TFHIRQuestionnaireOpRequest.load(params : TParseMap);
begin
  FIdentifier := params.getVar('identifier');
  FProfile := params.getVar('profile');
  FUrl := params.getVar('url');
  FSupportedOnly := StrToBool(params.getVar('supportedOnly'));
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
      result.addParameter('identifier', TFHIRString.create(FIdentifier));{oz.5f}
    if (FProfile <> '') then
      result.addParameter('profile', TFHIRString.create(FProfile));{oz.5f}
    if (FUrl <> '') then
      result.addParameter('url', TFHIRString.create(FUrl));{oz.5f}
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

procedure TFHIRQuestionnaireOpResponse.load(params : TParseMap);
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

procedure TFHIRTransformOpRequest.load(params : TParseMap);
begin
  FSource := params.getVar('source');
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
      result.addParameter('source', TFHIRString.create(FSource));{oz.5f}
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

procedure TFHIRTransformOpResponse.load(params : TParseMap);
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

procedure TFHIRReserveOpRequest.SetOwner(value : TFhirReference);
begin
  FOwner.free;
  FOwner := value;
end;

constructor TFHIRReserveOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRReserveOpRequest.load(params : TFHIRParameters);
begin
  FOwner := (params.param['owner'].value as TFhirReference).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIRReserveOpRequest.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRReserveOpRequest.Destroy;
begin
  FOwner.free;
  inherited;
end;

function TFHIRReserveOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FOwner <> nil) then
      result.addParameter('owner', FOwner.Link);{oz.5d}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRReserveOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['owner'], name);
end;

constructor TFHIRReserveOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRReserveOpResponse.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRReserveOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRReserveOpResponse.Destroy;
begin
  inherited;
end;

function TFHIRReserveOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRReserveOpResponse.isKnownName(name : String) : boolean;
begin
  result := false;
end;

procedure TFHIRStartOpRequest.SetOwner(value : TFhirReference);
begin
  FOwner.free;
  FOwner := value;
end;

constructor TFHIRStartOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRStartOpRequest.load(params : TFHIRParameters);
begin
  FOwner := (params.param['owner'].value as TFhirReference).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIRStartOpRequest.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRStartOpRequest.Destroy;
begin
  FOwner.free;
  inherited;
end;

function TFHIRStartOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FOwner <> nil) then
      result.addParameter('owner', FOwner.Link);{oz.5d}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRStartOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['owner'], name);
end;

constructor TFHIRStartOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRStartOpResponse.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRStartOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRStartOpResponse.Destroy;
begin
  inherited;
end;

function TFHIRStartOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRStartOpResponse.isKnownName(name : String) : boolean;
begin
  result := false;
end;

constructor TFHIRFinishOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRFinishOpRequest.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRFinishOpRequest.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRFinishOpRequest.Destroy;
begin
  inherited;
end;

function TFHIRFinishOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFinishOpRequest.isKnownName(name : String) : boolean;
begin
  result := false;
end;

constructor TFHIRFinishOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRFinishOpResponse.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRFinishOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRFinishOpResponse.Destroy;
begin
  inherited;
end;

function TFHIRFinishOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFinishOpResponse.isKnownName(name : String) : boolean;
begin
  result := false;
end;

procedure TFHIRFailOpRequest.SetReason(value : TFhirCodeableConcept);
begin
  FReason.free;
  FReason := value;
end;

constructor TFHIRFailOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRFailOpRequest.load(params : TFHIRParameters);
begin
  FReason := (params.param['reason'].value as TFhirCodeableConcept).Link; {ob.5d}
  loadExtensions(params);
end;

procedure TFHIRFailOpRequest.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRFailOpRequest.Destroy;
begin
  FReason.free;
  inherited;
end;

function TFHIRFailOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReason <> nil) then
      result.addParameter('reason', FReason.Link);{oz.5d}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFailOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['reason'], name);
end;

constructor TFHIRFailOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRFailOpResponse.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRFailOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRFailOpResponse.Destroy;
begin
  inherited;
end;

function TFHIRFailOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFailOpResponse.isKnownName(name : String) : boolean;
begin
  result := false;
end;

constructor TFHIRReleaseOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRReleaseOpRequest.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRReleaseOpRequest.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRReleaseOpRequest.Destroy;
begin
  inherited;
end;

function TFHIRReleaseOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRReleaseOpRequest.isKnownName(name : String) : boolean;
begin
  result := false;
end;

constructor TFHIRReleaseOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRReleaseOpResponse.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRReleaseOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRReleaseOpResponse.Destroy;
begin
  inherited;
end;

function TFHIRReleaseOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRReleaseOpResponse.isKnownName(name : String) : boolean;
begin
  result := false;
end;

constructor TFHIRSuspendOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRSuspendOpRequest.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRSuspendOpRequest.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRSuspendOpRequest.Destroy;
begin
  inherited;
end;

function TFHIRSuspendOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRSuspendOpRequest.isKnownName(name : String) : boolean;
begin
  result := false;
end;

constructor TFHIRSuspendOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRSuspendOpResponse.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRSuspendOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRSuspendOpResponse.Destroy;
begin
  inherited;
end;

function TFHIRSuspendOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRSuspendOpResponse.isKnownName(name : String) : boolean;
begin
  result := false;
end;

constructor TFHIRResumeOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRResumeOpRequest.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRResumeOpRequest.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRResumeOpRequest.Destroy;
begin
  inherited;
end;

function TFHIRResumeOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRResumeOpRequest.isKnownName(name : String) : boolean;
begin
  result := false;
end;

constructor TFHIRResumeOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRResumeOpResponse.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRResumeOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRResumeOpResponse.Destroy;
begin
  inherited;
end;

function TFHIRResumeOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRResumeOpResponse.isKnownName(name : String) : boolean;
begin
  result := false;
end;

constructor TFHIRCancelOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRCancelOpRequest.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRCancelOpRequest.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRCancelOpRequest.Destroy;
begin
  inherited;
end;

function TFHIRCancelOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRCancelOpRequest.isKnownName(name : String) : boolean;
begin
  result := false;
end;

constructor TFHIRCancelOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRCancelOpResponse.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRCancelOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRCancelOpResponse.Destroy;
begin
  inherited;
end;

function TFHIRCancelOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRCancelOpResponse.isKnownName(name : String) : boolean;
begin
  result := false;
end;

constructor TFHIRStopOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRStopOpRequest.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRStopOpRequest.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRStopOpRequest.Destroy;
begin
  inherited;
end;

function TFHIRStopOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRStopOpRequest.isKnownName(name : String) : boolean;
begin
  result := false;
end;

constructor TFHIRStopOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRStopOpResponse.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRStopOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRStopOpResponse.Destroy;
begin
  inherited;
end;

function TFHIRStopOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRStopOpResponse.isKnownName(name : String) : boolean;
begin
  result := false;
end;

procedure TFHIRSetInputOpInput.SetValueX(value : TFhirType);
begin
  FValueX.free;
  FValueX := value;
end;

constructor TFHIRSetInputOpInput.create;
begin
  inherited create();
end;

constructor TFHIRSetInputOpInput.create(params : TFhirParametersParameter);
begin
  inherited create();
  FName := params.str['name'];
  FValueX := (params.param['value[x]'].value as TFhirType).Link; {ob.5d}
  loadExtensions(params);
end;

destructor TFHIRSetInputOpInput.Destroy;
begin
  FValueX.free;
  inherited;
end;

function TFHIRSetInputOpInput.asParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FName <> '') then
      result.addParameter('name', TFHIRString.create(FName));{oz.5f}
    if (FValueX <> nil) then
      result.addParameter('value[x]', FValueX.Link);{oz.5d}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRSetInputOpInput.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['name', 'value[x]'], name);
end;

constructor TFHIRSetInputOpRequest.create;
begin
  inherited create();
  FInputList := TAdvList<TFHIRSetInputOpInput>.create;
end;

procedure TFHIRSetInputOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  for p in params.parameterList do
    if p.name = 'input' then
      FInputList.Add(TFHIRSetInputOpInput.create(p));{a}
  loadExtensions(params);
end;

procedure TFHIRSetInputOpRequest.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRSetInputOpRequest.Destroy;
begin
  FInputList.free;
  inherited;
end;

function TFHIRSetInputOpRequest.asParams : TFhirParameters;
var
  v1 : TFHIRSetInputOpInput;
begin
  result := TFHIRParameters.create;
  try
    for v1 in FInputList do
      result.AddParameter(v1.asParams('input'));
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRSetInputOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['input'], name);
end;

constructor TFHIRSetInputOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRSetInputOpResponse.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRSetInputOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRSetInputOpResponse.Destroy;
begin
  inherited;
end;

function TFHIRSetInputOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRSetInputOpResponse.isKnownName(name : String) : boolean;
begin
  result := false;
end;

constructor TFHIRSetOutputOpRequest.create;
begin
  inherited create();
end;

procedure TFHIRSetOutputOpRequest.load(params : TFHIRParameters);
begin
  loadExtensions(params);
end;

procedure TFHIRSetOutputOpRequest.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRSetOutputOpRequest.Destroy;
begin
  inherited;
end;

function TFHIRSetOutputOpRequest.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRSetOutputOpRequest.isKnownName(name : String) : boolean;
begin
  result := false;
end;

procedure TFHIRSetOutputOpOutput.SetValueX(value : TFhirType);
begin
  FValueX.free;
  FValueX := value;
end;

constructor TFHIRSetOutputOpOutput.create;
begin
  inherited create();
end;

constructor TFHIRSetOutputOpOutput.create(params : TFhirParametersParameter);
begin
  inherited create();
  FName := params.str['name'];
  FValueX := (params.param['value[x]'].value as TFhirType).Link; {ob.5d}
  loadExtensions(params);
end;

destructor TFHIRSetOutputOpOutput.Destroy;
begin
  FValueX.free;
  inherited;
end;

function TFHIRSetOutputOpOutput.asParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FName <> '') then
      result.addParameter('name', TFHIRString.create(FName));{oz.5f}
    if (FValueX <> nil) then
      result.addParameter('value[x]', FValueX.Link);{oz.5d}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRSetOutputOpOutput.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['name', 'value[x]'], name);
end;

constructor TFHIRSetOutputOpResponse.create;
begin
  inherited create();
  FOutputList := TAdvList<TFHIRSetOutputOpOutput>.create;
end;

procedure TFHIRSetOutputOpResponse.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  for p in params.parameterList do
    if p.name = 'output' then
      FOutputList.Add(TFHIRSetOutputOpOutput.create(p));{a}
  loadExtensions(params);
end;

procedure TFHIRSetOutputOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRSetOutputOpResponse.Destroy;
begin
  FOutputList.free;
  inherited;
end;

function TFHIRSetOutputOpResponse.asParams : TFhirParameters;
var
  v1 : TFHIRSetOutputOpOutput;
begin
  result := TFHIRParameters.create;
  try
    for v1 in FOutputList do
      result.AddParameter(v1.asParams('output'));
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRSetOutputOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['output'], name);
end;

end.

