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

{$IFNDEF FHIR_DSTU3}
This is the dstu3 version of the FHIR code
{$ENDIF}


interface

// FHIR v1.3.0 generated 2016-02-13T07:21:57+11:00

uses
  SysUtils, Classes, Generics.Collections, StringSupport, DecimalSupport, AdvBuffers, AdvGenerics, DateAndTime, FHIRBase, FHIRTypes, FHIRResources;

Type

  TFHIROperationRequest = class (TAdvObject)
  public
    constructor create(params : TFHIRParameters); overload; virtual;
    constructor create(); overload; override;
    function AsParams : TFHIRParameters; virtual;
  end;

  TFHIROperationResponse = class (TAdvObject)
  public
    constructor create(params : TFHIRParameters); overload; virtual;
    constructor create(); overload; override;
    function AsParams : TFHIRParameters; virtual;
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
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
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
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property return : TFhirValueSet read FReturn write SetReturn;
  end;

  //Operation lookup (Concept Look Up)
  TFHIRLookupOpRequest = class (TFHIROperationRequest)
  private
    FCode : String;
    FSystem : String;
    FVersion : String;
    FCoding : TFhirCoding;
    FDate : TDateAndTime;
    procedure SetCoding(value : TFhirCoding);
    procedure SetDate(value : TDateAndTime);
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property code : String read FCode write FCode;
    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property coding : TFhirCoding read FCoding write SetCoding;
    property date : TDateAndTime read FDate write SetDate;
  end;

  TFHIRLookupOpDesignation = class (TAdvObject)
  private
    FLanguage : String;
    FUse : TFhirCoding;
    FValue : String;
    procedure SetUse(value : TFhirCoding);
  public
    constructor create(params : TFhirParametersParameter); overload;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams(name : String) : TFHIRParametersParameter;
    property language : String read FLanguage write FLanguage;
    property use : TFhirCoding read FUse write SetUse;
    property value : String read FValue write FValue;
  end;

  TFHIRLookupOpResponse = class (TFHIROperationResponse)
  private
    FName : String;
    FVersion : String;
    FDisplay : String;
    FAbstract : Boolean;
    FDesignationList : TAdvList<TFHIRLookupOpDesignation>;
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property name : String read FName write FName;
    property version : String read FVersion write FVersion;
    property display : String read FDisplay write FDisplay;
    property abstract : Boolean read FAbstract write FAbstract;
    property designationList : TAdvList<TFHIRLookupOpDesignation> read FDesignationList;
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
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
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
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
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
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property resource : TFhirResource read FResource write SetResource;
    property mode : String read FMode write FMode;
    property profile : String read FProfile write FProfile;
  end;

  TFHIRValidateOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirOperationOutcome;
    procedure SetReturn(value : TFhirOperationOutcome);
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property return : TFhirOperationOutcome read FReturn write SetReturn;
  end;

  //Operation meta (Access a list of profiles, tags, and security labels)
  TFHIRMetaOpRequest = class (TFHIROperationRequest)
  private
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
  end;

  TFHIRMetaOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirMeta;
    procedure SetReturn(value : TFhirMeta);
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property return : TFhirMeta read FReturn write SetReturn;
  end;

  //Operation meta-add (Add profiles, tags, and security labels to a resource)
  TFHIRMetaAddOpRequest = class (TFHIROperationRequest)
  private
    FMeta : TFhirMeta;
    procedure SetMeta(value : TFhirMeta);
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property meta : TFhirMeta read FMeta write SetMeta;
  end;

  TFHIRMetaAddOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirMeta;
    procedure SetReturn(value : TFhirMeta);
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property return : TFhirMeta read FReturn write SetReturn;
  end;

  //Operation meta-delete (Delete profiles, tags, and security labels for a resource)
  TFHIRMetaDeleteOpRequest = class (TFHIROperationRequest)
  private
    FMeta : TFhirMeta;
    procedure SetMeta(value : TFhirMeta);
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property meta : TFhirMeta read FMeta write SetMeta;
  end;

  TFHIRMetaDeleteOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirMeta;
    procedure SetReturn(value : TFhirMeta);
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property return : TFhirMeta read FReturn write SetReturn;
  end;

  //Operation cds-hook (The CDS Hook operation is the core API request for CDS Hooks)
  TFHIRCdsHookOpOauth = class (TAdvObject)
  private
    FToken : String;
    FScope : String;
    FExpires : String;
  public
    constructor create(params : TFhirParametersParameter); overload;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams(name : String) : TFHIRParametersParameter;
    property token : String read FToken write FToken;
    property scope : String read FScope write FScope;
    property expires : String read FExpires write FExpires;
  end;

  TFHIRCdsHookOpSource = class (TAdvObject)
  private
    FLabel_ : String;
    FUrl : String;
  public
    constructor create(params : TFhirParametersParameter); overload;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams(name : String) : TFHIRParametersParameter;
    property label_ : String read FLabel_ write FLabel_;
    property url : String read FUrl write FUrl;
  end;

  TFHIRCdsHookOpSuggestion = class (TAdvObject)
  private
    FLabel_ : String;
    FCreate_List : TAdvList<TFhirResource>;
    FDeleteList : TList<String>;
  public
    constructor create(params : TFhirParametersParameter); overload;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams(name : String) : TFHIRParametersParameter;
    property label_ : String read FLabel_ write FLabel_;
    property create_List : TAdvList<TFhirResource> read FCreate_List;
    property deleteList : TList<String> read FDeleteList;
  end;

  TFHIRCdsHookOpLink_ = class (TAdvObject)
  private
    FLabel_ : String;
    FUrl : String;
  public
    constructor create(params : TFhirParametersParameter); overload;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams(name : String) : TFHIRParametersParameter;
    property label_ : String read FLabel_ write FLabel_;
    property url : String read FUrl write FUrl;
  end;

  TFHIRCdsHookOpCard = class (TAdvObject)
  private
    FSummary : String;
    FDetail : String;
    FIndicator : String;
    FSource : TFHIRCdsHookOpSource;
    FSuggestionList : TAdvList<TFHIRCdsHookOpSuggestion>;
    FLink_List : TAdvList<TFHIRCdsHookOpLink_>;
    procedure SetSource(value : TFHIRCdsHookOpSource);
  public
    constructor create(params : TFhirParametersParameter); overload;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams(name : String) : TFHIRParametersParameter;
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
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
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

  TFHIRCdsHookOpDecision = class (TAdvObject)
  private
    FCreate_List : TAdvList<TFhirResource>;
    FDeleteList : TList<String>;
  public
    constructor create(params : TFhirParametersParameter); overload;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams(name : String) : TFHIRParametersParameter;
    property create_List : TAdvList<TFhirResource> read FCreate_List;
    property deleteList : TList<String> read FDeleteList;
  end;

  TFHIRCdsHookOpResponse = class (TFHIROperationResponse)
  private
    FDecisionList : TAdvList<TFHIRCdsHookOpDecision>;
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property decisionList : TAdvList<TFHIRCdsHookOpDecision> read FDecisionList;
  end;

  //Operation document (Generate a Document)
  TFHIRDocumentOpRequest = class (TFHIROperationRequest)
  private
    FPersist : Boolean;
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property persist : Boolean read FPersist write FPersist;
  end;

  TFHIRDocumentOpResponse = class (TFHIROperationResponse)
  private
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
  end;

  //Operation translate (Concept Translation)
  TFHIRTranslateOpDependency = class (TAdvObject)
  private
    FElement : String;
    FConcept : TFhirCodeableConcept;
    procedure SetConcept(value : TFhirCodeableConcept);
  public
    constructor create(params : TFhirParametersParameter); overload;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams(name : String) : TFHIRParametersParameter;
    property element : String read FElement write FElement;
    property concept : TFhirCodeableConcept read FConcept write SetConcept;
  end;

  TFHIRTranslateOpRequest = class (TFHIROperationRequest)
  private
    FCode : String;
    FSystem : String;
    FVersion : String;
    FValueSet : String;
    FCoding : TFhirCoding;
    FCodeableConcept : TFhirCodeableConcept;
    FTarget : String;
    FDependencyList : TAdvList<TFHIRTranslateOpDependency>;
    procedure SetCoding(value : TFhirCoding);
    procedure SetCodeableConcept(value : TFhirCodeableConcept);
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property code : String read FCode write FCode;
    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property valueSet : String read FValueSet write FValueSet;
    property coding : TFhirCoding read FCoding write SetCoding;
    property codeableConcept : TFhirCodeableConcept read FCodeableConcept write SetCodeableConcept;
    property target : String read FTarget write FTarget;
    property dependencyList : TAdvList<TFHIRTranslateOpDependency> read FDependencyList;
  end;

  TFHIRTranslateOpProduct = class (TAdvObject)
  private
    FElement : String;
    FConcept : TFhirCoding;
    procedure SetConcept(value : TFhirCoding);
  public
    constructor create(params : TFhirParametersParameter); overload;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams(name : String) : TFHIRParametersParameter;
    property element : String read FElement write FElement;
    property concept : TFhirCoding read FConcept write SetConcept;
  end;

  TFHIRTranslateOpMatch = class (TAdvObject)
  private
    FEquivalence : String;
    FConcept : TFhirCoding;
    FProductList : TAdvList<TFHIRTranslateOpProduct>;
    procedure SetConcept(value : TFhirCoding);
  public
    constructor create(params : TFhirParametersParameter); overload;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams(name : String) : TFHIRParametersParameter;
    property equivalence : String read FEquivalence write FEquivalence;
    property concept : TFhirCoding read FConcept write SetConcept;
    property productList : TAdvList<TFHIRTranslateOpProduct> read FProductList;
  end;

  TFHIRTranslateOpResponse = class (TFHIROperationResponse)
  private
    FResult : Boolean;
    FMessage : String;
    FMatchList : TAdvList<TFHIRTranslateOpMatch>;
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
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
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property name : String read FName write FName;
    property conceptList : TAdvList<TFhirCoding> read FConceptList;
    property version : String read FVersion write FVersion;
  end;

  TFHIRClosureOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirConceptMap;
    procedure SetReturn(value : TFhirConceptMap);
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property return : TFhirConceptMap read FReturn write SetReturn;
  end;

  //Operation evaluate (Evaluate)
  TFHIREvaluateOpRequest = class (TFHIROperationRequest)
  private
    FRequestId : String;
    FEvaluateAtDateTime : TDateAndTime;
    FInputParameters : TFhirParameters;
    FSetting : TFhirCodeableConcept;
    FSettingContext : TFhirCodeableConcept;
    procedure SetEvaluateAtDateTime(value : TDateAndTime);
    procedure SetInputParameters(value : TFhirParameters);
    procedure SetSetting(value : TFhirCodeableConcept);
    procedure SetSettingContext(value : TFhirCodeableConcept);
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property requestId : String read FRequestId write FRequestId;
    property evaluateAtDateTime : TDateAndTime read FEvaluateAtDateTime write SetEvaluateAtDateTime;
    property inputParameters : TFhirParameters read FInputParameters write SetInputParameters;
    property setting : TFhirCodeableConcept read FSetting write SetSetting;
    property settingContext : TFhirCodeableConcept read FSettingContext write SetSettingContext;
  end;

  TFHIREvaluateOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirGuidanceResponse;
    procedure SetReturn(value : TFhirGuidanceResponse);
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property return : TFhirGuidanceResponse read FReturn write SetReturn;
  end;

  //Operation everything (Fetch Encounter Record)
  TFHIREverythingOpRequest = class (TFHIROperationRequest)
  private
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
  end;

  TFHIREverythingOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirBundle;
    procedure SetReturn(value : TFhirBundle);
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property return : TFhirBundle read FReturn write SetReturn;
  end;

  //Operation find (Find a functional list)
  TFHIRFindOpRequest = class (TFHIROperationRequest)
  private
    FPatient : String;
    FName : String;
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property patient : String read FPatient write FPatient;
    property name : String read FName write FName;
  end;

  TFHIRFindOpResponse = class (TFHIROperationResponse)
  private
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
  end;

  //Operation process-message (Process Message)
  TFHIRProcessMessageOpRequest = class (TFHIROperationRequest)
  private
    FContent : TFhirBundle;
    FAsync : Boolean;
    FResponseUrl : String;
    procedure SetContent(value : TFhirBundle);
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property content : TFhirBundle read FContent write SetContent;
    property async : Boolean read FAsync write FAsync;
    property responseUrl : String read FResponseUrl write FResponseUrl;
  end;

  TFHIRProcessMessageOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirBundle;
    procedure SetReturn(value : TFhirBundle);
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
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
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property identifier : String read FIdentifier write FIdentifier;
    property questionnaire : TFhirQuestionnaire read FQuestionnaire write SetQuestionnaire;
    property questionnaireRef : TFhirReference read FQuestionnaireRef write SetQuestionnaireRef;
    property subject : TFhirReference read FSubject write SetSubject;
    property contentList : TAdvList<TFhirReference> read FContentList;
    property local : Boolean read FLocal write FLocal;
  end;

  TFHIRPopulateOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirQuestionnaireResponse;
    procedure SetReturn(value : TFhirQuestionnaireResponse);
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property return : TFhirQuestionnaireResponse read FReturn write SetReturn;
  end;

  //Operation questionnaire (Build Questionnaire)
  TFHIRQuestionnaireOpRequest = class (TFHIROperationRequest)
  private
    FIdentifier : String;
    FProfile : String;
    FUrl : String;
    FSupportedOnly : Boolean;
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property identifier : String read FIdentifier write FIdentifier;
    property profile : String read FProfile write FProfile;
    property url : String read FUrl write FUrl;
    property supportedOnly : Boolean read FSupportedOnly write FSupportedOnly;
  end;

  TFHIRQuestionnaireOpResponse = class (TFHIROperationResponse)
  private
    FReturn : TFhirQuestionnaire;
    procedure SetReturn(value : TFhirQuestionnaire);
  public
    constructor create(params : TFHIRParameters); overload; override;
    constructor create; overload; override;
    destructor Destroy; override;
    function AsParams : TFHIRParameters; override;
    property return : TFhirQuestionnaire read FReturn write SetReturn;
  end;

implementation

uses
  FHIRUtilities;

function TFHIROperationRequest.AsParams: TFHIRParameters;
begin
  raise Exception.Create('Must be overriden');
end;

constructor TFHIROperationRequest.create(params: TFHIRParameters);
begin
  inherited Create;
end;

constructor TFHIROperationRequest.create;
begin
  inherited Create;
end;

function TFHIROperationResponse.AsParams: TFHIRParameters;
begin
  raise Exception.Create('Must be overriden');
end;

constructor TFHIROperationResponse.create(params: TFHIRParameters);
begin
  inherited Create;
end;

constructor TFHIROperationResponse.create;
begin
  inherited Create;
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

constructor TFHIRExpandOpRequest.create(params : TFHIRParameters);
begin
  inherited create();
  FIdentifier := params.str['identifier'];
  FValueSet := (params.res['valueSet'] as TFhirValueSet).Link;{ob.5a}
  FContext := params.str['context'];
  FFilter := params.str['filter'];
  FProfile := params.str['profile'];
  FDate := (params.param['date'].value as TFHIRDateTime).value;
  FOffset := params.str['offset'];
  FCount := params.str['count'];
end;

destructor TFHIRExpandOpRequest.Destroy;
begin
  FValueSet.free;
  FDate.free;
  inherited;
end;

function TFHIRExpandOpRequest.AsParams : TFhirParameters;
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
    result.link;
  finally
    result.free;
  end;
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

constructor TFHIRExpandOpResponse.create(params : TFHIRParameters);
begin
  inherited create();
  FReturn := (params.res['return'] as TFhirValueSet).Link;{ob.5a}
end;

destructor TFHIRExpandOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRExpandOpResponse.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    result.link;
  finally
    result.free;
  end;
end;

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
end;

constructor TFHIRLookupOpRequest.create(params : TFHIRParameters);
begin
  inherited create();
  FCode := params.str['code'];
  FSystem := params.str['system'];
  FVersion := params.str['version'];
  FCoding := (params.param['coding'].value as TFhirCoding).Link; {ob.5d}
  FDate := (params.param['date'].value as TFHIRDateTime).value;
end;

destructor TFHIRLookupOpRequest.Destroy;
begin
  FCoding.free;
  FDate.free;
  inherited;
end;

function TFHIRLookupOpRequest.AsParams : TFhirParameters;
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
    result.link;
  finally
    result.free;
  end;
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
end;

destructor TFHIRLookupOpDesignation.Destroy;
begin
  FUse.free;
  inherited;
end;

function TFHIRLookupOpDesignation.AsParams(name : String) : TFhirParametersParameter;
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
    result.link;
  finally
    result.free;
  end;
end;

constructor TFHIRLookupOpResponse.create;
begin
  inherited create();
  FDesignationList := TAdvList<TFHIRLookupOpDesignation>.create;
end;

constructor TFHIRLookupOpResponse.create(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  inherited create();
  FDesignationList := TAdvList<TFHIRLookupOpDesignation>.create;
  FName := params.str['name'];
  FVersion := params.str['version'];
  FDisplay := params.str['display'];
  FAbstract := params.bool['abstract'];
  for p in params.parameterList do
    if p.name = 'designation' then
      FDesignationList.Add(TFHIRLookupOpDesignation.create(p));{a}
end;

destructor TFHIRLookupOpResponse.Destroy;
begin
  FDesignationList.free;
  inherited;
end;

function TFHIRLookupOpResponse.AsParams : TFhirParameters;
var
  v1 : TFHIRLookupOpDesignation;
begin
  result := TFHIRParameters.create;
  try
    if (FName <> '') then
      result.addParameter('name', TFHIRString.create(FName));{oz.5f}
    if (FVersion <> '') then
      result.addParameter('version', TFHIRString.create(FVersion));{oz.5f}
    if (FDisplay <> '') then
      result.addParameter('display', TFHIRString.create(FDisplay));{oz.5f}
      result.addParameter('abstract', TFHIRBoolean.create(FAbstract));{oz.5f}
    for v1 in FDesignationList do
      result.AddParameter(v1.asParams('designation'));
    result.link;
  finally
    result.free;
  end;
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

constructor TFHIRValidateCodeOpRequest.create(params : TFHIRParameters);
begin
  inherited create();
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
end;

destructor TFHIRValidateCodeOpRequest.Destroy;
begin
  FValueSet.free;
  FCoding.free;
  FCodeableConcept.free;
  FDate.free;
  inherited;
end;

function TFHIRValidateCodeOpRequest.AsParams : TFhirParameters;
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
    result.link;
  finally
    result.free;
  end;
end;

constructor TFHIRValidateCodeOpResponse.create;
begin
  inherited create();
end;

constructor TFHIRValidateCodeOpResponse.create(params : TFHIRParameters);
begin
  inherited create();
  FResult := params.bool['result'];
  FMessage := params.str['message'];
  FDisplay := params.str['display'];
end;

destructor TFHIRValidateCodeOpResponse.Destroy;
begin
  inherited;
end;

function TFHIRValidateCodeOpResponse.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
      result.addParameter('result', TFHIRBoolean.create(FResult));{oz.5f}
    if (FMessage <> '') then
      result.addParameter('message', TFHIRString.create(FMessage));{oz.5f}
    if (FDisplay <> '') then
      result.addParameter('display', TFHIRString.create(FDisplay));{oz.5f}
    result.link;
  finally
    result.free;
  end;
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

constructor TFHIRValidateOpRequest.create(params : TFHIRParameters);
begin
  inherited create();
  FResource := (params.res['resource'] as TFhirResource).Link;{ob.5a}
  FMode := params.str['mode'];
  FProfile := params.str['profile'];
end;

destructor TFHIRValidateOpRequest.Destroy;
begin
  FResource.free;
  inherited;
end;

function TFHIRValidateOpRequest.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FResource <> nil) then
      result.addParameter('resource', FResource.Link);{oz.5a}
    if (FMode <> '') then
      result.addParameter('mode', TFHIRString.create(FMode));{oz.5f}
    if (FProfile <> '') then
      result.addParameter('profile', TFHIRString.create(FProfile));{oz.5f}
    result.link;
  finally
    result.free;
  end;
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

constructor TFHIRValidateOpResponse.create(params : TFHIRParameters);
begin
  inherited create();
  FReturn := (params.res['return'] as TFhirOperationOutcome).Link;{ob.5a}
end;

destructor TFHIRValidateOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRValidateOpResponse.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    result.link;
  finally
    result.free;
  end;
end;

constructor TFHIRMetaOpRequest.create;
begin
  inherited create();
end;

constructor TFHIRMetaOpRequest.create(params : TFHIRParameters);
begin
  inherited create();
end;

destructor TFHIRMetaOpRequest.Destroy;
begin
  inherited;
end;

function TFHIRMetaOpRequest.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    result.link;
  finally
    result.free;
  end;
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

constructor TFHIRMetaOpResponse.create(params : TFHIRParameters);
begin
  inherited create();
  FReturn := (params.param['return'].value as TFhirMeta).Link; {ob.5d}
end;

destructor TFHIRMetaOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRMetaOpResponse.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5d}
    result.link;
  finally
    result.free;
  end;
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

constructor TFHIRMetaAddOpRequest.create(params : TFHIRParameters);
begin
  inherited create();
  FMeta := (params.param['meta'].value as TFhirMeta).Link; {ob.5d}
end;

destructor TFHIRMetaAddOpRequest.Destroy;
begin
  FMeta.free;
  inherited;
end;

function TFHIRMetaAddOpRequest.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FMeta <> nil) then
      result.addParameter('meta', FMeta.Link);{oz.5d}
    result.link;
  finally
    result.free;
  end;
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

constructor TFHIRMetaAddOpResponse.create(params : TFHIRParameters);
begin
  inherited create();
  FReturn := (params.param['return'].value as TFhirMeta).Link; {ob.5d}
end;

destructor TFHIRMetaAddOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRMetaAddOpResponse.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5d}
    result.link;
  finally
    result.free;
  end;
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

constructor TFHIRMetaDeleteOpRequest.create(params : TFHIRParameters);
begin
  inherited create();
  FMeta := (params.param['meta'].value as TFhirMeta).Link; {ob.5d}
end;

destructor TFHIRMetaDeleteOpRequest.Destroy;
begin
  FMeta.free;
  inherited;
end;

function TFHIRMetaDeleteOpRequest.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FMeta <> nil) then
      result.addParameter('meta', FMeta.Link);{oz.5d}
    result.link;
  finally
    result.free;
  end;
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

constructor TFHIRMetaDeleteOpResponse.create(params : TFHIRParameters);
begin
  inherited create();
  FReturn := (params.param['return'].value as TFhirMeta).Link; {ob.5d}
end;

destructor TFHIRMetaDeleteOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRMetaDeleteOpResponse.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5d}
    result.link;
  finally
    result.free;
  end;
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
end;

destructor TFHIRCdsHookOpOauth.Destroy;
begin
  inherited;
end;

function TFHIRCdsHookOpOauth.AsParams(name : String) : TFhirParametersParameter;
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
    result.link;
  finally
    result.free;
  end;
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
end;

destructor TFHIRCdsHookOpSource.Destroy;
begin
  inherited;
end;

function TFHIRCdsHookOpSource.AsParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FLabel_ <> '') then
      result.addParameter('label', TFHIRString.create(FLabel_));{oz.5f}
    if (FUrl <> '') then
      result.addParameter('url', TFHIRString.create(FUrl));{oz.5f}
    result.link;
  finally
    result.free;
  end;
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
end;

destructor TFHIRCdsHookOpSuggestion.Destroy;
begin
  FCreate_List.free;
  FDeleteList.free;
  inherited;
end;

function TFHIRCdsHookOpSuggestion.AsParams(name : String) : TFhirParametersParameter;
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
    result.link;
  finally
    result.free;
  end;
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
end;

destructor TFHIRCdsHookOpLink_.Destroy;
begin
  inherited;
end;

function TFHIRCdsHookOpLink_.AsParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FLabel_ <> '') then
      result.addParameter('label', TFHIRString.create(FLabel_));{oz.5f}
    if (FUrl <> '') then
      result.addParameter('url', TFHIRString.create(FUrl));{oz.5f}
    result.link;
  finally
    result.free;
  end;
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
end;

destructor TFHIRCdsHookOpCard.Destroy;
begin
  FSource.free;
  FSuggestionList.free;
  FLink_List.free;
  inherited;
end;

function TFHIRCdsHookOpCard.AsParams(name : String) : TFhirParametersParameter;
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
    result.link;
  finally
    result.free;
  end;
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

constructor TFHIRCdsHookOpRequest.create(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  inherited create();
  FContextList := TAdvList<TFhirResource>.create;
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

function TFHIRCdsHookOpRequest.AsParams : TFhirParameters;
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
    result.link;
  finally
    result.free;
  end;
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
end;

destructor TFHIRCdsHookOpDecision.Destroy;
begin
  FCreate_List.free;
  FDeleteList.free;
  inherited;
end;

function TFHIRCdsHookOpDecision.AsParams(name : String) : TFhirParametersParameter;
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
    result.link;
  finally
    result.free;
  end;
end;

constructor TFHIRCdsHookOpResponse.create;
begin
  inherited create();
  FDecisionList := TAdvList<TFHIRCdsHookOpDecision>.create;
end;

constructor TFHIRCdsHookOpResponse.create(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  inherited create();
  FDecisionList := TAdvList<TFHIRCdsHookOpDecision>.create;
  for p in params.parameterList do
    if p.name = 'decision' then
      FDecisionList.Add(TFHIRCdsHookOpDecision.create(p));{a}
end;

destructor TFHIRCdsHookOpResponse.Destroy;
begin
  FDecisionList.free;
  inherited;
end;

function TFHIRCdsHookOpResponse.AsParams : TFhirParameters;
var
  v1 : TFHIRCdsHookOpDecision;
begin
  result := TFHIRParameters.create;
  try
    for v1 in FDecisionList do
      result.AddParameter(v1.asParams('decision'));
    result.link;
  finally
    result.free;
  end;
end;

constructor TFHIRDocumentOpRequest.create;
begin
  inherited create();
end;

constructor TFHIRDocumentOpRequest.create(params : TFHIRParameters);
begin
  inherited create();
  FPersist := params.bool['persist'];
end;

destructor TFHIRDocumentOpRequest.Destroy;
begin
  inherited;
end;

function TFHIRDocumentOpRequest.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
      result.addParameter('persist', TFHIRBoolean.create(FPersist));{oz.5f}
    result.link;
  finally
    result.free;
  end;
end;

constructor TFHIRDocumentOpResponse.create;
begin
  inherited create();
end;

constructor TFHIRDocumentOpResponse.create(params : TFHIRParameters);
begin
  inherited create();
end;

destructor TFHIRDocumentOpResponse.Destroy;
begin
  inherited;
end;

function TFHIRDocumentOpResponse.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    result.link;
  finally
    result.free;
  end;
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
end;

destructor TFHIRTranslateOpDependency.Destroy;
begin
  FConcept.free;
  inherited;
end;

function TFHIRTranslateOpDependency.AsParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FElement <> '') then
      result.addParameter('element', TFHIRString.create(FElement));{oz.5f}
    if (FConcept <> nil) then
      result.addParameter('concept', FConcept.Link);{oz.5d}
    result.link;
  finally
    result.free;
  end;
end;

constructor TFHIRTranslateOpRequest.create;
begin
  inherited create();
  FDependencyList := TAdvList<TFHIRTranslateOpDependency>.create;
end;

constructor TFHIRTranslateOpRequest.create(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  inherited create();
  FDependencyList := TAdvList<TFHIRTranslateOpDependency>.create;
  FCode := params.str['code'];
  FSystem := params.str['system'];
  FVersion := params.str['version'];
  FValueSet := params.str['valueSet'];
  FCoding := (params.param['coding'].value as TFhirCoding).Link; {ob.5d}
  FCodeableConcept := (params.param['codeableConcept'].value as TFhirCodeableConcept).Link; {ob.5d}
  FTarget := params.str['target'];
  for p in params.parameterList do
    if p.name = 'dependency' then
      FDependencyList.Add(TFHIRTranslateOpDependency.create(p));{a}
end;

destructor TFHIRTranslateOpRequest.Destroy;
begin
  FCoding.free;
  FCodeableConcept.free;
  FDependencyList.free;
  inherited;
end;

function TFHIRTranslateOpRequest.AsParams : TFhirParameters;
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
    if (FValueSet <> '') then
      result.addParameter('valueSet', TFHIRString.create(FValueSet));{oz.5f}
    if (FCoding <> nil) then
      result.addParameter('coding', FCoding.Link);{oz.5d}
    if (FCodeableConcept <> nil) then
      result.addParameter('codeableConcept', FCodeableConcept.Link);{oz.5d}
    if (FTarget <> '') then
      result.addParameter('target', TFHIRString.create(FTarget));{oz.5f}
    for v1 in FDependencyList do
      result.AddParameter(v1.asParams('dependency'));
    result.link;
  finally
    result.free;
  end;
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
end;

destructor TFHIRTranslateOpProduct.Destroy;
begin
  FConcept.free;
  inherited;
end;

function TFHIRTranslateOpProduct.AsParams(name : String) : TFhirParametersParameter;
begin
  result := TFHIRParametersParameter.create;
  try
    result.name := name;
    if (FElement <> '') then
      result.addParameter('element', TFHIRString.create(FElement));{oz.5f}
    if (FConcept <> nil) then
      result.addParameter('concept', FConcept.Link);{oz.5d}
    result.link;
  finally
    result.free;
  end;
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
end;

destructor TFHIRTranslateOpMatch.Destroy;
begin
  FConcept.free;
  FProductList.free;
  inherited;
end;

function TFHIRTranslateOpMatch.AsParams(name : String) : TFhirParametersParameter;
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
    result.link;
  finally
    result.free;
  end;
end;

constructor TFHIRTranslateOpResponse.create;
begin
  inherited create();
  FMatchList := TAdvList<TFHIRTranslateOpMatch>.create;
end;

constructor TFHIRTranslateOpResponse.create(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  inherited create();
  FMatchList := TAdvList<TFHIRTranslateOpMatch>.create;
  FResult := params.bool['result'];
  FMessage := params.str['message'];
  for p in params.parameterList do
    if p.name = 'match' then
      FMatchList.Add(TFHIRTranslateOpMatch.create(p));{a}
end;

destructor TFHIRTranslateOpResponse.Destroy;
begin
  FMatchList.free;
  inherited;
end;

function TFHIRTranslateOpResponse.AsParams : TFhirParameters;
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
    result.link;
  finally
    result.free;
  end;
end;

constructor TFHIRClosureOpRequest.create;
begin
  inherited create();
  FConceptList := TAdvList<TFhirCoding>.create;
end;

constructor TFHIRClosureOpRequest.create(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  inherited create();
  FConceptList := TAdvList<TFhirCoding>.create;
  FName := params.str['name'];
  for p in params.parameterList do
    if p.name = 'concept' then
      FConceptList.Add((p.value as TFhirCoding).Link);{a}
  FVersion := params.str['version'];
end;

destructor TFHIRClosureOpRequest.Destroy;
begin
  FConceptList.free;
  inherited;
end;

function TFHIRClosureOpRequest.AsParams : TFhirParameters;
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
    result.link;
  finally
    result.free;
  end;
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

constructor TFHIRClosureOpResponse.create(params : TFHIRParameters);
begin
  inherited create();
  FReturn := (params.res['return'] as TFhirConceptMap).Link;{ob.5a}
end;

destructor TFHIRClosureOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRClosureOpResponse.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    result.link;
  finally
    result.free;
  end;
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
end;

constructor TFHIREvaluateOpRequest.create(params : TFHIRParameters);
begin
  inherited create();
  FRequestId := params.str['requestId'];
  FEvaluateAtDateTime := (params.param['evaluateAtDateTime'].value as TFHIRDateTime).value;
  FInputParameters := (params.res['inputParameters'] as TFhirParameters).Link;{ob.5a}
  FSetting := (params.param['setting'].value as TFhirCodeableConcept).Link; {ob.5d}
  FSettingContext := (params.param['settingContext'].value as TFhirCodeableConcept).Link; {ob.5d}
end;

destructor TFHIREvaluateOpRequest.Destroy;
begin
  FEvaluateAtDateTime.free;
  FInputParameters.free;
  FSetting.free;
  FSettingContext.free;
  inherited;
end;

function TFHIREvaluateOpRequest.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FRequestId <> '') then
      result.addParameter('requestId', TFHIRString.create(FRequestId));{oz.5f}
    if (FEvaluateAtDateTime <> nil) then
      result.addParameter('evaluateAtDateTime', TFHIRDateTime.create(FEvaluateAtDateTime));{oz.5e}
    if (FInputParameters <> nil) then
      result.addParameter('inputParameters', FInputParameters.Link);{oz.5a}
    if (FSetting <> nil) then
      result.addParameter('setting', FSetting.Link);{oz.5d}
    if (FSettingContext <> nil) then
      result.addParameter('settingContext', FSettingContext.Link);{oz.5d}
    result.link;
  finally
    result.free;
  end;
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

constructor TFHIREvaluateOpResponse.create(params : TFHIRParameters);
begin
  inherited create();
  FReturn := (params.res['return'] as TFhirGuidanceResponse).Link;{ob.5a}
end;

destructor TFHIREvaluateOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIREvaluateOpResponse.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    result.link;
  finally
    result.free;
  end;
end;

constructor TFHIREverythingOpRequest.create;
begin
  inherited create();
end;

constructor TFHIREverythingOpRequest.create(params : TFHIRParameters);
begin
  inherited create();
end;

destructor TFHIREverythingOpRequest.Destroy;
begin
  inherited;
end;

function TFHIREverythingOpRequest.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    result.link;
  finally
    result.free;
  end;
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

constructor TFHIREverythingOpResponse.create(params : TFHIRParameters);
begin
  inherited create();
  FReturn := (params.res['return'] as TFhirBundle).Link;{ob.5a}
end;

destructor TFHIREverythingOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIREverythingOpResponse.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    result.link;
  finally
    result.free;
  end;
end;

constructor TFHIRFindOpRequest.create;
begin
  inherited create();
end;

constructor TFHIRFindOpRequest.create(params : TFHIRParameters);
begin
  inherited create();
  FPatient := params.str['patient'];
  FName := params.str['name'];
end;

destructor TFHIRFindOpRequest.Destroy;
begin
  inherited;
end;

function TFHIRFindOpRequest.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FPatient <> '') then
      result.addParameter('patient', TFHIRString.create(FPatient));{oz.5f}
    if (FName <> '') then
      result.addParameter('name', TFHIRString.create(FName));{oz.5f}
    result.link;
  finally
    result.free;
  end;
end;

constructor TFHIRFindOpResponse.create;
begin
  inherited create();
end;

constructor TFHIRFindOpResponse.create(params : TFHIRParameters);
begin
  inherited create();
end;

destructor TFHIRFindOpResponse.Destroy;
begin
  inherited;
end;

function TFHIRFindOpResponse.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    result.link;
  finally
    result.free;
  end;
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

constructor TFHIRProcessMessageOpRequest.create(params : TFHIRParameters);
begin
  inherited create();
  FContent := (params.res['content'] as TFhirBundle).Link;{ob.5a}
  FAsync := params.bool['async'];
  FResponseUrl := params.str['response-url'];
end;

destructor TFHIRProcessMessageOpRequest.Destroy;
begin
  FContent.free;
  inherited;
end;

function TFHIRProcessMessageOpRequest.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FContent <> nil) then
      result.addParameter('content', FContent.Link);{oz.5a}
      result.addParameter('async', TFHIRBoolean.create(FAsync));{oz.5f}
    if (FResponseUrl <> '') then
      result.addParameter('response-url', TFHIRString.create(FResponseUrl));{oz.5f}
    result.link;
  finally
    result.free;
  end;
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

constructor TFHIRProcessMessageOpResponse.create(params : TFHIRParameters);
begin
  inherited create();
  FReturn := (params.res['return'] as TFhirBundle).Link;{ob.5a}
end;

destructor TFHIRProcessMessageOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRProcessMessageOpResponse.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    result.link;
  finally
    result.free;
  end;
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

constructor TFHIRPopulateOpRequest.create(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  inherited create();
  FContentList := TAdvList<TFhirReference>.create;
  FIdentifier := params.str['identifier'];
  FQuestionnaire := (params.res['questionnaire'] as TFhirQuestionnaire).Link;{ob.5a}
  FQuestionnaireRef := (params.param['questionnaireRef'].value as TFhirReference).Link; {ob.5d}
  FSubject := (params.param['subject'].value as TFhirReference).Link; {ob.5d}
  for p in params.parameterList do
    if p.name = 'content' then
      FContentList.Add((p.value as TFhirReference).Link);{a}
  FLocal := params.bool['local'];
end;

destructor TFHIRPopulateOpRequest.Destroy;
begin
  FQuestionnaire.free;
  FQuestionnaireRef.free;
  FSubject.free;
  FContentList.free;
  inherited;
end;

function TFHIRPopulateOpRequest.AsParams : TFhirParameters;
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
    result.link;
  finally
    result.free;
  end;
end;

procedure TFHIRPopulateOpResponse.SetReturn(value : TFhirQuestionnaireResponse);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIRPopulateOpResponse.create;
begin
  inherited create();
end;

constructor TFHIRPopulateOpResponse.create(params : TFHIRParameters);
begin
  inherited create();
  FReturn := (params.res['return'] as TFhirQuestionnaireResponse).Link;{ob.5a}
end;

destructor TFHIRPopulateOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRPopulateOpResponse.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    result.link;
  finally
    result.free;
  end;
end;

constructor TFHIRQuestionnaireOpRequest.create;
begin
  inherited create();
end;

constructor TFHIRQuestionnaireOpRequest.create(params : TFHIRParameters);
begin
  inherited create();
  FIdentifier := params.str['identifier'];
  FProfile := params.str['profile'];
  FUrl := params.str['url'];
  FSupportedOnly := params.bool['supportedOnly'];
end;

destructor TFHIRQuestionnaireOpRequest.Destroy;
begin
  inherited;
end;

function TFHIRQuestionnaireOpRequest.AsParams : TFhirParameters;
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
    result.link;
  finally
    result.free;
  end;
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

constructor TFHIRQuestionnaireOpResponse.create(params : TFHIRParameters);
begin
  inherited create();
  FReturn := (params.res['return'] as TFhirQuestionnaire).Link;{ob.5a}
end;

destructor TFHIRQuestionnaireOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRQuestionnaireOpResponse.AsParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FReturn <> nil) then
      result.addParameter('return', FReturn.Link);{oz.5a}
    result.link;
  finally
    result.free;
  end;
end;

end.

