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

{$IFNDEF FHIR2}
This is the dstu2 version of the FHIR code
{$ENDIF}


interface

// FHIR v1.0.2 generated 2015-12-22T12:09:07+11:00

uses
  SysUtils, Classes, Generics.Collections, StringSupport, DecimalSupport, AdvBuffers, AdvGenerics, ParseMap, DateAndTime, FHIRBase, FHIRTypes, FHIRResources, FHIROpBase;

Type

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
    constructor create; overload; override;
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
    constructor create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
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
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property code : String read FCode write FCode;
    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property coding : TFhirCoding read FCoding write SetCoding;
    property date : TDateAndTime read FDate write SetDate;
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
    constructor create; overload;
    constructor create(params : TFhirParametersParameter); overload;
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
    FAbstract : Boolean;
    FDesignationList : TAdvList<TFHIRLookupOpDesignation>;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
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
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor create; overload; override;
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
    constructor create; overload; override;
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
    constructor create; overload; override;
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
    constructor create; overload; override;
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
    constructor create; overload; override;
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
    constructor create; overload; override;
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
    constructor create; overload; override;
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
    constructor create; overload; override;
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
    constructor create; overload; override;
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
    constructor create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirMeta read FReturn write SetReturn;
  end;

  //Operation document (Generate a Document)
  TFHIRDocumentOpRequest = class (TFHIROperationRequest)
  private
    FPersist : Boolean;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor create; overload; override;
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
    constructor create; overload; override;
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
    constructor create; overload;
    constructor create(params : TFhirParametersParameter); overload;
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
    FValueSet : String;
    FCoding : TFhirCoding;
    FCodeableConcept : TFhirCodeableConcept;
    FTarget : String;
    FDependencyList : TAdvList<TFHIRTranslateOpDependency>;
    procedure SetCoding(value : TFhirCoding);
    procedure SetCodeableConcept(value : TFhirCodeableConcept);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property code : String read FCode write FCode;
    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property valueSet : String read FValueSet write FValueSet;
    property coding : TFhirCoding read FCoding write SetCoding;
    property codeableConcept : TFhirCodeableConcept read FCodeableConcept write SetCodeableConcept;
    property target : String read FTarget write FTarget;
    property dependencyList : TAdvList<TFHIRTranslateOpDependency> read FDependencyList;
  end;

  TFHIRTranslateOpProduct = class (TFHIROperationObject)
  private
    FElement : String;
    FConcept : TFhirCoding;
    procedure SetConcept(value : TFhirCoding);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor create; overload;
    constructor create(params : TFhirParametersParameter); overload;
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
    procedure SetConcept(value : TFhirCoding);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor create; overload;
    constructor create(params : TFhirParametersParameter); overload;
    destructor Destroy; override;
    function asParams(name : String) : TFHIRParametersParameter;
    property equivalence : String read FEquivalence write FEquivalence;
    property concept : TFhirCoding read FConcept write SetConcept;
    property productList : TAdvList<TFHIRTranslateOpProduct> read FProductList;
  end;

  TFHIRTranslateOpResponse = class (TFHIROperationResponse)
  private
    FResult : Boolean;
    FMessage : String;
    FMatchList : TAdvList<TFHIRTranslateOpMatch>;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor create; overload; override;
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
    constructor create; overload; override;
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
    constructor create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirConceptMap read FReturn write SetReturn;
  end;

  //Operation everything (Fetch Encounter Record)
  TFHIREverythingOpRequest = class (TFHIROperationRequest)
  private
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor create; overload; override;
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
    constructor create; overload; override;
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
    constructor create; overload; override;
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
    constructor create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
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
    constructor create; overload; override;
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
    constructor create; overload; override;
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
    constructor create; overload; override;
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
    FReturn : TFhirQuestionnaireResponse;
    procedure SetReturn(value : TFhirQuestionnaireResponse);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirQuestionnaireResponse read FReturn write SetReturn;
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
    constructor create; overload; override;
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
    constructor create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property return : TFhirQuestionnaire read FReturn write SetReturn;
  end;

  //Operation guidance (guidance)
  TFHIRGuidanceOpRequest = class (TFHIROperationRequest)
  private
    FRequestList : TAdvList<TFhirBasic>;
    FInputResourceList : TAdvList<TFhirDomainResource>;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property requestList : TAdvList<TFhirBasic> read FRequestList;
    property inputResourceList : TAdvList<TFhirDomainResource> read FInputResourceList;
  end;

  TFHIRGuidanceOpResponse = class (TFHIROperationResponse)
  private
    FResponseList : TAdvList<TFhirBasic>;
    FOutputResourceList : TAdvList<TFhirDomainResource>;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property responseList : TAdvList<TFhirBasic> read FResponseList;
    property outputResourceList : TAdvList<TFhirDomainResource> read FOutputResourceList;
  end;

  //Operation guidance-requirements (guidanceRequirements)
  TFHIRGuidanceRequirementsOpRequest = class (TFHIROperationRequest)
  private
    FModuleIdentifierList : TAdvList<TFhirIdentifier>;
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property moduleIdentifierList : TAdvList<TFhirIdentifier> read FModuleIdentifierList;
  end;

  TFHIRGuidanceRequirementsOpResponse = class (TFHIROperationResponse)
  private
    FResult : TFhirBasic;
    procedure SetResult(value : TFhirBasic);
  protected
    function isKnownName(name : String) : boolean; override;
  public
    constructor create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : TParseMap); overload; override;
    function asParams : TFHIRParameters; override;
    property result : TFhirBasic read FResult write SetResult;
  end;

implementation

uses
  FHIRUtilities;

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

procedure TFHIRLookupOpRequest.load(params : TFHIRParameters);
begin
  FCode := params.str['code'];
  FSystem := params.str['system'];
  FVersion := params.str['version'];
  FCoding := (params.param['coding'].value as TFhirCoding).Link; {ob.5d}
  FDate := (params.param['date'].value as TFHIRDateTime).value;
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
  inherited;
end;

function TFHIRLookupOpRequest.asParams : TFhirParameters;
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
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRLookupOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['code', 'system', 'version', 'coding', 'date'], name);
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
  FDesignationList := TAdvList<TFHIRLookupOpDesignation>.create;
end;

procedure TFHIRLookupOpResponse.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  FName := params.str['name'];
  FVersion := params.str['version'];
  FDisplay := params.str['display'];
  FAbstract := params.bool['abstract'];
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
  FAbstract := StrToBool(params.getVar('abstract'));
  loadExtensions(params);
end;

destructor TFHIRLookupOpResponse.Destroy;
begin
  FDesignationList.free;
  inherited;
end;

function TFHIRLookupOpResponse.asParams : TFhirParameters;
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
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRLookupOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['name', 'version', 'display', 'abstract', 'designation'], name);
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
  FValueSet := params.str['valueSet'];
  FCoding := (params.param['coding'].value as TFhirCoding).Link; {ob.5d}
  FCodeableConcept := (params.param['codeableConcept'].value as TFhirCodeableConcept).Link; {ob.5d}
  FTarget := params.str['target'];
  for p in params.parameterList do
    if p.name = 'dependency' then
      FDependencyList.Add(TFHIRTranslateOpDependency.create(p));{a}
  loadExtensions(params);
end;

procedure TFHIRTranslateOpRequest.load(params : TParseMap);
begin
  FCode := params.getVar('code');
  FSystem := params.getVar('system');
  FVersion := params.getVar('version');
  FValueSet := params.getVar('valueSet');
  FTarget := params.getVar('target');
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
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRTranslateOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['code', 'system', 'version', 'valueSet', 'coding', 'codeableConcept', 'target', 'dependency'], name);
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
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRTranslateOpMatch.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['equivalence', 'concept', 'product'], name);
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

procedure TFHIRPopulateOpResponse.SetReturn(value : TFhirQuestionnaireResponse);
begin
  FReturn.free;
  FReturn := value;
end;

constructor TFHIRPopulateOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRPopulateOpResponse.load(params : TFHIRParameters);
begin
  FReturn := (params.res['return'] as TFhirQuestionnaireResponse).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRPopulateOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRPopulateOpResponse.Destroy;
begin
  FReturn.free;
  inherited;
end;

function TFHIRPopulateOpResponse.asParams : TFhirParameters;
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

function TFHIRPopulateOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['return'], name);
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

constructor TFHIRGuidanceOpRequest.create;
begin
  inherited create();
  FRequestList := TAdvList<TFhirBasic>.create;
  FInputResourceList := TAdvList<TFhirDomainResource>.create;
end;

procedure TFHIRGuidanceOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  for p in params.parameterList do
    if p.name = 'request' then
      FRequestList.Add((p.resource as TFhirBasic).Link);{ob.2}
  for p in params.parameterList do
    if p.name = 'inputResource' then
      FInputResourceList.Add((p.resource as TFhirDomainResource).Link);{ob.2}
  loadExtensions(params);
end;

procedure TFHIRGuidanceOpRequest.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRGuidanceOpRequest.Destroy;
begin
  FRequestList.free;
  FInputResourceList.free;
  inherited;
end;

function TFHIRGuidanceOpRequest.asParams : TFhirParameters;
var
  v1 : TFhirBasic;
  v2 : TFhirDomainResource;
begin
  result := TFHIRParameters.create;
  try
    for v1 in FRequestList do
      result.AddParameter('request', v1.Link);
    for v2 in FInputResourceList do
      result.AddParameter('inputResource', v2.Link);
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRGuidanceOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['request', 'inputResource'], name);
end;

constructor TFHIRGuidanceOpResponse.create;
begin
  inherited create();
  FResponseList := TAdvList<TFhirBasic>.create;
  FOutputResourceList := TAdvList<TFhirDomainResource>.create;
end;

procedure TFHIRGuidanceOpResponse.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  for p in params.parameterList do
    if p.name = 'response' then
      FResponseList.Add((p.resource as TFhirBasic).Link);{ob.2}
  for p in params.parameterList do
    if p.name = 'outputResource' then
      FOutputResourceList.Add((p.resource as TFhirDomainResource).Link);{ob.2}
  loadExtensions(params);
end;

procedure TFHIRGuidanceOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRGuidanceOpResponse.Destroy;
begin
  FResponseList.free;
  FOutputResourceList.free;
  inherited;
end;

function TFHIRGuidanceOpResponse.asParams : TFhirParameters;
var
  v1 : TFhirBasic;
  v2 : TFhirDomainResource;
begin
  result := TFHIRParameters.create;
  try
    for v1 in FResponseList do
      result.AddParameter('response', v1.Link);
    for v2 in FOutputResourceList do
      result.AddParameter('outputResource', v2.Link);
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRGuidanceOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['response', 'outputResource'], name);
end;

constructor TFHIRGuidanceRequirementsOpRequest.create;
begin
  inherited create();
  FModuleIdentifierList := TAdvList<TFhirIdentifier>.create;
end;

procedure TFHIRGuidanceRequirementsOpRequest.load(params : TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  for p in params.parameterList do
    if p.name = 'moduleIdentifier' then
      FModuleIdentifierList.Add((p.value as TFhirIdentifier).Link);{a}
  loadExtensions(params);
end;

procedure TFHIRGuidanceRequirementsOpRequest.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRGuidanceRequirementsOpRequest.Destroy;
begin
  FModuleIdentifierList.free;
  inherited;
end;

function TFHIRGuidanceRequirementsOpRequest.asParams : TFhirParameters;
var
  v1 : TFhirIdentifier;
begin
  result := TFHIRParameters.create;
  try
    for v1 in FModuleIdentifierList do
      result.AddParameter('moduleIdentifier', v1.Link);
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRGuidanceRequirementsOpRequest.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['moduleIdentifier'], name);
end;

procedure TFHIRGuidanceRequirementsOpResponse.SetResult(value : TFhirBasic);
begin
  FResult.free;
  FResult := value;
end;

constructor TFHIRGuidanceRequirementsOpResponse.create;
begin
  inherited create();
end;

procedure TFHIRGuidanceRequirementsOpResponse.load(params : TFHIRParameters);
begin
  FResult := (params.res['result'] as TFhirBasic).Link;{ob.5a}
  loadExtensions(params);
end;

procedure TFHIRGuidanceRequirementsOpResponse.load(params : TParseMap);
begin
  loadExtensions(params);
end;

destructor TFHIRGuidanceRequirementsOpResponse.Destroy;
begin
  FResult.free;
  inherited;
end;

function TFHIRGuidanceRequirementsOpResponse.asParams : TFhirParameters;
begin
  result := TFHIRParameters.create;
  try
    if (FResult <> nil) then
      result.addParameter('result', FResult.Link);{oz.5a}
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRGuidanceRequirementsOpResponse.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['result'], name);
end;

end.

