unit fhir2_operations;

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
{$I fhir2.inc}

interface

// FHIR v1.0.2 generated 2015-10-24T07:41:03+11:00

uses
  SysUtils, Classes, Generics.Collections, 
  fsl_base, fsl_utilities, fsl_stream, fsl_http,
  fhir2_base, fhir2_types, fhir2_opbase,
  fhir2_resources_base, fhir2_resources_canonical, fhir2_resources_admin, fhir2_resources_clinical, fhir2_resources_other;

Type

  //Operation expand (Value Set Expansion)
  TFHIRExpandOpRequest = class (TFHIROperationRequest)
  private
    FIdentifier : String;
    FValueSet : TFhirValueSet;
    FContext : String;
    FFilter : String;
    FProfile : String;
    FDate : TFslDateTime;
    FOffset : String;
    FCount : String;
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
    property identifier : String read FIdentifier write FIdentifier;
    property valueSet : TFhirValueSet read FValueSet write SetValueSet;
    property context : String read FContext write FContext;
    property filter : String read FFilter write FFilter;
    property profile : String read FProfile write FProfile;
    property date : TFslDateTime read FDate write FDate;
    property offset : String read FOffset write FOffset;
    property count : String read FCount write FCount;
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

  //Operation lookup (Concept Look Up)
  TFHIRLookupOpRequest = class (TFHIROperationRequest)
  private
    FCode : String;
    FSystem : String;
    FVersion : String;
    FCoding : TFhirCoding;
    FDate : TFslDateTime;
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

  TFHIRLookupOpResponse = class (TFHIROperationResponse)
  private
    FName : String;
    FVersion : String;
    FDisplay : String;
    FAbstract : Boolean;
    FDesignationList : TFslList<TFHIRLookupOpRespDesignation>;
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
    property abstract : Boolean read FAbstract write FAbstract;
    property designationList : TFslList<TFHIRLookupOpRespDesignation> read FDesignationList;
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
    FDate : TFslDateTime;
    FAbstract : Boolean;
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
    property identifier : String read FIdentifier write FIdentifier;
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
    FValueSet : String;
    FCoding : TFhirCoding;
    FCodeableConcept : TFhirCodeableConcept;
    FTarget : String;
    FDependencyList : TFslList<TFHIRTranslateOpReqDependency>;
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
    property valueSet : String read FValueSet write FValueSet;
    property coding : TFhirCoding read FCoding write SetCoding;
    property codeableConcept : TFhirCodeableConcept read FCodeableConcept write SetCodeableConcept;
    property target : String read FTarget write FTarget;
    property dependencyList : TFslList<TFHIRTranslateOpReqDependency> read FDependencyList;
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
    FReturn : TFhirQuestionnaireResponse;
    procedure SetReturn(value : TFhirQuestionnaireResponse);
  protected
    function isKnownName(name : String) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure load(params : TFHIRParameters); overload; override;
    procedure load(params : THTTPParameters); overload; override;
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

implementation

uses
  fhir2_utilities;

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
  FIdentifier := params.str['identifier'];
  FValueSet := (params.res['valueSet'] as TFhirValueSet).Link;{ob.5a}
  FContext := params.str['context'];
  FFilter := params.str['filter'];
  FProfile := params.str['profile'];
  FDate := TFslDateTime.fromXml(params.str['date']);
  FOffset := params.str['offset'];
  FCount := params.str['count'];
  loadExtensions(params);
end;

procedure TFHIRExpandOpRequest.load(params : THTTPParameters);
begin
  FIdentifier := params['identifier'];
  FContext := params['context'];
  FFilter := params['filter'];
  FProfile := params['profile'];
  FDate := TFslDateTime.fromXml(params['date']);
  FOffset := params['offset'];
  FCount := params['count'];
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
    if (FIdentifier <> '') then
      result.addParameter('identifier', TFHIRUri.create(FIdentifier));{oz.5f}
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

function TFHIRExpandOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FIdentifier.length * sizeof(char)) + 12);
  inc(result, FValueSet.sizeInBytes);
  inc(result, (FContext.length * sizeof(char)) + 12);
  inc(result, (FFilter.length * sizeof(char)) + 12);
  inc(result, (FProfile.length * sizeof(char)) + 12);
  inc(result, (FOffset.length * sizeof(char)) + 12);
  inc(result, (FCount.length * sizeof(char)) + 12);
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

procedure TFHIRLookupOpRequest.SetCoding(value : TFhirCoding);
begin
  FCoding.free;
  FCoding := value;
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
  if params.param['coding'] <> nil then
    FCoding := (params.param['coding'].value as TFhirCoding).Link; {ob.5d}
  FDate := TFslDateTime.fromXml(params.str['date']);
  loadExtensions(params);
end;

procedure TFHIRLookupOpRequest.load(params : THTTPParameters);
begin
  FCode := params['code'];
  FSystem := params['system'];
  FVersion := params['version'];
  FDate := TFslDateTime.fromXml(params['date']);
  loadExtensions(params);
end;

destructor TFHIRLookupOpRequest.Destroy;
begin
  FCoding.free;
  inherited;
end;

function TFHIRLookupOpRequest.asParams : TFhirParameters;
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

function TFHIRLookupOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCode.length * sizeof(char)) + 12);
  inc(result, (FSystem.length * sizeof(char)) + 12);
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, FCoding.sizeInBytes);
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

constructor TFHIRLookupOpResponse.create;
begin
  inherited create();
  FDesignationList := TFslList<TFHIRLookupOpRespDesignation>.create;
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
      FDesignationList.Add(TFHIRLookupOpRespDesignation.create(p));{a}
  loadExtensions(params);
end;

procedure TFHIRLookupOpResponse.load(params : THTTPParameters);
begin
  FName := params['name'];
  FVersion := params['version'];
  FDisplay := params['display'];
  FAbstract := StrToBoolDef(params['abstract'], false);
  loadExtensions(params);
end;

destructor TFHIRLookupOpResponse.Destroy;
begin
  FDesignationList.free;
  inherited;
end;

function TFHIRLookupOpResponse.asParams : TFhirParameters;
var
  v1 : TFHIRLookupOpRespDesignation;
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

function TFHIRLookupOpResponse.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, (FDisplay.length * sizeof(char)) + 12);
  inc(result, FDesignationList.sizeInBytes);
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
  FIdentifier := params.str['identifier'];
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
  loadExtensions(params);
end;

procedure TFHIRValidateCodeOpRequest.load(params : THTTPParameters);
begin
  FIdentifier := params['identifier'];
  FContext := params['context'];
  FCode := params['code'];
  FSystem := params['system'];
  FVersion := params['version'];
  FDisplay := params['display'];
  FDate := TFslDateTime.fromXml(params['date']);
  FAbstract := StrToBoolDef(params['abstract'], false);
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
    if (FIdentifier <> '') then
      result.addParameter('identifier', TFHIRUri.create(FIdentifier));{oz.5f}
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

function TFHIRValidateCodeOpRequest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FIdentifier.length * sizeof(char)) + 12);
  inc(result, (FContext.length * sizeof(char)) + 12);
  inc(result, FValueSet.sizeInBytes);
  inc(result, (FCode.length * sizeof(char)) + 12);
  inc(result, (FSystem.length * sizeof(char)) + 12);
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, (FDisplay.length * sizeof(char)) + 12);
  inc(result, FCoding.sizeInBytes);
  inc(result, FCodeableConcept.sizeInBytes);
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
  inc(result, (FValueSet.length * sizeof(char)) + 12);
  inc(result, FCoding.sizeInBytes);
  inc(result, FCodeableConcept.sizeInBytes);
  inc(result, (FTarget.length * sizeof(char)) + 12);
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
  FValueSet := params.str['valueSet'];
  if params.param['coding'] <> nil then
    FCoding := (params.param['coding'].value as TFhirCoding).Link; {ob.5d}
  if params.param['codeableConcept'] <> nil then
    FCodeableConcept := (params.param['codeableConcept'].value as TFhirCodeableConcept).Link; {ob.5d}
  FTarget := params.str['target'];
  for p in params.parameterList do
    if p.name = 'dependency' then
      FDependencyList.Add(TFHIRTranslateOpReqDependency.create(p));{a}
  loadExtensions(params);
end;

procedure TFHIRTranslateOpRequest.load(params : THTTPParameters);
begin
  FCode := params['code'];
  FSystem := params['system'];
  FVersion := params['version'];
  FValueSet := params['valueSet'];
  FTarget := params['target'];
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
    if (FValueSet <> '') then
      result.addParameter('valueSet', TFHIRUri.create(FValueSet));{oz.5f}
    if (FCoding <> nil) then
      result.addParameter('coding', FCoding.Link);{oz.5d}
    if (FCodeableConcept <> nil) then
      result.addParameter('codeableConcept', FCodeableConcept.Link);{oz.5d}
    if (FTarget <> '') then
      result.addParameter('target', TFHIRUri.create(FTarget));{oz.5f}
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
    writeExtensions(result);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRTranslateOpRespMatch.isKnownName(name : String) : boolean;
begin
  result := StringArrayExists(['equivalence', 'concept', 'product'], name);
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

procedure TFHIRPopulateOpResponse.load(params : THTTPParameters);
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

function TFHIRPopulateOpResponse.sizeInBytesV : cardinal;
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

end.

