unit InstanceValidator;

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

interface

uses
  Windows, SysUtils, Classes, RegularExpressions, Character,
  fsl_base, fsl_utilities, fsl_fpc,
  fhir_objects, fhir_utilities, fhir_xhtml, fhir_factory, 
  fhir4_types, fhir4_resources, fhir4_resources_base, fhir4_utilities, fhir4_elementmodel, fhir4_validator, fhir4_adaptor, fhir4_context, fhir4_pathnode, fhir4_pathengine;

Type
  TFHIRRferenceRefetcher = class (TFslObject)
  private
  public
    function validationPolicy(context : TObject; path, ref : String) : TFhirReferenceValidationPolicy;
    function fetch(context : TObject; ref : String) : TFhirMMElement;
  end;

  TFhirChildIterator = class (TFslObject)
  private
    function getElement : TFHIRMMElement;
    function getName : string;
    function getPath : string;
    function getCount : integer;
  public
    constructor Create(path : String; element : TFHIRMMElement);
    function next : boolean;
    property element : TFHIRMMElement read GetElement;
    property name : String read GetName;
    property path : String read GetPath;
    property count : integer read GetCount;
  end;

  TInstanceValidator = class (TFslObject)
  private
    extensionDomains : TStringList;
    anyExtensionsAllowed : boolean;
    bpWarnings : TBastPracticeWarnings;
    txTime, sdTime, fpeTime, overall, loadTime : cardinal;
    noTerminologyChecks : boolean;
    noExtensibleWarnings : boolean;
    noBindingMsgSuppressed : boolean;
    fetcher : TFHIRRferenceRefetcher;
    fpe : TFHIRPathEngine;

    function rule(errors : TFslList<TFhirValidationMessage>; invalid : TFhirIssueType; line : integer; col : integer; literalPath : String; test : boolean; message : String) : boolean;
    function warning(errors : TFslList<TFhirValidationMessage>; invalid : TFhirIssueType; line : integer; col : integer; literalPath : String; test : boolean; message : String) : boolean;
    function hint(errors : TFslList<TFhirValidationMessage>; invalid : TFhirIssueType; line : integer; col : integer; literalPath : String; test : boolean; message : String) : boolean;

    function allowUnknownExtension(url : String) : boolean; overload; virtual;
    function isKnownExtension(url : String) : boolean; overload; virtual;
    procedure bpCheck(errors : TFslList<TFhirValidationMessage>; invalid : TFhirIssueType; line : integer; col : integer; literalPath : String; test : boolean; message : String); overload; virtual;
    procedure validateRemainder(appContext : TFhirObject; errors : TFslList<TFhirValidationMessage>); overload; virtual;
    procedure checkElementUsage(errors : TFslList<TFhirValidationMessage>; element : TFhirMMElement; stack : TNodeStack); overload; virtual;
    function check(v1 : String; v2 : String) : boolean; overload; virtual;
    procedure checkAddress(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirAddress); overload; virtual;
    procedure checkAttachment(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirAttachment); overload; virtual;
    function checkCode(errors : TFslList<TFhirValidationMessage>; element : TFhirMMElement; path : String; code : String; system : String; display : String) : boolean; overload; virtual;
    function startsWithButIsNot(system : String; uris : Array of String) : boolean; overload; virtual;
    procedure checkCodeableConcept(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirCodeableConcept); overload; virtual;
    procedure checkCodeableConcept(errors : TFslList<TFhirValidationMessage>; path : String; element : TFhirMMElement; profile : TFhirStructureDefinition; theElementCntext : TFhirElementDefinition); overload; virtual;
    procedure checkMaxValueSet(errors : TFslList<TFhirValidationMessage>; path : String; element : TFhirMMElement; profile : TFhirStructureDefinition; maxVSUrl : String; cc : TFhirCodeableConcept); overload; virtual;
    procedure checkMaxValueSet(errors : TFslList<TFhirValidationMessage>; path : String; element : TFhirMMElement; profile : TFhirStructureDefinition; maxVSUrl : String; c : TFhirCoding); overload; virtual;
    procedure checkMaxValueSet(errors : TFslList<TFhirValidationMessage>; path : String; element : TFhirMMElement; profile : TFhirStructureDefinition; maxVSUrl : String; c : String); overload; virtual;
    function ccSummary(cc : TFhirCodeableConcept) : String; overload; virtual;
    procedure checkCoding(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirCoding); overload; virtual;
    procedure checkCoding(errors : TFslList<TFhirValidationMessage>; path : String; element : TFhirMMElement; profile : TFhirStructureDefinition; theElementCntext : TFhirElementDefinition; inCodeableConcept : boolean); overload; virtual;
    function isValueSet(url : String) : boolean; overload; virtual;
    procedure checkContactPoint(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirContactPoint); overload; virtual;
    procedure checkDeclaredProfiles(resourceProfiles : TValidationProfileSet; errors : TFslList<TFhirValidationMessage>; resource : TFhirMMElement; element : TFhirMMElement; stack : TNodeStack); overload; virtual;
    function checkExtension(hostContext : TFhirValidatorHostContext; errors : TFslList<TFhirValidationMessage>; path : String; resource : TFhirMMElement; element : TFhirMMElement; def : TFhirElementDefinition; profile : TFhirStructureDefinition; stack : TNodeStack) : TFhirStructureDefinition; overload; virtual;
    function getExtensionType(element : TFhirMMElement) : String; overload; virtual;
    function listExtensionTypes(ex : TFhirStructureDefinition) : TFslStringSet; overload; virtual;
    function checkExtensionContext(errors : TFslList<TFhirValidationMessage>; element : TFhirMMElement; definition : TFhirStructureDefinition; stack : TNodeStack; extensionParent : String) : boolean; overload; virtual;
    procedure checkFixedValue(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFHIRElement; propName : String; parent : TFhirMMElement); overload; virtual;
    procedure checkHumanName(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirHumanName); overload; virtual;
    procedure checkIdentifier(errors : TFslList<TFhirValidationMessage>; path : String; element : TFhirMMElement; context : TFhirElementDefinition); overload; virtual;
    procedure checkIdentifier(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirIdentifier); overload; virtual;
    procedure checkPeriod(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirPeriod); overload; virtual;
    procedure checkPrimitive(appContext : TFslObject; errors : TFslList<TFhirValidationMessage>; path : String; type_ : String; context : TFhirElementDefinition; e : TFhirMMElement; profile : TFhirStructureDefinition); overload; virtual;
    procedure checkInnerNames(errors : TFslList<TFhirValidationMessage>; e : TFhirMMElement; path : String; list : TFhirXHtmlNodeList); overload; virtual;
    procedure checkInnerNS(errors : TFslList<TFhirValidationMessage>; e : TFhirMMElement; path : String; list : TFhirXHtmlNodeList); overload; virtual;
    procedure checkPrimitiveBinding(errors : TFslList<TFhirValidationMessage>; path : String; type_ : String; elementContext : TFhirElementDefinition; element : TFhirMMElement; profile : TFhirStructureDefinition); overload; virtual;
    procedure checkQuantity(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirQuantity); overload; virtual;
    procedure checkRange(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirRange); overload; virtual;
    procedure checkRatio(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirRatio); overload; virtual;
    procedure checkReference(hostContext : TFhirValidatorHostContext; errors : TFslList<TFhirValidationMessage>; path : String; element : TFhirMMElement; profile : TFhirStructureDefinition; container : TFhirElementDefinition; parentType : String; stack : TNodeStack); overload; virtual;
    procedure doResourceProfile(hostContext : TFhirValidatorHostContext; resource : TFhirMMElement; profile : String; errors : TFslList<TFhirValidationMessage>; stack : TNodeStack; path : String; element : TFhirMMElement); overload; virtual;
    function getResourceProfiles(resource : TFhirMMElement; stack : TNodeStack) : TValidationProfileSet; overload; virtual;
    function addResourceProfile(errors : TFslList<TFhirValidationMessage>; resource : TFhirMMElement; profile : String; path : String; element : TFhirMMElement; stack : TNodeStack) : TValidationProfileSet; overload; virtual;
    function checkResourceType(type_ : String) : String; overload; virtual;
    procedure checkSampledData(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirSampledData); overload; virtual;
    procedure checkTiming(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirTiming); overload; virtual;
    function codeinExpansion(cnt : TFhirValueSetExpansionContains; system : String; code : String) : boolean; overload; virtual;
    function codeInExpansion(vs : TFhirValueSet; system : String; code : String) : boolean; overload; virtual;
    function describeReference(reference : String) : String; overload; virtual;
    function describeTypes(types : TFhirElementDefinitionTypeList) : String; overload; virtual;
    function findElement(profile : TFhirStructureDefinition; name : String) : TFhirElementDefinition; overload; virtual;
    function getBaseType(profile : TFhirStructureDefinition; pr : String) : String; overload; virtual;
    function getCodeDefinition(c : TFhirCodeSystemConcept; code : String) : TFhirCodeSystemConcept; overload; virtual;
    function getCodeDefinition(cs : TFhirCodeSystem; code : String) : TFhirCodeSystemConcept; overload; virtual;
    function getContainedById(container : TFhirMMElement; id : String) : TFhirMMElement; overload; virtual;
    function getCriteriaForDiscriminator(path : String; element : TFhirElementDefinition; discriminator : String; profile : TFhirStructureDefinition; removeResolve : boolean) : TFhirElementDefinition; overload; virtual;
    function getExtensionByUrl(extensions : TFslList<TFhirMMElement>; urlSimple : String) : TFhirMMElement; overload; virtual;
    function getFromBundle(bundle : TFhirMMElement; ref : String; fullUrl : String; errors : TFslList<TFhirValidationMessage>; path : String) : TFhirMMElement; overload; virtual;
    function getProfileForType(type_ : String; list : TFhirElementDefinitionTypeList) : TFhirStructureDefinition; overload; virtual;
    function getValueForDiscriminator(appContext : TFhirObject; errors : TFslList<TFhirValidationMessage>; element : TFhirMMElement; discriminator : String; criteria : TFhirElementDefinition; stack : TNodeStack) : TFhirMMElement; overload; virtual;
    function getCodeSystem(system : String) : TFhirCodeSystem; overload; virtual;
    function hasTime(fmt : String) : boolean; overload; virtual;
    function hasTimeZone(fmt : String) : boolean; overload; virtual;
    function isAbsolute(uri : String) : boolean; overload; virtual;
    function isValidFHIRUrn(uri : String) : boolean; overload; virtual;
    function isParametersEntry(path : String) : boolean; overload; virtual;
    function isBundleEntry(path : String) : boolean; overload; virtual;
    function isBundleOutcome(path : String) : boolean; overload; virtual;
    class function pathEntryHasName(thePathEntry : String; theName : String) : boolean; overload; virtual;
    function isPrimitiveType(code : String) : boolean; overload; virtual;
    function nameMatches(name : String; tail : String) : boolean; overload; virtual;
    function passesCodeWhitespaceRules(v : String) : boolean; overload; virtual;
    function localResolve(ref : String; stack : TNodeStack; errors : TFslList<TFhirValidationMessage>; path : String) : TFhirMMElement; overload; virtual;
    function resolve(appContext : TFhirObject; ref : String; stack : TNodeStack; errors : TFslList<TFhirValidationMessage>; path : String) : TFhirMMElement; overload; virtual;
    function resolveBindingReference(ctxt : TFhirDomainResource; reference : String; uri : String) : TFhirValueSet; overload; virtual;
    function resolve(uri : String; ref : String) : String; overload; virtual;
    function resolveInBundle(entries : TFslList<TFhirMMElement>; ref : String; fullUrl : String; type_ : String; id : String) : TFhirMMElement; overload; virtual;
    function resolveNameReference(snapshot : TFhirStructureDefinitionSnapshot; contentReference : String) : TFhirElementDefinition; overload; virtual;
    function resolveProfile(profile : TFhirStructureDefinition; pr : String) : TFhirStructureDefinition; overload; virtual;
    function resolveType(type_ : String; list : TFhirElementDefinitionTypeList) : TFhirElementDefinition; overload; virtual;
    function sliceMatches(hostContext : TFhirValidatorHostContext; element : TFhirMMElement; path : String; slicer : TFhirElementDefinition; ed : TFhirElementDefinition; profile : TFhirStructureDefinition; errors : TFslList<TFhirValidationMessage>; stack : TNodeStack) : boolean; overload; virtual;
    function evaluateSlicingExpression(hostContext : TFhirValidatorHostContext; element : TFhirMMElement; path : String; profile : TFhirStructureDefinition; expression : TFHIRPathExpressionNode) : boolean; overload; virtual;
    procedure buildPattternExpression(ed : TFhirElementDefinition; expression : TStringBuilder; discriminator : String; criteriaElement : TFhirElementDefinition); overload; virtual;
    procedure buildCodeableConceptExpression(ed : TFhirElementDefinition; expression : TStringBuilder; discriminator : String; cc : TFhirCodeableConcept); overload; virtual;
    procedure buildFixedExpression(ed : TFhirElementDefinition; expression : TStringBuilder; discriminator : String; criteriaElement : TFhirElementDefinition); overload; virtual;
    procedure start(hostContext : TFhirValidatorHostContext; errors : TFslList<TFhirValidationMessage>; resource : TFhirMMElement; element : TFhirMMElement; defn : TFhirStructureDefinition; stack : TNodeStack); overload; virtual;
    procedure validateCodeSystem(errors : TFslList<TFhirValidationMessage>; cs : TFhirMMElement; stack : TNodeStack); overload; virtual;
    procedure validateQuestionannaireResponse(errors : TFslList<TFhirValidationMessage>; element : TFhirMMElement; stack : TNodeStack); overload; virtual;
    procedure validateQuestionannaireResponseItem(qsrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; errors : TFslList<TFhirValidationMessage>; element : TFhirMMElement; stack : TNodeStack; inProgress : boolean); overload; virtual;
    procedure validateQuestionannaireResponseItem(qsrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; errors : TFslList<TFhirValidationMessage>; elements : TFslList<TFhirMMElement>; stack : TNodeStack; inProgress : boolean); overload; virtual;
    function getLinkIdIndex(qItems : TFslList<TFhirQuestionnaireItem>; linkId : String) : integer; overload; virtual;
    procedure validateQuestionannaireResponseItems(qsrc : TFhirQuestionnaire; qItems : TFslList<TFhirQuestionnaireItem>; errors : TFslList<TFhirValidationMessage>; element : TFhirMMElement; stack : TNodeStack; inProgress : boolean); overload; virtual;
    procedure validateQuestionnaireResponseItemQuantity(errors : TFslList<TFhirValidationMessage>; answer : TFhirMMElement; stack : TNodeStack); overload; virtual;
    function validateQuestionnaireResponseItemType(errors : TFslList<TFhirValidationMessage>; element : TFhirMMElement; stack : TNodeStack; types : String) : String; overload; virtual;
    function findQuestionnaireItem(qSrc : TFhirQuestionnaire; linkId : String) : TFhirQuestionnaireItem; overload; virtual;
    function findItem(list : TFslList<TFhirQuestionnaireItem>; linkId : String) : TFhirQuestionnaireItem; overload; virtual;
    procedure validateAnswerCode(errors : TFslList<TFhirValidationMessage>; value : TFhirMMElement; stack : TNodeStack; qSrc : TFhirQuestionnaire; ref : String; theOpenChoice : boolean); overload; virtual;
    procedure validateAnswerCode(errors : TFslList<TFhirValidationMessage>; answer : TFhirMMElement; stack : TNodeStack; qSrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; theOpenChoice : boolean); overload; virtual;
    procedure checkOption(errors : TFslList<TFhirValidationMessage>; answer : TFhirMMElement; stack : TNodeStack; qSrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; type_ : String); overload; virtual;
    procedure checkOption(errors : TFslList<TFhirValidationMessage>; answer : TFhirMMElement; stack : TNodeStack; qSrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; type_ : String; openChoice : boolean); overload; virtual;
    procedure checkIntegerOption(errors : TFslList<TFhirValidationMessage>; answer : TFhirMMElement; stack : TNodeStack; qSrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; openChoice : boolean); overload; virtual;
    procedure checkDateOption(errors : TFslList<TFhirValidationMessage>; answer : TFhirMMElement; stack : TNodeStack; qSrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; openChoice : boolean); overload; virtual;
    procedure checkTimeOption(errors : TFslList<TFhirValidationMessage>; answer : TFhirMMElement; stack : TNodeStack; qSrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; openChoice : boolean); overload; virtual;
    procedure checkStringOption(errors : TFslList<TFhirValidationMessage>; answer : TFhirMMElement; stack : TNodeStack; qSrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; openChoice : boolean); overload; virtual;
    procedure checkCodingOption(errors : TFslList<TFhirValidationMessage>; answer : TFhirMMElement; stack : TNodeStack; qSrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; openChoice : boolean); overload; virtual;
    function tail(path : String) : String; overload; virtual;
    function tryParse(ref : String) : String; overload; virtual;
    function typesAreAllReference(theType : TFhirElementDefinitionTypeList) : boolean; overload; virtual;
    procedure validateBundle(errors : TFslList<TFhirValidationMessage>; bundle : TFhirMMElement; stack : TNodeStack); overload; virtual;
    function getCanonicalURLForEntry(entry : TFhirMMElement) : String; overload; virtual;
    function getIdForEntry(entry : TFhirMMElement) : String; overload; virtual;
    procedure validateResourceIds(errors : TFslList<TFhirValidationMessage>; entries : TFslList<TFhirMMElement>; stack : TNodeStack); overload; virtual;
    procedure checkAllInterlinked(errors : TFslList<TFhirValidationMessage>; entries : TFslList<TFhirMMElement>; stack : TNodeStack; bundle : TFhirMMElement); overload; virtual;
    procedure followResourceLinks(entry : TFhirMMElement; visitedResources : TFslMap<TFhirMMElement>; candidateEntries : TFhirElementMap; candidateResources : TFslList<TFhirMMElement>; errors : TFslList<TFhirValidationMessage>; stack : TNodeStack); overload; virtual;
    procedure followResourceLinks(entry : TFhirMMElement; visitedResources : TFslMap<TFhirMMElement>; candidateEntries : TFhirElementMap; candidateResources : TFslList<TFhirMMElement>; errors : TFslList<TFhirValidationMessage>; stack : TNodeStack; depth : integer); overload; virtual;
    function findReferences(start : TFhirMMElement) : TStringList; overload; virtual;
    procedure findReferences(start : TFhirMMElement; references : TStringList); overload; virtual;
    procedure validateBundleReference(errors : TFslList<TFhirValidationMessage>; entries : TFslList<TFhirMMElement>; ref : TFhirMMElement; name : String; stack : TNodeStack; fullUrl : String; type_ : String; id : String); overload; virtual;
    procedure validateContains(hostContext : TFhirValidatorHostContext; errors : TFslList<TFhirValidationMessage>; path : String; child : TFhirElementDefinition; context : TFhirElementDefinition; resource : TFhirMMElement; element : TFhirMMElement; stack : TNodeStack; idstatus : TFhirIdStatus); overload; virtual;
    procedure validateDocument(errors : TFslList<TFhirValidationMessage>; entries : TFslList<TFhirMMElement>; composition : TFhirMMElement; stack : TNodeStack; fullUrl : String; id : String); overload; virtual;
    procedure validateElement(hostContext : TFhirValidatorHostContext; errors : TFslList<TFhirValidationMessage>; profile : TFhirStructureDefinition; definition : TFhirElementDefinition; cprofile : TFhirStructureDefinition; context : TFhirElementDefinition; resource : TFhirMMElement; element : TFhirMMElement; actualType : String; stack : TNodeStack; inCodeableConcept : boolean); overload; virtual;
    function idStatusForEntry(ep : TFhirMMElement; ei : TElementInfo) : TFhirIdStatus; overload; virtual;
    procedure checkInvariants(hostContext : TFhirValidatorHostContext; errors : TFslList<TFhirValidationMessage>; path : String; profile : TFhirStructureDefinition; ed : TFhirElementDefinition; typename : String; typeProfile : String; resource : TFhirMMElement; element : TFhirMMElement); overload; virtual;
    procedure validateMessage(errors : TFslList<TFhirValidationMessage>; entries : TFslList<TFhirMMElement>; messageHeader : TFhirMMElement; stack : TNodeStack; fullUrl : String; id : String); overload; virtual;
    procedure validateObservation(errors : TFslList<TFhirValidationMessage>; element : TFhirMMElement; stack : TNodeStack); overload; virtual;
    procedure validateResource(hostContext : TFhirValidatorHostContext; errors : TFslList<TFhirValidationMessage>; resource : TFhirMMElement; element : TFhirMMElement; defn : TFhirStructureDefinition; profiles : TValidationProfileSet; idstatus : TFhirIdStatus; stack : TNodeStack; isEntry : boolean); overload; virtual;
    procedure loadProfiles(profiles : TValidationProfileSet); overload; virtual;
    function getFirstEntry(bundle : TFhirMMElement) : TFhirMMElement; overload; virtual;
    procedure validateSections(errors : TFslList<TFhirValidationMessage>; entries : TFslList<TFhirMMElement>; focus : TFhirMMElement; stack : TNodeStack; fullUrl : String; id : String); overload; virtual;
    function valueMatchesCriteria(value : TFhirMMElement; criteria : TFhirElementDefinition) : boolean; overload; virtual;
    function yearIsValid(v : String) : boolean; overload; virtual;
    function matchSlice(hostContext : TFhirValidatorHostContext; errors : TFslList<TFhirValidationMessage>; profile : TFhirStructureDefinition; stack : TNodeStack;
      slicer : TFHIRElementDefinition; unsupportedSlicing : boolean; problematicPaths : TStringList; sliceOffset, i : integer; ed : TFHIRElementDefinition;
      childUnsupportedSlicing : boolean; ei : TElementInfo) : boolean;
  protected
    function sizeInBytesV : cardinal; override;
  public
    function getContext() : TFHIRWorkerContext; virtual;
    Property Context : TFHIRWorkerContext read GetContext;
    Property ValContext : TFHIRWorkerContext read GetContext;

    function isNoExtensibleWarnings() : boolean; overload; virtual;
    function setNoExtensibleWarnings(noExtensibleWarnings : boolean) : TInstanceValidator; overload; virtual;
    function isNoInvariantChecks() : boolean; overload; virtual;
    function setNoInvariantChecks(value : boolean) : TInstanceValidator; overload; virtual;
    function getFetcher() : TFhirIValidatorResourceFetcher; overload; virtual;
    function setFetcher(value : TFhirIValidatorResourceFetcher) : TInstanceValidator; overload; virtual;
    function isHintAboutNonMustSupport() : boolean; overload; virtual;
    procedure setHintAboutNonMustSupport(hintAboutNonMustSupport : boolean); overload; virtual;
    function validate(appContext : TFhirObject; errors : TFslList<TFhirValidationMessage>; stream : TStream; format : TFhirFormat) : TFHIRMMElement; overload; virtual;
    function validate(appContext : TFhirObject; errors : TFslList<TFhirValidationMessage>; stream : TStream; format : TFhirFormat; profile : String) : TFHIRMMElement; overload; virtual;
    function getBestPracticeWarningLevel() : TFhirBestPracticeWarningLevel; overload; virtual;
    function getCheckDisplay() : TFhirCheckDisplayOption; overload; virtual;
    function getExtensionDomains() : TStringList; overload; virtual;
    function isAnyExtensionsAllowed() : boolean; overload; virtual;
    function isErrorForUnknownProfiles() : boolean; overload; virtual;
    procedure setErrorForUnknownProfiles(errorForUnknownProfiles : boolean); overload; virtual;
    function isSuppressLoincSnomedMessages() : boolean; overload; virtual;
    procedure setAnyExtensionsAllowed(anyExtensionsAllowed : boolean); overload; virtual;
    function setBestPracticeWarningLevel(value : TFhirBestPracticeWarningLevel) : TInstanceValidator; overload; virtual;
    procedure setCheckDisplay(checkDisplay : TFhirCheckDisplayOption); overload; virtual;
    procedure setSuppressLoincSnomedMessages(suppressLoincSnomedMessages : boolean); overload; virtual;
    function getResourceIdRule() : TFhirIdStatus; overload; virtual;
    procedure setResourceIdRule(resourceIdRule : TFhirIdStatus); overload; virtual;
    function isAllowXsiLocation() : boolean; overload; virtual;
    procedure setAllowXsiLocation(allowXsiLocation : boolean); overload; virtual;
    procedure checkInvariant(hostContext : TFhirValidatorHostContext; errors : TFslList<TFhirValidationMessage>; path : String; profile : TFhirStructureDefinition; resource : TFhirMMElement; element : TFhirMMElement; inv : TFhirElementDefinitionConstraint); overload; virtual;
    function reportTimes() : String; overload; virtual;
    function isNoBindingMsgSuppressed() : boolean; overload; virtual;
    function setNoBindingMsgSuppressed(noBindingMsgSuppressed : boolean) : TInstanceValidator; overload; virtual;
    function isNoTerminologyChecks() : boolean; overload; virtual;
    function setNoTerminologyChecks(noTerminologyChecks : boolean) : TInstanceValidator; overload; virtual;
    procedure checkAllInvariants(); overload; virtual;
  end;

implementation

function TInstanceValidator.allowUnknownExtension(url : String) : boolean;
var
  s : String;
begin
  if (url.contains('example.org')) or (url.contains('acme.com')) or (url.contains('nema.org')) or (url.startsWith('http://hl7.org/fhir/tools/StructureDefinition/')) or (url.equals('http://hl7.org/fhir/StructureDefinition/structuredefinition-expression')) then
exit(true);

  for s in extensionDomains do
  begin
    if (url.startsWith(s)) then
exit(true);

  end;
exit(anyExtensionsAllowed);
end;

function TInstanceValidator.isKnownExtension(url : String) : boolean;
var
  s : String;
begin
  if (url.contains('example.org')) or (url.contains('acme.com')) or (url.contains('nema.org')) or (url.startsWith('http://hl7.org/fhir/tools/StructureDefinition/')) or (url.equals('http://hl7.org/fhir/StructureDefinition/structuredefinition-expression')) or (url.equals(IG_DEPENDSON_PACKAGE_EXTENSION)) then
exit(true);

  for s in extensionDomains do
  begin
    if (url.startsWith(s)) then
exit(true);

  end;
exit(false);
end;

procedure TInstanceValidator.bpCheck(errors : TFslList<TFhirValidationMessage>; invalid : TFhirIssueType; line : integer; col : integer; literalPath : String; test : boolean; message : String);
begin
  if (bpWarnings <> bpNull) then
      case (bpWarnings) of
        bpError:            rule(errors, invalid, line, col, literalPath, test, message);
        bpWarning:            warning(errors, invalid, line, col, literalPath, test, message);
        bpHint:            hint(errors, invalid, line, col, literalPath, test, message);
        else // do nothing
      end;
end;

procedure TInstanceValidator.validateRemainder(appContext : TFhirObject; errors : TFslList<TFhirValidationMessage>);
var
  processedResource : boolean;
  keys : TFslList<TFHIRObject>;
  resource : TFHIRObject;
begin
  repeat
    processedResource := false;
    keys := TFslList<TFHIRObject>.create();
    try
//      keys.addAll(resourceProfilesMap.keySet());
      for resource in keys do
      begin
//          ResourceProfiles rp := resourceProfilesMap.get(resource);
//          if (rp.hasUncheckedProfiles()) then
//          begin
//              processedResource := true;
//              start(new ValidatorHostContext(appContext), errors, rp.getOwner(), resource, null, rp.getStack());
//          end;
      end;
    finally
      keys.free;
    end;
  until not (processedResource);
end;

procedure TInstanceValidator.checkElementUsage(errors : TFslList<TFhirValidationMessage>; element : TFhirMMElement; stack : TNodeStack);
var
  elementUsage : String;
  prevName : String;
  elementCount : integer;
  ce : TFhirMMElement;
begin
  elementUsage :=  element.Tags['elementSupported'];
   hint(errors, itINFORMATIONAL, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), (elementUsage = '') or elementUsage.equals('Y'), String.format('The element %s is not marked as ''mustSupport'' in the profile %s. Consider not using the element, or marking the element as must-Support in the profile', [element.Name, element.prop.structure.Url]));
  if (element.children <> nil) then
      prevName :=  '';
      elementCount :=  0;
      for ce in element.children do
      begin
        if (ce.Name.equals(prevName)) then
          inc(elementCount)
        else
        begin
              elementCount := 1;
              prevName := ce.Name;
        end;

           checkElementUsage(errors, ce, stack.push(ce, elementCount, nil, nil));
      end;

end;

function TInstanceValidator.check(v1 : String; v2 : String) : boolean;
begin
  if v1 = '' then
    result := v1 = ''
  else
    result := v1 = v2;
end;

procedure TInstanceValidator.checkAddress(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirAddress);
var
  lines : TFslList<TFhirMMElement>;
  i : integer;
begin
   checkFixedValue(errors, path + '.use', focus.getNamedChild('use'), fixed.UseElement, 'use', focus);
   checkFixedValue(errors, path + '.text', focus.getNamedChild('text'), fixed.TextElement, 'text', focus);
   checkFixedValue(errors, path + '.city', focus.getNamedChild('city'), fixed.CityElement, 'city', focus);
   checkFixedValue(errors, path + '.state', focus.getNamedChild('state'), fixed.StateElement, 'state', focus);
   checkFixedValue(errors, path + '.country', focus.getNamedChild('country'), fixed.CountryElement, 'country', focus);
   checkFixedValue(errors, path + '.zip', focus.getNamedChild('zip'), fixed.PostalCodeElement, 'postalCode', focus);
  lines := TFslList<TFhirMMElement>.Create();
   focus.getNamedChildren('line', lines);
  if (rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, lines.count = fixed.lineList.count, 'Expected ' + Integer.toString(fixed.LineList.count) + ' but found ' + Integer.toString(lines.count) + ' line elements')) then
  for i := 0 to lines.count - 1 do
    checkFixedValue(errors, path + '.coding', lines[i], fixed.lineList[i], 'coding', focus);
end;

procedure TInstanceValidator.checkAttachment(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirAttachment);
begin
   checkFixedValue(errors, path + '.contentType', focus.getNamedChild('contentType'), fixed.ContentTypeElement, 'contentType', focus);
   checkFixedValue(errors, path + '.language', focus.getNamedChild('language'), fixed.LanguageElement, 'language', focus);
   checkFixedValue(errors, path + '.data', focus.getNamedChild('data'), fixed.DataElement, 'data', focus);
   checkFixedValue(errors, path + '.url', focus.getNamedChild('url'), fixed.UrlElement, 'url', focus);
   checkFixedValue(errors, path + '.size', focus.getNamedChild('size'), fixed.SizeElement, 'size', focus);
   checkFixedValue(errors, path + '.hash', focus.getNamedChild('hash'), fixed.HashElement, 'hash', focus);
   checkFixedValue(errors, path + '.title', focus.getNamedChild('title'), fixed.TitleElement, 'title', focus);
end;

function TInstanceValidator.checkCode(errors : TFslList<TFhirValidationMessage>; element : TFhirMMElement; path : String; code : String; system : String; display : String) : boolean;
var
  t : cardinal;
  ss : boolean;
  s : TValidationResult;
  cs : TFhirCodeSystem;
  def : TFhirCodeSystemConcept;
begin
  t := getTickCount;
  ss :=  context.supportsSystem(system, '');
  txTime := txTime + (getTickCount - t);
  if (ss) then
  begin
    t := getTickCount;
    s :=  context.validateCode(system, '', code, display);
    try
      txTime := txTime + (getTickCount - t);
      if (s = nil) then
        exit(true);

      if (s.isOk()) then
        if (s.Message <> '') then
          warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, s = nil, s.Message);
            exit(true);

      if (s.Severity = isInformation) then
        hint(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, s = nil, s.Message)
      else if (s.Severity = isWarning) then
        warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, s = nil, s.Message)
      else
        exit(rule(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, s = nil, s.Message));
    finally
      s.Free;
    end;
    exit(true);
  end
  else if (system.startsWith('http://hl7.org/fhir')) then
  begin
    if (StringArrayExistsSensitive(['http://hl7.org/fhir/sid/icd-10', 'http://hl7.org/fhir/sid/cvx', 'http://hl7.org/fhir/sid/icd-10-cm', 'http://hl7.org/fhir/sid/icd-9', 'http://hl7.org/fhir/sid/ndc', 'http://hl7.org/fhir/sid/srt'], system)) then
      exit(true)
    else
    begin
      cs :=  getCodeSystem(system);
      if (rule(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, cs <> nil, 'Unknown Code System ' + system)) then
      begin
        def :=  getCodeDefinition(cs, code);
        if (warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, def <> nil, 'Unknown Code (' + system + '#' + code + ')')) then
          exit(warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, (display = '') or (display = def.display), 'Display should be ''' + def.display + ''''));
      end;
      exit(false);
    end
  end
  else if (startsWithButIsNot(system, ['http://snomed.info/sct', 'http://loinc.org', 'http://unitsofmeasure.org', 'http://www.nlm.nih.gov/research/umls/rxnorm'])) then
  begin
    rule(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'Invalid System URI: ' + system);
    exit(false);
  end
  else
  begin
    try
      if (context.fetchResource(frtValueSet, system) <> nil) then
         rule(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'Invalid System URI: ' + system + ' - cannot use a value set URI as a system');
    except
    end;
    exit(true);
  end;
end;

function TInstanceValidator.startsWithButIsNot(system : String; uris : array of String) : boolean;
var
  s : String;
begin
  for s in uris do
  begin
    if (system <> s) and system.startsWith(s) then
      exit(true);
  end;
  exit(false);
end;

procedure TInstanceValidator.checkCodeableConcept(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirCodeableConcept);
var
  codings : TFslList<TFhirMMElement>;
  i : integer;
begin
   checkFixedValue(errors, path + '.text', focus.getNamedChild('text'), fixed.TextElement, 'text', focus);
   codings := TFslList<TFhirMMElement>.Create();
   try
     focus.getNamedChildren('coding', codings);
    if (rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, codings.count = fixed.CodingList.count, 'Expected ' + Integer.toString(fixed.CodingList.count) + ' but found ' + Integer.toString(codings.count) + ' coding elements')) then
    for i := 0 to codings.count -1 do
      checkFixedValue(errors, path + '.coding', codings[i], fixed.CodingList[i], 'coding', focus);
   finally
     codings.free;
   end;
end;

function readAsCoding(item: TFHIRMMElement): TFHIRCoding;
var
  c: TFHIRCoding;
begin
  c := TFHIRCoding.Create;
  try
    c.System := item.getNamedChildValue('system');
    c.Version := item.getNamedChildValue('version');
    c.code := item.getNamedChildValue('code');
    c.display := item.getNamedChildValue('display');
    result := c.Link;
  finally
    c.Free;
  end;
end;

function readAsCodeableConcept(element: TFHIRMMElement): TFHIRCodeableConcept;
var
  cc: TFHIRCodeableConcept;
  list: TFslList<TFHIRMMElement>;
  item: TFHIRMMElement;
begin
  if element = nil then
    exit(nil);

  cc := TFHIRCodeableConcept.Create;
  list := TFslList<TFHIRMMElement>.Create;
  try
    element.getNamedChildren('coding', list);
    for item in list do
      cc.CodingList.Add(readAsCoding(item));
    cc.text := element.getNamedChildValue('text');
    result := cc.Link;
  finally
    cc.Free;
    list.Free;
  end;
End;


procedure TInstanceValidator.checkCodeableConcept(errors : TFslList<TFhirValidationMessage>; path : String; element : TFhirMMElement; profile : TFhirStructureDefinition; theElementCntext : TFhirElementDefinition);
var
  binding : TFhirElementDefinitionBinding;
  valueset : TFhirValueSet;
  cc : TFHIRCodeableConcept;
  t : cardinal;
  bindingsOk : boolean;
  atLeastOneSystemIsSupported : boolean;
  nextCoding : TFHIRCoding;
  nextSystem, nextCode : String;
  vr : TValidationResult;
begin
  if not noTerminologyChecks and (theElementCntext <> nil) and (theElementCntext.binding <> nil) then
  begin
    binding := theElementCntext.binding;
    if (warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, binding <> nil, 'Binding for ' + path + ' missing (cc)')) then
    begin
      if (binding.valueSet <> '') then
      begin
        valueset :=  resolveBindingReference(profile, binding.valueSet, profile.Url);
        if (warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, valueset <> nil, 'ValueSet ' + describeReference(binding.valueSet) + ' not found')) then
        begin
          try
            cc := readAsCodeableConcept(element);
            if (not cc.hasCoding()) then
            begin
              if (binding.strength = BindingStrengthREQUIRED) then
                rule(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'No code provided, and a code is required from the value set ' + describeReference(binding.valueSet) + ' (' + valueset.url)
              else if (binding.strength = BindingStrengthEXTENSIBLE) then
                warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'No code provided, and a code should be provided from the value set ' + describeReference(binding.valueSet) + ' (' + valueset.url);
            end
            else
            begin
              t := getTickCount;
              atLeastOneSystemIsSupported := false;
              // Check whether the codes are appropriate for the type of binding we have
               bindingsOk := true;
               if (binding.strength <> BindingStrengthEXAMPLE) then
               begin
                 for nextCoding in cc.CodingList do
                 begin
                    nextSystem := nextCoding.system;
                    if (nextSystem <> '') and context.supportsSystem(nextSystem, '') then
                    begin
                      atLeastOneSystemIsSupported := true;
                      break;
                    end;
                 end;
               end;
              if not atLeastOneSystemIsSupported and (binding.strength = BindingStrengthEXAMPLE) then
                // ignore this since we can''t validate but it doesn''t matter..
              else
              begin
                vr := context.validateCode(cc, valueset);
                try
                  if (not vr.isOk()) then
                  begin
                    bindingsOk := false;
                    if (binding.strength = BindingStrengthREQUIRED) then
                      warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'Could not confirm that the codes provided are in the value set ' + describeReference(binding.valueSet) + ' and a code from this value set is required')
                    else if (binding.strength = BindingStrengthEXTENSIBLE) then
                    begin
                      if (binding.hasExtension('http://hl7.org/fhir/StructureDefinition/elementdefinition-maxValueSet')) then
                        checkMaxValueSet(errors, path, element, profile, binding.getExtensionString('http://hl7.org/fhir/StructureDefinition/elementdefinition-maxValueSet'), cc)
                      else if (not noExtensibleWarnings) then
                        warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'Could not confirm that the codes provided are in the value set ' + describeReference(binding.valueSet) + ' and a code should come from this value set unless it has no suitable code');
                    end
                    else if (binding.strength = BindingStrengthPREFERRED) then
                      hint(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'Could not confirm that the codes provided are in the value set ' + describeReference(binding.valueSet) + ' and a code is recommended to come from this value set');
                  end;
                finally
                  vr.Free;
                end;
              end;
              // to validate, we'll validate that the codes actually exist
              if (bindingsOk) then
              begin
                  for nextCoding in cc.CodingList do
                  begin
                      nextCode := nextCoding.code;
                      nextSystem := nextCoding.system;
                      if (nextCode <> '') and (nextSystem <> '') and context.supportsSystem(nextSystem, '') then
                      begin
                        vr := context.validateCode(nextSystem, '', nextCode);
                        try
                          if (not vr.isOk()) then
                            warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'Code "'+nextCode+'" is not a valid code in code system "'+nextSystem+'"');
                        finally
                          vr.Free;
                        end;
                      end;
                  end;
              end;
              txTime := txTime + (getTickCount - t);
            end;
          except
            on e : Exception do
             warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'Error ' + e.Message + ' validating CodeableConcept');
          end;
        end
        else if (binding.valueSet <> '') then
          hint(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'Binding by URI reference cannot be checked')
        else if (not noBindingMsgSuppressed) then
          hint(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'Binding for path ' + path + ' has no source, so can''t be checked');
      end;
    end;
  end;
end;

procedure TInstanceValidator.checkMaxValueSet(errors : TFslList<TFhirValidationMessage>; path : String; element : TFhirMMElement; profile : TFhirStructureDefinition; maxVSUrl : String; cc : TFhirCodeableConcept);
var
  valueset : TFhirValueSet;
  t : cardinal;
  vr : TValidationResult;
begin
  valueset :=  resolveBindingReference(profile, maxVSUrl, profile.url);
  if (warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, valueset <> nil, 'ValueSet ' + describeReference(maxVSUrl) + ' not found')) then
  try
    t := getTickCount;
    vr := context.validateCode(cc, valueset);
    try
      txTime := txTime + (getTickCount - t);
      if (not vr.isOk()) then
      begin
        rule(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'None of the codes provided are in the maximum value set ' + describeReference(maxVSUrl) + ' (' + valueset.url + ', and a code from this value set is required) (codes := ' + ccSummary(cc) + ')');
      end;
    finally
      vr.Free;
    end;
  except
    on e : Exception do
      warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'Error ' + e.Message + ' validating CodeableConcept using maxValueSet');
  end;
end;

procedure TInstanceValidator.checkMaxValueSet(errors : TFslList<TFhirValidationMessage>; path : String; element : TFhirMMElement; profile : TFhirStructureDefinition; maxVSUrl : String; c : TFhirCoding);
var
  valueset : TFhirValueSet;
  t : cardinal;
  vr : TValidationResult;
begin
  valueset :=  resolveBindingReference(profile, maxVSUrl, profile.url);
  if (warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, valueset <> nil, 'ValueSet ' + describeReference(maxVSUrl) + ' not found')) then
  try
    t := getTickCount;
    vr := context.validateCode(c, valueset);
    try
      txTime := txTime + (getTickCount - t);
      if (not vr.isOk()) then
      begin
        rule(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'The code provided is not in the maximum value set ' + describeReference(maxVSUrl) + ' (' + valueset.url + ', and a code from this value set is required) (code := ' + c.system + '#' + c.code + ')');
      end;
    finally
      vr.Free;
    end;
  except
    on e : exception do
      warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'Error ' + e.Message + ' validating CodeableConcept using maxValueSet');
  end;
end;

function TInstanceValidator.ccSummary(cc : TFhirCodeableConcept) : String;
var
  b : TStringBuilder;
  c : TFhirCoding;
begin
  b := TStringBuilder.Create();
  try
    for c in cc.codingList do
    begin
       b.append(','+c.system + '#' + c.code);
    end;
    exit(b.toString().Substring(1));
  finally
    b.Free;
  end;
end;

procedure TInstanceValidator.checkCoding(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirCoding);
begin
   checkFixedValue(errors, path + '.system', focus.getNamedChild('system'), fixed.SystemElement, 'system', focus);
   checkFixedValue(errors, path + '.code', focus.getNamedChild('code'), fixed.CodeElement, 'code', focus);
   checkFixedValue(errors, path + '.display', focus.getNamedChild('display'), fixed.DisplayElement, 'display', focus);
   checkFixedValue(errors, path + '.userSelected', focus.getNamedChild('userSelected'), fixed.UserSelectedElement, 'userSelected', focus);
end;

procedure TInstanceValidator.checkCoding(errors : TFslList<TFhirValidationMessage>; path : String; element : TFhirMMElement; profile : TFhirStructureDefinition; theElementCntext : TFhirElementDefinition; inCodeableConcept : boolean);
var
  code : String;
  system : String;
  display : String;
  binding : TFHIRElementDefinitionBinding;
  valueset : TFHIRValueSet;
  c : TFHIRCoding;
  t : cardinal;
  vr : TValidationResult;
begin
  code :=  element.getNamedChildValue('code');
  system :=  element.getNamedChildValue('system');
  display :=  element.getNamedChildValue('display');
  rule(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, isAbsolute(system), 'Coding.system must be an absolute reference, not a local reference');
  if (system <> '') and (code <> '') and not noTerminologyChecks then
    rule(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, not isValueSet(system), 'The Coding references a value set, not a code system ("' + system + '")');
  try
    if (checkCode(errors, element, path, code, system, display)) then
      if (theElementCntext <> nil) and (theElementCntext.binding <> nil) then
      begin
        binding := theElementCntext.binding;
        if (warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, binding <> nil, 'Binding for ' + path + ' missing')) then
        begin
          if (binding.valueSet <> '') then
          begin
            valueset := resolveBindingReference(profile, binding.valueSet, profile.url);
            if (warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, valueset <> nil, 'ValueSet ' + describeReference(binding.valueSet) + ' not found')) then
            begin
              try
                 c := readAsCoding(element);
                 t := getTickCount;
                 vr := nil;
                 try
                   if (binding.strength <> BindingStrengthEXAMPLE) then
                     vr := context.validateCode(c, valueset);
                   txTime := txTime + (getTickCount - t);
                   if (vr <> nil) and (not vr.isOk()) then
                   begin
                     if (binding.strength = BindingStrengthREQUIRED) then
                       warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'Could not confirm that the codes provided are in the value set ' + describeReference(binding.valueSet) + ' (' + valueset.url + ', and a code from this value set is required)')
                     else if (binding.strength = BindingStrengthEXTENSIBLE) then
                     begin
                       if (binding.hasExtension('http://hl7.org/fhir/StructureDefinition/elementdefinition-maxValueSet')) then
                         checkMaxValueSet(errors, path, element, profile, binding.getExtensionString('http://hl7.org/fhir/StructureDefinition/elementdefinition-maxValueSet'), c)
                       else if (not noExtensibleWarnings) then
                         warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'Could not confirm that the codes provided are in the value set ' + describeReference(binding.valueSet) + ' (' + valueset.url + ', and a code should come from this value set unless it has no suitable code)');
                     end
                     else if (binding.strength = BindingStrengthPREFERRED) then
                       hint(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'Could not confirm that the codes provided are in the value set ' + describeReference(binding.valueSet) + ' (' + valueset.url + ', and a code is recommended to come from this value set)')
                   end;
                 finally
                   vr.Free;
                 end;
              except
                on e : Exception do
                  warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'Error ' + e.Message + ' validating Coding');
              end;
            end
            else if (binding.ValueSet <> '') then
              hint(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'Binding by URI reference cannot be checked')
            else if (not inCodeableConcept and not noBindingMsgSuppressed) then
              hint(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'Binding for path ' + path + ' has no source, so can''t be checked');
          end;
        end;
      end;
  except
    on e : Exception do
      rule(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'Error ' + e.Message + ' validating Coding');
  end;
end;

function TInstanceValidator.isValueSet(url : String) : boolean;
var
  vs : TFhirResource;
begin
  try
    vs := context.fetchResource(frtValueSet, url);
    try
      result := vs <> nil;
    finally
      vs.Free;
    end;
  except
    result := false;
  end;
end;

procedure TInstanceValidator.checkContactPoint(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirContactPoint);
begin
   checkFixedValue(errors, path + '.system', focus.getNamedChild('system'), fixed.SystemElement, 'system', focus);
   checkFixedValue(errors, path + '.value', focus.getNamedChild('value'), fixed.ValueElement, 'value', focus);
   checkFixedValue(errors, path + '.use', focus.getNamedChild('use'), fixed.UseElement, 'use', focus);
   checkFixedValue(errors, path + '.period', focus.getNamedChild('period'), fixed.Period, 'period', focus);
end;

procedure TInstanceValidator.checkDeclaredProfiles(resourceProfiles : TValidationProfileSet; errors : TFslList<TFhirValidationMessage>; resource : TFhirMMElement; element : TFhirMMElement; stack : TNodeStack);
var
  meta : TFhirMMElement;
  profiles : TFslList<TFhirMMElement>;
  i : integer;
  profile : TFhirMMElement;
  ref : String;
  p : String;
  t : cardinal;
begin
  meta :=  element.getNamedChild('meta');
  if (meta <> nil) then
  begin
    profiles := TFslList<TFhirMMElement>.Create();
    try
      meta.getNamedChildren('profile', profiles);
      i :=  0;
      for profile in profiles do
      begin
        ref := profile.primitiveValue;
        p :=  stack.addToLiteralPath(['meta', 'profile', ':' + Integer.toString(i)]);
        if (rule(errors, itINVALID, element.LocationStart.line, element.LocationStart.col, p, ref <> '', 'StructureDefinition reference invalid')) then
        begin
          // t := getTickCount;
          // todo              resourceProfiles.addProfile(errors, ref, errorForUnknownProfiles, p, element);
          inc(i);
        end;
      end;
    finally
      profiles.Free;
    end;
  end;
end;

function TInstanceValidator.checkExtension(hostContext : TFhirValidatorHostContext; errors : TFslList<TFhirValidationMessage>; path : String; resource : TFhirMMElement; element : TFhirMMElement; def : TFhirElementDefinition; profile : TFhirStructureDefinition; stack : TNodeStack) : TFhirStructureDefinition;
var
  url : String;
  isModifier : boolean;
  t : cardinal;
  ex : TFhirStructureDefinition;
  allowedTypes : TFslStringSet;
  actualType : String;
begin
  url :=  element.getNamedChildValue('url');
  isModifier :=  element.Name.equals('modifierExtension');
  t := getTickCount;
  ex := context.fetchResource(frtStructureDefinition, url) as TFhirStructureDefinition;
  try
    sdTime := sdTime + (getTickCount - t);
    if (ex = nil) then
    begin
      if (not url.startsWith('http://hl7.org/fhir/4.0/StructureDefinition/extension-')) then
        if (rule(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, path, allowUnknownExtension(url), 'The extension ' + url + ' is unknown, and not allowed here')) then
           hint(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, path, isKnownExtension(url), 'Unknown extension ' + url)
    end
    else
      if (def.isModifier) then
         rule(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, path + '[url = ''' + url + ''']', ex.snapshot.elementList[0].isModifier, 'Extension modifier mismatch: the extension element is labelled as a modifier, but the underlying extension is not')
      else
         rule(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, path + '[url = ''' + url + ''']', not ex.snapshot.elementList[0].isModifier, 'Extension modifier mismatch: the extension element is not labelled as a modifier, but the underlying extension is');
  finally
    ex.Free;
  end;

  checkExtensionContext(errors, element, ex, stack, ex.url);
  if (isModifier) then
    rule(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, path + '[url = ''' + url + ''']', ex.snapshot.elementList[0].isModifier, 'The Extension ''' + url + ''' must be used as a modifierExtension')
  else
    rule(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, path + '[url = ''' + url + ''']', not ex.snapshot.elementList[0].isModifier, 'The Extension ''' + url + ''' must not be used as an extension (it''s a modifierExtension)');

  allowedTypes :=  listExtensionTypes(ex);
  actualType :=  getExtensionType(element);
  if (actualType = '') then
    rule(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, path + '[url = ''' + url + ''']', allowedTypes.isEmpty(), 'The Extension ''' + url + ''' definition is for a simple extension, so it must contain a value, not extensions')
  else
    rule(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, path + '[url = ''' + url + ''']', allowedTypes.contains(actualType), 'The Extension ''' + url + ''' definition allows for the types ' + allowedTypes.toString() + ' but found type ' + actualType);

  validateElement(hostContext, errors, ex, ex.snapshot.elementList[0], nil, nil, resource, element, 'Extension', stack, false);
  exit(ex);
end;

function TInstanceValidator.getExtensionType(element : TFhirMMElement) : String;
var
  e : TFhirMMElement;
  tn : String;
  ltn : String;
begin
  for e in element.children do
  begin
    if (e.Name.startsWith('value')) then
    begin
      tn :=  e.Name.substring(5);
      ltn := uncapitalise(tn);
      if (isPrimitiveType(ltn)) then
        exit(ltn)
      else
        exit(tn);
    end;
  end;
  exit('');
end;

function TInstanceValidator.listExtensionTypes(ex : TFhirStructureDefinition) : TFslStringSet;
var
  vd : TFhirElementDefinition;
  ed : TFhirElementDefinition;
//  res : TFslStringSet;
  tr : TFhirElementDefinitionType;
begin
  vd := nil;
  for ed in ex.snapshot.elementList do
  begin
    if (ed.path.startsWith('Extension.value')) then
      vd := ed;
    break;
  end;
  result := TFslStringSet.Create();
  try
    if (vd <> nil) and ('0' <> vd.max) then
      for tr in vd.type_List do
        result.add(tr.code);
    result.Link;
  finally
    result.Free;
  end;
end;

function TInstanceValidator.checkExtensionContext(errors : TFslList<TFhirValidationMessage>; element : TFhirMMElement; definition : TFhirStructureDefinition; stack : TNodeStack; extensionParent : String) : boolean;
begin
  exit(true);
end;

procedure TInstanceValidator.checkFixedValue(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFHIRElement; propName : String; parent : TFhirMMElement);
var
  value : String;
  extensions : TFslList<TFhirMMElement>;
  e : TFhirExtension;
  ex : TFhirMMElement;
begin
  if ((fixed = nil) or (fixed.isEmpty()) and (focus = nil)) then
    // this is all good
  else if (fixed = nil) and (focus <> nil) then
    rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, false, 'Unexpected element ' + focus.Name)
  else if ((fixed <> nil) and not fixed.isEmpty()) and (focus = nil) then
    rule(errors, itVALUE, parent.LocationStart.line, parent.LocationStart.col, path, false, 'Missing element ''' + propName + '''')
  else
  begin
    value := focus.primitiveValue;
    if (fixed is TFHIRBoolean) then
      rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, check(TFHIRBoolean(fixed).StringValue, value), 'Value is ''' + value + ''' but must be ''' + TFHIRBoolean(fixed).stringValue + '''')
    else if (fixed is TFHIRInteger) then
      rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, check(TFHIRInteger(fixed).stringValue, value), 'Value is ''' + value + ''' but must be ''' + TFHIRInteger(fixed).stringValue + '''')
    else if (fixed is TFHIRDecimal) then
      rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, check(TFHIRDecimal(fixed).stringValue, value), 'Value is ''' + value + ''' but must be ''' + TFHIRDecimal(fixed).stringValue + '''')
    else if (fixed is TFHIRBase64Binary) then
      rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, check(TFHIRBase64Binary(fixed).stringValue, value), 'Value is ''' + value + ''' but must be ''' + TFHIRBase64Binary(fixed).stringValue + '''')
    else if (fixed is TFHIRInstant) then
      rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, check(TFHIRInstant(fixed).StringValue, value), 'Value is ''' + value + ''' but must be ''' + TFHIRInstant(fixed).stringValue + '''')
    else if (fixed is TFHIRString) then
      rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, check(TFHIRString(fixed).value, value), 'Value is ''' + value + ''' but must be ''' +TFHIRString(fixed).value + '''')
    else if (fixed is TFHIRUri) then
      rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, check(TFHIRUri(fixed).value, value), 'Value is ''' + value + ''' but must be ''' + TFHIRUri(fixed).value + '''')
    else if (fixed is TFHIRDate) then
      rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, check(TFHIRDate(fixed).value.toString(), value), 'Value is ''' + value + ''' but must be ''' + TFHIRDate(fixed).StringValue + '''')
    else if (fixed is TFHIRDateTime) then
      rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, check(TFHIRDateTime(fixed).value.toString(), value), 'Value is ''' + value + ''' but must be ''' + TFHIRDateTime(fixed).StringValue + '''')
    else if (fixed is TFHIROid) then
      rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, check(TFHIROid(fixed).value, value), 'Value is ''' + value + ''' but must be ''' + TFHIROid(fixed).value + '''')
    else if (fixed is TFHIRUuid) then
      rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, check(TFHIRUuid(fixed).value, value), 'Value is ''' + value + ''' but must be ''' + TFHIRUuid(fixed).value + '''')
    else if (fixed is TFHIRCode) then
      rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, check(TFHIRCode(fixed).value, value), 'Value is ''' + value + ''' but must be ''' + TFHIRCode(fixed).value + '''')
    else if (fixed is TFHIRId) then
      rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, check(TFHIRId(fixed).value, value), 'Value is ''' + value + ''' but must be ''' + TFHIRId(fixed).value + '''')
    else if (fixed is TFHIRQuantity) then
      checkQuantity(errors, path, focus, fixed as TFHIRQuantity)
    else if (fixed is TFHIRAddress) then
      checkAddress(errors, path, focus, fixed as TFHIRAddress)
    else if (fixed is TFHIRContactPoint) then
      checkContactPoint(errors, path, focus, fixed as TFHIRContactPoint)
    else if (fixed is TFHIRAttachment) then
      checkAttachment(errors, path, focus, fixed as TFHIRAttachment)
    else if (fixed is TFHIRIdentifier) then
      checkIdentifier(errors, path, focus, fixed as TFHIRIdentifier)
    else if (fixed is TFHIRCoding) then
      checkCoding(errors, path, focus, fixed as TFHIRCoding)
    else if (fixed is TFHIRHumanName) then
      checkHumanName(errors, path, focus, fixed as TFHIRHumanName)
    else if (fixed is TFHIRCodeableConcept) then
      checkCodeableConcept(errors, path, focus, fixed as TFHIRCodeableConcept)
    else if (fixed is TFHIRTiming) then
      checkTiming(errors, path, focus, fixed as TFHIRTiming)
    else if (fixed is TFHIRPeriod) then
      checkPeriod(errors, path, focus, fixed as TFHIRPeriod)
    else if (fixed is TFHIRRange) then
      checkRange(errors, path, focus, fixed as TFHIRRange)
    else if (fixed is TFHIRRatio) then
      checkRatio(errors, path, focus, fixed as TFHIRRatio)
    else if (fixed is TFHIRSampledData) then
      checkSampledData(errors, path, focus, fixed as TFHIRSampledData)
    else
      rule(errors, itEXCEPTION, focus.LocationStart.line, focus.LocationStart.col, path, false, 'Unhandled fixed value type ' + fixed.fhirType);

    extensions := TFslList<TFhirMMElement>.Create();
    try
      focus.getNamedChildren('extension', extensions);
      if (fixed.ExtensionList.count = 0) then
        rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, extensions.count = 0, 'No extensions allowed, as the specified fixed value doesn''t contain any extensions')
      else
      begin
        if (rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, extensions.count = fixed.extensionList.count, 'Extensions count mismatch: expected ' + Integer.toString(fixed.extensionList.count) + ' but found ' + Integer.toString(extensions.count))) then
          for e in fixed.extensionList do
          begin
              ex :=  getExtensionByUrl(extensions, e.url);
              if (rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, ex <> nil, 'Extension count mismatch: unable to find extension: ' + e.url)) then
                   checkFixedValue(errors, path, ex.getNamedChild('extension').getNamedChild('value'), e.value, 'extension.value', ex.getNamedChild('extension'));
          end;
      end;
    finally
      extensions.Free;
    end;
  end;
end;

procedure TInstanceValidator.checkHumanName(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirHumanName);
var
  parts : TFslList<TFhirMMElement>;
  i : integer;
begin
  checkFixedValue(errors, path + '.use', focus.getNamedChild('use'), fixed.UseElement, 'use', focus);
  checkFixedValue(errors, path + '.text', focus.getNamedChild('text'), fixed.TextElement, 'text', focus);
  checkFixedValue(errors, path + '.period', focus.getNamedChild('period'), fixed.Period, 'period', focus);
  parts := TFslList<TFhirMMElement>.Create();
  try
    focus.getNamedChildren('family', parts);
    if (rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, (parts.count = 1) = (fixed.family <> ''), 'Expected ? but found ' + Integer.toString(parts.count) + ' family elements')) then
      for i := 0 to parts.count - 1 do
       checkFixedValue(errors, path + '.family', parts[i], fixed.FamilyElement, 'family', focus);

    focus.getNamedChildren('given', parts);
    if (rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, parts.count = fixed.givenList.count, 'Expected ' + Integer.toString(fixed.givenList.count) + ' but found ' + Integer.toString(parts.count) + ' given elements')) then
      for i := 0 to parts.count - 1 do
        checkFixedValue(errors, path + '.given', parts[i], fixed.givenList[i], 'given', focus);

    focus.getNamedChildren('prefix', parts);
    if (rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, parts.count = fixed.prefixList.count, 'Expected ' + Integer.toString(fixed.prefixList.count) + ' but found ' + Integer.toString(parts.count) + ' prefix elements')) then
      for i := 0 to parts.count - 1 do
        checkFixedValue(errors, path + '.prefix', parts[i], fixed.prefixList[i], 'prefix', focus);

    focus.getNamedChildren('suffix', parts);
    if (rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, parts.count = fixed.suffixList.count, 'Expected ' + Integer.toString(fixed.suffixList.count) + ' but found ' + Integer.toString(parts.count) + ' suffix elements')) then
      for i := 0 to parts.count - 1 do
        checkFixedValue(errors, path + '.suffix', parts[i], fixed.suffixList[i], 'suffix', focus);
  finally
    parts.free;
  end;
end;

procedure TInstanceValidator.checkIdentifier(errors : TFslList<TFhirValidationMessage>; path : String; element : TFhirMMElement; context : TFhirElementDefinition);
var
  system : String;
begin
  system :=  element.getNamedChildValue('system');
   rule(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, isAbsolute(system), 'Identifier.system must be an absolute reference, not a local reference');
end;

procedure TInstanceValidator.checkIdentifier(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirIdentifier);
begin
   checkFixedValue(errors, path + '.use', focus.getNamedChild('use'), fixed.UseElement, 'use', focus);
   checkFixedValue(errors, path + '.type', focus.getNamedChild('type'), fixed.type_Element, 'type', focus);
   checkFixedValue(errors, path + '.system', focus.getNamedChild('system'), fixed.SystemElement, 'system', focus);
   checkFixedValue(errors, path + '.value', focus.getNamedChild('value'), fixed.ValueElement, 'value', focus);
   checkFixedValue(errors, path + '.period', focus.getNamedChild('period'), fixed.Period, 'period', focus);
   checkFixedValue(errors, path + '.assigner', focus.getNamedChild('assigner'), fixed.Assigner, 'assigner', focus);
end;

procedure TInstanceValidator.checkPeriod(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirPeriod);
begin
   checkFixedValue(errors, path + '.start', focus.getNamedChild('start'), fixed.StartElement, 'start', focus);
   checkFixedValue(errors, path + '.end', focus.getNamedChild('end'), fixed.End_Element, 'end', focus);
end;

const
  EXT_REGEX = 'http://hl7.org/fhir/StructureDefinition/regex';

function matches(s : String; r : String) : boolean;
var
  regex : TRegex;
begin
  regex := TRegEx.Create(r, [roCompiled]);
  result := regex.IsMatch(s);
end;

procedure TInstanceValidator.checkPrimitive(appContext : TFslObject; errors : TFslList<TFhirValidationMessage>; path : String; type_ : String; context : TFhirElementDefinition; e : TFhirMMElement; profile : TFhirStructureDefinition);
var
  regex : String;
  encoded : String;
  charCount : integer;
  value : String;
  v : integer;
  head : String;
  i : integer;
  xhtml : TFhirXhtmlNode;
  ns : String;
  bytes : TBytes;
begin
  if (e.primitiveValue = '') then
  begin
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, e.hasChildren(), 'primitive types must have a value or must have child extensions');
    exit();
   end;

  regex :=  context.getExtensionString(EXT_REGEX);
  if (regex <> '') then
  begin
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, matches(e.primitiveValue, regex), 'Element value ''' + e.primitiveValue + ''' does not meet regex ''' + regex + '''');
  end;

  if (type_= 'boolean') then
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, ('true' = e.primitiveValue) or ('false' = e.primitiveValue), 'boolean values must be ''true'' or ''false''');

  if (type_= 'uri') or (type_= 'oid') or (type_= 'uuid') or (type_= 'url') or (type_= 'canonical') then
  begin
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, not e.primitiveValue.startsWith('oid:'), 'URI values cannot start with oid:');
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, not e.primitiveValue.startsWith('uuid:'), 'URI values cannot start with uuid:');
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, e.primitiveValue.equals(e.primitiveValue.trim().replace(' ', '')), 'URI values cannot have whitespace');
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, (context.maxLength = '') or (context.MaxLength = '0') or (e.primitiveValue.length <= StrToInt(context.MaxLength)), 'value is longer than permitted maximum length of ' + context.MaxLength);
    if (type_= 'oid') then
      if (rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, e.primitiveValue.startsWith('urn:oid:'), 'OIDs must start with urn:oid:')) then
        rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, isOid(e.primitiveValue.substring(8)), 'OIDs must be valid');
    if (type_= 'uuid') then
      if rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, e.primitiveValue.startsWith('urn:uuid:'), 'UUIDs must start with urn:uuid:') then
        rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, isUuid(e.primitiveValue.substring(8)), 'UUIDs must be valid');

// todo     if (fetcher <> nil) then
//           rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, fetcher.resolveURL(appContext, path, e.primitiveValue), 'URL value ''' + e.primitiveValue + ''' does not resolve');
  end;

  if (type_= 'id') then
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, isId(e.primitiveValue), 'id value ''' + e.primitiveValue + ''' is not valid');

  if (type_ = 'string') and e.hasPrimitiveValue() then
  begin
    if (rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, (e.primitiveValue = '') or (e.primitiveValue.length > 0), '@value cannot be empty')) then
      warning(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, (e.primitiveValue = '') or (e.primitiveValue.trim().equals(e.primitiveValue)), 'value should not start or finish with whitespace');
    if (rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, e.primitiveValue.length <= 1048576, 'value is longer than permitted maximum length of 1 MB (1048576 bytes)')) then
      rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, (context.maxLength = '') or (context.MaxLength = '0') or (e.primitiveValue.length <= StrToInt(context.MaxLength)), 'value is longer than permitted maximum length of ' + context.MaxLength);
  end;

  if (type_= 'dateTime') then
  begin
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, yearIsValid(e.primitiveValue), 'The value ''' + e.primitiveValue + ''' does not have a valid year');
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, matches(e.primitiveValue, '([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)(-(0[1-9]|1[0-2])(-(0[1-9]|[1-2][0-9]|3[0-1])(T([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\\.[0-9]+)?(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?)?)?)?'), 'Not a valid date time');
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, not hasTime(e.primitiveValue) or (hasTimeZone(e.primitiveValue)), 'if a date has a time, it must have a timezone');
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, (context.maxLength = '') or (context.MaxLength = '0') or (e.primitiveValue.length <= StrToInt(context.MaxLength)), 'value is longer than permitted maximum length of ' + context.MaxLength);
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, TFslDateTime.isValidXmlDate(e.primitiveValue), 'Not a valid date/time');
  end;

  if (type_= 'time') then
  begin
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, matches(e.primitiveValue, '([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)'), 'Not a valid time');
  end;

  if (type_= 'date') then
  begin
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, yearIsValid(e.primitiveValue), 'The value ''' + e.primitiveValue + ''' does not have a valid year');
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, matches(e.primitiveValue, '([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)(-(0[1-9]|1[0-2])(-(0[1-9]|[1-2][0-9]|3[0-1]))?)?'), 'Not a valid date');
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, (context.maxLength = '') or (context.MaxLength = '0') or (e.primitiveValue.length <= StrToInt(context.MaxLength)), 'value is longer than permitted maximum length of ' + context.MaxLength);
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, TFslDateTime.isValidXmlDate(e.primitiveValue), 'Not a valid date/time');
  end;

  if (type_= 'base64Binary') then
  begin
    try
      bytes := DecodeBase64(e.primitiveValue);
    except
      on ex : Exception do
        rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, false, 'The base64 value is not valid ('+ex.Message+')');
    end;
  end;

  if (type_= 'integer') or (type_= 'unsignedInt') or (type_= 'positiveInt') then
  begin
    if (rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, StringIsInteger32(e.primitiveValue), 'The value ''' + e.primitiveValue + ''' is not a valid integer')) then
    begin
      v := StrToInt(e.primitiveValue);
      rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, (context.maxValue = nil) or not (context.maxValue is TFhirInteger) or (StrToInt(TFhirInteger(context.maxValue).value) <= v), 'value is greater than permitted maximum value of ' + TFhirInteger(context.maxValue).StringValue);
      rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, (context.minValue = nil) or not (context.minValue is TFhirInteger) or (StrToInt(TFhirInteger(context.minValue).value) >= v), 'value is less than permitted maximum value of ' + TFhirInteger(context.maxValue).StringValue);
      if (type_= 'unsignedInt') then
         rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, v >= 0, 'value is less than permitted minimum value of 0');
      if (type_= 'positiveInt') then
         rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, v > 0, 'value is less than permitted minimum value of 1');
    end;
  end;

  if (type_= 'decimal') then
  begin
    if (e.primitiveValue <> '') then
      rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, StringIsDecimal(e.primitiveValue), 'The value ''' + e.primitiveValue + ''' is not a valid decimal');
    if (e.primitiveValue.contains('.')) then
    begin
      head :=  e.primitiveValue.substring(0, e.primitiveValue.indexOf('.'));
      if (head.startsWith('-')) then
        head := head.substring(1);
      i :=  0;
      while (head.startsWith('0')) do
      begin
        inc(i);
        head := head.substring(1);
      end;
      rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, i <= 1, 'The value ''' + e.primitiveValue + ''' is not a valid decimal (leading 0s)');
    end;
  end;

  if (type_= 'instant') then
  begin
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, matches(e.primitiveValue, '-?[0-9]{4}-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1])T([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\\.[0-9]+)?(Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))'), 'The instant ''' + e.primitiveValue + ''' is not valid (by regex)');
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, yearIsValid(e.primitiveValue), 'The value ''' + e.primitiveValue + ''' does not have a valid year');
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, TFslDateTime.isValidXmlDate(e.primitiveValue), 'Not a valid instant');
  end;

  if (type_= 'code') and (e.primitiveValue <> '') then
  begin
    // Technically, a code is restricted to string which has at least one character and no leading or trailing whitespace, and where there is no whitespace
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, passesCodeWhitespaceRules(e.primitiveValue), 'The code ''' + e.primitiveValue + ''' is not valid (whitespace rules)');
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, (context.maxLength = '') or (context.MaxLength = '0') or (e.primitiveValue.length <= StrToInt(context.MaxLength)), 'value is longer than permitted maximum length of ' + context.MaxLength);
  end;

  if (context.binding <> nil) and (e.primitiveValue <> '') then
    checkPrimitiveBinding(errors, path, type_, context, e, profile);

  if (type_= 'xhtml') then
  begin
    xhtml := e.xhtml;
    if (xhtml <> nil) then
    begin
      ns := xhtml.nsDecl;
      rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, XHTML_NS = ns, 'Wrong namespace on the XHTML (''' + ns + ''')');
      checkInnerNS(errors, e, path, xhtml.ChildNodes);
      rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, 'div' = xhtml.Name, 'Wrong name on the XHTML (''' + ns + ''') - must start with div');
      checkInnerNames(errors, e, path, xhtml.ChildNodes);
    end;
  end;

  // if it is nil, this is an error already noted in the parsers
  if (context.fixed <> nil) then
    checkFixedValue(errors, path, e, context.Fixed, context.SliceName, nil);
end;

procedure TInstanceValidator.checkInnerNames(errors : TFslList<TFhirValidationMessage>; e : TFhirMMElement; path : String; list : TFhirXhtmlNodeList);
var
  node : TFhirXhtmlNode;
  an : TFHIRAttribute;
  ok : boolean;
begin
  for node in list do
  begin
    if (node.NodeType = fhntElement) then
           rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, StringArrayExistsSensitive(['p', 'br', 'div', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'a', 'span', 'b', 'em', 'i', 'strong', 'small', 'big', 'tt', 'small', 'dfn', 'q', 'var', 'abbr', 'acronym', 'cite', 'blockquote', 'hr', 'address', 'bdo', 'kbd', 'q', 'sub', 'sup', 'ul', 'ol', 'li', 'dl', 'dt', 'dd', 'pre', 'table', 'caption', 'colgroup', 'col', 'thead', 'tr', 'tfoot', 'tbody', 'th', 'td', 'code', 'samp', 'img', 'map', 'area'], node.Name), 'Illegal element name in the XHTML (''' + node.Name + ''')');
    for an in node.Attributes do
    begin
      ok := an.Name.startsWith('xmlns') or (StringArrayExistsSensitive(['title', 'style', 'class', 'id', 'lang', 'xml:lang', 'dir', 'accesskey', 'tabindex', // tables
          'span', 'width', 'align', 'valign', 'char', 'charoff', 'abbr', 'axis', 'headers', 'scope', 'rowspan', 'colspan'], an.name))
         or (StringArrayExistsSensitive(['a.href', 'a.name', 'img.src', 'img.border', 'div.xmlns', 'blockquote.cite', 'q.cite', 'a.charset', 'a.type_', 'a.name', 'a.href', 'a.hreflang', 'a.rel', 'a.rev', 'a.shape', 'a.coords', 'img.src', 'img.alt', 'img.longdesc', 'img.height', 'img.width', 'img.usemap', 'img.ismap', 'map.name', 'area.shape', 'area.coords', 'area.href', 'area.nohref', 'area.alt', 'table.summary', 'table.width', 'table.border', 'table.frame', 'table.rules', 'table.cellspacing', 'table.cellpadding', 'pre.space', 'td.nowrap'],
           node.Name + '.' + an.Name));
      if (not ok) then
        rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, false, 'Illegal attribute name in the XHTML (''' + an.Name + ''' on ''' + node.Name + ''')');
    end;
    checkInnerNames(errors, e, path, node.ChildNodes);
  end;
end;

procedure TInstanceValidator.checkInnerNS(errors : TFslList<TFhirValidationMessage>; e : TFhirMMElement; path : String; list : TFhirXhtmlNodeList);
var
  node : TFhirXhtmlNode;
  ns : String;
begin
  for node in list do
  begin
    if (node.NodeType = fhntElement) then
      ns := node.nsDecl
    else
      ns := '';
    rule(errors, itINVALID, e.LocationStart.line, e.LocationStart.col, path, (ns = '') or (XHTML_NS = ns), 'Wrong namespace on the XHTML (''' + ns + ''')');
    checkInnerNS(errors, e, path, node.ChildNodes);
  end;
end;

procedure TInstanceValidator.checkPrimitiveBinding(errors : TFslList<TFhirValidationMessage>; path : String; type_ : String; elementContext : TFhirElementDefinition; element : TFhirMMElement; profile : TFhirStructureDefinition);
var
  value : String;
  binding : TFhirElementDefinitionBinding;
  vs : TFhirValueSet;
  t : cardinal;
  vr : TValidationResult;
begin
  if (not element.hasPrimitiveValue()) or not (('code' = type_) or ('string' = type_) or ('uri' = type_) or ('url' = type_) or ('canonical' = type_)) then
    exit();

  if (noTerminologyChecks) then
    exit();

  value := element.primitiveValue;
  binding := elementContext.binding;
  if (binding.valueSet <> '') then
  begin
    vs := resolveBindingReference(profile, binding.valueSet, profile.url);
    if (warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, vs <> nil, 'ValueSet '''+describeReference(binding.valueSet)+''' not found')) then
    begin
      t := getTickCount;
      vr := nil;
      try
        if (binding.strength <> BindingStrengthEXAMPLE) then
          vr := context.validateCode(SYSTEM_NOT_APPLICABLE, '', value, vs);
        txTime := txTime + (getTickCount - t);
        if (vr <> nil) and not vr.isOk() then
        begin
          if (binding.strength = BindingStrengthREQUIRED) then
            rule(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'The value provided (''' + value + ''') is not in the value set ' + describeReference(binding.valueSet) + ', and a code is required from this value set) (error message = ' + vr.Message + ')')
          else if (binding.strength = BindingStrengthEXTENSIBLE) then
          begin
            if (binding.hasExtension('http://hl7.org/fhir/StructureDefinition/elementdefinition-maxValueSet')) then
              checkMaxValueSet(errors, path, element, profile, binding.getExtensionString('http://hl7.org/fhir/StructureDefinition/elementdefinition-maxValueSet'), value);
            if (not noExtensibleWarnings) then
              warning(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'The value provided (''' + value + ''') is not in the value set ' + describeReference(binding.valueSet) + ', and a code should come from this value set unless it has no suitable code) (error message := ' + vr.Message + ')' )
            else if (binding.strength = BindingStrengthPREFERRED) then
              hint(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, false, 'The value provided (''' + value + ''') is not in the value set ' + describeReference(binding.valueSet) + ', and a code is recommended to come from this value set) (error message := ''' + vr.Message + ''')');
          end;
        end;
      finally
        vr.Free;
      end;
    end
  end
  else if (not noBindingMsgSuppressed) then
    hint(errors, itCODEINVALID, element.LocationStart.line, element.LocationStart.col, path, type_ <> 'code', 'Binding has no source, so can''t be checked');
end;

procedure TInstanceValidator.checkQuantity(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirQuantity);
begin
   checkFixedValue(errors, path + '.value', focus.getNamedChild('value'), fixed.ValueElement, 'value', focus);
   checkFixedValue(errors, path + '.comparator', focus.getNamedChild('comparator'), fixed.ComparatorElement, 'comparator', focus);
   checkFixedValue(errors, path + '.units', focus.getNamedChild('unit'), fixed.Unit_Element, 'units', focus);
   checkFixedValue(errors, path + '.system', focus.getNamedChild('system'), fixed.SystemElement, 'system', focus);
   checkFixedValue(errors, path + '.code', focus.getNamedChild('code'), fixed.CodeElement, 'code', focus);
end;

procedure TInstanceValidator.checkRange(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirRange);
begin
   checkFixedValue(errors, path + '.low', focus.getNamedChild('low'), fixed.Low, 'low', focus);
   checkFixedValue(errors, path + '.high', focus.getNamedChild('high'), fixed.High, 'high', focus);
end;

procedure TInstanceValidator.checkRatio(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirRatio);
begin
   checkFixedValue(errors, path + '.numerator', focus.getNamedChild('numerator'), fixed.Numerator, 'numerator', focus);
   checkFixedValue(errors, path + '.denominator', focus.getNamedChild('denominator'), fixed.Denominator, 'denominator', focus);
end;

function readAsIdentifier(item: TFHIRMMElement): TFHIRIdentifier;
var
  c: TFHIRIdentifier;
begin
  if item = nil then
    exit(nil);

  c := TFHIRIdentifier.Create;
  try
    c.system := item.getNamedChildValue('system');
    c.value := item.getNamedChildValue('value');
    result := c.Link;
  finally
    c.Free;
  end;

end;

function readAsReference(item: TFHIRMMElement): TFHIRReference;
var
  c: TFHIRReference;
begin
  c := TFHIRReference.Create;
  try
    c.reference := item.getNamedChildValue('reference');
    c.display := item.getNamedChildValue('display');
    c.identifier := readAsIdentifier(item.getNamedChild('identifier'));
    c.type_ := item.getNamedChildValue('type');
    result := c.Link;
  finally
    c.Free;
  end;
end;

procedure TInstanceValidator.checkReference(hostContext : TFhirValidatorHostContext; errors : TFslList<TFhirValidationMessage>; path : String; element : TFhirMMElement; profile : TFhirStructureDefinition; container : TFhirElementDefinition; parentType : String; stack : TNodeStack);
var
  reference : TFhirReference;
  ref : String;
  we : TFhirMMElement;
  refType : String;
  pol : TFhirReferenceValidationPolicy;
  ft : String;
  tu : String;
  containerType : TFhirElementDefinitionType;
  matchingResource : boolean;
  target : TFhirCanonical;
  sd : TFhirStructureDefinition;
  ok : boolean;
  b : TStringBuilder;
  type_ : TFhirElementDefinitionType;
  u : TFhirUri;
  pr : String;
  bt : String;
  modeOk : boolean;
  mode : TFhirResourceAggregationModeEnum;
  missingRef : boolean;
begin
  reference := readAsReference(element);
  try
    ref := reference.reference;
    if (ref = '') then
    begin
      if (reference.identifier = nil) or (reference.identifier.system = '') or (reference.Identifier.value = '') then
        warning(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, path, element.getNamedChildValue('display') <> '', 'A Reference without an actual reference or identifier should have a display');
      exit();
    end;

    we := localResolve(ref, stack, errors, path);
    if (ref.startsWith('#')) then
      refType := 'contained'
    else if (we = nil) then
      refType := 'remote'
    else
      refType := 'bundle';
    if (refType= 'contained') or (refType= 'bundle') then
      pol := rvpCHECK_VALID
    else if fetcher = nil then
      pol := rvpIGNORE
    else
      pol := fetcher.validationPolicy(nil {hostContext.appContext}, path, ref);

    if (pol in TFhirReferenceValidationPolicyCheckExists) then
    begin
      if (we = nil) then
      begin
        if (fetcher = nil) and (refType <> 'contained') then
          raise EFHIRException.create('Resource resolution services not provided')
        else
        begin
          we := fetcher.fetch(nil {hostContext.appContext}, ref);
        end;
      end;
      rule(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, path, we <> nil, 'Unable to resolve resource ''' + ref + '''');
    end;
    if (we <> nil) then
      ft := we.fhirType
    else
      ft := tryParse(ref);

    if (reference.type_ <> '') then
    begin
      if isAbsolute(reference.type_) then
        tu := reference.type_
      else
        tu := 'http://hl7.org/fhir/StructureDefinition/' + reference.type_;
      containerType := container.getType('Reference');
      if (not containerType.hasTargetProfile(tu) and not containerType.hasTargetProfile('http://hl7.org/fhir/StructureDefinition/Resource')) then
      begin
        matchingResource := false;
        for target in containerType.targetProfileList do
        begin
          sd := context.fetchResource(frtStructureDefinition, target.stringValue) as TFhirStructureDefinition;
          if (('http://hl7.org/fhir/StructureDefinition/' + sd.type_).equals(tu)) then
          begin
            matchingResource := true;
            break;
          end;
        end;
        rule(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, path, matchingResource, 'The type_ ''' + reference.type_ + ''' is not a valid Target for this element (must be one of ' + container.getType('Reference').targetProfileAsCSV+ ')');
      end;
      rule(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, path, (ft = '') or (ft = reference.fhirType), 'The specified type_ ''' + reference.fhirType + ''' does not match the found type_ ''' + ft + '''');
    end;

    if (we <> nil) and (pol in TFhirReferenceValidationPolicyCheckType) then
    begin
      if (warning(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, path, ft <> '', 'Unable to determine type_ of target resource')) then
      begin
        ok := false;
        b := TStringBuilder.Create();
        for type_ in container.type_List do
        begin
          if (not ok) and (type_.code = 'Reference') then
          begin
            if (type_.targetProfileList.Count = 0) or (type_.hasTargetProfile('http://hl7.org/fhir/StructureDefinition/Resource')) then
              ok := true
            else
            begin
              for u in type_.targetProfileList do
              begin
                pr := u.value;
                bt :=  getBaseType(profile, pr);
                sd :=  context.fetchResource(frtStructureDefinition, 'http://hl7.org/fhir/StructureDefinition/' + bt) as TFhirStructureDefinition;
                if (rule(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, path, bt <> '', 'Unable to resolve the profile reference ''' + pr + '''')) then
                begin
                  b.append(bt);
                  ok := bt.equals(ft);
                  if (ok) and (we <> nil) and (pol in TFhirReferenceValidationPolicyCheckValid) then
                    doResourceProfile(hostContext, we, pr, errors, stack.push(we, -1, nil, nil), path, element);
                end
                else
                  ok := true;

                if ok and (type_.aggregationList.Count > 0) then
                begin
                  modeOk := false;
                  for mode in type_.aggregation do
                  begin
                    if (mode = ResourceAggregationModeContained) and (refType= 'contained') then
                      modeOk := true
                    else if (mode = ResourceAggregationModeBundled) and (refType= 'bundled') then
                      modeOk := true
                    else if (mode = ResourceAggregationModeReferenced) and ((refType= 'bundled') or (refType= 'remote')) then
                      modeOk := true;
                  end;
                  rule(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, path, ok, 'Reference is ' + refType + ' which isn''t supported by the specified aggregation mode(s) for the reference');
                end;
                if (ok) then
                  break;
              end;
              if (not ok) and (type_.code = '*') then
                ok := true;
            end;
          end;
          rule(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, path, ok, 'Invalid Resource target type_. Found ' + ft + ', but expected one of (' + b.toString() + ')');
        end;
      end;
    end;

    if (we = nil) then
    begin
      missingRef := false;
      for type_ in container.type_List do
      begin
        if not missingRef and (type_.code = 'Reference') then
        begin
          if (type_.aggregation <> []) then
            for mode in type_.aggregation do
            begin
              if (mode in [ResourceAggregationModeContained, ResourceAggregationModeBundled]) then
                missingRef := true;
              break;
            end;
        end;
      end;
      rule(errors, itREQUIRED, -1, -1, path, not missingRef, 'Bundled or contained reference not found within the bundle/resource ' + ref);
    end;

    if (pol in TFhirReferenceValidationPolicyCheckValid) then
    begin
    // todo....
    end;

  finally
    reference.free;
  end;
end;

procedure TInstanceValidator.doResourceProfile(hostContext : TFhirValidatorHostContext; resource : TFhirMMElement; profile : String; errors : TFslList<TFhirValidationMessage>; stack : TNodeStack; path : String; element : TFhirMMElement);
//var
//  resourceProfiles : TValidationProfileSet;
begin
//  resourceProfiles :=  addResourceProfile(errors, resource, profile, path, element, stack);
//  if (resourceProfiles.isProcessed()) then
//    start(hostContext, errors, resource, resource, nil, stack);
end;

function TInstanceValidator.getResourceProfiles(resource : TFhirMMElement; stack : TNodeStack) : TValidationProfileSet;
var
  resourceProfiles : TValidationProfileSet;
begin
  resourceProfiles := nil;// resourceProfilesMap.get(resource);
//  if (resourceProfiles = nil) then
//      resourceProfiles := new ResourceProfiles(resource, stack);
//       resourceProfilesMap.put(resource, resourceProfiles);
//
  exit(resourceProfiles);
end;

function TInstanceValidator.addResourceProfile(errors : TFslList<TFhirValidationMessage>; resource : TFhirMMElement; profile : String; path : String; element : TFhirMMElement; stack : TNodeStack) : TValidationProfileSet;
var
  resourceProfiles : TValidationProfileSet;
begin
 resourceProfiles := nil; // getResourceProfiles(resource, stack);
//  resourceProfiles.addProfile(errors, profile, errorForUnknownProfiles, path, element);
  exit(resourceProfiles);
end;

function TInstanceValidator.checkResourceType(type_ : String) : String;
var
  t : cardinal;
begin
  t := getTickCount;
  try
    if (context.fetchResource(frtStructureDefinition, 'http://hl7.org/fhir/StructureDefinition/' + type_) <> nil) then
      exit(type_)
    else
      exit('');
  finally
    sdTime := sdTime + (getTickCount - t);
  end;
end;

procedure TInstanceValidator.checkSampledData(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirSampledData);
begin
   checkFixedValue(errors, path + '.origin', focus.getNamedChild('origin'), fixed.Origin, 'origin', focus);
   checkFixedValue(errors, path + '.period', focus.getNamedChild('period'), fixed.PeriodElement, 'period', focus);
   checkFixedValue(errors, path + '.factor', focus.getNamedChild('factor'), fixed.FactorElement, 'factor', focus);
   checkFixedValue(errors, path + '.lowerLimit', focus.getNamedChild('lowerLimit'), fixed.LowerLimitElement, 'lowerLimit', focus);
   checkFixedValue(errors, path + '.upperLimit', focus.getNamedChild('upperLimit'), fixed.UpperLimitElement, 'upperLimit', focus);
   checkFixedValue(errors, path + '.dimensions', focus.getNamedChild('dimensions'), fixed.DimensionsElement, 'dimensions', focus);
   checkFixedValue(errors, path + '.data', focus.getNamedChild('data'), fixed.DataElement, 'data', focus);
end;

procedure TInstanceValidator.checkTiming(errors : TFslList<TFhirValidationMessage>; path : String; focus : TFhirMMElement; fixed : TFhirTiming);
var
  events : TFslList<TFhirMMElement>;
  i : integer;
begin
  checkFixedValue(errors, path + '.repeat', focus.getNamedChild('repeat'), fixed.repeat_, 'value', focus);
  events := TFslList<TFhirMMElement>.Create();
  try
    focus.getNamedChildren('event', events);
    if (rule(errors, itVALUE, focus.LocationStart.line, focus.LocationStart.col, path, events.count = fixed.eventList.count, 'Expected ' + Integer.toString(fixed.eventList.count) + ' but found ' + Integer.toString(events.count) + ' event elements')) then
      for i := 0 to events.count - 1 do
        checkFixedValue(errors, path + '.event', events[i], fixed.eventList[i], 'event', focus);
  finally
    events.Free;
  end;
end;

function TInstanceValidator.codeinExpansion(cnt : TFhirValueSetExpansionContains; system : String; code : String) : boolean;
var
  c : TFhirValueSetExpansionContains;
begin
  for c in cnt.containsList do
  begin
    if (code = c.code) and (system = c.system) then
       exit(true);

    if (codeinExpansion(c, system, code)) then
       exit(true);
  end;
  exit(false);
end;

function TInstanceValidator.codeInExpansion(vs : TFhirValueSet; system : String; code : String) : boolean;
var
  c : TFhirValueSetExpansionContains;
begin
  for c in vs.expansion.containsList do
  begin
    if (code = c.code) and (system = c.system) then
       exit(true);

    if (codeinExpansion(c, system, code)) then
       exit(true);
  end;
  exit(false);
end;

function TInstanceValidator.describeReference(reference : String) : String;
begin
  if (reference = '') then
    exit('nil');
  exit(reference);
end;

function TInstanceValidator.describeTypes(types : TFhirElementDefinitionTypeList) : String;
var
  b : TStringBuilder;
  t : TFhirElementDefinitionType;
begin
  b := TStringBuilder.Create();
  try
    for t in types do
      b.append(','+t.code);
    exit(b.toString().Substring(1));
  finally
    b.Free;
  end;
end;

function TInstanceValidator.findElement(profile : TFhirStructureDefinition; name : String) : TFhirElementDefinition;
var
  c : TFhirElementDefinition;
begin
  for c in profile.snapshot.elementList do
    if (c.path = name) then
      exit(c);
  exit(nil);
end;

function TInstanceValidator.getBaseType(profile : TFhirStructureDefinition; pr : String) : String;
var
  p : TFhirStructureDefinition;
begin
  p := resolveProfile(profile, pr);
  try
    if (p = nil) then
      exit('')
    else
      exit(p.type_);
  finally
    p.Free;
  end;
end;

function TInstanceValidator.getCodeDefinition(c : TFhirCodeSystemConcept; code : String) : TFhirCodeSystemConcept;
var
  g : TFhirCodeSystemConcept;
  r : TFhirCodeSystemConcept;
begin
  if (code.equals(c.code)) then
    exit(c);

  for g in c.conceptList do
  begin
    r :=  getCodeDefinition(g, code);
    if (r <> nil) then
       exit(r);
  end;
  exit(nil);
end;

function TInstanceValidator.getCodeDefinition(cs : TFhirCodeSystem; code : String) : TFhirCodeSystemConcept;
var
  c : TFhirCodeSystemConcept;
  r : TFhirCodeSystemConcept;
begin
  for c in cs.conceptList do
  begin
    r :=  getCodeDefinition(c, code);
    if (r <> nil) then
       exit(r);
  end;
  exit(nil);
end;

function TInstanceValidator.getContainedById(container : TFhirMMElement; id : String) : TFhirMMElement;
var
  contained : TFslList<TFhirMMElement>;
  we : TFhirMMElement;
begin
  contained := TFslList<TFhirMMElement>.Create();
  try
    container.getNamedChildren('contained', contained);
    for we in contained do
    begin
      if (id.equals(we.getNamedChildValue('id'))) then
        exit(we);
    end;
    exit(nil);
  finally
    contained.Free;
  end;
end;

function TInstanceValidator.getCriteriaForDiscriminator(path : String; element : TFhirElementDefinition; discriminator : String; profile : TFhirStructureDefinition; removeResolve : boolean) : TFhirElementDefinition;
var
  expr : TFhirPathExpressionNode;
  t2 : cardinal;
  ed : TFhirElementDefinition;
begin
  if ('value' = discriminator) and (element.fixed <> nil) then
    exit(element);

  if (removeResolve) then
    if (discriminator = 'resolve()') then
      exit(element);

  if (discriminator.endsWith('.resolve()')) then
    discriminator := discriminator.substring(0, discriminator.length - 10);

  raise ETodo.Create('Not done yet');
  {
  expr := fpe.parse(discriminator);
  t2 := getTickCount;
  ed :=  fpe.evaluateDefinition(expr, profile, element);
  sdTime := sdTime + (getTickCount - t2);
  exit(ed);
  }
end;

function TInstanceValidator.getExtensionByUrl(extensions : TFslList<TFhirMMElement>; urlSimple : String) : TFhirMMElement;
var
  e : TFhirMMElement;
begin
  for e in extensions do
  begin
    if (urlSimple.equals(e.getNamedChildValue('url'))) then
      exit(e);

  end;
exit(nil);
end;

function TInstanceValidator.getFromBundle(bundle : TFhirMMElement; ref : String; fullUrl : String; errors : TFslList<TFhirValidationMessage>; path : String) : TFhirMMElement;
var
  targetUrl : String;
  version : String;
  base : String;
  parts : TArray<string>;
  id : String;
  entries : TFslList<TFhirMMElement>;
  match : TFhirMMElement;
  we : TFhirMMElement;
  r : TFhirMMElement;
  i : integer;
begin
  targetUrl := '';
  version :=  '';
  if (ref.startsWith('http')) or (ref.startsWith('urn')) then
    if (ref.contains('/_history/')) then
    begin
      targetUrl := ref.substring(0, ref.indexOf('/_history/') - 1);
      version := ref.substring(ref.indexOf('/_history/') + 10);
    end
    else
      targetUrl := ref
  else if (fullUrl = '') then
  begin
    rule(errors, itREQUIRED, -1, -1, path, path.startsWith('Bundle.signature'), 'Relative Reference appears inside Bundle whose entry is missing a fullUrl');
    exit(nil);
  end
  else if (length(ref.split(['/'])) <> 2) and (length(ref.split(['/'])) <> 4) then
  begin
    rule(errors, itINVALID, -1, -1, path, false, 'Relative URLs must be of the format [ResourceName]/[id].  Encountered ' + ref);
    exit(nil);
  end
  else
  begin
    base :=  '';
    if (fullUrl.startsWith('urn')) then
    begin
      parts :=  fullUrl.split([':']);
      for i := 0 to length(parts) - 1 do
        base := base + parts[i] + ':';
    end
    else
    begin
      parts := fullUrl.split(['/']);
      for i := 0 to length(parts) - 3 do
        base := base + parts[i] + '/';
    end;
  end;
  id := '';
  if (ref.contains('/_history/')) then
  begin
    version := ref.substring(ref.indexOf('/_history/') + 10);
    id := ref.substring(0, ref.indexOf('/_history/')).split(['/'])[1];
  end
  else if (base.startsWith('urn')) then
    id := ref.split(['/'])[1]
  else
    id := ref;

  targetUrl := base + id;
  entries := TFslList<TFhirMMElement>.Create();
  try
    bundle.getNamedChildren('entry', entries);
    match := nil;
    for we in entries do
    begin
      if (targetUrl.equals(we.getChildValue('fullUrl'))) then
        r := we.getNamedChild('resource')
      else
        r := nil;
      if (version.isEmpty()) then
      begin
        rule(errors, itFORBIDDEN, -1, -1, path, match = nil, 'Multiple matches in bundle for reference ' + ref);
        match := r;
      end
      else
      try
        if (version = r.getNamedChild('meta').getChildValue('versionId')) then
        begin
          rule(errors, itFORBIDDEN, -1, -1, path, match = nil, 'Multiple matches in bundle for reference ' + ref);
          match := r;
        end;
      except
        warning(errors, itREQUIRED, -1, -1, path, (r.getNamedChild('meta') <> nil) and (r.getNamedChild('meta').getChildValue('versionId') <> ''), 'Entries matching fullURL ' + targetUrl + ' should declare meta/versionId because there are version-specific references');
      end;
    end;
    warning(errors, itREQUIRED, -1, -1, path, (match <> nil) or (not targetUrl.startsWith('urn')), 'URN reference is not locally contained within the bundle ' + ref);
    exit(match);
  finally
    entries.free;
  end;
end;

function TInstanceValidator.getProfileForType(type_ : String; list : TFhirElementDefinitionTypeList) : TFhirStructureDefinition;
var
  tr : TFhirElementDefinitionType;
  url : String;
  t : cardinal;
  sd : TFhirStructureDefinition;
begin
  for tr in list do
  begin
    url := tr.code;
    if (not isAbsoluteUrl(url)) then
      url := 'http://hl7.org/fhir/StructureDefinition/' + url;
    t := getTickCount;
    sd :=  context.fetchResource(frtStructureDefinition, url) as TFhirStructureDefinition;
    sdTime := sdTime + (getTickCount - t);
    if (sd <> nil) and ((sd.type_ = type_) or (sd.url = type_)) and (sd.snapshot <> nil) then
      exit(sd);
  end;
  exit(nil);
end;

function TInstanceValidator.getValueForDiscriminator(appContext : TFhirObject; errors : TFslList<TFhirValidationMessage>; element : TFhirMMElement; discriminator : String; criteria : TFhirElementDefinition; stack : TNodeStack) : TFhirMMElement;
var
  p : String;
  focus : TFhirMMElement;
  dlist : TArray<String>;
  d : String;
  url : String;
  target : TFhirMMElement;
  children : TFHIRSelectionList;
begin
  p := stack.getLiteralPath() + '.' + element.Name;
  focus := element;
  dlist :=  discriminator.split(['.']);
  for d in dlist do
  begin
    if (focus.fhirType = 'Reference') and (d = 'reference') then
    begin
      url :=  focus.getChildValue('reference');
      if (url = '') then
        raise EFHIRException.create('No reference resolving discriminator ' + discriminator + ' from ' + element.prop.name);

      target :=  resolve(appContext, url, stack, errors, p);
      if (target = nil) then
        raise EFHIRException.create('Unable to find resource ' + url + ' at ' + d + ' resolving discriminator ' + discriminator + ' from ' + element.prop.Name);
      focus := target;
    end
    else if (d = 'value') and focus.isPrimitive() then
      exit(focus)
    else
    begin
      children := TFHIRSelectionList.create;
      try
        focus.GetChildrenByName(d, children);
        if (children.Empty) then
          raise EFHIRException.create('Unable to find ' + d + ' resolving discriminator ' + discriminator + ' from ' + element.Prop.Name);
        if (children.count > 1) then
          raise EFHIRException.create('Found ' + Integer.toString(children.count) + ' items for ' + d + ' resolving discriminator ' + discriminator + ' from ' + element.Prop.Name);

        focus := children[0].value.Link as TFHIRMMElement;
        p := p + '.' + d;
      finally
        children.Free;
      end;
    end;
  end;
  exit(focus);
end;

function TInstanceValidator.getCodeSystem(system : String) : TFhirCodeSystem;
var
  t : cardinal;
begin
  t := getTickCount;
  try
    result := context.fetchResource(frtCodeSystem, system) as TFhirCodeSystem;
  finally
    txTime := txTime + (getTickCount - t);
  end;
end;

function TInstanceValidator.hasTime(fmt : String) : boolean;
begin
  result := fmt.contains('T');
end;

function TInstanceValidator.hasTimeZone(fmt : String) : boolean;
begin
  result := (fmt.length > 10) and fmt.substring(10).contains('-') or fmt.substring(10).contains('+') or fmt.substring(10).contains('Z');
end;

function TInstanceValidator.hint(errors: TFslList<TFhirValidationMessage>;
  invalid: TFhirIssueType; line, col: integer; literalPath: String;
  test: boolean; message: String): boolean;
begin
   raise Exception.Create('Error Message');
end;

function TInstanceValidator.isAbsolute(uri : String) : boolean;
begin
  result := (uri = '') or uri.startsWith('http:') or uri.startsWith('https:') or uri.startsWith('urn:uuid:') or uri.startsWith('urn:oid:') or
      uri.startsWith('urn:ietf:') or uri.startsWith('urn:iso:') or uri.startsWith('urn:iso-astm:') or isValidFHIRUrn(uri);
end;

function TInstanceValidator.isValidFHIRUrn(uri : String) : boolean;
begin
  result := (uri = 'urn:x-fhir:uk:id:nhs-number') or uri.startsWith('urn:');
end;

function TInstanceValidator.isParametersEntry(path : String) : boolean;
var
  parts : TArray<String>;
begin
  parts :=  path.split(['.']);
  result := (length(parts) > 2) and (parts[length(parts) - 1] = 'resource') and pathEntryHasName(parts[length(parts) - 2], 'parameter') or pathEntryHasName(parts[length(parts) - 2], 'part');
end;

function TInstanceValidator.isBundleEntry(path : String) : boolean;
var
  parts : TArray<String>;
begin
  parts :=  path.split(['.']);
  result := (length(parts) > 2) and (parts[length(parts) - 1] = 'resource') and  pathEntryHasName(parts[length(parts) - 2], 'entry');
end;

function TInstanceValidator.isBundleOutcome(path : String) : boolean;
var
  parts : TArray<String>;
begin
  parts :=  path.split(['.']);
  result := (length(parts) > 2) and (parts[length(parts) - 1] = 'outcome') and pathEntryHasName(parts[length(parts) - 2], 'response');
end;

function TInstanceValidator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, extensionDomains.sizeInBytes);
  inc(result, bpWarnings.sizeInBytes);
  inc(result, txTime.sizeInBytes);
  inc(result,  sdTime.sizeInBytes);
  inc(result,  fpeTime.sizeInBytes);
  inc(result,  overall.sizeInBytes);
  inc(result,  loadTime.sizeInBytes);
  inc(result, fetcher.sizeInBytes);
  inc(result, fpe.sizeInBytes);
end;

class function TInstanceValidator.pathEntryHasName(thePathEntry : String; theName : String) : boolean;
begin
  if (thePathEntry = theName) then
    exit(true);

  if (thePathEntry.length >= theName.length + 3) and (thePathEntry.startsWith(theName)) and (thePathEntry[theName.length] = '[') then
    exit(true);
  result := false;
end;

function TInstanceValidator.isPrimitiveType(code : String) : boolean;
var
  sd : TFhirStructureDefinition;
begin
  sd :=  context.fetchStructureDefinition(code);
  try
    result := (sd <> nil) and (sd.kind = StructureDefinitionKindPrimitiveType);
  finally
    sd.Free;
  end;
end;

function TInstanceValidator.nameMatches(name : String; tail : String) : boolean;
begin
  if (tail.endsWith('[x]')) then
    result := name.startsWith(tail.substring(0, tail.length - 3))
  else
    result := name = tail;
end;

function TInstanceValidator.passesCodeWhitespaceRules(v : String) : boolean;
var
  lastWasSpace : boolean;
  c : char;
begin
  if (not v.trim().equals(v)) then
    exit(false);

  lastWasSpace := true;
  for c in v do
  begin
    if (c = ' ') then
      if (lastWasSpace) then
        exit(false)
      else
        lastWasSpace := true
    else if (c.IsWhiteSpace) then
      exit(false)
    else
      lastWasSpace := false;
  end;
  exit(true);
end;

function TInstanceValidator.localResolve(ref : String; stack : TNodeStack; errors : TFslList<TFhirValidationMessage>; path : String) : TFhirMMElement;
var
  fullUrl : String;
  res : TFHIRMMElement;
begin
  if (ref.startsWith('#')) then
  begin
      // work back through the contained list.
      // really, there should only be one level for this (contained resources cannot contain
      // contained resources), but we'll leave that to some other code to worry about
    while (stack <> nil) and (stack.element <> nil) do
    begin
      if (stack.element.prop.isResource()) then
      begin
        // ok, we'll try to find the contained reference
        res := getContainedById(stack.element, ref.substring(1));
        if (res <> nil) then
          exit(res);
      end;
      if (stack.element.special = fsecBUNDLE_ENTRY) then
      begin
        // we don't try to resolve contained references across this boundary
        exit(nil);
      end;
      stack := stack.parent;
    end;
    exit(nil);
  end
  else
  begin
    // work back through the contained list - if any of them are bundles, try to resolve
    // the resource in the bundle
    fullUrl := '';
    while (stack <> nil) and (stack.element <> nil) do
    begin
      if (stack.element.special = fsecBUNDLE_ENTRY) and (fullUrl = '') and (stack.parent.element.Name= 'entry') then
      begin
        // we don't try to resolve contained references across this boundary
        fullUrl := stack.parent.element.getChildValue('fullUrl');
        if (fullUrl = '') then
          rule(errors, itREQUIRED, stack.parent.element.LocationStart.line, stack.parent.element.LocationStart.col, stack.parent.getLiteralPath(), fullUrl <> '', 'Bundle entry missing fullUrl');
      end;
      if ('Bundle' = stack.element.type_) then
      begin
        res := getFromBundle(stack.element, ref, fullUrl, errors, path);
        exit(res);
      end;
      stack := stack.parent;
    end;
    exit(nil);
  end;
end;

function TInstanceValidator.matchSlice(hostContext: TFhirValidatorHostContext;
  errors: TFslList<TFhirValidationMessage>; profile: TFhirStructureDefinition;
  stack: TNodeStack; slicer: TFHIRElementDefinition;
  unsupportedSlicing: boolean; problematicPaths: TStringList; sliceOffset,
  i: integer; ed: TFHIRElementDefinition; childUnsupportedSlicing: boolean;
  ei: TElementInfo): boolean;
begin
   raise Exception.Create('Error Message');

end;

function TInstanceValidator.resolve(appContext : TFhirObject; ref : String; stack : TNodeStack; errors : TFslList<TFhirValidationMessage>; path : String) : TFhirMMElement;
var
  local : TFhirMMElement;
begin
  local :=  localResolve(ref, stack, errors, path);
  if (local <> nil) then
exit(local);

  if (fetcher = nil) then
exit(nil);

exit(fetcher.fetch(appContext, ref));
end;

function TInstanceValidator.resolveBindingReference(ctxt : TFhirDomainResource; reference : String; uri : String) : TFhirValueSet;
var
  c : TFhirResource;
  t : cardinal;
  fr : TFhirValueSet;
begin
  if (reference <> '') then
  begin
    if (reference.startsWith('#')) then
    begin
      for c in ctxt.containedList do
      begin
        if (c.id = reference.substring(1)) and (c is TFHIRValueSet) then
        exit(c.link as TFhirValueSet);
      end;
      exit(nil);
    end
    else
    begin
      if (not isAbsoluteUrl(reference)) then
        reference := resolve(uri, reference);
      t := getTickCount;
      fr :=  context.fetchValueSet(reference);
      txTime := txTime + (getTickCount - t);
      exit(fr);
    end
  end
  else
    exit(nil);
end;

function TInstanceValidator.resolve(uri : String; ref : String) : String;
var
  up : TArray<String>;
  rp : TArray<String>;
  b : TStringBuilder;
  i : integer;
begin
  if (uri = '') then
    exit(ref);

  up :=  uri.split(['/']);
  rp :=  ref.split(['/']);
  if (context.getResourceNames().contains(up[length(up) - 2])) and (context.getResourceNames().contains(rp[0])) then
  begin
    b := TStringBuilder.Create();
    try
      for i := 0 to length(up) - 3 do
      begin
        b.append(up[i]);
        b.append('/');
      end;
      b.append(ref);
      exit(b.toString());
    finally
      b.Free;
    end;
  end
  else
    exit(ref);
end;

function TInstanceValidator.resolveInBundle(entries : TFslList<TFhirMMElement>; ref : String; fullUrl : String; type_ : String; id : String) : TFhirMMElement;
var
  entry : TFhirMMElement;
  fu : String;
  u : String;
  parts : TArray<String>;
  t : String;
  i : String;
  resource : TFhirMMElement;
  et : String;
  eid : String;
begin
  if (isAbsoluteUrl(ref)) then
  begin
    for entry in entries do
    begin
      fu :=  entry.getNamedChildValue('fullUrl');
      if (ref.equals(fu)) then
         exit(entry)
      end;
    exit(nil);
  end
  else
  begin
    u := '';
    if (fullUrl <> '') and (fullUrl.endsWith(type_ + '/' + id)) then
      u := fullUrl.substring((type_ + '/' + id).length) + ref;

    parts :=  ref.split(['/']);
    if (length(parts) >= 2) then
    begin
      t := parts[0];
      i := parts[1];
      for entry in entries do
      begin
        fu :=  entry.getNamedChildValue('fullUrl');
        if (u <> '') and (fullUrl.equals(u)) then
          exit(entry);

        if (u = '') then
        begin
          resource := entry.getNamedChild('resource');
          et := resource.type_;
          eid := resource.getNamedChildValue('id');
          if (t = et) and (i = eid) then
            exit(entry);
        end;
      end;
    end;
    exit(nil);
  end;
end;

function TInstanceValidator.resolveNameReference(snapshot : TFhirStructureDefinitionSnapshot; contentReference : String) : TFhirElementDefinition;
var
  ed : TFhirElementDefinition;
begin
  for ed in snapshot.elementList do
  begin
    if (contentReference = '#' + ed.id) then
      exit(ed);
  end;
  exit(nil);
end;

function TInstanceValidator.resolveProfile(profile : TFhirStructureDefinition; pr : String) : TFhirStructureDefinition;
var
  r : TFhirResource;
  t : cardinal;
  fr : TFhirStructureDefinition;
begin
  if (pr.startsWith('#')) then
  begin
    for r in profile.containedList do
    begin
      if (r.id.equals(pr.substring(1))) and (r is TFHIRStructureDefinition) then
        exit(r.link as TFhirStructureDefinition);
    end;
    exit(nil);
  end
  else
  begin
    t := getTickCount;
    fr := context.fetchStructureDefinition(pr);
    sdTime := sdTime + (getTickCount - t);
    exit(fr);
  end;
end;

function TInstanceValidator.resolveType(type_ : String; list : TFhirElementDefinitionTypeList) : TFhirElementDefinition;
var
  tr : TFhirElementDefinitionType;
  url : String;
  t : cardinal;
  sd : TFhirStructureDefinition;
begin
  for tr in list do
  begin
    url := tr.code;
    if (not isAbsoluteUrl(url)) then
      url := 'http://hl7.org/fhir/StructureDefinition/' + url;

    t := getTickCount;
    sd :=  context.fetchStructureDefinition(url);
    try
      sdTime := sdTime + (getTickCount - t);
      if (sd <> nil) and (sd.type_ = type_) or (sd.url = type_) and (sd.snapshot <> nil) then
        exit(sd.snapshot.elementList[0]);
    finally
      sd.free;
    end;
  end;
  exit(nil);
end;

function TInstanceValidator.rule(errors: TFslList<TFhirValidationMessage>;
  invalid: TFhirIssueType; line, col: integer; literalPath: String;
  test: boolean; message: String): boolean;
begin
   raise Exception.Create('Error Message');
end;

function TInstanceValidator.sliceMatches(hostContext : TFhirValidatorHostContext; element : TFhirMMElement; path : String; slicer : TFhirElementDefinition; ed : TFhirElementDefinition; profile : TFhirStructureDefinition; errors : TFslList<TFhirValidationMessage>; stack : TNodeStack) : boolean;
var
  n : TFhirPathExpressionNode;
  t : cardinal;
  expression : TStringBuilder;
  s : TFhirElementDefinitionSlicingDiscriminator;
  discriminator : String;
  criteriaElement : TFhirElementDefinition;
  type_ : String;
  lastNode : String;
  list : TFhirCanonicalList;
  msg : String;
  ok : boolean;
begin
  if (not slicer.slicing.hasDiscriminatorList) then
    exit(false);

  n := ed.Tag as TFHIRPathExpressionNode;
  if (n = nil) then
  begin
    t := getTickCount;
    expression := TStringBuilder.Create('true');
    for s in slicer.slicing.discriminatorList do
    begin
      discriminator := s.path;
      criteriaElement :=  getCriteriaForDiscriminator(path, ed, discriminator, profile, s.type_ = DiscriminatorTypeProfile);
      if (s.type_ = DiscriminatorTypeType) then
      begin
        type_ := '';
        if (not criteriaElement.path.contains('[')) and (discriminator.contains('[')) then
        begin
          discriminator := discriminator.substring(0, discriminator.indexOf('['));
          lastNode :=  tail(discriminator);
          type_ := tail(criteriaElement.path).substring(lastNode.length);
          type_ := type_.substring(0, 1).ToLower + type_.substring(1);
        end
        else if (not criteriaElement.hasType_List) or (criteriaElement.type_List.count = 1) then
        begin
          if (discriminator.contains('[')) then
            discriminator := discriminator.substring(0, discriminator.indexOf('['));
          type_ := criteriaElement.type_List[0].code;
        end;
        if (type_ = '') then
          raise EDefinitionException.create('Discriminator (' + discriminator + ') is based on type_, but slice ' + ed.id + ' does not declare a type_');
        if (discriminator.isEmpty()) then
          expression.append(' and this is ' + type_)
        else
          expression.append(' and ' + discriminator + ' is ' + type_);
      end
      else if (s.type_ = DiscriminatorTypePROFILE) then
      begin
        if (criteriaElement.type_List.count = 0) then
          raise EDefinitionException.create('Profile based discriminators nust have a type_ (' + criteriaElement.id + ')');
        if (criteriaElement.type_List.count <> 1) then
          raise EDefinitionException.create('Profile based discriminators nust have only one type_ (' + criteriaElement.id + ')');
        if discriminator.endsWith('.resolve()') or (discriminator= 'resolve()') then
          list := criteriaElement.type_List[0].targetProfileList
        else
          list := criteriaElement.type_List[0].profileList;
        if (list.count = 0) then
          raise EDefinitionException.create('Profile based discriminators nust have a type_ with a profile (' + criteriaElement.id + ')');
        if (list.count > 1) then
         raise EDefinitionException.create('Profile based discriminators nust have a type_ with only one profile (' + criteriaElement.id + ')');
        expression.append(' and ' + discriminator + '.conformsTo(''' + list[0].value + ''')');
      end
      else if (s.type_ = DiscriminatorTypeEXISTS) then
      begin
        if (criteriaElement.minElement <> nil) and (criteriaElement.min >= '1') then
          expression.append(' and (' + discriminator + '.exists())')
        else if (criteriaElement.maxElement <> nil) and (criteriaElement.max = '0') then
          expression.append(' and (' + discriminator + '.exists().not())')
        else
          raise EDefinitionException.create('Discriminator (' + discriminator + ') is based on element existence, but slice ' + ed.id + ' neither sets min>:=1 or max:=0');
      end
      else if (criteriaElement.fixed <> nil) then
        buildFixedExpression(ed, expression, discriminator, criteriaElement)
      else if (criteriaElement.pattern <> nil) then
        buildPattternExpression(ed, expression, discriminator, criteriaElement)
      else if (criteriaElement.binding <> nil) and (criteriaElement.binding.strengthElement <> nil) and
         (criteriaElement.binding.strength = BindingStrengthREQUIRED) and (criteriaElement.binding.valueSet <> '') then
        expression.append(' and (' + discriminator + ' memberOf ''' + criteriaElement.binding.valueSet + ''')')
      else
        raise EDefinitionException.create('Could not match discriminator (' + discriminator + ') for slice ' + ed.id + ' in profile ' + profile.url + ' - does not have fixed value, binding or existence assertions');
    end;
    try
      n := fpe.parse(expression.toString());
    except
       on e : Exception do
        raise EFHIRException.create('Problem processing expression ' + expression.ToString + ' in profile ' + profile.url + ' path ' + path + ': ' + e.Message);
    end;
    fpeTime := fpeTime + (getTickCount - t);
    ed.Tag := n;
  end;
  result := evaluateSlicingExpression(hostContext, element, path, profile, n);
end;

function TInstanceValidator.evaluateSlicingExpression(hostContext : TFhirValidatorHostContext; element : TFhirMMElement; path : String; profile : TFhirStructureDefinition; expression : TFHIRPathExpressionNode) : boolean;
var
  msg : String;
  ok : boolean;
  t : Cardinal;
begin
  try
    t := GetTickCount;
    ok := fpe.evaluateToBoolean(hostContext, nil {hostContext.}, element, expression);
    fpeTime := fpeTime + (GetTickCount - t);
    msg := fpe.UseLog;
  except
    on ex : Exception do
      raise EFHIRException.create('Problem evaluating slicing expression for element in profile ' + profile.Url + ' path "' + path + ' (fhirPath = '+expression.toString+'): ' + ex.Message);
  end;
  result := ok;
end;

procedure TInstanceValidator.buildPattternExpression(ed : TFhirElementDefinition; expression : TStringBuilder; discriminator : String; criteriaElement : TFhirElementDefinition);
var
  pattern : TFhirType;
  cc : TFhirCodeableConcept;
begin
  pattern := criteriaElement.pattern;
  if (pattern is TFHIRCodeableConcept) then
  begin
    cc := pattern as TFhirCodeableConcept;
    buildCodeableConceptExpression(ed, expression, discriminator, cc);
  end
  else
    raise EDefinitionException.create('Unsupported fixed pattern type_ for discriminator(' + discriminator + ') for slice ' + ed.id + ': ' + pattern.fhirType);
end;

procedure TInstanceValidator.buildCodeableConceptExpression(ed : TFhirElementDefinition; expression : TStringBuilder; discriminator : String; cc : TFhirCodeableConcept);
var
  c : TFhirCoding;
  first : boolean;
begin
  if (cc.text <> '') then
    raise EDefinitionException.create('Unsupported CodeableConcept pattern - using text - for discriminator(' + discriminator + ') for slice ' + ed.id);

  if (not cc.hasCoding()) or (cc.CodingList.count > 1) then
    raise EDefinitionException.create('Unsupported CodeableConcept pattern - must be just one coding - for discriminator(' + discriminator + ') for slice ' + ed.id);

  c := cc.codingList[0];
  if (c.hasExtensions) or (cc.hasExtensions) then
    raise EDefinitionException.create('Unsupported CodeableConcept pattern - extensions are not allowed - for discriminator(' + discriminator + ') for slice ' + ed.id);

  expression.append(' and ' + discriminator + '.coding.where(');
  first := true;
  if (c.system <> '') then
  begin
    first := false;
    expression.append('system := ''' + c.system + '''');
  end;
  if (c.version <> '') then
  begin
    if (first) then first := false else expression.append(' and ');
    expression.append('version := ''' + c.Version + '''');
  end;
  if (c.code <> '') then
  begin
    if (first) then first := false else expression.append(' and ');
    expression.append('code := ''' + c.code + '''');
  end;
  if (c.display <> '') then
  begin
    if (first) then first := false else expression.append(' and ');
    expression.append('display := ''' + c.display + '''');
  end;
  expression.append(').exists()');
end;

procedure TInstanceValidator.buildFixedExpression(ed : TFhirElementDefinition; expression : TStringBuilder; discriminator : String; criteriaElement : TFhirElementDefinition);
var
  fixed : TFhirType;
  cc : TFhirCodeableConcept;
  json : String;
  escapedString : String;
begin
  fixed := criteriaElement.fixed;
  if (fixed is TFHIRCodeableConcept) then
  begin
    cc := fixed as TFhirCodeableConcept;
    buildCodeableConceptExpression(ed, expression, discriminator, cc);
  end
  else
  begin
    expression.append(' and (' + discriminator + ' := ');
    if (fixed is TFHIRString) then
      expression.append('''' + fixed.primitiveValue + '''')
    else if (fixed is TFHIRUri) then
      expression.append('''' + fixed.primitiveValue + '''')
    else if (fixed is TFHIRInteger) then
      expression.append(fixed.primitiveValue)
    else if (fixed is TFHIRDecimal) then
      expression.append(fixed.primitiveValue)
    else if (fixed is TFHIRBoolean) then
      expression.append(fixed.primitiveValue)
    else
      raise EDefinitionException.create('Unsupported fixed value type_ for discriminator(' + discriminator + ') for slice ' + ed.id + ': ' + fixed.fhirType);
    expression.append(')');
  end;
end;

procedure TInstanceValidator.start(hostContext : TFhirValidatorHostContext; errors : TFslList<TFhirValidationMessage>; resource : TFhirMMElement; element : TFhirMMElement; defn : TFhirStructureDefinition; stack : TNodeStack);
var
  resourceProfiles : TValidationProfileSet;
  profileUsage : TFhirProfileUsage;
begin
  resourceProfiles :=  getResourceProfiles(element, stack);
  if (not resourceProfiles.isProcessed) then
     checkDeclaredProfiles(resourceProfiles, errors, resource, element, stack);

  if (not resourceProfiles.isProcessed) then
      resourceProfiles.IsProcessed := true;
      if (not resourceProfiles.hasProfiles) and ((rule(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), defn.snapshot <> nil, 'StructureDefinition has no snapshot - validation is against the snapshot, so it must be provided'))) then
           validateElement(hostContext, errors, defn, defn.snapshot.elementList[0], nil, nil, resource, element, element.Name, stack, false);

  if (element.type_= 'Bundle') then
    validateBundle(errors, element, stack)
  else if (element.type_ = 'Observation') then
    validateObservation(errors, element, stack)
  else if (element.type_= 'QuestionnaireResponse') then
    validateQuestionannaireResponse(errors, element, stack)
  else  if (element.type_= 'CodeSystem') then
    validateCodeSystem(errors, element, stack);

  for profileUsage in resourceProfiles.uncheckedProfiles do
  begin
      profileUsage.Checked := true;
       validateElement(hostContext, errors, profileUsage.profile, profileUsage.profile.snapshot.elementList[0], nil, nil, resource, element, element.Name, stack, false);
// todo: re-enable this when I can deal with the impact...   (GG)

// if (not profileUsage.profile.type_.equals(resource.fhirType()))

  end;
end;

procedure TInstanceValidator.validateCodeSystem(errors : TFslList<TFhirValidationMessage>; cs : TFhirMMElement; stack : TNodeStack);
var
  url : String;
  vsu : String;
  vs : TFhirValueSet;
begin
  url :=  cs.getNamedChildValue('url');
  vsu :=  cs.getNamedChildValue('valueSet');
  if vsu <> '' then
  begin
    vs := context.fetchValueSet(vsu);
    if (vs <> nil) then
        if (rule(errors, itBUSINESSRULE, cs.LocationStart.line,0, stack.getLiteralPath(), (vs.compose <> nil) and (vs.Expansion= nil), 'CodeSystem ' + url + ' has an "all system" value set of ' + vsu + ', but it is an expansion')) then
          if (rule(errors, itBUSINESSRULE, cs.LocationStart.line,0, stack.getLiteralPath(), vs.compose.includeList.count = 1, 'CodeSystem ' + url + ' has a "all system" value set of ' + vsu + ', but doesn''t have a single include')) then
            if (rule(errors, itBUSINESSRULE, cs.LocationStart.line,0, stack.getLiteralPath(), vs.compose.includeList[0].system.equals(url), 'CodeSystem ' + url + ' has a "all system" value set of ' + vsu + ', but doesn''t have a matching system (' + vs.compose.includeList[0].system + ')')) then
                 rule(errors, itBUSINESSRULE, cs.LocationStart.line,0, stack.getLiteralPath(), not vs.compose.includeList[0].hasValueSetList and not vs.compose.includeList[0].hasConceptList and not vs.compose.includeList[0].hasFilterList, 'CodeSystem ' + url + ' has a "all system" value set of ' + vsu + ', but the include has extra details');
// todo... try getting the value set the other way...
  end;
end;

procedure TInstanceValidator.validateQuestionannaireResponse(errors : TFslList<TFhirValidationMessage>; element : TFhirMMElement; stack : TNodeStack);
var
  q : TFhirMMElement;
  questionnaire : String;
  t : cardinal;
  qsrc : TFhirQuestionnaire;
  inProgress : boolean;
begin
//  q :=  element.getNamedChild('questionnaire');
//  questionnaire := '';
//  if (q <> nil) then
//  begin
//      if (isNotBlank(q.value)) then
//          questionnaire := q.value;
//else
//        if (isNotBlank(q.getChildValue('reference'))) then
//            questionnaire := q.getChildValue('reference');
//
//
//
//  if (hint(errors, itREQUIRED, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), questionnaire <> nil, 'No questionnaire is identified, so no validation can be performed against the base questionnaire')) then
//      t := getTickCount;
//      qsrc :=  context.fetchResource(Questionnaire.class, questionnaire);
//      sdTime := sdTime + (getTickCount - t);
//      if (warning(errors, itREQUIRED, q.LocationStart.line, q.LocationStart.col, stack.getLiteralPath(), qsrc <> nil, 'The questionnaire could not be resolved, so no validation can be performed against the base questionnaire')) then
//          inProgress :=  'in-progress' = element.getNamedChildValue('status'));
//           validateQuestionannaireResponseItems(qsrc, qsrc.getItem(), errors, element, stack, inProgress);
//
//
end;

procedure TInstanceValidator.validateQuestionannaireResponseItem(qsrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; errors : TFslList<TFhirValidationMessage>; element : TFhirMMElement; stack : TNodeStack; inProgress : boolean);
//var
//  text : String;
//  answers : TFslList<TFhirMMElement>;
//  answer : TFhirMMElement;
//  ns : TNodeStack;
//  itemType : String;
//  items : TFslList<TFhirMMElement>;
begin
//  text :=  element.getNamedChildValue('text');
//   rule(errors, itINVALID, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), Utilities.noString(text)) or (text.equals(qItem.getText()), 'If text exists, it must match the questionnaire definition for linkId ' + qItem.getLinkId());
//  answers := TFslList<TFhirMMElement>.Create();
//   element.getNamedChildren('answer', answers);
//  if (inProgress) then
//     warning(errors, itREQUIRED, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), (answers.count > 0)) or (not qItem.getRequired(), 'No response answer found for required item ' + qItem.getLinkId());
//else
//     rule(errors, itREQUIRED, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), (answers.count > 0)) or (not qItem.getRequired(), 'No response answer found for required item ' + qItem.getLinkId());
//
//  if (answers.count > 1) then
//     rule(errors, itINVALID, answers.get(1).LocationStart.line, answers.get(1).LocationStart.col, stack.getLiteralPath(), qItem.getRepeats(), 'Only one response answer item with this linkId allowed');
//
//  for answer in answers do
//  begin
//      ns :=  stack.push(answer, -1, nil, nil);
//      case (qItem.type_) of
//        GROUP:            rule(errors, itSTRUCTURE, answer.LocationStart.line, answer.LocationStart.col, stack.getLiteralPath(), false, 'Items of type_ group should not have answers');
//break;
//        // nothing
//DISPLAY: break;
//        BOOLEAN:            validateQuestionnaireResponseItemType(errors, answer, ns, 'boolean');
//break;
//        DECIMAL:            validateQuestionnaireResponseItemType(errors, answer, ns, 'decimal');
//break;
//        INTEGER:            validateQuestionnaireResponseItemType(errors, answer, ns, 'integer');
//break;
//        DATE:            validateQuestionnaireResponseItemType(errors, answer, ns, 'date');
//break;
//        DATETIME:            validateQuestionnaireResponseItemType(errors, answer, ns, 'dateTime');
//break;
//        TIME:            validateQuestionnaireResponseItemType(errors, answer, ns, 'time');
//break;
//        STRING:            validateQuestionnaireResponseItemType(errors, answer, ns, 'string');
//break;
//        TEXT:            validateQuestionnaireResponseItemType(errors, answer, ns, 'text');
//break;
//        URL:            validateQuestionnaireResponseItemType(errors, answer, ns, 'uri');
//break;
//        ATTACHMENT:            validateQuestionnaireResponseItemType(errors, answer, ns, 'Attachment');
//break;
//        REFERENCE:            validateQuestionnaireResponseItemType(errors, answer, ns, 'Reference');
//break;
//        QUANTITY:           if ('Quantity' = validateQuestionnaireResponseItemType(errors, answer, ns, 'Quantity'))) then
//            if (qItem.hasExtension('???')) then
//               validateQuestionnaireResponseItemQuantity(errors, answer, ns);
//
//
//break;
//        CHOICE:           itemType :=  validateQuestionnaireResponseItemType(errors, answer, ns, 'Coding', 'date', 'time', 'integer', 'string');
//          if (itemType <> nil) then
//              if (itemType= 'Coding')) then
//                 validateAnswerCode(errors, answer, ns, qsrc, qItem, false);
//else
//                if (itemType= 'date')) then
//                   checkOption(errors, answer, ns, qsrc, qItem, 'date');
//else
//                  if (itemType= 'time')) then
//                     checkOption(errors, answer, ns, qsrc, qItem, 'time');
//else
//                    if (itemType= 'integer')) then
//                       checkOption(errors, answer, ns, qsrc, qItem, 'integer');
//else
//                      if (itemType= 'string')) then
//                         checkOption(errors, answer, ns, qsrc, qItem, 'string');
//
//
//
//
//
//
//break;
//        OPENCHOICE:           itemType := validateQuestionnaireResponseItemType(errors, answer, ns, 'Coding', 'date', 'time', 'integer', 'string');
//          if (itemType <> nil) then
//              if (itemType= 'Coding')) then
//                 validateAnswerCode(errors, answer, ns, qsrc, qItem, true);
//else
//                if (itemType= 'date')) then
//                   checkOption(errors, answer, ns, qsrc, qItem, 'date');
//else
//                  if (itemType= 'time')) then
//                     checkOption(errors, answer, ns, qsrc, qItem, 'time');
//else
//                    if (itemType= 'integer')) then
//                       checkOption(errors, answer, ns, qsrc, qItem, 'integer');
//else
//                      if (itemType= 'string')) then
//                         checkOption(errors, answer, ns, qsrc, qItem, 'string', true);
//
//
//
//
//
//
//break;
//        QUESTION:         nil: begincom.github.javaparser.ast.stmt.BreakStmt - Aend; // no validation
//break;
//      end;
//       validateQuestionannaireResponseItems(qsrc, qItem.getItem(), errors, answer, stack, inProgress);
//  end;
//  if (qItem.type_ = nil) then
//       fail(errors, itREQUIRED, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), false, 'Definition for item ' + qItem.getLinkId() + ' does not contain a type_');
//else
//    if (qItem.type_ = QuestionnaireItemType.DISPLAY) then
//        items := TFslList<TFhirMMElement>.Create();
//         element.getNamedChildren('item', items);
//         rule(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), items.isEmpty(), 'Items not of type_ DISPLAY should not have items - linkId begin0end;', qItem.getLinkId());
//else
//         validateQuestionannaireResponseItems(qsrc, qItem.getItem(), errors, element, stack, inProgress);
//
//
end;

procedure TInstanceValidator.validateQuestionannaireResponseItem(qsrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; errors : TFslList<TFhirValidationMessage>; elements : TFslList<TFhirMMElement>; stack : TNodeStack; inProgress : boolean);
//var
//  element : TFhirMMElement;
//  ns : TNodeStack;
begin
//  if (elements.count > 1) then
//     rule(errors, itINVALID, elements.get(1).LocationStart.line, elements.get(1).LocationStart.col, stack.getLiteralPath(), qItem.getRepeats(), 'Only one response item with this linkId allowed - ' + qItem.getLinkId());
//
//  for element in elements do
//  begin
//      ns :=  stack.push(element, -1, nil, nil);
//       validateQuestionannaireResponseItem(qsrc, qItem, errors, element, ns, inProgress);
//  end;
end;

function TInstanceValidator.getLinkIdIndex(qItems : TFslList<TFhirQuestionnaireItem>; linkId : String) : integer;
begin
//for (int i := 0; i < qItems.count; i++) begin
//    if (linkId.equals(qItems[i].getLinkId()))
//        return i;
//end;
//exit(-1);
  result := -1;
end;

procedure TInstanceValidator.validateQuestionannaireResponseItems(qsrc : TFhirQuestionnaire; qItems : TFslList<TFhirQuestionnaireItem>; errors : TFslList<TFhirValidationMessage>; element : TFhirMMElement; stack : TNodeStack; inProgress : boolean);
//var
//  items : TFslList<TFhirMMElement>;
//  map : TFslMap<TFslList<TFhirMMElement>>;
//  lastIndex : integer;
//  item : TFhirMMElement;
//  linkId : String;
//  index : integer;
//  qItem : TFhirQuestionnaireItem;
//  ns : TNodeStack;
//  mapItem : TFslList<TFhirMMElement>;
//  qItem : TFhirQuestionnaireItem;
//  mapItem : TFslList<TFhirMMElement>;
begin
//  items := TFslList<TFhirMMElement>.Create();
//   element.getNamedChildren('item', items);
//  map := TFslMap<TFslList<TFhirMMElement>>.Create();
//  lastIndex :=  -1;
//  for item in items do
//  begin
//      linkId :=  item.getNamedChildValue('linkId');
//      if (rule(errors, itREQUIRED, item.LocationStart.line, item.LocationStart.col, stack.getLiteralPath(), not Utilities.noString(linkId), 'No LinkId, so can''t be validated')) then
//          index :=  getLinkIdIndex(qItems, linkId);
//          if (index = -1) then
//              qItem :=  findQuestionnaireItem(qsrc, linkId);
//              if (qItem <> nil) then
//                   rule(errors, itSTRUCTURE, item.LocationStart.line, item.LocationStart.col, stack.getLiteralPath(), index > -1, 'Structural Error: item is in the wrong place');
//                  ns :=  stack.push(item, -1, nil, nil);
//                   validateQuestionannaireResponseItem(qsrc, qItem, errors, element, ns, inProgress);
//else
//                 rule(errors, itNOTFOUND, item.LocationStart.line, item.LocationStart.col, stack.getLiteralPath(), index > -1, 'LinkId \''' + linkId + '\' not found in questionnaire');
//
//else
//               rule(errors, itSTRUCTURE, item.LocationStart.line, item.LocationStart.col, stack.getLiteralPath(), index >:= lastIndex, 'Structural Error: items are out of order');
//              lastIndex := index;
//              mapItem :=  map.get(linkId);
//              if (mapItem = nil) then
//                  mapItem := new ArrayList<Element>();
//                   map.put(linkId, mapItem);
//
//              mapItem..add(item);
//
//
//  end;
//  for qItem in qItems do
//  begin
//      mapItem :=  map.get(qItem.getLinkId());
//      if (mapItem <> nil) then
//         validateQuestionannaireResponseItem(qsrc, qItem, errors, mapItem, stack, inProgress);
//else
//         rule(errors, itREQUIRED, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), not qItem.getRequired(), 'No response found for required item ' + qItem.getLinkId());
//
//  end;
end;

procedure TInstanceValidator.validateQuestionnaireResponseItemQuantity(errors : TFslList<TFhirValidationMessage>; answer : TFhirMMElement; stack : TNodeStack);
begin
end;

function TInstanceValidator.validateQuestionnaireResponseItemType(errors : TFslList<TFhirValidationMessage>; element : TFhirMMElement; stack : TNodeStack; types : String) : String;
//var
//  values : TFslList<TFhirMMElement>;
//  ns : TNodeStack;
//  l : TFhirCommaSeparatedStringBuilder;
//  s : String;
begin
//  values := TFslList<TFhirMMElement>.Create();
//   element.getNamedChildrenWithWildcard('value[x]', values);
//for (int i := 0; i < types.length; i++) begin
//    if (types[i]= 'text')) begin
//        types[i] := 'string';
//    end;
//end;
//  if (values.count > 0) then
//      ns :=  stack.push(values[0], -1, nil, nil);
//      l := TFhirCommaSeparatedStringBuilder.Create();
//      for s in types do
//      begin
//           l.append(s);
//          if (values[0].Name= 'value' + Utilities.capitalize(s))) then
//exit(begincom.github.javaparser.ast.expr.EnclosedExpr - Cend; (s));
//
//      end;
//      if (types.length = 1) then
//         rule(errors, itSTRUCTURE, values[0].LocationStart.line, values[0].LocationStart.col, ns.getLiteralPath(), false, 'Answer value must be of type_ ' + types[0]);
//else
//         rule(errors, itSTRUCTURE, values[0].LocationStart.line, values[0].LocationStart.col, ns.getLiteralPath(), false, 'Answer value must be one of the types ' + l.toString());
//
//
//exit(nil);
end;

function TInstanceValidator.findQuestionnaireItem(qSrc : TFhirQuestionnaire; linkId : String) : TFhirQuestionnaireItem;
begin
//exit(findItem(qSrc.getItem(), linkId));
  result := nil;
end;

function TInstanceValidator.findItem(list : TFslList<TFhirQuestionnaireItem>; linkId : String) : TFhirQuestionnaireItem;
//var
//  item : TFhirQuestionnaireItem;
//  result : TFhirQuestionnaireItem;
begin
//  for item in list do
//  begin
//      if (linkId.equals(item.getLinkId())) then
//exit(item);
//
//      result :=  findItem(item.getItem(), linkId);
//      if (result <> nil) then
//exit(result);
//
//  end;
//exit(nil);
  result := nil;
end;

procedure TInstanceValidator.validateAnswerCode(errors : TFslList<TFhirValidationMessage>; value : TFhirMMElement; stack : TNodeStack; qSrc : TFhirQuestionnaire; ref : String; theOpenChoice : boolean);
//var
//  vs : TFhirValueSet;
begin
//  vs :=  resolveBindingReference(qSrc, ref, qSrc.url);
//  if (warning(errors, itCODEINVALID, value.LocationStart.line, value.LocationStart.col, stack.getLiteralPath(), vs <> nil, 'ValueSet ' + describeReference(ref) + ' not found')) then
// try begin
//    Coding c := ObjectConverter.readAsCoding(value);
//    if (isBlank(c.code)) and (isBlank(c.system)) and (isNotBlank(c.display)) begin
//        if (theOpenChoice) begin
//            return;
//        end;
//    end;
//    long t := getTickCount;
//    ValidationResult res := context.validateCode(c, vs);
//    txTime := txTime + (getTickCount - t);
//    if (not res.isOk())
//        rule(errors, reitCODEINVALID, value.LocationStart.line, value.LocationStart.col, stack.getLiteralPath(), false, 'The value provided (' + c.system + '::' + c.code + ') is not in the options value set in the questionnaire');
//end; catch (Exception e) begin
//    warning(errors, itCODEINVALID, value.LocationStart.line, value.LocationStart.col, stack.getLiteralPath(), false, 'Error ' + e.Message + ' validating Coding against Questionnaire Options');
//end;
//
end;

procedure TInstanceValidator.validateAnswerCode(errors : TFslList<TFhirValidationMessage>; answer : TFhirMMElement; stack : TNodeStack; qSrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; theOpenChoice : boolean);
//var
//  v : TFhirMMElement;
//  ns : TNodeStack;
begin
//  v :=  answer.getNamedChild('valueCoding');
//  ns :=  stack.push(v, -1, nil, nil);
//  if (qItem.getAnswerOption().count > 0) then
//     checkCodingOption(errors, answer, stack, qSrc, qItem, theOpenChoice);
//else
//    if (qItem.answerValueSet <> nil) then
//       validateAnswerCode(errors, v, stack, qSrc, qItem.getAnswerValueSet(), theOpenChoice);
//else
//       hint(errors, itSTRUCTURE, v.LocationStart.line, v.LocationStart.col, stack.getLiteralPath(), false, 'Cannot validate options because no option or options are provided');

end;

procedure TInstanceValidator.checkOption(errors : TFslList<TFhirValidationMessage>; answer : TFhirMMElement; stack : TNodeStack; qSrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; type_ : String);
begin
//   checkOption(errors, answer, stack, qSrc, qItem, type_, false);
end;

procedure TInstanceValidator.checkOption(errors : TFslList<TFhirValidationMessage>; answer : TFhirMMElement; stack : TNodeStack; qSrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; type_ : String; openChoice : boolean);
begin
//  if (type_= 'integer')) then
//     checkIntegerOption(errors, answer, stack, qSrc, qItem, openChoice);
//else
//    if (type_= 'date')) then
//       checkDateOption(errors, answer, stack, qSrc, qItem, openChoice);
//else
//      if (type_= 'time')) then
//         checkTimeOption(errors, answer, stack, qSrc, qItem, openChoice);
//else
//        if (type_= 'string')) then
//           checkStringOption(errors, answer, stack, qSrc, qItem, openChoice);
//else
//          if (type_= 'Coding')) then
//             checkCodingOption(errors, answer, stack, qSrc, qItem, openChoice);
//
//
//
//
//
end;

procedure TInstanceValidator.checkIntegerOption(errors : TFslList<TFhirValidationMessage>; answer : TFhirMMElement; stack : TNodeStack; qSrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; openChoice : boolean);
//var
//  v : TFhirMMElement;
//  ns : TNodeStack;
//  list : TFslList<TFhirIntegerType>;
//  components : TFhirQuestionnaireItemAnswerOption;
//  found : boolean;
//  item : TFhirIntegerType;
begin
//  v :=  answer.getNamedChild('valueInteger');
//  ns :=  stack.push(v, -1, nil, nil);
//  if (qItem.getAnswerOption().count > 0) then
//      list := TFslList<TFhirIntegerType>.Create();
//      for components in qItem.answerOption do
//      begin
// try begin
//    list.add(components.getValueIntegerType());
//end; catch (FHIRException e) begin
//// If it's the wrong type_, just keep going
//end;
//      end;
//      if (list.isEmpty()) and (not openChoice) then
//           rule(errors, itSTRUCTURE, v.LocationStart.line, v.LocationStart.col, stack.getLiteralPath(), false, 'Option list has no option values of type_ integer');
//else
//          found := false;
//          for item in list do
//          begin
//              if (item.value = Integer.parseInt(v.primitiveValue)) then
//                  found := true;
//break;
//
//          end;
//          if (not found) then
//               rule(errors, itSTRUCTURE, v.LocationStart.line, v.LocationStart.col, stack.getLiteralPath(), found, 'The integer ' + v.primitiveValue + ' is not a valid option');
//
//
//else
//     hint(errors, itSTRUCTURE, v.LocationStart.line, v.LocationStart.col, stack.getLiteralPath(), false, 'Cannot validate integer answer option because no option list is provided');
//
end;

procedure TInstanceValidator.checkDateOption(errors : TFslList<TFhirValidationMessage>; answer : TFhirMMElement; stack : TNodeStack; qSrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; openChoice : boolean);
//var
//  v : TFhirMMElement;
//  ns : TNodeStack;
//  list : TFslList<TFhirDateType>;
//  components : TFhirQuestionnaireItemAnswerOption;
//  found : boolean;
//  item : TFhirDateType;
begin
//  v :=  answer.getNamedChild('valueDate');
//  ns :=  stack.push(v, -1, nil, nil);
//  if (qItem.getAnswerOption().count > 0) then
//      list := TFslList<TFhirDateType>.Create();
//      for components in qItem.answerOption do
//      begin
// try begin
//    list.add(components.getValueDateType());
//end; catch (FHIRException e) begin
//// If it's the wrong type_, just keep going
//end;
//      end;
//      if (list.isEmpty()) and (not openChoice) then
//           rule(errors, itSTRUCTURE, v.LocationStart.line, v.LocationStart.col, stack.getLiteralPath(), false, 'Option list has no option values of type_ date');
//else
//          found := false;
//          for item in list do
//          begin
//              if (item.value.equals(v.primitiveValue)) then
//                  found := true;
//break;
//
//          end;
//          if (not found) then
//               rule(errors, itSTRUCTURE, v.LocationStart.line, v.LocationStart.col, stack.getLiteralPath(), found, 'The date ' + v.primitiveValue + ' is not a valid option');
//
//
//else
//     hint(errors, itSTRUCTURE, v.LocationStart.line, v.LocationStart.col, stack.getLiteralPath(), false, 'Cannot validate date answer option because no option list is provided');
//
end;

procedure TInstanceValidator.checkTimeOption(errors : TFslList<TFhirValidationMessage>; answer : TFhirMMElement; stack : TNodeStack; qSrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; openChoice : boolean);
//var
//  v : TFhirMMElement;
//  ns : TNodeStack;
//  list : TFslList<TFhirTimeType>;
//  components : TFhirQuestionnaireItemAnswerOption;
//  found : boolean;
//  item : TFhirTimeType;
begin
//  v :=  answer.getNamedChild('valueTime');
//  ns :=  stack.push(v, -1, nil, nil);
//  if (qItem.getAnswerOption().count > 0) then
//      list := TFslList<TFhirTimeType>.Create();
//      for components in qItem.answerOption do
//      begin
// try begin
//    list.add(components.getValueTimeType());
//end; catch (FHIRException e) begin
//// If it's the wrong type_, just keep going
//end;
//      end;
//      if (list.isEmpty()) and (not openChoice) then
//           rule(errors, itSTRUCTURE, v.LocationStart.line, v.LocationStart.col, stack.getLiteralPath(), false, 'Option list has no option values of type_ time');
//else
//          found := false;
//          for item in list do
//          begin
//              if (item.value.equals(v.primitiveValue)) then
//                  found := true;
//break;
//
//          end;
//          if (not found) then
//               rule(errors, itSTRUCTURE, v.LocationStart.line, v.LocationStart.col, stack.getLiteralPath(), found, 'The time ' + v.primitiveValue + ' is not a valid option');
//
//
//else
//     hint(errors, itSTRUCTURE, v.LocationStart.line, v.LocationStart.col, stack.getLiteralPath(), false, 'Cannot validate time answer option because no option list is provided');
//
end;

procedure TInstanceValidator.checkStringOption(errors : TFslList<TFhirValidationMessage>; answer : TFhirMMElement; stack : TNodeStack; qSrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; openChoice : boolean);
//var
//  v : TFhirMMElement;
//  ns : TNodeStack;
//  list : TFslList<TFhirStringType>;
//  components : TFhirQuestionnaireItemAnswerOption;
//  found : boolean;
//  item : TFhirStringType;
begin
//  v :=  answer.getNamedChild('valueString');
//  ns :=  stack.push(v, -1, nil, nil);
//  if (qItem.getAnswerOption().count > 0) then
//      list := TFslList<TFhirStringType>.Create();
//      for components in qItem.answerOption do
//      begin
// try begin
//    if (components.value <> nil) begin
//        list.add(components.getValueStringType());
//    end;
//end; catch (FHIRException e) begin
//// If it's the wrong type_, just keep going
//end;
//      end;
//      if (list.isEmpty()) and (not openChoice) then
//           rule(errors, itSTRUCTURE, v.LocationStart.line, v.LocationStart.col, stack.getLiteralPath(), false, 'Option list has no option values of type_ string');
//else
//          found := false;
//          for item in list do
//          begin
//              if (item.value.equals((v.primitiveValue))) then
//                  found := true;
//break;
//
//          end;
//          if (not found) then
//               rule(errors, itSTRUCTURE, v.LocationStart.line, v.LocationStart.col, stack.getLiteralPath(), found, 'The string ' + v.primitiveValue + ' is not a valid option');
//
//
//else
//       hint(errors, itSTRUCTURE, v.LocationStart.line, v.LocationStart.col, stack.getLiteralPath(), false, 'Cannot validate string answer option because no option list is provided');
//
end;

procedure TInstanceValidator.checkCodingOption(errors : TFslList<TFhirValidationMessage>; answer : TFhirMMElement; stack : TNodeStack; qSrc : TFhirQuestionnaire; qItem : TFhirQuestionnaireItem; openChoice : boolean);
//var
//  v : TFhirMMElement;
//  system : String;
//  code : String;
//  ns : TNodeStack;
//  list : TFslList<TFhirCoding>;
//  components : TFhirQuestionnaireItemAnswerOption;
//  found : boolean;
//  item : TFhirCoding;
begin
//  v :=  answer.getNamedChild('valueCoding');
//  system :=  v.getNamedChildValue('system');
//  code :=  v.getNamedChildValue('code');
//  ns :=  stack.push(v, -1, nil, nil);
//  if (qItem.getAnswerOption().count > 0) then
//      list := TFslList<TFhirCoding>.Create();
//      for components in qItem.answerOption do
//      begin
// try begin
//    if (components.value <> nil) begin
//        list.add(components.getValueCoding());
//    end;
//end; catch (FHIRException e) begin
//// If it's the wrong type_, just keep going
//end;
//      end;
//      if (list.isEmpty()) and (not openChoice) then
//           rule(errors, itSTRUCTURE, v.LocationStart.line, v.LocationStart.col, stack.getLiteralPath(), false, 'Option list has no option values of type_ coding');
//else
//          found := false;
//          for item in list do
//          begin
//              if (ObjectUtil.equals(item.system, system)) and (ObjectUtil.equals(item.code, code)) then
//                  found := true;
//break;
//
//          end;
//          if (not found) then
//               rule(errors, itSTRUCTURE, v.LocationStart.line, v.LocationStart.col, stack.getLiteralPath(), found, 'The code ' + system + '::' + code + ' is not a valid option');
//
//
//else
//     hint(errors, itSTRUCTURE, v.LocationStart.line, v.LocationStart.col, stack.getLiteralPath(), false, 'Cannot validate Coding option because no option list is provided');
//
end;

function TInstanceValidator.tail(path : String) : String;
begin
exit(path.substring(path.lastIndexOf('.') + 1));
end;

function TInstanceValidator.tryParse(ref : String) : String;
var
  parts : TArray<String>;
begin
  parts :=  ref.split(['/']);
  case (length(parts)) of
     1: exit('');
     2: exit(checkResourceType(parts[0]));
  else if (parts[length(parts) - 2]= '_history') then
    exit(checkResourceType(parts[length(parts) - 4]))
  else
    exit(checkResourceType(parts[length(parts) - 2]));
  end;
end;

function TInstanceValidator.typesAreAllReference(theType : TFhirElementDefinitionTypeList) : boolean;
var
  typeRefComponent : TFhirElementDefinitionType;
begin
  for typeRefComponent in theType do
  begin
    if not (typeRefComponent.code = 'Reference') then
      exit(false);
  end;
  exit(true);
end;

procedure TInstanceValidator.validateBundle(errors : TFslList<TFhirValidationMessage>; bundle : TFhirMMElement; stack : TNodeStack);
//var
//  entries : TFslList<TFhirMMElement>;
//  type_ : String;
//  firstEntry : TFhirMMElement;
//  firstStack : TNodeStack;
//  fullUrl : String;
//  resource : TFhirMMElement;
//  id : String;
//  entry : TFhirMMElement;
//  url : String;
begin
//  entries := TFslList<TFhirMMElement>.Create();
//  try
//    bundle.getNamedChildren('entry', entries);
//    type_ :=  bundle.getNamedChildValue('type_');
//    if (entries.count = 0) then
//      rule(errors, itINVALID, stack.getLiteralPath(), not (type_= 'document')) or (type_= 'message')), 'Documents or Messages must contain at least one entry')
//    else
//    begin
//      firstEntry :=  entries[0];
//      firstStack :=  stack.push(firstEntry, 1, nil, nil);
//      fullUrl :=  firstEntry.getNamedChildValue('fullUrl');
//      if (type_= 'document')) then
//          resource :=  firstEntry.getNamedChild('resource');
//          id :=  resource.getNamedChildValue('id');
//          if (rule(errors, itINVALID, firstEntry.LocationStart.line, firstEntry.LocationStart.col, stack.addToLiteralPath('entry', ':0'), resource <> nil, 'No resource on first entry')) then
//               validateDocument(errors, entries, resource, firstStack.push(resource, -1, nil, nil), fullUrl, id);
//
//           checkAllInterlinked(errors, entries, stack, bundle);
//
//      if (type_= 'message')) then
//          resource :=  firstEntry.getNamedChild('resource');
//          id :=  resource.getNamedChildValue('id');
//          if (rule(errors, itINVALID, firstEntry.LocationStart.line, firstEntry.LocationStart.col, stack.addToLiteralPath('entry', ':0'), resource <> nil, 'No resource on first entry')) then
//               validateMessage(errors, entries, resource, firstStack.push(resource, -1, nil, nil), fullUrl, id);
//
//           checkAllInterlinked(errors, entries, stack, bundle);
//
//// We do not yet have rules requiring that the id and fullUrl match when dealing with messaging Bundles
//
//// validateResourceIds(errors, entries, stack);
//
//
//  for entry in entries do
//  begin
//      fullUrl :=  entry.getNamedChildValue('fullUrl');
//      url :=  getCanonicalURLForEntry(entry);
//      id :=  getIdForEntry(entry);
//      if (url <> nil) then
//           rule(errors, itINVALID, entry.LocationStart.line, entry.LocationStart.col, stack.addToLiteralPath('entry', ':0'), not url.equals(fullUrl)) or ((matches(url, Constants.URI_REGEX)) and (url.endsWith('/' + id)), 'The canonical URL (' + url + ') cannot match the fullUrl (' + fullUrl + ') unless the resource id (' + id + ') also matches');
//           rule(errors, itINVALID, entry.LocationStart.line, entry.LocationStart.col, stack.addToLiteralPath('entry', ':0'), not url.equals(fullUrl)) or (serverBase = nil) or ((url.equals(Utilities.pathURL(serverBase, entry.getNamedChild('resource').fhirType(), id))), 'The canonical URL (' + url + ') cannot match the fullUrl (' + fullUrl + ') unless on the canonical server itself');
//
//  end;
//  finally
//    entries.free;
//  end;
end;

function TInstanceValidator.getCanonicalURLForEntry(entry : TFhirMMElement) : String;
var
  e : TFhirMMElement;
begin
  e := entry.getNamedChild('resource');
  if (e = nil) then
    exit('');
  exit(e.getNamedChildValue('url'));
end;

function TInstanceValidator.getIdForEntry(entry : TFhirMMElement) : String;
var
  e : TFhirMMElement;
begin
  e :=  entry.getNamedChild('resource');
  if (e = nil) then
    exit('');

  exit(e.getNamedChildValue('id'));
end;

procedure TInstanceValidator.validateResourceIds(errors : TFslList<TFhirValidationMessage>; entries : TFslList<TFhirMMElement>; stack : TNodeStack);
//var
//  i : integer;
//  entry : TFhirMMElement;
//  fullUrl : String;
//  resource : TFhirMMElement;
//  id : String;
//  urlId : String;
begin
//  i :=  1;
//  for entry in entries do
//  begin
//      fullUrl :=  entry.getNamedChildValue('fullUrl');
//      resource :=  entry.getNamedChild('resource');
//      id := begincom.github.javaparser.ast.expr.ConditionalExpr - Cend; resource <> nil ? resource.getNamedChildValue('id') : nil;
//      if (id <> nil) and (fullUrl <> '') then
//          urlId := nil;
//          if (fullUrl.startsWith('https://')) or (fullUrl.startsWith('http://')) then
//              urlId := fullUrl.substring(fullUrl.lastIndexOf('/') + 1);
//else
//            if (fullUrl.startsWith('urn:uuid')) or (fullUrl.startsWith('urn:oid')) then
//                urlId := fullUrl.substring(fullUrl.lastIndexOf(':') + 1);
//
//
//           rule(errors, itINVALID, entry.LocationStart.line, entry.LocationStart.col, stack.addToLiteralPath('entry[' + i + ']'), urlId.equals(id), 'Resource ID does not match the ID in the entry full URL (''' + id + '' vs ''' + fullUrl + '') ');
//
//       i++;
//  end;
end;

procedure TInstanceValidator.checkAllInterlinked(errors : TFslList<TFhirValidationMessage>; entries : TFslList<TFhirMMElement>; stack : TNodeStack; bundle : TFhirMMElement);
//var
//  visitedResources : TFslMap<TFhirMMElement>;
//  candidateEntries : TFhirHashMap<Element,Element>;
//  candidateResources : TFslList<TFhirMMElement>;
//  entry : TFhirMMElement;
//  sheets : TStringList;
//  links : TFslList<TFhirMMElement>;
//  link : TFhirMMElement;
//  r : TFhirMMElement;
//  url : String;
//  unusedResources : TFslList<TFhirMMElement>;
//  reverseLinksFound : boolean;
//  i : integer;
//  entry : TFhirMMElement;
begin
//  visitedResources := TFslMap<TFhirMMElement>.Create();
//  candidateEntries := TFhirHashMap<Element,Element>.Create();
//  candidateResources := TFslList<TFhirMMElement>.Create();
//  for entry in entries do
//  begin
//       candidateEntries.put(entry.getNamedChild('resource'), entry);
//      candidateResources..add(entry.getNamedChild('resource'));
//  end;
//  sheets := TStringList.Create();
//  links :=  bundle.getChildren('link');
//  for link in links do
//  begin
//      if (link.getChildValue('relation')= 'stylesheet')) then
//          sheets..add(link.getChildValue('url'));
//
//  end;
//  if (not sheets.isEmpty()) then
//      for r in candidateResources do
//      begin
//          url :=  r.getChildValue('fullUrl');
//          if (sheets.contains(url)) then
//             visitedResources.put(url, r);
//
//      end;
//
//  unusedResources := TFslList<TFhirMMElement>.Create();
//  reverseLinksFound := ;
//begincom.github.javaparser.ast.stmt.DoStmt - Aend; do begin
//    reverseLinksFound := false;
//    followResourceLinks(entries[0], visitedResources, candidateEntries, candidateResources, errors, stack);
//    unusedResources.clear();
//    unusedResources.addAll(candidateResources);
//    unusedResources.removeAll(visitedResources.values());
//    for (Element unusedResource : unusedResources) begin
//        List<String> references := findReferences(unusedResource);
//        for (String reference : references) begin
//            if (visitedResources.containsKey(reference)) begin
//                visitedResources.put(reference, unusedResource);
//                reverseLinksFound := true;
//            end;
//        end;
//    end;
//end; while (reverseLinksFound);
//  i :=  0;
//  for entry in entries do
//  begin
//       rule(errors, itINFORMATIONAL, entry.LocationStart.line, entry.LocationStart.col, stack.addToLiteralPath('entry' + '[' + (i + 1) + ']'), not unusedResources.contains(entry.getNamedChild('resource')), 'Entry isn't reachable by traversing from first Bundle entry');
//       i++;
//  end;
//// Lloyd Todo - Loop above four lines to check if the remaining unused resources point *to* any of the visitedResources and if so, add those to the visitedList until nothing more is added.  Any that are still left over are errors
//
end;

procedure TInstanceValidator.followResourceLinks(entry : TFhirMMElement; visitedResources : TFslMap<TFhirMMElement>; candidateEntries : TFhirElementMap; candidateResources : TFslList<TFhirMMElement>; errors : TFslList<TFhirValidationMessage>; stack : TNodeStack);
begin
//   followResourceLinks(entry, visitedResources, candidateEntries, candidateResources, errors, stack, 0);
end;

procedure TInstanceValidator.followResourceLinks(entry : TFhirMMElement; visitedResources : TFslMap<TFhirMMElement>; candidateEntries : TFhirElementMap; candidateResources : TFslList<TFhirMMElement>; errors : TFslList<TFhirValidationMessage>; stack : TNodeStack; depth : integer);
//var
//  resource : TFhirMMElement;
//  references : TStringList;
//  reference : String;
//  r : TFhirMMElement;
begin
//  resource :=  entry.getNamedChild('resource');
//  if (visitedResources.containsValue(resource)) then
//exit();
//
//   visitedResources.put(entry.getNamedChildValue('fullUrl'), resource);
//  references :=  findReferences(resource);
//  for reference in references do
//  begin
//      r :=  getFromBundle(stack.element, reference, entry.getChildValue('fullUrl'), new ArrayList<ValidationMessage>(), stack.addToLiteralPath('entry[' + candidateResources.indexOf(resource) + ']'));
//      if (r <> nil) and (not visitedResources.containsValue(r)) then
//           followResourceLinks(candidateEntries.get(r), visitedResources, candidateEntries, candidateResources, errors, stack, depth + 1);
//
//  end;
end;

function TInstanceValidator.findReferences(start : TFhirMMElement) : TStringList;
var
  references : TStringList;
begin
  references := TStringList.Create();
  findReferences(start, references);
  exit(references);
end;

procedure TInstanceValidator.findReferences(start : TFhirMMElement; references : TStringList);
var
  child : TFhirMMElement;
  ref : String;
begin
  for child in start.children do
  begin
    if (child.type_= 'Reference') then
    begin
      ref :=  child.getChildValue('reference');
      if (ref <> '') and (not ref.startsWith('#')) then
        references.add(ref);
    end;
    findReferences(child, references);
  end;
end;

procedure TInstanceValidator.validateBundleReference(errors : TFslList<TFhirValidationMessage>; entries : TFslList<TFhirMMElement>; ref : TFhirMMElement; name : String; stack : TNodeStack; fullUrl : String; type_ : String; id : String);
//var
//  reference : String;
//  target : TFhirMMElement;
begin
//  reference := '';
// try begin
//    reference := ref.getNamedChildValue('reference');
//end; catch (Error e) begin
//end;
//  if (ref <> nil) and (not Utilities.noString(reference)) then
//      target :=  resolveInBundle(entries, reference, fullUrl, type_, id);
//       rule(errors, itINVALID, ref.LocationStart.line, ref.LocationStart.col, stack.addToLiteralPath('reference'), target <> nil, 'Unable to resolve the target of the reference in the bundle (' + name + ')');
//
end;

procedure TInstanceValidator.validateContains(hostContext : TFhirValidatorHostContext; errors : TFslList<TFhirValidationMessage>; path : String; child : TFhirElementDefinition; context : TFhirElementDefinition; resource : TFhirMMElement; element : TFhirMMElement; stack : TNodeStack; idstatus : TFhirIdStatus);
//var
//  resourceName : String;
//  t : cardinal;
//  profile : TFhirStructureDefinition;
begin
//  resourceName := element.type_;
//  t := getTickCount;
//  profile :=  self.context.fetchResource(frtStructureDefinition, 'http://hl7.org/fhir/StructureDefinition/' + resourceName);
//  sdTime := sdTime + (getTickCount - t);
//  if (element.getSpecial() = SpecialElement.BUNDLE_ENTRY) or (element.getSpecial() = SpecialElement.BUNDLE_OUTCOME) or (element.getSpecial() = SpecialElement.PARAMETER) then
//    resource := element;
//
//  if (rule(errors, itINVALID, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), profile <> nil, 'No profile found for contained resource of type_ ''' + resourceName + ''')) then
//     validateResource(hostContext.forContained(element), errors, resource, element, profile, nil, idstatus, stack, false);
//
end;

procedure TInstanceValidator.validateDocument(errors : TFslList<TFhirValidationMessage>; entries : TFslList<TFhirMMElement>; composition : TFhirMMElement; stack : TNodeStack; fullUrl : String; id : String);
//var
//  elem : TFhirMMElement;
begin
//  if (rule(errors, itINVALID, composition.LocationStart.line, composition.LocationStart.col, stack.getLiteralPath(), composition.type_= 'Composition'), 'The first entry in a document must be a composition')) then
//      elem :=  composition.getNamedChild('subject');
//      if (rule(errors, itINVALID, composition.LocationStart.line, composition.LocationStart.col, stack.getLiteralPath(), elem <> nil, 'A document composition must have a subject')) then
//         validateBundleReference(errors, entries, elem, 'Composition Subject', stack.push(elem, -1, nil, nil), fullUrl, 'Composition', id);
//
//       validateSections(errors, entries, composition, stack, fullUrl, id);
end;

procedure TInstanceValidator.validateElement(hostContext : TFhirValidatorHostContext; errors : TFslList<TFhirValidationMessage>; profile : TFhirStructureDefinition; definition : TFhirElementDefinition; cprofile : TFhirStructureDefinition; context : TFhirElementDefinition; resource : TFhirMMElement; element : TFhirMMElement; actualType : String; stack : TNodeStack; inCodeableConcept : boolean);
var
  ancestor : TNodeStack;
  childDefinitions : TFhirElementDefinitionList;
  dt : TFhirStructureDefinition;
  children : TFslList<TElementInfo>;
//  iter : TFhirChildIterator;
  slicer : TFhirElementDefinition;
  unsupportedSlicing : boolean;
  problematicPaths : TStringList;
  slicingPath : String;
  sliceOffset : integer;
  last : integer;
  lastSlice : integer;
  ei : TElementInfo;
  sliceInfo : String;
  isXmlAttr : boolean;
//  r : TFhirEnumeration<PropertyRepresentation>;
  ed : TFhirElementDefinition;
  count : integer;
  slices : TFhirElementDefinitionList;
  sed : TFhirElementDefinition;
  location : String;
  profiles : TStringList;
  type_ : String;
  typeDefn : TFhirElementDefinition;
  usesMustSupport : String;
  pe : TFhirElementDefinition;
  elementSupported : String;
  prefix : String;
  t : TFhirElementDefinitionType;
  trc : TFhirElementDefinitionType;
  localStack : TNodeStack;
  localStackLiterapPath : String;
  eiPath : String;
  thisIsCodeableConcept : boolean;
  p : TFhirStructureDefinition;
  elementValidated : boolean;
  goodProfiles : TFslMap<TFslList<TFHIRValidationMessage>>;
  badProfiles : TFslList<TFslList<TFhirValidationMessage>>;
  typeProfile : String;
  profileErrors : TFslList<TFhirValidationMessage>;
  hasError : boolean;
  msg : TFhirValidationMessage;
  messages : TFslList<TFhirValidationMessage>;
  index : integer;
  nextPath : String;
  iter : TFhirChildIterator;
  i : integer;
  childUnsupportedSlicing : boolean;
  process : boolean;
  errorContext : String;
  pu : TFhirCanonical;
  url, tl : String;

  function pdesc(profile : TFhirStructureDefinition) : string;
  begin
     if profile = nil then
       result := ''
     else
       result := ' for the profile ' + profile.url;
  end;
  function eDesc(ed : TFhirElementDefinition) : string;
  begin
    result := tail(ed.path);
    if ed.sliceName <> '' then
    begin
      result := result + '['+ed.sliceName;
      if ed.label_ <> '' then
        result := result + '(' +ed.label_+')';
      result := result + ']';
    end;
  end;
begin
  if (resource.Name = 'contained') then
  begin
    ancestor := stack;
    while (not ancestor.element.isResource()) or (ancestor.element.Name= 'contained') do
       ancestor := ancestor.parent;
    checkInvariants(hostContext, errors, stack.getLiteralPath(), profile, definition, '', '', ancestor.element, element);
  end
  else
    checkInvariants(hostContext, errors, stack.getLiteralPath(), profile, definition, '', '', resource, element);

  if (definition.fixed <> nil) then
     checkFixedValue(errors, stack.getLiteralPath(), element, definition.fixed, definition.sliceName, nil);

  childDefinitions := self.Context.getChildMap(profile, definition);
  try
    if (childDefinitions.Count = 0) then
    begin
      if (actualType = '') then
        exit();

      dt :=  self.context.fetchStructureDefinition('http://hl7.org/fhir/StructureDefinition/' + actualType);
      if (dt = nil) then
       raise EDefinitionException.create('Unable to resolve actual type_ ' + actualType);
      try
        childDefinitions := self.Context.getChildMap(dt, dt.snapshot.elementList[0]);
      finally
        dt.Free;
      end;
    end;

    // 1. List the children, and remember their exact path (convenience)
    children := TFslList<TElementInfo>.Create();
    problematicPaths := TStringList.Create();
    try
      iter := TFhirChildIterator.Create(stack.getLiteralPath, element);
      try
        while (iter.next()) do
          children.add(TElementInfo.create(iter.name, iter.element, iter.path, iter.count));
      finally
        iter.Free;
      end;

      // 2. assign children to a definition
      // for each definition, for each child, check whether it belongs in the slice
      slicer := nil;
      unsupportedSlicing := false;
      slicingPath := '';
      sliceOffset :=  0;
      for i := 0 to childDefinitions.count - 1 do
      begin
        ed := childDefinitions[i];
        childUnsupportedSlicing := false;
        process := true;
        if (ed.slicing <> nil) and (not ed.slicing.ordered) then
          slicingPath := ed.path
        else if (slicingPath <> '') and (ed.path = slicingPath) then
          // nothing
        else if (slicingPath <> '') and (not ed.path.startsWith(slicingPath)) then
          slicingPath := '';
        // where are we with slicing
        if (ed.slicing <> nil) then
        begin
          if (slicer <> nil) and (slicer.path = ed.path) then
          begin
            errorContext := 'profile ' + profile.url;
            if (not resource.getChildValue('id').isEmpty()) then
              errorContext := errorContext +'; instance ' + resource.getChildValue('id');
            raise EDefinitionException.create('Slice encountered midway through path on ' + slicer.path + '; ' + errorContext);
          end;
          slicer := ed;
          process := false;
          sliceOffset := i;
        end else if (slicer <> nil) and (not slicer.path.equals(ed.path)) then
          slicer := nil;

        // if (process) begin
        for ei in children do
          unsupportedSlicing := matchSlice(hostContext, errors, profile, stack, slicer, unsupportedSlicing, problematicPaths, sliceOffset, i, ed, childUnsupportedSlicing, ei);
      end;

      last :=  -1;
      lastSlice :=  -1;
      for ei in children do
      begin
        sliceInfo :=  '';
        if (slicer <> nil) then
          sliceInfo := ' (slice: ' + slicer.path + ')';

        if (not unsupportedSlicing) then
          if (ei.additionalSlice) and (ei.definition <> nil) then
          begin
            if (ei.definition.slicing.rules in [ResourceSlicingRulesOpen, ResourceSlicingRulesOpenAtEnd]) then
              hint(errors, itINFORMATIONAL, ei.LocationStart.line, ei.LocationStart.col, ei.path, false, 'This element does not match any known slice' + pDesc(profile))
            else if (ei.definition.slicing.rules = ResourceSlicingRulesClosed) then
              rule(errors, itINVALID, ei.LocationStart.line, ei.LocationStart.col, ei.path, false, 'This element does not match any known slice' + pDesc(profile) + ' and slicing is CLOSED')
          end
          else
          begin
             if (not profile.abstract) then
                hint(errors, itNOTSUPPORTED, ei.LocationStart.line, ei.LocationStart.col, ei.path, (ei.definition <> nil), 'Could not verify slice for profile ' + profile.url);
          end;

        // TODO: Should get the order of elements correct when parsing elements that are XML attributes vs. elements
        isXmlAttr := false;
        if (ei.definition <> nil) then
         isXmlAttr := PropertyRepresentationXmlAttr in ei.definition.representation;
        if (not profile.getExtensionBool('http://hl7.org/fhir/StructureDefinition/structuredefinition-xml-no-order')) then
          rule(errors, itINVALID, ei.LocationStart.line, ei.LocationStart.col, ei.path, (ei.definition = nil) or (ei.index >= last) or isXmlAttr, 'As specified by profile ' + profile.url + ', Element "' + ei.name + '" is out of order');

        if (ei.slice <> nil) and (ei.index = last) and (ei.slice.slicing.ordered) then
          rule(errors, itINVALID, ei.LocationStart.line, ei.LocationStart.col, ei.path, (ei.definition = nil) or (ei.sliceindex >= lastSlice) or isXmlAttr, 'As specified by profile ' + profile.url + ', Element "' + ei.name + '" is out of order in ordered slice');

        if (ei.definition = nil) or (not isXmlAttr) then
          last := ei.index;
        if (ei.slice <> nil) then
          lastSlice := ei.sliceindex
        else
          lastSlice := -1;
      end;

      // 3. report any definitions that have a cardinality problem
      for ed in childDefinitions do
      begin
        if (ed.representation = []) then
        begin
          count :=  0;
          slices := nil;
          if (ed.slicing <> nil) then
            slices := self.Context.getSliceList(profile, ed);
          try
            for ei in children do
            begin
              if (ei.definition = ed) then
                 inc(count)
              else if (slices <> nil) then
              begin
                for sed in slices do
                  if (ei.definition = sed) then
                  begin
                    inc(count);
                    break;
                  end;
              end;
            end;
            location := 'Profile ' + profile.url + ', Element ''' + stack.getLiteralPath() + '.' + eDesc(ed);
            if (ed.minInt <> 0) then
            begin
              if (problematicPaths.IndexOf(ed.path) > -1) then
                 hint(errors, itNOTSUPPORTED, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), count >= ed.minInt, location + ': Unable to check minimum required (' + ed.min + ') due to lack of slicing validation')
              else
                 rule(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), count >= ed.minInt, location + ': minimum required := ' + ed.min + ', but only found ' + Integer.toString(count));
            end;
            if (ed.maxElement <> nil) and (ed.max <> '*') then
            begin
              if (problematicPaths.IndexOf(ed.path) > -1) then
                 hint(errors, itNOTSUPPORTED, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), count <= ed.maxInt, location + ': Unable to check max allowed (' + ed.max + ') due to lack of slicing validation')
              else
                 rule(errors, itSTRUCTURE, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), count <= ed.maxInt, location + ': max allowed := ' + ed.max + ', but found ' + Integer.toString(count));
            end;
          finally
            slices.Free;
          end;
        end;
      end;
    finally
      problematicPaths.Free;
    end;

    // 5. inspect each child for validity
    for ei in children do
    begin
      profiles := TStringList.Create();
      try
        if (ei.definition <> nil) then
        begin
          type_ := '';
          typeDefn := nil;
          usesMustSupport :=  profile.Tags['usesMustSupport'];
          if (usesMustSupport = '') then
          begin
            usesMustSupport := 'N';
            for pe in profile.snapshot.elementList do
            begin
              if (pe.mustSupport) then
              begin
                usesMustSupport := 'Y';
                break;
              end;
            end;
            profile.tags['usesMustSupport'] := usesMustSupport;
          end;
          if (usesMustSupport= 'Y') then
          begin
            elementSupported := ei.element.tags['elementSupported'];
            if (elementSupported = '') or (ei.definition.mustSupport) then
              if (ei.definition.mustSupport) then
                ei.element.tags['elementSupported'] :=  'Y'
              else
                ei.element.tags['elementSupported'] := 'N';
          end;

          if (ei.definition.type_List.count = 1) and ('*' <> ei.definition.type_List[0].code) and ('Element' <> ei.definition.type_List[0].code) and ('BackboneElement' <> ei.definition.type_List[0].code) then
          begin
            type_ := ei.definition.type_List[0].code;
            if (ei.definition.type_List[0].hasProfileList) then
              profiles.add(ei.definition.type_List[0].profileList[0].value);
          end
          else if (ei.definition.type_List.count = 1) and (ei.definition.type_List[0].code = '*') then
          begin
            prefix := tail(ei.definition.path);
            assert(prefix.endsWith('[x]'));
            type_ := ei.name.substring(prefix.length - 3);
            if (isPrimitiveType(type_)) then
              type_ := uncapitalise(type_);

            if (ei.definition.type_List[0].hasProfileList) then
              profiles.add(ei.definition.type_List[0].profileList[0].value)
          end
          else if (ei.definition.type_List.count > 1) then
          begin
            prefix :=  tail(ei.definition.path);
            assert(typesAreAllReference(ei.definition.type_List) or (PropertyRepresentationTypeAttr in ei.definition.representation) or prefix.endsWith('[x]'), prefix);
            if (PropertyRepresentationTypeAttr in ei.definition.representation) then
              type_ := ei.element.type_
            else
            begin
              prefix := prefix.substring(0, prefix.length - 3);
              for t in ei.definition.type_List do
              begin
                if prefix + capitalise(t.code) = ei.name then
                begin
                  type_ := t.code;
                  if (t.hasProfileList) and (type_<> 'Reference') then
                    profiles.add(t.profileList[0].value);
                end;
              end;
            end;
            if (type_ = '') then
            begin
              trc :=  ei.definition.type_List[0];
              if (trc.code = 'Reference') then
                type_ := 'Reference'
              else
                rule(errors, itSTRUCTURE, ei.LocationStart.line, ei.LocationStart.col, stack.getLiteralPath(), false, 'The type_ of element ' + ei.name + ' is not known, which is illegal. Valid types at this point are ' + describeTypes(ei.definition.type_List));
            end
          end
          else if (ei.definition.contentReference <> '') then
            typeDefn := resolveNameReference(profile.snapshot, ei.definition.contentReference)
          else if (ei.definition.type_List.count = 1) and ('Element' = ei.definition.type_List[0].code) or ('BackboneElement' = ei.definition.type_List[0].code) then
          begin
            if (ei.definition.type_List[0].hasProfileList) then
            begin
              pu := ei.definition.type_List[0].profileList[0];
              if (pu.hasExtension('http://hl7.org/fhir/StructureDefinition/elementdefinition-profile-element')) then
                profiles.add(pu.value+'#'+pu.getExtensionString('http://hl7.org/fhir/StructureDefinition/elementdefinition-profile-element'))
              else
                profiles.add(pu.value);
            end;
          end;

          if (type_ <> '') then
          begin
            if (type_.startsWith('@')) then
            begin
              ei.definition := findElement(profile, type_.substring(1));
              type_ := '';
            end;
          end;

          if type_ = '' then
            localStack :=  stack.push(ei.element, ei.count, ei.definition, typeDefn)
          else
            localStack :=  stack.push(ei.element, ei.count, ei.definition, resolveType(type_, ei.definition.type_List));
          try
            localStackLiterapPath := localStack.getLiteralPath;
            eiPath := ei.path;
            assert(eiPath = localStackLiterapPath,  'ei.path: ' + ei.path + '  -  localStack.getLiteralPath: ' + localStackLiterapPath);
            thisIsCodeableConcept := false;
            ei.element.markValidation(profile, ei.definition);
            if (type_ <> '') then
            begin
              if (isPrimitiveType(type_)) then
                checkPrimitive(hostContext, errors, ei.path, type_, ei.definition, ei.element, profile);
              // else
               // checkNonPrimitive(appContext, errors, ei.path, type_, ei.definition, ei.element, profile);

              if (type_= 'Identifier') then
                checkIdentifier(errors, ei.path, ei.element, ei.definition)
              else if (type_= 'Coding') then
                checkCoding(errors, ei.path, ei.element, profile, ei.definition, inCodeableConcept)
              else if (type_= 'CodeableConcept') then
              begin
                checkCodeableConcept(errors, ei.path, ei.element, profile, ei.definition);
                thisIsCodeableConcept := true;
              end
              else if (type_= 'Reference') then
                checkReference(hostContext, errors, ei.path, ei.element, profile, ei.definition, actualType, localStack)
              // We only check extensions if we're not in a complex extension or if the element we're dealing with is not defined as part of that complex extension
              else if (type_= 'Extension') and (ei.element.getChildValue('url').contains('/')) then
                checkExtension(hostContext, errors, ei.path, resource, ei.element, ei.definition, profile, localStack)
              else if (type_= 'Resource') then
                validateContains(hostContext, errors, ei.path, ei.definition, definition, resource, ei.element, localStack, idStatusForEntry(element, ei));
                // (str.matches('.*([.,/])work\\1$'))
            end
            else if (rule(errors, itSTRUCTURE, ei.locationStart.line, ei.locationStart.col, stack.getLiteralPath(), ei.definition <> nil, 'Unrecognised Content ' + ei.name)) then
              validateElement(hostContext, errors, profile, ei.definition, nil, nil, resource, ei.element, type_, localStack, false);

            p := nil;
            elementValidated := false;
            if (profiles.Count = 0) then
            begin
              if type_ <> '' then
              begin
                p := getProfileForType(type_, ei.definition.type_List);
                try
                  // If dealing with a primitive type_, then we need to check the current child against
                  // the invariants (constraints) on the current element, because otherwise it only gets
                  // checked against the primary type_'s invariants: LLoyd
                  // if (p.getKind() = StructureDefinitionKind.PRIMITIVETYPE) begin
                  // checkInvariants(hostContext, errors, ei.path, profile, ei.definition, nil, nil, resource, ei.element);
                  // end;
                  rule(errors, itSTRUCTURE, ei.LocationStart.line, ei.LocationStart.col, ei.path, p <> nil, 'Unknown type_ ' + type_);
                finally
                  p.Free;
                end;
              end
            end
            else if (profiles.count = 1) then
            begin
              url := profiles[0];
              if (url.contains('#')) then
              begin
                tl := url.substring(url.indexOf('#')+1);
                url := url.substring(0, url.indexOf('#'));
              end;
              p := self.context.fetchStructureDefinition(profiles[0]);
              try
                rule(errors, itSTRUCTURE, ei.LocationStart.line, ei.LocationStart.col, ei.path, p <> nil, 'Unknown profile ' + profiles[0]);
              finally
                p.Free;
              end;
            end
            else
            begin
              elementValidated := true;
              goodProfiles := TFslMap<TFslList<TFHIRValidationMessage>>.Create('good.profiles');
              badProfiles := TFslList<TFslList<TFHIRValidationMessage>>.Create();
              try
                for typeProfile in profiles do
                begin
                  p := self.context.fetchStructureDefinition(typeProfile);
                  try
                    if (rule(errors, itSTRUCTURE, ei.LocationStart.line, ei.LocationStart.col, ei.path, p <> nil, 'Unknown profile ' + typeProfile)) then
                    begin
                      profileErrors := TFslList<TFhirValidationMessage>.Create();
                      try
                        validateElement(hostContext, profileErrors, p, p.snapshot.elementList[0], profile, ei.definition, resource, ei.element, type_, localStack, thisIsCodeableConcept);
                        hasError := false;
                        for msg in profileErrors do
                        begin
                          if (msg.level in [isFatal, isError]) then
                          begin
                            hasError := true;
                            break;
                          end;
                        end;
                        if (hasError) then
                          badProfiles.add(profileErrors.link)
                        else
                          goodProfiles.add(typeProfile, profileErrors.link);
                      finally
                        profileErrors.free;
                      end;
                    end;
                  finally
                    p.free;
                  end;
                end;
                if (goodProfiles.count = 1) then
                  errors.AddAll(goodProfiles[goodProfiles.SortedKeys[0]])
                else if (goodProfiles.count = 0) then
                begin
                  rule(errors, itSTRUCTURE, ei.LocationStart.line, ei.LocationStart.col, ei.path, false, 'Unable to find matching profile among choices: ' + profiles.ToString);
                  for messages in badProfiles do
                    errors.AddAll(messages);
                end
                else
                begin
                  warning(errors, itSTRUCTURE, ei.LocationStart.line, ei.LocationStart.col, ei.path, false, 'Found multiple matching profiles among choices: ' + goodProfiles.Keys.ToString);
                  for messages in goodProfiles.values do
                    errors.addAll(messages);
                end;
              finally
                goodProfiles.Free;
                badProfiles.Free;
              end;
            end;

            if (p <> nil) then
            begin
              if (not elementValidated) then
                validateElement(hostContext, errors, p, p.snapshot.elementList[0], profile, ei.definition, resource, ei.element, type_, localStack, thisIsCodeableConcept);
              index :=  profile.snapshot.elementList.indexOf(ei.definition);
              if (index < profile.snapshot.elementList.count - 1) then
              begin
                nextPath := profile.snapshot.elementList[index + 1].path;
                if (nextPath <> ei.definition.path) and (nextPath.startsWith(ei.definition.path)) then
                  validateElement(hostContext, errors, profile, ei.definition, nil, nil, resource, ei.element, type_, localStack, thisIsCodeableConcept);
              end;
            end;
          finally
            localStack.Free;
          end;
        end;
      finally
        profiles.free;
      end;
    end;
  finally
    childDefinitions.Free;
  end;
end;

function TInstanceValidator.idStatusForEntry(ep : TFhirMMElement; ei : TElementInfo) : TFhirIdStatus;
var
  req : TFhirMMElement;
  resp : TFhirMMElement;
  fullUrl : TFhirMMElement;
  method : TFhirMMElement;
  url : TFhirMMElement;
  s : String;
begin
  if (isBundleEntry(ei.path)) then
  begin
    req :=  ep.getNamedChild('request');
    resp :=  ep.getNamedChild('response');
    fullUrl :=  ep.getNamedChild('fullUrl');
    method := nil;
    url := nil;
    if (req <> nil) then
    begin
      method := req.getNamedChild('method');
      url := req.getNamedChild('url');
    end;

    if (resp <> nil) then
      exit(risOPTIONAL);

    if (method = nil) then
    begin
      if (fullUrl = nil) then
        exit(risREQUIRED)
      else if (fullUrl.primitiveValue.startsWith('urn:uuid:')) or (fullUrl.primitiveValue.startsWith('urn:oid:')) then
        exit(risOPTIONAL)
      else
        exit(risREQUIRED);
    end
    else
    begin
      s := method.primitiveValue;
      if (s = 'PUT') then
      begin
        if (url = nil) then
          exit(risREQUIRED)
        else
          exit(risOPTIONAL);
      end
      else if (s= 'POST') then
        exit(risOPTIONAL)
      else
        exit(risOPTIONAL);
    end
  end
  else if (isParametersEntry(ei.path)) or (isBundleOutcome(ei.path)) then
    exit(risOPTIONAL)
  else
    exit(risREQUIRED);
end;

procedure TInstanceValidator.checkInvariants(hostContext : TFhirValidatorHostContext; errors : TFslList<TFhirValidationMessage>; path : String; profile : TFhirStructureDefinition; ed : TFhirElementDefinition; typename : String; typeProfile : String; resource : TFhirMMElement; element : TFhirMMElement);
var
  inv : TFhirElementDefinitionConstraint;
begin
  for inv in ed.constraintList do
  begin
    if (inv.expression <> '') then
      checkInvariant(hostContext, errors, path, profile, resource, element, inv);
  end;
end;

procedure TInstanceValidator.validateMessage(errors : TFslList<TFhirValidationMessage>; entries : TFslList<TFhirMMElement>; messageHeader : TFhirMMElement; stack : TNodeStack; fullUrl : String; id : String);
//var
//  elements : TFslList<TFhirMMElement>;
//  elem : TFhirMMElement;
begin
//  if (rule(errors, itINVALID, messageHeader.LocationStart.line, messageHeader.LocationStart.col, stack.getLiteralPath(), messageHeader.type_= 'MessageHeader', 'The first entry in a message must be a MessageHeader')) then
//  begin
//      elements :=  messageHeader.getChildren('data');
//      for elem in elements do
//      begin
//         validateBundleReference(errors, entries, elem, 'MessageHeader Data', stack.push(elem, -1, nil, nil), fullUrl, 'MessageHeader', id);
//      end;
//
end;

procedure TInstanceValidator.validateObservation(errors : TFslList<TFhirValidationMessage>; element : TFhirMMElement; stack : TNodeStack);
//var
//  performers : TFslList<TFhirMMElement>;
begin
//   bpCheck(errors, itINVALID, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), element.getNamedChild('subject') <> nil, 'All observations should have a subject');
//  performers := TFslList<TFhirMMElement>.Create();
//   element.getNamedChildren('performer', performers);
//   bpCheck(errors, itINVALID, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), performers.count > 0, 'All observations should have a performer');
//   bpCheck(errors, itINVALID, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), element.getNamedChild('effectiveDateTime') <> nil) or (element.getNamedChild('effectivePeriod') <> nil, 'All observations should have an effectiveDateTime or an effectivePeriod');
//// all observations should have a subject, a performer, and a time
//
end;

procedure TInstanceValidator.validateResource(hostContext : TFhirValidatorHostContext; errors : TFslList<TFhirValidationMessage>; resource : TFhirMMElement; element : TFhirMMElement; defn : TFhirStructureDefinition; profiles : TValidationProfileSet; idstatus : TFhirIdStatus; stack : TNodeStack; isEntry : boolean);
var
  ok : boolean;
  resourceName : String;
  t : cardinal;
  type_ : String;
  first : TFhirMMElement;
begin
  assert(stack <> nil);
  assert(resource <> nil);
  ok := true;
  resourceName := element.type_;
  if (defn = nil) then
  begin
    t := getTickCount;
    defn := element.Prop.Structure;
    if (defn = nil) then
      defn := context.fetchStructureDefinition('http://hl7.org/fhir/StructureDefinition/' + resourceName)
    else
     defn.Link;
    if (profiles <> nil) then
      getResourceProfiles(resource, stack).addProfiles(errors, profiles, stack.getLiteralPath(), element, isEntry);
    sdTime := sdTime + (getTickCount - t);
    ok := rule(errors, itINVALID, element.LocationStart.line, element.LocationStart.col, stack.addToLiteralPath(resourceName), defn <> nil, 'No definition found for resource type_ ''' + resourceName + '''');
  end
  else
    defn.link;
  try
    if defn.kind = StructureDefinitionKindLogical then
      type_ := defn.id
    else
      type_ := defn.type_;
    if (type_ <> resourceName) and (resourceName = 'Bundle') then
    begin
      first := getFirstEntry(element);
      if (first <> nil) and (first.type_ = type_) then
      begin
        element := first;
        resourceName := element.type_;
        idstatus := risOPTIONAL;
      end;
    end;
    ok := rule(errors, itINVALID, -1, -1, stack.getLiteralPath(), type_.equals(resourceName), 'Specified profile type_ was ''' + type_ + ''', but found type_ ''' + resourceName + '''');
    if (ok) then
      if (idstatus = risREQUIRED) and ((element.getNamedChild('id') = nil)) then
        rule(errors, itINVALID, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), false, 'Resource requires an id, but none is present')
      else if (idstatus = risPROHIBITED) and ((element.getNamedChild('id') <> nil)) then
        rule(errors, itINVALID, element.LocationStart.line, element.LocationStart.col, stack.getLiteralPath(), false, 'Resource has an id, but none is allowed');
    start(hostContext, errors, resource, element, defn, stack);
  finally
    defn.free;
  end;
end;

procedure TInstanceValidator.loadProfiles(profiles : TValidationProfileSet);
var
  profile : String;
  p : TFhirStructureDefinition;
begin
  if (profiles <> nil) then
  begin
    for profile in profiles.canonicalUrls do
    begin
      p := context.fetchStructureDefinition(profile);
      if (p = nil) then
        raise EDefinitionException.create('StructureDefinition ''' + profile + ''' not found');
      profiles.definitions.add(p);
    end;
  end;
end;

function TInstanceValidator.getFirstEntry(bundle : TFhirMMElement) : TFhirMMElement;
var
  list : TFslList<TFhirMMElement>;
  resource : TFhirMMElement;
begin
  list := TFslList<TFhirMMElement>.Create();
  try
    bundle.getNamedChildren('entry', list);
    if (list.Count = 0) then
      exit(nil);
    result := list[0].getNamedChild('resource');
  finally
    list.Free;
  end;
end;

procedure TInstanceValidator.validateSections(errors : TFslList<TFhirValidationMessage>; entries : TFslList<TFhirMMElement>; focus : TFhirMMElement; stack : TNodeStack; fullUrl : String; id : String);
var
  sections : TFslList<TFhirMMElement>;
  i : integer;
  section : TFhirMMElement;
  localStack : TNodeStack;
begin
  sections := TFslList<TFhirMMElement>.Create();
  try
    focus.getNamedChildren('entry', sections);
    i :=  0;
    for section in sections do
    begin
      localStack :=  stack.push(section, 1, nil, nil);
      try
        validateBundleReference(errors, entries, section.getNamedChild('content'), 'Section Content', localStack, fullUrl, 'Composition', id);
        validateSections(errors, entries, section, localStack, fullUrl, id);
        inc(i);
      finally
        localStack.Free;
      end;
    end;
  finally
    sections.Free;
  end;
end;

function TInstanceValidator.valueMatchesCriteria(value : TFhirMMElement; criteria : TFhirElementDefinition) : boolean;
var
  msgs : TFslList<TFhirValidationMessage>;
begin
  if (criteria.fixed <> nil) then
  begin
    msgs := TFslList<TFhirValidationMessage>.Create();
    try
      checkFixedValue(msgs, 'beginvirtualend;', value, criteria.fixed, 'value', nil);
      exit(msgs.count = 0);
    finally
      msgs.free;
    end;
  end
  else if (criteria.binding <> nil) and (criteria.binding.strength = BindingStrengthREQUIRED) and (criteria.binding.valueSet <> '') then
    raise EFHIRException.create('Unable to resolve slice matching - slice matching by value set not done')
  else
    raise EFHIRException.create('Unable to resolve slice matching - no fixed value or required value set');
end;

function TInstanceValidator.warning(errors: TFslList<TFhirValidationMessage>;
  invalid: TFhirIssueType; line, col: integer; literalPath: String;
  test: boolean; message: String): boolean;
begin
   raise Exception.Create('Error Message');
end;

function TInstanceValidator.yearIsValid(v : String) : boolean;
var
  i : integer;
begin
  if (v = '') then
    exit(false);

  i := StrToIntDef(copy(v, 1, 4), -1);
  result := (i >= 1800) and (i <= 2100);
end;

function TInstanceValidator.isNoExtensibleWarnings() : boolean;
begin
  exit(noExtensibleWarnings);
end;

function TInstanceValidator.setNoExtensibleWarnings(noExtensibleWarnings : boolean) : TInstanceValidator;
begin
  self.noExtensibleWarnings := noExtensibleWarnings;
  result := self;
end;

function TInstanceValidator.isNoInvariantChecks() : boolean;
begin
//  exit(NoInvariantChecks);
  result := false;
end;

function TInstanceValidator.setNoInvariantChecks(value : boolean) : TInstanceValidator;
begin
//  self.noInvariantChecks := value;
  result := self;
end;

function TInstanceValidator.getFetcher() : TFhirIValidatorResourceFetcher;
begin
//  exit(self.fetcher);
  result := nil;
end;

function TInstanceValidator.setFetcher(value : TFhirIValidatorResourceFetcher) : TInstanceValidator;
begin
//  self.fetcher := value;
  result := self;
end;

function TInstanceValidator.isHintAboutNonMustSupport() : boolean;
begin
//  exit(FhintAboutNonMustSupport);
  result := false;
end;

procedure TInstanceValidator.setHintAboutNonMustSupport(hintAboutNonMustSupport : boolean);
begin
//  self.hintAboutNonMustSupport := hintAboutNonMustSupport;
end;

function TInstanceValidator.validate(appContext : TFhirObject; errors : TFslList<TFhirValidationMessage>; stream : TStream; format : TFhirFormat) : TFHIRMMElement;
var
  ps : TValidationProfileSet;
begin
  ps := TValidationProfileSet.Create;
  try
  result := nil;
//    result := validate(appContext, errors, stream, format, ps);
  finally
    ps.free;
  end;
end;

function TInstanceValidator.validate(appContext : TFhirObject; errors : TFslList<TFhirValidationMessage>; stream : TStream; format : TFhirFormat; profile : String) : TFHIRMMElement;
var
  ps : TValidationProfileSet;
begin
  ps := TValidationProfileSet.Create(profile);
  try
  result := nil;
//    result := validate(appContext, errors, stream, format, ps);
  finally
    ps.free;
  end;
end;

function TInstanceValidator.getBestPracticeWarningLevel() : TFhirBestPracticeWarningLevel;
begin
  //exit(bpWarnings);
  result := xx;
end;

function TInstanceValidator.getCheckDisplay() : TFhirCheckDisplayOption;
begin
//exit(checkDisplay);
  result := yyy;
end;

function TInstanceValidator.getContext() : TFHIRWorkerContext;
begin
exit(context);
end;

function TInstanceValidator.getExtensionDomains() : TStringList;
begin
exit(extensionDomains);
end;

function TInstanceValidator.isAnyExtensionsAllowed() : boolean;
begin
exit(anyExtensionsAllowed);
end;

function TInstanceValidator.isErrorForUnknownProfiles() : boolean;
begin
//exit(errorForUnknownProfiles);
  result := false;
end;

procedure TInstanceValidator.setErrorForUnknownProfiles(errorForUnknownProfiles : boolean);
begin
//  self.errorForUnknownProfiles := errorForUnknownProfiles;
end;

function TInstanceValidator.isSuppressLoincSnomedMessages() : boolean;
begin
//exit(suppressLoincSnomedMessages);
  result := false;
end;

procedure TInstanceValidator.setAnyExtensionsAllowed(anyExtensionsAllowed : boolean);
begin
  self.anyExtensionsAllowed := anyExtensionsAllowed;
end;

function TInstanceValidator.setBestPracticeWarningLevel(value : TFhirBestPracticeWarningLevel) : TInstanceValidator;
begin
//  bpWarnings := value;
  result := self;
end;

procedure TInstanceValidator.setCheckDisplay(checkDisplay : TFhirCheckDisplayOption);
begin
//  self.checkDisplay := checkDisplay;
end;

procedure TInstanceValidator.setSuppressLoincSnomedMessages(suppressLoincSnomedMessages : boolean);
begin
//  self.suppressLoincSnomedMessages := suppressLoincSnomedMessages;
end;

function TInstanceValidator.getResourceIdRule() : TFhirIdStatus;
begin
  //exit(resourceIdRule);
  result := risOptional;
end;

procedure TInstanceValidator.setResourceIdRule(resourceIdRule : TFhirIdStatus);
begin
//  self.resourceIdRule := resourceIdRule;
end;

function TInstanceValidator.isAllowXsiLocation() : boolean;
begin
//exit(allowXsiLocation);
  result := false;
end;

procedure TInstanceValidator.setAllowXsiLocation(allowXsiLocation : boolean);
begin
//  self.allowXsiLocation := allowXsiLocation;
end;

procedure TInstanceValidator.checkInvariant(hostContext : TFhirValidatorHostContext; errors : TFslList<TFhirValidationMessage>; path : String; profile : TFhirStructureDefinition; resource : TFhirMMElement; element : TFhirMMElement; inv : TFhirElementDefinitionConstraint);
var
  n : TFhirPathExpressionNode;
  t : cardinal;
  msg : String;
  ok : boolean;
begin
  n := inv.Tag as TFHIRPathExpressionNode;
  t := getTickCount;
  try
    n := fpe.parse(inv.expression);
  except
    on e : exception do
      raise EDefinitionException.create('Problem processing expression ' + inv.expression + ' in profile ' + profile.url + ' path ' + path + ': ' + e.Message);
  end;
  fpeTime := fpeTime + (getTickCount - t);
  inv.Tag := n;

  msg := '';
  ok := false;
  try
    t := getTickCount;
    ok := fpe.evaluateToBoolean(hostContext, resource, element, n);
    fpeTime := fpeTime + (getTickCount - t);
    msg := fpe.UseLog;
  except
    on ex : exception do
    begin
      ok := false;
      msg := ex.Message;
    end;
  end;
  if (not ok) then
  begin
    if (msg <> '') then
      msg := ' (' + msg + ')';

    if (inv.getExtensionBoolean('http://hl7.org/fhir/StructureDefinition/elementdefinition-bestpractice')) then
    begin
      case bpWarnings of
        bpError: rule(errors, itINVARIANT, element.LocationStart.line, element.LocationStart.col, path, ok, inv.human + msg + ' [' + inv.expression + ']');
        bpWarning: warning(errors, itINVARIANT, element.LocationStart.line, element.LocationStart.col, path, ok, inv.human + msg + ' [' + inv.expression + ']');
      else // bpNull, bpHint:
        hint(errors, itINVARIANT, element.LocationStart.line, element.LocationStart.col, path, ok, inv.human + msg + ' [' + inv.expression + ']');
      end;
    end;
    if (inv.Severity = ConstraintSeverityError) then
      rule(errors, itINVARIANT, element.LocationStart.line, element.LocationStart.col, path, ok, inv.human + msg + ' [' + inv.expression + ']')
    else if (inv.Severity = ConstraintSeverityWarning) then
      warning(errors, itINVARIANT, element.LocationStart.line, element.LocationStart.line, path, ok, inv.human + msg + ' [' + inv.expression + ']');
  end;
end;

function TInstanceValidator.reportTimes() : String;
var
  s : String;
begin
  s :=  String.format('Times: overall := %d, tx := %d, sd := %d, load := %d, fpe := %d', [overall, txTime, sdTime, loadTime, fpeTime]);
  overall := 0;
  txTime := 0;
  sdTime := 0;
  loadTime := 0;
  fpeTime := 0;
exit(s);
end;

function TInstanceValidator.isNoBindingMsgSuppressed() : boolean;
begin
exit(noBindingMsgSuppressed);
end;

function TInstanceValidator.setNoBindingMsgSuppressed(noBindingMsgSuppressed : boolean) : TInstanceValidator;
begin
  self.noBindingMsgSuppressed := noBindingMsgSuppressed;
  result := self;
end;

function TInstanceValidator.isNoTerminologyChecks() : boolean;
begin
  exit(noTerminologyChecks);
end;

function TInstanceValidator.setNoTerminologyChecks(noTerminologyChecks : boolean) : TInstanceValidator;
begin
  self.noTerminologyChecks := noTerminologyChecks;
  result := self;
end;

procedure TInstanceValidator.checkAllInvariants();
//var
//  sd : TFhirStructureDefinition;
//  ed : TFhirElementDefinition;
//  inv : TFhirElementDefinitionConstraint;
begin
//  for sd in context.allStructures() do
//  begin
//      if (sd.getDerivation() = TypeDerivationRule.SPECIALIZATION) then
//          for ed in sd.snapshot.element do
//          begin
//              for inv in ed.constraint do
//              begin
//                  if (inv.expression <> nil) then
// try begin
//    ExpressionNode n := (ExpressionNode) inv.getUserData('validator.expression.cache');
//    if (n = nil) begin
//        n := fpe.parse(inv.expression);
//        inv.setUserData('validator.expression.cache', n);
//    end;
//    fpe.check(nil, sd.getKind() = StructureDefinitionKind.RESOURCE ? sd.type_ : 'DomainResource', ed.path, n);
//end; catch (Exception e) begin
//    System.out.println('Error processing structure [' + sd.id + '] path ' + ed.path + ':' + inv.getKey() + ' (\''' + inv.expression + '\'): ' + e.Message);
//end;
//
//              end;
//          end;
//
//  end;
end;

(*
//          unsupportedSlicing = matchSlice(hostContext, errors, profile, stack, slicer, unsupportedSlicing, problematicPaths, sliceOffset, i, ed, childUnsupportedSlicing, ei);

        begin
        boolean match := false;
        if (slicer = nil) or (slicer = ed) begin
            match := nameMatches(ei.name, tail(ed.path));
        end; else begin
            // ei.slice := slice;
            if (nameMatches(ei.name, tail(ed.path)))
                try begin
                    match := sliceMatches(hostContext, ei.element, ei.path, slicer, ed, profile, errors, stack);
                    if (match) begin
                        ei.slice := slicer;
                        // Since a defined slice was found, this is not an additional (undefined) slice.
                        ei.additionalSlice := false;
                    end; else if (ei.slice = nil) begin
                        // if the specified slice is undefined, keep track of the fact this is an additional (undefined) slice, but only if a slice wasn't found previously
                        ei.additionalSlice := true;
                    end;
                end; catch (FHIRException e) begin
                    rule(errors, itPROCESSING, ei.LocationStart.line, ei.LocationStart.col, ei.path, false, e.Message);
                    unsupportedSlicing := true;
                    childUnsupportedSlicing := true;
                end;
        end;
        if (match) begin
            boolean isOk := ei.definition = nil) or (ei.definition = slicer;
            if (rule(errors, itINVALID, ei.LocationStart.line, ei.LocationStart.col, ei.path, isOk, 'Profile ' + profile.url + ', Element matches more than one slice - ' + (ei.definition = nil) or (not ei.definition.hasSliceName() ? '' : ei.definition.getSliceName()) + ', ' + (ed.hasSliceName() ? ed.getSliceName() : ''))) begin
                ei.definition := ed;
                if (ei.slice = nil) begin
                    ei.index := i;
                end; else begin
                    ei.index := sliceOffset;
                    ei.sliceindex := i - (sliceOffset + 1);
                end;
            end;
        end; else if (childUnsupportedSlicing) begin
            problematicPaths.add(ed.path);
        end;
    end;
// end;
end;
*)

{ TFHIRRferenceRefetcher }

function TFHIRRferenceRefetcher.fetch(context: TObject;
  ref: String): TFhirMMElement;
begin
  result := nil;
end;

function TFHIRRferenceRefetcher.validationPolicy(context: TObject; path,
  ref: String): TFhirReferenceValidationPolicy;
begin
  result := rvpIGNORE;
end;

{ TFhirChildIterator }

constructor TFhirChildIterator.Create(path: String; element: TFHIRMMElement);
begin
  inherited create;
end;

function TFhirChildIterator.getCount: integer;
begin
  result := 0;
end;

function TFhirChildIterator.getElement: TFHIRMMElement;
begin
  result := nil;
end;

function TFhirChildIterator.getName: string;
begin
  result := '';
end;

function TFhirChildIterator.getPath: string;
begin
  result := '';
end;

function TFhirChildIterator.next: boolean;
begin
  result := false;
end;

procedure TInstanceValidator.checkMaxValueSet(
  errors: TFslList<TFhirValidationMessage>; path: String;
  element: TFhirMMElement; profile: TFhirStructureDefinition; maxVSUrl,
  c: String);
begin
   raise Exception.Create('Error Message');

end;

end.

