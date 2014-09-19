unit QuestionnaireBuilder;

interface

uses
  SysUtils, Classes, Generics.Collections,
  GUIDSupport, DateAndTime, AdvObjects, ShellSupport, StringSupport, AdvStringMatches,
  FHIRResources, FHIRComponents, FHIRTypes, FHIRConstants, FHIRBase, FHIRAtomFeed, FHIRParser,
  FHIRUtilities, FHIRSupport, ProfileManager;

Const
  TYPE_EXTENSION = 'http://www.healthintersections.com.au/fhir/Profile/metadata#type';
  TYPE_REFERENCE = 'http://www.healthintersections.com.au/fhir/Profile/metadata#reference';
  FLYOVER_REFERENCE = 'http://hl7.org/fhir/Profile/questionnaire-extensions#flyover';
  EXTENSION_FILTER_ONLY = 'http://www.healthintersections.com.au/fhir/Profile/metadata#expandNeedsFilter';
  MaxListboxCodings = 20;


Type
  TResourceWithReference = class (TAdvObject)
  private
    FReference: String;
    FResource: TFHIRResource;
    procedure SetResource(const Value: TFHIRResource);
  public
    Constructor Create(reference : String; resource : TFHIRResource);
    Destructor Destroy; override;
    property Reference : String read FReference write FReference;
    property Resource : TFHIRResource read FResource write SetResource;
  end;


  TGetValueSetExpansion = function(vs : TFHIRValueSet; ref : TFhirResourceReference; limit : integer; allowIncomplete : Boolean; dependencies : TStringList) : TFhirValueSet of object;
  TLookupCodeEvent = function(system, code : String) : String of object;
  TLookupReferenceEvent = function(Context : TFHIRRequest; uri : String) : TResourceWithReference of object;

  {
 * This class takes a profile, and builds a questionnaire from it
 *
 * If you then convert this questionnaire to a form using the
 * XMLTools form builder, and then take the QuestionnaireAnswers
 * this creates, you can use QuestionnaireInstanceConvert to
 * build an instance the conforms to the profile
 *
 * FHIR context:
 *   conceptLocator, codeSystems, valueSets, maps, client, profiles
 * You don't have to provide any of these, but
 * the more you provide, the better the conversion will be
 *
 * @author Grahame
  }
  TQuestionnaireBuilder = class (TAdvObject)
  private
    FProfiles : TProfileManager;
    lastid : integer;
    FResource: TFhirResource;
    FStructure: TFhirProfileStructure;
    FProfile: TfhirProfile;
    FQuestionnaire: TFhirQuestionnaire;
    FAnswers: TFhirQuestionnaireAnswers;
    FQuestionnaireId: String;
    FFactory : TFHIRFactory;
    FOnExpand : TGetValueSetExpansion;
    vsCache : TAdvStringMatch;
    FPrebuiltQuestionnaire: TFhirQuestionnaire;
    FOnLookupCode : TLookupCodeEvent;
    FOnLookupReference : TLookupReferenceEvent;
    FContext : TFHIRRequest;
    FDependencies: TList<String>;

    function nextId(prefix : string) : String;

    function getChildList(structure :TFHIRProfileStructure; path : String) : TFhirProfileStructureSnapshotElementList; overload;
    function getChildList(structure :TFHIRProfileStructure; element : TFhirProfileStructureSnapshotElement) : TFhirProfileStructureSnapshotElementList; overload;
    function isExempt(element, child: TFhirProfileStructureSnapshotElement) : boolean;

    function getSystemForCode(vs : TFHIRValueSet; code : String; path : String) : String;
    function resolveValueSet(profile : TFHIRProfile; binding : TFhirProfileStructureSnapshotElementDefinitionBinding) : TFHIRValueSet; overload;
    function resolveValueSet(url : String) : TFHIRValueSet; overload;
    function makeAnyValueSet : TFhirValueSet;

    function expandTypeList(types: TFhirProfileStructureSnapshotElementDefinitionTypeList): TFhirProfileStructureSnapshotElementDefinitionTypeList;
    function makeTypeList(profile : TFHIRProfile; types : TFhirProfileStructureSnapshotElementDefinitionTypeList; path : String) : TFHIRValueSet;
    function convertType(v: TFhirElement; t: string; path : String): TFhirElement; overload;
    function convertType(value : TFHIRObject; af : TFhirAnswerFormat; vs : TFHIRValueSet; path : String) : TFhirType; overload;
    procedure selectTypes(profile : TFHIRProfile; sub : TFHIRQuestionnaireGroup; t: TFhirProfileStructureSnapshotElementDefinitionType; source, dest: TFhirQuestionnaireAnswersGroupList);
    function instanceOf(t : TFhirProfileStructureSnapshotElementDefinitionType; obj : TFHIRObject) : boolean;

    function addQuestion(group: TFHIRQuestionnaireGroup; af: TFhirAnswerFormat; path, id, name: String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList; vs : TFHIRValueSet = nil): TFhirQuestionnaireGroupQuestion;

    procedure addAddressQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addAgeQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addAttachmentQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addBinaryQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addBooleanQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addCodeQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addCodeableConceptQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addCodingQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addContactQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addDateTimeQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addDecimalQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addDurationQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addExtensionQuestions(profile : TFHIRProfile; group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; profileURL : String; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addHumanNameQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addIdRefQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addIdentifierQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addInstantQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addIntegerQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addPeriodQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addQuantityQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addRangeQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addRatioQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addReferenceQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; profileURL : String; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addSampledDataQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addScheduleQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addStringQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addTimeQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure addUriQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);

    function processAnswerGroup(group : TFhirQuestionnaireAnswersGroup; context : TFhirElement; defn :  TProfileDefinition) : boolean;

    procedure processDataType(profile : TFHIRProfile; group: TFHIRQuestionnaireGroup; element: TFhirProfileStructureSnapshotElement; path: String; t: TFhirProfileStructureSnapshotElementDefinitionType; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure buildQuestion(group : TFHIRQuestionnaireGroup; profile : TFHIRProfile; structure :TFHIRProfileStructure; element : TFhirProfileStructureSnapshotElement; path : String; answerGroups : TFhirQuestionnaireAnswersGroupList);
	  procedure buildGroup(group : TFHIRQuestionnaireGroup; profile : TFHIRProfile; structure :TFHIRProfileStructure; element : TFhirProfileStructureSnapshotElement; parents : TFhirProfileStructureSnapshotElementList; answerGroups : TFhirQuestionnaireAnswersGroupList);
    procedure processMetadata;
    procedure SetProfiles(const Value: TProfileManager);
    procedure SetProfile(const Value: TFhirProfile);
    procedure SetResource(const Value: TFhirResource);
    procedure SetStructure(const Value: TFhirProfileStructure);
    procedure processExisting(path : String; answerGroups, nAnswers: TFhirQuestionnaireAnswersGroupList);
    procedure SetAnswers(const Value: TFhirQuestionnaireAnswers);
    procedure SetPrebuiltQuestionnaire(const Value: TFhirQuestionnaire);
    procedure SetContext(const Value: TFHIRRequest);
  public
    Constructor create; override;
    Destructor Destroy; override;

    Property Profiles : TProfileManager read FProfiles write SetProfiles;
    Property OnExpand : TGetValueSetExpansion read FOnExpand write FOnExpand;
    Property onLookupCode : TLookupCodeEvent read FonLookupCode write FonLookupCode;
    Property onLookupReference : TLookupReferenceEvent read FonLookupReference write FonLookupReference;
    Property Context : TFHIRRequest read FContext write SetContext;

    Property Profile : TFhirProfile read FProfile write SetProfile;
    Property Structure : TFhirProfileStructure read FStructure write SetStructure;
    Property Resource : TFhirResource read FResource write SetResource;
    Property Questionnaire : TFhirQuestionnaire read FQuestionnaire;
    Property Answers : TFhirQuestionnaireAnswers read FAnswers write SetAnswers;
    Property QuestionnaireId : String read FQuestionnaireId write FQuestionnaireId;
    Property Dependencies : TList<String> read FDependencies;

    // sometimes, when this is used, the questionnaire is already build and cached, and we are
    // processing the answers. for technical reasons, we still go through the process, but
    // we don't do the intensive parts of the work (save time)
    Property PrebuiltQuestionnaire : TFhirQuestionnaire read FPrebuiltQuestionnaire write SetPrebuiltQuestionnaire;

    procedure Build;
    procedure UnBuild;
  end;

implementation

Uses
  NarrativeGenerator;

Function tail(path : String) : String;
begin
  result := path.substring(path.lastIndexOf('.')+1);
end;

{ TResourceWithReference }

constructor TResourceWithReference.Create(reference: String; resource: TFHIRResource);
begin
  inherited Create;
  self.Reference := reference;
  self.Resource := resource;

end;

destructor TResourceWithReference.Destroy;
begin
  FResource.free;
  inherited;
end;

procedure TResourceWithReference.SetResource(const Value: TFHIRResource);
begin
  FResource.free;
  FResource := Value;
end;

{ TQuestionnaireBuilder }

procedure TQuestionnaireBuilder.build;
var
  entry : TFhirProfileStructure;
  i : integer;
  list : TFhirProfileStructureSnapshotElementList;
  answerGroups : TFhirQuestionnaireAnswersGroupList;
  group : TFhirQuestionnaireGroup;
begin
  if profile = nil then
    raise Exception.Create('QuestionnaireBuilder.build: No Profile provided');

  if (profile.structureList.IsEmpty) then
		raise Exception.create('QuestionnaireBuilder.build: no structure found');

  if Structure = nil then
  begin
    for i := 0 to profile.structureList.Count - 1 do
      if profile.structureList[i].publishST then
        if structure = nil then
          structure := profile.structureList[i].Link
        else
          raise Exception.create('buildQuestionnaire: if there is more than one published structure in the profile, you must choose one');
    if (structure = nil) then
      raise Exception.create('buildQuestionnaire: no published structure found');
  end;

  if (not profile.structureList.ExistsByReference(structure)) then
 		raise Exception.create('buildQuestionnaire: profile/structure mismatch');

  if resource <> nil then
    if Structure.type_ST <> CODES_TFhirResourceType[resource.ResourceType] then
      raise Exception.Create('Wrong Type');

  if FPrebuiltQuestionnaire <> nil then
    FQuestionnaire := FPrebuiltQuestionnaire.Link
  else
    FQuestionnaire := TFHIRQuestionnaire.Create();
  if resource <> nil then
    FAnswers := TFhirQuestionnaireAnswers.Create;
  processMetadata;


  list := TFhirProfileStructureSnapshotElementList.Create;
  answerGroups := TFhirQuestionnaireAnswersGroupList.create;
  try
    if resource <> nil then
      answerGroups.Add(FAnswers.group.Link);
    if PrebuiltQuestionnaire <> nil then
    begin
     // give it a fake group to build
     group := TFhirQuestionnaireGroup.Create;
     try
      buildGroup(group, profile, structure, structure.snapshot.elementList[0], list, answerGroups);
     finally
       group.free;
     end;
    end
    else
      buildGroup(FQuestionnaire.group, profile, structure, structure.snapshot.elementList[0], list, answerGroups);
  finally
    list.Free;
    answerGroups.Free;
  end;

  if FAnswers <> nil then
    FAnswers.collapseAllContained;
end;

procedure TQuestionnaireBuilder.processExisting(path : String; answerGroups, nAnswers: TFhirQuestionnaireAnswersGroupList);
var
  k: Integer;
  j: Integer;
  ans: TFhirQuestionnaireAnswersGroup;
  children: TFHIRObjectList;
begin
  // processing existing data
  for j := 0 to answerGroups.Count - 1 do
  begin
    children := TFHIRObjectList.Create;
    try
      TFHIRObject(answerGroups[j].tag).ListChildrenByName(tail(path), children);
      for k := 0 to children.Count - 1 do
        if children[k] <> nil then
        begin
          ans := answerGroups[j].groupList.Append;
          ans.Tag := children[k].Link;
          nAnswers.add(ans.link);
        end;
    finally
      children.Free;
    end;
  end;
end;

destructor TQuestionnaireBuilder.Destroy;
begin
  FDependencies.Free;
  vsCache.Free;
  FResource.Free;
  FStructure.Free;
  FProfile.Free;
  FQuestionnaire.Free;
  FAnswers.Free;
  FProfiles.Free;
  FPrebuiltQuestionnaire.Free;
  FContext.free;
  inherited;
end;

function convertStatus(status : TFhirResourceProfileStatus) : TFHIRQuestionnaireStatus;
begin
  case (status) of
		ResourceProfileStatusActive: result := QuestionnaireStatusPublished;
		ResourceProfileStatusDraft: result := QuestionnaireStatusDraft;
		ResourceProfileStatusRetired : result := QuestionnaireStatusRetired;
	else
  result := QuestionnaireStatusNull;
	end;
end;

procedure TQuestionnaireBuilder.processMetadata;
var
  id : TFhirIdentifier;
  i : integer;
begin
  // todo: can we derive a more informative identifier from the questionnaire if we have a profile
  if FPrebuiltQuestionnaire = nil then
  begin
    id := FQuestionnaire.identifierList.Append;
    id.SystemST := 'urn:ietf:rfc:3986';
    id.ValueST := FQuestionnaireId;
    FQuestionnaire.VersionST := profile.VersionST;
    FQuestionnaire.StatusST := convertStatus(profile.StatusST);
    FQuestionnaire.DateST := profile.DateST.link;
    FQuestionnaire.publisherST := profile.PublisherST;
    FQuestionnaire.Group := TFhirQuestionnaireGroup.Create;
    FQuestionnaire.group.conceptList.AddAll(profile.codeList);
  end;

  if FAnswers <> nil then
  begin
    // no identifier - this is transient
    FQuestionnaire.xmlId := nextId('qs');
    FAnswers.questionnaire := TFhirResourceReference.Create;
    FAnswers.questionnaire.referenceST := '#'+FQuestionnaire.xmlId;
    FAnswers.containedList.Add(FQuestionnaire.Link);
    FAnswers.statusST := QuestionnaireAnswersStatusInProgress;
    FAnswers.Group := TFhirQuestionnaireAnswersGroup.Create;
    FAnswers.Group.Tag := FResource.Link;
  end;
end;

function TQuestionnaireBuilder.resolveValueSet(url: String): TFHIRValueSet;
var
  ref : TFhirResourceReference;
  dependencies : TStringList;
  s : String;
begin
  result := nil;
  if PrebuiltQuestionnaire <> nil then
    exit; // we don't do anything with value sets in this case

  if vsCache.ExistsByKey(url) then
    result := FQuestionnaire.contained[vsCache.GetValueByKey(url)].link as TFhirValueSet
  else
  begin
    ref := TFhirResourceReference.Create;
    dependencies := TStringList.create;
    try
      ref.referenceST := url;
      try
        result := OnExpand(nil, ref, MaxListboxCodings, false, dependencies);
        for s in dependencies do
          if not FDependencies.Contains(s) then
            FDependencies.Add(s);
      except
        on e: ETooCostly do
        begin
          result := TFhirValueSet.Create;
          try
            result.identifierST := ref.referenceST;
            result.link;
          finally
            result.Free;
          end;
        end;
        on e : Exception do
          raise;
      end;
    finally
      dependencies.Free;
      ref.Free;
    end;
  end;
end;

function TQuestionnaireBuilder.resolveValueSet(profile: TFHIRProfile; binding: TFhirProfileStructureSnapshotElementDefinitionBinding): TFHIRValueSet;
var
  ref : TFhirResourceReference;
  vs : TFHIRValueSet;
  dependencies : TStringList;
  s : String;
begin
  result := nil;
  if PrebuiltQuestionnaire <> nil then
    exit; // we don't do anything with value sets in this case

  if (binding = nil) or not (binding.reference is TFHIRResourceReference) then
    exit;

  dependencies := TStringList.create;
  try
    ref := binding.reference as TFHIRResourceReference;
    if ref.referenceST.StartsWith('#') then
    begin
      vs := TFhirValueSet(Fprofile.contained[ref.referenceST.Substring(1)]);
      try
        result := OnExpand(vs, nil, MaxListboxCodings, false, dependencies);
        for s in dependencies do
          if not FDependencies.Contains(s) then
            FDependencies.Add(s);
      except
        on e: ETooCostly do
        begin
          result := TFhirValueSet.Create;
          try
            result.identifierST := ref.referenceST;
            result.link;
          finally
            result.Free;
          end;
        end;
        on e : Exception do
          raise;
      end;

    end
    else if vsCache.ExistsByKey(ref.referenceST) then
      result := FQuestionnaire.contained[vsCache.GetValueByKey(ref.referenceST)].link as TFhirValueSet
    else
      try
        result := OnExpand(nil, ref, MaxListboxCodings, false, dependencies);
        for s in dependencies do
          if not FDependencies.Contains(s) then
            FDependencies.Add(s);
      except
        on e: ETooCostly do
        begin
          result := TFhirValueSet.Create;
          try
            result.identifierST := ref.referenceST;
            result.link;
          finally
            result.Free;
          end;
        end;
        on e : Exception do
          raise;
      end;
  finally
    dependencies.Free;
  end;
end;

procedure TQuestionnaireBuilder.SetAnswers(const Value: TFhirQuestionnaireAnswers);
begin
  FAnswers.Free;
  FAnswers := Value;
end;

procedure TQuestionnaireBuilder.SetContext(const Value: TFHIRRequest);
begin
  FContext.Free;
  FContext := Value;
end;

procedure TQuestionnaireBuilder.SetPrebuiltQuestionnaire(const Value: TFhirQuestionnaire);
begin
  FPrebuiltQuestionnaire.Free;
  FPrebuiltQuestionnaire := Value;
end;

procedure TQuestionnaireBuilder.SetProfile(const Value: TFHIRProfile);
begin
  FProfile.Free;
  FProfile := Value;
end;

procedure TQuestionnaireBuilder.SetProfiles(const Value: TProfileManager);
begin
  FProfiles.Free;
  FProfiles := Value;
end;

procedure TQuestionnaireBuilder.SetResource(const Value: TFhirResource);
begin
  FResource.Free;
  FResource := Value;
end;

procedure TQuestionnaireBuilder.SetStructure(const Value: TFhirProfileStructure);
begin
  FStructure.Free;;
  FStructure := Value;
end;

procedure TQuestionnaireBuilder.UnBuild;
var
  i : integer;
  defn : TProfileDefinition;
  gen : TNarrativeGenerator;
begin
  if Profile = nil then
    raise Exception.Create('A Profile is required');

  if Structure = nil then
  begin
    for i := 0 to profile.structureList.Count - 1 do
      if profile.structureList[i].publishST then
        if structure = nil then
          structure := profile.structureList[i].Link
        else
          raise Exception.create('buildQuestionnaire: if there is more than one published structure in the profile, you must choose one');
    if (structure = nil) then
      raise Exception.create('buildQuestionnaire: no published structure found');
  end;

  if Structure = nil then
    raise Exception.Create('A Structure is required');
  if Answers = nil then
    raise Exception.Create('A set of answers is required');
  if Answers.group = nil then
    raise Exception.Create('A base group is required in the answers');

  if Answers.group.linkIdST <> Structure.snapshot.elementList[0].pathST then
    raise Exception.Create('Mismatch between answers and profile');

  Resource := FFactory.makeByName(structure.type_ST) as TFhirResource;

  defn := TProfileDefinition.create(profiles.Link, profile.link, structure.link);
  try
    processAnswerGroup(Answers.group, resource, defn);
  finally
    defn.free;
  end;

  gen := TNarrativeGenerator.Create('', FProfiles.Link, OnLookupCode, onLookupReference, context.Link);
  try
    gen.generate(Resource, profile);
  finally
    gen.Free;
  end;
end;

function TQuestionnaireBuilder.convertType(v : TFhirElement; t : string; path : String) : TFhirElement;
var
  s : String;
begin
  s := v.FhirType;
  if (s = t) then
    result := v.link
  else if ((s = 'string') and (t = 'code')) then
    result := TFhirEnum.Create(TFHIRString(v).value)
  else if ((s = 'string') and (t = 'uri')) then
    result := TFhirUri.Create(TFHIRString(v).value)
  else if ((s = 'Coding') and (t = 'code')) then
    result := TFhirEnum.Create(TFHIRCoding(v).codeST)
  else if ((s = 'dateTime') and (t = 'date')) then
    result := TFhirDate.Create(TFhirDateTime(v).value.Link)
  else
    raise Exception.Create('Unable to convert from '+s+' to '+t+' at path = '+path);
end;

function isPrimitive(t : TFhirProfileStructureSnapshotElementDefinitionType) : Boolean; overload;
begin
  result := (t <> nil) and (StringArrayExistsSensitive(['string', 'code', 'boolean', 'integer', 'decimal', 'date', 'dateTime', 'instant', 'time', 'ResourceReference'], t.codeST));
end;

function allTypesSame(types : TFhirProfileStructureSnapshotElementDefinitionTypeList) : boolean;
var
  i : integer;
  s : String;
begin
  result := true;
  s := types[0].codeSt;
  for i := 1 to types.count-1 do
    if s <> types[i].codeST then
      result := false;
end;

function determineType(g : TFhirQuestionnaireAnswersGroup) : TFhirProfileStructureSnapshotElementDefinitionType;
var
  q : TFhirQuestionnaireAnswersGroupQuestion;
  cc : TFhirCoding;
begin
  result := nil;
  if (g.questionList.Count <> 1) then
    exit;
  q := g.questionList[0];
  if (q.linkIdST <> g.linkIdST+'._type') then
    exit;
  if q.answerList.Count <> 1 then
    exit;
  if q.answerList[0].value is TFhirCoding then
  begin
    cc := TFhirCoding(q.answerList[0].value);
    result := TFhirProfileStructureSnapshotElementDefinitionType.Create;
    try
      result.tagValue := cc.codeST;
      if cc.systemST = 'http://hl7.org/fhir/resource-types' then
      begin
        result.codeST := 'ResourceReference';
        result.profileST := 'http://hl7.org/fhir/Profile/'+cc.codeST;
      end
      else // cc.systemST = 'http://hl7.org/fhir/data-types'
      begin
        result.codeST := cc.codeST;
      end;
      result.Link;
    finally
      result.Free;
    end;
  end;
end;

function selectTypeGroup(g : TFhirQuestionnaireAnswersGroup; t : TFhirProfileStructureSnapshotElementDefinitionType) : TFhirQuestionnaireAnswersGroup;
var
  i : integer;
begin
  result := nil;
  for i := 0 to g.questionList[0].groupList.count - 1 do
    if g.questionList[0].groupList[i].linkIdST = g.linkIdST+'._'+t.TagValue then
      result := g.questionList[0].groupList[i];
end;

function TQuestionnaireBuilder.processAnswerGroup(group : TFhirQuestionnaireAnswersGroup; context : TFhirElement; defn :  TProfileDefinition) : boolean;
var
  i : integer;
  g : TFhirQuestionnaireAnswersGroup;
  q : TFhirQuestionnaireAnswersGroupQuestion;
  a : TFhirQuestionnaireAnswersGroupQuestionAnswer;
  d : TProfileDefinition;
  t : TFhirProfileStructureSnapshotElementDefinitionType;
  o : TFHIRElement;
  j: Integer;
begin
  result := false;

  for i := 0 to group.groupList.Count - 1 do
  begin
    g := group.groupList[i];
    d := defn.getById(g.linkIdST);
    try
      t := nil;
      try
        if d.hasTypeChoice then
        begin
          t := determineType(g);
          d.setType(t.link);
          // now, select the group for the type
          g := selectTypeGroup(g, t);
        end
        else
          t := d.statedType.link;

        if ((g <> nil) and (t <> nil)) or not d.hasTypeChoice then
        begin
          if (isPrimitive(t)) then
          begin
            if (g.questionList.Count <> 1) then
              raise Exception.Create('Unexpected Condition: a group for a primitive type with more than one question @ '+g.linkIdST);
            if (g.groupList.Count > 0) then
              raise Exception.Create('Unexpected Condition: a group for a primitive type with groups @ '+g.linkIdST);
            q := g.questionList[0];
            for j := 0 to q.answerList.Count - 1 do
            begin
              a := q.answerList[j];
              if a.value <> nil then
              begin
                context.setProperty(d.name, convertType(a.value, t.codeST, g.linkIdST));
                result := true;
              end
              else
                raise Exception.Create('Empty answer for '+g.linkIdST);
            end;
          end
          else
          begin
            if t = nil then
              o := FFactory.makeByName(d.path)
            else
              o := FFactory.makeByName(t.codeST);
            try
              if processAnswerGroup(g, o, d) then
              begin
                context.setProperty(d.name, o.Link);
                result := true;
              end;
            finally
              o.Free;
            end;
          end;
        end;
      finally
        t.Free;
      end;
    finally
      d.free;
    end;
  end;

  for i := 0 to group.questionList.Count - 1 do
  begin
    q := group.questionList[i];
    d := defn.getById(q.linkIdST);
    try
      if d.hasTypeChoice then
        raise Exception.Create('not done yet - shouldn''t get here??');
      for j := 0 to q.answerList.Count - 1 do
      begin
        a := q.answerList[j];
        context.setProperty(d.name, convertType(a.value, d.statedType.codeST, q.linkIdST));
        result := true;
      end;
    finally
      d.free;
    end;
  end;
end;

procedure TQuestionnaireBuilder.buildGroup(group: TFHIRQuestionnaireGroup; profile: TFHIRProfile; structure: TFHIRProfileStructure; element: TFhirProfileStructureSnapshotElement; parents: TFhirProfileStructureSnapshotElementList; answerGroups : TFhirQuestionnaireAnswersGroupList);
var
  i : integer;
  list : TFhirProfileStructureSnapshotElementList;
  child : TFhirProfileStructureSnapshotElement;
  nparents : TFhirProfileStructureSnapshotElementList;
  childGroup : TFHIRQuestionnaireGroup;
  nAnswers : TFhirQuestionnaireAnswersGroupList;
begin
  group.LinkIdST := element.PathST; // todo: this will be wrong when we start slicing
  group.TitleST := element.Definition.ShortST; // todo - may need to prepend the name tail...
  group.TextST := element.Definition.commentsST;
  group.SetExtensionString(FLYOVER_REFERENCE, element.Definition.FormalST);
  group.RequiredST := element.Definition.Min.Value > '0';
  group.RepeatsST := element.Definition.Max.Value <> '1';

  for i := 0 to answerGroups.Count - 1 do
  begin
    answerGroups[i].linkIdST := group.linkIdST;
    answerGroups[i].TitleST := group.TitleST;
    answerGroups[i].TextST := group.TextST;
  end;

  // now, we iterate the children
  list := getChildList(structure, element);
  try
    for i := 0 to list.Count - 1 do
    begin
      child := list[i];

			if (child.Definition = nil) then
				raise Exception.create('Found an element with no definition generating a Questionnaire');

      if (not isExempt(element, child)) and (not parents.ExistsByReference(child)) then
      begin
        nparents := TFhirProfileStructureSnapshotElementList.Create;
        try
          nparents.Assign(parents);
          nparents.add(child.link);
          childGroup := group.groupList.Append;

          nAnswers := TFhirQuestionnaireAnswersGroupList.Create;
          try
             processExisting(child.pathST, answerGroups, nAnswers);
            // if the element has a type, we add a question. else we add a group on the basis that
            // it will have children of it's own
            if (child.Definition.type_List.isEmpty) then
              buildGroup(childGroup, profile, structure, child, nparents, nAnswers)
            else
              buildQuestion(childGroup, profile, structure, child, child.pathST, nAnswers);
          finally
            nAnswers.Free;
          end;
        finally
          nparents.Free;
        end;
      end;
	  end;
  finally
    list.Free;
  end;
end;

function TQuestionnaireBuilder.getChildList(structure :TFHIRProfileStructure; path : String) : TFhirProfileStructureSnapshotElementList;
var
  i : integer;
  e : TFhirProfileStructureSnapshotElement;
  p, tail : String;
begin
  result := TFhirProfileStructureSnapshotElementList.Create;
  try
    for i := 0 to structure.snapshot.elementList.Count - 1 do
    begin
      e := structure.snapshot.elementList[i];
      p := e.pathST;

      if (e.definition.nameReferenceST <> '') and path.startsWith(p) then
      begin
        result.Free;
        if (path.length > p.length) then
          result := getChildList(structure, e.Definition.NameReferenceST+'.'+path.substring(p.length+1))
        else
          result := getChildList(structure, e.Definition.NameReferenceST);
      end
      else if p.startsWith(path+'.') and (p <> path) then
      begin
        tail := p.substring(path.length+1);
        if (not tail.contains('.')) then
          result.add(e.Link);
      end;
    end;
    result.link;
  finally
    result.Free;
  end;
end;


function TQuestionnaireBuilder.getChildList(structure :TFHIRProfileStructure; element : TFhirProfileStructureSnapshotElement) : TFhirProfileStructureSnapshotElementList;
begin
  result := getChildList(structure, element.PathST);
end;

function TQuestionnaireBuilder.getSystemForCode(vs: TFHIRValueSet; code: String; path : String): String;
var
  i, q : integer;
begin
  if (vs = nil) then
  begin
    if FPrebuiltQuestionnaire <> nil then
    begin
      for q := 0 to FPrebuiltQuestionnaire.containedList.Count - 1 do
        if FPrebuiltQuestionnaire.containedList[q] is TFhirValueSet then
        begin
          vs := TFhirValueSet(FPrebuiltQuestionnaire.containedList[q]);
          if (vs.expansion <> nil) then
          begin
            for i := 0 to vs.expansion.containsList.Count - 1 do
              if vs.expansion.containsList[i].codeST = code then
                if result = '' then
                  result := vs.expansion.containsList[i].systemST
                 else
                  raise Exception.Create('Multiple matches in '+vs.identifierST+' for code '+code+' at path = '+path);
          end;
        end;
    end;
    raise Exception.Create('Logic error'+' at path = '+path);
  end;
  result := '';
  for i := 0 to vs.expansion.containsList.Count - 1 Do
  begin
    if vs.expansion.containsList[i].codeST = code then
      if result = '' then
        result := vs.expansion.containsList[i].systemST
      else
        raise Exception.Create('Multiple matches in '+vs.identifierST+' for code '+code+' at path = '+path);
  end;
  if result = '' then
    raise Exception.Create('Unable to resolve code '+code+' at path = '+path);
end;

function TQuestionnaireBuilder.isExempt(element, child: TFhirProfileStructureSnapshotElement) : boolean;
var
  n, t : string;
begin
  n := tail(child.PathST);
  if not element.Definition.type_List.isEmpty then
    t :=  element.Definition.type_List[0].CodeST;

  // we don't generate questions for the base stuff in every element
	if (t = 'Resource') and
				((n = 'text') or (n = 'language') or (n = 'contained')) then
    result := true
		// we don't generate questions for extensions
	else if (n = 'extension') or (n = 'modifierExtension') then
  begin
    if (child.definition.type_List.Count > 0) and (child.definition.type_List[0].profileST <> '') then
      result := false
    else
      result := true
  end
  else
	  result := false;
end;

function TQuestionnaireBuilder.expandTypeList(types: TFhirProfileStructureSnapshotElementDefinitionTypeList): TFhirProfileStructureSnapshotElementDefinitionTypeList;
var
  i : integer;
  t : TFhirProfileStructureSnapshotElementDefinitionType;
begin
  result := TFhirProfileStructureSnapshotElementDefinitionTypeList.create;
  try
    for i := 0 to types.Count - 1 do
    begin
      t := types[i];
      if (t.profileST <> '') then
        result.Add(t.Link)
      else if (t.codeST = '*') then
      begin
        result.Append.codeST := 'integer';
        result.Append.codeST := 'decimal';
        result.Append.codeST := 'dateTime';
        result.Append.codeST := 'date';
        result.Append.codeST := 'instant';
        result.Append.codeST := 'time';
        result.Append.codeST := 'string';
        result.Append.codeST := 'uri';
        result.Append.codeST := 'boolean';
        result.Append.codeST := 'Coding';
        result.Append.codeST := 'CodeableConcept';
        result.Append.codeST := 'Attachment';
        result.Append.codeST := 'Identifier';
        result.Append.codeST := 'Quantity';
        result.Append.codeST := 'Range';
        result.Append.codeST := 'Period';
        result.Append.codeST := 'Ratio';
        result.Append.codeST := 'HumanName';
        result.Append.codeST := 'Address';
        result.Append.codeST := 'Contact';
        result.Append.codeST := 'Schedule';
        result.Append.codeST := 'ResourceReference';
      end
      else
        result.Add(t.Link);
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TQuestionnaireBuilder.makeAnyValueSet: TFhirValueSet;
begin
  if vsCache.ExistsByKey(ANY_CODE_VS) then
    result := FQuestionnaire.contained[vsCache.GetValueByKey(ANY_CODE_VS)].link as TFhirValueSet
  else
  begin
    result := TFhirValueSet.Create;
    try
      result.identifierST := ANY_CODE_VS;
      result.nameST := 'All codes known to the system';
      result.descriptionST := 'All codes known to the system';
      result.statusST := ValuesetStatusActive;
      result.compose := TFhirValueSetCompose.create;
      result.compose.includeList.Append.systemST := ANY_CODE_VS;
      result.link;
    finally
      result.Free;
    end;
  end;

end;

function TQuestionnaireBuilder.makeTypeList(profile : TFHIRProfile; types: TFhirProfileStructureSnapshotElementDefinitionTypeList; path : String): TFHIRValueSet;
var
  vs : TFhirValueset;
  i : integer;
  t : TFhirProfileStructureSnapshotElementDefinitionType;
  cc : TFhirValueSetExpansionContains;
  structure : TFhirProfileStructure;
begin
  vs := TFhirValueset.Create;
  try
    vs.identifierST := NewGuidURN;
    vs.nameST := 'Type options for '+path;
    vs.descriptionST := vs.nameST;
    vs.statusST := ValuesetStatusActive;
    vs.expansion := TFhirValueSetExpansion.Create;
    vs.expansion.timestampST := NowUTC;
    for i := 0 to types.Count - 1 do
    begin
      t := types[i];
      cc := vs.expansion.containsList.Append;
      if (t.codeST = 'ResourceReference') and (t.profileST.startsWith('http://hl7.org/fhir/Profile/')) then
      begin
        cc.codeST := t.profileST.Substring(28);
        cc.systemST := 'http://hl7.org/fhir/resource-types';
        cc.displayST := cc.codeST;
      end
      else if (t.profileST <> '') and FProfiles.getStructure(profile, t.profileST, profile, structure) then
      begin
        cc.codeST := t.profileST;
        cc.displayST := structure.nameST;
        cc.systemST := 'http://hl7.org/fhir/resource-types';
      end
      else
      begin
        cc.codeST := t.codeST;
        cc.displayST := t.codeST;
        cc.systemST := 'http://hl7.org/fhir/data-types';
      end;
      t.TagValue := cc.codeST;
    end;
    result := vs.Link;
  finally
    vs.Free;
  end;
end;

function TQuestionnaireBuilder.nextId(prefix : string): String;
begin
  inc(lastid);
  result := prefix+inttostr(lastid);
end;

function TQuestionnaireBuilder.instanceOf(t : TFhirProfileStructureSnapshotElementDefinitionType; obj : TFHIRObject) : boolean;
var
  url : String;
begin
  if t.codeST = 'ResourceReference' then
  begin
    if not (obj is TFHIRResourceReference) then
      result := false
    else
    begin
      url := TFHIRResourceReference(obj).referenceST;
      {
      there are several problems here around profile matching. This process is degenerative, and there's probably nothing we can do to solve it
      }
      if url.StartsWith('http:') or url.StartsWith('https:') then
        result := true
      else if (t.profileST.startsWith('http://hl7.org/fhir/Profile/')) then
        result := url.StartsWith(t.profileST.Substring(28)+'/')
      else
        result := true;
    end;
  end
  else if t.codeST = 'Quantity' then
    result := obj is TFHIRQuantity
  else
    raise Exception.Create('Not Done Yet');
end;

procedure TQuestionnaireBuilder.selectTypes(profile : TFHIRProfile; sub : TFHIRQuestionnaireGroup; t : TFhirProfileStructureSnapshotElementDefinitionType; source, dest : TFhirQuestionnaireAnswersGroupList);
var
  i : integer;
  temp : TFhirQuestionnaireAnswersGroupList;
  subg : TFhirQuestionnaireAnswersGroup;
  q : TFhirQuestionnaireAnswersGroupQuestion;
  cc : TFhirCoding;
  structure : TFhirProfileStructure;
begin
  temp := TFhirQuestionnaireAnswersGroupList.Create;
  try
    for i := 0 to source.count - 1 do
      if instanceOf(t, source[i].tag as TFHIRObject) then
        temp.add(source[i].link);
    for i := 0 to temp.count- 1 do
      source.DeleteByReference(temp[i]);
    for i := 0 to temp.count- 1 do
    begin
      // 1st the answer:
      assert(temp[i].questionList.count = 0); // it should be empty
      q := temp[i].questionList.Append;
      q.linkIdST := temp[i].linkIdST+'._type';
      q.textST := 'type';

      cc := TFHIRCoding.Create;
      q.answerList.append.value := cc;
      if (t.codeST = 'ResourceReference') and (t.profileST.startsWith('http://hl7.org/fhir/Profile/')) then
      begin
        cc.codeST := t.profileST.Substring(28);
        cc.systemST := 'http://hl7.org/fhir/resource-types';
      end
      else if (t.profileST <> '') and FProfiles.getStructure(profile, t.profileST, profile, structure) then
      begin
        cc.codeST := t.profileST;
        cc.systemST := 'http://hl7.org/fhir/resource-types';
      end
      else
      begin
        cc.codeST := t.codeST;
        cc.systemST := 'http://hl7.org/fhir/data-types';
      end;

      // 1st: create the subgroup
      subg := q.groupList.Append;
      dest.Add(subg.Link);
      subg.linkIdST := sub.linkIdST;
      subg.textST := sub.textST;
      subg.Tag := temp[i].Tag.Link;

    end;
  finally
    temp.Free;
  end;
end;

// most of the types are complex in regard to the Questionnaire, so they are still groups
	// there will be questions for each component
procedure TQuestionnaireBuilder.buildQuestion(group : TFHIRQuestionnaireGroup; profile : TFHIRProfile; structure :TFHIRProfileStructure; element : TFhirProfileStructureSnapshotElement; path : String; answerGroups : TFhirQuestionnaireAnswersGroupList);
var
  t, p : TFhirProfileStructureSnapshotElementDefinitionType;
  q : TFhirQuestionnaireGroupQuestion;
  types : TFhirProfileStructureSnapshotElementDefinitionTypeList;
  i, j, k : integer;
  sub : TFHIRQuestionnaireGroup;
  selected : TFhirQuestionnaireAnswersGroupList;
begin
  group.LinkIdST := path;

  // in this context, we don't have any concepts to mark...
  group.TextST := element.Definition.ShortST; // prefix with name?
  group.RequiredST := element.Definition.Min.Value > '0';
  group.RepeatsST := element.Definition.Max.Value <> '1';

  for i := 0 to answerGroups.Count - 1 do
  begin
    answerGroups[i].linkIdST := group.linkIdST;
    answerGroups[i].TitleST := group.TitleST;
    answerGroups[i].TextST := group.TextST;
  end;

  if element.Definition.commentsST <> '' then
    group.setExtensionString(FLYOVER_REFERENCE, element.Definition.FormalST+' '+element.Definition.commentsST)
  else
    group.setExtensionString(FLYOVER_REFERENCE, element.Definition.FormalST);

  if (element.Definition.type_List.Count > 1) or (element.Definition.type_List[0].CodeST = '*') then
  begin
    types := expandTypeList(element.definition.type_List);
    try
      q := addQuestion(group, AnswerFormatChoice, element.pathST, '_type', 'type', true, nil, makeTypeList(profile, types, element.pathST));
      for i := 0 to types.Count - 1 do
      begin
        t := types[i];
        sub := q.groupList.Append;
        sub.LinkIdST := element.PathST+'._'+t.tagValue;
        sub.TextST := t.TagValue;
        // always optional, never repeats

        selected := TFhirQuestionnaireAnswersGroupList.create;
        try
          selectTypes(profile, sub, t, answerGroups, selected);
          processDataType(profile, sub, element, element.PathST+'._'+t.tagValue, t, group.requiredST, selected);
        finally
          selected.free;
        end;
      end;
    finally
      types.free;
    end;
  end
  else
    // now we have to build the question panel for each different data type
    processDataType(profile, group, element, element.PathST, element.Definition.Type_list[0], group.requiredST, answerGroups);
end;

procedure TQuestionnaireBuilder.processDataType(profile : TFHIRProfile; group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; t : TFhirProfileStructureSnapshotElementDefinitionType; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
begin
  if (t.CodeST = 'code') then
    addCodeQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'string') or (t.CodeST = 'id') or (t.CodeST = 'oid') then
    addStringQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'uri') then
    addUriQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'boolean') then
    addBooleanQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'decimal') then
    addDecimalQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'dateTime') or (t.CodeST = 'date') then
    addDateTimeQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'instant') then
    addInstantQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'time') then
    addTimeQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'CodeableConcept') then
    addCodeableConceptQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'Period') then
    addPeriodQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'Ratio') then
    addRatioQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'HumanName') then
    addHumanNameQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'Address') then
    addAddressQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'Contact') then
    addContactQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'Identifier') then
    addIdentifierQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'integer') then
    addIntegerQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'Coding') then
    addCodingQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'Quantity') then
    addQuantityQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'ResourceReference') then
    addReferenceQuestions(group, element, path, required, t.profileST, answerGroups)
  else if (t.CodeST = 'idref') then
    addIdRefQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'Duration') then
    addDurationQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'base64Binary') then
    addBinaryQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'Attachment') then
    addAttachmentQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'Age') then
    addAgeQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'Range') then
    addRangeQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'Schedule') then
    addScheduleQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'SampledData') then
    addSampledDataQuestions(group, element, path, required, answerGroups)
  else if (t.CodeST = 'Extension') then
    addExtensionQuestions(profile, group, element, path, required, t.profileST, answerGroups)
  else
    raise Exception.create('Unhandled Data Type: '+t.CodeST+' on element '+element.PathST);
end;

function isPrimitive(obj : TAdvObject) : boolean; overload;
begin
  result := (obj is TFHIRBoolean) or (obj is TFHIRInteger) or (obj is TFHIRDecimal) or (obj is TFHIRBase64Binary) or (obj is TFHIRInstant) or (obj is TFHIRString) or (obj is TFHIRUri) or
            (obj is TFHIRDate) or (obj is TFHIRDateTime) or (obj is TFHIRTime) or (obj is TFHIRCode) or (obj is TFHIROid) or (obj is TFHIRUuid) or (obj is TFHIRId) or (obj is TFHIRResourceReference);
end;

function TQuestionnaireBuilder.convertType(value : TFHIRObject; af : TFhirAnswerFormat; vs : TFHIRValueSet; path : String) : TFhirType;
begin
  result := nil;
  case af of
    // simple cases
    AnswerFormatBoolean: if value is TFhirBoolean then result := value.link as TFhirType;
    AnswerFormatDecimal: if value is TFhirDecimal then result := value.link as TFhirType;
    AnswerFormatInteger: if value is TFhirInteger then result := value.link as TFhirType;
    AnswerFormatDate: if value is TFhirDate then result := value.link as TFhirType;
    AnswerFormatDateTime: if value is TFhirDateTime then result := value.link as TFhirType;
    AnswerFormatInstant: if value is TFhirInstant then result := value.link as TFhirType;
    AnswerFormatTime: if value is TFhirTime then result := value.link as TFhirType;
    AnswerFormatString:
      if value is TFhirString then
        result := value.link as TFhirType
      else if value is TFhirUri then
        result := TFHIRString.Create(TFhirUri(value).value);

    AnswerFormatText: if value is TFhirString then result := value.link as TFhirType;
    AnswerFormatQuantity: if value is TFhirQuantity then result := value.link as TFhirType;

    // complex cases:
    // ? AnswerFormatAttachment: ...?
    AnswerFormatChoice, AnswerFormatOpenChoice :
      if value is TFhirCoding then
        result := value.link as TFhirType
      else if value is TFHIREnum then
      begin
        result := TFhirCoding.create;
        TFhirCoding(result).codeST := TFHIREnum(value).value;
        TFhirCoding(result).systemST := getSystemForCode(vs, TFHIREnum(value).value, path);
      end
      else if value is TFHIRString then
      begin
        result := TFhirCoding.create;
        TFhirCoding(result).codeST := TFHIRString(value).value;
        TFhirCoding(result).systemST := getSystemForCode(vs, TFHIRString(value).value, path);
      end;

    AnswerFormatReference:
      if value is TFHIRResourceReference then
        result := value.link as TFhirType
      else if value is TFHIRString then
      begin
        result := TFHIRResourceReference.Create;
        TFHIRResourceReference(result).referenceST := TFHIRString(value).value;
      end;
  end;

  if (result = nil) then
    raise Exception.Create('Unable to convert from "'+value.className+'" for Answer Format '+CODES_TFHIRAnswerFormat[af]+', path = '+path);
end;



constructor TQuestionnaireBuilder.create;
begin
  inherited;
  vsCache := TAdvStringMatch.create;
  FDependencies := TList<String>.create;
end;

function TQuestionnaireBuilder.addQuestion(group : TFHIRQuestionnaireGroup; af : TFhirAnswerFormat; path, id, name : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList; vs : TFHIRValueSet) : TFhirQuestionnaireGroupQuestion;
var
  i, j : integer;
  aq : TFhirQuestionnaireAnswersGroupQuestion;
  children : TFHIRObjectList;
  vse : TFhirValueSet;
begin
  try
    result := group.questionList.Append;
    if vs <> nil then
    begin
      result.options := TFhirResourceReference.Create;
      if (vs.expansion = nil) then
      begin
        result.options.referenceST := vs.identifierST;
        result.options.addExtension(EXTENSION_FILTER_ONLY, TFhirBoolean.Create(true));
      end
      else
      begin
        if (vs.xmlId = '') then
        begin
          vse := vs.Clone;
          try
            vse.xmlId := nextId('vs');
            vsCache.Add(vse.identifierST, vse.xmlId);
            vse.text := nil;
            vse.define := nil;
            vse.compose := nil;
            vse.telecomList.Clear;
            vse.purpose := nil;
            vse.publisher := nil;
            vse.copyright := nil;
            questionnaire.containedList.Add(vse.Link);
            result.options.referenceST := '#'+vse.xmlId;
          finally
            vse.Free;
          end;
        end
        else
          result.options.referenceST := '#'+vs.xmlId;
      end;
    end;

    result.LinkIdST := path+'.'+id;
    result.TextST := name;
    result.Type_ST := af;
    result.RequiredST := required;
    result.RepeatsST := false;

    if (id.endsWith('/1')) then
      id := id.substring(0, id.length-2);

    if assigned(answerGroups) then
    begin
      for i := 0 to answerGroups.Count - 1 do
      begin
        children := TFHIRObjectList.Create;
        try
          aq := nil;

          if isPrimitive(answerGroups[i].Tag) then
            children.add(answerGroups[i].Tag.Link)
          else if answerGroups[i].Tag is TFHIREnum then
            children.add(TFHIRString.create(TFHIREnum(answerGroups[i].Tag).value))
          else
            TFHIRObject(answerGroups[i].Tag).ListChildrenByName(id, children);

          for j := 0 to children.Count - 1 do
            if children[j] <> nil then
            begin
              if (aq = nil) then
              begin
                aq := answerGroups[i].questionList.Append;
                aq.LinkIdST := result.linkIdST;
                aq.TextST := result.textST;
              end;
              aq.answerList.append.value := convertType(children[j], af, vs, result.linkIdST);
            end;
        finally
          children.Free;
        end;
      end;
    end;
  finally
    vs.Free;
  end;
end;

function UnCamelCase(s : String) : String;
var
  i, j : integer;
begin
  setLength(result, length(s) * 2);
  i := 1;
  j := 1;
  while (i <= length(s)) do
  begin
    if Upcase(s[i]) = s[i] then
    begin
      result[j] := ' ';
      inc(j);
    end;
    result[j] := s[i];
    inc(j);
    inc(i);
  end;
  setLength(result, j-1);
  result := Result.ToLower;
end;

procedure TQuestionnaireBuilder.addCodeQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
var
  i : integer;
  vs : TFHIRValueSet;
begin
  group.setExtensionString(TYPE_EXTENSION, 'code');
  vs := resolveValueSet(nil, element.Definition.Binding);
  if vs = nil then
    vs := makeAnyValueSet;
  addQuestion(group, AnswerFormatChoice, path, 'value', unCamelCase(Tail(element.pathST)), required, answerGroups, vs);
  group.text := nil;
  for i := 0 to answerGroups.count - 1 do
    answerGroups[i].text := nil;
end;

// Primitives ------------------------------------------------------------------
procedure TQuestionnaireBuilder.addStringQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
var
  i : integer;
begin
  group.setExtensionString(TYPE_EXTENSION, 'string');
  addQuestion(group, AnswerFormatString, path, 'value', group.textST, required, answerGroups);
  group.text := nil;
  for i := 0 to answerGroups.count - 1 do
    answerGroups[i].text := nil;
end;

procedure TQuestionnaireBuilder.addTimeQuestions(group: TFHIRQuestionnaireGroup; element: TFhirProfileStructureSnapshotElement; path: String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
var
  i : integer;
begin
  group.setExtensionString(TYPE_EXTENSION, 'time');
  addQuestion(group, AnswerFormatTime, path, 'value', group.textST, required, answerGroups);
  group.text := nil;
  for i := 0 to answerGroups.count - 1 do
    answerGroups[i].text := nil;
end;

procedure TQuestionnaireBuilder.addUriQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
var
  i : integer;
begin
  group.setExtensionString(TYPE_EXTENSION, 'uri');
  addQuestion(group, AnswerFormatString, path, 'value', group.textST, required, answerGroups);
  group.text := nil;
  for i := 0 to answerGroups.count - 1 do
    answerGroups[i].text := nil;
end;


procedure TQuestionnaireBuilder.addBooleanQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
var
  i : integer;
begin
  group.setExtensionString(TYPE_EXTENSION, 'boolean');
	addQuestion(group, AnswerFormatBoolean, path, 'value', group.textST, required, answerGroups);
  group.text := nil;
  for i := 0 to answerGroups.count - 1 do
    answerGroups[i].text := nil;
end;

procedure TQuestionnaireBuilder.addDecimalQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
var
  i : integer;
begin
  group.setExtensionString(TYPE_EXTENSION, 'decimal');
  addQuestion(group, AnswerFormatDecimal, path, 'value', group.textST, required, answerGroups);
  group.text := nil;
  for i := 0 to answerGroups.count - 1 do
    answerGroups[i].text := nil;
end;

procedure TQuestionnaireBuilder.addIntegerQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
var
  i : integer;
begin
  group.setExtensionString(TYPE_EXTENSION, 'integer');
  addQuestion(group, AnswerFormatInteger, path, 'value', group.textST, required, answerGroups);
  group.text := nil;
  for i := 0 to answerGroups.count - 1 do
    answerGroups[i].text := nil;
end;

procedure TQuestionnaireBuilder.addDateTimeQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
var
  i : integer;
begin
  group.setExtensionString(TYPE_EXTENSION, 'datetime');
  addQuestion(group, AnswerFormatDateTime, path, 'value', group.textST, required, answerGroups);
  group.text := nil;
  for i := 0 to answerGroups.count - 1 do
    answerGroups[i].text := nil;
end;

procedure TQuestionnaireBuilder.addInstantQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
var
  i : integer;
begin
  group.setExtensionString(TYPE_EXTENSION, 'instant');
  addQuestion(group, AnswerFormatInstant, path, 'value', group.textST, required, answerGroups);
  group.text := nil;
  for i := 0 to answerGroups.count - 1 do
    answerGroups[i].text := nil;
end;

procedure TQuestionnaireBuilder.addBinaryQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
begin
  group.setExtensionString(TYPE_EXTENSION, 'binary');
  // ? Lloyd: how to support binary content
end;

// Complex Types ---------------------------------------------------------------

function AnswerTypeForBinding(binding : TFhirProfileStructureSnapshotElementDefinitionBinding) : TFhirAnswerFormat;
begin
  if (binding = nil) then
    result := AnswerFormatOpenChoice
  else if (binding.isExtensibleST) then
    result := AnswerFormatOpenChoice
  else
    result := AnswerFormatChoice;
end;

procedure TQuestionnaireBuilder.addCodingQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
var
  i : integer;
begin
  group.setExtensionString(TYPE_EXTENSION, 'Coding');
  addQuestion(group, AnswerTypeForBinding(element.Definition.Binding), path, 'value', group.textST, required, answerGroups, resolveValueSet(nil, element.Definition.Binding));
  group.text := nil;
  for i := 0 to answerGroups.count - 1 do
    answerGroups[i].text := nil;
end;

procedure TQuestionnaireBuilder.addCodeableConceptQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
begin
  group.setExtensionString(TYPE_EXTENSION, 'CodeableConcept');
  addQuestion(group, AnswerTypeForBinding(element.Definition.Binding), path, 'coding', 'code:', false, answerGroups, resolveValueSet(nil, element.Definition.Binding));
  addQuestion(group, AnswerFormatOpenChoice, path, 'coding/1', 'other codes:', false, answerGroups, makeAnyValueSet).RepeatsST := true;
	addQuestion(group, AnswerFormatString, path, 'text', 'text:', required, answerGroups);
end;

procedure TQuestionnaireBuilder.addPeriodQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Period');
	addQuestion(group, AnswerFormatDateTime, path, 'start', 'start:', false, answerGroups);
	addQuestion(group, AnswerFormatDateTime, path, 'end', 'end:', false, answerGroups);
end;

procedure TQuestionnaireBuilder.addRatioQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Ratio');
	addQuestion(group, AnswerFormatDecimal, path, 'numerator', 'numerator:', false, answerGroups);
	addQuestion(group, AnswerFormatDecimal, path, 'denominator', 'denominator:', false, answerGroups);
	addQuestion(group, AnswerFormatString, path, 'units', 'units:', required, answerGroups);
end;

procedure TQuestionnaireBuilder.addHumanNameQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Name');
	addQuestion(group, AnswerFormatString, path, 'text', 'text:', false, answerGroups);
	addQuestion(group, AnswerFormatString, path, 'family', 'family:', required, answerGroups).RepeatsST := true;
	addQuestion(group, AnswerFormatString, path, 'given', 'given:', false, answerGroups).RepeatsST := true;
end;

procedure TQuestionnaireBuilder.addAddressQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
var
  ref : TFhirResourceReference;
begin
  group.setExtensionString(TYPE_EXTENSION, 'Address');
  addQuestion(group, AnswerFormatString, path, 'text', 'text:', false, answerGroups);
  addQuestion(group, AnswerFormatString, path, 'line', 'line:', false, answerGroups).RepeatsST := true;
  addQuestion(group, AnswerFormatString, path, 'city', 'city:', false, answerGroups);
  addQuestion(group, AnswerFormatString, path, 'state', 'state:', false, answerGroups);
  addQuestion(group, AnswerFormatString, path, 'zip', 'zip:', false, answerGroups);
  addQuestion(group, AnswerFormatString, path, 'country', 'country:', false, answerGroups);
	addQuestion(group, AnswerFormatChoice, path, 'use', 'use:', false, answerGroups, resolveValueSet('http://hl7.org/fhir/vs/address-use'));
end;

procedure TQuestionnaireBuilder.addContactQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Contact');
	addQuestion(group, AnswerFormatChoice, path, 'system', 'type:', false, answerGroups, resolveValueSet('http://hl7.org/fhir/vs/contact-system'));
	addQuestion(group, AnswerFormatString, path, 'value', 'value:', required, answerGroups);
	addQuestion(group, AnswerFormatChoice, path, 'use', 'use:', false, answerGroups, resolveValueSet('http://hl7.org/fhir/vs/contact-use'));
end;

procedure TQuestionnaireBuilder.addIdentifierQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Identifier');
	addQuestion(group, AnswerFormatString, path, 'label', 'label:', false, answerGroups);
	addQuestion(group, AnswerFormatString, path, 'system', 'system:', false, answerGroups);
	addQuestion(group, AnswerFormatString, path, 'value', 'value:', required, answerGroups);
end;

procedure TQuestionnaireBuilder.addQuantityQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Quantity');
 	addQuestion(group, AnswerFormatChoice, path, 'comparator', 'comp:', false, answerGroups, resolveValueSet('http://hl7.org/fhir/vs/quantity-comparator'));
 	addQuestion(group, AnswerFormatDecimal, path, 'value', 'value:', required, answerGroups);
  addQuestion(group, AnswerFormatString, path, 'units', 'units:', required, answerGroups);
  addQuestion(group, AnswerFormatString, path, 'code', 'coded units:', false, answerGroups);
  addQuestion(group, AnswerFormatString, path, 'system', 'units system:', false, answerGroups);
end;

procedure TQuestionnaireBuilder.addAgeQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Age');
 	addQuestion(group, AnswerFormatChoice, path, 'comparator', 'comp:', false, answerGroups, resolveValueSet('http://hl7.org/fhir/vs/quantity-comparator'));
 	addQuestion(group, AnswerFormatDecimal, path, 'value', 'value:', required, answerGroups);
  addQuestion(group, AnswerFormatChoice, path, 'units', 'units:', required, answerGroups, resolveValueSet('http://hl7.org/fhir/vs/duration-units'));
end;

procedure TQuestionnaireBuilder.addDurationQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Duration');
	addQuestion(group, AnswerFormatDecimal, path, 'value', 'value:', false, answerGroups);
	addQuestion(group, AnswerFormatString, path, 'units', 'units:', false, answerGroups);
end;

procedure TQuestionnaireBuilder.addAttachmentQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Attachment');
//  raise Exception.Create('addAttachmentQuestions not Done Yet');
end;

procedure TQuestionnaireBuilder.addRangeQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Range');
	addQuestion(group, AnswerFormatDecimal, path, 'low', 'low:', false, answerGroups);
	addQuestion(group, AnswerFormatDecimal, path, 'high', 'high:', false, answerGroups);
	addQuestion(group, AnswerFormatString, path, 'units', 'units:', required, answerGroups);
end;

procedure TQuestionnaireBuilder.addSampledDataQuestions(group: TFHIRQuestionnaireGroup; element: TFhirProfileStructureSnapshotElement; path: String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
begin
  group.setExtensionString(TYPE_EXTENSION, 'SampledData');
end;

procedure TQuestionnaireBuilder.addScheduleQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Schedule');
//  raise Exception.Create('addScheduleQuestions not Done Yet');
end;

// Special Types ---------------------------------------------------------------

procedure TQuestionnaireBuilder.addReferenceQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; profileURL : String; answerGroups : TFhirQuestionnaireAnswersGroupList);
var
  rn : String;
  i : integer;
  q : TFhirQuestionnaireGroupQuestion;
begin
  group.setExtensionString(TYPE_EXTENSION, 'ResourceReference');

  q := addQuestion(group, AnswerFormatReference, path, 'value', group.textST, required, answerGroups);
  group.text := nil;
  if profileURL.startsWith('http://hl7.org/fhir/Profile/') then
    rn := profileURL.Substring(28)
  else
    rn := 'Any';
  if (rn = 'Any') then
    q.setExtensionString(TYPE_REFERENCE, '/_search?subject=$subj&patient=$subj&encounter=$encounter')
  else
    q.setExtensionString(TYPE_REFERENCE, '/'+rn+'?subject=$subj&patient=$subj&encounter=$encounter');
  for i := 0 to answerGroups.count - 1 do
    answerGroups[i].text := nil;
end;

procedure TQuestionnaireBuilder.addIdRefQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; answerGroups : TFhirQuestionnaireAnswersGroupList);
begin
//  raise Exception.Create('not Done Yet');
end;

procedure TQuestionnaireBuilder.addExtensionQuestions(profile : TFHIRProfile; group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; required : boolean; profileURL : String; answerGroups : TFhirQuestionnaireAnswersGroupList);
var
  extension : TFhirProfileExtensionDefn;
begin
  // is this a  profiled extension, then we add it
  if (profileURL <> '') and profiles.getExtensionDefn(profile, profileURL, profile, extension) then
  begin
    if answerGroups.count > 0 then
      raise Exception.Create('Debug this');
    if extension.elementList.Count = 1 then
      buildQuestion(group, profile, nil, extension.elementList[0], path+'.extension['+profileURL+']', answerGroups)
    else
      raise Exception.Create('Not done yet');
  end;
end;

end.


