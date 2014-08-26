unit QuestionnaireBuilder;

interface

uses
  SysUtils,
  GUIDSupport,
  DateAndTime,
  AdvObjects,
  FHIRResources, FHIRComponents, FHIRTypes,
  FHIRUtilities, ProfileManager;

Const
  TYPE_EXTENSION = 'http://www.healthintersections.com.au/fhir/Profile/metadata#type';
  TYPE_REFERENCE = 'http://www.healthintersections.com.au/fhir/Profile/metadata#reference';
  FLYOVER_REFERENCE = 'http://hl7.org/fhir/Profile/questionnaire-extensions#flyover';

Type
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

    function nextId : String;

    function getChildList(structure :TFHIRProfileStructure; path : String) : TFhirProfileStructureSnapshotElementList; overload;
    function getChildList(structure :TFHIRProfileStructure; element : TFhirProfileStructureSnapshotElement) : TFhirProfileStructureSnapshotElementList; overload;
    function isExempt(element, child: TFhirProfileStructureSnapshotElement) : boolean;

    function expandTypeList(types: TFhirProfileStructureSnapshotElementDefinitionTypeList): TFhirProfileStructureSnapshotElementDefinitionTypeList;
    function makeTypeList(questionnaire : TFHIRQuestionnaire; profile : TFHIRProfile; types : TFhirProfileStructureSnapshotElementDefinitionTypeList; path : String) : TFhirResourceReference;
    procedure buildQuestion(questionnaire : TFHIRQuestionnaire; group : TFHIRQuestionnaireGroup; profile : TFHIRProfile; structure :TFHIRProfileStructure; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure processDataType(questionnaire : TFhirQuestionnaire; profile : TFHIRProfile; group: TFHIRQuestionnaireGroup; element: TFhirProfileStructureSnapshotElement; path: String; t: TFhirProfileStructureSnapshotElementDefinitionType);


    procedure addAddressQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addAgeQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addAttachmentQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addBinaryQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addBooleanQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addCodeQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addCodeableConceptQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addCodingQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addContactQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addDateTimeQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addDecimalQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addDurationQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addExtensionQuestions(questionnaire : TFhirQuestionnaire; profile : TFHIRProfile; group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; profileURL : String);
    procedure addHumanNameQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addIdRefQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addIdentifierQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addInstantQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addIntegerQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addPeriodQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addQuantityQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addRangeQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addRatioQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addReferenceQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; profileURL : String);
    procedure addSampledDataQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addScheduleQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addStringQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addTimeQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    procedure addUriQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
    function addQuestion(group: TFHIRQuestionnaireGroup; af: TFhirAnswerFormat; path, id, name: String): TFhirQuestionnaireGroupQuestion;

    procedure processMetadata(result : TFHIRQuestionnaire; profile : TFHIRProfile; structure :TFHIRProfileStructure);
	  procedure buildGroup(questionnaire : TFHIRQuestionnaire; group : TFHIRQuestionnaireGroup; profile : TFHIRProfile; structure :TFHIRProfileStructure; element : TFhirProfileStructureSnapshotElement; parents : TFhirProfileStructureSnapshotElementList);
    procedure SetProfiles(const Value: TProfileManager);
  public
    Destructor Destroy; override;

    Property Profiles : TProfileManager read FProfiles write SetProfiles;

   {
  	 * Given a profile, build a questionnaire.
     *
  	 * The profile must have a single structure in it
  	 *
  	 * @param profile the profile to build a questionnaire from
  	 * @return the questionnaire that represents the profile
   }
	 function buildQuestionnaire(profile : TFHIRProfile) : TFHIRQuestionnaire; overload;

   {
	   * Given a profile with a selected structure, build a questionnaire.
	   *
   	 * @param profile the profile to build a questionnaire from
   	 * @return the questionnaire that represents the profile
   	 * @throws Exception
   }
   function buildQuestionnaire(profile : TFHIRProfile; structure : TFhirProfileStructure) : TFHIRQuestionnaire; overload;

  end;

implementation


Function tail(path : String) : String;
begin
  result := path.substring(path.lastIndexOf('.')+1);
end;


{ TQuestionnaireBuilder }

function TQuestionnaireBuilder.buildQuestionnaire(profile: TFHIRProfile): TFHIRQuestionnaire;
var
  entry : TFhirProfileStructure;
  i : integer;
begin
  if (profile.structureList.IsEmpty) then
		raise Exception.create('buildQuestionnaire: no structure found');

  entry := nil;
  for i := 0 to profile.structureList.Count - 1 do
    if profile.structureList[i].publishST then
      if entry = nil then
        entry := profile.structureList[i]
      else
        raise Exception.create('buildQuestionnaire: if there is more than one published structure in the profile, you must choose one');
  if (entry = nil) then
		raise Exception.create('buildQuestionnaire: no published structure found');
  result := buildQuestionnaire(profile, entry);
end;

function TQuestionnaireBuilder.buildQuestionnaire(profile: TFHIRProfile; structure: TFhirProfileStructure): TFHIRQuestionnaire;
var
  list : TFhirProfileStructureSnapshotElementList;
begin
  if (not profile.structureList.ExistsByReference(structure)) then
 		raise Exception.create('buildQuestionnaire: profile/structure mismatch');
 	if (structure.snapshot.elementList[0].Definition = nil) then
 		raise Exception.create('Found an element with no definition generating a Questionnaire');
  result := TFHIRQuestionnaire.Create();
  try
    processMetadata(result, profile, structure);
    list := TFhirProfileStructureSnapshotElementList.Create;
    try
      buildGroup(result, result.group, profile, structure, structure.snapshot.elementList[0], list);
    finally
      list.Free;
    end;
  {  if (profiles <> nil) then
    begin
      NarrativeGenerator ngen := new NarrativeGenerator("", conceptLocator, codeSystems, valueSets, maps, profiles, client);
      ngen.generate(result);
    end;
    return result;
  }
    result.Link;
  finally
    result.Free;
  end;
end;

destructor TQuestionnaireBuilder.Destroy;
begin
  FProfiles.Free;
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


procedure TQuestionnaireBuilder.processMetadata(result: TFHIRQuestionnaire; profile: TFHIRProfile; structure: TFHIRProfileStructure);
var
  id : TFhirIdentifier;
  i : integer;
begin
  // todo: can we derive a more informative identifier from the questionnaire if we have a profile
  id := result.identifierList.Append;
	id.SystemST := 'urn:ietf:rfc:3986';
  if (profile.URL <> nil) and (profile.URLST.contains('/profile/')) then
    id.ValueST := profile.URLST.replace('/profile/', '/questionnaire')
  else
    id.ValueST := NewGuidURN;
  result.VersionST := profile.VersionST;
  result.StatusST := convertStatus(profile.StatusST);
  result.DateST := profile.DateST.link;
  result.publisherST := profile.PublisherST;
  result.Group := TFhirQuestionnaireGroup.Create;
  result.group.conceptList.AddAll(profile.codeList);
end;


procedure TQuestionnaireBuilder.SetProfiles(const Value: TProfileManager);
begin
  FProfiles.Free;
  FProfiles := Value;
end;

procedure TQuestionnaireBuilder.buildGroup(questionnaire : TFHIRQuestionnaire; group: TFHIRQuestionnaireGroup; profile: TFHIRProfile; structure: TFHIRProfileStructure; element: TFhirProfileStructureSnapshotElement; parents: TFhirProfileStructureSnapshotElementList);
var
  i : integer;
  list : TFhirProfileStructureSnapshotElementList;
  child : TFhirProfileStructureSnapshotElement;
  nparents : TFhirProfileStructureSnapshotElementList;
  childGroup : TFHIRQuestionnaireGroup;
begin
  group.LinkIdST := element.PathST; // todo: this will be wrong when we start slicing
  group.TitleST := element.Definition.ShortST; // todo - may need to prepend the name tail...
  group.TextST := element.Definition.commentsST;
  group.SetExtensionString(FLYOVER_REFERENCE, element.Definition.FormalST);
  group.RequiredST := element.Definition.Min.Value > '0';
  group.RepeatsST := element.Definition.Max.Value <> '1';

  // now, we iterate the children
  list := getChildList(structure, element);
  try
    for i := 0 to list.Count - 1 do
    begin
      child := list[i];

	  	// if the element as a type, we add a question. else we add a group on the basis that
	  	// it will have children of it's own
			if (child.Definition = nil) then
				raise Exception.create('Found an element with no definition generating a Questionnaire');

			if (not isExempt(element, child)) and (not parents.ExistsByReference(child)) then
      begin
        nparents := TFhirProfileStructureSnapshotElementList.Create;
        try
          nparents.Assign(parents);
          nparents.add(child.link);
          childGroup := group.groupList.Append;
          if (child.Definition.type_List.isEmpty) then
            buildGroup(questionnaire, childGroup, profile, structure, child, nparents)
          else
            buildQuestion(questionnaire, childGroup, profile, structure, child, child.pathST);
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

function TQuestionnaireBuilder.makeTypeList(questionnaire: TFHIRQuestionnaire; profile : TFHIRProfile; types: TFhirProfileStructureSnapshotElementDefinitionTypeList; path : String): TFhirResourceReference;
var
  vs : TFhirValueset;
  i : integer;
  t : TFhirProfileStructureSnapshotElementDefinitionType;
  cc : TFhirValueSetDefineConcept;
  structure : TFhirProfileStructure;
begin
  vs := TFhirValueset.Create;
  try
    vs.nameST := 'Type options for '+path;
    vs.descriptionST := vs.nameST;
    vs.statusST := ValuesetStatusActive;
    vs.define := TFhirValueSetDefine.Create;
    vs.define.systemST := NewGuidURN;
    for i := 0 to types.Count - 1 do
    begin
      t := types[i];
      cc := vs.define.conceptList.Append;
      if (t.codeST = 'ResourceReference') and (t.profileST.startsWith('http://hl7.org/fhir/Profile/')) then
      begin
        cc.codeST := t.profileST.Substring(28);
        cc.displayST := cc.codeST;
      end
      else if (t.profileST <> '') and FProfiles.getStructure(profile, t.profileST, profile, structure) then
      begin
        cc.codeST := t.profileST;
        cc.displayST := structure.nameST;
      end
      else
      begin
        cc.codeST := t.codeST;
        cc.displayST := t.codeST;
      end;
      t.TagValue := cc.codeST;
    end;
    vs.xmlId := nextId;
    questionnaire.containedList.Add(vs.Link);
    result := TFhirResourceReference.Create;
    result.referenceST := '#'+vs.xmlId;
  finally
    vs.Free;
  end;
end;

function TQuestionnaireBuilder.nextId: String;
begin
  inc(lastid);
  result := 'vs'+inttostr(lastid);
end;

// most of the types are complex in regard to the Questionnaire, so they are still groups
	// there will be questions for each component
procedure TQuestionnaireBuilder.buildQuestion(questionnaire : TFHIRQuestionnaire; group : TFHIRQuestionnaireGroup; profile : TFHIRProfile; structure :TFHIRProfileStructure; element : TFhirProfileStructureSnapshotElement; path : String);
var
  t : TFhirProfileStructureSnapshotElementDefinitionType;
  q : TFhirQuestionnaireGroupQuestion;
  types : TFhirProfileStructureSnapshotElementDefinitionTypeList;
  i : integer;
  sub : TFHIRQuestionnaireGroup;
begin
  group.LinkIdST := path;

  // in this context, we don't have any concepts to mark...
  group.TextST := element.Definition.ShortST; // prefix with name?
  group.RequiredST := element.Definition.Min.Value > '0';
  group.RepeatsST := element.Definition.Max.Value <> '1';

  if element.Definition.commentsST <> '' then
    group.setExtensionString(FLYOVER_REFERENCE, element.Definition.FormalST+' '+element.Definition.commentsST)
  else
    group.setExtensionString(FLYOVER_REFERENCE, element.Definition.FormalST);

  if (element.Definition.type_List.Count > 1) or (element.Definition.type_List[0].CodeST = '*') then
  begin
    q := addQuestion(group, AnswerFormatChoice, element.pathST, '_type', 'type');
    types := expandTypeList(element.definition.type_List);
    try
      q.options := makeTypeList(questionnaire, profile, types, element.pathST);
      for i := 0 to types.Count - 1 do
      begin
        t := types[i];
        sub := q.groupList.Append;
        sub.LinkIdST := element.PathST+'._'+t.tagValue;
        sub.TextST := t.TagValue;
        // always optional, never repeats
        processDataType(questionnaire, profile, sub, element, element.PathST+'._'+t.tagValue, t);
      end;
    finally
      types.free;
    end;
  end
  else
    // now we have to build the question panel for each different data type
    processDataType(questionnaire, profile, group, element, element.PathST, element.Definition.Type_list[0]);
end;

procedure TQuestionnaireBuilder.processDataType(questionnaire : TFhirQuestionnaire; profile : TFHIRProfile; group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; t : TFhirProfileStructureSnapshotElementDefinitionType);
begin
  if (t.CodeST = 'code') then
    addCodeQuestions(group, element, path)
  else if (t.CodeST = 'string') or (t.CodeST = 'id') or (t.CodeST = 'oid') then
    addStringQuestions(group, element, path)
  else if (t.CodeST = 'uri') then
    addUriQuestions(group, element, path)
  else if (t.CodeST = 'boolean') then
    addBooleanQuestions(group, element, path)
  else if (t.CodeST = 'decimal') then
    addDecimalQuestions(group, element, path)
  else if (t.CodeST = 'dateTime') or (t.CodeST = 'date') then
    addDateTimeQuestions(group, element, path)
  else if (t.CodeST = 'instant') then
    addInstantQuestions(group, element, path)
  else if (t.CodeST = 'time') then
    addTimeQuestions(group, element, path)
  else if (t.CodeST = 'CodeableConcept') then
    addCodeableConceptQuestions(group, element, path)
  else if (t.CodeST = 'Period') then
    addPeriodQuestions(group, element, path)
  else if (t.CodeST = 'Ratio') then
    addRatioQuestions(group, element, path)
  else if (t.CodeST = 'HumanName') then
    addHumanNameQuestions(group, element, path)
  else if (t.CodeST = 'Address') then
    addAddressQuestions(group, element, path)
  else if (t.CodeST = 'Contact') then
    addContactQuestions(group, element, path)
  else if (t.CodeST = 'Identifier') then
    addIdentifierQuestions(group, element, path)
  else if (t.CodeST = 'integer') then
    addIntegerQuestions(group, element, path)
  else if (t.CodeST = 'Coding') then
    addCodingQuestions(group, element, path)
  else if (t.CodeST = 'Quantity') then
    addQuantityQuestions(group, element, path)
  else if (t.CodeST = 'ResourceReference') then
    addReferenceQuestions(group, element, path, t.profileST)
  else if (t.CodeST = 'idref') then
    addIdRefQuestions(group, element, path)
  else if (t.CodeST = 'Duration') then
    addDurationQuestions(group, element, path)
  else if (t.CodeST = 'base64Binary') then
    addBinaryQuestions(group, element, path)
  else if (t.CodeST = 'Attachment') then
    addAttachmentQuestions(group, element, path)
  else if (t.CodeST = 'Age') then
    addAgeQuestions(group, element, path)
  else if (t.CodeST = 'Range') then
    addRangeQuestions(group, element, path)
  else if (t.CodeST = 'Schedule') then
    addScheduleQuestions(group, element, path)
  else if (t.CodeST = 'SampledData') then
    addSampledDataQuestions(group, element, path)
  else if (t.CodeST = 'Extension') then
    addExtensionQuestions(questionnaire, profile, group, element, path, t.profileST)
  else
    raise Exception.create('Unhandled Data Type: '+t.CodeST+' on element '+element.PathST);
end;

function TQuestionnaireBuilder.addQuestion(group : TFHIRQuestionnaireGroup; af : TFhirAnswerFormat; path, id, name : String) : TFhirQuestionnaireGroupQuestion;
begin
 result := group.questionList.Append;
 result.LinkIdST := path+'.'+id;
 result.TextST := name;
 result.Type_ST := af;
 result.RequiredST := false;
 result.RepeatsST := false;
end;

function hasValueSet(element : TFhirProfileStructureSnapshotElement) : boolean;
begin
	result := (element.Definition.Binding <> nil) and (element.Definition.Binding.Reference is TFHIRResourceReference);
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

procedure TQuestionnaireBuilder.addCodeQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
var
  q : TFhirQuestionnaireGroupQuestion;
begin
  group.setExtensionString(TYPE_EXTENSION, 'code');
  if (hasValueSet(element)) then
  begin
    q := addQuestion(group, AnswerFormatChoice, path, 'value', unCamelCase(Tail(element.pathST)));
    q.options := (element.Definition.Binding.Reference as TFhirResourceReference).Link;
  end
  else
    q := addQuestion(group, AnswerFormatString, path, 'value', group.textST);
  group.text := nil;
end;

// Primitives ------------------------------------------------------------------
procedure TQuestionnaireBuilder.addStringQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
begin
  group.setExtensionString(TYPE_EXTENSION, 'string');
  addQuestion(group, AnswerFormatString, path, 'value', group.textST);
  group.text := nil;
end;

procedure TQuestionnaireBuilder.addTimeQuestions(group: TFHIRQuestionnaireGroup; element: TFhirProfileStructureSnapshotElement; path: String);
begin
  group.setExtensionString(TYPE_EXTENSION, 'time');
  addQuestion(group, AnswerFormatTime, path, 'value', group.textST);
  group.text := nil;
end;

procedure TQuestionnaireBuilder.addUriQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
begin
  group.setExtensionString(TYPE_EXTENSION, 'uri');
  addQuestion(group, AnswerFormatString, path, 'value', group.textST);
  group.text := nil;
end;

procedure TQuestionnaireBuilder.addBooleanQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
begin
  group.setExtensionString(TYPE_EXTENSION, 'boolean');
	addQuestion(group, AnswerFormatBoolean, path, 'value', group.textST);
  group.text := nil;
end;

procedure TQuestionnaireBuilder.addDecimalQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
begin
  group.setExtensionString(TYPE_EXTENSION, 'decimal');
  addQuestion(group, AnswerFormatDecimal, path, 'value', group.textST);
  group.text := nil;
end;

procedure TQuestionnaireBuilder.addIntegerQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
begin
  group.setExtensionString(TYPE_EXTENSION, 'integer');
  addQuestion(group, AnswerFormatInteger, path, 'value', group.textST);
  group.text := nil;
end;

procedure TQuestionnaireBuilder.addDateTimeQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
begin
  group.setExtensionString(TYPE_EXTENSION, 'datetime');
  addQuestion(group, AnswerFormatDateTime, path, 'value', group.textST);
  group.text := nil;
end;

procedure TQuestionnaireBuilder.addInstantQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
begin
  group.setExtensionString(TYPE_EXTENSION, 'instant');
  addQuestion(group, AnswerFormatInstant, path, 'value', group.textST);
  group.text := nil;
end;

procedure TQuestionnaireBuilder.addBinaryQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
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

procedure TQuestionnaireBuilder.addCodingQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
var
  q : TFhirQuestionnaireGroupQuestion;
begin
  group.setExtensionString(TYPE_EXTENSION, 'Coding');

  q := addQuestion(group, AnswerTypeForBinding(element.Definition.Binding), path, 'value', 'Code:');
  if (hasValueSet(element)) then
    q.options := (element.Definition.Binding.Reference as TFhirResourceReference).Link;
  // lloyd: what here...?
end;

procedure TQuestionnaireBuilder.addCodeableConceptQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
var
  question : TFhirQuestionnaireGroupQuestion;
begin
  group.setExtensionString(TYPE_EXTENSION, 'CodeableConcept');
  if (hasValueSet(element)) then
  begin
    question := addQuestion(group, AnswerTypeForBinding(element.Definition.Binding), path, 'coding', 'code:');
    question.RepeatsST := true;
    question.options := (element.Definition.Binding.Reference as TFhirResourceReference).Link;
  end
  else
  	addQuestion(group, AnswerFormatOpenchoice, path, 'coding', 'code:').RepeatsST := true;
	addQuestion(group, AnswerFormatString, path, 'text', 'text:');
end;

procedure TQuestionnaireBuilder.addPeriodQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Period');
	addQuestion(group, AnswerFormatDateTime, path, 'low', 'start:');
	addQuestion(group, AnswerFormatDateTime, path, 'end', 'end:');
end;

procedure TQuestionnaireBuilder.addRatioQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Ratio');
	addQuestion(group, AnswerFormatDecimal, path, 'numerator', 'numerator:');
	addQuestion(group, AnswerFormatDecimal, path, 'denominator', 'denominator:');
	addQuestion(group, AnswerFormatString, path, 'units', 'units:');
end;

procedure TQuestionnaireBuilder.addHumanNameQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Name');
	addQuestion(group, AnswerFormatString, path, 'text', 'text:');
	addQuestion(group, AnswerFormatString, path, 'family', 'family:').RepeatsST := true;
	addQuestion(group, AnswerFormatString, path, 'given', 'given:').RepeatsST := true;
end;

procedure TQuestionnaireBuilder.addAddressQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Address');
  addQuestion(group, AnswerFormatString, path, 'text', 'text:');
  addQuestion(group, AnswerFormatString, path, 'line', 'line:').RepeatsST := true;
  addQuestion(group, AnswerFormatString, path, 'city', 'city:');
  addQuestion(group, AnswerFormatString, path, 'state', 'state:');
  addQuestion(group, AnswerFormatString, path, 'zip', 'zip:');
  addQuestion(group, AnswerFormatString, path, 'country', 'country:');
end;

procedure TQuestionnaireBuilder.addContactQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
var
  ref : TFhirResourceReference;
begin
  group.setExtensionString(TYPE_EXTENSION, 'Contact');
  ref := TFhirResourceReference.Create;
	addQuestion(group, AnswerFormatChoice, path, 'type', 'type:').Options := ref;
  ref.referenceST := 'http://hl7.org/fhir/vs/contact-system';
	addQuestion(group, AnswerFormatString, path, 'value', 'value:');
end;

procedure TQuestionnaireBuilder.addIdentifierQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Identifier');
	addQuestion(group, AnswerFormatString, path, 'label', 'label:');
	addQuestion(group, AnswerFormatString, path, 'system', 'system:');
	addQuestion(group, AnswerFormatString, path, 'value', 'value:');
end;

procedure TQuestionnaireBuilder.addQuantityQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
var
  ref : TFhirResourceReference;
begin
  group.setExtensionString(TYPE_EXTENSION, 'Quantity');
  ref := TFhirResourceReference.Create;
 	addQuestion(group, AnswerFormatChoice, path, 'comparator', 'comp:').Options := ref;
  ref.referenceST := 'http://hl7.org/fhir/vs/quantity-comparator';
 	addQuestion(group, AnswerFormatDecimal, path, 'value', 'value:');
  addQuestion(group, AnswerFormatString, path, 'units', 'units:');
end;

procedure TQuestionnaireBuilder.addAgeQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
var
  ref : TFhirResourceReference;
begin
  group.setExtensionString(TYPE_EXTENSION, 'Age');
  ref := TFhirResourceReference.Create;
 	addQuestion(group, AnswerFormatChoice, path, 'comparator', 'comp:').Options := ref;
  ref.referenceST := 'http://hl7.org/fhir/vs/quantity-comparator';
 	addQuestion(group, AnswerFormatDecimal, path, 'value', 'value:');
  ref := TFhirResourceReference.Create;
  ref.referenceST := 'http://hl7.org/fhir/vs/duration-units';
  addQuestion(group, AnswerFormatChoice, path, 'units', 'units:');
end;

procedure TQuestionnaireBuilder.addDurationQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Duration');
	addQuestion(group, AnswerFormatDecimal, path, 'value', 'value:');
	addQuestion(group, AnswerFormatString, path, 'units', 'units:');
end;

procedure TQuestionnaireBuilder.addAttachmentQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Attachment');
//  raise Exception.Create('addAttachmentQuestions not Done Yet');
end;

procedure TQuestionnaireBuilder.addRangeQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Range');
	addQuestion(group, AnswerFormatDecimal, path, 'low', 'low:');
	addQuestion(group, AnswerFormatDecimal, path, 'high', 'high:');
	addQuestion(group, AnswerFormatString, path, 'units', 'units:');
end;

procedure TQuestionnaireBuilder.addSampledDataQuestions(group: TFHIRQuestionnaireGroup; element: TFhirProfileStructureSnapshotElement; path: String);
begin
  group.setExtensionString(TYPE_EXTENSION, 'SampledData');
end;

procedure TQuestionnaireBuilder.addScheduleQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
begin
  group.setExtensionString(TYPE_EXTENSION, 'Schedule');
//  raise Exception.Create('addScheduleQuestions not Done Yet');
end;

// Special Types ---------------------------------------------------------------

procedure TQuestionnaireBuilder.addReferenceQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; profileURL : String);
var
  rn : String;
begin
  group.setExtensionString(TYPE_EXTENSION, 'ResourceReference');
  if profileURL.startsWith('http://hl7.org/fhir/Profile/') then
    rn := profileURL.Substring(28)
  else
    rn := 'Any';
  if (rn = 'Any') then
    group.setExtensionString(TYPE_REFERENCE, '/_search?subject=$subj&patient=$subj&encounter=$encounter')
  else
    group.setExtensionString(TYPE_REFERENCE, '/'+rn+'?subject=$subj&patient=$subj&encounter=$encounter');
  addQuestion(group, AnswerFormatReference, path, 'value', group.textST);
  group.text := nil;
end;

procedure TQuestionnaireBuilder.addIdRefQuestions(group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String);
begin
//  raise Exception.Create('not Done Yet');
end;

procedure TQuestionnaireBuilder.addExtensionQuestions(questionnaire : TFhirQuestionnaire; profile : TFHIRProfile; group : TFHIRQuestionnaireGroup; element : TFhirProfileStructureSnapshotElement; path : String; profileURL : String);
var
  extension : TFhirProfileExtensionDefn;
begin
  // is this a  profiled extension, then we add it
  if (profileURL <> '') and profiles.getExtensionDefn(profile, profileURL, profile, extension) then
  begin
    if extension.elementList.Count = 1 then
      buildQuestion(questionnaire, group, profile, nil, extension.elementList[0], path+'.extension['+profileURL+']')
    else
      raise Exception.Create('Not done yet');
  end;
end;

end.
