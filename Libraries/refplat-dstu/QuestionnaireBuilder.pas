unit QuestionnaireBuilder;

interface

uses
  SysUtils,
  GUIDSupport,
  AdvObjects,
  FHIRResources, FHIRComponents, FHIRTypes;

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
    procedure processMetadata(result : TFHIRQuestionnaire; profile : TFHIRProfile; structure :TFHIRProfileStructure);
	  procedure buildGroup(group : TFHIRQuestionnaireGroup; profile : TFHIRProfile; structure :TFHIRProfileStructure; element : TFhirProfileStructureElement; parents : TFhirProfileStructureElementList);
  public

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


{ TQuestionnaireBuilder }

function TQuestionnaireBuilder.buildQuestionnaire(profile: TFHIRProfile): TFHIRQuestionnaire;
begin
  if (profile.structureList.IsEmpty) then
		raise Exception.create('buildQuestionnaire: no structure found');
	if (profile.structureList.count <> 1) then
    raise Exception.create('buildQuestionnaire: if there is more than one structure in the profile, you must choose one');
  result := buildQuestionnaire(profile, profile.structureList[0]);
end;

function TQuestionnaireBuilder.buildQuestionnaire(profile: TFHIRProfile; structure: TFhirProfileStructure): TFHIRQuestionnaire;
var
  list : TFhirProfileStructureElementList;
begin
  if (not profile.structureList.ExistsByReference(structure)) then
 		raise Exception.create('buildQuestionnaire: profile/structure mismatch');
 	if (structure.{getSnapshot().}elementList[0].Definition = nil) then
 		raise Exception.create('Found an element with no definition generating a Questionnaire');
  result := TFHIRQuestionnaire.Create();
  try
    processMetadata(result, profile, structure);
    list := TFhirProfileStructureElementList.Create;
    try
      buildGroup(result.group, profile, structure, structure.{snapshot.}elementList[0], list);
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
begin
  // todo: can we derive a more informative identifier from the questionnaire if we have a profile
  id := result.identifierList.Append;
	id.SystemST := 'urn:ietf:rfc:3986';
		if (profile.Identifier <> nil) and (profile.identifierST.contains('/profile/')) then
			id.ValueST := profile.identifierST.replace('/profile/', '/questionnaire')
		else
			id.ValueST := NewGuidURN;
//		result.VersionST := profile.VersionST;
		result.StatusST := convertStatus(profile.StatusST);
//		result.DateST := profile.DateST;
		result. := profile.PublisherST;
		result.Group := new GroupComponent());
		for (Coding t : profile.Code())
			result.Group().Concept().add(t);

end;

(*


	private void processMetadata(Questionnaire result, Profile profile, ProfileStructureComponent structure) begin
  end;

	// a group has been created. We have to fill it out based on the provided element
	// profile and structure are in context so that we can resolve references
	private void buildGroup(GroupComponent group, Profile profile, ProfileStructureComponent structure, ElementComponent element, List<ElementComponent> parents) throws Exception begin

	  group.LinkIdST := element.PathST()); // todo: this will be wrong when we start slicing
	  group.TitleST := element.Definition().ShortST()); // todo - may need to prepend the name tail...
	  group.TextST := element.Definition().FormalST());
	  group.RequiredST := element.Definition().Min().Value() > 0);
	  group.RepeatsST( := element.Definition().Max().Value().equals('1'));

	  // now, we iterate the children
	  for (ElementComponent child : ProfileUtilities.ChildList(structure, element)) begin
	  	// if the element as a type, we add a question. else we add a group on the basis that
	  	// it will have children of it's own
			if (child.Definition() = null) then
				raise Exception.create('Found an element with no definition generating a Questionnaire');

			if (!isExempt(element, child) && !parents.contains(child)) begin then
				List<ElementComponent> nparents := new ArrayList<ElementComponent>();
				nparents.addAll(parents);
				nparents.add(child);
				GroupComponent childGroup := group.addGroup();
				if (child.Definition().Type().isEmpty() ) begin then
					buildGroup(childGroup, profile, structure, child, nparents);
				end; else begin
					buildQuestion(childGroup, profile, structure, child);
				end;
			end;
	  end;
  end;

  private String tail(String path) begin
    return path.substring(path.lastIndexOf('.')+1);
  end;

	private boolean isExempt(ElementComponent element, ElementComponent child) begin
		String name := tail(child.PathST());
		String type := element.Definition().Type().isEmpty() ? '' : element.Definition().Type()[0].CodeST();

	  // we don't generate questions for the base stuff in every element
		if ('Resource'.equals(type) && then
				(name.equals('text') || name.equals('language') || name.equals('contained')))
				return true;

		// we don't generate questions for extensions
		if (name.equals('extension') || name.equals('modifierExtension') ) then
			return true;

	  return false;
  end;

	// most of the types are complex in regard to the Questionnaire, so they are still groups
	// there will be questions for each component
	private void buildQuestion(GroupComponent group, Profile profile, ProfileStructureComponent structure, ElementComponent element) throws Exception begin
    group.LinkIdST := element.PathST());
    // in this context, we don't have any concepts to mark...
    group.TextST := element.Definition().ShortST()); // prefix with name?
    group.RequiredST := element.Definition().Min().Value() > 0);
    group.RepeatsST( := element.Definition().Max().Value().equals('1'));

    if (element.Definition().Type().size() = 1) begin then
      //    	raise Exception.create('Multiple types not handled yet');

      // no we have to build the question panel for each different data type
      TypeRefComponent type := element.Definition().Type()[0];
      if (type.CodeST().equals('*')) then
        return;

      if (type.CodeST().equals('code')) then
        addCodeQuestions(group, element);
      else if (type.CodeST().equals('string') || type.CodeST().equals('id') || type.CodeST().equals('oid')) then
        addStringQuestions(group, element);
      else if (type.CodeST().equals('uri')) then
        addUriQuestions(group, element);
      else if (type.CodeST().equals('boolean')) then
        addBooleanQuestions(group, element);
      else if (type.CodeST().equals('decimal')) then
        addDecimalQuestions(group, element);
      else if (type.CodeST().equals('dateTime') || type.CodeST().equals('date')) then
        addDateTimeQuestions(group, element);
      else if (type.CodeST().equals('instant')) then
        addInstantQuestions(group, element);
      else if (type.CodeST().equals('CodeableConcept')) then
        addCodeableConceptQuestions(group, element);
      else if (type.CodeST().equals('Period')) then
        addPeriodQuestions(group, element);
      else if (type.CodeST().equals('Ratio')) then
        addRatioQuestions(group, element);
      else if (type.CodeST().equals('HumanName')) then
        addHumanNameQuestions(group, element);
      else if (type.CodeST().equals('Address')) then
        addAddressQuestions(group, element);
      else if (type.CodeST().equals('Contact')) then
        addContactQuestions(group, element);
      else if (type.CodeST().equals('Identifier')) then
        addIdentifierQuestions(group, element);
      else if (type.CodeST().equals('integer')) then
        addIntegerQuestions(group, element);
      else if (type.CodeST().equals('Coding')) then
        addCodingQuestions(group, element);
      else if (type.CodeST().equals('Quantity')) then
        addQuantityQuestions(group, element);
      else if (type.CodeST().equals('ResourceReference')) then
        addReferenceQuestions(group, element);
      else if (type.CodeST().equals('idref')) then
        addIdRefQuestions(group, element);
      else if (type.CodeST().equals('Duration')) then
        addDurationQuestions(group, element);
      else if (type.CodeST().equals('base64Binary')) then
        addBinaryQuestions(group, element);
      else if (type.CodeST().equals('Attachment')) then
        addAttachmentQuestions(group, element);
      else if (type.CodeST().equals('Range')) then
        addRangeQuestions(group, element);
      else if (type.CodeST().equals('Schedule')) then
        addScheduleQuestions(group, element);
      else if (type.CodeST().equals('Extension')) then
        addExtensionQuestions(group, element);
      else
        raise Exception.create('Unhandled Data Type: '+type.CodeST()+' on element '+element.PathST(');
    end;
  end;

	private void addExtensionQuestions(GroupComponent group, ElementComponent element) begin
    // TODO Auto-generated method stub

  end;

  private void addRangeQuestions(GroupComponent group, ElementComponent element) begin
    // TODO Auto-generated method stub

  end;

  private void addScheduleQuestions(GroupComponent group, ElementComponent element) begin
    // TODO Auto-generated method stub

  end;

  private void addDurationQuestions(GroupComponent group, ElementComponent element) begin

  end;

  private void addAttachmentQuestions(GroupComponent group, ElementComponent element) begin

  end;

  private void addBinaryQuestions(GroupComponent group, ElementComponent element) begin
    // TODO Auto-generated method stub

  end;

  private void addIdRefQuestions(GroupComponent group, ElementComponent element) begin

	end;

	private void addCodeableConceptQuestions(GroupComponent group, ElementComponent element) begin
		addQuestion(group, AnswerFormat.string, 'text', 'text:');
		if (hasValueSet(element)) then
  		addQuestion(group, AnswerFormat.openchoice, 'coding', 'code:').RepeatsST := true).Options := getValueSet(element));
		else
  		addQuestion(group, AnswerFormat.openchoice, 'coding', 'code:').RepeatsST := true);
	end;

	private void addCodingQuestions(GroupComponent group, ElementComponent element) begin
		if (hasValueSet(element)) then
  		addQuestion(group, AnswerFormat.openchoice, 'coding', 'code:').Options := getValueSet(element));
		else
  		addQuestion(group, AnswerFormat.openchoice, 'coding', 'code:');
	end;

	private void addPeriodQuestions(GroupComponent group, ElementComponent element) begin
		addQuestion(group, AnswerFormat.dateTime, 'low', 'start:');
		addQuestion(group, AnswerFormat.dateTime, 'end', 'end:');
	end;

	private void addHumanNameQuestions(GroupComponent group, ElementComponent element) begin
		addQuestion(group, AnswerFormat.string, 'text', 'text:');
		addQuestion(group, AnswerFormat.string, 'family', 'family:').RepeatsST := true);
		addQuestion(group, AnswerFormat.string, 'given', 'given:').RepeatsST := true);
	end;

	private void addAddressQuestions(GroupComponent group, ElementComponent element) begin
		addQuestion(group, AnswerFormat.string, 'text', 'text:');
		addQuestion(group, AnswerFormat.string, 'line', 'line:').RepeatsST := true);
		addQuestion(group, AnswerFormat.string, 'city', 'city:');
		addQuestion(group, AnswerFormat.string, 'state', 'state:');
		addQuestion(group, AnswerFormat.string, 'zip', 'zip:');
		addQuestion(group, AnswerFormat.string, 'country', 'country:');
	end;

	private void addContactQuestions(GroupComponent group, ElementComponent element) begin
		addQuestion(group, AnswerFormat.choice, 'type', 'type:').Options := new ResourceReference().ReferenceST( := http://hl7.org/fhir/vs/contact-system'));
		addQuestion(group, AnswerFormat.string, 'value', 'value:');
	end;

	private void addQuantityQuestions(GroupComponent group, ElementComponent element) begin
		addQuestion(group, AnswerFormat.choice, 'comparator', 'comp:').Options := new ResourceReference().ReferenceST( := http://hl7.org/fhir/vs/quantity-comparator'));
		addQuestion(group, AnswerFormat.decimal, 'value', 'value:');
		addQuestion(group, AnswerFormat.string, 'units', 'units:');
	end;

	private void addRatioQuestions(GroupComponent group, ElementComponent element) begin
		addQuestion(group, AnswerFormat.decimal, 'value', 'low:');
		addQuestion(group, AnswerFormat.decimal, 'value', 'high:');
		addQuestion(group, AnswerFormat.string, 'units', 'units:');
	end;

	private void addIdentifierQuestions(GroupComponent group, ElementComponent element) begin
		addQuestion(group, AnswerFormat.string, 'label', 'label:');
		addQuestion(group, AnswerFormat.string, 'system', 'system:');
		addQuestion(group, AnswerFormat.string, 'value', 'value:');
	end;

	private void addReferenceQuestions(GroupComponent group, ElementComponent element) begin
		// todo
		addQuestion(group, AnswerFormat.string, 'reference', 'url:');
	end;

	private void addStringQuestions(GroupComponent group, ElementComponent element) begin
		addQuestion(group, AnswerFormat.string, 'string', 'value');
	end;

	private void addDecimalQuestions(GroupComponent group, ElementComponent element) begin
		addQuestion(group, AnswerFormat.decimal, 'string', 'value');
	end;

	private void addIntegerQuestions(GroupComponent group, ElementComponent element) begin
		addQuestion(group, AnswerFormat.integer, 'string', 'value');
	end;

	private void addUriQuestions(GroupComponent group, ElementComponent element) begin
		addQuestion(group, AnswerFormat.string, 'string', 'url:');
	end;

	private void addDateTimeQuestions(GroupComponent group, ElementComponent element) begin
		addQuestion(group, AnswerFormat.dateTime, 'dateTime', 'date/time:');
	end;

	private void addInstantQuestions(GroupComponent group, ElementComponent element) begin
		addQuestion(group, AnswerFormat.instant, 'instant', 'value:');
	end;

	private void addBooleanQuestions(GroupComponent group, ElementComponent element) begin
		addQuestion(group, AnswerFormat.boolean_, 'boolean', 'value');
	end;

	private void addCodeQuestions(GroupComponent group, ElementComponent element) begin
		if (hasValueSet(element)) then
			addQuestion(group, AnswerFormat.string, 'code', 'code:');
		else
			addQuestion(group, AnswerFormat.choice, 'code', 'code:').Options := getValueSet(element));
  end;

	private ResourceReference getValueSet(ElementComponent element) begin
	  if (element = null|| element.Definition() = null || element.Definition().Binding() = null || element.Definition().Binding().Reference() instanceof Uri) then
	    return null;
	  else
	    return (ResourceReference) element.Definition().Binding().Reference();
  end;

	private boolean hasValueSet(ElementComponent element) begin
	  return element.Definition().Binding() = null || !(element.Definition().Binding().Reference() instanceof ResourceReference);
  end;

	private QuestionComponent addQuestion(GroupComponent group, AnswerFormat af, String id, String name) begin
	  QuestionComponent q := group.addQuestion();
	  q.LinkIdST := id);
	  q.TextST := name);
	  q.TypeST := af);
	  q.RequiredST := true);
	  q.RepeatsST := false);
	  return q;
  end;

	// FHIR context. You don't have to provide any of these, but
	// the more you provide, the better the conversion will be
  private TerminologyServices conceptLocator;
  private Map<String, AtomEntry<ValueSet>> codeSystems;
  private Map<String, AtomEntry<ValueSet>> valueSets;
  private Map<String, AtomEntry<ConceptMap>> maps;
  private FHIRClient client;
  private Map<String, Profile> profiles;
	public TerminologyServices getConceptLocator() begin
		return conceptLocator;
	end;
	public void setConceptLocator(TerminologyServices conceptLocator) begin
		this.conceptLocator := conceptLocator;
	end;
	public Map<String, AtomEntry<ValueSet>> getCodeSystems() begin
		return codeSystems;
	end;
	public void setCodeSystems(Map<String, AtomEntry<ValueSet>> codeSystems) begin
		this.codeSystems := codeSystems;
	end;
	public Map<String, AtomEntry<ValueSet>> getValueSets() begin
		return valueSets;
	end;
	public void setValueSets(Map<String, AtomEntry<ValueSet>> valueSets) begin
		this.valueSets := valueSets;
	end;
	public Map<String, AtomEntry<ConceptMap>> getMaps() begin
		return maps;
	end;
	public void setMaps(Map<String, AtomEntry<ConceptMap>> maps) begin
		this.maps := maps;
	end;
	public FHIRClient getClient() begin
		return client;
	end;
	public void setClient(FHIRClient client) begin
		this.client := client;
	end;
	public Map<String, Profile> getProfiles() begin
		return profiles;
	end;
	public void setProfiles(Map<String, Profile> profiles) begin
		this.profiles := profiles;
	end;

*)

procedure TQuestionnaireBuilder.buildGroup(group: TFHIRQuestionnaireGroup;
  profile: TFHIRProfile; structure: TFHIRProfileStructure;
  element: TFhirProfileStructureElement;
  parents: TFhirProfileStructureElementList);
begin
    !
end;


end.
