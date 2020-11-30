unit fhir3_narrative2;

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
  SysUtils, Generics.Collections,
  fsl_base, fsl_utilities,
  fhir_objects, fhir_xhtml,  fhir_utilities,
  fhir3_resources, fhir3_types, fhir3_constants, fhir3_utilities, fhir3_profiles, fhir3_questionnaire;

type

  TNarrativeGenerator = class (TFslObject)
  private
    Fprefix : String;
    FProfiles : TProfileManager;
    FOnLookupCode : TLookupCodeEvent;
    FOnLookupReference : TLookupReferenceEvent;
    FContext : TFslObject;

    procedure generateByProfile(res : TFHIRDomainResource; e : TFHIRObject; allElements : TFhirElementDefinitionList; defn : TFhirElementDefinition; children : TFhirElementDefinitionList; x : TFHIRXhtmlNode; path : String; showCodeDetails : boolean);
    function getChildrenForPath(elements : TFhirElementDefinitionList; path : String) : TFhirElementDefinitionList;
    procedure inject(res : TFHIRDomainResource; x : TFHIRXhtmlNode; status : TFhirNarrativeStatusEnum);
    procedure renderLeaf(res : TFHIRResource; e : TFHIRObject; defn : TFhirElementDefinition; x : TFHIRXhtmlNode; title, showCodeDetails : boolean; displayHints : TFslStringDictionary);
    procedure readDisplayHints(defn : TFhirElementDefinition; hints : TFslStringDictionary);
    function getElementDefinition(elements : TFhirElementDefinitionList; path : String) : TFhirElementDefinition;
    function exemptFromRendering(child : TFhirElementDefinition) : boolean;
    function isDefaultValue(displayHints : TFslStringDictionary; list: TFHIRObjectList) : boolean;
    function renderAsList(child : TFhirElementDefinition) : boolean;
    function canDoTable(path : String; p : TFHIRProperty; grandChildren  : TFhirElementDefinitionList) : boolean;
    procedure addColumnHeadings(tr : TFHIRXhtmlNode; grandChildren : TFhirElementDefinitionList);
    procedure addColumnValues(res : TFHIRResource; tr : TFHIRXhtmlNode; grandChildren : TFhirElementDefinitionList; v : TFHIRElement; showCodeDetails : boolean; displayHints : TFslStringDictionary);
    function isDefault(displayHints : TFslStringDictionary; primitiveType : TFHIRPrimitiveType) : boolean;
    procedure getValues(path : string; p : TFHIRProperty; e : TFhirElementDefinition; list : TFHIRObjectList);
    function canCollapse(e : TFhirElementDefinition) : boolean;
    procedure generateResourceSummary(x : TFHIRXhtmlNode; res : TFHIRResourceV; textAlready, showCodeDetails : boolean);
    function includeInSummary(child : TFhirElementDefinition): boolean;
    function displayLeaf(res : TFHIRResource; e : TFHIRElement; defn : TFhirElementDefinition; x : TFHIRXhtmlNode; name : String; showCodeDetails : boolean) : boolean;

    function isPrimitive(e : TFhirElementDefinition) : boolean; overload;
    function capitalize(s : String):String;
    function camelCase(s : String):String;
    function tail(s : String):String;
    function pluralizeMe(s : String):String;

    procedure renderCodeableConcept(v : TFHIRCodeableConcept; x : TFHIRXhtmlNode; showCodeDetails : boolean);
    procedure renderCoding(v : TFHIRCoding; x : TFHIRXhtmlNode; showCodeDetails : boolean);
    procedure renderIdentifier(v : TFHIRIdentifier; x : TFHIRXhtmlNode);
    procedure renderAddress(v : TFHIRAddress; x : TFHIRXhtmlNode);
    procedure renderHumanName(v : TFHIRHumanName; x : TFHIRXhtmlNode);
    procedure renderContact(v : TFHIRContactPoint; x : TFHIRXhtmlNode);
    procedure renderUri(v : TFHIRUri; x : TFHIRXhtmlNode);
    procedure renderSchedule(v : TFHIRTiming; x : TFHIRXhtmlNode);
    procedure renderQuantity(v : TFHIRQuantity; x : TFHIRXhtmlNode; showCodeDetails : boolean);
    function displayIdentifier(v : TFHIRIdentifier) : String;
    function displayAddress(v : TFHIRAddress) : String;
    function displayHumanName(v : TFHIRHumanName) : String;
    function displayContact(v : TFHIRContactPoint) : String;
    function displaySchedule(v : TFHIRTiming) : String;
    function displayCodeable(v : TFHIRCodeableConcept) : String;

    function resolveReference(res : TFHIRResource; url : String) : TResourceWithReference;
    function lookupCode(system, version, code : String) : String;
    function describeSystem(system : String) : String; overload;
    function describeSystem(system : TFHIRContactPointSystemEnum) : String;  overload;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(prefix : String; profiles : TProfileManager; onLookpuCode : TLookupCodeEvent; onLookpuReference : TLookupReferenceEvent; context : TFslObject);
    destructor Destroy; Override;

    Procedure generate(r : TFHIRDomainResource; profile : TFHirStructureDefinition);

  end;

implementation

{ TNarrativeGenerator }

function TNarrativeGenerator.capitalize(s : String):String;
begin
  if( s = '') then
    result := ''
  else
    result := UpperCase(s.substring(0, 1)) + s.substring(1);
end;

function TNarrativeGenerator.camelCase(s : String):String;
begin
  result := s;
end;

function TNarrativeGenerator.pluralizeMe(s : String):String;
begin
  result := s+'s';
end;

Constructor TNarrativeGenerator.create(prefix : String; profiles : TProfileManager; onLookpuCode : TLookupCodeEvent; onLookpuReference : TLookupReferenceEvent; context : TFslObject);
begin
  inherited create;
  Fprefix := prefix;
  FProfiles := profiles;
  FOnLookupCode := onLookpuCode;
  FOnLookupReference := onLookpuReference;
  FContext := context;
end;

Procedure TNarrativeGenerator.generate(r : TFHIRDomainResource; profile : TFHirStructureDefinition);
var
  x : TFHIRXhtmlNode;
begin
  if (r.modifierExtensionList.Count > 0) then
    raise EFHIRException.create('Unable to generate narrative for resource of type '+CODES_TFHIRResourceType[r.ResourceType]+' because it has modifier extensions');

  x := TFHIRXhtmlNode.create;
  try
    x.Name := 'div';
    x.NodeType := fhntElement;

    x.addTag('p').addTag('b').addText('Generated Narrative'); // +(showCodeDetails ? ' with Details' : ''));
    try
      generateByProfile(r, r, profile.snapshot.elementList, profile.snapshot.ElementList[0], getChildrenForPath(profile.snapshot.ElementList, CODES_TFHIRResourceType[r.ResourceType]), x, CODES_TFHIRResourceType[r.ResourceType], true);
    except
      on e : Exception do
         x.addTag('p').addTag('b').setAttribute('style', 'color: maroon').addText('Exception generating Narrative: '+e.Message);
    end;
    inject(r, x, NarrativeStatusGenerated);
  finally
    x.free;
  end;
end;

procedure TNarrativeGenerator.generateByProfile(res : TFHIRDomainResource; e : TFHIRObject; allElements : TFhirElementDefinitionList; defn : TFhirElementDefinition; children : TFhirElementDefinitionList; x : TFHIRXhtmlNode; path : String; showCodeDetails : boolean);
var
  i : integer;
  iter : TFHIRPropertyIterator;
  child : TFhirElementDefinition;
  displayHints : TFslStringDictionary;
  grandChildren : TFhirElementDefinitionList;
  para, list, tbl, bq : TFHIRXhtmlNode;
  name : String;
  first : boolean;
begin
  displayHints := TFslStringDictionary.create;
  try
    if (children.isEmpty) then
    begin
      readDisplayHints(defn, displayHints);
      renderLeaf(res, e, defn, x, false, showCodeDetails, displayHints)
    end
    else
    begin
      iter := e.createIterator(true, true);
      try
        while (iter.More) do
        begin
          if (iter.Current.hasValue) then
          begin
            child := getElementDefinition(children, path+'.'+iter.Current.Name);
            readDisplayHints(child, displayHints);
            if (not exemptFromRendering(child)) then
            begin
              grandChildren := getChildrenForPath(allElements, path+'.'+iter.Current.Name);
              try
                if (iter.current.values <> nil) and not iter.Current.values.IsEmpty and (child <> nil) then
                begin
                  if (isPrimitive(child)) then
                  begin
                    para := x.addTag('p');
                    name := iter.current.Name;
                    if (name.endsWith('[x]')) then
                      name := name.substring(0, name.length - 3);
                    if (showCodeDetails or not isDefaultValue(displayHints, iter.Current.Values)) then
                    begin
                      para.addTag('b').addText(name);
                      para.addText(': ');
                      if (renderAsList(child) and (iter.Current.values.count > 1)) then
                      begin
                        list := x.addTag('ul');
                        for i := 0 to iter.Current.values.Count - 1 do
                          renderLeaf(res, iter.Current.values[i] as TFhirElement, child, list.addTag('li'), false, showCodeDetails, displayHints);
                      end
                      else
                      begin
                        first := true;
                        for i := 0 to iter.Current.values.Count - 1 do
                        begin
                          if (first) then
                            first := false
                          else
                            para.addText(', ');
                          renderLeaf(res, iter.Current.Values[i] as TFhirElement, child, para, false, showCodeDetails, displayHints);
                        end;
                      end;
                    end
                    else if (canDoTable(path, iter.Current, grandChildren)) then
                    begin
                      x.addTag('h3').addText(capitalize(camelCase(pluralizeMe(iter.current.Name))));
                      tbl := x.addTag('table').setAttribute('class', 'grid');
                      addColumnHeadings(tbl.addTag('tr'), grandChildren);
                      for i := 0 to iter.Current.Values.Count - 1 do
                        if (iter.Current.Values[i] <> nil) then
                          addColumnValues(res, tbl.addTag('tr'), grandChildren, iter.Current.Values[i] as TFhirElement, showCodeDetails, displayHints);
                    end;
                  end
                  else
                  begin
                    for i := 0 to iter.Current.Values.Count - 1 do
                      if (iter.Current.Values[i] <> nil) then
                      begin
                      bq := x.addTag('blockquote');
                      bq.addTag('p').addTag('b').addText(iter.Current.Name);
                      generateByProfile(res, iter.Current.Values[i] as TFhirElement, allElements, child, grandChildren, bq, path+'.'+iter.Current.Name, showCodeDetails);
                    end;
                  end;
                end;
              finally
                grandChildren.free;
              end;
            end;
          end;

          iter.Next;
        end;
      finally
        iter.Free;
      end;
    end;
  finally
    displayHints.free;
  end;
end;

function TNarrativeGenerator.isDefaultValue(displayHints : TFslStringDictionary; list: TFHIRObjectList) : boolean;
begin
  if (list.count <> 1) then
    result := false
  else if (list[0] is TFHIRType) and (list[0] is TFHIRPrimitiveType) then
    result := isDefault(displayHints, TFHIRPrimitiveType(list[0]))
  else
    result := false;
end;

function TNarrativeGenerator.isDefault(displayHints : TFslStringDictionary; primitiveType : TFHIRPrimitiveType) : boolean;
var
  v : string;
begin
  v := primitiveType.StringValue;
  result := (v <> '') and displayHints.containsKey('default') and (v = displayHints['default']);
end;

function TNarrativeGenerator.exemptFromRendering(child : TFhirElementDefinition) : boolean;
begin
  if (child = nil) then
    result := false
  else if ('Composition.subject' = child.Path) then
    result := true
  else if ('Composition.section' = child.Path) then
    result := true
  else
    result := false;
end;

function TNarrativeGenerator.renderAsList(child : TFhirElementDefinition) : boolean;
var
  t : String;
begin
  result := true;
  if (child.Type_List.count <> 1) then
  begin
    t := child.Type_List[0].Code;
    if (t = 'Address') or (t = 'Reference') then
      result := true;
  end;
end;

procedure TNarrativeGenerator.addColumnHeadings(tr : TFHIRXhtmlNode; grandChildren : TFhirElementDefinitionList);
var
  i : integer;
begin
  for i := 0 to grandChildren.Count - 1 do
    tr.addTag('td').addTag('b').addText(capitalize(tail(grandChildren[i].Path)));
end;

procedure TNarrativeGenerator.addColumnValues(res : TFHIRResource; tr : TFHIRXhtmlNode; grandChildren : TFhirElementDefinitionList; v : TFHIRElement; showCodeDetails : boolean; displayHints : TFslStringDictionary);
var
  i : integer;
  e : TFhirElementDefinition;
  list : TFHIRSelectionList;
begin
  for i := 0 to grandChildren.Count - 1 do
  begin
    e := grandChildren[i];
    list := TFHIRSelectionList.Create;
    try
      v.ListChildrenByName(tail(e.path), list);
      if (list.Count = 0) then
        tr.addTag('td').addText(' ')
      else
        renderLeaf(res, list[0].value as TFhirElement, e, tr.addTag('td'), false, showCodeDetails, displayHints);
    finally
      list.Free;
    end;
  end;
end;

function TNarrativeGenerator.tail(s : String):String;
begin
  result := s.substring(s.lastIndexOf('.')+1);
end;

function TNarrativeGenerator.canDoTable(path : String; p : TFHIRProperty; grandChildren  : TFhirElementDefinitionList) : boolean;
var
  i : integer;
  e : TFhirElementDefinition;
  list : TFHIRObjectList;
begin
  result := true;
  list := TFHIRObjectList.Create;
  try
    for i := 0 to grandChildren.Count - 1 do
    begin
      e := grandChildren[i];
      list.Clear;
      getValues(path, p, e, list);
      if (list.count > 1) or not isPrimitive(e) or not canCollapse(e) then
        result := false;
    end;
  finally
    list.Free;
  end;
end;

procedure TNarrativeGenerator.getValues(path : string; p : TFHIRProperty; e : TFhirElementDefinition; list : TFHIRObjectList);
var
  i : integer;
  v : TFhirElement;
  iter : TFHIRPropertyIterator;
begin
  for i := 0 to p.Values.Count - 1 do
  begin
    v := p.Values[i] as TFhirElement;
    iter := v.createIterator(true, true);
    try
      while iter.More do
      begin
        if ((path+'.'+p.Name+'.'+iter.Current.Name).equals(e.Path))  then
          list.add(p.link);
        iter.Next;
      end;
    finally
      iter.Free;
    end;
  end;
end;

function TNarrativeGenerator.canCollapse(e : TFhirElementDefinition) : boolean;
begin
  // we can collapse any data type
  result := not e.Type_List.isEmpty;
end;

function TNarrativeGenerator.isPrimitive(e : TFhirElementDefinition) : boolean;
begin
  //we can tell if e is a primitive because it has types
  result := not e.Type_List.isEmpty;
end;

function TNarrativeGenerator.lookupCode(system, version, code: String): String;
begin
  if Assigned(FOnLookupCode) then
    result := FOnLookupCode(system, version, code)
  else
    result := '';
end;

function TNarrativeGenerator.getElementDefinition(elements : TFhirElementDefinitionList; path : String) : TFhirElementDefinition;
var
  i : integer;
  element : TFhirElementDefinition;
begin
  result := nil;
  for i := 0 to elements.Count- 1 do
  begin
    element := elements[i];
    if (element.Path = path) then
      result := element;
  end;
end;

procedure TNarrativeGenerator.renderLeaf(res : TFHIRResource; e : TFHIRObject; defn : TFhirElementDefinition; x : TFHIRXhtmlNode; title, showCodeDetails : boolean; displayHints : TFslStringDictionary);
var
  p : TFHIRPeriod;
  r : TFhirReference;
  c : TFHIRXhtmlNode;
  tr : TResourceWithReference;
begin
  if (e = nil) then
    exit;

  if (e is TFHIRString) then
    x.addText(TFHIRString(e).Value)
  else if (e is TFHIRCode) then
    x.addText(TFHIRCode(e).Value)
  else if (e is TFHIRId) then
    x.addText(TFHIRId(e).Value)
  else if (e is TFhirExtension) then
    x.addText('Todo')
  else if (e is TFHIRInstant) then
    x.addText(TFHIRInstant(e).Value.toXML)
  else if (e is TFHIRDateTime) then
    x.addText(TFHIRDateTime(e).Value.toXML)
  else if (e is TFHIRDate) then
    x.addText(TFHIRDate(e).Value.toXML)
  else if (e is TFhirEnum) then
    x.addText(TFhirEnum(e).value) // todo: look up a display name if there is one
  else if (e is TFhirBoolean) then
    x.addText(BoolToStr(TFhirBoolean(e).Value))
  else if (e is TFHIRCodeableConcept)  then
    renderCodeableConcept(TFHIRCodeableConcept(e), x, showCodeDetails)
  else if (e is TFHIRCoding) then
    renderCoding(TFHIRCoding(e), x, showCodeDetails)
  else if (e is TFHIRIdentifier) then
    renderIdentifier(TFHIRIdentifier(e), x)
  else if (e is TFHIRInteger) then
    x.addText(TFHIRInteger(e).Value)
  else if (e is TFHIRDecimal) then
    x.addText(TFHIRDecimal(e).value)
  else if (e is TFHIRHumanName) then
    renderHumanName(TFHIRHumanName(e), x)
  else if (e is TFHIRAddress) then
    renderAddress(TFHIRAddress(e), x)
  else if (e is TFHIRContactPoint) then
    renderContact(TFHIRContactPoint(e), x)
  else if (e is TFHIRUri) then
    renderUri(TFHIRUri(e), x)
  else if (e is TFHIRTiming) then
    renderSchedule(TFHIRTiming(e), x)
  else if (e is TFHIRQuantity) then
    renderQuantity(TFHIRQuantity(e), x, showCodeDetails)
  else if (e is TFHIRRatio) then
  begin
    renderQuantity(TFHIRRatio(e).Numerator, x, showCodeDetails);
    x.addText('/');
    renderQuantity(TFHIRRatio(e).Denominator, x, showCodeDetails);
  end
  else if (e is TFHIRPeriod) then
  begin
    p := TFHIRPeriod(e);
    if p.Start.null then
      x.addText('??')
    else
      x.addText(p.Start.toXML);
    x.addText(' --> ');
    if p.end_.null then
      x.addText('??')
    else
    x.addText(p.End_.toXML);
  end
  else if (e is TFhirReference) then
  begin
    r := TFhirReference(e);
    c := x;
    tr := nil;
    try
      if (r.referenceElement <> nil) then
      begin
        tr := resolveReference(res, r.Reference);
        if (not r.Reference.startsWith('#')) then
        begin
          if (tr <> nil) and (tr.Reference <> '') then
            c := x.addTag('a').setattribute('href', tr.Reference)
          else
            c := x.addTag('a').setattribute('href', r.Reference);
        end;
      end;
      // what to display: if text is provided, then that. if the reference was resolved, then show the generated narrative
      if (r.Display <> '') then
      begin
        c.addText(r.Display);
        if (tr <> nil) then
        begin
          c.addText('. Generated Summary: ');
          generateResourceSummary(c, tr.Resource, true, r.Reference.startsWith('#'));
        end;
      end
      else if (tr <> nil) then
        generateResourceSummary(c, tr.Resource, r.Reference.startsWith('#'), r.Reference.startsWith('#'))
      else
        c.addText(r.Reference);
    finally
      tr.Free;
    end;
  end
  else if (not (e is TFHIRAttachment)) then
    raise EFHIRException.create('type '+e.ClassName+' not handled yet');
end;

function TNarrativeGenerator.displayLeaf(res : TFHIRResource; e : TFHIRElement; defn : TFhirElementDefinition; x : TFHIRXhtmlNode; name : String; showCodeDetails : boolean) : boolean;
var
  displayHints : TFslStringDictionary;
  p : TFHIRPeriod;
  r : TFhirReference;
begin
  result := false;
  if (e = nil) then
    exit;

  displayHints := TFslStringDictionary.create;
  try
    readDisplayHints(defn, displayHints);
    if (name.endsWith('[x]')) then
      name := name.substring(0, name.length - 3);

    if (not showCodeDetails) and (e is TFHIRType) and (e is TFHIRPrimitiveType) and isDefault(displayHints, TFHIRPrimitiveType(e)) then
     exit;

    if (e is TFHIRString) then
    begin
      x.addText(name+': '+TFHIRString(e).Value);
      result := true;
    end
    else if (e is TFHIRCode) then
    begin
      x.addText(name+': '+TFHIRCode(e).Value);
      result := true;
    end
    else if (e is TFHIRId) then
    begin
      x.addText(name+': '+TFHIRId(e).Value);
      result := true;
    end
    else if (e is TFHIRDateTime) then
    begin
      x.addText(name+': '+TFHIRDateTime(e).Value.toXML);
      result := true;
    end
    else if (e is TFHIRInstant) then
    begin
      x.addText(name+': '+TFHIRInstant(e).Value.toXML);
      result := true;
    end
    else if (e is TFHIRExtension) then
    begin
      x.addText('Extensions: todo');
      result := true;
    end
    else if (e is TFHIRDate) then
    begin
      x.addText(name+': '+TFHIRDate(e).Value.toXML);
      result := true;
    end
    else if (e is TFhirEnum) then
    begin
      x.addText(TFhirEnum(e).Value); // todo: look up a display name if there is one
      result := true;
    end
    else if (e is TFHIRBoolean) then
    begin
      if (TFHIRBoolean(e).Value) then
      begin
        x.addText(name);
        result := true;
      end;
    end
    else if (e is TFHIRCodeableConcept) then
    begin
      renderCodeableConcept(TFHIRCodeableConcept(e), x, showCodeDetails);
      result := true;
    end
    else if (e is TFHIRCoding) then
    begin
      renderCoding(TFHIRCoding(e), x, showCodeDetails);
      result := true;
    end
    else if (e is TFHIRInteger) then
    begin
      x.addText(TFHIRInteger(e).Value);
      result := true;
    end
    else if (e is TFHIRDecimal) then
    begin
      x.addText(TFHIRDecimal(e).Value);
      result := true;
    end
    else if (e is TFHIRIdentifier) then
    begin
      renderIdentifier(TFHIRIdentifier(e), x);
      result := true;
    end
    else if (e is TFHIRHumanName) then
    begin
      renderHumanName(TFHIRHumanName(e), x);
      result := true;
    end
    else if (e is TFHIRAddress) then
    begin
      renderAddress(TFHIRAddress(e), x);
      result := true;
    end
    else if (e is TFHIRContactPoint) then
    begin
      renderContact(TFHIRContactPoint(e), x);
      result := true;
    end
    else if (e is TFHIRTiming) then
    begin
      renderSchedule(TFHIRTiming(e), x);
      result := true;
    end
    else if (e is TFHIRQuantity) then
    begin
      renderQuantity(TFHIRQuantity(e), x, showCodeDetails);
      result := true;
    end
    else if (e is TFHIRRatio) then
    begin
      renderQuantity(TFHIRRatio(e).Numerator, x, showCodeDetails);
      x.addText('/');
      renderQuantity(TFHIRRatio(e).Denominator, x, showCodeDetails);
      result := true;
    end
    else if (e is TFHIRPeriod) then
    begin
      p := TFHIRPeriod(e);
      if p.Start.null then
        x.addText('??')
      else
        x.addText(p.Start.toXML);
      x.addText(' --> ');
      if p.end_.null then
        x.addText('??')
      else
      x.addText(p.End_.toXML);
    end
    else if (e is TFhirReference) then
    begin
      r := TFhirReference(e);
      if (r.Display <> '') then
        x.addText(r.Display)
      else if (r.Reference <> '') then
      begin
  //      tr := resolveReference(res, r.Reference);
  //      try
          x.addText(r.Reference);
  //      finally
  //        tr.Free;
  //      end;
      end
      else
        x.addText('??');
      result := true;
    end
    else if (not (e is TFHIRAttachment)) then
      raise EFHIRException.create('type '+e.ClassName+' not handled yet');
  finally
    displayHints.Free;
  end;
end;

procedure TNarrativeGenerator.readDisplayHints(defn : TFhirElementDefinition; hints : TFslStringDictionary);
var
  d, item : String;
  list, parts : TArray<String>;
begin
  hints.Clear;
  if (defn <> nil) then
  begin
    d := defn.getextensionString('http://hl7.org/fhir/Profile/tools-extensions#display-hint');
    if (d <> '') then
    begin
      list := d.split([';']);
      for item in list do
      begin
        parts := item.split([':']);
        hints.Add(parts[0].trim, parts[1].trim);
      end;
    end;
  end;
end;

procedure TNarrativeGenerator.generateResourceSummary(x : TFHIRXhtmlNode; res : TFHIRResourceV; textAlready, showCodeDetails : boolean);
var
  div_ : TFHIRXhtmlNode;
  path : String;
  profile : TFHirStructureDefinition;
  firstElement, last, first : boolean;
  struc : TFHirStructureDefinitionSnapshot;
  iter : TFHIRPropertyIterator;
  child : TFhirElementDefinition;
  v : TFhirElement;
  i : integer;
  dres :  TFhirDomainResource;
begin
  if not (res is TFHIRDomainResource) then
    raise EFHIRException.create('Not handled yet');
  dres := TFHIRDomainResource(res);

  if (not textAlready) then
  begin
    if (dres.Text <> nil) and (dres.Text.Div_ <> nil) then
    begin
      div_ := dres.Text.Div_;
      if (div_.allChildrenAreText) then
        x.ChildNodes.addAll(div_.ChildNodes)
      else if (div_.ChildNodes.count <> 1) and (div_.ChildNodes[0].allChildrenAreText) then
        x.ChildNodes.addAll(div_.ChildNodes[0].ChildNodes);
    end;
    x.addText('Generated Summary: ');
  end;
  path := CODES_TFhirResourceType[dres.ResourceType];
  profile := Fprofiles.ProfileByType[dres.ResourceType];
  if (profile <> nil) then
    x.addText('unknown resource ' +path)
  else
  begin
    struc := profile.snapshot;

    firstElement := true;
    last := false;

    iter := res.createIterator(true, true);
    try
      while iter.More do
      begin
        child := getElementDefinition(struc.ElementList, path+'.'+iter.current.Name);
        if (iter.current.Values <> nil) and (iter.current.Values.count > 0) and (iter.current.Values[0] <> nil) and (child <> nil) and (isPrimitive(child)) and (includeInSummary(child)) then
        begin
          if (firstElement) then
            firstElement := false
          else if (last) then
            x.addText('; ');
          first := true;
          last := false;
          for i := 0  to iter.Current.Values.Count - 1 do
          begin
            v := iter.Current.Values[i] as TFhirElement;
            if (first) then
              first := false
            else if (last) then
              x.addText(', ');
            last := displayLeaf(dres, v, child, x, iter.Current.Name, showCodeDetails) or last;
          end;
        end;
      end;
    finally
      iter.Free;
    end;
  end;
end;

function TNarrativeGenerator.includeInSummary(child : TFhirElementDefinition): boolean;
var
  t : string;
begin
  if (child.IsModifier) then
    result := true
  else if (child.MustSupport) then
    result := true
  else if (child.Type_List.count <> 1) then
  begin
    result := true;
    t := child.Type_List[0].Code;
    if (t = 'Address')or (t = 'Contact') or (t = 'Reference') or (t = 'Uri') then
      result := false;
  end
  else
    result := true;
end;

procedure TNarrativeGenerator.renderCodeableConcept(v : TFHIRCodeableConcept; x : TFHIRXhtmlNode; showCodeDetails : boolean);
var
  s, s1 : String;
  i : integer;
  sp : TFHIRXhtmlNode;
  first : boolean;
begin
  s := v.Text;
  if (s = '') then
    for i := 0 to v.codingList.Count - 1 do
      if (s = '') then
        s := v.codingList[i].Display;

  if (s = '') then
  begin
    // still? ok, let's try looking it up
    for i := 0 to v.codingList.Count - 1 do
      if (s = '') and (v.codingList[i].Code <> '') and (v.codingList[i].System <> '') then
         s := lookupCode(v.codingList[i].System, v.codingList[i].Version, v.codingList[i].Code);
  end;

  if (s = '') then
  begin
    if (v.CodingList.isEmpty) then
      s := ''
    else
      s := v.CodingList[0].Code;
  end;

  if (showCodeDetails) then
  begin
    x.addText(s+' ');
    sp := x.addTag('span');
    sp.setAttribute('style', 'background: LightGoldenRodYellow ');
    sp.addText('(Details ');
    first := true;
    for i := 0 to v.codingList.Count - 1 do
    begin
      if (first) then
      begin
        sp.addText(': ');
        first := false;
      end
      else
        sp.addText('; ');
      sp.addText('{'+describeSystem(v.codingList[i].System)+' code "'+v.codingList[i].Code+'" := "'+lookupCode(v.codingList[i].System, v.codingList[i].Version, v.codingList[i].Code)+'", given as "'+v.codingList[i].Display+'"}');
    end;
    sp.addText(')');
  end
  else
  begin
    s1 := '';
    for i := 0 to v.codingList.Count - 1 do
    begin
      if (v.codingList[i].Code <> '') and (v.codingList[i].System <> '') then
      begin
        if s1 <> '' then
          s1 := s1 + ', ';
        s1 := s1 + ('{'+v.codingList[i].System+' '+v.codingList[i].Code+'}');
      end;
    end;

    x.addTag('span').setAttribute('title', 'Codes: '+s1).addText(s);
  end;
end;

procedure TNarrativeGenerator.renderCoding(v : TFHIRCoding; x : TFHIRXhtmlNode; showCodeDetails : boolean);
var
  s  : String;
begin
  s := '';
  if (v.Display <> '') then
    s := v.Display;
  if (s = '') then
    s := lookupCode(v.System, v.version, v.Code);

  if (s = '') then
    s := v.Code;

  if (showCodeDetails) then
    x.addText(s+' (Details: '+describeSystem(v.System)+' code '+v.Code+' := "'+lookupCode(v.System, v.version, v.Code)+'", stated as "'+v.Display+'")')
  else
    x.addTag('span').setAttribute('title', '{'+v.System+' '+v.Code+'}').addText(s);
end;

function TNarrativeGenerator.describeSystem(system : String) : String;
begin
  if (system = '') then
    result := '[not stated]';
  if (system.equals('http://loinc.org')) then
    result := 'LOINC';
  if (system.startsWith('http://snomed.info')) then
    result := 'SNOMED CT'
  else
    result := system;
end;

procedure TNarrativeGenerator.renderIdentifier(v : TFHIRIdentifier; x : TFHIRXhtmlNode);
begin
  x.addText(displayIdentifier(v));
end;

procedure TNarrativeGenerator.renderSchedule(v : TFHIRTiming; x : TFHIRXhtmlNode);
begin
  x.addText(displaySchedule(v));
end;

procedure TNarrativeGenerator.renderQuantity(v : TFHIRQuantity; x : TFHIRXhtmlNode; showCodeDetails : boolean);
var
  sp : TFHIRXhtmlNode;
begin
  if (v.comparatorElement <> nil) then
    x.addText(v.comparatorElement.value);
  x.addText(v.Value);
  if (v.Unit_ <> '') then
    x.addText(' '+v.Unit_)
  else if (v.Code <> '') then
    x.addText(' '+v.Code);
  if (showCodeDetails) and (v.Code <> '') then
  begin
    sp := x.addTag('span');
    sp.setAttribute('style', 'background: LightGoldenRodYellow ');
    sp.addText(' (Details: '+describeSystem(v.System)+' code '+v.Code+' := "'+lookupCode(v.System, '', v.Code)+'")');
  end;
end;


procedure TNarrativeGenerator.renderHumanName(v : TFHIRHumanName; x : TFHIRXhtmlNode);
begin
  x.addText(displayHumanName(v));
end;

procedure TNarrativeGenerator.renderAddress(v : TFHIRAddress; x : TFHIRXhtmlNode);
begin
  x.addText(displayAddress(v));
end;

procedure TNarrativeGenerator.renderContact(v : TFHIRContactPoint; x : TFHIRXhtmlNode);
begin
  x.addText(displayContact(v));
end;

procedure TNarrativeGenerator.renderUri(v : TFHIRUri; x : TFHIRXhtmlNode);
begin
  x.addTag('a').setAttribute('href', v.Value).addText(v.Value);
end;

function TNarrativeGenerator.resolveReference(res: TFHIRResource; url: String): TResourceWithReference;
var
  r : TFhirResource;
begin
  result := nil;
  if (url.startsWith('#') and (res is TFhirDomainResource)) then
  begin
    r := TFhirDomainResource(res).Contained[url.substring(1)];
    if r <> nil then
      result := TResourceWithReference.Create('', r);
  end
  else if url <> '' then
    result := FOnLookupReference(FContext, url);
end;

function TNarrativeGenerator.displaySchedule(v : TFHIRTiming) : String;
begin
  result := '';
 { if (s.Event.count > 1) or ((s.Repeat <> nil) and (not s.Event.isEmpty)) then
  begin
    for (Period p : s.Event) begin
      c.append(displayPeriod(p));
    end;
    return c.toString; statedType, id, ver : String;
  end
  else if (s.Repeat <> nil) then
  begin
    ScheduleRepeatComponent rep := s.Repeat; statedType, id, ver : String;
    StringBuilder b := new StringBuilder; statedType, id, ver : String;
    if (s.Event.count <> 1)
      b.append('Starting '+displayPeriod(s.Event[0))+', ');
    if (rep.When <> nil) then
    begin
      b.append(rep.Duration.toString+' '+displayTimeUnits(rep.Units));
      b.append(' ');
      b.append(displayEventCode(rep.When));
    end
    else
    begin
      if (rep.Frequency <> 1) then
        b.append('Once per ')
      else
        b.append(Integer.toString(rep.Frequency)+' per ');
      b.append(rep.Duration.toString+' '+displayTimeUnits(rep.Units));
      if (rep.Count <> nil) then
        b.append(' '+Integer.toString(rep.Count)+' times')
      else if (rep.End <> nil) then
        b.append(' until '+rep.End.toHumanDisplay);
    end;
    return b.toString; statedType, id, ver : String;
  end
  else
    result := '??';       }
end;

function TNarrativeGenerator.displayHumanName(v : TFHIRHumanName) : String;
var
  s : TStringBuilder;
  i : integer;
begin
  s := TStringBuilder.Create;
  try
    if (v.Text <> '') then
      s.append(v.Text)
    else
    begin
      for i := 0 to v.givenList.Count - 1 do
      begin
        s.append(v.givenList[i].Value);
        s.append(' ');
      end;
      if v.family <> '' then
      begin
        s.append(v.family);
        s.append(' ');
      end;
    end;
    if (v.useElement <> nil) and (v.Use <> NameUseUsual) then
      s.append('('+v.UseElement.value+')');
    result := s.toString;
  finally
    s.free;
  end;
end;


function TNarrativeGenerator.displayAddress(v : TFHIRAddress) : String;
var
  s : TStringBuilder;
  i : integer;
begin
  s := TStringBuilder.Create;
  try
    if (v.Text <> '') then
      s.append(v.Text)
    else
    begin
      for i := 0 to v.lineList.Count - 1 do
      begin
        s.append(v.lineList[i].Value);
        s.append(' ');
      end;
      if (v.City <> '') then
      begin
        s.append(v.City);
        s.append(' ');
      end;
      if (v.State <> '') then
      begin
        s.append(v.State);
        s.append(' ');
      end;

      if (v.postalCode <> '') then
      begin
        s.append(v.postalCode);
        s.append(' ');
      end;

      if (v.Country <> '') then
      begin
        s.append(v.Country);
        s.append(' ');
      end;
    end;
    if (v.useElement <> nil) then
      s.append('('+v.useElement.value+')');
    result := s.toString;
  finally
    s.free;
  end;
end;


function TNarrativeGenerator.displayCodeable(v: TFHIRCodeableConcept): String;
begin
  result := '';
  if (v <> nil) then
  begin
    if (v.text <> '') then
      result := v.text
    else if (v.hasCodingList) then
      result := v.codingList[0].display;
  end;
end;

function TNarrativeGenerator.displayContact(v : TFHIRContactPoint) : String;
begin
  result := describeSystem(v.System);
  if (v.Value = '') then
    result := result + '-unknown-'
  else
    result := result + v.Value;
  if (v.useElement <> nil) then
    result := result + '('+v.useElement.value+')';
end;

function TNarrativeGenerator.describeSystem(system : TFHIRContactPointSystemEnum) : String;
begin
  case system of
    ContactPointSystemNull: result := '';
    ContactPointSystemPhone: result := 'ph: ';
    ContactPointSystemFax: result := 'fax: ';
    ContactPointSystemEmail: result := '';
    ContactPointSystemOther: result := '';
  end;
end;

destructor TNarrativeGenerator.Destroy;
begin
  FProfiles.Free;
  FContext.Free;
  inherited;
end;

function TNarrativeGenerator.displayIdentifier(v : TFHIRIdentifier) : String;
var
  s : String;
begin
  if v.value = '' then
    s := '??'
  else
    s := v.Value;

  if (v.type_ <> nil) then
    s := displayCodeable(v.type_)+' := '+s;

  if (v.useElement <> nil) then
    s := s + ' ('+v.useElement.value+')';
  result := s;
end;


function TNarrativeGenerator.getChildrenForPath(elements : TFhirElementDefinitionList; path : String) : TFhirElementDefinitionList;
var
  i, j : integer;
  e, t : TFhirElementDefinition;
  name : String;
begin
  // do we need to do a name reference substitution?
  for i := 0  to elements.Count - 1 do
  begin
    e := elements[i];
    if (e.Path = path) and (e.ContentReference <> '') then
    begin
      name := e.ContentReference;
      t := nil;
      // now, resolve the name
      for j := 0 to elements.Count - 1 do
        if name = '#'+elements[j].id then
          t := elements[j];
      if (t <> nil) then
        raise EFHIRException.create('Unable to resolve name reference '+name+' trying to resolve '+path);
      path := t.Path;
      break;
    end;
  end;

  result := TFhirElementDefinitionList.Create;
  try
    for i := 0 to elements.Count - 1 do
    begin
      e := elements[i];
      if e.Path.startsWith(path+'.') and (not e.Path.substring(path.length+1).contains('.')) and
         not (e.Path.endsWith('.extension') or e.Path.endsWith('.modifierExtension')) then
        result.add(e.Link);
    end;
    result.link;
  finally
    result.free;
  end;
end;


procedure TNarrativeGenerator.inject(res : TFHIRDomainResource; x : TFHIRXhtmlNode; status : TFhirNarrativeStatusEnum);
begin
  if not x.hasAttribute('xmlns') then
    x.attribute('xmlns', XHTML_NS);
  if (res.Text = nil) then
    res.Text := TFHIRNarrative.create;
  if (res.Text.div_ = nil) or (res.Text.Div_.ChildNodes.isEmpty) then
  begin
    res.Text.Div_ := x.Link;
    res.Text.Status := status;
  end
  else
  begin
    res.Text.Div_.addTag('hr');
    res.Text.Div_.ChildNodes.addAll(x.ChildNodes);
  end;
end;



function TNarrativeGenerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (Fprefix.length * sizeof(char)) + 12);
  inc(result, FProfiles.sizeInBytes);
  inc(result, FContext.sizeInBytes);
end;

end.

