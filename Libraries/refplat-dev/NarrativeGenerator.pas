unit NarrativeGenerator;

{Copyright (c) 2011+, HL7, Inc
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

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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

interface

uses
  SysUtils, Generics.Collections,
  AdvObjects, StringSupport,
  FHIRResources, FHIRComponents, FHIRTypes, FHIRBase, FHIRCOnstants, FHIRUtilities, FHIRSupport,
  ProfileManager, QuestionnaireBuilder;

type

  TNarrativeGenerator = class (TAdvObject)
  private
    Fprefix : String;
    FProfiles : TProfileManager;
    FOnLookupCode : TLookupCodeEvent;
    FOnLookupReference : TLookupReferenceEvent;
    FContext : TFHIRRequest;

    function getByName(profile : TFHIRProfile; name : String) : TFhirProfileStructureSnapshot;
    procedure generateByProfile(res : TFHIRResource; e : TFHIRElement; allElements : TFhirProfileStructureSnapshotElementList; defn : TFhirProfileStructureSnapshotElement; children : TFhirProfileStructureSnapshotElementList; x : TFHIRXhtmlNode; path : String; showCodeDetails : boolean);
    function getChildrenForPath(elements : TFhirProfileStructureSnapshotElementList; path : String) : TFhirProfileStructureSnapshotElementList;
    procedure inject(res : TFHIRResource; x : TFHIRXhtmlNode; status : TFhirNarrativeStatus);
    procedure renderLeaf(res : TFHIRResource; e : TFHIRElement; defn : TFhirProfileStructureSnapshotElement; x : TFHIRXhtmlNode; title, showCodeDetails : boolean; displayHints : TDictionary<String, String>);
    procedure readDisplayHints(defn : TFhirProfileStructureSnapshotElement; hints : TDictionary<String, String>);
    function getElementDefinition(elements : TFhirProfileStructureSnapshotElementList; path : String) : TFhirProfileStructureSnapshotElement;
    function exemptFromRendering(child : TFhirProfileStructureSnapshotElement) : boolean;
    function isDefaultValue(displayHints : TDictionary<String, String>; list: TFHIRObjectList) : boolean;
    function renderAsList(child : TFhirProfileStructureSnapshotElement) : boolean;
    function canDoTable(path : String; p : TFHIRProperty; grandChildren  : TFhirProfileStructureSnapshotElementList) : boolean;
    procedure addColumnHeadings(tr : TFHIRXhtmlNode; grandChildren : TFhirProfileStructureSnapshotElementList);
    procedure addColumnValues(res : TFHIRResource; tr : TFHIRXhtmlNode; grandChildren : TFhirProfileStructureSnapshotElementList; v : TFHIRElement; showCodeDetails : boolean; displayHints : TDictionary<String, String>);
    function isDefault(displayHints : TDictionary<String, String>; primitiveType : TFHIRPrimitiveType) : boolean;
    procedure getValues(path : string; p : TFHIRProperty; e : TFhirProfileStructureSnapshotElement; list : TFHIRObjectList);
    function canCollapse(e : TFhirProfileStructureSnapshotElement) : boolean;
    procedure generateResourceSummary(x : TFHIRXhtmlNode; res : TFHIRResource; textAlready, showCodeDetails : boolean);
    function includeInSummary(child : TFhirProfileStructureSnapshotElement): boolean;
    function displayLeaf(res : TFHIRResource; e : TFHIRElement; defn : TFhirProfileStructureSnapshotElement; x : TFHIRXhtmlNode; name : String; showCodeDetails : boolean) : boolean;

    function isPrimitive(e : TFhirProfileStructureSnapshotElement) : boolean; overload;
    function capitalize(s : String):String;
    function camelCase(s : String):String;
    function tail(s : String):String;
    function pluralizeMe(s : String):String;

    procedure renderCodeableConcept(v : TFHIRCodeableConcept; x : TFHIRXhtmlNode; showCodeDetails : boolean);
    procedure renderCoding(v : TFHIRCoding; x : TFHIRXhtmlNode; showCodeDetails : boolean);
    procedure renderIdentifier(v : TFHIRIdentifier; x : TFHIRXhtmlNode);
    procedure renderAddress(v : TFHIRAddress; x : TFHIRXhtmlNode);
    procedure renderHumanName(v : TFHIRHumanName; x : TFHIRXhtmlNode);
    procedure renderContact(v : TFHIRContact; x : TFHIRXhtmlNode);
    procedure renderUri(v : TFHIRUri; x : TFHIRXhtmlNode);
    procedure renderSchedule(v : TFHIRSchedule; x : TFHIRXhtmlNode);
    procedure renderQuantity(v : TFHIRQuantity; x : TFHIRXhtmlNode; showCodeDetails : boolean);
    function displayIdentifier(v : TFHIRIdentifier) : String;
    function displayAddress(v : TFHIRAddress) : String;
    function displayHumanName(v : TFHIRHumanName) : String;
    function displayContact(v : TFHIRContact) : String;
    function displaySchedule(v : TFHIRSchedule) : String;

    function resolveReference(res : TFHIRResource; url : String) : TResourceWithReference;
    function lookupCode(system, code : String) : String;
    function describeSystem(system : String) : String; overload;
    function describeSystem(system : TFHIRContactSystem) : String;  overload;
  public
    Constructor Create(prefix : String; profiles : TProfileManager; onLookpuCode : TLookupCodeEvent; onLookpuReference : TLookupReferenceEvent; context : TFHIRRequest);
    Destructor Destroy; Override;

    Procedure generate(r : TFHIRResource; profile : TFHIRProfile);

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

Constructor TNarrativeGenerator.create(prefix : String; profiles : TProfileManager; onLookpuCode : TLookupCodeEvent; onLookpuReference : TLookupReferenceEvent; context : TFHIRRequest);
begin
  inherited create;
  Fprefix := prefix;
  FProfiles := profiles;
  FOnLookupCode := onLookpuCode;
  FOnLookupReference := onLookpuReference;
  FContext := context;
end;

Procedure TNarrativeGenerator.generate(r : TFHIRResource; profile : TFHIRProfile);
var
  ps : TFhirProfileStructureSnapshot;
  x : TFHIRXhtmlNode;
begin
  if (r.hasModifierExtensions) then
    raise Exception.create('Unable to generate narrative for resource of type '+CODES_TFHIRResourceType[r.ResourceType]+' because it has modifier extensions');
  ps := getByName(profile, CODES_TFHIRResourceType[r.ResourceType]);

  x := TFHIRXhtmlNode.create;
  try
    x.Name := 'div';
    x.NodeType := fhntElement;

    x.addTag('p').addTag('b').addText('Generated Narrative'); // +(showCodeDetails ? ' with Details' : ''));
    try
      generateByProfile(r, r, ps.elementList, ps.ElementList[0], getChildrenForPath(ps.ElementList, CODES_TFHIRResourceType[r.ResourceType]), x, CODES_TFHIRResourceType[r.ResourceType], true);
    except
      on e : Exception do
         x.addTag('p').addTag('b').setAttribute('style', 'color: maroon').addText('Exception generating Narrative: '+e.Message);
    end;
    inject(r, x, NarrativeStatusGenerated);
  finally
    x.free;
  end;
end;

procedure TNarrativeGenerator.generateByProfile(res : TFHIRResource; e : TFHIRElement; allElements : TFhirProfileStructureSnapshotElementList; defn : TFhirProfileStructureSnapshotElement; children : TFhirProfileStructureSnapshotElementList; x : TFHIRXhtmlNode; path : String; showCodeDetails : boolean);
var
  i : integer;
  iter : TFHIRPropertyIterator;
  child : TFhirProfileStructureSnapshotElement;
  displayHints : TDictionary<String, String>;
  grandChildren : TFhirProfileStructureSnapshotElementList;
  para, list, tbl, bq : TFHIRXhtmlNode;
  name : String;
  first : boolean;
begin
  displayHints := TDictionary<String, String>.create;
  try
    if (children.isEmpty) then
    begin
      readDisplayHints(defn, displayHints);
      renderLeaf(res, e, defn, x, false, showCodeDetails, displayHints)
    end
    else
    begin
      iter := e.createIterator(true);
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
                if (iter.current.List <> nil) and not iter.Current.List.IsEmpty and (child <> nil) then
                begin
                  if (isPrimitive(child)) then
                  begin
                    para := x.addTag('p');
                    name := iter.current.Name;
                    if (name.endsWith('[x]')) then
                      name := name.substring(0, name.length - 3);
                    if (showCodeDetails or not isDefaultValue(displayHints, iter.Current.List)) then
                    begin
                      para.addTag('b').addText(name);
                      para.addText(': ');
                      if (renderAsList(child) and (iter.Current.List.count > 1)) then
                      begin
                        list := x.addTag('ul');
                        for i := 0 to iter.Current.List.Count - 1 do
                          renderLeaf(res, iter.Current.List[i] as TFhirElement, child, list.addTag('li'), false, showCodeDetails, displayHints);
                      end
                      else
                      begin
                        first := true;
                        for i := 0 to iter.Current.List.Count - 1 do
                        begin
                          if (first) then
                            first := false
                          else
                            para.addText(', ');
                          renderLeaf(res, iter.Current.List[i] as TFhirElement, child, para, false, showCodeDetails, displayHints);
                        end;
                      end;
                    end
                    else if (canDoTable(path, iter.Current, grandChildren)) then
                    begin
                      x.addTag('h3').addText(capitalize(camelCase(pluralizeMe(iter.current.Name))));
                      tbl := x.addTag('table').setAttribute('class', 'grid');
                      addColumnHeadings(tbl.addTag('tr'), grandChildren);
                      for i := 0 to iter.Current.List.Count - 1 do
                        if (iter.Current.List[i] <> nil) then
                          addColumnValues(res, tbl.addTag('tr'), grandChildren, iter.Current.List[i] as TFhirElement, showCodeDetails, displayHints);
                    end;
                  end
                  else
                  begin
                    for i := 0 to iter.Current.List.Count - 1 do
                      if (iter.Current.List[i] <> nil) then
                      begin
                      bq := x.addTag('blockquote');
                      bq.addTag('p').addTag('b').addText(iter.Current.Name);
                      generateByProfile(res, iter.Current.List[i] as TFhirElement, allElements, child, grandChildren, bq, path+'.'+iter.Current.Name, showCodeDetails);
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

function TNarrativeGenerator.isDefaultValue(displayHints : TDictionary<String, String>; list: TFHIRObjectList) : boolean;
begin
  if (list.count <> 1) then
    result := false
  else if (list[0] is TFHIRType) and (list[0] is TFHIRPrimitiveType) then
    result := isDefault(displayHints, TFHIRPrimitiveType(list[0]))
  else
    result := false;
end;

function TNarrativeGenerator.isDefault(displayHints : TDictionary<String, String>; primitiveType : TFHIRPrimitiveType) : boolean;
var
  v : string;
begin
  v := primitiveType.AsStringValue;
  result := (v <> '') and displayHints.containsKey('default') and (v = displayHints['default']);
end;

function TNarrativeGenerator.exemptFromRendering(child : TFhirProfileStructureSnapshotElement) : boolean;
begin
  if (child = nil) then
    result := false
  else if ('Composition.subject' = child.PathST) then
    result := true
  else if ('Composition.section' = child.PathST) then
    result := true
  else
    result := false;
end;

function TNarrativeGenerator.renderAsList(child : TFhirProfileStructureSnapshotElement) : boolean;
var
  t : String;
begin
  result := true;
  if (child.Definition.Type_List.count <> 1) then
  begin
    t := child.Definition.Type_List[0].CodeST;
    if (t = 'Address') or (t = 'ResourceReference') then
      result := true;
  end;
end;

procedure TNarrativeGenerator.addColumnHeadings(tr : TFHIRXhtmlNode; grandChildren : TFhirProfileStructureSnapshotElementList);
var
  i : integer;
begin
  for i := 0 to grandChildren.Count - 1 do
    tr.addTag('td').addTag('b').addText(capitalize(tail(grandChildren[i].PathST)));
end;

procedure TNarrativeGenerator.addColumnValues(res : TFHIRResource; tr : TFHIRXhtmlNode; grandChildren : TFhirProfileStructureSnapshotElementList; v : TFHIRElement; showCodeDetails : boolean; displayHints : TDictionary<String, String>);
var
  i : integer;
  e : TFhirProfileStructureSnapshotElement;
  list : TFHIRObjectList;
begin
  for i := 0 to grandChildren.Count - 1 do
  begin
    e := grandChildren[i];
    list := TFHIRObjectList.Create;
    try
      v.ListChildrenByName(tail(e.pathST), list);
      if (list.Count = 0) then
        tr.addTag('td').addText(' ')
      else
        renderLeaf(res, list[0] as TFhirElement, e, tr.addTag('td'), false, showCodeDetails, displayHints);
    finally
      list.Free;
    end;
  end;
end;

function TNarrativeGenerator.tail(s : String):String;
begin
  result := s.substring(s.lastIndexOf('.')+1);
end;

function TNarrativeGenerator.canDoTable(path : String; p : TFHIRProperty; grandChildren  : TFhirProfileStructureSnapshotElementList) : boolean;
var
  i : integer;
  e : TFhirProfileStructureSnapshotElement;
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

procedure TNarrativeGenerator.getValues(path : string; p : TFHIRProperty; e : TFhirProfileStructureSnapshotElement; list : TFHIRObjectList);
var
  i, j : integer;
  v : TFhirElement;
  iter : TFHIRPropertyIterator;
begin
  for i := 0 to p.List.Count - 1 do
  begin
    v := p.List[i] as TFhirElement;
    iter := v.createIterator(true);
    try
      while iter.More do
      begin
        if ((path+'.'+p.Name+'.'+iter.Current.Name).equals(e.PathST))  then
          list.add(p.link);
        iter.Next;
      end;
    finally
      iter.Free;
    end;
  end;
end;

function TNarrativeGenerator.canCollapse(e : TFhirProfileStructureSnapshotElement) : boolean;
begin
  // we can collapse any data type
  result := not e.Definition.Type_List.isEmpty;
end;

function TNarrativeGenerator.isPrimitive(e : TFhirProfileStructureSnapshotElement) : boolean;
begin
  //we can tell if e is a primitive because it has types
  result := not e.Definition.Type_List.isEmpty;
end;

function TNarrativeGenerator.lookupCode(system, code: String): String;
begin
  if Assigned(FOnLookupCode) then
    result := FOnLookupCode(system, code)
  else
    result := '';
end;

function TNarrativeGenerator.getElementDefinition(elements : TFhirProfileStructureSnapshotElementList; path : String) : TFhirProfileStructureSnapshotElement;
var
  i : integer;
  element : TFhirProfileStructureSnapshotElement;
begin
  result := nil;
  for i := 0 to elements.Count- 1 do
  begin
    element := elements[i];
    if (element.PathST = path) then
      result := element;
  end;
end;

procedure TNarrativeGenerator.renderLeaf(res : TFHIRResource; e : TFHIRElement; defn : TFhirProfileStructureSnapshotElement; x : TFHIRXhtmlNode; title, showCodeDetails : boolean; displayHints : TDictionary<String, String>);
var
  p : TFHIRPeriod;
  r : TFhirResourceReference;
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
    x.addText(TFHIRInstant(e).Value.AsXML)
  else if (e is TFHIRDateTime) then
    x.addText(TFHIRDateTime(e).Value.AsXML)
  else if (e is TFHIRDate) then
    x.addText(TFHIRDate(e).Value.AsXML)
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
  else if (e is TFHIRContact) then
    renderContact(TFHIRContact(e), x)
  else if (e is TFHIRUri) then
    renderUri(TFHIRUri(e), x)
  else if (e is TFHIRSchedule) then
    renderSchedule(TFHIRSchedule(e), x)
  else if (e is TFHIRQuantity) or (e is TFHIRDuration) then
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
    if p.Start = nil then
      x.addText('??')
    else
      x.addText(p.StartST.AsXML);
    x.addText(' --> ');
    if p.end_ST = nil then
      x.addText('??')
    else
    x.addText(p.End_ST.AsXML);
  end
  else if (e is TFHIRResourceReference) then
  begin
    r := TFHIRResourceReference(e);
    c := x;
    tr := nil;
    try
      if (r.Reference <> nil) then
      begin
        tr := resolveReference(res, r.ReferenceST);
        if (not r.ReferenceST.startsWith('#')) then
        begin
          if (tr <> nil) and (tr.Reference <> '') then
            c := x.addTag('a').setattribute('href', tr.Reference)
          else
            c := x.addTag('a').setattribute('href', r.ReferenceST);
        end;
      end;
      // what to display: if text is provided, then that. if the reference was resolved, then show the generated narrative
      if (r.Display <> nil) then
      begin
        c.addText(r.DisplayST);
        if (tr <> nil) then
        begin
          c.addText('. Generated Summary: ');
          generateResourceSummary(c, tr.Resource, true, r.ReferenceST.startsWith('#'));
        end;
      end
      else if (tr <> nil) then
        generateResourceSummary(c, tr.Resource, r.ReferenceST.startsWith('#'), r.ReferenceST.startsWith('#'))
      else
        c.addText(r.ReferenceST);
    finally
      tr.Free;
    end;
  end
  else if (not (e is TFHIRAttachment)) then
    raise Exception.create('type '+e.ClassName+' not handled yet');
end;

function TNarrativeGenerator.displayLeaf(res : TFHIRResource; e : TFHIRElement; defn : TFhirProfileStructureSnapshotElement; x : TFHIRXhtmlNode; name : String; showCodeDetails : boolean) : boolean;
var
  displayHints : TDictionary<String, String>;
  p : TFHIRPeriod;
  r : TFhirResourceReference;
  tr : TResourceWithReference;
begin
  result := false;
  if (e = nil) then
    exit;

  displayHints := TDictionary<String,String>.create;
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
      x.addText(name+': '+TFHIRDateTime(e).Value.AsXML);
      result := true;
    end
    else if (e is TFHIRInstant) then
    begin
      x.addText(name+': '+TFHIRInstant(e).Value.AsXML);
      result := true;
    end
    else if (e is TFHIRExtension) then
    begin
      x.addText('Extensions: todo');
      result := true;
    end
    else if (e is TFHIRDate) then
    begin
      x.addText(name+': '+TFHIRDate(e).Value.AsXML);
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
    else if (e is TFHIRContact) then
    begin
      renderContact(TFHIRContact(e), x);
      result := true;
    end
    else if (e is TFHIRSchedule) then
    begin
      renderSchedule(TFHIRSchedule(e), x);
      result := true;
    end
    else if (e is TFHIRQuantity) or (e is TFHIRDuration) then
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
      if p.Start = nil then
        x.addText('??')
      else
        x.addText(p.StartST.AsXML);
      x.addText(' --> ');
      if p.end_ST = nil then
        x.addText('??')
      else
      x.addText(p.End_ST.AsXML);
    end
    else if (e is TFHIRResourceReference) then
    begin
      r := TFHIRResourceReference(e);
      if (r.Display <> nil) then
        x.addText(r.DisplayST)
      else if (r.Reference <> nil) then
      begin
  //      tr := resolveReference(res, r.ReferenceST);
  //      try
          x.addText(r.ReferenceST);
  //      finally
  //        tr.Free;
  //      end;
      end
      else
        x.addText('??');
      result := true;
    end
    else if (not (e is TFHIRAttachment)) then
      raise Exception.create('type '+e.ClassName+' not handled yet');
  finally
    displayHints.Free;
  end;
end;

procedure TNarrativeGenerator.readDisplayHints(defn : TFhirProfileStructureSnapshotElement; hints : TDictionary<String, String>);
var
  d, item : String;
  list, parts : TArray<String>;
begin
  hints.Clear;
  if (defn <> nil) then
  begin
    d := defn.Definition.getextensionString('http://hl7.org/fhir/Profile/tools-extensions#display-hint');
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

procedure TNarrativeGenerator.generateResourceSummary(x : TFHIRXhtmlNode; res : TFHIRResource; textAlready, showCodeDetails : boolean);
var
  div_ : TFHIRXhtmlNode;
  path : String;
  profile : TFHIRProfile;
  firstElement, last, first : boolean;
  struc : TFhirProfileStructureSnapshot;
  iter : TFHIRPropertyIterator;
  child : TFhirProfileStructureSnapshotElement;
  v : TFhirElement;
  i : integer;
begin
  if (not textAlready) then
  begin
    if (res.Text <> nil) and (res.Text.Div_ <> nil) then
    begin
      div_ := res.Text.Div_;
      if (div_.allChildrenAreText) then
        x.ChildNodes.addAll(div_.ChildNodes)
      else if (div_.ChildNodes.count <> 1) and (div_.ChildNodes[0].allChildrenAreText) then
        x.ChildNodes.addAll(div_.ChildNodes[0].ChildNodes);
    end;
    x.addText('Generated Summary: ');
  end;
  path := CODES_TFhirResourceType[res.ResourceType];
  profile := Fprofiles.ProfileByType[res.ResourceType];
  if (profile <> nil) then
    x.addText('unknown resource ' +path)
  else
  begin
    struc := profile.StructureList[0].snapshot; // todo: how to do this better?

    firstElement := true;
    last := false;

    iter := res.createIterator(true);
    try
      while iter.More do
      begin
        child := getElementDefinition(struc.ElementList, path+'.'+iter.current.Name);
        if (iter.current.List <> nil) and (iter.current.List.count > 0) and (iter.current.List[0] <> nil) and (child <> nil) and (isPrimitive(child)) and (includeInSummary(child)) then
        begin
          if (firstElement) then
            firstElement := false
          else if (last) then
            x.addText('; ');
          first := true;
          last := false;
          for i := 0  to iter.Current.List.Count - 1 do
          begin
            v := iter.Current.List[i] as TFhirElement;
            if (first) then
              first := false
            else if (last) then
              x.addText(', ');
            last := displayLeaf(res, v, child, x, iter.Current.Name, showCodeDetails) or last;
          end;
        end;
      end;
    finally
      iter.Free;
    end;
  end;
end;

function TNarrativeGenerator.includeInSummary(child : TFhirProfileStructureSnapshotElement): boolean;
var
  t : string;
begin
  if (child.Definition.IsModifierST) then
    result := true
  else if (child.Definition.MustSupportST) then
    result := true
  else if (child.Definition.Type_List.count <> 1) then
  begin
    result := true;
    t := child.Definition.Type_List[0].CodeST;
    if (t = 'Address')or (t = 'Contact') or (t = 'ResourceReference') or (t = 'Uri') then
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
  s := v.TextST;
  if (s = '') then
    for i := 0 to v.codingList.Count - 1 do
      if (s = '') then
        s := v.codingList[i].DisplayST;

  if (s = '') then
  begin
    // still? ok, let's try looking it up
    for i := 0 to v.codingList.Count - 1 do
      if (s = '') and (v.codingList[i].Code <> nil) and (v.codingList[i].System <> nil) then
         s := lookupCode(v.codingList[i].SystemST, v.codingList[i].CodeST);
  end;

  if (s = '') then
  begin
    if (v.CodingList.isEmpty) then
      s := ''
    else
      s := v.CodingList[0].CodeST;
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
      sp.addText('{'+describeSystem(v.codingList[i].SystemST)+' code "'+v.codingList[i].CodeST+'" := "'+lookupCode(v.codingList[i].SystemST, v.codingList[i].CodeST)+'", given as "'+v.codingList[i].DisplayST+'"}');
    end;
    sp.addText(')');
  end
  else
  begin
    s1 := '';
    for i := 0 to v.codingList.Count - 1 do
    begin
      if (v.codingList[i].Code <> nil) and (v.codingList[i].System <> nil) then
      begin
        if s1 <> '' then
          s1 := s1 + ', ';
        s1 := s1 + ('{'+v.codingList[i].SystemST+' '+v.codingList[i].CodeST+'}');
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
  if (v.Display <> nil) then
    s := v.DisplayST;
  if (s = '') then
    s := lookupCode(v.SystemST, v.CodeST);

  if (s = '') then
    s := v.CodeST;

  if (showCodeDetails) then
    x.addText(s+' (Details: '+describeSystem(v.SystemST)+' code '+v.CodeST+' := "'+lookupCode(v.SystemST, v.CodeST)+'", stated as "'+v.DisplayST+'")')
  else
    x.addTag('span').setAttribute('title', '{'+v.SystemST+' '+v.CodeST+'}').addText(s);
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

procedure TNarrativeGenerator.renderSchedule(v : TFHIRSchedule; x : TFHIRXhtmlNode);
begin
  x.addText(displaySchedule(v));
end;

procedure TNarrativeGenerator.renderQuantity(v : TFHIRQuantity; x : TFHIRXhtmlNode; showCodeDetails : boolean);
var
  sp : TFHIRXhtmlNode;
begin
  if (v.Comparator <> nil) then
    x.addText(v.Comparator.value);
  x.addText(v.ValueST);
  if (v.Units <> nil) then
    x.addText(' '+v.UnitsST)
  else if (v.Code <> nil) then
    x.addText(' '+v.CodeST);
  if (showCodeDetails) and (v.Code <> nil) then
  begin
    sp := x.addTag('span');
    sp.setAttribute('style', 'background: LightGoldenRodYellow ');
    sp.addText(' (Details: '+describeSystem(v.SystemST)+' code '+v.CodeST+' := "'+lookupCode(v.SystemST, v.CodeST)+'")');
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

procedure TNarrativeGenerator.renderContact(v : TFHIRContact; x : TFHIRXhtmlNode);
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
  if (url.startsWith('#')) then
  begin
    res := res.Contained[url.substring(1)];
    if res <> nil then
      result := TResourceWithReference.Create('', r);
  end
  else if url <> '' then
    result := FOnLookupReference(FContext, url);
end;

function TNarrativeGenerator.displaySchedule(v : TFHIRSchedule) : String;
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
      b.append(rep.DurationST.toString+' '+displayTimeUnits(rep.UnitsST));
      b.append(' ');
      b.append(displayEventCode(rep.WhenST));
    end
    else
    begin
      if (rep.FrequencyST <> 1) then
        b.append('Once per ')
      else
        b.append(Integer.toString(rep.FrequencyST)+' per ');
      b.append(rep.DurationST.toString+' '+displayTimeUnits(rep.UnitsST));
      if (rep.Count <> nil) then
        b.append(' '+Integer.toString(rep.CountST)+' times')
      else if (rep.End <> nil) then
        b.append(' until '+rep.EndST.toHumanDisplay);
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
    if (v.Text <> nil) then
      s.append(v.TextST)
    else
    begin
      for i := 0 to v.givenList.Count - 1 do
      begin
        s.append(v.givenList[i].Value);
        s.append(' ');
      end;
      for i := 0 to v.familyList.Count - 1 do
      begin
        s.append(v.familyList[i].Value);
        s.append(' ');
      end;
    end;
    if (v.Use <> nil) and (v.UseST <> NameUseUsual) then
      s.append('('+v.Use.value+')');
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
    if (v.Text <> nil) then
      s.append(v.TextST)
    else
    begin
      for i := 0 to v.lineList.Count - 1 do
      begin
        s.append(v.lineList[i].Value);
        s.append(' ');
      end;
      if (v.City <> nil) then
      begin
        s.append(v.CityST);
        s.append(' ');
      end;
      if (v.State <> nil) then
      begin
        s.append(v.StateST);
        s.append(' ');
      end;

      if (v.Zip <> nil) then
      begin
        s.append(v.ZipST);
        s.append(' ');
      end;

      if (v.Country <> nil) then
      begin
        s.append(v.CountryST);
        s.append(' ');
      end;
    end;
    if (v.Use <> nil) then
      s.append('('+v.Use.value+')');
    result := s.toString;
  finally
    s.free;
  end;
end;


function TNarrativeGenerator.displayContact(v : TFHIRContact) : String;
begin
  result := describeSystem(v.SystemST);
  if (v.Value = nil) then
    result := result + '-unknown-'
  else
    result := result + v.ValueST;
  if (v.Use <> nil) then
    result := result + '('+v.Use.value+')';
end;

function TNarrativeGenerator.describeSystem(system : TFHIRContactSystem) : String;
begin
  case system of
    ContactSystemNull: result := '';
    ContactSystemPhone: result := 'ph: ';
    ContactSystemFax: result := 'fax: ';
    ContactSystemEmail: result := '';
    ContactSystemUrl: result := '';
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
  if v.value = nil then
    s := '??'
  else
    s := v.ValueST;

  if (v.label_ST <> '') then
    s := v.Label_ST+' := '+s;

  if (v.Use <> nil) then
    s := s + ' ('+v.Use.value+')';
  result := s;
end;


function TNarrativeGenerator.getChildrenForPath(elements : TFhirProfileStructureSnapshotElementList; path : String) : TFhirProfileStructureSnapshotElementList;
var
  i, j : integer;
  e, t : TFhirProfileStructureSnapshotElement;
  name : String;
begin
  // do we need to do a name reference substitution?
  for i := 0  to elements.Count - 1 do
  begin
    e := elements[i];
    if (e.PathST = path) and (e.Definition.NameReferenceST <> '') then
    begin
      name := e.Definition.NameReferenceST;
      t := nil;
      // now, resolve the name
      for j := 0 to elements.Count - 1 do
        if name = elements[j].nameST then
          t := elements[j];
      if (t <> nil) then
        raise Exception.create('Unable to resolve name reference '+name+' trying to resolve '+path);
      path := t.PathST;
      break;
    end;
  end;

  result := TFhirProfileStructureSnapshotElementList.Create;
  try
    for i := 0 to elements.Count - 1 do
    begin
      e := elements[i];
      if e.PathST.startsWith(path+'.') and (not e.PathST.substring(path.length+1).contains('.')) and
         not (e.PathST.endsWith('.extension') or e.PathST.endsWith('.modifierExtension')) then
        result.add(e.Link);
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TNarrativeGenerator.getByName(profile : TFHIRProfile; name : String) : TFhirProfileStructureSnapshot;
var
  i : integer;
begin
  result := nil;
  for I := 0 to profile.structureList.Count - 1 do
    if ((profile.structureList[i].nameST = name) or (profile.structureList[i].type_ST = name)) and (profile.structureList[i].snapshot <> nil) then
      result := profile.structureList[i].snapshot;
  if result = nil then
    raise Exception.create('unable to find snapshot for '+name);
end;

procedure TNarrativeGenerator.inject(res : TFHIRResource; x : TFHIRXhtmlNode; status : TFhirNarrativeStatus);
begin
  if (res.Text = nil) then
    res.Text := TFHIRNarrative.create;
  if (res.Text.div_ = nil) or (res.Text.Div_.ChildNodes.isEmpty) then
  begin
    res.Text.Div_ := x.Link;
    res.Text.StatusST := status;
  end
  else
  begin
    res.Text.Div_.addTag('hr');
    res.Text.Div_.ChildNodes.addAll(x.ChildNodes);
  end;
end;



end.
(*
  public class ResourceWithReference begin

    private String reference;
    private Resource resource;

    public ResourceWithReference(String reference, Resource resource) begin
      this.reference := reference;
      this.resource := resource;
    end;

    public String getReference begin
      return reference;
    end;

    public Resource getResource begin
      return resource;
    end;
  end;
  *)
