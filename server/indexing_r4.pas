unit indexing_r4;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{
Copyright (c) 2001+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

interface

{
outstanding search issues:
* text searching

combinations to enable:
  name[family eq x and given eq y]


// todo: bundle is special...

}
uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_xml,
  fhir_objects, fhir_xhtml, fhir_common,  fhir_utilities, fhir_pathengine,
  fhir4_types, fhir4_resources_base, fhir4_resources, fhir4_constants, fhir4_indexinfo, fhir4_utilities, fhir4_pathengine, fhir4_context,
  fhir_indexing,
  ftx_ucum_services,
  session, indexing, tags, utilities, server_constants;

Type
  TFhirIndexManager4 = class (TFhirIndexManager)
  private
    FMasterKey : Integer;
    FforTesting : boolean;

    procedure GetBoundaries(value : String; comparator: TFhirQuantityComparatorEnum; var low, high : String);

    function EncodeXhtml(r : TFhirDomainResource) : TBytes;
    procedure recordSpace(space : string; key : integer);
    function TypeForKey(key : integer) : String;

    // addToCompartment
//    procedure patientCompartment(key : integer; reference : TFhirReference); overload;
    procedure patientCompartmentNot(key : integer; type_, id : String); overload;
    procedure patientCompartment(key : integer; type_, id : String); overload;

  protected
    // very primitives
    procedure index(aType : String; key, parent : integer; value1, value2, name : String); overload;
    procedure index(aType : String; key, parent : integer; value, name : String); overload;
    procedure index2(aType : String; key, parent : integer; value, name : String); overload;

    // primitives
    procedure index(aType : String; key, parent : integer; value : Boolean; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirString; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirUri; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirEnum; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirInteger; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirDecimal; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirBoolean; name : String); overload;

    // intervals of time
    procedure index(aType : String; key, parent : integer; min, max : TDateTime; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirInstant; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirDateTime; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirDate; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirPeriod; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirTiming; name : String); overload;

    // complexes
    procedure index(aType : String; key, parent : integer; value : TFhirRatio; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirQuantity; name : String; units : string = ''); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirMoney; name : String; units : string = ''); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirRange; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirSampledData; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirCoding; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirCodingList; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirCodeableConcept; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirStructureDefinitionContext; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirCodeableConceptList; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirIdentifier; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirIdentifierList; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirHumanName; name, phoneticName : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirAddress; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirContactPoint; name : String); overload;
    procedure index(appInfo : TFslObject; context : TFhirResource; aType : String; key, parent : integer; value : TFhirReference; name : String; specificType : String = ''); overload;
    procedure index(appInfo : TFslObject; context : TFhirResource; aType : String; key, parent : integer; value : TFhirReferenceList; name : String; specificType : String = ''); overload;

    // structure holder
    function index(aType : String; key, parent : integer; name : String) : Integer; overload;

    procedure processCompartmentTags(key : integer; id: String; tags : TFHIRTagList);
    procedure processUnCompartmentTags(key : integer; id: String; tags : TFHIRTagList);

    procedure checkTags(resource : TFhirResource; tags : TFHIRTagList);
    procedure evaluateByFHIRPath(key : integer; context, resource : TFhirResource; appInfo : TFslObject);
    function transform(base : TFHIRObject; uri : String) : TFHIRObject;
  public
    function Link : TFhirIndexManager4; overload;
    function execute(key : integer; id: String; res : TFhirResourceV; tags : TFHIRTagList; appInfo : TFslObject) : TFslList<TFHIRCompartmentId>; override;
  end;

implementation

Function EncodeNYSIISValue(value : TFhirString) : String; overload;
begin
  if value = nil then
    result := ''
  else
  result := EncodeNYSIIS(value.value);
end;

{ TFhirIndexManager4 }

function TFhirIndexManager4.EncodeXhtml(r: TFhirDomainResource): TBytes;
var
  x, body : TFhirXHtmlNode;
begin
    if r.ResourceType <> frtBinary then
    begin
      x := TFhirXHtmlNode.Create;
      try
        x.NodeType := fhntElement;
        x.Name := 'html';
        x.AddChild('head').AddChild('title').AddText(CODES_TFHIRResourceType[r.ResourceType]);
        body := x.AddChild('body');
        if (r.language = '') then
          body.SetAttribute('lang', 'en')
        else
          body.SetAttribute('lang', r.language);
        if (r.text <> nil) and (r.text.div_ <> nil) then
          body.ChildNodes.Add(r.text.div_.Link);
        result := TEncoding.UTF8.GetBytes(TFHIRXhtmlParser.compose(x)); // don't compress, sql server has to read it.
      finally
        x.Free;
      end;
    end;
end;


procedure TFhirIndexManager4.index(aType : String; key, parent : integer; value: TFhirString; name: String);
begin
  if (value <> nil) then
    index(aType, key, parent, value.value, name);
end;

procedure TFhirIndexManager4.index(aType : String; key, parent : integer; value: TFhirUri; name: String);
begin
  if (value <> nil) then
    index(aType, key, parent, value.value, name);
end;

procedure TFhirIndexManager4.index(aType : String; key, parent : integer; value: TFhirCodeableConcept; name: String);
var
  i : integer;
begin
  if value <> nil then
  begin
    for i := 0 to value.codingList.count - 1 do
      index(aType, key, parent, value.codingList[i], name);
    if value.text <> '' then
      index2(aType, key, parent, value.text, name);
  End;
end;

procedure TFhirIndexManager4.index(aType : String; key, parent : integer; value, name: String);
var
  ndx : TFhirIndex;
  types : TFhirSearchParamTypeList;

begin
  if (value = '') then
    exit;
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name+' on type '+aType);

  if StringIsInteger32(value) then
    types := [sptString, sptToken, sptDate, sptReference, sptNumber, sptUri]
  else
    types := [sptString, sptToken, sptDate, sptReference, sptUri];
  if not (ndx.SearchType in types) then //todo: fix up text
    raise EFHIRException.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing string');
//  if ndx.SearchType = sptString then
    value := lowercase(RemoveAccents(copy(value, 1, INDEX_ENTRY_LENGTH)));
//  else if (length(value) > INDEX_ENTRY_LENGTH) then
//     raise EFHIRException.create('string too long for indexing: '+value+ ' ('+inttostr(length(value))+' chars)');
  FEntries.add(FConnection, key, parent, ndx, 0, value, '', 0, '', ndx.SearchType);
end;

procedure TFhirIndexManager4.index2(aType : String; key, parent : integer; value, name: String);
var
  ndx : TFhirIndex;
begin
  if (value = '') then
    exit;
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name+' on type '+aType);
  if not (ndx.SearchType in [sptToken, sptReference]) then //todo: fix up text
    raise EFHIRException.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing string');
  value := lowercase(RemoveAccents(copy(value, 1, INDEX_ENTRY_LENGTH)));
  FEntries.add(FConnection, key, parent, ndx, 0, '', value, 0, '', sptString);
end;

function TFhirIndexManager4.Link: TFhirIndexManager4;
begin
  result := TFhirIndexManager4 (inherited Link);
end;

procedure TFhirIndexManager4.index(aType : String; key, parent : integer; value1, value2, name: String);
var
  ndx : TFhirIndex;
begin
  if (value1 = '') then
    exit;
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name+' on type '+aType);
  if not (ndx.SearchType in [sptString, sptToken, sptDate]) then //todo: fix up text
    raise EFHIRException.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing string');

  if ndx.SearchType = sptString then
    value1 := lowercase(RemoveAccents(copy(value1, 1, INDEX_ENTRY_LENGTH)))
  else  if (length(value1) > INDEX_ENTRY_LENGTH) then
    raise EFHIRException.create('string too long for indexing: '+value1+ ' ('+inttostr(length(value1))+' chars)');

  if ndx.SearchType = sptString then
    value2 := lowercase(RemoveAccents(copy(value2, 1, INDEX_ENTRY_LENGTH)))
  else if (length(value2) > INDEX_ENTRY_LENGTH) then
    raise EFHIRException.create('string too long for indexing: '+value2+ ' ('+inttostr(length(value2))+' chars)');

  FEntries.add(FConnection, key, parent, ndx, 0, value1, value2, 0, '', ndx.SearchType);
end;


procedure TFhirIndexManager4.index(aType: String; key, parent: integer; value: Boolean; name: String);
var
  ndx : TFhirIndex;
  concept : integer;
begin
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name+' on type '+aType);
  if not (ndx.SearchType in [sptToken, sptString]) then //todo: fix up text
    raise EFHIRException.create('Unsuitable index '+name+' of type '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing enumeration on '+aType);
  concept := TerminologyServer.enterIntoClosure(FConnection, aType+'.'+name, 'http://hl7.org/fhir/special-values', BooleanToString(value));
  assert(concept <> 0);
  FEntries.add(FConnection, key, parent, ndx, 0, BooleanToString(value), '', 0, '', ndx.SearchType, false, concept);
end;


procedure TFhirIndexManager4.index(aType : String; key, parent : integer; value: TFhirEnum; name: String);
var
  ndx : TFhirIndex;
  concept : integer;
begin
  if (value = nil) or (value.value = '') then
    exit;

  if (value.system = '') then
    exit;
//    raise EFHIRException.create('no system provided');

  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name+' on type '+aType);
  if not (ndx.SearchType in [sptToken]) then //todo: fix up text
    raise EFHIRException.create('Unsuitable index '+name+' of type '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing enumeration on '+aType);
  if (length(value.value) > INDEX_ENTRY_LENGTH) then
     raise EFHIRException.create('string too long for indexing: '+value.value+ ' ('+inttostr(length(value.value))+' chars)');
  if value.system <> '' then
  begin
    concept := TerminologyServer.enterIntoClosure(FConnection, aType+'.'+name, value.system, value.value);
    assert(concept <> 0);
  end
  else
    concept := 0;

  FEntries.add(FConnection, key, parent, ndx, 0, value.value, '', 0, '', ndx.SearchType, false, concept);
end;


procedure TFhirIndexManager4.index(aType : String; key, parent : integer; value: TFhirInstant; name: String);
begin
  if (value <> nil) and (value.value.notNull) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

function TFhirIndexManager4.transform(base: TFHIRObject; uri: String): TFHIRObject;
begin
  raise EFHIRTodo.create('TFhirIndexManager4.transform');
end;

function TFhirIndexManager4.TypeForKey(key: integer): String;
var
  t : TFHIRResourceConfig;
begin
  result := '';
  for t in FResConfig.Values do
    if t.key = key then
      result := t.name;
end;

procedure TFhirIndexManager4.evaluateByFHIRPath(key : integer; context, resource: TFhirResource; appInfo : TFslObject);
var
  path : TFHIRPathEngine;
  i : integer;
  ndx : TFhirIndex;
  matches : TFHIRSelectionList;
  match : TFHIRSelection;
  work : TFHIRObject;
  a : TFhirResourceType;
  s : string;
  ie : TFhirIndexEntry;
begin
  path := engine as TFHIRPathEngine;
  for i := 0 to FInfo.Indexes.Count - 1 do
  begin
    ndx := FInfo.Indexes[i];

    if (ndx.Path <> '') and (ndx.ResourceType = resource.fhirType) then
    begin
      matches := path.evaluate(appInfo, resource, ndx.Path);
      try
        for match in matches do
        begin
          // custom resource support : do we need to do a transform?
          if ndx.mapping = '' then
            work := match.value.Link
          else
            work := transform(match.value, ndx.mapping);
          try
            if ndx.SearchType = sptComposite then
              // ignore for now
            else case ndx.Usage of
              sxpNull: raise EFHIRException.create('Path is not defined properly');
              sxpNormal:
                begin
                if work is TFhirString then
                  index(resource.fhirType, key, 0, TFhirString(work), ndx.Name)
                else if work is TFhirUri then
                  index(resource.fhirType, key, 0, TFhirUri(work), ndx.Name)
                else if work is TFhirEnum then
                  index(resource.fhirType, key, 0, TFhirEnum(work), ndx.Name)
                else if work is TFhirInteger  then
                  index(resource.fhirType, key, 0, TFhirInteger(work), ndx.Name)
                else if work is TFhirDecimal then
                  index(resource.fhirType, key, 0, TFhirDecimal(work), ndx.Name)
                else if work is TFhirBoolean  then
                  index(resource.fhirType, key, 0, TFhirBoolean(work), ndx.Name)
                else if work is TFhirInstant  then
                  index(resource.fhirType, key, 0, TFhirInstant(work), ndx.Name)
                else if work is TFhirDateTime  then
                  index(resource.fhirType, key, 0, TFhirDateTime(work), ndx.Name)
                else if work is TFhirDate  then
                  index(resource.fhirType, key, 0, TFhirDate(work), ndx.Name)
                else if work is TFhirPeriod  then
                  index(resource.fhirType, key, 0, TFhirPeriod(work), ndx.Name)
                else if work is TFhirTiming  then
                  index(resource.fhirType, key, 0, TFhirTiming(work), ndx.Name)
                else if work is TFhirRatio  then
                  index(resource.fhirType, key, 0, TFhirRatio(work), ndx.Name)
                else if work is TFhirQuantity  then
                  index(resource.fhirType, key, 0, TFhirQuantity(work), ndx.Name)
                else if work is TFhirRange  then
                  index(resource.fhirType, key, 0, TFhirRange(work), ndx.Name)
                else if work is TFhirSampledData  then
                  index(resource.fhirType, key, 0, TFhirSampledData(work), ndx.Name)
                else if work is TFhirCoding  then
                  index(resource.fhirType, key, 0, TFhirCoding(work), ndx.Name)
                else if work is TFhirCodeableConcept  then
                  index(resource.fhirType, key, 0, TFhirCodeableConcept(work), ndx.Name)
                else if work is TFhirIdentifier  then
                  index(resource.fhirType, key, 0, TFhirIdentifier(work), ndx.Name)
                else if work is TFhirHumanName  then
                  index(resource.fhirType, key, 0, TFhirHumanName(work), ndx.Name, '')
                else if work is TFhirAddress  then
                  index(resource.fhirType, key, 0, TFhirAddress(work), ndx.Name)
                else if work is TFhirContactPoint  then
                  index(resource.fhirType, key, 0, TFhirContactPoint(work), ndx.Name)
                else if work is TFhirReference then
                  index(appInfo, context, resource.fhirType, key, 0, TFhirReference(work), ndx.Name, ndx.specifiedTarget)
                else if work is TFhirMoney then
                  index(resource.fhirType, key, 0, TFhirMoney(work), ndx.Name)
                else if work is TFhirStructureDefinitionContext then
                  index(resource.fhirType, key, 0, TFhirStructureDefinitionContext(work), ndx.Name)
                else if work is TFhirResource then
                  // index(context, resource.fhirType, key, 0, TFhirReference(work), ndx.Name, ndx.specifiedTarget)
                else if not (work is TFHIRAttachment) and not (work is TFHIRBase64Binary) then
                  raise EFHIRException.create('The type '+work.FhirType+' is not supported in FIndexManager for the index '+ndx.Name+' for the expression '+ndx.Path);
                end;
              sxpPhonetic:
                begin
                if work is TFhirString then
                  index(resource.fhirType, key, 0, EncodeNYSIIS(TFhirString(work).value), ndx.Name)
                else if work is TFhirHumanName then
                  index(resource.fhirType, key, 0, TFhirHumanName(work), '', ndx.Name)
                else
                  raise EFHIRException.create('The type '+work.FhirType+' is not supported in FIndexManager for the index '+ndx.Name+' for the expression '+ndx.Path);
                end;
              sxpNearby:
                begin
                  // todo when a chance arises
                end;
              sxpDistance:
                begin
                  // todo when a chance arises
                end;
              sxpOther:
                begin
                  // todo when a chance arises
                end;
            end;
          finally
            work.Free;
          end;
        end;
      finally
        matches.free;
      end;
    end;
  end;

  // ok, now compartment information
  for a in [frtPatient, frtPractitioner, frtRelatedPerson, frtEncounter, frtDevice] do
  begin
    // a resource is automatically in it's own compartment
    if (a = resource.ResourceType) then
      FCompartments.add(key, FResConfig[CODES_TFHIRResourceType[a]].key, key, resource.fhirType, resource.id);
    if FInfo.Compartments.existsInCompartment(CODES_TFHIRResourceType[a], resource.fhirType) then
      for s in FInfo.Compartments.getIndexNames(CODES_TFHIRResourceType[a], resource.fhirType) do
      begin
        if (s <> '{def}') and not s.Contains('.') then // we already did this one above, so we just ignore this here and compartments with '.' in them are in error
        begin
          ndx := FInfo.Indexes.getByName(resource.fhirType, s);
          if (ndx = nil) then
            raise EFHIRException.create('Unknown index '+s+' on '+CODES_TFhirResourceType[resource.ResourceType]);
          for ie in FEntries do
          begin
            if (ie.Key = key) and (ie.IndexKey = ndx.Key) and (ie.TargetType = CODES_TFhirResourceType[a]) then
              FCompartments.add(key, FResConfig[CODES_TFHIRResourceType[a]].key, ie.Target, CODES_TFhirResourceType[a], ie.Value1);
          end;
        end;
      end;
  end;
end;

function TFhirIndexManager4.execute(key : integer; id : String; res : TFhirResourceV; tags : TFHIRTagList; appInfo : TFslObject) : TFslList<TFHIRCompartmentId>;
var
  i : integer;
  entry : TFhirIndexEntry;
  dummy : string;
  keys : string;
  comps: TFslList<TFHIRCompartmentId>;
  resource : TFhirResource;
begin
  result := nil;
  resource := res as TFhirResource;

  checkTags(resource, tags);
  FforTesting := tags.hasTestingTag;
  FEntries.clear;
  FEntries.KeyEvent := KeyEvent;

  FMasterKey := key;
  keys := '';
  FConnection.sql := 'select ResourceKey from Ids where MasterResourceKey = '+inttostr(key);
  FConnection.prepare;
  FConnection.execute;
  while FConnection.fetchnext do
    CommaAdd(keys, FConnection.ColStringByName['ResourceKey']);
  FConnection.terminate;

  if keys <> '' then
  begin
    FConnection.ExecSQL('delete from Compartments where ResourceKey in ('+keys+')');
    FConnection.ExecSQL('update IndexEntries set Flag = 2 where ResourceKey in ('+keys+') or Target in ('+keys+')');
    FConnection.ExecSQL('delete from SearchEntries where ResourceKey in ('+keys+')');
  end;
  FConnection.ExecSQL('update Ids set deleted = 1 where MasterResourceKey = '+inttostr(key));
  FCompartments.Clear;

  processCompartmentTags(key, id, tags);
  evaluateByFHIRPath(key, resource, resource, appInfo);
  processUnCompartmentTags(key, id, tags);

  if resource is TFhirDomainResource then
  begin
    FConnection.SQL := 'insert into IndexEntries (EntryKey, IndexKey, ResourceKey, SrcTesting, Flag, Extension, Xhtml) values (:k, :i, :r, :ft, 1, ''html'', :xb)';
    FConnection.prepare;
    FConnection.BindInteger('k', FKeyEvent(FConnection, ktEntries, '', dummy));
    FConnection.BindInteger('i', FInfo.NarrativeIndex);
    FConnection.BindInteger('r', key);
    FConnection.BindIntegerFromBoolean('ft', FforTesting);
    FConnection.BindBlob('xb', EncodeXhtml(TFhirDomainResource(resource)));
    FConnection.execute;
    FConnection.terminate;
  end;

  FConnection.SQL := 'insert into IndexEntries (EntryKey, IndexKey, ResourceKey, Parent, MasterResourceKey, SpaceKey, Value, Value2, SrcTesting, Flag, target, concept) values (:k, :i, :r, :p, :m, :s, :v, :v2, :ft, :f, :t, :c)';
  FConnection.prepare;
  for i := 0 to FEntries.Count - 1 Do
  begin
    entry := FEntries[i];
    FConnection.BindInteger('k', FEntries[i].EntryKey);
    FConnection.BindInteger('i', entry.IndexKey);
    FConnection.BindInteger('r', entry.key);
    if entry.parent = 0 then
      FConnection.BindNull('p')
    else
      FConnection.BindInteger('p', entry.parent);
    if entry.key <> key then
      FConnection.BindInteger('m', key)
    else
      FConnection.BindNull('m');
    if entry.Flag then
      FConnection.BindInteger('f', 1)
    else
      FConnection.BindInteger('f', 0);
    if entry.concept = 0 then
      FConnection.BindNull('c')
    else
      FConnection.BindInteger('c', entry.concept);

    if entry.RefType = 0 then
      FConnection.BindNull('s')
    else
      FConnection.BindInteger('s', entry.RefType);
    FConnection.BindIntegerFromBoolean('ft', FforTesting);
    FConnection.BindString('v', entry.Value1);
    FConnection.BindString('v2', entry.Value2);
    if (entry.Target = 0) or (entry.Target = FMasterKey) then
      FConnection.BindNull('t')
    else
      FConnection.BindInteger('t', entry.target);
    try
      FConnection.execute;
    except
      on e:exception do
        raise EFHIRException.create('Exception storing values "'+entry.Value1+'" and "'+entry.Value2+'": '+e.message);
    end;
  end;
  FConnection.terminate;

  comps := TFslList<TFHIRCompartmentId>.create;
  try
    if FCompartments.Count > 0 then
    begin
      FConnection.SQL := 'insert into Compartments (ResourceCompartmentKey, ResourceKey, TypeKey, CompartmentKey, Id) values (:pk, :r, :ct, :ck, :id)';
      FConnection.prepare;
      for i := 0 to FCompartments.Count - 1 Do
      begin
        comps.Add(TFhirCompartmentId.Create(FCompartments[i].Enum, FCompartments[i].Id));
        FConnection.BindInteger('pk', FKeyEvent(FConnection, ktCompartment, '', dummy));
        FConnection.BindInteger('r', FCompartments[i].key);
        FConnection.BindInteger('ct', FCompartments[i].typekey);
        FConnection.BindString('id', FCompartments[i].id);
        if FCompartments[i].ckey > 0 then
          FConnection.BindInteger('ck', FCompartments[i].ckey)
        else
          FConnection.BindNull('ck');
        FConnection.execute;
      end;
      FConnection.terminate;
    end;
    result := comps.link;
  finally
    result.free;
  end;
end;

procedure TFhirIndexManager4.index(aType : String; key, parent : integer; value: TFhirCoding; name: String);
var
  ndx : TFhirIndex;
  ref : integer;
  concept : integer;
begin
  if (value = nil) or (value.code = '') then
    exit;
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index a simple type in an index that is a resource join');
  if ndx.SearchType <> sptToken then
    raise EFHIRException.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing Coding');
  if (value.system <> '') then
  begin
    if not FSpaces.ResolveSpace(value.system, ref) then
      RecordSpace(value.system, ref);
    concept := TerminologyServer.enterIntoClosure(FConnection, aType+'.'+name, value.system, value.code);
  end
  else
  begin
    ref := 0;
    concept := 0;
  end;

  if (length(value.code) > INDEX_ENTRY_LENGTH) then
    raise EFHIRException.create('code too long for indexing: '+value.code);
  if value.display <> '' then
    FEntries.add(FConnection, key, parent, ndx, ref, value.code, lowercase(RemoveAccents(copy(value.display, 1, INDEX_ENTRY_LENGTH))), 0, '', ndx.SearchType, false, concept)
  else
    FEntries.add(FConnection, key, parent, ndx, ref, value.code, '', 0, '', ndx.SearchType, false, concept);
end;

Function ComparatorPrefix(v : String; c : TFhirQuantityComparatorEnum) : String;
begin
  case c of
    QuantityComparatorLessThan : result := '<'+v;
    QuantityComparatorLessOrEquals : result := '<='+v;
    QuantityComparatorGreaterOrEquals : result := '>='+v;
    QuantityComparatorGreaterThan : result := '>'+v;
  else
    result := v;
  end;
end;

procedure TFhirIndexManager4.GetBoundaries(value : String; comparator: TFhirQuantityComparatorEnum; var low, high : String);
var
  dec : TFslDecimal;
begin
  dec := TFslDecimal.ValueOf(value);
  case comparator of
    QuantityComparatorNull :
      begin
      low := dec.lowerBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, false);
      high := dec.upperBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      end;
    QuantityComparatorLessThan :
      begin
      low := TFslDecimal.makeInfinity.Negated.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      high := dec.upperBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      end;
    QuantityComparatorLessOrEquals :
      begin
      low := TFslDecimal.makeInfinity.Negated.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      high := dec.immediateLowerBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      end;
    QuantityComparatorGreaterOrEquals :
      begin
      low := dec.lowerBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, false);
      high := TFslDecimal.makeInfinity.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      end;
    QuantityComparatorGreaterThan :
      begin
      low := dec.immediateUpperBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, false);
      high := TFslDecimal.makeInfinity.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      end;
  end;
end;


procedure TFhirIndexManager4.index(aType : String; key, parent : integer; value : TFhirRange; name : String);
var
  ndx : TFhirIndex;
  v1, v2, crap : String;
  ref : integer;
  specified, canonical : TUcumPair;
begin
  if value = nil then
    exit;

  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index a simple type in an index that is a resource join: "'+name+'"');
  if not (ndx.SearchType in [sptToken, sptNumber, sptQuantity]) then
    raise EFHIRException.create('Unsuitable index "'+name+'" '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing range');

  if (value.low = nil) then
    v1 := TFslDecimal.makeInfinity.Negated.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, false)
  else
    GetBoundaries(value.low.value, QuantityComparatorNull, v1, crap);
  if (value.high = nil) then
    v2 := TFslDecimal.makeInfinity.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true)
  else
    GetBoundaries(value.high.value, QuantityComparatorNull, crap, v2);

  if (length(v1) > INDEX_ENTRY_LENGTH) then
      raise EFHIRException.create('quantity.value too long for indexing: "'+v1+ '" ('+inttostr(length(v1))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
  if (length(v2) > INDEX_ENTRY_LENGTH) then
      raise EFHIRException.create('quantity.value too long for indexing: "'+v2+ '" ('+inttostr(length(v2))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
  if not FSpaces.ResolveSpace(value.low.unit_, ref) then
    recordSpace(value.low.unit_, ref);
  FEntries.add(FConnection, key, parent, ndx, ref, v1, v2, 0, '', ndx.SearchType);
  if value.low.system <> '' then
  begin
    if not FSpaces.ResolveSpace(value.low.system+'#'+value.low.code, ref) then
      recordSpace(value.low.system, ref);
    FEntries.add(FConnection, key, parent, ndx, ref, v1, v2, 0, '', ndx.SearchType);
  end;

  // ok, if there's a ucum code:
  if (value.low.code <> '') and (value.low.system = 'http://unitsofmeasure.org') and (FTerminologyServer.CommonTerminologies.Ucum <> nil) then
  begin
    specified := TUcumPair.create;
    try
      specified.Value := TFslDecimal.ValueOf(value.low.value);
      specified.UnitCode := value.low.code;
      canonical := FTerminologyServer.CommonTerminologies.Ucum.getCanonicalForm(specified);
      try
        GetBoundaries(canonical.Value.AsString, QuantityComparatorNull, v1, v2);
        if (length(v1) > INDEX_ENTRY_LENGTH) then
          raise EFHIRException.create('quantity.value too long for indexing: "'+v1+ '" ('+inttostr(length(v1))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
        if (length(v2) > INDEX_ENTRY_LENGTH) then
          raise EFHIRException.create('quantity.value too long for indexing: "'+v2+ '" ('+inttostr(length(v2))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
        if not FSpaces.ResolveSpace('urn:ucum-canonical#'+canonical.UnitCode, ref) then
          recordSpace('urn:ucum-canonical#'+canonical.UnitCode, ref);
        FEntries.add(FConnection, key, parent, ndx, ref, v1, v2, 0, '', ndx.SearchType, true);
      finally
        canonical.free;
      end;
    finally
      specified.free;
    end;
  end;
end;

procedure TFhirIndexManager4.index(aType : String; key, parent : integer; value : TFhirQuantity; name : String; units : string = '');
var
  ndx : TFhirIndex;
  v1, v2 : String;
  ref : integer;
  specified, canonical : TUcumPair;
begin
  if value = nil then
    exit;
  if value.value = '' then
    exit;

  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index a simple type in an index that is a resource join: "'+name+'"');
  if not (ndx.SearchType in [sptToken, sptNumber, sptQuantity]) then
    raise EFHIRException.create('Unsuitable index "'+name+'" '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing quantity');

  GetBoundaries(value.value, value.comparator, v1, v2);

  if (length(v1) > INDEX_ENTRY_LENGTH) then
      raise EFHIRException.create('quantity.value too long for indexing: "'+v1+ '" ('+inttostr(length(v1))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
  if (length(v2) > INDEX_ENTRY_LENGTH) then
      raise EFHIRException.create('quantity.value too long for indexing: "'+v2+ '" ('+inttostr(length(v2))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
  if not FSpaces.ResolveSpace(value.unit_, ref) then
    recordSpace(value.unit_, ref);
  FEntries.add(FConnection, key, parent, ndx, ref, v1, v2, 0, '', ndx.SearchType);
  if value.system <> '' then
  begin
    if not FSpaces.ResolveSpace(value.system+'#'+value.code, ref) then
      recordSpace(value.system+'#'+value.code, ref);
    FEntries.add(FConnection, key, parent, ndx, ref, v1, v2, 0, '', ndx.SearchType);
  end;

  // ok, if there's a ucum code:
  if (value.code <> '') and (value.system = 'http://unitsofmeasure.org') and (FTerminologyServer.CommonTerminologies.Ucum <> Nil) then
  begin
    specified := TUcumPair.create;
    try
      specified.Value := TFslDecimal.ValueOf(value.value);
      specified.UnitCode := value.code;
      canonical := FTerminologyServer.CommonTerminologies.Ucum.getCanonicalForm(specified);
      try
        GetBoundaries(canonical.Value.AsString, value.comparator, v1, v2);
        if (length(v1) > INDEX_ENTRY_LENGTH) then
          raise EFHIRException.create('quantity.value too long for indexing: "'+v1+ '" ('+inttostr(length(v1))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
        if (length(v2) > INDEX_ENTRY_LENGTH) then
          raise EFHIRException.create('quantity.value too long for indexing: "'+v2+ '" ('+inttostr(length(v2))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
        if not FSpaces.ResolveSpace('urn:ucum-canonical#'+canonical.UnitCode, ref) then
          recordSpace('urn:ucum-canonical#'+canonical.UnitCode, ref);
        FEntries.add(FConnection, key, parent, ndx, ref, v1, v2, 0, '', ndx.SearchType, true);
      finally
        canonical.free;
      end;
    finally
      specified.free;
    end;
  end;
end;

procedure TFhirIndexManager4.index(aType : String; key, parent : integer; value : TFhirPeriod; name : String);
begin
  if (value <> nil) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager4.index(aType : String; key, parent : integer; value : TFhirTiming; name : String);
begin
  if (value <> nil) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager4.index(aType : String; key, parent : integer; value: TFhirDateTime; name: String);
begin
  if (value <> nil) and (value.value.notNull) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager4.index(aType : String; key, parent : integer; min, max : TDateTime; name: String);
var
  ndx : TFhirIndex;
begin
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType = sptDate) then
    raise EFHIRException.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing date');
  FEntries.add(FConnection, key, parent, ndx, 0, TFslDateTime.make(min, dttzUnknown).toHL7, TFslDateTime.make(max, dttzUnknown).toHL7, 0, '', ndx.SearchType);
end;

procedure TFhirIndexManager4.index(aType : String; key, parent : integer; value: TFhirIdentifier; name: String);
var
  ndx : TFhirIndex;
  ref : integer;
begin
  if (value = nil) or (value.value = '') then
    exit;
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index an identifier in an index that is a resource join, index name '+name);
  if not (ndx.SearchType in [sptToken]) then
    raise EFHIRException.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing Identifier');
  ref := 0;
  if (value.system <> '') then
    if not FSpaces.ResolveSpace(value.system, ref) then
      recordSpace(value.system, ref);
  if (length(value.value) > INDEX_ENTRY_LENGTH) then
    raise EFHIRException.create('id too long for indexing: '+value.value);
  FEntries.add(FConnection, key, parent, ndx, ref, value.value, '', 0, '', ndx.SearchType);
end;

procedure TFhirIndexManager4.index(aType : String; key, parent : integer; value: TFhirAddress; name: String);
var
  i : integer;
begin
  if (value = nil) then
    exit;
  for i := 0 to value.lineList.count - 1 do
    index(aType, key, parent, value.lineList[i], name);
  index(aType, key, parent, value.cityElement, name);
  index(aType, key, parent, value.stateElement, name);
  index(aType, key, parent, value.countryElement, name);
  index(aType, key, parent, value.postalCodeElement, name);

  index(aType, key, parent, value.cityElement, name+'-city');
  index(aType, key, parent, value.countryElement, name+'-country');
  index(aType, key, parent, value.postalCodeElement, name+'-postalcode');
  index(aType, key, parent, value.stateElement, name+'-state');
  index(aType, key, parent, value.useElement, name+'-use');
end;

procedure TFhirIndexManager4.index(aType : String; key, parent : integer; value: TFhirContactPoint; name: String);
var
  ndx : TFhirIndex;
  ref : integer;
begin
  if (value = nil) or (value.value = '') then
    exit;
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType in [sptToken, sptString]) then
    raise EFHIRException.create('Unsuitable index '+name+':'+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing Contact on '+aType);
  ref := 0;
  if (value.systemElement <> nil) and (value.systemElement.value <> '') then
    if not FSpaces.ResolveSpace(value.systemElement.value, ref) then
      recordSpace(value.systemElement.value, ref);
  if (length(value.value) > INDEX_ENTRY_LENGTH) then
    raise EFHIRException.create('contact value too long for indexing: '+value.value);
  FEntries.add(FConnection, key, parent, ndx, ref, value.value, '', 0, '', ndx.SearchType);
end;

procedure TFhirIndexManager4.index(aType: String; key, parent: integer; value: TFhirIdentifierList; name: String);
var
  i : integer;
begin
  if (value <> nil) then
    for i := 0 to value.Count - 1 do
      index(atype, key, parent, value[i], name);
end;

procedure TFhirIndexManager4.index(aType: String; key, parent: integer; value: TFhirCodingList; name: String);
var
  i : integer;
begin
  if (value <> nil) then
    for i := 0 to value.Count - 1 do
      index(atype, key, parent, value[i], name);
end;

procedure TFhirIndexManager4.index(aType: String; key, parent: integer; value: TFhirCodeableConceptList; name: String);
var
  i : integer;
begin
  if (value <> nil) then
    for i := 0 to value.Count - 1 do
      index(atype, key, parent, value[i], name);
end;

procedure TFhirIndexManager4.index(aType: String; key, parent: integer; value: TFhirSampledData; name: String);
begin
 // todo
end;

procedure TFhirIndexManager4.index(aType: String; key, parent: integer; value: TFhirRatio; name: String);
begin
  // don't have a clue what to do here
end;

procedure TFhirIndexManager4.index(aType : String; key, parent : integer; value: TFhirHumanName; name, phoneticName: String);
var
  i : integer;
  s : String;
begin
  if (value = nil) then
    exit;
  if (name <> '') then
  begin
    index(aType, key, parent, value.text, name);
    if value.family <> '' then
      for s in value.family.Split([' ', '-']) do
        index(aType, key, parent, s, name);
    for i := 0 to value.givenList.count - 1 do
      index(aType, key, parent, value.givenList[i], name);
    for i := 0 to value.prefixList.count - 1 do
      index(aType, key, parent, value.prefixList[i], name);
    for i := 0 to value.suffixList.count - 1 do
      index(aType, key, parent, value.suffixList[i], name);
  end;

  if phoneticName <> '' then
  begin
    if value.family <> '' then
      for s in value.family.Split([' ', '-']) do
        index(aType, key, parent, EncodeNYSIIS(s), phoneticName);
    for i := 0 to value.givenList.count - 1 do
      index(aType, key, parent, EncodeNYSIIS(value.givenList[i].value), phoneticName);
    for i := 0 to value.prefixList.count - 1 do
      index(aType, key, parent, EncodeNYSIIS(value.prefixList[i].value), phoneticName);
    for i := 0 to value.suffixList.count - 1 do
      index(aType, key, parent, EncodeNYSIIS(value.suffixList[i].value), phoneticName);
  end;
end;

{
procedure TFhirIndexManager4.index(aType : String; key, parent : integer; value: TFhirDecimal; name: String);
var
  ndx : TFhirIndex;
begin
  if (value = nil) or (value.value = '') then
    exit;
  ndx := FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType in [sptString, sptToken]) then
    raise EFHIRException.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing decimal');
  FEntries.add(FConnection, key, ndx, 0, value.value, '', 0, ndx.SearchType);
end;
}

function isLocalTypeReference(url : String; var type_, id : String) : boolean;
var
  p : TArray<String>;
begin
  p := url.Split(['/']);
  if (length(p) = 2) or ((length(p) = 4) and (p[2] = '_history')) then
  begin
    result := isResourceName(p[0]) and IsId(p[1]);
    if result then
    begin
      type_ := p[0];
      id := p[1];
    end;
  end
  else
    result := false;
end;

function sumContainedResources(resource : TFhirDomainResource) : string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to resource.containedList.Count - 1 do
    result := result + ',' + resource.containedList[i].xmlId;
  delete(result, 1, 1);
end;

procedure TFhirIndexManager4.index(appInfo : TFslObject; context : TFhirResource; aType : String; key, parent : integer; value: TFhirReference; name: String; specificType : String = '');
var
  ndx : TFhirIndex;
  ref, i : integer;
  target : integer;
  type_, id : String;
  contained : TFhirResource;
  url : String;
  ok : boolean;
  ttype : TFhirResourceType;
begin
  ttype := frtNull;
  if (value = nil) then
    exit;
  if (value.reference = '') and (value.display <> '') then
  begin
    index(aType, key, parent, value.displayElement, name);
    exit;
  end;
  if (value.reference = '') then
    exit;

  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) and (name = 'patient') then
    ndx := FInfo.Indexes.getByName(aType, 'subject');
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
// ggtodo - until target types are sorted out....
//  if (ndx.TargetTypes = []) then
//    raise EFHIRException.create('Attempt to index a resource join in an index ('+aType+'/'+name+') that is a not a join (has no target types)');
  if ndx.SearchType <> sptReference then
    raise EFHIRException.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing reference on a '+aType);

  if (length(value.reference) > INDEX_ENTRY_LENGTH) then
    raise EFHIRException.create('resource url too long for indexing: '+value.reference);

 {
  ! if the value has a value, then we need to index the value, even though we don't actually have it as a resource
  ! what we do is construct it with a fictional GUID id and index that
  }

  target := 0;
  ref := 0;
  ok := false;

  if StringStartsWith(value.reference, '#') then
  begin
    if context is TFhirDomainResource then
      contained := FindContainedResource(TFhirDomainResource(context), value)
    else
      raise EFHIRException.create('Reference to contained resource found in a resource that does not have contained resources"');
    if contained = nil then
      raise EFHIRException.create('No contained resource found in resource for "'+value.reference+'", list from '+CODES_TFHIRResourceType[context.ResourceType]+' = "'+sumContainedResources(TFhirDomainResource(context))+'"');
    if (specificType = '') or (contained.fhirType = specificType) then
    begin
      ttype := contained.ResourceType;
      if not FSpaces.ResolveSpace(CODES_TFHIRResourceType[contained.ResourceType], ref) then
        recordSpace(CODES_TFHIRResourceType[contained.ResourceType], ref);
      target := FKeyEvent(FConnection, ktResource, contained.fhirType, id);
      FConnection.execSql('update Types set LastId = '+id+' where ResourceTypeKey = '+inttostr(ref)+' and LastId < '+id);
      FConnection.SQL := 'insert into Ids (ResourceKey, ResourceTypeKey, Id, MostRecent, MasterResourceKey, ForTesting) values (:k, :r, :i, null, '+inttostr(FMasterKey)+', :ft)';
      FConnection.Prepare;
      FConnection.BindInteger('k', target);
      FConnection.BindInteger('r', ref);
      FConnection.BindString('i', id);
      FConnection.BindIntegerFromBoolean('ft', FforTesting);
      FConnection.Execute;
      FConnection.Terminate;
      evaluateByFHIRPath(target, context, contained, appInfo);
      ok := true;
    end;
  end
  else
  begin
    url := value.reference;
    for i := 0 to FBases.Count -1 do
    begin
      if StringStartsWith(url, FBases[i]+'/') then
        url := copy(Url, length(FBases[i])+2, $FFFF);
    end;
    if isLocalTypeReference(url, type_, id) then
    begin
      if (specificType = '') or (type_ = specificType) then
      begin
        ttype := ResourceTypeByName(type_);
        if not FSpaces.ResolveSpace(type_, ref) then
          recordSpace(type_, ref);

        FConnection.sql := 'Select ResourceKey from Ids as i, Types as t where i.ResourceTypeKey = t.ResourceTypeKey and ResourceName = :t and Id = :id';
        FConnection.Prepare;
        FConnection.BindString('t', type_);
        FConnection.BindString('id', id);
        FConnection.Execute;
        if FConnection.FetchNext then
          target := FConnection.ColIntegerByName['ResourceKey']; // otherwise we try and link it up if we ever see the resource that this refers to
        FConnection.Terminate;
        ok := true;
      end;
    end
    else if url.startsWith('http:') or url.startsWith('https:') then
    begin
      id := url;
      ok := true;
    end;
  end;

  if ok then
    FEntries.add(FConnection, key, parent, ndx, ref, id, '', target, CODES_TFhirResourceType[ttype], ndx.SearchType);
end;


procedure TFhirIndexManager4.index(aType: String; key, parent: integer; value: TFhirInteger; name: String);
var
  ndx : TFhirIndex;
  v1, v2 : String;
begin
  if (value = nil) or (value.value = '') then
    exit;
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType in [sptString, sptNumber, sptToken]) then
    raise EFHIRException.create('Unsuitable index '+name+' : '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing integer');
  GetBoundaries(value.value, QuantityComparatorNull, v1, v2);
  FEntries.add(FConnection, key, parent, ndx, 0, v1, v2, 0, '', ndx.SearchType);
end;




procedure TFhirIndexManager4.index(aType: String; key, parent: integer; value: TFhirDate; name: String);
begin
  if (value <> nil) and (value.value.notNull) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager4.index(aType: String; key, parent: integer; value: TFhirBoolean; name: String);
begin
  if (value <> nil) then
    index(aType, key, parent, value.value, name);
end;

//procedure TFhirIndexManager4.patientCompartment(key : integer; reference: TFhirReference);
//var
//  sid : string;
//begin
//  if reference = nil then
//    exit;
//  if reference.reference = '' then
//    exit;
//  if StringStartsWith(reference.reference, '#') then
//    exit; // what to do in this case?
//  if not StringStartsWith(reference.reference, 'Patient/') then
//    exit; // what to do in this case?
//  sid := copy(reference.reference, 9, $FF);
//  if (pos('/', sid) > 0) then
//    sid := copy(sid, 1, pos('/', sid) - 1);
//  patientCompartment(key, 'Patient', sid);
//end;


procedure TFhirIndexManager4.patientCompartment(key : integer; type_, id : String);
begin
  FConnection.sql := 'Select i.ResourceTypeKey, ResourceKey from Ids as i, Types as t where i.ResourceTypeKey = t.ResourceTypeKey and ResourceName = :t and Id = :id';
  FConnection.Prepare;
  FConnection.BindString('t', type_);
  FConnection.BindString('id', id);
  FConnection.Execute;
  if FConnection.FetchNext then
    FCompartments.add(key, FConnection.ColIntegerByName['ResourceTypeKey'], FConnection.ColIntegerByName['ResourceKey'], TypeForKey(FConnection.ColIntegerByName['ResourceTypeKey']), id);
  FConnection.Terminate;
end;

procedure TFhirIndexManager4.patientCompartmentNot(key : integer; type_, id : String);
begin
  FCompartments.removeById(id);
end;

procedure TFhirIndexManager4.processCompartmentTags(key: integer; id: String; tags: TFHIRTagList);
var
  i : integer;
begin
  for i := 0 to tags.Count - 1 do
    if (tags[i].system = TAG_FHIR_SYSTEM) and (tags[i].code = TAG_COMPARTMENT_IN) then
      patientCompartment(key, 'Patient', tags[i].display);

end;

procedure TFhirIndexManager4.processUnCompartmentTags(key: integer; id: String; tags: TFHIRTagList);
var
  i : integer;
begin
  for i := 0 to tags.Count - 1 do
    if (tags[i].system = TAG_FHIR_SYSTEM) and (tags[i].code = TAG_COMPARTMENT_OUT) then
      patientCompartmentNot(key, 'Patient', tags[i].display);

end;

procedure TFhirIndexManager4.recordSpace(space: string; key: integer);
begin
  FConnection.SQL := 'insert into Spaces (SpaceKey, Space) values ('+inttostr(key)+', :s)';
  FConnection.prepare;
  FConnection.BindString('s', space);
  FConnection.execute;
  FConnection.terminate;
end;

function TFhirIndexManager4.index(aType: String; key, parent: integer; name: String): Integer;
var
  ndx : TFhirComposite;
begin
  ndx := FInfo.Composites.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown composite index '+name+' on type '+aType);
  if (ndx.Key = 0) then
    raise EFHIRException.create('unknown composite index '+ndx.Name);
  result := FEntries.add(FConnection, key, parent, ndx);
end;

procedure TFhirIndexManager4.index(aType: String; key, parent: integer; value: TFhirMoney; name, units: string);
var
  ndx : TFhirIndex;
  v1, v2 : String;
  ref : integer;
begin
  if value = nil then
    exit;
  if value.value = '' then
    exit;

  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index a simple type in an index that is a resource join: "'+name+'"');
  if not (ndx.SearchType in [sptToken, sptNumber, sptQuantity]) then
    raise EFHIRException.create('Unsuitable index "'+name+'" '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing quantity');

  GetBoundaries(value.value, QuantityComparatorNull, v1, v2);

  if (length(v1) > INDEX_ENTRY_LENGTH) then
      raise EFHIRException.create('quantity.value too long for indexing: "'+v1+ '" ('+inttostr(length(v1))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
  if (length(v2) > INDEX_ENTRY_LENGTH) then
      raise EFHIRException.create('quantity.value too long for indexing: "'+v2+ '" ('+inttostr(length(v2))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
  if not FSpaces.ResolveSpace('urn:iso:std:iso:4217#'+value.currency, ref) then
    recordSpace('urn:iso:std:iso:4217#'+value.currency, ref);
  FEntries.add(FConnection, key, parent, ndx, ref, v1, v2, 0, '', ndx.SearchType);
end;

procedure TFhirIndexManager4.index(aType: String; key, parent: integer; value: TFhirStructureDefinitionContext; name: String);
var
  c : TFHIRCoding;
begin
  if value = nil then
    exit;
  if value.type_Element = nil then
    exit;
  c := TFhirCoding.Create('http://hl7.org/fhir/extension-context-type#'+value.type_Element.value, value.expression);
  try
    index(aType, key, parent, c, name);
  finally
    c.Free;
  end;
end;

procedure TFhirIndexManager4.index(aType: String; key, parent: integer; value: TFhirDecimal; name: String);
var
  ndx : TFhirIndex;
  v1,v2 : String;
begin
  if (value = nil) or (value.value = '') then
    exit;
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType in [sptString, sptNumber, sptToken]) then
    raise EFHIRException.create('Unsuitable index '+name+' : '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing integer');
  GetBoundaries(value.value, QuantityComparatorNull, v1, v2);
  FEntries.add(FConnection, key, parent, ndx, 0, v1, v2, 0, '', ndx.SearchType);
end;

procedure TFhirIndexManager4.index(appInfo : TFslObject; context: TFhirResource; aType: String; key, parent: integer; value: TFhirReferenceList; name: String; specificType : String = '');
var
  i : integer;
begin
  if (value <> nil) then
    for i := 0 to value.Count - 1 do
      index(appInfo, context, atype, key, parent, value[i], name, specificType);
end;

procedure TFhirIndexManager4.checkTags(resource: TFhirResource; tags: TFHIRTagList);
var
  c : integer;
begin
  c := 0;
  if (resource.meta <> nil) then
    c := resource.meta.tagList.Count + resource.meta.securityList.Count + resource.meta.profileList.Count;
  if c <> tags.Count then
    raise EFHIRException.create('Tags out of sync');
end;

end.



