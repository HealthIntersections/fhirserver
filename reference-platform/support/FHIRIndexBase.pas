unit FHIRIndexBase;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  SysUtils,
  System.Generics.Collections,
  AdvObjects, AdvObjectLists, AdvGenerics,
  FHIRBase, FHIRTypes, FHIRConstants, FHIRResources;

type
  TFhirIndex = class (TAdvObject)
  private
    FResourceType : String;
    FKey: Integer;
    FName: String;
    FDescription : String;
    FSearchType: TFhirSearchParamTypeEnum;
    FTargetTypes : TArray<String>;
    FURI: String;
    FPath : String;
    FUsage : TFhirSearchXpathUsageEnum;
    FMapping : String;
    FExpression: TFHIRPathExpressionNode;
    procedure SetExpression(const Value: TFHIRPathExpressionNode);
  public
    destructor Destroy; override;
    function Link : TFhirIndex; Overload;
    function Clone : TFhirIndex; Overload;
    procedure Assign(source : TAdvObject); Override;

    property ResourceType : String read FResourceType write FResourceType;
    property Name : String read FName write FName;
    Property Description : String read FDescription write FDescription;
    Property Key : Integer read FKey write FKey;
    Property SearchType : TFhirSearchParamTypeEnum read FSearchType write FSearchType;
    Property TargetTypes : TArray<String> read FTargetTypes write FTargetTypes;
    Property URI : String read FURI write FURI;
    Property Path : String read FPath;
    Property Usage : TFhirSearchXpathUsageEnum read FUsage;
    Property Mapping : String read FMapping write FMapping;
    property expression : TFHIRPathExpressionNode read FExpression write SetExpression;

    function specifiedTarget : String;

    function summary : String;
  end;

  TFhirIndexList = class (TAdvObjectList)
  private
    function GetItemN(iIndex: integer): TFhirIndex;
  protected
    function ItemClass : TAdvObjectClass; override;
  public
    function Link : TFhirIndexList; Overload;

    function getByName(atype : String; name : String): TFhirIndex;
    function add(aResourceType : String; name, description : String; aType : TFhirSearchParamTypeEnum; aTargetTypes : Array of String; path : String; usage : TFhirSearchXpathUsageEnum): TFhirIndex; overload;
    function add(aResourceType : String; name, description : String; aType : TFhirSearchParamTypeEnum; aTargetTypes : Array of String; path : String; usage : TFhirSearchXpathUsageEnum; url : String): TFhirIndex; overload;
    function add(resourceType : String; sp : TFhirSearchParameter): TFhirIndex; overload;
    Property Item[iIndex : integer] : TFhirIndex read GetItemN; default;
    function listByType(aType : String) : TAdvList<TFhirIndex>;
  end;

  TFhirComposite = class (TAdvObject)
  private
    FResourceType : String;
    FKey: Integer;
    FName: String;
    FComponents : TDictionary<String, String>;
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function Link : TFhirComposite; Overload;
    function Clone : TFhirComposite; Overload;
    procedure Assign(source : TAdvObject); Override;

    property ResourceType : String read FResourceType write FResourceType;
    property Name : String read FName write FName;
    Property Key : Integer read FKey write FKey;
    Property Components : TDictionary<String, String> read FComponents;
  end;

  TFhirCompositeList = class (TAdvObjectList)
  private
    function GetItemN(iIndex: integer): TFhirComposite;
  protected
    function ItemClass : TAdvObjectClass; override;
  public
    function Link : TFhirCompositeList; Overload;

    function getByName(aType : String; name : String): TFhirComposite;
    procedure add(aResourceType : String; name : String; components : array of String); overload;
    Property Item[iIndex : integer] : TFhirComposite read GetItemN; default;
  end;

  // this defines the compartments. Contains the search parameters that define the compartment
  TFHIRCompartmentList = class (TAdvObject)
  private
    FPatientCompartment : TAdvMap<TAdvStringSet>;
    FPractitionerCompartment : TAdvMap<TAdvStringSet>;
    FEncounterCompartment : TAdvMap<TAdvStringSet>;
    FRelatedPersonCompartment : TAdvMap<TAdvStringSet>;
    FDeviceCompartment : TAdvMap<TAdvStringSet>;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    Function Link : TFHIRCompartmentList; overload;
    function existsInCompartment(comp: TFHIRResourceType; resource : String) : boolean;
    function getIndexNames(comp: TFHIRResourceType; resource : String) : TAdvStringSet;
    function hasCompartment(comp: TFHIRResourceType) : boolean;
    procedure register(comp: TFHIRResourceType; resource : String; indexes : array of String); overload;
//    procedure register(comp: TFHIRResourceType; resource : String; list : String); overload;
  end;


implementation

{ TFhirIndex }

procedure TFhirIndex.assign(source: TAdvObject);
begin
  inherited;
  FKey := TFhirIndex(source).FKey;
  FName := TFhirIndex(source).FName;
  FSearchType := TFhirIndex(source).FSearchType;
  FResourceType := TFhirIndex(source).FResourceType;
  TargetTypes := TFhirIndex(source).TargetTypes;
end;

function TFhirIndex.Clone: TFhirIndex;
begin
  result := TFhirIndex(Inherited Clone);
end;

destructor TFhirIndex.Destroy;
begin
  FExpression.Free;
  inherited;
end;

function TFhirIndex.Link: TFhirIndex;
begin
  result := TFhirIndex(Inherited Link);
end;

procedure TFhirIndex.SetExpression(const Value: TFHIRPathExpressionNode);
begin
  FExpression.Free;
  FExpression := Value;
end;

function TFhirIndex.specifiedTarget: String;
var
  a : String;
  s : String;
begin
  result := '';
  for a in ALL_RESOURCE_TYPE_NAMES do
    for s in FTargetTypes do
      if s = a then
        if result = '' then
          result := a
        else
          exit('');
end;

function TFhirIndex.summary: String;
begin
  result := name+' : '+CODES_TFhirSearchParamTypeEnum[SearchType];
end;

{ TFhirIndexList }

function TFhirIndexList.add(aResourceType : String; name, description : String; aType : TFhirSearchParamTypeEnum; aTargetTypes : Array of String; path : String; usage : TFhirSearchXpathUsageEnum) : TFHIRIndex;
begin
  result := add(aResourceType, name, description, aType, aTargetTypes, path, usage, 'http://hl7.org/fhir/SearchParameter/'+aResourceType+'-'+name.Replace('[', '').Replace(']', ''));
end;


function TFhirIndexList.add(aResourceType : String; name, description : String; aType : TFhirSearchParamTypeEnum; aTargetTypes : Array of String; path : String; usage : TFhirSearchXpathUsageEnum; url: String) : TFHIRIndex;
var
  ndx : TFhirIndex;
  i : integer;
begin
  ndx := TFhirIndex.Create;
  try
    ndx.ResourceType := aResourceType;
    ndx.name := name;
    ndx.SearchType := aType;
    SetLength(ndx.FTargetTypes, length(aTargetTypes));
    for i := 0 to length(ndx.TargetTypes)-1 do
      ndx.FTargetTypes[i] := aTargetTypes[i];
    ndx.URI := url;
    ndx.description := description;
    ndx.FPath := path;
    ndx.FUsage := usage;
    inherited add(ndx.Link);
    result := ndx;
  finally
    ndx.free;
  end;
end;

function TFhirIndexList.add(resourceType : String; sp: TFhirSearchParameter) : TFhirIndex;
var
  targets : TArray<String>;
  i : integer;
begin
  SetLength(targets, sp.targetList.Count);
  for i := 0 to sp.targetList.Count - 1 do
    targets[i] := sp.targetList[i].value;

  result := add(resourceType, sp.name, sp.description, sp.type_, targets, '', sp.xpathUsage);
end;

function TFhirIndexList.getByName(atype, name: String): TFhirIndex;
var
  i : integer;
begin
  i := 0;
  result := nil;
  while (result = nil) and (i < Count) do
  begin
    if SameText(item[i].name, name) and SameText(item[i].FResourceType, atype) then
      result := item[i];
    inc(i);
  end;
end;

function TFhirIndexList.GetItemN(iIndex: integer): TFhirIndex;
begin
  result := TFhirIndex(ObjectByIndex[iIndex]);
end;

function TFhirIndexList.ItemClass: TAdvObjectClass;
begin
  result := TFhirIndex;
end;

function TFhirIndexList.Link: TFhirIndexList;
begin
  result := TFhirIndexList(Inherited Link);
end;

function TFhirIndexList.listByType(aType: String): TAdvList<TFhirIndex>;
var
  i : integer;
begin
  result := TAdvList<TFhirIndex>.create;
  try
    for i := 0 to Count - 1 do
      if (Item[i].ResourceType = aType) then
        result.Add(Item[i].Link);
    result.link;
  finally
    result.Free;
  end;
end;

{ TFhirComposite }

procedure TFhirComposite.Assign(source: TAdvObject);
var
  s : String;
begin
  inherited;
  FResourceType := TFhirComposite(source).FResourceType;
  FKey := TFhirComposite(source).FKey;
  FName := TFhirComposite(source).FName;
  for s in TFhirComposite(source).FComponents.Keys do
    FComponents.Add(s, TFhirComposite(source).FComponents[s]);
end;

function TFhirComposite.Clone: TFhirComposite;
begin
  result := TFhirComposite(inherited Clone);
end;

constructor TFhirComposite.Create;
begin
  inherited;
  FComponents := TDictionary<String,String>.create;
end;

destructor TFhirComposite.Destroy;
begin
  FComponents.Free;
  inherited;
end;

function TFhirComposite.Link: TFhirComposite;
begin
  result := TFhirComposite(inherited Link);
end;

{ TFhirCompositeList }

procedure TFhirCompositeList.add(aResourceType: string; name: String; components: array of String);
var
  ndx : TFhirComposite;
  i : integer;
begin
  ndx := TFhirComposite.Create;
  try
    ndx.ResourceType := aResourceType;
    ndx.name := name;
    i := 0;
    while (i < length(components)) do
    begin
      ndx.Components.Add(components[i], components[i+1]);
      inc(i, 2);
    end;
    inherited add(ndx.Link);
  finally
    ndx.free;
  end;

end;

function TFhirCompositeList.getByName(aType: String; name: String): TFhirComposite;
var
  i : integer;
begin
  i := 0;
  result := nil;
  while (result = nil) and (i < Count) do
  begin
    if SameText(item[i].name, name) and (item[i].FResourceType = atype) then
      result := item[i];
    inc(i);
  end;
end;

function TFhirCompositeList.GetItemN(iIndex: integer): TFhirComposite;
begin
  result := TFhirComposite(ObjectByIndex[iIndex]
  );
end;

function TFhirCompositeList.ItemClass: TAdvObjectClass;
begin
  result := TFhirComposite;
end;

function TFhirCompositeList.Link: TFhirCompositeList;
begin
  result := TFhirCompositeList(inherited Link);
end;



{ TFHIRCompartmentList }

constructor TFHIRCompartmentList.Create;
begin
  inherited;
  FPatientCompartment := TAdvMap<TAdvStringSet>.create;
  FPractitionerCompartment := TAdvMap<TAdvStringSet>.create;
  FEncounterCompartment := TAdvMap<TAdvStringSet>.create;
  FRelatedPersonCompartment := TAdvMap<TAdvStringSet>.create;
  FDeviceCompartment := TAdvMap<TAdvStringSet>.create;
end;

destructor TFHIRCompartmentList.Destroy;
begin
  FPatientCompartment.free;
  FPractitionerCompartment.free;
  FEncounterCompartment.free;
  FRelatedPersonCompartment.free;
  FDeviceCompartment.free;
  inherited;
end;

function TFHIRCompartmentList.existsInCompartment(comp: TFHIRResourceType; resource : String) : boolean;
begin
  case comp of
    frtPatient : result := FPatientCompartment.containsKey(resource);
    frtPractitioner : result := FPractitionerCompartment.ContainsKey(resource);
    frtEncounter : result := FEncounterCompartment.containsKey(resource);
    frtRelatedPerson : result := FRelatedPersonCompartment.containsKey(resource);
    frtDevice : result := FDeviceCompartment.containsKey(resource);
  else
    result := false
  end;
end;

function TFHIRCompartmentList.getIndexNames(comp: TFHIRResourceType; resource : String) : TAdvStringSet;
begin
  case comp of
    frtPatient : result := FPatientCompartment[resource];
    frtPractitioner : result := FPractitionerCompartment[resource];
    frtEncounter : result := FEncounterCompartment[resource];
    frtRelatedPerson : result := FRelatedPersonCompartment[resource];
    frtDevice : result := FDeviceCompartment[resource];
  else
    result := nil
  end;
end;

function TFHIRCompartmentList.hasCompartment(comp: TFHIRResourceType): boolean;
begin
  case comp of
    frtPatient : result := true;
    frtPractitioner : result := true;
    frtEncounter : result := true;
    frtRelatedPerson : result := true;
    frtDevice : result := true;
  else
    result := false
  end;
end;

function TFHIRCompartmentList.Link: TFHIRCompartmentList;
begin
  result := TFHIRCompartmentList(inherited link);
end;

//procedure TFHIRCompartmentList.registerComp(comp: TFHIRResourceType; resource, list: String);
//begin
//  raise Exception.Create('not done yet');
//end;
//
procedure TFHIRCompartmentList.register(comp: TFHIRResourceType; resource : String; indexes : array of String);
begin
  case comp of
    frtPatient : FPatientCompartment.add(resource, TAdvStringSet.create(indexes));
    frtPractitioner : FPractitionerCompartment.Add(resource, TAdvStringSet.create(indexes));
    frtEncounter : FEncounterCompartment.add(resource, TAdvStringSet.create(indexes));
    frtRelatedPerson : FRelatedPersonCompartment.add(resource, TAdvStringSet.create(indexes));
    frtDevice : FDeviceCompartment.add(resource, TAdvStringSet.create(indexes));
  else
    raise Exception.Create('Unknown compartment');
  end;
end;


end.
