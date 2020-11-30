unit fhir_indexing;

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

{$I fhir.inc}

interface

uses
  SysUtils,
  Generics.Collections,
  fsl_base, fsl_collections, 
  fhir_objects, fhir_common, fhir_pathengine, fhir_factory;

type
  TFhirIndex = class (TFslObject)
  private
    FFactory : TFHIRFactory;
    FResourceType : String;
    FKey: Integer;
    FName: String;
    FDescription : String;
    FSearchType: TFHIRSearchParamType;
    FTargetTypes : TArray<String>;
    FURI: String;
    FPath : String;
    FUsage : TFhirSearchXpathUsage;
    FMapping : String;
    FExpression: TFHIRPathExpressionNodeV;
    procedure SetExpression(const Value: TFHIRPathExpressionNodeV);
    procedure SetKey(const Value: Integer);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(factory : TFHIRFactory);
    destructor Destroy; override;
    function Link : TFhirIndex; Overload;
    function Clone : TFhirIndex; Overload;
    procedure Assign(source : TFslObject); Override;

    property ResourceType : String read FResourceType write FResourceType;
    property Name : String read FName write FName;
    Property Description : String read FDescription write FDescription;
    Property Key : Integer read FKey write SetKey;
    Property SearchType : TFHIRSearchParamType read FSearchType write FSearchType;
    Property TargetTypes : TArray<String> read FTargetTypes write FTargetTypes;
    Property URI : String read FURI write FURI;
    Property Path : String read FPath;
    Property Usage : TFhirSearchXpathUsage read FUsage;
    Property Mapping : String read FMapping write FMapping;
    property expression : TFHIRPathExpressionNodeV read FExpression write SetExpression;

    function specifiedTarget : String;

    function summary : String;
  end;

  TFhirIndexList = class (TFslObjectList)
  private
    FFactory : TFHIRFactory;
    function GetItemN(iIndex: integer): TFhirIndex;
  protected
    function ItemClass : TFslObjectClass; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(factory : TFHIRFactory);
    destructor Destroy; override;
    function Link : TFhirIndexList; Overload;

    function getByName(atype : String; name : String): TFhirIndex;
    function add(aResourceType : String; name, description : String; aType : TFHIRSearchParamType; aTargetTypes : Array of String; path : String; usage : TFhirSearchXpathUsage): TFhirIndex; overload;
    function add(aResourceType : String; name, description : String; aType : TFHIRSearchParamType; aTargetTypes : Array of String; path : String; usage : TFhirSearchXpathUsage; url : String): TFhirIndex; overload;
    function add(resourceType : String; sp : TFhirSearchParameterW): TFhirIndex; overload;
    Property Item[iIndex : integer] : TFhirIndex read GetItemN; default;
    function listByType(aType : String) : TFslList<TFhirIndex>;
    property Factory : TFHIRFactory read FFactory;
  end;

  TFhirComposite = class (TFslObject)
  private
    FResourceType : String;
    FKey: Integer;
    FName: String;
    FComponents : TFslStringDictionary;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Link : TFhirComposite; Overload;
    function Clone : TFhirComposite; Overload;
    procedure Assign(source : TFslObject); Override;

    property ResourceType : String read FResourceType write FResourceType;
    property Name : String read FName write FName;
    Property Key : Integer read FKey write FKey;
    Property Components : TFslStringDictionary read FComponents;
  end;

  TFhirCompositeList = class (TFslObjectList)
  private
    function GetItemN(iIndex: integer): TFhirComposite;
  protected
    function ItemClass : TFslObjectClass; override;
  public
    function Link : TFhirCompositeList; Overload;

    function getByName(aType : String; name : String): TFhirComposite;
    procedure add(aResourceType : String; name : String; components : array of String); overload;
    Property Item[iIndex : integer] : TFhirComposite read GetItemN; default;
  end;

  // this defines the compartments. Contains the search parameters that define the compartment
  TFHIRCompartmentList = class (TFslObject)
  private
    FPatientCompartment : TFslMap<TFslStringSet>;
    FPractitionerCompartment : TFslMap<TFslStringSet>;
    FEncounterCompartment : TFslMap<TFslStringSet>;
    FRelatedPersonCompartment : TFslMap<TFslStringSet>;
    FDeviceCompartment : TFslMap<TFslStringSet>;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    Function Link : TFHIRCompartmentList; overload;
    function existsInCompartment(comp: string; resource : String) : boolean;
    function getIndexNames(comp: string; resource : String) : TFslStringSet;
    function hasCompartment(comp: string) : boolean;
    procedure register(comp: string; resource : String; indexes : array of String); overload;
//    procedure register(comp: TFHIRResourceType; resource : String; list : String); overload;
  end;

  TFHIRIndexBuilder = class abstract (TFslObject)
  public
    procedure registerIndexes(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList); virtual; abstract;
  end;

implementation

{ TFhirIndex }

procedure TFhirIndex.assign(source: TFslObject);
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

constructor TFhirIndex.Create(factory : TFHIRFactory);
begin
  inherited create;
  FFactory := factory;
end;

destructor TFhirIndex.Destroy;
begin
  FFactory.Free;
  FExpression.Free;
  inherited;
end;

function TFhirIndex.Link: TFhirIndex;
begin
  result := TFhirIndex(Inherited Link);
end;

procedure TFhirIndex.SetExpression(const Value: TFHIRPathExpressionNodeV);
begin
  FExpression.Free;
  FExpression := Value;
end;

procedure TFhirIndex.SetKey(const Value: Integer);
begin
  FKey := Value;
end;

function TFhirIndex.specifiedTarget: String;
var
  a : String;
  s : String;
begin
  result := '';
  for a in FFactory.ResourceNames do
    for s in FTargetTypes do
      if s = a then
        if result = '' then
          result := a
        else
          exit('');
end;

function TFhirIndex.summary: String;
begin
  result := name+' : '+CODES_TFHIRSearchParamType[SearchType];
end;

function TFhirIndex.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFactory.sizeInBytes);
  inc(result, (FResourceType.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FDescription.length * sizeof(char)) + 12);
  inc(result, (FURI.length * sizeof(char)) + 12);
  inc(result, (FPath.length * sizeof(char)) + 12);
  inc(result, (FMapping.length * sizeof(char)) + 12);
  inc(result, FExpression.sizeInBytes);
end;

{ TFhirIndexList }

function TFhirIndexList.add(aResourceType : String; name, description : String; aType : TFHIRSearchParamType; aTargetTypes : Array of String; path : String; usage : TFhirSearchXpathUsage) : TFHIRIndex;
begin
  result := add(aResourceType, name, description, aType, aTargetTypes, path, usage, 'http://hl7.org/fhir/SearchParameter/'+aResourceType+'-'+name.Replace('[', '').Replace(']', ''));
end;


function TFhirIndexList.add(aResourceType : String; name, description : String; aType : TFHIRSearchParamType; aTargetTypes : Array of String; path : String; usage : TFhirSearchXpathUsage; url: String) : TFHIRIndex;
var
  ndx : TFhirIndex;
  i : integer;
begin
  ndx := TFhirIndex.Create(FFactory.link);
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

function TFhirIndexList.add(resourceType : String; sp: TFhirSearchParameterW) : TFhirIndex;
begin
  result := add(resourceType, sp.name, sp.description, sp.type_, sp.targets, '', sp.xpathUsage);
end;

constructor TFhirIndexList.Create(factory: TFHIRFactory);
begin
  inherited Create;
  FFactory := factory;
end;

destructor TFhirIndexList.Destroy;
begin
  FFactory.Free;
  inherited;
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

function TFhirIndexList.ItemClass: TFslObjectClass;
begin
  result := TFhirIndex;
end;

function TFhirIndexList.Link: TFhirIndexList;
begin
  result := TFhirIndexList(Inherited Link);
end;

function TFhirIndexList.listByType(aType: String): TFslList<TFhirIndex>;
var
  i : integer;
begin
  result := TFslList<TFhirIndex>.create;
  try
    for i := 0 to Count - 1 do
      if (Item[i].ResourceType = aType) then
        result.Add(Item[i].Link);
    result.link;
  finally
    result.Free;
  end;
end;

function TFhirIndexList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFactory.sizeInBytes);
end;

{ TFhirComposite }

procedure TFhirComposite.Assign(source: TFslObject);
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
  FComponents := TFslStringDictionary.create;
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

function TFhirComposite.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FResourceType.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FComponents.sizeInBytes);
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

function TFhirCompositeList.ItemClass: TFslObjectClass;
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
  FPatientCompartment := TFslMap<TFslStringSet>.create('Patient Compartment');
  FPractitionerCompartment := TFslMap<TFslStringSet>.create('Practitioner Compartment');
  FEncounterCompartment := TFslMap<TFslStringSet>.create('Encounter Compartment');
  FRelatedPersonCompartment := TFslMap<TFslStringSet>.create('RelatedPerson Compartment');
  FDeviceCompartment := TFslMap<TFslStringSet>.create('Device Compartment');
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

function TFHIRCompartmentList.existsInCompartment(comp: string; resource : String) : boolean;
begin
  if comp = 'Patient' then result := FPatientCompartment.containsKey(resource)
  else if comp = 'Practitioner' then result := FPractitionerCompartment.ContainsKey(resource)
  else if comp = 'Encounter' then result := FEncounterCompartment.containsKey(resource)
  else if comp = 'RelatedPerson' then result := FRelatedPersonCompartment.containsKey(resource)
  else if comp = 'Device' then result := FDeviceCompartment.containsKey(resource)
  else
    result := false
end;

function TFHIRCompartmentList.getIndexNames(comp: String; resource : String) : TFslStringSet;
begin
  if comp = 'Patient' then result := FPatientCompartment[resource]
  else if comp = 'Practitioner' then result := FPractitionerCompartment[resource]
  else if comp = 'Encounter' then result := FEncounterCompartment[resource]
  else if comp = 'RelatedPerson' then result := FRelatedPersonCompartment[resource]
  else if comp = 'Device' then result := FDeviceCompartment[resource]
  else
    result := nil
end;

function TFHIRCompartmentList.hasCompartment(comp: String): boolean;
begin
  if comp = 'Patient' then result := true
  else if comp = 'Practitioner' then result := true
  else if comp = 'Encounter' then result := true
  else if comp = 'RelatedPerson' then result := true
  else if comp = 'Device' then result := true
  else
    result := false
end;

function TFHIRCompartmentList.Link: TFHIRCompartmentList;
begin
  result := TFHIRCompartmentList(inherited link);
end;

//procedure TFHIRCompartmentList.registerComp(comp: TFHIRResourceType; resource, list: String);
//begin
//  raise EFHIRTodo.create();
//end;
//
procedure TFHIRCompartmentList.register(comp: String; resource : String; indexes : array of String);
begin
  if comp = 'Patient' then
    FPatientCompartment.add(resource, TFslStringSet.create(indexes))
  else if comp = 'Practitioner' then
    FPractitionerCompartment.Add(resource, TFslStringSet.create(indexes))
  else if comp = 'Encounter' then
    FEncounterCompartment.add(resource, TFslStringSet.create(indexes))
  else if comp = 'RelatedPerson' then
    FRelatedPersonCompartment.add(resource, TFslStringSet.create(indexes))
  else if comp = 'Device' then
    FDeviceCompartment.add(resource, TFslStringSet.create(indexes))
  else
    raise EFHIRException.create('Unknown compartment');
end;


function TFHIRCompartmentList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPatientCompartment.sizeInBytes);
  inc(result, FPractitionerCompartment.sizeInBytes);
  inc(result, FEncounterCompartment.sizeInBytes);
  inc(result, FRelatedPersonCompartment.sizeInBytes);
  inc(result, FDeviceCompartment.sizeInBytes);
end;

end.
