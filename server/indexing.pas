unit indexing;

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
  SysUtils, Classes,
  fsl_base, fsl_threads, fsl_utilities,
  fhir_objects, fhir_common, fhir_factory,  fhir_pathengine,
  fdb_manager,
  fhir_indexing,
  ftx_ucum_services, tx_server,
  server_constants, tags, session, utilities;

Const
  INDEX_ENTRY_LENGTH = 210;
  NARRATIVE_INDEX_NAME = '_text';
  LIST_ITEM_INDEX_NAME = 'item';
  GROUP_MEMBER_INDEX_NAME = 'member';

type
  TKeyType = (ktResource, ktEntries, ktCompartment);

  TFHIRBaseServerFactory = class abstract (TFslObject)
  public
    function makeIndexes : TFHIRIndexBuilder; virtual; abstract;
  end;

  TFHIRGetNextKey = function (connection : TFDBConnection; keytype : TKeyType; aType : String; var id : string) : Integer of Object;

  TFhirIndexEntry = class (TFslObject)
  private
    FKey: integer;
    FEntryKey : integer;
    FIndexKey : integer;
    FValue1: String;
    FValue2: String;
    FRefType: integer;
    FTarget: integer;
    FConcept : integer;
    FType: TFhirSearchParamType;
    FParent: Integer;
    FFlag: boolean;
    FName : String;
    FTargetType : String;
  public
    function Link : TFhirIndexEntry; overload;

    Property EntryKey : Integer Read FEntryKey write FEntryKey;
    Property IndexKey : Integer Read FIndexKey write FIndexKey;
    property Key : integer read FKey write FKey;
    Property Parent : Integer read FParent write FParent;
    property Value1 : String read FValue1 write FValue1;
    property Value2 : String read FValue2 write FValue2;
    property RefType : integer read FRefType write FRefType;
    Property target : integer read FTarget write FTarget;
    Property concept : integer read FConcept write FConcept;
    Property type_ : TFhirSearchParamType read FType write FType;
    Property flag : boolean read FFlag write FFlag;
    Property TargetType : String read FTargetType write FTargetType;
  end;

  TFhirIndexEntryList = class (TFslList<TFhirIndexEntry>)
  private
    FKeyEvent : TFHIRGetNextKey;
//    procedure filter(indexes : TFhirIndexList; name : String; list : TFslList<TFhirIndexEntry>);
  public
    function add(connection : TFDBConnection; key, parent : integer; index : TFhirIndex; ref : integer; value1, value2 : String; target : integer; ttype : String; type_ : TFhirSearchParamType; flag : boolean = false; concept : integer = 0) : integer; overload;
    function add(connection : TFDBConnection; key, parent : integer; index : TFhirComposite) : integer; overload;
    property KeyEvent : TFHIRGetNextKey read FKeyEvent write FKeyEvent;
  end;

  TFhirCompartmentEntry = class (TFslObject)
  private
    FCKey: integer;
    FKey: integer;
    FId: string;
    FTypeKey: integer;
    FEnum: String;
  public
    function link : TFhirCompartmentEntry; overload;
    property Key : integer read FKey write FKey; // id of resource that is in a compartment
    property Enum : String read FEnum write FEnum;
    property TypeKey : integer read FTypeKey write FTypeKey; // resource type key for the compartment type
    property Id : string read FId write FId; // field two of composite id for compartment - compartment id
    property CKey : integer read FCKey write FCKey; // key for the resource that creates this compartment
  end;

  TFhirCompartmentEntryList = class (TFslList<TFhirCompartmentEntry>)
  public
    procedure add(key, tkey, ckey : integer; enum : String; id : string);
    procedure removeById(id : String);
  end;

  TFhirIndexSpaces = class (TFslObject)
  private
    FLock : TFslLock;
    FSpaces : TStringList;
    FLast : integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure RecordSpace(space : String; key:integer);
    function ResolveSpace(space : String; var key :integer) : boolean;
  end;

  TFHIRIndexInformation = class (TFslObject)
  private
    FIndexes : TFhirIndexList;
    FComposites : TFhirCompositeList;
    FCompartments : TFHIRCompartmentList;
    FNarrativeIndex : Integer;
    FListItemIndex : Integer;
    FGroupMemberIndex: integer;
    FServerFactory : TFHIRBaseServerFactory;
    procedure buildIndexes;
  public
    constructor Create(factory : TFHIRFactory; serverFactory : TFHIRBaseServerFactory);
    destructor Destroy; override;
    function Link : TFHIRIndexInformation; overload;
    function factory : TFHIRFactory;
    procedure ReconcileIndexes(connection : TFDBConnection);

    Function GetTargetsByName(types : TArray<String>; name : String) : TArray<String>;
    Function GetKeyByName(name : String) : integer;
    Function GetTypeByName(types : TArray<String>; name : String) : TFhirSearchParamType;
    Function GetComposite(types : TArray<String>; name : String; var otypes : TArray<String>) : TFhirComposite;

    property Indexes : TFhirIndexList read FIndexes;
    property Composites : TFhirCompositeList read FComposites;
    Property NarrativeIndex : integer read FNarrativeIndex;
    Property ListItemIndex : integer read FListItemIndex;
    Property GroupMemberIndex : integer read FGroupMemberIndex;
    property Compartments : TFHIRCompartmentList read FCompartments;
  end;

  TFhirIndexManager = class;
  TFHIRResolveReferenceVEvent = function(indexer : TFhirIndexManager; appInfo : TFslObject; sUrl : String) : TFHIRResourceV of Object;
  TFhirIndexManager = class (TFslObject)
  private
    procedure SetConnection(const Value: TFDBConnection);
    procedure SetContext(const Value: TFHIRWorkerContextWithFactory);
    procedure SetInfo(const Value: TFHIRIndexInformation);
    procedure SetResConfig(const Value: TFslMap<TFHIRResourceConfig>);
    procedure SetSpaces(const Value: TFhirIndexSpaces);
    procedure SetUcum(const Value: TUcumServices);
    procedure SetEngine(const Value: TFHIRPathEngineV);
  protected
    FSpaces : TFhirIndexSpaces;
    FKeyEvent : TFHIRGetNextKey;
    FBases : TStringList;
    FContext : TFHIRWorkerContextWithFactory;
    FTerminologyServer : TTerminologyServer;
    FResConfig: TFslMap<TFHIRResourceConfig>;
    FConnection : TFDBConnection;
    FCompartments : TFhirCompartmentEntryList;
    FInfo : TFHIRIndexInformation;
    FEntries : TFhirIndexEntryList;
    FUcum : TUcumServices;
    FEngine : TFHIRPathEngineV;

    FOnResolveReference: TFHIRResolveReferenceVEvent;
    procedure SetTerminologyServer(const Value: TTerminologyServer);
  public
    constructor Create; override;
    destructor Destroy; override;

    property TerminologyServer : TTerminologyServer read FTerminologyServer write SetTerminologyServer;
    property Bases : TStringList read FBases write FBases;
    property KeyEvent : TFHIRGetNextKey read FKeyEvent write FKeyEvent;
    property Definitions : TFHIRIndexInformation read FInfo write SetInfo;
    property Spaces : TFhirIndexSpaces read FSpaces write SetSpaces;
    property Connection : TFDBConnection read FConnection write SetConnection;
    property Context : TFHIRWorkerContextWithFactory read FContext write SetContext;
    property ResConfig : TFslMap<TFHIRResourceConfig> read FResConfig write SetResConfig;
    property Ucum : TUcumServices read FUcum write SetUcum;
    property Engine : TFHIRPathEngineV read FEngine write SetEngine;

    function execute(key : integer; id: String; resource : TFhirResourceV; tags : TFHIRTagList; appInfo : TFslObject) : TFslList<TFHIRCompartmentId>; virtual; abstract;
    property OnResolveReference : TFHIRResolveReferenceVEvent read FOnResolveReference write FOnResolveReference;
    function doResolve(source : TFHIRPathEngineV; appInfo : TFslObject; url : String) : TFHIRObject;
  end;

function findPrefix(var value : String; subst : String) : boolean;

implementation

function findPrefix(var value : String; subst : String) : boolean;
begin
  result := value.StartsWith(subst);
  if result then
    value := value.Substring(subst.Length);
end;

{ TFhirIndexEntry }

function TFhirIndexEntry.Link: TFhirIndexEntry;
begin
  result := TFhirIndexEntry(inherited Link);
end;

{ TFhirCompartmentEntry }

function TFhirCompartmentEntry.link: TFhirCompartmentEntry;
begin
  result := TFhirCompartmentEntry(inherited Link);
end;

{ TFhirIndexEntryList }

function TFhirIndexEntryList.add(connection : TFDBConnection; key, parent : integer; index: TFhirIndex; ref: integer; value1, value2: String; target : Integer; ttype : String; type_ : TFhirSearchParamType; flag : boolean = false; concept : integer = 0) : integer;
var
  entry : TFhirIndexEntry;
  dummy : string;
  I: Integer;
begin
  result := -1;

  if (Index.Key = 0) then
    raise EFHIRException.create('unknown index '+index.Name);

  case type_ of
    sptNumber, sptQuantity :
      begin
        Assert(length(value1) > INDEX_DIGITS + INDEX_DECIMALS); // check they've been normalised
        Assert(length(value2) > INDEX_DIGITS + INDEX_DECIMALS);
      end;
    sptString :
      begin
        value1 := RemoveAccents(value1).ToLower;
        value2 := RemoveAccents(value2).ToLower;
      end;
    sptDate : ; // nothing

    sptUri : ; // nothing

    sptToken :
      begin
      value2 := removeAccents(value2).ToLower;
      end;
    sptReference : ; // nothing
  else
    // null, Composite
    raise EFHIRException.create('Unhandled type generating index');
  end;

  for i := 0 to count - 1 do
  begin
    entry := Items[i];
    if (entry.Value1 = lowercase(value1)) and (entry.Value2 = lowercase(value2)) and (entry.FIndexKey = index.key) and (entry.target = target) and (entry.type_ = type_) and (entry.flag = flag) and (entry.RefType = ref) then
      exit;
  end;


  entry := TFhirIndexEntry.create;
  try
    entry.FName := index.Name;
    entry.EntryKey := KeyEvent(connection, ktEntries, '', dummy);
    result := entry.EntryKey;
    entry.IndexKey := index.Key;
    entry.key := key;
    entry.parent := parent;
    entry.Value1 := lowercase(value1);
    entry.Value2 := lowercase(value2);
    entry.RefType := ref;
    entry.type_ := type_;
    entry.target := target;
    entry.concept := concept;
    entry.flag := flag;
    entry.targetType := ttype;
    Inherited Add(entry.Link);
  finally
    entry.free;
  end;
end;


function TFhirIndexEntryList.add(connection : TFDBConnection; key, parent: integer; index: TFhirComposite): integer;
var
  entry : TFhirIndexEntry;
  dummy : string;
begin
  if (Index.Key = 0) then
    raise EFHIRException.create('unknown index '+index.Name);

  entry := TFhirIndexEntry.create;
  try
    entry.EntryKey := KeyEvent(connection, ktEntries, '', dummy);
    result := entry.EntryKey;
    entry.IndexKey := index.Key;
    entry.key := key;
    entry.parent := parent;
    Inherited Add(entry.Link);
  finally
    entry.free;
  end;
end;

//procedure TFhirIndexEntryList.filter(indexes : TFhirIndexList;  name: String; list: TFslList<TFhirIndexEntry>);
//var
//  i : integer;
//begin
//  for i := 0 to Count - 1 do
//    if Items[i].FName = name then
//      list.Add(Items[i].Link as TFhirIndexEntry);
//end;
//
{ TFhirIndexSpaces }

constructor TFhirIndexSpaces.Create();
begin
  inherited create;
  FSpaces := TStringList.Create;
  FSpaces.Sorted := true;
  FLock := TFslLock.Create('Spaces');
end;


destructor TFhirIndexSpaces.destroy;
begin
  FSpaces.free;
  FLock.Free;
  inherited;
end;

procedure TFhirIndexSpaces.RecordSpace(space: String; key: integer);
begin
  if space.trim <> space then
    raise EFHIRException.create('Illegal System Value "'+space+'" - cannot have leading or trailing whitespace');
  FLock.Lock;
  try
    FSpaces.AddObject(space, TObject(key));
    if key > FLast then
      FLast := Key;
  finally
    FLock.Unlock;
  end;

end;

function TFhirIndexSpaces.ResolveSpace(space : String; var key :integer) : boolean;
var
  i : integer;
begin
  if space.trim <> space then
    raise EFHIRException.create('Illegal System Value "'+space+'" - cannot have leading or trailing whitespace');
  FLock.Lock;
  try
    result := FSpaces.Find(space, i);
    if result then
      key := integer(FSpaces.objects[i])
    else
    begin
      inc(FLast);
      key := FLast;
      FSpaces.AddObject(space, TObject(key));
    end;
  finally
    FLock.Unlock;
  end;
end;

{ TFhirCompartmentEntryList }

procedure TFhirCompartmentEntryList.add(key, tkey, ckey: integer; enum : String; id: string);
var
  item : TFhirCompartmentEntry;
begin
  item := TFhirCompartmentEntry.create;
  try
    item.key := key;
    item.typekey := tkey;
    item.Enum := enum;
    item.ckey := ckey;
    item.id := id;
    inherited add(item.Link);
  finally
    item.free;
  end;
end;

procedure TFhirCompartmentEntryList.removeById(id: String);
var
  i : integer;
begin
  for i := count - 1 downto 0 do
    if items[i].Id = id then
      Delete(i);
end;

{ TFHIRIndexInformation }

constructor TFHIRIndexInformation.Create(factory : TFHIRFactory; serverFactory : TFHIRBaseServerFactory);
begin
  inherited Create;
  FServerFactory := serverFactory;
  FIndexes := TFhirIndexList.create(factory);
  FComposites := TFhirCompositeList.create;
  FCompartments := TFHIRCompartmentList.Create;
  buildIndexes;
end;

destructor TFHIRIndexInformation.Destroy;
begin
  FServerFactory.Free;
  FIndexes.Free;
  FComposites.Free;
  FCompartments.Free;
  inherited;
end;

procedure TFHIRIndexInformation.buildIndexes;
var
  builder : TFHIRIndexBuilder;
begin
  builder := FServerFactory.makeIndexes;
  try
     builder.registerIndexes(FIndexes, FCompartments);
  finally
    builder.Free;
  end;

  // manual additions:
  composites.add('DocumentReference', 'relatesTo', ['code', 'relation', 'target', 'relatesTo']);
  composites.add('Group', 'characteristic', ['value', 'value', 'code', 'characteristic']);
  composites.add('Observation', 'related', ['target', 'related-target', 'type', 'related-type']);
  composites.add('DiagnosticOrder', 'event', ['status', 'event-status', 'date', 'event-date']);
  composites.add('DiagnosticOrder', 'item', ['status', 'item-status', 'code', 'item-code', 'site', 'bodysite', 'event', 'item-event']);
  composites.add('DiagnosticOrder', 'item-event', ['status', 'item-past-status', 'date', 'item-date', 'actor', 'actor']);
  composites.add('Patient', 'name', ['given', 'given', 'family', 'family']);

  // DAF
  indexes.add('Condition', 'identifier', 'identifier', sptToken, [], '', sxpNull);
  indexes.add('Patient', 'mothersMaidenName', 'Search based on Patient mother''s Maiden Name', sptString, [], '', sxpNull, 'http://hl7.org/fhir/SearchParameter/patient-extensions-Patient-mothersMaidenName');
  indexes.add('Patient', 'birthOrderBoolean', 'Search based on Patient''s birth order (boolean or integer)', sptString, [], '', sxpNull, 'http://hl7.org/fhir/SearchParameter/patient-extensions-Patient-mothersMaidenName');
  indexes.add('Patient', 'age', 'Search based on Patient''s age', sptNumber, [], '', sxpNull, 'http://hl7.org/fhir/SearchParameter/patient-extensions-Patient-age');
  indexes.add('FamilyMemberHistory', 'familymemberhistorycondition', 'Search for a history of a particular condition within a patient''s family', sptToken, [], '', sxpNull);
  if factory.version = fhirVersionRelease2 then
  begin
    indexes.add('List', 'identifier', 'identifier', sptToken, [], '', sxpNull);

    // custom
    indexes.add('ClaimResponse', 'request', 'request claim link', sptReference, ['Claim'], '', sxpNull);
  end;
end;

function TFHIRIndexInformation.factory: TFHIRFactory;
begin
  result := FIndexes.factory;
end;

function TFHIRIndexInformation.Link: TFHIRIndexInformation;
begin
  result := TFHIRIndexInformation(inherited Link);
end;

procedure TFHIRIndexInformation.ReconcileIndexes(connection: TFDBConnection);
var
  i : integer;
begin
  connection.SQL := 'select * from Indexes';
  connection.prepare;
  connection.execute;
  while connection.FetchNext do
  begin
    for i := 0 to FIndexes.Count - 1 Do
      if SameText(FIndexes[i].Name, connection.ColStringByName['Name']) then
        FIndexes[i].key := connection.ColIntegerByName['IndexKey'];

    for i := 0 to FComposites.Count - 1 Do
      if SameText(FComposites[i].Name, connection.ColStringByName['Name']) then
        FComposites[i].key := connection.ColIntegerByName['IndexKey'];

    if connection.ColStringByName['Name'] = NARRATIVE_INDEX_NAME then
      FNarrativeIndex := connection.ColIntegerByName['IndexKey'];
    if connection.ColStringByName['Name'] = LIST_ITEM_INDEX_NAME then
      FListItemIndex := connection.ColIntegerByName['IndexKey'];
    if connection.ColStringByName['Name'] = GROUP_MEMBER_INDEX_NAME then
      FGroupMemberIndex := connection.ColIntegerByName['IndexKey'];

  end;
  connection.terminate;
end;

function TFhirIndexInformation.GetTargetsByName(types: TArray<String>; name: String): TArray<String>;
var
  i : integer;
  s : String;
  res : TStringList;
  ok : boolean;
begin
  res := TStringList.Create;
  try
    res.Duplicates := dupIgnore;
  for i := 0 to FIndexes.Count - 1 Do
    if SameText(FIndexes[i].Name, name) then
    begin
      ok := false;
      for s in types do
        ok := ok or (s = FIndexes[i].ResourceType);
      if ok then
          for s in FIndexes[i].TargetTypes do
            res.Add(s);
      end;
    result := res.ToStringArray;
  finally
    res.Free;
    end;
end;

function TFhirIndexInformation.GetKeyByName(name: String): integer;
var
  i : integer;
begin
  result := 0;
  for i := 0 to FIndexes.Count - 1 Do
    if FIndexes[i].Name = name then
    begin
      result := FIndexes[i].Key;
      exit;
    end;
end;

function TFhirIndexInformation.GetTypeByName(types: TArray<String>; name: String): TFHIRSearchParamType;
var
  i : integer;
  s : String;
  ok : boolean;
begin
  result := sptNull;
  for i := 0 to FIndexes.Count - 1 Do
    if SameText(FIndexes[i].Name, name) then
    begin
      ok := length(types) = 0;
      for s in types do
        ok := ok or (s = FIndexes[i].ResourceType);
      if ok then
        if (result <> sptNull) and (result <> FIndexes[i].SearchType) And ((FIndexes[i].SearchType in [sptDate, sptToken]) or (result in [sptDate, sptToken])) then
          raise EFHIRException.create('Chained Parameters cross resource joins that create disparate index handling requirements')
        else
          result := FIndexes[i].SearchType;
    end;
end;

function TFhirIndexInformation.GetComposite(types: TArray<String>; name: String; var otypes: TArray<String>): TFhirComposite;
var
  i : integer;
  s : String;
  ok : boolean;
begin
  oTypes := types;

  i := 0;
  result := nil;
  while (i < FComposites.Count) do
  begin
    if SameText(FComposites.item[i].name, name) then
    begin
      ok := length(types) = 0;
      for s in types do
        ok := ok or (s = FComposites.item[i].ResourceType);
      if ok then
        if result = nil then
        begin
          result := FComposites.item[i];
          oTypes := TArray<String>.create(FComposites.item[i].ResourceType);
        end
        else
          raise EFHIRException.create('Ambiguous composite reference "'+name+'"');
    end;
    inc(i);
  end;
end;


{ TFhirIndexManager }

constructor TFhirIndexManager.Create();
begin
  inherited Create;
  FCompartments := TFhirCompartmentEntryList.create;
  FEntries := TFhirIndexEntryList.Create;
end;

destructor TFhirIndexManager.Destroy;
begin
  FUcum.Free;
  FEngine.Free;
  FContext.Free;
  FTerminologyServer.free;
  FCompartments.Free;
  FSpaces.Free;
  FEntries.Free;
  FInfo.Free;
  FConnection.Free;
  FResConfig.Free;
  inherited;
end;

function TFhirIndexManager.doResolve(source: TFHIRPathEngineV; appInfo: TFslObject; url: String): TFHIRObject;
begin
  // ok, we'll ask the host engine...
  if not assigned(FOnResolveReference) then
    raise Exception.Create('No resolve reference service provided');
  result := FOnResolveReference(self, appInfo, url);
end;

procedure TFhirIndexManager.SetConnection(const Value: TFDBConnection);
begin
  FConnection.Free;
  FConnection := Value;
end;

procedure TFhirIndexManager.SetContext(const Value: TFHIRWorkerContextWithFactory);
begin
  FContext.Free;
  FContext := Value;
end;

procedure TFhirIndexManager.SetEngine(const Value: TFHIRPathEngineV);
begin
  FEngine := Value;
end;

procedure TFhirIndexManager.SetInfo(const Value: TFHIRIndexInformation);
begin
  FInfo.Free;
  FInfo := Value;
end;

procedure TFhirIndexManager.SetResConfig(const Value: TFslMap<TFHIRResourceConfig>);
begin
  FResConfig.Free;
  FResConfig := Value;
end;

procedure TFhirIndexManager.SetSpaces(const Value: TFhirIndexSpaces);
begin
  FSpaces.Free;
  FSpaces := Value;
end;

procedure TFhirIndexManager.SetTerminologyServer(const Value: TTerminologyServer);
begin
  FTerminologyServer.Free;
  FTerminologyServer := Value;
end;

procedure TFhirIndexManager.SetUcum(const Value: TUcumServices);
begin
  FUcum.Free;
  FUcum := Value;
end;

end.
