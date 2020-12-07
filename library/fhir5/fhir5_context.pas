unit fhir5_context;

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
  fsl_base, fsl_utilities, fsl_threads,
  fsl_http,
  fsl_npm, fsl_npm_cache,
  fhir_objects, fhir_factory, fhir_common, 
  fhir5_types, fhir5_resources, fhir5_resources_base;

type
  TFHIRCustomResourceInformation = class (TFslObject)
  private
    FName: String;
    FSearchParameters: TFslList<TFHIRSearchParameter>;
    FDefinition: TFHIRStructureDefinition;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(definition : TFHIRStructureDefinition);
    destructor Destroy; override;
    function Link : TFHIRCustomResourceInformation; overload;
    property Name : String read FName;
    property Definition : TFHIRStructureDefinition read FDefinition;
    property SearchParameters : TFslList<TFHIRSearchParameter> read FSearchParameters;
  end;

 TFHIRMetadataResourceManager<T : TFHIRMetadataResource> = class (TFslObject)
  private
    FMap : TFslMap<T>;
    FList : TFslList<T>;
    procedure updateList(url, version: String);
    {$IFDEF FPC}
    function sort(sender : TObject; const L, R: T): Integer;
    {$ENDIF}
  protected
    function sizeInBytesV : cardinal; override;
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function link : TFHIRMetadataResourceManager<T>; overload;
    function clone : TFHIRMetadataResourceManager<T>; overload;
    procedure Assign(oSource : TFslObject); override;

    procedure see(r: T);
    procedure drop(id : String);
    function get(url: String): T; overload;
    function has(url: String): boolean; overload;
    function get(url, version: String): T; overload;
    function has(url, version: String): boolean; overload;
    function count: integer;
    procedure clear;
    procedure listAll(list: TFslList<T>);
    procedure listAllM(list: TFslList<TFHIRMetadataResource>);
  end;


  TResourceMemoryCache = class;

  TFHIRWorkerContext = class abstract (TFHIRWorkerContextWithFactory)
  private
    FOverrideVersionNs: String;  protected
    function GetVersion: TFHIRVersion; override;
  public
    function link : TFHIRWorkerContext; overload;

    procedure listStructures(list : TFslList<TFHIRStructureDefinition>); overload; virtual; abstract;
    function fetchResource(t : TFhirResourceType; url : String) : TFhirResource; overload; virtual; abstract;
    function fetchCodeSystem(url : String ) : TFhirCodeSystem;
    function fetchValueSet(url : String ) : TFhirValueSet;
    function fetchConceptMap(url : String ) : TFhirConceptMap;
    function fetchStructureDefinition(url : String ) : TFhirStructureDefinition;
    function fetchStructureMap(url : String ) : TFhirStructureMap;
    function expand(vs : TFhirValueSet; options : TExpansionOperationOptionSet = []) : TFHIRValueSet; overload; virtual; abstract;
    function validateCode(systemUri, version, code : String; vs : TFhirValueSet) : TValidationResult; overload; virtual; abstract;
    function validateCode(systemUri, version, code : String) : TValidationResult; overload; virtual; abstract;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; overload; virtual; abstract;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; overload; virtual; abstract;
    function getChildMap(profile : TFHIRStructureDefinition; element : TFhirElementDefinition) : TFHIRElementDefinitionList; virtual;  abstract;
    function getSliceList(profile : TFHIRStructureDefinition; element : TFhirElementDefinition) : TFHIRElementDefinitionList; virtual;
    function getStructure(url : String) : TFHIRStructureDefinition; overload; virtual; abstract;
    function getStructure(ns, name : String) : TFHIRStructureDefinition; overload; virtual; abstract;
    function getCustomResource(name : String) : TFHIRCustomResourceInformation; virtual; abstract;
    function hasCustomResource(name : String) : boolean; virtual; abstract;

    // override version independent variants:
    function fetchResource(rType : String; url : String) : TFhirResourceV; overload; override;
    function expand(vs : TFhirValueSetW; options : TExpansionOperationOptionSet = []) : TFHIRValueSetW; overload; override;
    function validateCode(systemUri, version, code : String; vs : TFhirValueSetW) : TValidationResult; overload; override;
    procedure listStructures(list : TFslList<TFhirStructureDefinitionW>); overload; override;

    procedure loadFromCache(cache : TResourceMemoryCache); overload;
    property OverrideVersionNs : String read FOverrideVersionNs write FOverrideVersionNs;
  end;
  TFHIRWorkerContext5 = TFHIRWorkerContext;

  TResourceMemoryCache = class (TFslObject)
  private
    Flist : TFslList<TFHIRResource>;
    FLoadInfo : TPackageLoadingInformation;
    FResourceTypes: TArray<String>;
    FPackages: TArray<String>;
    FOnLog: TWorkProgressEvent;
    FLoadPackage : String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TResourceMemoryCache; overload;

    property List : TFslList<TFHIRResource> read FList;
    procedure load(rType, id : String; stream : TStream);
    property Packages : TArray<String> read FPackages write FPackages;
    property ResourceTypes : TArray<String> read FResourceTypes write FResourceTypes;
    property OnLog : TWorkProgressEvent read FOnLog write FOnLog;

    procedure checkLoaded;
  end;

implementation

uses
  fhir5_utilities, fhir5_parserBase, fhir5_json, fhir_utilities;

{ TFHIRWorkerContext }

function TFHIRWorkerContext.GetVersion: TFHIRVersion;
begin
  result := fhirVersionRelease5;
end;

function TFHIRWorkerContext.link: TFHIRWorkerContext;
begin
  result := TFHIRWorkerContext(inherited link);
end;

function TFHIRWorkerContext.expand(vs: TFhirValueSetW; options : TExpansionOperationOptionSet = []): TFHIRValueSetW;
begin
  result := Factory.wrapValueSet(expand(vs.Resource as TFhirValueSet, options));
end;

function TFHIRWorkerContext.validateCode(systemUri, version, code: String; vs: TFhirValueSetW): TValidationResult;
begin
  result := validateCode(systemUri, version, code, vs.Resource as TFHIRValueSet);
end;

function TFHIRWorkerContext.fetchCodeSystem(url: String): TFhirCodeSystem;
begin
  result := fetchResource(frtCodeSystem, url) as TFHIRCodeSystem;
end;

function TFHIRWorkerContext.fetchConceptMap(url: String): TFhirConceptMap;
begin
  result := fetchResource(frtConceptMap, url) as TFHIRConceptMap;
end;

function TFHIRWorkerContext.fetchResource(rType, url: String): TFhirResourceV;
var
  t : TFhirResourceType;
begin
  if RecogniseFHIRResourceName(rType, t) then
    result := fetchResource(t, url)
  else
    raise EFHIRException.create('Unknown type '+rType+' in '+versionString);
end;

function TFHIRWorkerContext.fetchStructureDefinition(url: String): TFhirStructureDefinition;
begin
  result := fetchResource(frtStructureDefinition, url) as TFHIRStructureDefinition;
end;

function TFHIRWorkerContext.fetchStructureMap(url: String): TFhirStructureMap;
begin
  result := fetchResource(frtStructureMap, url) as TFHIRStructureMap;
end;

function TFHIRWorkerContext.fetchValueSet(url: String): TFhirValueSet;
begin
  result := fetchResource(frtValueSet, url) as TFHIRValueSet;
end;

function TFHIRWorkerContext.getSliceList(profile: TFHIRStructureDefinition; element: TFhirElementDefinition): TFHIRElementDefinitionList;
begin
  raise Exception.Create('Error Message');
end;

procedure TFHIRWorkerContext.listStructures(list: TFslList<TFhirStructureDefinitionW>);
var
  l : TFslList<TFHIRStructureDefinition>;
  sd : TFHIRStructureDefinition;
begin
  l := TFslList<TFHIRStructureDefinition>.create;
  try
    listStructures(l);
    for sd in l do
      list.add(factory.wrapStructureDefinition(sd.link));
  finally
    l.Free;
  end;
end;


procedure TFHIRWorkerContext.loadFromCache(cache: TResourceMemoryCache);
var
  r : TFhirResource;
begin
  cache.checkLoaded;
  for r in cache.List do
    SeeResource(r);
end;

{ TFHIRCustomResourceInformation }

constructor TFHIRCustomResourceInformation.Create(definition: TFHIRStructureDefinition);
begin
  inherited Create;
  FDefinition := definition;
  FName := definition.snapshot.elementList[0].path;
  FSearchParameters := TFslList<TFHIRSearchParameter>.create;
end;

destructor TFHIRCustomResourceInformation.Destroy;
begin
  FSearchParameters.Free;
  FDefinition.Free;
  inherited;
end;

function TFHIRCustomResourceInformation.Link: TFHIRCustomResourceInformation;
begin
  result := TFHIRCustomResourceInformation(inherited Link);
end;


function TFHIRCustomResourceInformation.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FSearchParameters.sizeInBytes);
  inc(result, FDefinition.sizeInBytes);
end;

{ TResourceMemoryCache }

procedure TResourceMemoryCache.checkLoaded;
var
  cache : TFHIRPackageManager;
  s, l, r : String;
begin
  if FList.Empty then
  begin
    cache := TFHIRPackageManager.Create(true);
    try
      cache.onWork := FOnLog;
      for s in FPackages do
      begin
        FLoadPackage := s;
        StringSplit(s, '#', l, r);
        cache.loadPackage(l, r, FResourceTypes, FLoadInfo);
      end;
    finally
      cache.Free;
    end;
  end;
end;

constructor TResourceMemoryCache.Create;
begin
  inherited;
  Flist := TFslList<TFhirResource>.create;
  FLoadInfo := TPackageLoadingInformation.Create('4.2');
  FLoadInfo.OnLoadEvent := load;
end;

destructor TResourceMemoryCache.Destroy;
begin
  FLoadInfo.Free;
  FList.Free;
  inherited;
end;

function TResourceMemoryCache.Link: TResourceMemoryCache;
begin
  result := TResourceMemoryCache(inherited link);
end;

procedure TResourceMemoryCache.load(rType, id: String; stream: TStream);
var
  p : TFHIRJsonParser;
begin
  p := TFHIRJsonParser.Create(nil, THTTPLanguages.create('en'));
  try
    p.source := stream;
    p.Parse;
    FList.add(p.resource.link as TFhirResource);
  finally
    p.Free;
  end;

end;

{ TFHIRMetadataResourceManager<T> }

constructor TFHIRMetadataResourceManager<T>.Create;
begin
  inherited;
  FMap := TFslMap<T>.create('metadata resource manager '+t.className);
  FMap.defaultValue := T(nil);
  FList := TFslList<T>.create;
end;

destructor TFHIRMetadataResourceManager<T>.Destroy;
begin
  FMap.Free;
  FList.Free;
  inherited;
end;

procedure TFHIRMetadataResourceManager<T>.Assign(oSource: TFslObject);
var
  src : TFHIRMetadataResourceManager<T>;
begin
  inherited;
  src := oSource as TFHIRMetadataResourceManager<T>;
  FMap.Clear;
  FList.Clear;
  FMap.addAll(src.FMap);
  Flist.addAll(src.FList);
end;

function TFHIRMetadataResourceManager<T>.clone: TFHIRMetadataResourceManager<T>;
begin
  result := TFHIRMetadataResourceManager<T>(inherited clone);
end;

function TFHIRMetadataResourceManager<T>.link: TFHIRMetadataResourceManager<T>;
begin
  result := TFHIRMetadataResourceManager<T>(inherited link);
end;

procedure TFHIRMetadataResourceManager<T>.see(r : T);
begin
  if (r.id = '') then
    r.id := newGUIDId;
  if (FMap.containsKey(r.id)) then
    drop(r.id);

  FList.add(r.link);
  FMap.add(r.id, r.link); // we do this so we can drop by id

  if (r.url <> '') then
  begin
    // first, this is the correct resource for this version (if it has a version)
    if (r.version <> '') then
    begin
      FMap.add(r.url+'|'+r.version, r.link);
    end;
    updateList(r.url, r.version);
  end;
end;

{$IFDEF FPC}
function TFHIRMetadataResourceManager<T>.sort(sender : TObject; const L, R: T): Integer;
var
  v1, v2, mm1, mm2 : string;
begin
  v1 := l.version;
  v2 := r.version;
  if (v1 = '') and (v2 = '') then
    result := FList.indexOf(l) - FList.indexOf(r)
  else if (v1 = '') then
    result := -1
  else if (v2 = '') then
    result := 1
  else
  begin
    mm1 := TFHIRVersions.getMajMin(v1);
    mm2 := TFHIRVersions.getMajMin(v2);
    if (mm1 = '') or (mm2 = '') then
      result := v1.compareTo(v2)
    else
      result := CompareText(mm1, mm2);
  end;
end;
{$ENDIF}

procedure TFHIRMetadataResourceManager<T>.updateList(url, version : String);
var
  rl : TFslList<T>;
  tt, latest : T;
  lv : String;
begin
  rl := TFslList<T>.create;
  try
    for tt in FList do
    begin
      if (url = tt.url) and not rl.contains(tt) then
        rl.add(tt.link);
    end;

    if (rl.count > 0) then
    begin
      // sort by version as much as we are able
      {$IFDEF FPC}
      rl.sortE(sort);
      {$ELSE}
      rl.sortF(function (const L, R: T): Integer
        var v1, v2, mm1, mm2 : string;
        begin
          v1 := l.version;
          v2 := r.version;
          if (v1 = '') and (v2 = '') then
            result := FList.indexOf(l) - FList.indexOf(r)
          else if (v1 = '') then
            result := -1
          else if (v2 = '') then
            result := 1
          else
          begin
            mm1 := TFHIRVersions.getMajMin(v1);
            mm2 := TFHIRVersions.getMajMin(v2);
            if (mm1 = '') or (mm2 = '') then
              result := v1.compareTo(v2)
            else
              result := CompareText(mm1, mm2);
          end;
        end);
      {$ENDIF}

      // the current is the latest
      FMap.AddOrSetValue(url, rl[rl.count-1].link);
      // now, also, the latest for major/minor
      if (version <> '') then
      begin
        latest := T(nil);
        for tt in rl do
        begin
          if (TFHIRVersions.matches(tt.version, version)) then
            latest := tt;
        end;
        if (latest <> T(nil)) then // might be null if it's not using semver
        begin
          lv := TFHIRVersions.getMajMin(latest.version);
          if (lv <> version) then
            FMap.AddOrSetValue(url+'|'+lv, rl[rl.count-1].link);
        end;
      end;
    end;
  finally
   rl.free;
  end;
end;

function TFHIRMetadataResourceManager<T>.get(url : String) : T;
begin
  result := FMap[url];
end;

function TFHIRMetadataResourceManager<T>.has(url : String) : boolean;
begin
  result := FMap.containsKey(url);
end;

function TFHIRMetadataResourceManager<T>.get(url, version : string) : T;
var
  mm : String;
begin
  if (FMap.containsKey(url+'|'+version)) then
    result := FMap[url+'|'+version]
  else
  begin
    mm := TFHIRVersions.getMajMin(version);
    if (mm <> '') then
      result := FMap[url+'|'+mm]
    else
      result := T(nil);
  end;
end;

function TFHIRMetadataResourceManager<T>.has(url, version : string) : boolean;
var
  mm : String;
begin
  if (FMap.containsKey(url+'|'+version)) then
    result := true
  else
  begin
    mm := TFHIRVersions.getMajMin(version);
    if (mm <> '') then
      result := FMap.containsKey(url+'|'+mm)
    else
     result := false;
  end;
end;

function TFHIRMetadataResourceManager<T>.count : integer;
begin
  result := FList.count;
end;

procedure TFHIRMetadataResourceManager<T>.drop(id : String);
var
  res : T;
  mm : String;
begin
  res := FMap[id];
  if (res <> T(nil)) then
  begin
    FList.remove(res);
    FMap.remove(id);
    FMap.remove(res.url);
    if (res.version <> '') then
    begin
      FMap.remove(res.url+'|'+res.version);
      mm := TFHIRVersions.getMajMin(res.version);
      if (mm <> '') then
        FMap.remove(res.url+'|'+mm);
    end;
    updateList(res.url, res.version);
  end;
end;

procedure TFHIRMetadataResourceManager<T>.listAll(list : TFslList<T>);
begin
  list.addAll(Flist);
end;

procedure TFHIRMetadataResourceManager<T>.listAllM(list : TFslList<TFHIRMetadataResource>);
var
  tt : T;
begin
  for tt in FList do
    list.add(tt.link);
end;

procedure TFHIRMetadataResourceManager<T>.clear();
begin
  FList.clear();
  FMap.clear();
end;

function TFHIRMetadataResourceManager<T>.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FMap.sizeInBytes);
  inc(result, FList.sizeInBytes);
end;

function TResourceMemoryCache.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Flist.sizeInBytes);
  inc(result, FLoadInfo.sizeInBytes);
//  inc(result, FResourceTypes.sizeInBytes);
//  inc(result, FPackages.sizeInBytes);
  inc(result, (FLoadPackage.length * sizeof(char)) + 12);
end;

end.
