unit FHIR.R4.Context;

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


interface

uses
  SysUtils, Classes,
  FHIR.Support.Base, FHIR.Support.Utilities,
  FHIR.Cache.PackageManager,
  FHIR.Base.Objects, FHIR.Base.Factory, FHIR.Base.Common, FHIR.Base.Lang,
  FHIR.R4.Types, FHIR.R4.Resources;

type
  TFHIRCustomResourceInformation = class (TFslObject)
  private
    FName: String;
    FSearchParameters: TFslList<TFHIRSearchParameter>;
    FDefinition: TFHIRStructureDefinition;
  public
    constructor Create(definition : TFHIRStructureDefinition);
    destructor Destroy; override;
    function Link : TFHIRCustomResourceInformation; overload;
    property Name : String read FName;
    property Definition : TFHIRStructureDefinition read FDefinition;
    property SearchParameters : TFslList<TFHIRSearchParameter> read FSearchParameters;
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
    function expand(vs : TFhirValueSet; options : TExpansionOperationOptionSet = []) : TFHIRValueSet; overload; virtual; abstract;
    function validateCode(system, version, code : String; vs : TFhirValueSet) : TValidationResult; overload; virtual; abstract;
    function validateCode(system, version, code : String) : TValidationResult; overload; virtual; abstract;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; overload; virtual; abstract;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; overload; virtual; abstract;
    function getChildMap(profile : TFHIRStructureDefinition; element : TFhirElementDefinition) : TFHIRElementDefinitionList; virtual;  abstract;
    function getStructure(url : String) : TFHIRStructureDefinition; overload; virtual; abstract;
    function getStructure(ns, name : String) : TFHIRStructureDefinition; overload; virtual; abstract;
    function getCustomResource(name : String) : TFHIRCustomResourceInformation; virtual; abstract;
    function hasCustomResource(name : String) : boolean; virtual; abstract;

    // override version independent variants:
    function fetchResource(rType : String; url : String) : TFhirResourceV; overload; override;
    function expand(vs : TFhirValueSetW; options : TExpansionOperationOptionSet = []) : TFHIRValueSetW; overload; override;
    function validateCode(system, version, code : String; vs : TFhirValueSetW) : TValidationResult; overload; override;
    procedure listStructures(list : TFslList<TFhirStructureDefinitionW>); overload; override;

    procedure loadFromCache(cache : TResourceMemoryCache); overload;
    property OverrideVersionNs : String read FOverrideVersionNs write FOverrideVersionNs;
  end;

  TResourceMemoryCache = class (TFslObject)
  private
    Flist : TFslList<TFHIRResource>;
    FResourceTypes: TArray<String>;
    FPackages: TArray<String>;
    FOnLog: TWorkProgressEvent;
    FLoadPackage : String;
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
  FHIR.R4.Utilities, FHIR.R4.ParserBase, FHIR.R4.Json;

{ TFHIRWorkerContext }

function TFHIRWorkerContext.GetVersion: TFHIRVersion;
begin
  result := fhirVersionRelease4;
end;

function TFHIRWorkerContext.link: TFHIRWorkerContext;
begin
  result := TFHIRWorkerContext(inherited link);
end;

function TFHIRWorkerContext.expand(vs: TFhirValueSetW; options : TExpansionOperationOptionSet = []): TFHIRValueSetW;
begin
  result := Factory.wrapValueSet(expand(vs.Resource as TFhirValueSet, options));
end;

function TFHIRWorkerContext.validateCode(system, version, code: String; vs: TFhirValueSetW): TValidationResult;
begin
  result := validateCode(system, version, code, vs.Resource as TFHIRValueSet);
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
      for s in FPackages do
      begin
        FLoadPackage := s;
        StringSplit(s, '#', l, r);
        cache.loadPackage(l, r, FResourceTypes, load, FOnLog);
      end;
    finally
      cache.Free;
    end;
  end;
end;

constructor TResourceMemoryCache.Create;
begin
  inherited;
  Flist := TFslList<TFhirResource>.create
end;

destructor TResourceMemoryCache.Destroy;
begin
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
  p := TFHIRJsonParser.Create(nil, 'en');
  try
    p.source := stream;
    p.Parse;
    FList.add(p.resource.link as TFhirResource);
  finally
    p.Free;
  end;

end;

end.
