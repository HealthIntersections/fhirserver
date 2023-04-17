unit fhir_package_upload;

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

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_utilities, fsl_logging, fsl_lang, fsl_npm, fsl_npm_cache,
  fhir_objects, fhir_client, fhir_factory, fhir_common;

type

  { TFHIRPackageUploader }

  TFHIRPackageUploader = class (TFslObject)
  private
    FAPIResources: boolean;
    FBulkImport: boolean;
    FClient: TFHIRClientV;
    FDefinitionResources: boolean;
    FDependencies: boolean;
    FExampleResources: boolean;
    FFactory: TFHIRFactory;
    FNoErrorIfExists: boolean;
    FOnLog: TLogEvent;
    FProfileResources: boolean;
    FQuestionnaireResources: boolean;
    FStopForErrors: boolean;
    FTerminologyResources: boolean;

    FLoadedPackages : TStringList;
    FCache : TFHIRPackageManager;

    procedure SetClient(AValue: TFHIRClientV);
    procedure SetFactory(AValue: TFHIRFactory);

    procedure log(msg : String);
    function describe(r : TNpmPackageResource) : String;

    function resourceInScope(name : String) : boolean;
    function checkResourceById(rType, id : String) : boolean;
    function checkResourceByUrl(rType, url, version : String) : boolean;
    procedure uploadResource(npm : TNpmPackage; r : TNpmPackageResource);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure uploadPackage(package : String);

    property client : TFHIRClientV read FClient write SetClient;
    property factory : TFHIRFactory read FFactory write SetFactory;

    // settings:
    property TerminologyResources : boolean read FTerminologyResources write FTerminologyResources;
    property ProfileResources : boolean read FProfileResources write FProfileResources;
    property APIResources : boolean read FAPIResources write FAPIResources;
    property QuestionnaireResources : boolean read FQuestionnaireResources write FQuestionnaireResources;
    property DefinitionResources : boolean read FDefinitionResources write FDefinitionResources;
    property ExampleResources : boolean read FExampleResources write FExampleResources;
    property Dependencies : boolean read FDependencies write FDependencies;
    property BulkImport : boolean read FBulkImport write FBulkImport;
    property StopForErrors : boolean read FStopForErrors write FStopForErrors;
    property NoErrorIfExists : boolean read FNoErrorIfExists write FNoErrorIfExists;

    Property OnLog : TLogEvent read FOnLog write FOnLog;
  end;

implementation

{ TFHIRPackageUploader }

constructor TFHIRPackageUploader.Create;
begin
  inherited Create;
  FLoadedPackages := TStringList.create;
  FCache := TFHIRPackageManager.create(true);
end;

destructor TFHIRPackageUploader.Destroy;
begin
  FClient.free;
  FFactory.free;
  FLoadedPackages.free;
  FCache.Free;
  inherited Destroy;
end;

procedure TFHIRPackageUploader.SetClient(AValue: TFHIRClientV);
begin
  FClient.free;
  FClient := AValue;
end;

procedure TFHIRPackageUploader.SetFactory(AValue: TFHIRFactory);
begin
  FFactory.free;
  FFactory := AValue;
end;

procedure TFHIRPackageUploader.uploadResource(npm : TNpmPackage; r: TNpmPackageResource);
var
  byId, exists : boolean;
  res : TFHIRResourceV;
  id : String;
begin
  if not resourceInScope(r.ResourceType) then
    exit;

  byId := r.URL = '';
  if byId then
    exists := checkResourceById(r.ResourceType, r.Id)
  else
    exists := checkResourceByUrl(r.ResourceType, r.Url, r.version);

  if exists then
  begin
    if NoErrorIfExists then
    begin
      log('Resource '+describe(r)+' already exists - not uploading');
      exit;
    end
    else
    begin
      log('Resource '+describe(r)+' already exists - halting');
      raise EFHIRException.create('Resource '+describe(r)+' already exists');
    end
  end;

  res := FFactory.parseJson(nil, npm.loadBytes(r));
  try
    if byId then
    begin
      FClient.updateResourceV(res);
      id := res.id;
    end
    else
    begin
      FClient.createResourceV(res, id);
    end;
  finally
    res.free;
  end;
end;

procedure TFHIRPackageUploader.uploadPackage(package : String);
var
  pck : TNpmPackage;
  d : String;
  f : TNpmPackageFolder;
  r : TNpmPackageResource;
begin
  if FLoadedPackages.IndexOf(package) > -1 then
    exit;
  FLoadedPackages.Add(package);

  pck := FCache.loadPackage(package);
  try
    if FDependencies then
      for d in pck.dependencies do
        uploadPackage(d);
    for f in pck.folders.Values do
      for r in f.resources do
        uploadResource(pck, r);
  finally
    pck.free;
  end;
end;

procedure TFHIRPackageUploader.log(msg : String);
begin
  if assigned(FOnLog) then
    FOnLog(msg);
end;

function TFHIRPackageUploader.describe(r : TNpmPackageResource) : String;
begin
  if r.URL = '' then
    result := r.ResourceType+'/'+r.Id
  else if r.version = '' then
    result := r.ResourceType+'/'+r.Id+' ('+r.url+')'
  else
    result := r.ResourceType+'/'+r.Id+' ('+r.url+'|'+r.version+')';
end;

function TFHIRPackageUploader.resourceInScope(name : String) : boolean;
begin
  result := false;

  if TerminologyResources and StringArrayExists(['CodeSystem', 'ValueSet', 'ConceptMap', 'NamingSystem'], name) then
    exit(true);

  if ProfileResources and StringArrayExists(['StructureDefinition', 'StructureMap', 'MessageDefinition', 'ImplementationGuide'], name) then
    exit(true);

  if APIResources and StringArrayExists(['CapabilityStatement', 'Conformance', 'SearchParameter', 'OperationDefinition', 'CompartmentDefinition', 'GraphDefinition', '', ''], name) then
    exit(true);

  if QuestionnaireResources and StringArrayExists(['Questionnaire', 'QuestionnaireResponse'], name) then
    exit(true);

  if DefinitionResources and (name.Contains('Definition') or StringArrayExists(['RequestGroup', 'Measure'], name)) then
    exit(true);

  if ExampleResources then
    result := not (
       StringArrayExists(['CodeSystem', 'ValueSet', 'ConceptMap', 'NamingSystem'], name) or
       StringArrayExists(['StructureDefinition', 'StructureMap', 'MessageDefinition', 'ImplementationGuide'], name) or
       StringArrayExists(['CapabilityStatement', 'Conformance', 'SearchParameter', 'OperationDefinition', 'CompartmentDefinition', 'GraphDefinition', '', ''], name) or
       StringArrayExists(['Questionnaire', 'QuestionnaireResponse'], name) or
       (name.Contains('Definition') or StringArrayExists(['RequestGroup', 'Measure'], name)));
end;

function TFHIRPackageUploader.checkResourceById(rType, id : String) : boolean;
var
  res : TFHIRResourceV;
begin
  res := FClient.readResourceV(rType, id);
  try
    result := res <> nil;
  finally
    res.free;
  end;
end;

function TFHIRPackageUploader.checkResourceByUrl(rType, url, version : String) : boolean;
var
  bnd : TFHIRBundleW;
  params : String;
begin
  params := '_summary=count&url='+url;
  if (version <> '') then
    params := params+'&version='+version;

  bnd := FFactory.wrapBundle(FClient.searchV(rType, false, params));
  try
    result := bnd.total > 1;
  finally
    bnd.free;
  end;
end;

end.

