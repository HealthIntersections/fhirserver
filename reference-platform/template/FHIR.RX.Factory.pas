unit FHIR.R{{v}}.Factory;

{$I fhir.inc}

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

interface

// FHIR v3.4.0 generated 2018-05-15T06:48:00+10:00

uses
  FHIR.Ucum.IFace,
  FHIR.Base.Objects, FHIR.Base.Parser, FHIR.Base.Validator, FHIR.Base.Narrative, FHIR.Base.Factory, FHIR.Base.PathEngine, FHIR.Base.Xhtml, FHIR.Base.Common,
  FHIR.Client.Base, FHIR.Client.Threaded;

type
  TFHIRFactoryR{{v}} = class (TFHIRFactory)
  public
    function version : TFHIRVersion; override;
    function versionString : String; override;
    function description : String; override;
    function resourceNames : TArray<String>; override;
    function makeParser(worker : TFHIRWorkerContextV; format : TFHIRFormat; lang : String) : TFHIRParser; override;
    function makeComposer(worker : TFHIRWorkerContextV; format : TFHIRFormat; lang : String; style: TFHIROutputStyle) : TFHIRComposer; override;
    function makeValidator(worker : TFHIRWorkerContextV) : TFHIRValidatorV; override;
    function makeGenerator(worker : TFHIRWorkerContextV) : TFHIRNarrativeGeneratorBase; override;
    function makePathEngine(worker : TFHIRWorkerContextV; ucum : TUcumServiceInterface) : TFHIRPathEngineV; override;
    function createFromProfile(worker : TFHIRWorkerContextV; profile : TFhirStructureDefinitionW) : TFHIRResourceV; override;
    function makeClient(worker : TFHIRWorkerContextV; url : String; kind : TFHIRClientType; fmt : TFHIRFormat; timeout : cardinal; proxy : String) : TFhirClientV; overload; override;
    function makeClientThreaded(worker : TFHIRWorkerContextV; internal : TFhirClientV; event : TThreadManagementEvent) : TFhirClientV; overload; override;

    function getXhtml(res : TFHIRResourceV) : TFHIRXhtmlNode; override;
    function resetXhtml(res : TFHIRResourceV) : TFHIRXhtmlNode; override;

    procedure checkNoImplicitRules(res : TFHIRObject; method, param : string); override;
    procedure checkNoModifiers(res : TFHIRObject; method, param : string; allowed : TArray<String> = []); override;

    function makeByName(const name : String) : TFHIRObject; override;
    function makeBoolean(b : boolean): TFHIRObject; override;
    function makeCode(s : string) : TFHIRObject; override;
    function makeString(s : string) : TFHIRObject; override;
    function makeInteger(s : string) : TFHIRObject; override;
    function makeDecimal(s : string) : TFHIRObject; override;
    function makeBase64Binary(s : string) : TFHIRObject; override;
    function makeParameters : TFHIRParametersW; override;
    function wrapCapabilityStatement(r : TFHIRResourceV) : TFHIRCapabilityStatementW; override;
    function wrapStructureDefinition(r : TFHIRResourceV) : TFhirStructureDefinitionW; override;
    function wrapValueSet(r : TFHIRResourceV) : TFhirValueSetW; override;
    function wrapCodeSystem(r : TFHIRResourceV) : TFhirCodeSystemW; override;
    function wrapExtension(o : TFHIRObject) : TFhirExtensionW; override;
    function wrapCoding(o : TFHIRObject) : TFhirCodingW; override;
    function wrapOperationOutcome(r : TFHIRResourceV) : TFhirOperationOutcomeW; override;
    function wrapBundle(r : TFHIRResourceV) : TFhirBundleW; override;
  end;
  TFHIRFactoryX = TFHIRFactoryR{{v}};

implementation

uses
  Soap.EncdDecd,
  FHIR.Client.HTTP,
  FHIR.R{{v}}.Types, FHIR.R{{v}}.Resources, FHIR.R{{v}}.Parser, FHIR.R{{v}}.Context, FHIR.R{{v}}.Validator, FHIR.R{{v}}.Profiles,
  FHIR.R{{v}}.Narrative, FHIR.R{{v}}.PathEngine, FHIR.R{{v}}.Constants, FHIR.R{{v}}.Client, FHIR.R{{v}}.Common, FHIR.R{{v}}.Utilities;

{ TFHIRFactoryR{{v}} }

procedure TFHIRFactoryR{{v}}.checkNoImplicitRules(res: TFHIRObject; method, param: string);
begin
  if res is TFHIRResource then
    (res as TFHIRResource).checkNoImplicitRules(method, param);
end;

procedure TFHIRFactoryR{{v}}.checkNoModifiers(res: TFHIRObject; method, param: string; allowed : TArray<String> = []);
begin
  if res is TFHIRDomainResource then
    TFHIRDomainResource(res).checkNoModifiers(method, param)
  else if res is TFHIRBackboneElement then
    TFHIRBackboneElement(res).checkNoModifiers(method, param)
end;

function TFHIRFactoryR{{v}}.createFromProfile(worker: TFHIRWorkerContextV; profile: TFhirStructureDefinitionW): TFHIRResourceV;
var
  pu : TProfileUtilities;
begin
  pu := TProfileUtilities.create(worker.Link as TFHIRWorkerContext, nil);
  try
    result := pu.populateByProfile(profile.Resource as TFhirStructureDefinition);
  finally
    pu.Free;
  end;
end;

function TFHIRFactoryR{{v}}.description: String;
begin
  result := 'R{{v}} ('+FHIR_GENERATED_VERSION+')';
end;

function TFHIRFactoryR{{v}}.getXhtml(res: TFHIRResourceV): TFHIRXhtmlNode;
var
  r : TFHIRDomainResource;
begin
  if res = nil then
    exit(nil);
  if not (res is TFHIRDomainResource) then
    exit(nil);
  r := res as TFHIRDomainResource;
  if (r.text = nil) then
    result := nil
  else
    result := r.text.div_;
end;

function TFHIRFactoryR{{v}}.makeClient(worker: TFHIRWorkerContextV; url: String; kind : TFHIRClientType; fmt : TFHIRFormat; timeout: cardinal; proxy: String): TFhirClientV;
var
  http : TFHIRHTTPCommunicator;
begin
  http := TFHIRHTTPCommunicator.Create(url);
  try
    if kind = fctCrossPlatform then
      http.UseIndy := true;
    http.timeout := timeout;
    http.proxy := proxy;
    result := TFhirClient4.create(worker, 'en', http.link);
    try
      result.format := fmt;
      result.link;
    finally
      result.Free;
    end;
  finally
    http.free;
  end;
end;

function TFHIRFactoryR{{v}}.makeClientThreaded(worker: TFHIRWorkerContextV; internal: TFhirClientV; event: TThreadManagementEvent): TFhirClientV;
var
  c : TFhirThreadedCommunicator;
begin
  c := TFhirThreadedCommunicator.Create(internal, event);
  try
    result := TFhirClient4.create(worker, 'en', c.link);
    try
      result.format := internal.format;
      result.link;
    finally
      result.Free;
    end;
  finally
    c.free;
  end;
end;

function TFHIRFactoryR{{v}}.makeCode(s: string): TFHIRObject;
begin
  result := TFhirCode.Create(s);
end;

function TFHIRFactoryR{{v}}.makeComposer(worker: TFHIRWorkerContextV; format: TFHIRFormat; lang: String; style: TFHIROutputStyle): TFHIRComposer;
begin
  result := TFHIRParsers4.composer(worker as TFHIRWorkerContext, format, lang, style);
end;

function TFHIRFactoryR{{v}}.makeDecimal(s: string): TFHIRObject;
begin
  result := TFhirDecimal.Create(s);
end;

function TFHIRFactoryR{{v}}.makeGenerator(worker: TFHIRWorkerContextV): TFHIRNarrativeGeneratorBase;
begin
  result := TFHIRNarrativeGenerator.create(worker);
end;

function TFHIRFactoryR{{v}}.makeInteger(s: string): TFHIRObject;
begin
  result := TFhirInteger.Create(s);
end;

function TFHIRFactoryR{{v}}.makeParameters: TFHIRParametersW;
begin
  result := TFHIRParameters4.Create(TFHIRParameters.Create);
end;

function TFHIRFactoryR{{v}}.makeParser(worker: TFHIRWorkerContextV; format: TFHIRFormat; lang: String): TFHIRParser;
begin
  result := TFHIRParsers4.parser(worker as TFHIRWorkerContext, format, lang);
end;

function TFHIRFactoryR{{v}}.makePathEngine(worker: TFHIRWorkerContextV; ucum : TUcumServiceInterface): TFHIRPathEngineV;
begin
  result := TFHIRPathEngine.Create(worker as TFHIRWorkerContext, ucum);
end;

function TFHIRFactoryR{{v}}.makeString(s: string): TFHIRObject;
begin
  result := TFhirString.Create(s);
end;

function TFHIRFactoryR{{v}}.makeValidator(worker: TFHIRWorkerContextV): TFHIRValidatorV;
begin
  result := TFHIRValidator{{v}}.Create(worker as TFHIRWorkerContext);
end;

function TFHIRFactoryR{{v}}.resetXhtml(res: TFHIRResourceV): TFHIRXhtmlNode;
var
  r : TFHIRDomainResource;
begin
  if res = nil then
    exit(nil);
  if not (res is TFHIRDomainResource) then
    exit(nil);
  r := res as TFHIRDomainResource;
  r.text := TFHIRNarrative.Create;
  r.text.status := NarrativeStatusGenerated;
  r.text.div_ := TFhirXHtmlNode.Create('div');
  result := r.text.div_;
end;

function TFHIRFactoryR{{v}}.resourceNames: TArray<String>;
var
  a : TFhirResourceType;
begin
  SetLength(result, length(CODES_TFhirResourceType)-2);
  for a in ALL_RESOURCE_TYPES do
    if not (a in [frtNull, frtCustom]) then
      result[ord(a)-1] := CODES_TFhirResourceType[a];
end;

function TFHIRFactoryR{{v}}.version: TFHIRVersion;
begin
  result := fhirVersionRelease4;
end;

function TFHIRFactoryR{{v}}.versionString: String;
begin
  result := FHIR_GENERATED_VERSION;
end;

function TFHIRFactoryR{{v}}.wrapBundle(r: TFHIRResourceV): TFhirBundleW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirBundle4.Create(r);
end;

function TFHIRFactoryR{{v}}.wrapCapabilityStatement(r: TFHIRResourceV): TFHIRCapabilityStatementW;
begin
  if r = nil then
    result := nil
  else
    result := TFHIRCapabilityStatement4.create(r);
end;

function TFHIRFactoryR{{v}}.wrapCodeSystem(r: TFHIRResourceV): TFhirCodeSystemW;
begin
  if r = nil then
    result := nil
  else
    result := TFHIRCodeSystem4.create(r);
end;

function TFHIRFactoryR{{v}}.wrapCoding(o: TFHIRObject): TFhirCodingW;
begin
  result := TFhirCoding4.create(o);
end;

function TFHIRFactoryR{{v}}.wrapExtension(o: TFHIRObject): TFhirExtensionW;
begin
  result := TFhirExtension4.create(o);
end;

function TFHIRFactoryR{{v}}.wrapOperationOutcome(r: TFHIRResourceV): TFhirOperationOutcomeW;
begin
  if r = nil then
    result := nil
  else
    result := TFhirOperationOutcome4.Create(r);
end;

function TFHIRFactoryR{{v}}.wrapStructureDefinition(r: TFHIRResourceV): TFhirStructureDefinitionW;
begin
  if r = nil then
    result := nil
  else
    result := TFHIRStructureDefinition4.create(r);
end;

function TFHIRFactoryR{{v}}.wrapValueSet(r: TFHIRResourceV): TFhirValueSetW;
begin
  if r = nil then
    result := nil
  else
    result := TFHIRValueSet4.create(r);
end;

function TFHIRFactoryR{{v}}.makeBase64Binary(s: string): TFHIRObject;
begin
  result := TFhirBase64Binary.Create(decodeBase64(s));
end;

function TFHIRFactoryR{{v}}.makeBoolean(b: boolean): TFHIRObject;
begin
  result := TFhirBoolean.Create(b);
end;

function TFHIRFactoryR{{v}}.makeByName(const name : String) : TFHIRObject;
begin
{{fact}}
  else
    result := nil;
end;



end.
