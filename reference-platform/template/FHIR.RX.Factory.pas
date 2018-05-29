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

// {{gen}}

uses
  FHIR.Ucum.IFace,
  FHIR.Base.Objects, FHIR.Base.Parser, FHIR.Base.Validator, FHIR.Base.Narrative, FHIR.Base.Factory, FHIR.Base.PathEngine,
  FHIR.Base.Common,
  FHIR.Client.Base, FHIR.Client.Threaded;

type
  TFHIRFactoryR{{v}} = class (TFHIRFactory)
  public
    function version : TFHIRVersion; override;
    function versionString : String; override;
    function description : String; override;
    function makeParser(worker : TFHIRWorkerContextV; format : TFHIRFormat; lang : String) : TFHIRParser; override;
    function makeComposer(worker : TFHIRWorkerContextV; format : TFHIRFormat; lang : String; style: TFHIROutputStyle) : TFHIRComposer; override;
    function makeValidator(worker : TFHIRWorkerContextV) : TFHIRValidatorV; override;
    function makeGenerator(worker : TFHIRWorkerContextV) : TFHIRNarrativeGeneratorBase; override;
    function makePathEngine(worker : TFHIRWorkerContextV; ucum : TUcumServiceInterface) : TFHIRPathEngineV; override;
    function makeClientHTTP(worker : TFHIRWorkerContextV; url : String; fmt : TFHIRFormat; timeout : cardinal; proxy : String) : TFhirClientV; overload; override;
    function makeClientThreaded(worker : TFHIRWorkerContextV; internal : TFhirClientV; event : TThreadManagementEvent) : TFhirClientV; overload; override;

    function makeByName(const name : String) : TFHIRObject; override;
    function makeBoolean(b : boolean): TFHIRObject; override;
    function makeCode(s : string) : TFHIRObject; override;
    function makeString(s : string) : TFHIRObject; override;
    function makeInteger(s : string) : TFHIRObject; override;
    function makeDecimal(s : string) : TFHIRObject; override;
    function makeBase64Binary(s : string) : TFHIRObject; override;
    function makeParameters : TFHIRParametersW; override;
    function wrapCapabilityStatement(r : TFHIRResourceV) : TFHIRCapabilityStatementW; override;
  end;

implementation

uses
  Soap.EncdDecd,
  FHIR.Client.HTTP,
  FHIR.R{{v}}.Types, FHIR.R{{v}}.Resources, FHIR.R{{v}}.Parser, FHIR.R{{v}}.Context, FHIR.R{{v}}.Validator,
  FHIR.R{{v}}.Narrative, FHIR.R{{v}}.PathEngine, FHIR.R{{v}}.Constants, FHIR.R{{v}}.Client, FHIR.R{{v}}.Common;

{ TFHIRFactoryR{{v}} }

function TFHIRFactoryR{{v}}.description: String;
begin
  result := 'R{{v}} ('+FHIR_GENERATED_VERSION+')';
end;

function TFHIRFactoryR{{v}}.makeClientHTTP(worker: TFHIRWorkerContextV; url: String; fmt : TFHIRFormat; timeout: cardinal; proxy: String): TFhirClientV;
var
  http : TFHIRHTTPCommunicator;
begin
  http := TFHIRHTTPCommunicator.Create(url);
  try
    http.timeout := timeout;
    http.proxy := proxy;
    result := TFhirClient{{v}}.create(worker, 'en', http.link);
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

function TFHIRFactoryR{{v}}.makeClientThreaded(worker: TFHIRWorkerContextV;
  internal: TFhirClientV; event: TThreadManagementEvent): TFhirClientV;
begin

end;

function TFHIRFactoryR{{v}}.makeCode(s: string): TFHIRObject;
begin
  result := TFhirCode.Create(s);
end;

function TFHIRFactoryR{{v}}.makeComposer(worker: TFHIRWorkerContextV; format: TFHIRFormat; lang: String; style: TFHIROutputStyle): TFHIRComposer;
begin
  result := TFHIRParsers{{v}}.composer(worker as TFHIRWorkerContext, format, lang, style);
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

end;

function TFHIRFactoryR{{v}}.makeParser(worker: TFHIRWorkerContextV; format: TFHIRFormat; lang: String): TFHIRParser;
begin
  result := TFHIRParsers{{v}}.parser(worker as TFHIRWorkerContext, format, lang);
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

function TFHIRFactoryR{{v}}.version: TFHIRVersion;
begin
  result := fhirVersionRelease{{v}};
end;

function TFHIRFactoryR{{v}}.versionString: String;
begin
  result := FHIR_GENERATED_VERSION;
end;

function TFHIRFactoryR{{v}}.wrapCapabilityStatement(r: TFHIRResourceV): TFHIRCapabilityStatementW;
begin
  result := TFHIRCapabilityStatement{{v}}.create(r);
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
