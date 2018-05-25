unit FHIR.Base.Factory;

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
  FHIR.Support.Objects, FHIR.Support.Generics,
  FHIR.Ucum.IFace,
  FHIR.Base.Objects, FHIR.Base.Parser, FHIR.Base.Validator, FHIR.Base.Narrative, FHIR.Base.PathEngine, FHIR.XVersion.Resources,
  FHIR.Client.Base{, FHIR.Client.Threaded{, FHIR.Client.HTTP};

type
  TFHIRFactory = class (TFslObject)
  public
    function link : TFHIRFactory; overload;
    function version : TFHIRVersion; virtual;
    function versionString : String; virtual;
    function description : String; virtual;

    function makeParser(worker : TFHIRWorkerContextV; format : TFHIRFormat; lang : String) : TFHIRParser; virtual;
    function makeComposer(worker : TFHIRWorkerContextV; format : TFHIRFormat; lang : String; style: TFHIROutputStyle) : TFHIRComposer; virtual;
    function makeValidator(worker : TFHIRWorkerContextV) : TFHIRValidatorV; virtual;
    function makeGenerator(worker : TFHIRWorkerContextV) : TFHIRNarrativeGeneratorBase; virtual;
    function makePathEngine(worker : TFHIRWorkerContextV; ucum : TUcumServiceInterface) : TFHIRPathEngineV; virtual;
    function createFromProfile(worker : TFHIRWorkerContextV; profile : TFhirStructureDefinitionW) : TFHIRResourceV; virtual;

    function makeClient(worker : TFHIRWorkerContextV; url : String; fmt : TFHIRFormat) : TFhirClientV; overload;
    function makeClient(worker : TFHIRWorkerContextV; url : String; kind : TFHIRClientType; fmt : TFHIRFormat) : TFhirClientV; overload;
    function makeClient(worker : TFHIRWorkerContextV; url : String; kind : TFHIRClientType; fmt : TFHIRFormat; timeout : cardinal) : TFhirClientV; overload;
    function makeClient(worker : TFHIRWorkerContextV; url : String; kind : TFHIRClientType; fmt : TFHIRFormat; timeout : cardinal; proxy : String) : TFhirClientV; overload; virtual; // because using indy is necessary if you're writing a server, or unixready code
    function makeClientThreaded(worker : TFHIRWorkerContextV; internal : TFhirClientV; event : TThreadManagementEvent) : TFhirClientV; overload; virtual;

    function makeByName(const name : String) : TFHIRObject; virtual;
    function makeBoolean(b : boolean): TFHIRObject; virtual;
    function makeCode(s : string) : TFHIRObject; virtual;
    function makeString(s : string) : TFHIRObject; virtual;
    function makeInteger(s : string) : TFHIRObject; virtual;
    function makeDecimal(s : string) : TFHIRObject; virtual;
    function makeBase64Binary(s : string) : TFHIRObject; virtual; // must DecodeBase64
    function makeParameters : TFHIRParametersW; virtual;

    function wrapCapabilityStatement(r : TFHIRResourceV) : TFHIRCapabilityStatementW; virtual;
    function wrapStructureDefinition(r : TFHIRResourceV) : TFhirStructureDefinitionW; virtual;
  end;

  TFHIRVersionFactories = class (TFslObject)
  private
    FVersionArray : array [TFHIRVersion] of TFHIRFactory;
    function getHasVersion(v: TFHIRVersion): boolean;
    function getVersion(v: TFHIRVersion): TFHIRFactory;
    procedure SetVersion(v: TFHIRVersion; const Value: TFHIRFactory);
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRVersionFactories; overload;

    property version[v : TFHIRVersion] : TFHIRFactory read getVersion write SetVersion; default;
    property hasVersion[v : TFHIRVersion] : boolean read getHasVersion;
  end;

  TFHIRWorkerContextWithFactory = class (TFHIRWorkerContextV)
  private
    FFactory : TFHIRFactory;
  public
    constructor Create(factory : TFHIRFactory); virtual;
    destructor Destroy; override;

    function link : TFHIRWorkerContextWithFactory;

    property Factory : TFHIRFactory read FFactory;

    procedure loadResourceJson(rType, id : String; json : TStream); override;
    procedure seeResource(res : TFHIRResourceV); virtual;

    function getResourceNames : TFslStringSet; virtual; abstract;
    function fetchResource(rType : String; url : String) : TFhirResourceV; virtual; abstract;
    function expand(vs : TFhirValueSetW) : TFHIRValueSetW; virtual; abstract;
    function supportsSystem(system, version : string) : boolean; virtual; abstract;
    function validateCode(system, version, code, display : String) : TValidationResult; overload; virtual; abstract;
    function validateCode(system, version, code : String; vs : TFhirValueSetW) : TValidationResult; overload; virtual; abstract;
    function allResourceNames : TArray<String>; virtual; abstract;
    function nonSecureResourceNames : TArray<String>; virtual; abstract;
    procedure listStructures(list : TFslList<TFhirStructureDefinitionW>); overload; virtual; abstract;
  end;

implementation

{ TFHIRFactory }

function TFHIRFactory.createFromProfile(worker: TFHIRWorkerContextV; profile: TFhirStructureDefinitionW): TFHIRResourceV;
begin
  raise Exception.Create('The implementation of TFHIRFactory.makePathEngine should never be called');
end;

function TFHIRFactory.description: String;
begin
  result := 'Unknown version';
end;

function TFHIRFactory.link: TFHIRFactory;
begin
  result := TFHIRFactory(inherited link);
end;

function TFHIRFactory.makeClient(worker : TFHIRWorkerContextV; url : String; fmt : TFHIRFormat) : TFhirClientV;
begin
  result := makeClient(worker, url, fctCrossPlatform, fmt, 0, '');
end;

function TFHIRFactory.makeClient(worker : TFHIRWorkerContextV; url : String; kind : TFHIRClientType; fmt : TFHIRFormat) : TFhirClientV;
begin
  result := makeClient(worker, url, kind, fmt, 0, '');
end;

function TFHIRFactory.makeClient(worker : TFHIRWorkerContextV; url : String; kind : TFHIRClientType; fmt : TFHIRFormat; timeout : cardinal) : TFhirClientV;
begin
  result := makeClient(worker, url, kind, fmt, timeout, '');
end;

function TFHIRFactory.makeClient(worker : TFHIRWorkerContextV; url : String; kind : TFHIRClientType; fmt : TFHIRFormat; timeout : cardinal; proxy : String) : TFhirClientV;
begin
  raise Exception.Create('The implementation of TFHIRFactory.makeClient should never be called');
end;

function TFHIRFactory.makeBase64Binary(s: string): TFHIRObject;
begin
  raise Exception.Create('The implementation of TFHIRFactory.makeBase64Binary should never be called');
end;

function TFHIRFactory.makeBoolean(b: boolean): TFHIRObject;
begin
  raise Exception.Create('The implementation of TFHIRFactory.makeBoolean should never be called');
end;

function TFHIRFactory.makeByName(const name: String): TFHIRObject;
begin
  raise Exception.Create('The implementation of TFHIRFactory.makeByName should never be called');
end;

function TFHIRFactory.makeClientThreaded(worker: TFHIRWorkerContextV; internal: TFhirClientV; event: TThreadManagementEvent): TFhirClientV;
begin
  raise Exception.Create('The implementation of TFHIRFactory.makeClientThreaded should never be called');
end;

function TFHIRFactory.makeCode(s: string): TFHIRObject;
begin
  raise Exception.Create('The implementation of TFHIRFactory.makeCode should never be called');
end;

function TFHIRFactory.makeComposer(worker: TFHIRWorkerContextV; format: TFHIRFormat; lang: String; style: TFHIROutputStyle): TFHIRComposer;
begin
  raise Exception.Create('The implementation of TFHIRFactory.makeComposer should never be called');
end;

function TFHIRFactory.makeDecimal(s: string): TFHIRObject;
begin
  raise Exception.Create('The implementation of TFHIRFactory.makeDecimal should never be called');
end;

function TFHIRFactory.makeGenerator(worker: TFHIRWorkerContextV): TFHIRNarrativeGeneratorBase;
begin
  raise Exception.Create('The implementation of TFHIRFactory.makeGenerator should never be called');
end;

function TFHIRFactory.makeInteger(s: string): TFHIRObject;
begin
  raise Exception.Create('The implementation of TFHIRFactory.makeInteger should never be called');
end;

function TFHIRFactory.makeParameters: TFHIRParametersW;
begin
  raise Exception.Create('The implementation of TFHIRFactory.makeParameters should never be called');
end;

function TFHIRFactory.makeParser(worker: TFHIRWorkerContextV; format: TFHIRFormat; lang: String): TFHIRParser;
begin
  raise Exception.Create('The implementation of TFHIRFactory.makeComposer should never be called');
end;

function TFHIRFactory.makePathEngine(worker: TFHIRWorkerContextV; ucum : TUcumServiceInterface): TFHIRPathEngineV;
begin
  raise Exception.Create('The implementation of TFHIRFactory.makePathEngine should never be called');
end;

function TFHIRFactory.makeString(s: string): TFHIRObject;
begin
  raise Exception.Create('The implementation of TFHIRFactory.makeString should never be called');
end;

function TFHIRFactory.makeValidator(worker: TFHIRWorkerContextV): TFHIRValidatorV;
begin
  raise Exception.Create('The implementation of TFHIRFactory.makeComposer should never be called');
end;

function TFHIRFactory.version: TFHIRVersion;
begin
  result := fhirVersionUnknown;
end;


function TFHIRFactory.versionString: String;
begin
  result := '??';
end;

function TFHIRFactory.wrapCapabilityStatement(r: TFHIRResourceV): TFHIRCapabilityStatementW;
begin
  raise Exception.Create('The implementation of TFHIRFactory.wrapCapabilityStatement should never be called');
end;

function TFHIRFactory.wrapStructureDefinition(r: TFHIRResourceV): TFhirStructureDefinitionW;
begin
  raise Exception.Create('The implementation of TFHIRFactory.wrapCapabilityStatement should never be called');
end;

{ TFHIRVersionFactories }

constructor TFHIRVersionFactories.Create;
var
  v : TFHIRVersion;
begin
  inherited;
  for v in FHIR_ALL_VERSIONS do
    FVersionArray[v] := nil;
end;

destructor TFHIRVersionFactories.Destroy;
var
  v : TFHIRVersion;
begin
  for v in FHIR_ALL_VERSIONS do
    FVersionArray[v].free;
  inherited;
end;

function TFHIRVersionFactories.getHasVersion(v: TFHIRVersion): boolean;
begin
  result := FVersionArray[v] <> nil;
end;

function TFHIRVersionFactories.getVersion(v: TFHIRVersion): TFHIRFactory;
begin
  result := FVersionArray[v];
end;

function TFHIRVersionFactories.link: TFHIRVersionFactories;
begin
  result := TFHIRVersionFactories(inherited link);
end;

procedure TFHIRVersionFactories.SetVersion(v: TFHIRVersion; const Value: TFHIRFactory);
begin
  FVersionArray[v].free;
  FVersionArray[v] := value;
end;

{ TFHIRWorkerContextWithFactory }

constructor TFHIRWorkerContextWithFactory.Create(factory: TFHIRFactory);
begin
  inherited Create;
  FFactory := factory;
end;

destructor TFHIRWorkerContextWithFactory.Destroy;
begin
  FFactory.link;
  inherited;
end;

function TFHIRWorkerContextWithFactory.link: TFHIRWorkerContextWithFactory;
begin
  result := TFHIRWorkerContextWithFactory(inherited link);
end;

procedure TFHIRWorkerContextWithFactory.loadResourceJson(rtype, id: String; json: TStream);
var
  p : TFHIRParser;
begin
  p := Factory.makeParser(self.link, ffJson, 'en');
  try
    p.source := json;
    p.Parse;
    SeeResource(p.resource);
  finally
    p.Free;
  end;
end;

procedure TFHIRWorkerContextWithFactory.seeResource(res: TFHIRResourceV);
begin
end;

end.
