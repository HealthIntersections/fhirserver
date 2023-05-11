unit FHIR.Npp.Context;


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
  SysUtils, Generics.Collections,
  fsl_base,
  fsl_npm_cache,
  fhir_objects, 
  fhir_parser, fhir_validator, fhir_narrative, fhir_utilities,
  fhir_factory, fhir_pathengine, fhir_client, fhir_common;

type
  TFHIRNppContext = class;

  TFHIRVersionLoadingStatus = (vlsNotSupported, vlsNotLoaded, vlsLoading, vlsLoadingFailed, vlsLoaded);

  TFHIRNppVersionFactory = class (TFslObject)
  private
    FContext : TFHIRNppContext; // no link
    FWorker : TFHIRWorkerContextWithFactory;
    FFactory : TFHIRFactory;
    FSource: string;
    FError: String;
    FEngine: TFHIRPathEngineV;
    procedure SetWorker(const Value: TFHIRWorkerContextWithFactory);
  public
    constructor Create(factory : TFHIRFactory);
    destructor Destroy; override;
    function link : TFHIRNppVersionFactory; overload;

    property Worker : TFHIRWorkerContextWithFactory read FWorker write SetWorker;
    property Factory : TFHIRFactory read FFactory;
    property Engine : TFHIRPathEngineV read FEngine;

    property source : string read FSource write FSource;
    property error : String read FError write FError;

    function makeParser(fmt : TFHIRFormat) : TFHIRParser;
    function makeComposer(fmt : TFHIRFormat) : TFHIRComposer;
    function makeValidator : TFHIRValidatorV;
    function makeNarrative : TFHIRNarrativeGeneratorBase;
    function makePathEngine : TFHIRPathEngineV;
  end;

  TFHIRNppServerConnection = class (TFslObject)
  private
    FClient: TFhirClientV;
    FStatement: TFHIRCapabilityStatementW;
    procedure SetClient(const Value: TFhirClientV);
    procedure SetStatement(const Value: TFHIRCapabilityStatementW);
  public
    destructor Destroy; override;
    function link : TFHIRNppServerConnection; overload;

    property client : TFhirClientV read FClient write SetClient;
    property statement : TFHIRCapabilityStatementW read FStatement write SetStatement;
  end;

  TFHIRNppContext = class (TFslObject)
  private
    FVersionList : Array[TFHIRVersion] of TFHIRNppVersionFactory;
    FVersionStatus : Array[TFHIRVersion] of TFHIRVersionLoadingStatus;
    FLoadInfoList : Array[TFHIRVersion] of TPackageLoadingInformation;
    FVersions: TFHIRVersionFactories;
    FCache : TFHIRPackageManager;
    FConnections: TFslMap<TFHIRNppServerConnection>;

    function GetVersion(a: TFHIRVersion): TFHIRNppVersionFactory;
    procedure SetVersion(a: TFHIRVersion; const Value: TFHIRNppVersionFactory);
    function GetVersionLoading(a: TFHIRVersion): TFHIRVersionLoadingStatus;
    procedure SetVersionLoading(a: TFHIRVersion; const Value: TFHIRVersionLoadingStatus);
    function GetLoadInfo(a: TFHIRVersion): TPackageLoadingInformation;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TFHIRNppContext;

    property Version[a : TFHIRVersion] : TFHIRNppVersionFactory read GetVersion write SetVersion;
    property LoadInfo[a : TFHIRVersion] : TPackageLoadingInformation read GetLoadInfo;
    property VersionLoading[a : TFHIRVersion] : TFHIRVersionLoadingStatus read GetVersionLoading write SetVersionLoading;

    property versions : TFHIRVersionFactories read FVersions;
    function versionInfo : String;

    property connections : TFslMap<TFHIRNppServerConnection> read FConnections;

    property Cache : TFHIRPackageManager read FCache;
  end;

const
  CODES_TFHIRVersionLoadingStatus : array [TFHIRVersionLoadingStatus] of String = ('Not Supported', 'Not Loaded', 'Loading', 'Loading Failed', 'Loaded');

implementation

{ TFHIRNppContext }

constructor TFHIRNppContext.Create;
var
  v : TFHIRVersion;
begin
  inherited;
  FConnections := TFslMap<TFHIRNppServerConnection>.create('Connections');
  FVersions := TFHIRVersionFactories.Create;

  for v := low(TFHIRVersion) to High(TFHIRVersion) do
  begin
    FVersionList[v] := nil;
    VersionLoading[v] := vlsNotSupported;
  end;
  FCache := TFHIRPackageManager.Create(true);
end;

destructor TFHIRNppContext.Destroy;
var
  v : TFHIRVersion;
begin
  FCache.Free;
  for v := low(TFHIRVersion) to High(TFHIRVersion) do
  begin
    FVersionList[v].Free;
    FLoadInfoList[v].free;
  end;
  FVersions.Free;
  FConnections.Free;
  inherited;
end;

function TFHIRNppContext.GetLoadInfo(a: TFHIRVersion): TPackageLoadingInformation;
begin
  if FLoadInfoList[a] = nil then
    FLoadInfoList[a] := TPackageLoadingInformation.Create(TFHIRVersions.getMajMin(a));
  result := FLoadInfoList[a];
end;

function TFHIRNppContext.GetVersion(a: TFHIRVersion): TFHIRNppVersionFactory;
begin
  if VersionLoading[a] <> vlsLoaded then
    raise EFHIRException.create('Version not loaded (yet?)');
  result := FVersionList[a];
end;

function TFHIRNppContext.GetVersionLoading(a: TFHIRVersion): TFHIRVersionLoadingStatus;
begin
 result := FVersionStatus[a];
end;

function TFHIRNppContext.Link: TFHIRNppContext;
begin
  result := TFHIRNppContext(inherited link);
end;

procedure TFHIRNppContext.SetVersion(a: TFHIRVersion; const Value: TFHIRNppVersionFactory);
begin
  if (a = fhirVersionUnknown) then
    raise EFHIRException.create('Illegal Version');
  value.FContext := self;
  FVersionList[a] := value;
  FVersions[a] := value.FFactory.link;
end;


procedure TFHIRNppContext.SetVersionLoading(a: TFHIRVersion; const Value: TFHIRVersionLoadingStatus);
begin
  if (a = fhirVersionUnknown) and (value <> vlsNotSupported) then
    raise EFHIRException.create('Illegal Version');
  FVersionStatus[a] := value;
end;

function TFHIRNppContext.versionInfo: String;
var
  v : TFHIRVersion;
begin
  result := '';
  for v := fhirVersionRelease2 to fhirVersionRelease4 do
    case VersionLoading[v] of
      vlsNotSupported: ; // nothing
      vlsNotLoaded :
        result := result + 'R'+CODES_TFHIRVersion[v]+': Turned off'+#13#10;
      vlsLoading :
        result := result + 'R'+CODES_TFHIRVersion[v]+': Loading'+#13#10;
      vlsLoadingFailed : result := result + 'R'+CODES_TFHIRVersion[v]+': Loading from '+Version[v].source+' failed: '+Version[v].error+#13#10;
      vlsLoaded :  result := result + 'R'+CODES_TFHIRVersion[v]+': Loaded from '+Version[v].source+#13#10;
    end;
end;

{ TFHIRNppVersionFactory }

function TFHIRNppVersionFactory.makeComposer(fmt: TFHIRFormat): TFHIRComposer;
begin
  result := FFactory.makeComposer(FWorker.link, fmt, FWorker.lang, OutputStylePretty);
end;

function TFHIRNppVersionFactory.makeNarrative: TFHIRNarrativeGeneratorBase;
begin
  result := FFactory.makeGenerator(FWorker.link);
end;

constructor TFHIRNppVersionFactory.Create(factory : TFHIRFactory);
begin
  inherited create;
  FFactory := factory;
end;

destructor TFHIRNppVersionFactory.Destroy;
begin
  FEngine.Free;
  FFactory.Free;
  FWorker.Free;
  inherited;
end;

function TFHIRNppVersionFactory.link: TFHIRNppVersionFactory;
begin
  result := TFHIRNppVersionFactory(inherited link);
end;

function TFHIRNppVersionFactory.makeParser(fmt: TFHIRFormat): TFHIRParser;
begin
  result := FFactory.makeParser(FWorker.link, fmt, FWorker.lang);
end;

function TFHIRNppVersionFactory.makePathEngine: TFHIRPathEngineV;
begin
  result := FFactory.makePathEngine(FWorker.link, nil);
end;

procedure TFHIRNppVersionFactory.SetWorker(const Value: TFHIRWorkerContextWithFactory);
begin
  FWorker.Free;
  FWorker := Value;
  FEngine.Free;
  FEngine := Factory.makePathEngine(FWorker.link, nil);
end;

function TFHIRNppVersionFactory.makeValidator: TFHIRValidatorV;
begin
  result := FFactory.makeValidator(FWorker.link);
end;

{ TFHIRNppServerConnection }

destructor TFHIRNppServerConnection.Destroy;
begin
  FClient.Free;
  FStatement.Free;
  inherited;
end;

function TFHIRNppServerConnection.link: TFHIRNppServerConnection;
begin
  result := TFHIRNppServerConnection(inherited link);
end;

procedure TFHIRNppServerConnection.SetClient(const Value: TFhirClientV);
begin
  FClient.Free;
  FClient := Value;
end;

procedure TFHIRNppServerConnection.SetStatement(const Value: TFHIRCapabilityStatementW);
begin
  FStatement.Free;
  FStatement := Value;
end;

end.
