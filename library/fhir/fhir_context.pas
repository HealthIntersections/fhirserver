unit fhir_context;

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

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_collections, fsl_threads, fsl_logging, fsl_http, fsl_npm, fsl_npm_cache,
  fhir_objects, fhir_parser, fhir_common, fhir_factory;

type
  TFHIRWorkerContextWithFactory = fhir_factory.TFHIRWorkerContextWithFactory;

  { TFHIRLoadContextTaskRequest }

  TFHIRLoadContextTaskRequest = class abstract (TBackgroundTaskRequestPackage)
   private
     FContext: TFHIRWorkerContextWithFactory;
     FIgnoreEmptyCodeSystems: boolean;
     FPackages: TStringList;
     procedure SetContext(AValue: TFHIRWorkerContextWithFactory);
   public
     constructor Create; override;
     destructor Destroy; override;

     property context : TFHIRWorkerContextWithFactory read FContext write SetContext;
     property packages : TStringList read FPackages;
     property ignoreEmptyCodeSystems : boolean read FIgnoreEmptyCodeSystems write FIgnoreEmptyCodeSystems;
   end;

   { TFHIRLoadContextTaskResponse }

   TFHIRLoadContextTaskResponse = class abstract (TBackgroundTaskResponsePackage)
   private
     FContext: TFHIRWorkerContextWithFactory;
     procedure SetContext(AValue: TFHIRWorkerContextWithFactory);
   public
     destructor Destroy; override;
     property context : TFHIRWorkerContextWithFactory read FContext write SetContext;
   end;

   { TFHIRContextLoaderEngine }

   TFHIRContextLoaderEngine = class (TBackgroundTaskEngine)
   private
     FNpm : TFHIRPackageManager;
     FBase, FStep : double;
     procedure loadPackage(context : TFHIRWorkerContextWithFactory; id : String; ignoreEmptyCodeSystems : boolean);
     procedure doProgress(state : String; c, t : integer);
     procedure loadResource(context : TFHIRWorkerContextWithFactory; res: TFHIRResourceV; ignoreEmptyCodeSystems: boolean);
   protected
     function canCancel : boolean; override;
   public
     destructor Destroy; override;

     function name : String; override;

     procedure execute(request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage); override;
   end;

var
  GContextLoaderTaskId : integer;

implementation

{ TFHIRLoadContextTaskResponse }

destructor TFHIRLoadContextTaskResponse.Destroy;
begin
  FContext.free;
  inherited Destroy;
end;

procedure TFHIRLoadContextTaskResponse.SetContext(AValue: TFHIRWorkerContextWithFactory);
begin
  FContext.free;
  FContext := AValue;
end;

{ TFHIRLoadContextTaskRequest }

constructor TFHIRLoadContextTaskRequest.Create;
begin
  inherited Create;
  FPackages := TStringList.create;
end;

destructor TFHIRLoadContextTaskRequest.Destroy;
begin
  FPackages.Free;
  FContext.Free;
  inherited Destroy;
end;

procedure TFHIRLoadContextTaskRequest.SetContext(AValue: TFHIRWorkerContextWithFactory);
begin
  FContext.Free;
  FContext := AValue;
end;

{ TFHIRContextLoaderEngine }

function TFHIRContextLoaderEngine.canCancel: boolean;
begin
  result := false;
end;

destructor TFHIRContextLoaderEngine.Destroy;
begin
  inherited Destroy;
end;

function TFHIRContextLoaderEngine.name: String;
begin
  Result := 'Package Loader';
end;

procedure TFHIRContextLoaderEngine.execute(request: TBackgroundTaskRequestPackage; response: TBackgroundTaskResponsePackage);
var
  req : TFHIRLoadContextTaskRequest;
  resp : TFHIRLoadContextTaskResponse;
  s : String;
begin
  req := request as TFHIRLoadContextTaskRequest;
  resp := response as TFHIRLoadContextTaskResponse;

  FBase := 0;
  FStep := 100 / req.packages.count;
  for s in req.packages do
  begin
    loadPackage(req.context, s, req.ignoreEmptyCodeSystems);
    FBase := FBase + FStep;
  end;
end;

procedure TFHIRContextLoaderEngine.loadResource(context : TFHIRWorkerContextWithFactory; res : TFHIRResourceV; ignoreEmptyCodeSystems : boolean);
var
  cs : TFhirCodeSystemW;
begin
  if res.fhirType = 'CodeSystem' then
  begin
    cs := context.factory.wrapCodeSystem(res.link);
    try
      if (not ignoreEmptyCodeSystems or (cs.content in [cscmFragment, cscmComplete, cscmSupplement])) then
        context.seeResource(res);
    finally
      cs.Free;
    end;
  end
  else
    context.seeResource(res);
end;

procedure TFHIRContextLoaderEngine.loadPackage(context : TFHIRWorkerContextWithFactory; id : String; ignoreEmptyCodeSystems : boolean);
var
  npm : TNpmPackage;
  s : String;
  res : TFHIRResourceV;
  i : integer;
  p : TFHIRParser;
  a : TArray<String>;
begin
  doProgress('Loading Package' +id, 0, 1);

  if (FNpm = nil) then
    FNpm := TFHIRPackageManager.Create(false);
  i := 0;

  p := context.factory.makeParser(context.Link, ffJson, defLang);
  try
    npm := FNpm.loadPackage(id);
    try
      context.Packages.Add(npm.name+'#'+npm.version);
      a := npm.listResources(['CodeSystem', 'ValueSet', 'NamingSystem', 'ConceptMap', 'StructureDefinition', 'StructureMap', 'CapabilityStatement', 'OperationDefinition']);
      for s in a do
      begin
        doProgress('Loading Package'+npm.name+'#'+npm.version, i, length(a));
        inc(i);
        try
          res := p.parseResource(npm.loadBytes(s));
        except
          on e : Exception do
            raise EFHIRException.Create('Error Parsing '+s+': '+e.Message);
        end;
        try
          try
            loadResource(context, res, ignoreEmptyCodeSystems);
          except
            on e : Exception do
              raise EFHIRException.Create('Error Loading '+s+': '+e.Message);
          end;
        finally
          res.Free;
        end;
      end;
      Logging.log('Loaded '+npm.name+'#'+npm.version+': '+inttostr(i)+' resources');
    finally
      npm.Free;
    end;
  finally
    p.Free;
  end;
  doProgress('Loading Package' +id, 1, 1);
end;

procedure TFHIRContextLoaderEngine.doProgress(state : String; c, t: integer);
begin
  progress(state, trunc(FBase + (FStep * (c / t))));
end;

initialization
  GContextLoaderTaskId := GBackgroundTasks.registerTaskEngine(TFHIRContextLoaderEngine.create);
end.

