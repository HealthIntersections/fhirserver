unit FHIR.Conversion.Engine;

interface

uses
  SysUtils, Classes,
  FHIR.Javascript,
  FHIR.Support.Base, FHIR.Support.Utilities,
  FHIR.Web.Parsers,
  FHIR.Base.Objects, FHIR.Base.Factory, FHIR.Client.Base,
  FHIR.R4.Client, FHIR.R4.Types, FHIR.R4.Resources, FHIR.R4.Utilities; // Terminology Layer

(*
function lookup(coded, params) : Parameters;
function translate(conceptMap, code, params) : Parameters;
function expand(valueSet, params) : ValueSet;
function validateVS(valueSet, coded, params) : Parameters;
function validateCS(codeSystem, coded, params) : Parameters;
function subsumes(system, coded1, coded2) : code;
From the Mapping API:

function translateCode(code, srcSystem, dstSystem) : String;
Additional Conversion support:

function factory(typeName) : Object;
function runJS(scriptName, routineName, params....) : Object | void;
function runMap(url, source[, target], callBacks...)
function runLiquid(fileName, source, type[, format]) : Object;
function runMarkdown(fileName, source) : Object;
function translateUri((value, type)) : String;
function translateDate(date, srcFmt, dstFmt) : String;
*)

type
  TFHIRConversionEngine = class (TFslObject)
  private
    FWorker : TFHIRWorkerContextWithFactory;
    FTerminologyServer : TFhirClient4;
    FAPI : TFhirClientV;

    function svcLookup(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
  public
    constructor Create(worker : TFHIRWorkerContextWithFactory; txServer : TFhirClient4; api : TFhirClientV);
    destructor Destroy; override;

    class procedure registerConversionEngine(js : TJavascript; worker : TFHIRWorkerContextWithFactory);
  end;

implementation

{ TFHIRConversionEngine }

constructor TFHIRConversionEngine.Create(worker: TFHIRWorkerContextWithFactory; txServer : TFhirClient4; api : TFhirClientV);
begin
  Inherited Create;
  FWorker := worker;
  FTerminologyServer := txServer;
  FAPI := api;
end;

destructor TFHIRConversionEngine.Destroy;
begin
  FTerminologyServer.Free;
  FAPI.Free;
  FWorker.Free;
  inherited;
end;

class procedure TFHIRConversionEngine.registerConversionEngine(js: TJavascript; worker: TFHIRWorkerContextWithFactory);
begin
  // nothing yet
end;

function TFHIRConversionEngine.svcLookup(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
var
  coding : TFHIRCoding;
  params, s : String;
  pIn, pOut : TFhirParameters;
  p : TFhirParametersParameter;
  pm : TParseMap;
begin
  coding := nil;
  try
  //function lookup(coding, params) : Parameters;
  //function lookup(system, code, params) : Parameters;
  //function lookup(system, version, code, params) : Parameters;
    case Length(Parameters) of
      2: begin
         coding := js.getWrapped<TFHIRCoding>(parameters[0]);
         params := js.asString(parameters[1]);
         end;
      3: begin
         coding := TFhirCoding.Create;
         coding.system := js.asString(parameters[0]);
         coding.code := js.asString(parameters[1]);
         params := js.asString(parameters[2]);
         end;
      4: begin
         coding := TFhirCoding.Create;
         coding.system := js.asString(parameters[0]);
         coding.code := js.asString(parameters[1]);
         params := js.asString(parameters[2]);
         end;
    else
      raise EJavascriptSource.Create('Wrong number of parameters to lookup - must be 2 - 4');
    end;
    pIn := TFhirParameters.Create;
    try
      pIn.AddParameter('coding', coding.Link);
      pm := TParseMap.Create(params);
      try
        if pm.has('date') then
          pIn.AddParameter('date', TFhirDateTime.Create(TFslDateTime.fromXML(pm.GetVar('date'))));
        if pm.has('displayLanguage') then
          pIn.AddParameter('displayLanguage', TFhirCode.Create(pm.GetVar('displayLanguage')));
        if pm.has('property') then
          for s in pm.GetVar('property').Split([';']) do
            pIn.AddParameter('displayLanguage', TFhirCode.Create(s));
        pOut := FTerminologyServer.operation(frtCodeSystem, 'lookup', pIn) as TFhirParameters;
        try
          result := js.wrap(pOut.Link, true);
        finally
          pOut.Free;
        end;
      finally
        pm.Free;
      end;
    finally
      pIn.Free;
    end;
  finally
    coding.free;
  end;
end;

end.
