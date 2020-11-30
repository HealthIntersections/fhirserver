unit fxver_conversion_engine;

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
  fsl_base, fsl_utilities, fsl_http, fsl_javascript,
  fhir_objects, fhir_factory, fhir_client, fhir_javascript,
  fhir4_client, fhir4_types, fhir4_resources, fhir4_resources_base, fhir4_utilities; // Terminology Layer

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
  pm : THTTPParameters;
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
      pm := THTTPParameters.Create(params);
      try
        if pm.has('date') then
          pIn.AddParameter('date', TFhirDateTime.Create(TFslDateTime.fromXML(pm['date'])));
        if pm.has('displayLanguage') then
          pIn.AddParameter('displayLanguage', TFhirCode.Create(pm['displayLanguage']));
        if pm.has('property') then
          for s in pm['property'].Split([';']) do
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
