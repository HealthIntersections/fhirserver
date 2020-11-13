unit event_js;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$I fhir.inc}

{$IFDEF NO_JS}
This should not be included when NO_JS is defined
{$ENDIF}

interface

uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_http,
  fsl_javascript,
  fhir_objects, 
  session;

type
  TFHIRServerJSHelper = class (TFslObject)
  private
    function FHIRRequestGetBaseUrlJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetCommandTypeJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetCompartmentJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetElementsJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetExternalRequestIdJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetIdJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetIfMatchJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetIfModifiedSinceJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetIfNoneExistJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetIfNoneMatchJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetInternalRequestIdJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetLangJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetLastModifiedJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetOperationNameJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetProvenanceJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetResourceJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetResourceTypeJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetSecureJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetStrictSearchJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetSummaryJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetTagsJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetUrlJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    function FHIRRequestGetVersionIdJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    procedure FHIRRequestSetBaseUrlJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetCommandTypeJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetCompartmentJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetElementsJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetExternalRequestIdJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetIdJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetIfMatchJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetIfModifiedSinceJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetIfNoneExistJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetIfNoneMatchJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetInternalRequestIdJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetLangJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetLastModifiedJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetOperationNameJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetProvenanceJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetResourceJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetResourceTypeJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetSecureJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetStrictSearchJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetSummaryJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetTagsJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetUrlJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    procedure FHIRRequestSetVersionIdJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    function FHIRSessionCanReadJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRSessionCanWriteJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRSessionCompartmentsJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRSessionCreatedJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRSessionEmailJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRSessionExpiresJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRSessionIdJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRSessionIsAnonymousJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRSessionProviderJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRSessionScopesJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRSessionSessionNameJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRSessionSystemEvidenceJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRSessionSystemNameJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRSessionUserEvidenceJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRSessionUserKeyJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function FHIRSessionUserNameJs(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
  public
    class procedure registerFHIRServerEvent(js : TJavascript);
  end;

implementation

function TFHIRServerJSHelper.FHIRSessionProviderJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(Names_TFHIRAuthProvider[TFhirSession(this).ProviderCode]);
end;

function TFHIRServerJSHelper.FHIRSessionIdJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirSession(this).Id);
end;

function TFHIRServerJSHelper.FHIRSessionUserKeyJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirSession(this).UserKey);
end;

function TFHIRServerJSHelper.FHIRSessionUserNameJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirSession(this).UserName);
end;

function TFHIRServerJsHelper.FHIRSessionUserEvidenceJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(CODES_UserIdEvidence[TFhirSession(this).UserEvidence]);
end;

function TFHIRServerJsHelper.FHIRSessionSystemNameJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirSession(this).SystemName);
end;

function TFHIRServerJsHelper.FHIRSessionSystemEvidenceJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(CODES_SystemIdEvidence[TFhirSession(this).SystemEvidence]);
end;

function TFHIRServerJsHelper.FHIRSessionSessionNameJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirSession(this).SessionName);
end;

function TFHIRServerJsHelper.FHIRSessionEmailJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirSession(this).Email);
end;

function TFHIRServerJsHelper.FHIRSessionExpiresJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFslDateTime.make(TFhirSession(this).Expires, dttzUTC).toXML);
end;

function TFHIRServerJsHelper.FHIRSessionCreatedJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFslDateTime.make(TFhirSession(this).FirstCreated, dttzUTC).toXML);
end;

function TFHIRServerJsHelper.FHIRSessionCanReadJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirSession(this).canRead(js.asString(parameters[1])));
end;

function TFHIRServerJsHelper.FHIRSessionCanWriteJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirSession(this).canWrite(js.asString(parameters[1])));
end;

function TFHIRServerJsHelper.FHIRSessionCompartmentsJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  {$IFDEF FPC}
  raise EFHIRException.create('todo');
  {$ELSE}
  result := js.makeArray(TFhirSession(this).Compartments.Count, function (js : TJavascript; context : pointer; i : integer) : JsValueRef
     begin
       result := js.wrap(TFhirSession(this).Compartments[i].Id);
     end, nil);
  {$ENDIF}
end;

function TFHIRServerJsHelper.FHIRSessionScopesJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  sl : TArray<String>;
begin
  {$IFDEF FPC}
  raise EFHIRException.create('todo');
  {$ELSE}
  sl := TFhirSession(this).scopes.Split([' ']);
  result := js.makeArray(length(sl), function (js : TJavascript; context : pointer; i : integer) : JsValueRef
     begin
       result := js.wrap(sl[i]);
     end, nil);
  {$ENDIF}
end;

function TFHIRServerJsHelper.FHIRSessionIsAnonymousJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirSession(this).isAnonymous);
end;


function TFHIRServerJsHelper.FHIRRequestGetUrlJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).url);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetUrlJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).url := js.asString(value);
end;

function TFHIRServerJsHelper.FHIRRequestGetSecureJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).secure);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetSecureJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).secure := js.asBoolean(value);
end;

function TFHIRServerJsHelper.FHIRRequestGetBaseUrlJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).baseUrl);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetBaseUrlJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).baseUrl := js.asString(value);
end;

function TFHIRServerJsHelper.FHIRRequestGetResourceTypeJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).ResourceName);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetResourceTypeJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).ResourceName := js.asString(value);
end;

function TFHIRServerJsHelper.FHIRRequestGetCommandTypeJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(CODES_TFHIRCommandType[TFHIRRequest(this).CommandType]);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetCommandTypeJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  i : integer;
begin
  i := StringArrayIndexOfSensitive(CODES_TFHIRCommandType, js.asString(value));
  if i = -1 then
    raise EFHIRException.create('Unknown code '+js.asString(value));
  TFHIRRequest(this).CommandType := TFHIRCommandType(i);
end;

function TFHIRServerJsHelper.FHIRRequestGetIdJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).Id);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetIdJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).Id := js.asString(value);
end;

function TFHIRServerJsHelper.FHIRRequestGetVersionIdJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).SubId);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetVersionIdJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).SubId := js.asString(value);
end;

function TFHIRServerJsHelper.FHIRRequestGetOperationNameJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).OperationName);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetOperationNameJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).OperationName := js.asString(value);
end;

function TFHIRServerJsHelper.FHIRRequestGetResourceJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  if TFHIRRequest(this).Resource = nil then
    result := js.getNull
  else
    result := js.wrap(TFHIRRequest(this).Resource.Link, TFHIRRequest(this).Resource.fhirType, true);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetResourceJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  raise EFHIRException.create('todo');
end;

function TFHIRServerJsHelper.FHIRRequestGetTagsJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  raise EFHIRException.create('todo');
  // todo: make this a manager
//  result := js.makeArray(TFHIRRequest(this).Tags.Count, function (i : integer) : JsValueRef
//     begin
//       result := js.wrap(TFHIRRequest(this).Tags[i].asCoding, 'Coding', true);
//     end);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetTagsJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  raise EFHIRException.create('todo');
end;

function TFHIRServerJsHelper.FHIRRequestGetElementsJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.makeManagedArray(TStringListManager.create(TFHIRRequest(this).Elements));
end;

procedure TFHIRServerJsHelper.FHIRRequestSetElementsJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  raise EFHIRException.create('todo');
end;

function TFHIRServerJsHelper.FHIRRequestGetCompartmentJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  raise EFHIRException.create('todo');
  //  := js.makeManagedArray(TStringListManager.create(TFHIRRequest(this).SessionCompartments));
end;

procedure TFHIRServerJsHelper.FHIRRequestSetCompartmentJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  raise EFHIRException.create('todo');
end;

function TFHIRServerJsHelper.FHIRRequestGetLastModifiedJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFslDateTime.make(TFHIRRequest(this).lastModifiedDate, dttzUTC).toXML);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetLastModifiedJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).lastModifiedDate := TFslDateTime.fromXML(js.asString(value)).makeUTC.DateTime;
end;

function TFHIRServerJsHelper.FHIRRequestGetLangJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).Lang.header);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetLangJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).Lang := THTTPLanguages.create(js.asString(value));
end;

function TFHIRServerJsHelper.FHIRRequestGetSummaryJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(CODES_TFHIRSummaryOption[TFHIRRequest(this).Summary]);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetSummaryJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  i : integer;
begin
  i := StringArrayIndexOfSensitive(CODES_TFHIRSummaryOption, js.asString(value));
  if i = -1 then
    raise EFHIRException.create('Unknown code '+js.asString(value));
  TFHIRRequest(this).CommandType := TFHIRCommandType(i);
end;

function TFHIRServerJsHelper.FHIRRequestGetIfMatchJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).IfMatch);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetIfMatchJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).IfMatch := js.asString(value);
end;

function TFHIRServerJsHelper.FHIRRequestGetIfNoneMatchJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).IfNoneMatch);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetIfNoneMatchJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).IfNoneMatch := js.asString(value);
end;

function TFHIRServerJsHelper.FHIRRequestGetIfModifiedSinceJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFslDateTime.make(TFHIRRequest(this).IfModifiedSince, dttzUTC).toXML);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetIfModifiedSinceJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).IfModifiedSince := TFslDateTime.fromXML(js.asString(value)).makeUTC.DateTime;
end;

function TFHIRServerJsHelper.FHIRRequestGetIfNoneExistJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).IfNoneExist);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetIfNoneExistJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).IfNoneExist := js.asString(value);
end;

function TFHIRServerJsHelper.FHIRRequestGetProvenanceJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  if TFHIRRequest(this).Provenance = nil then
    result := js.getNull
  else
    result := js.wrap(TFHIRRequest(this).Provenance.link, TFHIRRequest(this).Provenance.fhirType, true);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetProvenanceJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  raise EFHIRException.create('todo');
end;

function TFHIRServerJsHelper.FHIRRequestGetExternalRequestIdJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).externalRequestId);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetExternalRequestIdJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).externalRequestId := js.asString(value);
end;

function TFHIRServerJsHelper.FHIRRequestGetInternalRequestIdJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).internalRequestId);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetInternalRequestIdJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).internalRequestId := js.asString(value);
end;

function TFHIRServerJsHelper.FHIRRequestGetStrictSearchJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).strictSearch);
end;

procedure TFHIRServerJsHelper.FHIRRequestSetStrictSearchJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).strictSearch := js.asBoolean(value);
end;


class procedure TFHIRServerJsHelper.registerFHIRServerEvent(js : TJavascript);
var
  def : TJavascriptClassDefinition;
  this : TFHIRServerJSHelper;
begin
  this := TFHIRServerJSHelper.Create;
  js.ownObject(this);
  def := js.defineClass('Session', nil);
  def.defineRoutine('provider', nil, this.FHIRSessionProviderJs);
  def.defineRoutine('id', nil, this.FHIRSessionIdJs);
  def.defineRoutine('userKey', nil, this.FHIRSessionUserKeyJs);
  def.defineRoutine('userName', nil, this.FHIRSessionUserNameJs);
  def.defineRoutine('userEvidence', nil, this.FHIRSessionUserEvidenceJs);
  def.defineRoutine('systemName', nil, this.FHIRSessionSystemNameJs);
  def.defineRoutine('systemEvidence', nil, this.FHIRSessionSystemEvidenceJs);
  def.defineRoutine('sessionName', nil, this.FHIRSessionSessionNameJs);
  def.defineRoutine('email', nil, this.FHIRSessionEmailJs);
  def.defineRoutine('expires', nil, this.FHIRSessionExpiresJs);
  def.defineRoutine('created', nil, this.FHIRSessionCreatedJs);
  def.defineRoutine('canRead', nil, this.FHIRSessionCanReadJs);
  def.defineRoutine('canWrite', nil, this.FHIRSessionCanWriteJs);
  def.defineRoutine('compartments', nil, this.FHIRSessionCompartmentsJs);
  def.defineRoutine('scopes', nil, this.FHIRSessionScopesJs);
  def.defineRoutine('isAnonymous', nil, this.FHIRSessionIsAnonymousJs);

  def := js.defineClass('Event', nil);
  def.defineProperty('url', nil, this.FHIRRequestGetUrlJs, this.FHIRRequestSetUrlJs);
  def.defineProperty('secure', nil, this.FHIRRequestGetSecureJs, this.FHIRRequestSetSecureJs);
  def.defineProperty('baseUrl', nil, this.FHIRRequestGetBaseUrlJs, this.FHIRRequestSetBaseUrlJs);
  def.defineProperty('resourceType', nil, this.FHIRRequestGetResourceTypeJs, this.FHIRRequestSetResourceTypeJs);
  def.defineProperty('commandType', nil, this.FHIRRequestGetCommandTypeJs, this.FHIRRequestSetCommandTypeJs);
  def.defineProperty('id', nil, this.FHIRRequestGetIdJs, this.FHIRRequestSetIdJs);
  def.defineProperty('versionId', nil, this.FHIRRequestGetVersionIdJs, this.FHIRRequestSetVersionIdJs);
  def.defineProperty('operationName', nil, this.FHIRRequestGetOperationNameJs, this.FHIRRequestSetOperationNameJs);
  def.defineProperty('resource', nil, this.FHIRRequestGetResourceJs, this.FHIRRequestSetResourceJs);
  def.defineProperty('tags', nil, this.FHIRRequestGetTagsJs, this.FHIRRequestSetTagsJs);
  def.defineProperty('elements', nil, this.FHIRRequestGetElementsJs, this.FHIRRequestSetElementsJs);
  def.defineProperty('compartment', nil, this.FHIRRequestGetCompartmentJs, this.FHIRRequestSetCompartmentJs);
  def.defineProperty('lastModified', nil, this.FHIRRequestGetLastModifiedJs, this.FHIRRequestSetLastModifiedJs);
  def.defineProperty('lang', nil, this.FHIRRequestGetLangJs, this.FHIRRequestSetLangJs);
  def.defineProperty('summary', nil, this.FHIRRequestGetSummaryJs, this.FHIRRequestSetSummaryJs);
  def.defineProperty('ifMatch', nil, this.FHIRRequestGetIfMatchJs, this.FHIRRequestSetIfMatchJs);
  def.defineProperty('ifNoneMatch', nil, this.FHIRRequestGetIfNoneMatchJs, this.FHIRRequestSetIfNoneMatchJs);
  def.defineProperty('ifModifiedSince', nil, this.FHIRRequestGetIfModifiedSinceJs, this.FHIRRequestSetIfModifiedSinceJs);
  def.defineProperty('ifNoneExist', nil, this.FHIRRequestGetIfNoneExistJs, this.FHIRRequestSetIfNoneExistJs);
  def.defineProperty('provenance', nil, this.FHIRRequestGetProvenanceJs, this.FHIRRequestSetProvenanceJs);
  def.defineProperty('externalRequestId', nil, this.FHIRRequestGetExternalRequestIdJs, this.FHIRRequestSetExternalRequestIdJs);
  def.defineProperty('internalRequestId', nil, this.FHIRRequestGetInternalRequestIdJs, this.FHIRRequestSetInternalRequestIdJs);
  def.defineProperty('strictSearch', nil, this.FHIRRequestGetStrictSearchJs, this.FHIRRequestSetStrictSearchJs);
end;

end.
