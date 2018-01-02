unit ServerEventJs;

interface

uses
  SysUtils, Classes,
  DateSupport, StringSupport,
  Javascript,
  FHIRBase, FHIRResources, FHIRClient, FHIRUtilities, FHIRSupport;

procedure registerFHIRServerEvent(js : TJavascript);

implementation

function FHIRSessionProviderJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(Names_TFHIRAuthProvider[TFhirSession(this).ProviderCode]);
end;

function FHIRSessionIdJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirSession(this).Id);
end;

function FHIRSessionUserKeyJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirSession(this).UserKey);
end;

function FHIRSessionUserNameJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirSession(this).UserName);
end;

function FHIRSessionUserEvidenceJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(CODES_UserIdEvidence[TFhirSession(this).UserEvidence]);
end;

function FHIRSessionSystemNameJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirSession(this).SystemName);
end;

function FHIRSessionSystemEvidenceJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(CODES_SystemIdEvidence[TFhirSession(this).SystemEvidence]);
end;

function FHIRSessionSessionNameJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirSession(this).SessionName);
end;

function FHIRSessionEmailJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirSession(this).Email);
end;

function FHIRSessionExpiresJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TDateTimeEx.make(TFhirSession(this).Expires, dttzUTC).toXML);
end;

function FHIRSessionCreatedJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TDateTimeEx.make(TFhirSession(this).FirstCreated, dttzUTC).toXML);
end;

function FHIRSessionCanReadJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirSession(this).canRead(js.asString(parameters[1])));
end;

function FHIRSessionCanWriteJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirSession(this).canWrite(js.asString(parameters[1])));
end;

function FHIRSessionCompartmentsJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.makeArray(TFhirSession(this).Compartments.Count, function (i : integer) : JsValueRef
     begin
       result := js.wrap(TFhirSession(this).Compartments[i].Id);
     end);
end;

function FHIRSessionScopesJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
var
  sl : TArray<String>;
begin
  sl := TFhirSession(this).scopes.Split([' ']);
  result := js.makeArray(length(sl), function (i : integer) : JsValueRef
     begin
       result := js.wrap(sl[i]);
     end);
end;

function FHIRSessionIsAnonymousJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; parameters : TJsValues ) : JsValueRef;
begin
  result := js.wrap(TFhirSession(this).isAnonymous);
end;


function FHIRRequestGetUrlJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).url);
end;

procedure FHIRRequestSetUrlJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).url := js.asString(value);
end;

function FHIRRequestGetSecureJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).secure);
end;

procedure FHIRRequestSetSecureJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).secure := js.asBoolean(value);
end;

function FHIRRequestGetBaseUrlJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).baseUrl);
end;

procedure FHIRRequestSetBaseUrlJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).baseUrl := js.asString(value);
end;

function FHIRRequestGetResourceTypeJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).ResourceName);
end;

procedure FHIRRequestSetResourceTypeJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).ResourceName := js.asString(value);
end;

function FHIRRequestGetCommandTypeJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(CODES_TFHIRCommandType[TFHIRRequest(this).CommandType]);
end;

procedure FHIRRequestSetCommandTypeJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  i : integer;
begin
  i := StringArrayIndexOfSensitive(CODES_TFHIRCommandType, js.asString(value));
  if i = -1 then
    raise Exception.Create('Unknown code '+js.asString(value));
  TFHIRRequest(this).CommandType := TFHIRCommandType(i);
end;

function FHIRRequestGetIdJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).Id);
end;

procedure FHIRRequestSetIdJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).Id := js.asString(value);
end;

function FHIRRequestGetVersionIdJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).SubId);
end;

procedure FHIRRequestSetVersionIdJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).SubId := js.asString(value);
end;

function FHIRRequestGetOperationNameJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).OperationName);
end;

procedure FHIRRequestSetOperationNameJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).OperationName := js.asString(value);
end;

function FHIRRequestGetResourceJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  if TFHIRRequest(this).Resource = nil then
    result := js.getNull
  else
    result := js.wrap(TFHIRRequest(this).Resource.Link, TFHIRRequest(this).Resource.fhirType, true);
end;

procedure FHIRRequestSetResourceJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  raise Exception.Create('todo');
end;

function FHIRRequestGetTagsJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  raise Exception.Create('todo');
  // todo: make this a manager
//  result := js.makeArray(TFHIRRequest(this).Tags.Count, function (i : integer) : JsValueRef
//     begin
//       result := js.wrap(TFHIRRequest(this).Tags[i].asCoding, 'Coding', true);
//     end);
end;

procedure FHIRRequestSetTagsJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  raise Exception.Create('todo');
end;

function FHIRRequestGetElementsJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.makeManagedArray(TStringListManager.create(TFHIRRequest(this).Elements));
end;

procedure FHIRRequestSetElementsJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  raise Exception.Create('todo');
end;

function FHIRRequestGetCompartmentJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  raise Exception.Create('todo');
  //  := js.makeManagedArray(TStringListManager.create(TFHIRRequest(this).SessionCompartments));
end;

procedure FHIRRequestSetCompartmentJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  raise Exception.Create('todo');
end;

function FHIRRequestGetLastModifiedJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TDateTimeEx.make(TFHIRRequest(this).lastModifiedDate, dttzUTC).toXML);
end;

procedure FHIRRequestSetLastModifiedJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).lastModifiedDate := TDateTimeEx.fromXML(js.asString(value)).makeUTC.DateTime;
end;

function FHIRRequestGetLangJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).Lang);
end;

procedure FHIRRequestSetLangJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).Lang := js.asString(value);
end;

function FHIRRequestGetSummaryJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(CODES_TFHIRSummaryOption[TFHIRRequest(this).Summary]);
end;

procedure FHIRRequestSetSummaryJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
var
  i : integer;
begin
  i := StringArrayIndexOfSensitive(CODES_TFHIRSummaryOption, js.asString(value));
  if i = -1 then
    raise Exception.Create('Unknown code '+js.asString(value));
  TFHIRRequest(this).CommandType := TFHIRCommandType(i);
end;

function FHIRRequestGetIfMatchJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).IfMatch);
end;

procedure FHIRRequestSetIfMatchJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).IfMatch := js.asString(value);
end;

function FHIRRequestGetIfNoneMatchJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).IfNoneMatch);
end;

procedure FHIRRequestSetIfNoneMatchJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).IfNoneMatch := js.asString(value);
end;

function FHIRRequestGetIfModifiedSinceJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TDateTimeEx.make(TFHIRRequest(this).IfModifiedSince, dttzUTC).toXML);
end;

procedure FHIRRequestSetIfModifiedSinceJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).IfModifiedSince := TDateTimeEx.fromXML(js.asString(value)).makeUTC.DateTime;
end;

function FHIRRequestGetIfNoneExistJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).IfNoneExist);
end;

procedure FHIRRequestSetIfNoneExistJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).IfNoneExist := js.asString(value);
end;

function FHIRRequestGetProvenanceJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  if TFHIRRequest(this).Provenance = nil then
    result := js.getNull
  else
    result := js.wrap(TFHIRRequest(this).Provenance.link, TFHIRRequest(this).Provenance.fhirType, true);
end;

procedure FHIRRequestSetProvenanceJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  raise Exception.Create('todo');
end;

function FHIRRequestGetExternalRequestIdJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).externalRequestId);
end;

procedure FHIRRequestSetExternalRequestIdJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).externalRequestId := js.asString(value);
end;

function FHIRRequestGetInternalRequestIdJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).internalRequestId);
end;

procedure FHIRRequestSetInternalRequestIdJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).internalRequestId := js.asString(value);
end;

function FHIRRequestGetStrictSearchJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject) : JsValueRef;
begin
  result := js.wrap(TFHIRRequest(this).strictSearch);
end;

procedure FHIRRequestSetStrictSearchJs(js : TJavascript; propDef : TJavascriptRegisteredProperty; this : TObject; value : TJsValue);
begin
  TFHIRRequest(this).strictSearch := js.asBoolean(value);
end;


procedure registerFHIRServerEvent(js : TJavascript);
var
  def : TJavascriptClassDefinition;
begin
  def := js.defineClass('Session', nil);
  def.defineRoutine('provider', nil, FHIRSessionProviderJs);
  def.defineRoutine('id', nil, FHIRSessionIdJs);
  def.defineRoutine('userKey', nil, FHIRSessionUserKeyJs);
  def.defineRoutine('userName', nil, FHIRSessionUserNameJs);
  def.defineRoutine('userEvidence', nil, FHIRSessionUserEvidenceJs);
  def.defineRoutine('systemName', nil, FHIRSessionSystemNameJs);
  def.defineRoutine('systemEvidence', nil, FHIRSessionSystemEvidenceJs);
  def.defineRoutine('sessionName', nil, FHIRSessionSessionNameJs);
  def.defineRoutine('email', nil, FHIRSessionEmailJs);
  def.defineRoutine('expires', nil, FHIRSessionExpiresJs);
  def.defineRoutine('created', nil, FHIRSessionCreatedJs);
  def.defineRoutine('canRead', nil, FHIRSessionCanReadJs);
  def.defineRoutine('canWrite', nil, FHIRSessionCanWriteJs);
  def.defineRoutine('compartments', nil, FHIRSessionCompartmentsJs);
  def.defineRoutine('scopes', nil, FHIRSessionScopesJs);
  def.defineRoutine('isAnonymous', nil, FHIRSessionIsAnonymousJs);

  def := js.defineClass('Event', nil);
  def.defineProperty('url', nil, FHIRRequestGetUrlJs, FHIRRequestSetUrlJs);
  def.defineProperty('secure', nil, FHIRRequestGetSecureJs, FHIRRequestSetSecureJs);
  def.defineProperty('baseUrl', nil, FHIRRequestGetBaseUrlJs, FHIRRequestSetBaseUrlJs);
  def.defineProperty('resourceType', nil, FHIRRequestGetResourceTypeJs, FHIRRequestSetResourceTypeJs);
  def.defineProperty('commandType', nil, FHIRRequestGetCommandTypeJs, FHIRRequestSetCommandTypeJs);
  def.defineProperty('id', nil, FHIRRequestGetIdJs, FHIRRequestSetIdJs);
  def.defineProperty('versionId', nil, FHIRRequestGetVersionIdJs, FHIRRequestSetVersionIdJs);
  def.defineProperty('operationName', nil, FHIRRequestGetOperationNameJs, FHIRRequestSetOperationNameJs);
  def.defineProperty('resource', nil, FHIRRequestGetResourceJs, FHIRRequestSetResourceJs);
  def.defineProperty('tags', nil, FHIRRequestGetTagsJs, FHIRRequestSetTagsJs);
  def.defineProperty('elements', nil, FHIRRequestGetElementsJs, FHIRRequestSetElementsJs);
  def.defineProperty('compartment', nil, FHIRRequestGetCompartmentJs, FHIRRequestSetCompartmentJs);
  def.defineProperty('lastModified', nil, FHIRRequestGetLastModifiedJs, FHIRRequestSetLastModifiedJs);
  def.defineProperty('lang', nil, FHIRRequestGetLangJs, FHIRRequestSetLangJs);
  def.defineProperty('summary', nil, FHIRRequestGetSummaryJs, FHIRRequestSetSummaryJs);
  def.defineProperty('ifMatch', nil, FHIRRequestGetIfMatchJs, FHIRRequestSetIfMatchJs);
  def.defineProperty('ifNoneMatch', nil, FHIRRequestGetIfNoneMatchJs, FHIRRequestSetIfNoneMatchJs);
  def.defineProperty('ifModifiedSince', nil, FHIRRequestGetIfModifiedSinceJs, FHIRRequestSetIfModifiedSinceJs);
  def.defineProperty('ifNoneExist', nil, FHIRRequestGetIfNoneExistJs, FHIRRequestSetIfNoneExistJs);
  def.defineProperty('provenance', nil, FHIRRequestGetProvenanceJs, FHIRRequestSetProvenanceJs);
  def.defineProperty('externalRequestId', nil, FHIRRequestGetExternalRequestIdJs, FHIRRequestSetExternalRequestIdJs);
  def.defineProperty('internalRequestId', nil, FHIRRequestGetInternalRequestIdJs, FHIRRequestSetInternalRequestIdJs);
  def.defineProperty('strictSearch', nil, FHIRRequestGetStrictSearchJs, FHIRRequestSetStrictSearchJs);
end;

end.
