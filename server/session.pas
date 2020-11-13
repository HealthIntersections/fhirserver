unit session;

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
  SysUtils, Classes, Generics.Collections,
  IdGlobal,
  fsl_base, fsl_utilities, fsl_stream, fsl_json, fsl_crypto, fsl_scim, fsl_xml,
  fsl_http, fsl_graphql,
  fhir_objects,  fhir_xhtml, fhir_common, fhir_factory,
  fhir_indexing, security, tags, bundlebuilder;

Type
  TFHIRRequestOrigin = (roRest, roOperation, roConfig, roSubscription, roSweep, roUpload, roRestInternal);
  TCreateIdState = (idNoNew, idMaybeNew, idIsNew, idCheckNew);
  TFHIRCacheControl = (cacheNotAtAll, cacheAsException, cacheNormal, cacheLong);
  TFHIRUserIdEvidence = (userNoInformation, userAnonymous, userInternal, userLogin, userExternalOAuth, userBearerJWT);
  TFHIRSystemIdEvidence = (systemNoInformation, systemUnknown, systemInternal, systemFromOAuth, systemFromOWin, systemFromCertificate, systemFromJWT);

Const
  CODES_UserIdEvidence : array [TFHIRUserIdEvidence] of string = ('?', 'Anonymous', 'Internal' ,'Login', 'External AOuth', 'JWT Bearer Token');
  CODES_SystemIdEvidence : array [TFHIRSystemIdEvidence] of String = ('?', 'Unknown', 'This Server', 'by OAuth', 'By OWin', 'By Certificate', 'By JWT');

Type

  {$M+}

  TFHIRCompartmentId = class (TFslObject)
  private
    FResourceType : String;
    FId: String;
  public
    constructor Create(ResourceType, id : String);
    function Link : TFHIRCompartmentId; overload;

    property ResourceType : String read FResourceType write FResourceType;
    property Id : String read FId write FId;

    function ToString : String; override;
  end;

  TFHIRRequest = class;
  TFHIRResponse = class;

  TFHIRFormatAdaptor = class abstract (TFslObject)
  public
    Function Link : TFHIRFormatAdaptor; Overload;

    function ResourceName : String; virtual; abstract;
    function NewIdStatus : TCreateIdState; virtual; abstract;
    procedure load(req : TFHIRRequest; stream : TStream); virtual; abstract;
    procedure compose(response : TFHIRResponse; stream : TStream); virtual; abstract;
    function MimeType : String; virtual; abstract;

    procedure editSearch(req : TFHIRRequest); virtual; abstract;
  end;


  {
    User session associated with a FHIR request.                                                         9
    There is always a session, but it may only have ip address

    If the user has logged in using OAuth, then the user may have identifying details
    if the user login matches a User resource, that will be available too. The information
    may differ if the OAuth provided information differs from that in the resource (the
    resource is not updated by the OAuth information)
  }
  TFhirSession = class (TFslObject)
  private
    Fworker : TFHIRWorkerContextWithFactory;
    FProviderCode : TFHIRAuthProvider;
    FProviderName : String;
    FId : String;
    FSessionName : String;
    FSystemName: String;
    FUserName: String;
    FCookie : String;
    FExpires : TDateTime;
    FOriginal: String;
    FKey: Integer;
    FEmail: String;
    FInnerToken, FOuterToken: String;
    FNextTokenCheck: TDateTime;
    FUseCount: integer;
    FFirstCreated: TDateTime;
    FJwt : TJWT;
    FJwtPacked : String;
    FsessionLength: String;
    FUser : TSCIMUser;
    FUserEvidence : TFHIRUserIdEvidence;
    FSystemEvidence : TFHIRSystemIdEvidence;
    FSecurity : TFHIRSecurityRights;
    FSecure : boolean;
    FUserKey: integer;
    FTestScript: TFhirTestScriptW;
    FExternalUserKey: integer;
    FCompartments : TFslList<TFHIRCompartmentId>;

    procedure SetJwt(const Value: TJWT);
    procedure SetUser(const Value: TSCIMUser);
    function GetScopes: String;
    procedure setScopes(scopes: String);
    procedure SetTestScript(const Value: TFhirTestScriptW);
  public
    constructor Create(worker : TFHIRWorkerContextWithFactory; secure : boolean);
    destructor Destroy; Override;
    function Link : TFhirSession; overload;
    procedure describe(b : TStringBuilder);
    Property scopes : String read GetScopes write SetScopes;

    {
      Primary database key for this session. Don't change!
    }
    Property Key : Integer read FKey write FKey;

    {
      Which Authorization provider scopes the login (blank = direct login)
    }
    Property ProviderCode : TFHIRAuthProvider read FProviderCode write FProviderCode;
    Property ProviderName : String read FProviderName write FProviderName;

   {
      the OAuth authorization token (Don't change!)
      This is the OAuth token for the identity server
    }
    Property InnerToken : String read FInnerToken write FInnerToken;

    {
      the OAuth authorization token (Don't change!)
      This is the OAuth token for this server
    }

    Property OuterToken : String read FOuterToken write FOuterToken;    {
      OAuth provided user id
    }
    Property Id : String read FId write FId;

    {
      OAuth provider internal key
    }
    property UserKey : integer read FUserKey write FUserKey;

    {
      Name that describes the user associated with this session
    }
    property UserName : String read FUserName write FUserName;

    {
      Basis of the evidence for the user identification
    }
    Property UserEvidence : TFHIRUseridEvidence read FUserEvidence write FUserEvidence;

    {
      Name that describes the user associated with this session
    }
    property SystemName : String read FSystemName write FSystemName;

    {
      Basis of the evidence for the system identification
    }
    Property SystemEvidence : TFHIRSystemIdEvidence read FSystemEvidence write FSystemEvidence;

    {
      Summary name for the session
    }
    Property SessionName : String read FSessionName write FSessionName;

    {
      OAuth provided Users email address
    }
    Property Email : String read FEmail write FEmail;

    {
      the shared secret between client and server that identifies this session (i.e. cookie value). (Don't change!)
    }
    Property Cookie : String read FCookie write FCookie;

    {
      When this session expires
    }
    Property Expires : TDateTime read FExpires write FExpires;

    Property FirstCreated : TDateTime read FFirstCreated;

    {
      When the token is next going to be checked with the OAuth server
    }
    Property NextTokenCheck : TDateTime read FNextTokenCheck write FNextTokenCheck;

    {
      The url that the session started with (used through the login process)
    }
    Property OriginalUrl : String read FOriginal write FOriginal;

    {
      User resource associated with this session (if a matching one exists)
    }
    Property User : TSCIMUser read FUser write SetUser;

    {
      The JWT token (Open ID Connect token) associated with this session
    }
    Property JWT : TJWT read FJwt write SetJwt;

    {
      The JWT packed and signed using RSA
    }
    Property JWTPacked : string read FJWTPacked write FJWTPacked;

    Property useCount : integer read FUseCount write FUseCount;
    function canRead(resourceName : String):boolean;
    function canReadAll : boolean;
    function canWrite(resourceName : String):boolean;
    function canGetUser : boolean;
    function canAdministerUsers : boolean;
    procedure allowAll; // for internal use

    property sessionLength : String read FsessionLength write FsessionLength;
    Property TestScript : TFhirTestScriptW read FTestScript write SetTestScript;
    property ExternalUserKey : integer read FExternalUserKey write FExternalUserKey;
    property Compartments : TFslList<TFHIRCompartmentId> read FCompartments;
    procedure buildRow(b : TFslStringBuilder);

    function isAnonymous : boolean;
  end;

  {
    A FHIR request.

    The request may have been received over a FHIR RESTful interface, or by receiving
    a resource or an atom bundle directly from some other kind of interface.

    The request may be modified by a script. HL7Connect will ignore changes to the following
    properties: url, baseURL, resourceType, and format. These properties should be treated as read-only,
    but the other properties can be changed as desired
  }
  TFHIRRequest = class (TFslObject)
  Private
    Fworker: TFHIRWorkerContextWithFactory;
    FCompartmentInformation : TFHIRCompartmentList;
    FId: String;
    FSubId: String;
    FCommandType: TFHIRCommandType;
    FResourceName : String;
    FFormat: TFHIRFormat;
    FResource: TFhirResourceV;
    FUrl: String;
    FBaseUrl: String;
    ForiginalId: String;
    FversionId: String;
    FlastModifiedDate: TDateTime;
    FParams: THTTPParameters;
    FSource: TFslBuffer;
//    FcontentLocation: String;
    FDefaultSearch: boolean;
    FLang: THTTPLanguages;
    FSession: TFhirSession;
    FTags : TFHIRTagList;
    FIp: string;
    FCompartment : TFHIRCompartmentId;
    FForm: TMimeMessage;
    FOperationName: String;
    FIfMatch : String;
    FProvenance: TFhirProvenanceW;
    FIfNoneMatch: String;
    FIfModifiedSince: TDateTime;
    FIfNoneExist: String;
    FSummary: TFHIRSummaryOption;
    FOrigin : TFHIRRequestOrigin;
    FSecure : Boolean;
    FPatchJson: TJsonArray;
    FPatchXml: TMXmlElement;
    FExternalRequestId: String;
    FInternalRequestId: String;
    // FCustom : TFHIRCustomResourceInformation;
    FStrictSearch: boolean;
    FAdaptor : TFHIRFormatAdaptor;
    FLoadObjects : boolean;
    FGraphQL: TGraphQLPackage;
    FElements: TStringList;
    FVersion: TFHIRVersion;
    FTransactionResource: TFhirResourceV;
    procedure SetResource(const Value: TFhirResourceV);
    procedure SetSource(const Value: TFslBuffer);
    procedure SetSession(const Value: TFhirSession);
    procedure SetProvenance(const Value: TFhirProvenanceW);
    procedure processParams;
    procedure SetForm(const Value: TMimeMessage);
    procedure SetPatchJson(const Value: TJsonArray);
    // function RecogniseCustomResource(stype : String; var resourceType : TFhirResourceType) : boolean;
    procedure SetResourceName(const Value: String);
    procedure SetAdaptor(const Value: TFHIRFormatAdaptor);
    procedure SetGraphQL(const Value: TGraphQLPackage);
    procedure SetParams(const Value: THTTPParameters);
    procedure SetPatchXml(const Value: TMXmlElement);
    procedure SetCompartment(const Value: TFHIRCompartmentId);
    function GetHasCompartments: boolean;

    procedure SetTransactionResource(const Value: TFhirResourceV);
    procedure SetLang(const Value: THTTPLanguages);

  Public
    constructor Create(worker: TFHIRWorkerContextWithFactory; origin : TFHIRRequestOrigin; compartmentInformation : TFHIRCompartmentList);
    destructor Destroy; Override;
    Function Link : TFHIRRequest; Overload;

    function clone() : TFHIRRequest;
    Function Compose : String;
    procedure LoadParams(s : String); overload;
    procedure LoadParams(form : TMimeMessage); overload;
    Function LogSummary : String;
    function XMLSummary : String;
    Procedure CopyPost(stream : TStream);
    Property Source : TFslBuffer read FSource write SetSource;
    Property Session : TFhirSession read FSession write SetSession;
    Property ip : string read FIp write FIp;
    Property form : TMimeMessage read FForm write SetForm;
    function canRead(resourceName : String):boolean;
    function canWrite(resourceName : String):boolean;
    function canGetUser : boolean;
    function NewIdStatus : TCreateIdState;
    procedure reset;
    property Context : TFHIRWorkerContextWithFactory read FWorker;
    property Adaptor : TFHIRFormatAdaptor read FAdaptor write SetAdaptor;

    // main rest function. Set the following things before calling this:
    // form
    // also, the base must be stipped out before calling this
    function preAnalyse(url : String) : String;
    procedure analyse(sCommand, sUrl : String; out relativeReferenceAdjustment : integer; adaptors : TFslMap<TFHIRFormatAdaptor>);

    Property DefaultSearch : boolean read FDefaultSearch write FDefaultSearch;

    {
      any parameters associated with the request (part after the ? in the url). Use
      for search/update
    }
    property Parameters : THTTPParameters read FParams write SetParams;

    function hasTestingTag : boolean;
  
    {
      Preferred language of the requester (used for error messages)
    }
    Property Lang : THTTPLanguages read FLang write SetLang;

  published
    {
      The full URL of the original request, if the request was made on a RESTful interface (else empty)
    }
    property url : String read FUrl write FUrl;

    {
      Whether the request was made on an SSL interface or not (SSL = Smart App Launch as well)
    }
    property secure : Boolean read FSecure write FSecure;

    {
      The baseURL (see the FHIR specification under "HTTP REST interface) of the interface,
      if the request was made on a RESTful interface (else empty)
    }
    property baseUrl : String read FBaseUrl write FBaseUrl;

    {
      The type of the resource. Cannot be changed

      frtNull if this is a bundle
    }
    Property ResourceName : String Read FResourceName write SetResourceName;
//    Property ResourceEnum : TFHIRResourceType Read FResourceEnum;

    {
      The command (http transaction). This can be changed, though it is unusual to
      change the command (consequences can be unexpected and dramatic)

      fcmdUnknown if this is not on a RESTful interface
    }
    Property CommandType : TFHIRCommandType Read FCommandType Write FCommandType;

    {
      The resource id associated with the request, if one is identified as part of the request
    }
    Property Id : String Read FId write FId;

    {
      A secondary id associated with the request (only used for the version id in a version specific request)
    }
    Property SubId : String Read FSubId write FSubId;

    {
      The name of an operation, if an operation was invoked
    }
    Property OperationName : String read FOperationName write FOperationName;

    {
      The format of the request, if known and identified (xml, json, or xhtml). Derived
      from the content-type and/or extension in the url, or configuration
    }
    Property PostFormat : TFHIRFormat read FFormat write FFormat;

    {
      the actual resource, if a resource was submitted as part of the request.

      Note that actual kind of the resource will be one of the ones defined as
      part of the FHIR specification
    }
    Property Resource : TFhirResourceV read FResource write SetResource;
    Property transactionResource : TFhirResourceV read FTransactionResource write SetTransactionResource;

    Property patchJson : TJsonArray read FPatchJson write SetPatchJson;
    Property patchXml : TMXmlElement read FPatchXml write SetPatchXml;
    property GraphQL : TGraphQLPackage read FGraphQL write SetGraphQL;

    property Version : TFHIRVersion read FVersion write FVersion;

    {
      Tags on the request - if it's a resource directly
    }
    property Tags : TFHIRTagList read FTags;

    property Elements : TStringList read FElements;

    {
      The specified originalId of the resource in the request (if present) (i.e. in a transaction)
    }
    Property originalId : String read ForiginalId write ForiginalId;

    {
      If the user is limited to a set of compartments, this is the list (comma separated, with quotes)
    }
    Property compartment : TFHIRCompartmentId read FCompartment write SetCompartment;
    property hasCompartments : boolean read GetHasCompartments;
    function SessionCompartments : TFslList<TFHIRCompartmentId>;


    {
      Quoted Content location on request. Used for version aware updates. Only on RESTful interface
    }
//    Property contentLocation : String read FcontentLocation write FcontentLocation;

//    {
//      The ETag of the resource identified in the request (if present)
//    }
//    Property e_versionId : String read FversionId write FversionId;

    {
      The last modified date of the resource identified in the request (if present)
    }
    Property lastModifiedDate : TDateTime read FlastModifiedDate write FlastModifiedDate;

    {
      What kind of summary is requested
    }
    Property Summary : TFHIRSummaryOption read FSummary write FSummary;

    Property IfMatch : String read FIfMatch write FIfMatch;
    Property IfNoneMatch : String read FIfNoneMatch write FIfNoneMatch;
    Property IfModifiedSince : TDateTime read FIfModifiedSince write FIfModifiedSince;
    Property IfNoneExist : String read FIfNoneExist write FIfNoneExist;

    Property Provenance : TFhirProvenanceW read FProvenance write SetProvenance;
    Property Origin : TFHIRRequestOrigin read FOrigin;

    property externalRequestId : String read FExternalRequestId write FExternalRequestId;
    property internalRequestId : String read FInternalRequestId write FInternalRequestId;
    property strictSearch : boolean read FStrictSearch write FStrictSearch;
    property loadObjects : boolean read FLoadObjects write FLoadObjects;
  End;

  TCreateBundleBuilderEvent = procedure (request : TFHIRRequest; context : TFHIRResponse; aType : TBundleType; out builder : TFhirBundleBuilder) of object;

  {
    A FHIR response.

    This is a response for a RESTful interface, or some other kind of response. The
    HTTP code is used as the logical outcome with other kinds of interfaces.

    A response may have only one of
      * a body
      * a resource
      * a bundle

    The string body is used for error messages, or to return xhtml or schema etc.

    A script may modify any of the values of the response, though changing the
    type of the resource may have unexpected catastrophic outcomes.
  }
  TFHIRResponse = class (TFslObject)
  private
    Fworker: TFHIRWorkerContextWithFactory;
    FHTTPCode: Integer;
    FBody: String;
    FMessage: String;
    FResource: TFhirResourceV;
    FversionId: String;
    ForiginalId: String;
    FlastModifiedDate: TDateTime;
    FContentType: String;
    FFormat: TFHIRFormat;
    FContentLocation: String;
    FLocation: String;
    FTags : TFHIRTagList;
    Flinks : TFslStringDictionary;
    FOrigin: String;
    FId: String;
    FOutcome: TFHIROperationOutcomeW;
    FCacheControl : TFHIRCacheControl;
    FProgress: String;
    FStream : TFslStream;
    FOnCreateBuilder: TCreateBundleBuilderEvent;
    FVersion: TFHIRVersion;

    procedure SetResource(const Value: TFhirResourceV);
    procedure SetOutcome(const Value: TFHIROperationOutcomeW);
    procedure SetStream(const Value: TFslStream);
  public
    constructor Create(worker: TFHIRWorkerContextWithFactory);
    destructor Destroy; Override;

      Function Link : TFHIRResponse; Overload;

    {
      The logical outcome of the request. Usual values are
        * 0 - the outcome of the transaction is not yet known
        * 200 - the operation completed successfully
        * 202 - the content was accepted (an http variation on 200)
        * 400 - the user made a bad request
        * 404 - the resource wasn't found
        * 500 - some general kind of error

      Any http status code may be used, including codes not defined in the
      http standard (i.e. return 299 to prevent a proxy from caching the response).
      HL7Connect will follow the http standard and use the first digit to determine
      the general outcome
    }
    Property HTTPCode : Integer read FHTTPCode write FHTTPCode;

    {
      a specific message to go in the HTTP response line. If left blank,
      HL7Connect will fill this out from the http specification
    }
    Property Message : String read FMessage write FMessage;

    {
      a series of characters that constitute the body of the response. Usually this is
      plain text, but other content (xhtml, schema) could be placed in here.
      If using other than plain text, set the @contentType
    }
    Property Body : String read FBody write FBody;

    {
      the actual resource that is the result of the transaction.

      Note that actual kind of the resource will be one of the ones defined as
      part of the FHIR specification
    }
    Property Resource : TFhirResourceV read FResource write SetResource;

    Property outcome : TFHIROperationOutcomeW read FOutcome write SetOutcome;
    Property Stream : TFslStream read FStream write SetStream;

    {
      The format for the response, if known and identified (xml, or json). Derived
      from the requested content-type and/or extension in the url, or configuration
    }
    Property Format : TFHIRFormat read FFormat write FFormat;

    {
      The content type of the response. if left blank, this will be determined
      automatically (text/plain for body, and type as specifed in the FHIR
      specification for resources and bundles.
    }
    Property ContentType : String read FContentType write FContentType;

    {
      The originalId of the resource - if known
    }
    Property originalId : String read ForiginalId write ForiginalId;

    {
      The underlying id, if there is one. Only used internally - not represented on the wire
    }
    Property Id : String read FId write FId;

    {
      The ETag to go in the response
    }
    Property versionId : String read FversionId write FversionId;

    {
      The Last Modified Date to go in the response
    }
    Property lastModifiedDate : TDateTime read FlastModifiedDate write FlastModifiedDate;

    {
      Content-Location in HTTP response
    }
    Property ContentLocation : String read FContentLocation write FContentLocation;

    {
      Location in HTTP response (only used for Create operation)
    }
    Property Location : String read FLocation write FLocation;

    {
      Tags for the response
    }
    property Tags : TFHIRTagList read FTags;

    property Version : TFHIRVersion read FVersion write FVersion;

    property links : TFslStringDictionary read Flinks;

    {
      HTTP Origin header - see http://en.wikipedia.org/wiki/Cross-origin_resource_sharing

      If this has a value when the response is returned, then it will be returned in the Access-Control-Allow-Origin header
    }
    Property Origin : String read FOrigin write FOrigin;

    {
      The degree of caching to use on the response
    }
    Property CacheControl : TFHIRCacheControl read FCacheControl write FCacheControl;

    property Progress : String read FProgress write FProgress;
    property OnCreateBuilder : TCreateBundleBuilderEvent read FOnCreateBuilder write FOnCreateBuilder;
  end;


Function IdTail(s : String):String;
Function IdHead(s : String):String;

implementation

uses
  fhir_parser;


{ TFHIRRequest }

function TFHIRRequest.preAnalyse(url : String) : String;
var
  i : integer;
begin
  i := StringFind(uRL, ['?']);
  if (i = 0) then
    LoadParams('')
  else
  begin
    LoadParams(copy(uRL, i+1, $FFF));
    url := copy(uRL, 1, i-1);
  end;
  if url.StartsWith('/') then
    url := url.Substring(1);
  if form <> nil then
    LoadParams(form);
  result := url;
end;

Procedure CheckId(const lang : THTTPLanguages; id : String);
var
  i : integer;
begin
  if (Length(id) > ID_LENGTH) then
    Raise ERestfulException.Create('TFhirWebServer.CheckId', HTTP_ERR_BAD_REQUEST, itInvalid, 'MSG_ID_TOO_LONG', lang, [id]);
  for i := 1 to length(id) do
    if not CharInSet(id[i], ['a'..'z', '0'..'9', 'A'..'Z', '.', '-', '_']) then
      Raise ERestfulException.Create('TFhirWebServer.CheckId', HTTP_ERR_BAD_REQUEST, itInvalid, 'MSG_ID_INVALID', lang, [id, id[i]]);
end;


procedure TFHIRRequest.analyse(sCommand, sUrl: String; out relativeReferenceAdjustment : integer; adaptors : TFslMap<TFHIRFormatAdaptor>);
Var
  sId, sType : String;
  Function NextSegment(var url : String):String;
  var
    i : integer;
  Begin
    i := StringFind(url, ['/']);
    if i = 0 then
    begin
      result := url;
      url := '';
    end
    else
    begin
      inc(relativeReferenceAdjustment);
      result := copy(url, 1, i-1);
      url := copy(url, i + 1, $FFF);
    end;
  End;
  procedure ForceMethod(sMethod : String);
  begin
    if (sCommand <> sMethod) Then
      raise ERestfulException.Create('TFhirWebServer.HTTPRequest', HTTP_ERR_FORBIDDEN, itInvalid, 'MSG_BAD_FORMAT', lang, [sUrl, sMethod]);
  end;
var
  s, soURL : String;
begin
  Elements.Clear;
  Elements.CommaText := Parameters['_elements'];

  soURL := sUrl;
  relativeReferenceAdjustment := 0;
  sType := NextSegment(sURL);
  if (sType = '') Then
  begin
    if (sCommand = 'POST') then
    begin
      if form <> nil then
      begin
        s := Parameters['op'];
        if (s = 'transaction') or (s = '') then
          CommandType := fcmdTransaction
        else if (s = 'validation') or (s = 'validate') then
          CommandType := fcmdValidate
        else if (s = 'batch') then
          CommandType := fcmdBatch
        else
          raise EFHIRException.create('Unknown Operation: '+s);
      end
      else
        CommandType := fcmdTransaction
    end
    else
    begin
      ForceMethod('GET');
      if Parameters.Count > 0 then
        CommandType := fcmdSearch
      else
        CommandType := fcmdUnknown;
    end
  end
  else if (sType = '_web') then // special
  begin
    if sCommand <> 'POST' then
      ForceMethod('GET');
    CommandType := fcmdWebUI;
    id := sUrl;
    sUrl := '';
  end
  else if StringStartsWith(sType, '$', false)  then
  begin
    CommandType := fcmdOperation;
    OperationName := sType.Substring(1);
    if sCommand <> 'GET' then
      ForceMethod('POST');
  end
  else if (sType = '_history') then
  begin
    ForceMethod('GET');
    CommandType := fcmdHistorySystem;
  end
  else if (sType = 'metadata') Then
  begin
    CommandType := fcmdMetadata;
    ForceMethod('GET');
  end
  else if (sType = 'task') Then
  begin
    CommandType := fcmdTask;
    StringSplit(sURL, '/', FId, FSubId);
    sUrl := '';
    if FId.endsWith('.zip') then
      StringSplit(FId, '.', FId, FSubId);
    if not IsGuid(FId) then
      raise ERestfulException.Create('TFhirWebServer.HTTPRequest', HTTP_ERR_FORBIDDEN, itInvalid, 'MSG_BAD_FORMAT', lang, [sUrl, FId]);
    if sCommand = 'DELETE' then
    begin
      if FSubid <> '' then
        raise ERestfulException.Create('TFhirWebServer.HTTPRequest', HTTP_ERR_FORBIDDEN, itInvalid, 'MSG_BAD_FORMAT', lang, [sUrl, FSubid]);
      CommandType := fcmdDeleteTask;
    end
    else
      ForceMethod('GET');
  end
  else if (sType = 'validation') then
  begin
    CommandType := fcmdValidate;
    ForceMethod('POST');
    if Parameters.has('profile') and (Parameters['profile'] <> '') then
      tags.AddTag(0, tcProfile, 'urn:ietf:rfc:3986', 'http://localhost/'+Parameters['profile'], '');
  end
  else if (sType = '_search') then
  begin
    CommandType := fcmdSearch;
    if (sCommand <> 'GET') and (sCommand <> 'POST') then
      raise ERestfulException.Create('TFhirWebServer.HTTPRequest', HTTP_ERR_BAD_REQUEST, itInvalid, 'MSG_BAD_SYNTAX', lang, [soURL]);
  end
  else
  begin
    Adaptor := nil;
    if (sType <> '') then
      if (adaptors <> nil) and adaptors.ContainsKey(sType) then
      begin
        adaptor := adaptors[sType].link;
        ResourceName := adaptor.ResourceName;
      end
      else if not Fworker.factory.isResourceName(sType) {and not RecogniseCustomResource(sType, aResourceType)} then
        Raise ERestfulException.Create('TFhirWebServer.HTTPRequest', HTTP_ERR_NOTFOUND, itNotSupported, 'MSG_NO_MODULE', lang, [sType])
      else
        ResourceName := sType;
    sId := NextSegment(sURL);
    if sId = '' then
    begin
      if sCommand = 'GET' then
      begin
        CommandType := fcmdSearch;
        DefaultSearch := true;
      end
      else if sCommand = 'POST' then
        CommandType := fcmdCreate
      else if (scommand = 'DELETE') then
      begin
        CommandType := fcmdDelete;
        DefaultSearch := true;
      end
      else
        raise ERestfulException.Create('TFhirWebServer.HTTPRequest', HTTP_ERR_BAD_REQUEST, itInvalid, 'MSG_BAD_SYNTAX', lang, [soURL]);
    end
    else if StringStartsWith(sId, '$', false) then
    begin
      CommandType := fcmdOperation;
      OperationName := sId.Substring(1);
      if sCommand <> 'GET' then
        ForceMethod('POST');
    end
    else if not StringStartsWith(sId, '_', true) Then
    begin
      // operations on a resource
      CheckId(lang, sId);
      Id := sId;
      sId := NextSegment(sUrl);
      if (sId = '') Then
      begin
        if sCommand = 'GET' Then
          CommandType := fcmdRead
        else if sCommand = 'PUT' Then
          CommandType := fcmdUpdate
        else if sCommand = 'PATCH' Then
          CommandType := fcmdPatch
        else if sCommand = 'DELETE' Then
          CommandType := fcmdDelete
        else if sCommand = 'OPTIONS' then // CORS
          CommandType := fcmdMetadata
        else
          raise ERestfulException.Create('TFhirWebServer.HTTPRequest', HTTP_ERR_FORBIDDEN, itNotSupported, 'MSG_BAD_FORMAT', lang, [soURL, 'GET, PUT or DELETE']);
      end
      else if StringStartsWith(sId, '$', false)  then
      begin
        CommandType := fcmdOperation;
        OperationName := sId.Substring(1);
        if sCommand <> 'GET' then
          ForceMethod('POST');
      end
      else if (sId = '_history') then
      begin
        sId := NextSegment(sURL);
        if sId = '' then
        begin
          ForceMethod('GET');
          CommandType := fcmdHistoryInstance;
        end
        else if not StringStartsWith(sId, '_', true) Then
        begin
          CheckId(lang, sId);
          SubId := sId;
          sId := NextSegment(sURL);
          if sid = '' then
          begin
            ForceMethod('GET');
            CommandType := fcmdVersionRead;
          end
          else if (sId = '$meta') then
          begin
            ForceMethod('GET');
            CommandType := fcmdOperation;
            OperationName := 'meta';
          end
          else if (sId = '$meta-add') then
            begin
              ForceMethod('POST');
            CommandType := fcmdOperation;
            OperationName := 'meta-add';
      end
          else if (sId = '$meta-delete') then
      begin
            ForceMethod('POST');
            CommandType := fcmdOperation;
            OperationName := 'meta-delete';
          end
          else
            raise ERestfulException.Create('TFhirWebServer.HTTPRequest', HTTP_ERR_BAD_REQUEST, itInvalid, 'MSG_BAD_SYNTAX', lang, [soURL]);
        end
        else
          raise ERestfulException.Create('TFhirWebServer.HTTPRequest', HTTP_ERR_BAD_REQUEST, itInvalid, 'MSG_BAD_SYNTAX', lang, [soURL]);
      end
      else if sId = '*' then // all types
      begin
        if not FCompartmentInformation.hasCompartment(ResourceName) then
          raise ERestfulException.Create('TFhirWebServer.HTTPRequest', HTTP_ERR_FORBIDDEN, itNotSupported, 'MSG_UNKNOWN_COMPARTMENT', lang, [soURL, 'GET, POST or DELETE']);

        compartment := TFHIRCompartmentId.Create(ResourceName, Id);
        CommandType := fcmdSearch;
        ResourceName := '';
        Id := '';
      end
      else if FWorker.factory.isResourceName(sId) {or FWorker.hasCustomResource(sId)} then
      begin
        if FCompartmentInformation.existsInCompartment(ResourceName, sId) then
        begin
          if not FCompartmentInformation.hasCompartment(ResourceName) then
            raise ERestfulException.Create('TFhirWebServer.HTTPRequest', HTTP_ERR_FORBIDDEN, itNotSupported, 'MSG_UNKNOWN_COMPARTMENT', lang, [soURL, 'GET, POST or DELETE']);

          compartment := TFHIRCompartmentId.Create(ResourceName, Id);
          CommandType := fcmdSearch;
          ResourceName := sId;
          Id := '';
        end
        else
          raise ERestfulException.Create('TFhirWebServer.HTTPRequest', HTTP_ERR_BAD_REQUEST, itInvalid, 'MSG_BAD_SYNTAX', lang, [soURL]);
      end
      else
        raise ERestfulException.Create('TFhirWebServer.HTTPRequest', HTTP_ERR_BAD_REQUEST, itInvalid, 'MSG_BAD_SYNTAX', lang, [soURL]);
    end
    else if (sId = '_validate') Then
    begin
      ForceMethod('POST');
      CommandType := fcmdValidate;
      sId := NextSegment(sUrl);
      if sId <> '' Then
      Begin
        if (sURL <> '') or (sId = '') Then
          raise ERestfulException.Create('TFhirWebServer.HTTPRequest', HTTP_ERR_BAD_REQUEST, itInvalid, 'MSG_BAD_SYNTAX', lang, [soURL]);
        CheckId(lang, copy(sId, 2, $FF));
        Id := copy(sId, 2, $FF);
      End;
    end
    else if (sId = '_search') or (sId = '_search.xml') or (sId = '_search.json') Then
      CommandType := fcmdSearch
    else if (sId = '_history') or (sId = '_history.xml') or (sId = '_history.json') Then
    begin
      ForceMethod('GET');
      CommandType := fcmdHistoryType
    end
    // Extension on this server - remove?
    else if (sId = '_upload') or (sId = '_upload.htm') then
    begin
      CommandType := fcmdUpload;
      PostFormat := ffXhtml;
    end
    else
      raise ERestfulException.Create('TFhirWebServer.HTTPRequest', HTTP_ERR_BAD_REQUEST, itInvalid, 'MSG_BAD_SYNTAX', lang, ['URL'+soURL]);
  End;
  if (CommandType <> fcmdNull)  then
    if (sURL <> '') then
      raise ERestfulException.Create('TFhirWebServer.HTTPRequest', HTTP_ERR_BAD_REQUEST, itInvalid, 'MSG_BAD_SYNTAX', lang, [soURL]);
  if (CommandType = fcmdSearch) and (Adaptor <> nil) then
    Adaptor.editSearch(self);
end;

function TFHIRRequest.canGetUser: boolean;
begin
  if session = nil then
    result := true
  else
    result := session.canGetUser;
end;

function TFHIRRequest.canRead(resourceName : String): boolean;
begin
  if session = nil then
    result := true
  else
    result := session.canRead(resourceName);
end;

function TFHIRRequest.canWrite(resourceName : String): boolean;
begin
  if session = nil then
    result := true
  else
    result := session.canWrite(resourceName);
end;

function TFHIRRequest.clone: TFHIRRequest;
begin
  result := TFHIRRequest.Create(FWorker.link, FOrigin, FCompartmentInformation.Link);
  result.FFormat := FFormat;
  result.FBaseUrl := FBaseUrl;
  result.FLang := FLang;
  result.FSession := FSession.Link;
  result.FIp := FIp;
  result.FCompartment := FCompartment.Link;
  result.FSummary := FSummary;
  result.FOrigin := FOrigin;
  result.FSecure := FSecure;
end;

function TFHIRRequest.Compose: String;
var
  comp : TFHIRComposer;
  stream : TStringStream;
begin
  stream := TStringStream.Create('');
  try
    comp := Fworker.Factory.makeComposer(Fworker.link, ffXml, lang, OutputStylePretty);
    try
      comp.Compose(stream, resource);
    finally
      comp.free;
    end;
    result := stream.DataString;
  finally
    stream.free;
  end;
end;

procedure TFHIRRequest.CopyPost(stream: TStream);
var
  p, t : integer;
  b : TBytes;
begin
  p := stream.Position;
  t := stream.size - stream.position;
  SetLength(b, t);
  Stream.Read(b[0], t);
  stream.position := p;
  FSource := TFslBuffer.create;
  FSource.AsBytes := b;
end;

constructor TFHIRRequest.Create(worker: TFHIRWorkerContextWithFactory; origin : TFHIRRequestOrigin; compartmentInformation : TFHIRCompartmentList);
begin
  inherited Create;
  FWorker := worker;
  FTags := TFHIRTagList.create(worker.Factory.link);
  FOrigin := origin;
  FCompartmentInformation := compartmentInformation;
  FElements := TStringList.Create;
  Version := worker.Factory.version;
end;

destructor TFHIRRequest.Destroy;
begin
  FElements.Free;
  FAdaptor.Free;
  FCompartmentInformation.free;
//  FCustom.Free;
  FWorker.Free;
  FPatchJson.Free;
  FTags.free;
//  FSession.Free;  //possibly pass it to the end?
  FSource.Free;
  FResource.Free;
  FTransactionResource.Free;
  FProvenance.Free;
  FForm.Free;
  FParams.Free;
  FGraphQL.Free;
  FPatchXml.Free;
  FCompartment.Free;
  FSession.Free;
  inherited;
end;


function TFHIRRequest.GetHasCompartments: boolean;
begin
  result := (FCompartment <> nil) or (Session.Compartments.Count > 0);
end;

function TFHIRRequest.hasTestingTag: boolean;
begin
  result := Tags.hasTestingTag;
end;

function TFHIRRequest.Link: TFHIRRequest;
begin
  result := TFHIRRequest(Inherited Link);
end;

procedure TFHIRRequest.LoadParams(s: String);
begin
  if (FParams <> nil) then
    FParams.Free;
  FParams := nil;
  FParams := THTTPParameters.create(s);
  processParams;
end;

procedure TFHIRRequest.LoadParams(form: TMimeMessage);
var
  i : integer;
  p : TMimePart;
  s, n : String;
begin
  for i := 0 to form.Parts.Count - 1 do
  begin
    p := form.Parts[i];
    if (p.MediaType = '') then
    begin
      n := p.ParamName;
      s := p.Content.AsText.trimRight([#13, #10]);
      if (n <> '') and (not s.Contains(#10)) then
        FParams.add(n, s);
    end;
  end;
  processParams;
end;


function TFHIRRequest.LogSummary: String;
begin
  result := CODES_TFHIRCommandType[CommandType]+'\('+CODES_TFHIRFormat[PostFormat]+')'+ResourceName+'\'+Id;
  if SubId <> '' then
    result := result + '\'+SubId;
end;

function TFHIRRequest.NewIdStatus: TCreateIdState;
begin
  if (Adaptor <> nil) then
    result := Adaptor.NewIdStatus
  else
    result := idNoNew;
end;

procedure TFHIRRequest.processParams;
var
  s : String;
begin
  s := FParams['_summary'];
  if s = 'true' then
    Summary := soSummary
  else if s = 'text' then
    Summary := soText
  else if s = 'data' then
    Summary := soData
  else if s = 'count' then
    Summary := soCount
  else
    Summary := soFull;
end;

{
function TFHIRRequest.RecogniseCustomResource(stype: String; var resourceType: TFhirResourceType): boolean;
begin
  FCustom := Fworker.getCustomResource(stype);
  result := FCustom <> nil;
  if result then
    resourceType := frtCustom;
end;
}

procedure TFHIRRequest.reset;
begin
  FId := '';
  FSubId := '';
  FCommandType := fcmdUnknown;
  FResourceName := '';
  FResource.Free;
  FResource := nil;
  FUrl := '';
  FParams.Free;
  FParams := nil;
  if FSource <> nil then
    FSource.Clear;
  FDefaultSearch := false;
  FForm := nil;
  FOperationName := '';
  FIfMatch  := '';
  FIfNoneMatch := '';
  FIfModifiedSince := 0;
  FIfNoneExist := '';
  FSummary := soFull;
end;

function TFHIRRequest.SessionCompartments: TFslList<TFHIRCompartmentId>;
begin
  if Session = nil then
    result := nil
  else
    result := Session.Compartments;
end;

procedure TFHIRRequest.SetAdaptor(const Value: TFHIRFormatAdaptor);
begin
  FAdaptor.Free;
  FAdaptor := Value;
  if FAdaptor <> nil then
    FLoadObjects := true;
end;

procedure TFHIRRequest.SetCompartment(const Value: TFHIRCompartmentId);
begin
  FCompartment.Free;
  FCompartment := Value;
end;

procedure TFHIRRequest.SetForm(const Value: TMimeMessage);
begin
  FForm.Free;
  FForm := Value;
end;

procedure TFHIRRequest.SetGraphQL(const Value: TGraphQLPackage);
begin
  FGraphQL.Free;
  FGraphQL := Value;
end;

procedure TFHIRRequest.SetLang(const Value: THTTPLanguages);
begin
  FLang := Value;
end;

procedure TFHIRRequest.SetParams(const Value: THTTPParameters);
begin
  FParams.Free;
  FParams := Value;
end;

procedure TFHIRRequest.SetPatchJson(const Value: TJsonArray);
begin
  FPatchJson.Free;
  FPatchJson := Value;
end;

procedure TFHIRRequest.SetPatchXml(const Value: TMXmlElement);
begin
  FPatchXml.Free;
  FPatchXml := Value;
end;

procedure TFHIRRequest.SetProvenance(const Value: TFhirProvenanceW);
begin
  FProvenance.Free;
  FProvenance := Value;
end;

procedure TFHIRRequest.SetResource(const Value: TFhirResourceV);
begin
  FResource.Free;
  FResource := Value;
end;

procedure TFHIRRequest.SetResourceName(const Value: String);
begin
  FResourceName := Value;
{  if (value = '') then
    FResourceEnum := frtNull
  else if StringArrayExistsInSensitive(CODES_TFhirResourceType, Value) then
    FResourceEnum := TFhirResourceType(StringArrayIndexOfSensitive(CODES_TFhirResourceType, Value))
  else
    FResourceEnum := frtCustom;
}
end;

procedure TFHIRRequest.SetSession(const Value: TFhirSession);
begin
  FSession.Free;
  FSession := Value;
end;

procedure TFHIRRequest.SetSource(const Value: TFslBuffer);
begin
  FSource.Free;
  FSource := Value;
end;

procedure TFHIRRequest.SetTransactionResource(const Value: TFhirResourceV);
begin
  FTransactionResource.Free;
  FTransactionResource := Value;
end;

function TFHIRRequest.XMLSummary: String;
  procedure addValue(n, v : String; t : boolean);
  begin
    if (t) then
      result := result+'  '+n+': '+v+#13#10;
  end;
begin
  result := #13#10;
  addValue('Command', CODES_TFHIRCommandType[CommandType], true);
  addValue('Url', FUrl, true);
  addValue('format', CODES_TFHIRFormat[PostFormat], true);
  addValue('type', ResourceName, true);
  addValue('id', FId, true);
  addValue('subId', FSubId, FSubId <> '');
  addValue('baseUrl', FBaseUrl, FBaseUrl <> '');
  addValue('originalId', ForiginalId, ForiginalId <> '');
  addValue('versionId', FversionId, FversionId <> '');
  addValue('Last-Modified', FormatDateTime('c', FlastModifiedDate), FlastModifiedDate <> 0);
//  addValue('Content-Location', FcontentLocation, FcontentLocation <> '');
  addValue('defaultSearch', BooleanToString(FDefaultSearch), CommandType = fcmdSearch);
  addValue('Language', FLang.header, FLang.header <> '');
  addValue('Tag', FTags.ToString, FTags.count > 0);
  addValue('ip', FIp, FIp <> '');
  if FCompartment <> nil then
    result := result+'  Compartment: '+FCompartment.ToString+#13#10;
end;

{ TFHIRResponse }

constructor TFHIRResponse.Create(worker: TFHIRWorkerContextWithFactory);
begin
  inherited create;
  Fworker := worker;
  FTags := TFHIRTagList.create(worker.Factory.link);
  Flinks := TFslStringDictionary.create;
  FCacheControl := cacheNormal;
  Version := worker.version;
end;

destructor TFHIRResponse.Destroy;
begin
  FStream.Free;
  FOutcome.Free;
  Flinks.Free;
  FTags.free;
  FResource.Free;
  FWorker.Free;
  inherited;
end;

//function TFHIRResponse.GetBundle: TFhirBundle;
//begin
//  if not (resource is TFHIRBundle) then
//    raise EFHIRException.create('Attempt to cast a '+resource.FhirType+' to a Bundle');
//  result := FResource as TFhirBundle;
//end;
//procedure TFHIRResponse.SetBundle(const Value: TFhirBundle);
//begin
//  SetResource(value);
//end;


function TFHIRResponse.Link: TFHIRResponse;
begin
  result := TFHIRResponse(Inherited Link);
end;

procedure TFHIRResponse.SetOutcome(const Value: TFHIROperationOutcomeW);
begin
  FOutcome.Free;
  FOutcome := Value;
end;

procedure TFHIRResponse.SetResource(const Value: TFhirResourceV);
begin
  FResource.free;
  FResource := nil;
  FResource := Value;
end;

procedure TFHIRResponse.SetStream(const Value: TFslStream);
begin
  FStream.Free;
  FStream := Value;
end;

{ TFhirSession }

procedure TFhirSession.allowAll;
begin
  FSecurity.allowAll;
end;

procedure TFhirSession.buildRow(b: TFslStringBuilder);
begin
  b.append(' <tr>');
  if FSecure then
    b.append('<td><img src="sec_secure.png"/></td>')
  else
    b.append('<td><img src="sec_anonymous.png"/></td>');
  b.append('<td>'+FormatTextToHTML(FSessionName)+'</td>');
  b.append('<td>'+FormatTextToHTML(FUserName)+'</td>');
  b.append('<td>'+FormatTextToHTML(FSystemName)+'</td>');
  b.append('<td>'+FormatTextToHTML(ProviderName+' ('+CODES_UserIdEvidence[FUserEvidence]+')')+'</td>');
  b.append('<td>'+DescribePeriod(now - FFirstCreated)+'</td>');
  b.append('<td>'+DescribePeriod(FExpires - now)+'</td>');
  b.append('<td>');
  if FSecurity.canAdministerUsers then
    b.append('<img src="sec_admin.png"/> ');
  if FSecurity.canGetUserInfo then
    b.append('<img src="sec_user.png"/> ');
  if FSecurity.canReadAny then
    b.append('<img src="sec_read.png"/> ');
  if FSecurity.canWriteAny then
    b.append('<img src="sec_write.png"/> ');
  b.append('</td>');
  if FTestScript <> nil then
    b.append('<td><img src="recording.png"/></td>')
  else
    b.append('<td></td>');
  b.append('<td><a style="font: bold; text-decoration: none; background-color: #EEEEEE; color: #333333; padding: 2px 6px 2px 6px; border-top: 1px solid #CCCCCC; border-right: 1px solid #333333; border-bottom'+
     ': 1px solid #333333;  border-left: 1px solid #CCCCCC;" href="auth/logout-all?cookie='+FCookie+'">Close</a></td>');
  b.append('</tr>'#13#10);
end;

function TFhirSession.canAdministerUsers: boolean;
begin
  result := FSecurity.canAdministerUsers;
end;

function TFhirSession.canGetUser: boolean;
begin
  result := FSecurity.canGetUserInfo;
end;

function TFhirSession.canRead(resourceName : String): boolean;
begin
  result := FSecurity.canRead(resourceName);
end;

function TFhirSession.canReadAll: boolean;
begin
  result := FSecurity.canReadAll;
end;

function TFhirSession.canWrite(resourceName : String): boolean;
begin
  result := FSecurity.canWrite(resourceName);
end;

constructor TFhirSession.Create(worker : TFHIRWorkerContextWithFactory; secure : boolean);
begin
  inherited Create;
  FWOrker := worker;
  FSecure := secure;
  FFirstCreated := now;
  FCompartments := TFslList<TFHIRCompartmentId>.create;
end;

procedure TFhirSession.describe(b: TStringBuilder);
var
  id : TFHIRCompartmentId;
  first : boolean;
begin
  b.Append('<tr>');
//  session key
  b.Append('<td>');
  b.Append(inttostr(FKey));
  b.Append('</td>');
//  identity
  b.Append('<td>');
  b.Append(FId);
  b.Append('</td>');
//  userkey
  b.Append('<td>');
  b.Append(inttostr(FUserKey));
  b.Append('</td>');
//  name
  b.Append('<td>');
  b.Append(FormatTextToHTML(FSessionName));
  b.Append('</td>');
  b.Append('<td>');
  b.Append(FormatTextToHTML(FSystemName));
  b.Append(' (');
  b.Append(CODES_SystemIdEvidence[FSystemEvidence]);
  b.Append(')');
  b.Append('</td>');
  b.Append('<td>');
  b.Append(FormatTextToHTML(FUserName));
  b.Append(' (');
  b.Append(CODES_UserIdEvidence[FUserEvidence]);
  if (FUserEvidence in [userExternalOAuth, userBearerJWT]) then
  begin
    b.Append(' via ');
    b.Append(ProviderName);
  end;
  b.Append(')');
  b.Append('</td>');
  b.Append('<td>');
  b.Append(FormatTextToHTML(FUserName));
  b.Append('</td>');

  //  created
  b.Append('<td>');
  b.Append(FormatDateTime('c', FFirstCreated));
  b.Append('</td>');
//  expires
  b.Append('<td>');
  b.Append(FormatDateTime('c', FExpires));
  b.Append('</td>');
//  next check time
  b.Append('<td>');
  b.Append(FormatDateTime('c', FNextTokenCheck));
  b.Append('</td>');
//  next use count
  b.Append('<td>');
  b.Append(inttostr(FUseCount));
  b.Append('</td>');
//  scopes
  b.Append('<td>');
  b.Append(FSecurity.source);
  b.Append('</td>');
//  compartments
  b.Append('<td>');
  first := true;
  for id in FCompartments do
  begin
    if first then
      first := false
    else
      b.Append(', ');
    b.Append(id.ToString);
  end;
  b.Append('</td>');
  b.Append('</tr>'#13#10);
end;

destructor TFhirSession.Destroy;
begin
  FTestScript.Free;
  FSecurity.Free;
  FJwt.free;
  FCompartments.Free;
  FUser.Free;
  FWOrker.Free;
  inherited;
end;

function TFhirSession.GetScopes: String;
begin
  result := FSecurity.source;
end;

function TFhirSession.isAnonymous: boolean;
begin
  result := SystemEvidence = systemUnknown;
end;

function TFhirSession.Link: TFhirSession;
begin
  result := TFhirSession(inherited Link);
end;


procedure TFhirSession.SetJwt(const Value: TJWT);
begin
  FJwt.free;
  FJwt := Value;
end;

procedure TFhirSession.setScopes(scopes: String);
begin
  FSecurity.Free;
  FSecurity := TFHIRSecurityRights.create(FWorker.Link, FUser, scopes, FSecure);
end;

procedure TFhirSession.SetTestScript(const Value: TFhirTestScriptW);
begin
  FTestScript.Free;
  FTestScript := Value;
end;

procedure TFhirSession.SetUser(const Value: TSCIMUser);
begin
  FUser.Free;
  FUser := Value;
end;


{ TFHIRFormatAdaptor }

function TFHIRFormatAdaptor.Link: TFHIRFormatAdaptor;
begin
  result := TFHIRFormatAdaptor(inherited Link);
end;


{ TFHIRCompartmentId }

constructor TFHIRCompartmentId.Create(ResourceType, id: String);
begin
  inherited Create;
  FResourceType := ResourceType;
  FId := id;
end;

function TFHIRCompartmentId.Link: TFHIRCompartmentId;
begin
  result := TFHIRCompartmentId(inherited Link);
end;

function TFHIRCompartmentId.ToString: String;
begin
  result := FResourceType+'/'+FId;
end;

Function IdTail(s : String):String;
begin
  if (pos('/', s) = 0) then
    result := s
  else
  begin
    result := copy(s, LastDelimiter('/', s)+1, $FF);
    if result[1] = '@' then
      delete(result, 1, 1);
  end;
end;

Function IdHead(s : String):String;
begin
  if (pos('/', s) > 0) then
    result := copy(s, 1, LastDelimiter('/', s))
  else
    result := '';
end;


end.



