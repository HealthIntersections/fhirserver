unit FHIROperation;

{
Copyright (c) 2001-2013, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{
todo:

Grahame, I see you don't respond to either of the following:
http://hl7connect.healthintersections.com.au/svc/fhir/condition/search?subject=patient/350
http://hl7connect.healthintersections.com.au/svc/fhir/condition/search?subject=patient/@350

}

uses
  SysUtils, Classes,
  RegExpr, KDate, HL7V2DateSupport, DateAndTime, ParseMap, KCritSct, TextUtilities, ZLib,
  DateSupport, StringSupport, EncodeSupport, GuidSupport, BytesSupport,
  KDBManager, KDBDialects,
  AdvObjects, AdvIntegerObjectMatches, AdvMemories, AdvBuffers, AdvVclStreams, AdvStringObjectMatches, AdvStringMatches,
  AdvStringBuilders, AdvObjectLists, AdvNames,

  FHIRBase, FHIRSupport, FHIRResources, FHIRConstants, FHIRComponents, FHIRTypes, FHIRAtomFeed, FHIRParserBase,
  FHIRParser, FHIRUtilities, FHIRLang, FHIRIndexManagers, FHIRValidator, FHIRValueSetExpander, FHIRTags, FHIRDataStore;

const
  SEARCH_PARAM_NAME_ID = 'search-id';
  SEARCH_PARAM_NAME_OFFSET = 'search-offset';
  SEARCH_PARAM_NAME_COUNT = '_count';
  SEARCH_PARAM_NAME_SORT = 'search-sort';
  SEARCH_PARAM_NAME_SUMMARY = '_summary';
  SEARCH_PAGE_DEFAULT = 50;
  SEARCH_PAGE_LIMIT = 1000;
  SUMMARY_SEARCH_PAGE_LIMIT = 10000;

  CURRENT_FHIR_STORAGE_VERSION = 2;

  RELEASE_DATE = '20131103';

type


  TCreateIdState = (idNoNew, idMaybeNew, idIsNew);

  TFHIRTransactionEntry = class (TAdvName)
  private
    id : String;
    originalId : String;
    key : integer;
    new : boolean;
    deleted : Boolean;
    resType : String;
    function summary : string;
  end;

  TFHIRTransactionEntryList = class (TAdvNameList)
  private
    function GetEntry(iIndex: Integer): TFHIRTransactionEntry;
  public
    function ExistsByTypeAndId(entry : TFHIRTransactionEntry):boolean;
    Function GetByName(oName : String) : TFHIRTransactionEntry; Overload;
    Property entries[iIndex : Integer] : TFHIRTransactionEntry Read GetEntry; Default;
  end;

  TReferenceList = class (TStringList)
  public
    procedure seeReference(id : String);
    function asSql : String;
  end;

  TAbbreviationServer = class (TAdvObject)
  private
    FIndex : integer;
  public
    function next : String;
  end;


  TFhirOperation = class (TAdvObject)
  private
    FRepository : TFHIRDataStore;
    FRequest: TFHIRRequest;
    FResponse : TFHIRResponse;
    FConnection : TKDBConnection;
    FFactory : TFHIRFactory;
    FIndexer : TFHIRIndexManager;
    FLang : String;
    FTestServer : Boolean;
    FValidate: boolean;

    FSpaces: TFHIRIndexSpaces;
    FAudits : TFhirResourceList;


    procedure SetRequest(const Value: TFHIRRequest);
    procedure SetResponse(const Value: TFHIRResponse);

    function opAllowed(resource : TFHIRResourceType; command : TFHIRCommandType) : Boolean;

    function FindSavedSearch(const sId : String; var key, link, sql : String; var total : Integer; var wantSummary : boolean): boolean;
    function BuildSearchResultSet(typekey : integer; aType : TFHIRResourceType; params : TParseMap; compartments, compartmentId : String; var link, sql : String; var total : Integer; var wantSummary : boolean):String;
    procedure ProcessDefaultSearch(typekey : integer; aType : TFHIRResourceType; params : TParseMap; compartments, compartmentId : String; id, key : string; var link, sql : String; var total : Integer; var wantSummary : boolean);
    Function ProcessParam(types : TFHIRResourceTypeSet; code : String; abbrev : TAbbreviationServer; name : String; value : String; nested : boolean; var bFirst : Boolean; var bHandled : Boolean; var wantSummary : boolean) : String;
    procedure BuildSearchForm(request: TFHIRRequest; response : TFHIRResponse);
    function GetValueSetById(id, base : String) : TFHIRValueSet;
    function GetProfileById(id : String) : TFHIRProfile;
    function GetValueSetByIdentity(id : String) : TFHIRValueSet;
    function constructValueSet(params : TParseMap; var used : String; allowNull : Boolean) : TFhirValueset;
    procedure ProcessValueSetExpansion(resource : TFHIRResource; params : TParseMap; feed : TFHIRAtomFeed; includes : TReferenceList; base : String);
    procedure ProcessValueSetValidation(resource : TFHIRResource; params : TParseMap; feed : TFHIRAtomFeed; includes : TReferenceList; base, lang : String);
    procedure ProcessConceptMapTranslation(resource : TFHIRResource; params : TParseMap; feed : TFHIRAtomFeed; includes : TReferenceList; base, lang : String);

    function FindResource(aType : TFHIRResourceType; sId : String; bAllowDeleted : boolean; var resourceKey : integer; var originalId : String; response: TFHIRResponse; compartments : String): boolean;
    function FindResourceVersion(aType : TFHIRResourceType; sId, sVersionId : String; bAllowDeleted : boolean; var resourceVersionKey : integer; response: TFHIRResponse): boolean;
    procedure NotFound(request: TFHIRRequest; response : TFHIRResponse);
    procedure VersionNotFound(request: TFHIRRequest; response : TFHIRResponse);
    procedure TypeNotFound(request: TFHIRRequest; response : TFHIRResponse);
    function GetNewResourceId(aType : TFHIRResourceType; var id : string; var key : integer):Boolean;
    function AddNewResourceId(aType : TFHIRResourceType; id : string; var resourceKey : integer) : Boolean;
    Procedure CollectIncludes(includes : TReferenceList; resource : TFHIRResource; xpath : String);
    Function TypeForKey(key : integer):TFHIRResourceType;
    Procedure LoadTags(tags : TFHIRAtomCategoryList; ResourceKey : integer);
    procedure CommitTags(tags : TFHIRAtomCategoryList; key : integer);
    Procedure ProcessBlob(response : TFHIRResponse; wantSummary : boolean);
    Function ResolveSearchId(atype : TFHIRResourceType; params : String) : String;
    procedure ScanId(entry : TFHIRAtomEntry; ids : TFHIRTransactionEntryList);
    procedure ConfirmId(entry : TFHIRAtomEntry; ids : TFHIRTransactionEntryList);
    procedure adjustReferences(base : String; entry : TFHIRAtomEntry; ids : TFHIRTransactionEntryList);
    function commitResource(context: TFHIRValidatorContext; upload : boolean; entry : TFHIRAtomEntry; id : TFHIRTransactionEntry; session : TFhirSession; resp : TFHIRAtomFeed) : boolean;

    function BuildResponseMessage(request : TFHIRRequest; incoming : TFhirMessageHeader) : TFhirMessageHeader;
    procedure ProcessMessage(request : TFHIRRequest; msg, resp : TFhirMessageHeader; feed : TFHIRAtomFeed);
    procedure ProcessMsgQuery(request: TFHIRRequest; response : TFHIRResponse; feed : TFHIRAtomFeed);
//    procedure ProcessMsgClaim(request : TFHIRRequest; incoming, outgoing: TFhirMessageHeader; infeed, outfeed: TFHIRAtomFeed);
    function MessageCreateResource(context : TFHIRValidatorContext; request : TFHIRRequest; res : TFHIRResource) : string;
    function EncodeResource(r : TFhirResource) : TBytes;
    function EncodeResourceSummary(r : TFhirResource) : TBytes;
    function EncodeFeed(r : TFHIRAtomFeed) : TBytes;

    procedure AuditRest(session : TFhirSession; ip : string; resourceType : TFhirResourceType; id, ver : String; op : TFHIRCommandType; httpCode : Integer; name, message : String);
    procedure CheckCompartments(actual, allowed : String);
    procedure ExecuteRead(request: TFHIRRequest; response : TFHIRResponse);
    function ExecuteUpdate(context: TFHIRValidatorContext; upload : boolean; request: TFHIRRequest; response : TFHIRResponse) : Boolean;
    procedure ExecuteVersionRead(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteDelete(request: TFHIRRequest; response : TFHIRResponse);
    function ExecuteValidation(context: TFHIRValidatorContext; request: TFHIRRequest; response : TFHIRResponse; opDesc : String) : boolean;
    procedure ExecuteHistory(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteSearch(request: TFHIRRequest; response : TFHIRResponse);
    Function ExecuteCreate(context: TFHIRValidatorContext; upload : boolean; request: TFHIRRequest; response : TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String;
    procedure ExecuteConformanceStmt(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteUpload(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteTransaction(upload : boolean; request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteMailBox(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteGetTags(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteUpdateTags(request: TFHIRRequest; response : TFHIRResponse);
    procedure ExecuteDeleteTags(request: TFHIRRequest; response : TFHIRResponse);
    procedure SetConnection(const Value: TKDBConnection);
    procedure checkDateFormat(s : string);
    procedure StoreAudits;
    procedure ReIndex;
    procedure clear(a : TFhirResourceTypeSet);
    function IdentifyValueset(resource : TFHIRResource; params: TParseMap; base: String; var used, cacheId: string; allowNull : boolean = false): TFHIRValueSet;
  public
    Constructor Create(lang : String; repository : TFHIRDataStore);
    Destructor Destroy; Override;
    Property Request: TFHIRRequest read FRequest write SetRequest;
    Property Response : TFHIRResponse read FResponse write SetResponse;
    Property Connection : TKDBConnection read FConnection write SetConnection;
    Property Repository : TFHIRDataStore read FRepository;

    // internal utility functions
//    procedure addParam(srch : TConformanceRestResourceSearch; html : TAdvStringBuilder; n, d : String; t : TSearchParamType; r : TSearchRepeatBehavior);
    function AddResourceToFeed(feed: TFHIRAtomFeed; sId, sType, title, link, author, base : String; updated : TDateTime; resource : TFHIRResource; originalId : String; tags : TFHIRAtomCategoryList; bIncomplete : boolean) : TFHIRAtomEntry; overload;
    function AddResourceToFeed(feed : TFHIRAtomFeed; sId, sType, base : String; originalId : String; WantSummary : Boolean) : TFHIRAtomEntry; overload;
    function AddDeletedResourceToFeed(feed : TFHIRAtomFeed; sId, sType, base : String; originalId : String) : TFHIRAtomEntry;
    function check(response : TFHIRResponse; test : boolean; code : Integer; lang, message : String) : Boolean;
    Function LoadBinaryResource(stream : TMemoryStream) : TFhirBinary;

    // called before scripts run - check for fundamental logical errors
    procedure PreCheck;

    // after scripts have been run, before kernel actually processes the request
    procedure Inspect;  virtual;

    // when want an MRN to store the resource against
    function GetPatientId : String; virtual;

    // called when kernel actually wants to process against the store
    Function Execute : String;  virtual;

    property lang : String read FLang write FLang;
    Property Validate : boolean read FValidate write FValidate;
    Property TestServer : boolean read FTestServer write FTestServer;
  end;


implementation

function ZDecompressBytes(const s: TBytes): TBytes;
var
  buffer: Pointer;
  size  : Integer;
begin
  ZDecompress(Pointer(s[0]),Length(s),buffer,size);

  SetLength(result,size);
  Move(buffer^,result[0],size);

  FreeMem(buffer);
end;


{ TFhirStorage }

constructor TFhirOperation.Create(lang : String; repository : TFHIRDataStore);
begin
  inherited Create;
  FLang := lang;
  FRepository := repository;

  FAudits := TFhirResourceList.create;
end;

destructor TFhirOperation.Destroy;
begin
  FAudits.Free;
  FIndexer.Free;
  FSpaces.free;
  FFactory.Free;
  FRequest.Free;
  FResponse.Free;
  FRepository.Free;
  inherited;
end;


function TFhirOperation.AddResourceToFeed(feed: TFHIRAtomFeed; sId, sType, title, link, author, base : String; updated : TDateTime; resource : TFHIRResource; originalId : String; tags : TFHIRAtomCategoryList; bIncomplete : boolean) : TFHIRAtomEntry;
var
  entry : TFHIRAtomEntry;
begin
  entry := TFHIRAtomEntry.Create;
  try
    entry.title := title;
    entry.links.AddValue(link, 'self');
    entry.updated := TDateAndTime.CreateUTC(updated);
    entry.id := base+CODES_TFHIRResourceType[resource.resourceType]+'/'+sId;
    entry.published_ := nowUTC;
    entry.authorName := author;
    entry.resource := resource.Link;
    entry.summary := TFhirXHtmlNode.create;
    entry.summary.NodeType := fhntElement;
    entry.originalId := originalId;
    entry.summary.Name := 'div';
    entry.categories.CopyTags(tags);
    if bIncomplete then
      entry.categories.addTag(TAG_READONLY);
    if (entry.resource.text <> nil) and (entry.resource.text.div_ <> nil) Then
      entry.summary.ChildNodes.Assign(entry.resource.text.div_.ChildNodes)
    else
      entry.summary.AddText('--'+GetFhirMessage('MSG_NO_SUMMARY', 'en')+'--');
    feed.entries.add(entry.Link);
    result := entry;
  finally
    entry.Free;
  end;
end;



function TFhirOperation.AddResourceToFeed(feed: TFHIRAtomFeed; sId, sType, base, originalId : String; WantSummary : Boolean) : TFHIRAtomEntry;
var
  parser : TFhirParser;
  stream : TStream;
  binary : TFhirBinary;
  tags : TFHIRAtomCategoryList;
begin
  if (sId = '') Then
    sId := FConnection.ColStringByName['Id'];

  tags := TFHIRAtomCategoryList.create;
  try
    tags.decodeJson(FConnection.ColMemoryByName['Tags']);
    if sType = 'Binary' then
    begin
      binary := LoadBinaryResource(FConnection.ColMemoryByName['Content']);
      try
        result := AddResourceToFeed(feed, sId, sType, 'Binary "'+sId+'" '+GetFhirMessage('NAME_VERSION', lang)+' "'+FConnection.ColStringByName['VersionId']+'"',
                      request.baseUrl+lowercase(sType)+'/'+sId+'/_history/'+FConnection.ColStringByName['VersionId'],
                      FConnection.ColStringByName['Name'], base, TSToDateTime(FConnection.ColTimeStampByName['StatedDate']), binary, originalId, tags, wantSummary);
      finally
        binary.free;
      end;
    end
    else
    begin
      if WantSummary then
      begin
        stream := FConnection.ColMemoryByName['Summary'];
        tags.AddTagDescription(TAG_SUMMARY, 'Summary')
      end
      else
        stream := FConnection.ColMemoryByName['Content'];
      stream.position := 0;
      parser := MakeParser(lang, ffXml, stream);
      try
        result := AddResourceToFeed(feed, sId, sType, GetFhirMessage(CODES_TFhirResourceType[parser.resource.resourceType], lang)+' "'+sId+'" '+GetFhirMessage('NAME_VERSION', lang)+' "'+FConnection.ColStringByName['VersionId']+'"',
                      request.baseUrl+CODES_TFhirResourceType[parser.resource.resourceType]+'/'+sId+'/_history/'+FConnection.ColStringByName['VersionId'],
                      FConnection.ColStringByName['Name'], base, TSToDateTime(FConnection.ColTimeStampByName['StatedDate']), parser.resource, originalId, tags, wantSummary);
      finally
        parser.Free;
      end;
    end;
  finally
    tags.free;
  end;
end;

function TFhirOperation.check(response: TFHIRResponse; test: boolean; code : Integer; lang, message: String): Boolean;
begin
  result := test;
  if not test then
  begin
    response.HTTPCode := code;
    response.Message := message;
    response.ContentType := 'text/plain';
    response.Body := message;
    response.feed := nil;
    response.Resource := BuildOperationOutcome(lang, message);
  end;
end;

(*
function TFhirOperation.EncodeFeedForStorage(feed: TFHIRAtomFeed; categories : TFHIRAtomCategoryList): String;
var
  s : TStringStream;
  xml : TFHIRXmlComposer;
begin
  s := TStringStream.create('');
  try
    xml := TFHIRXmlComposer.create(lang);
    try
      xml.Compose(s, feed, false);
    finally
      xml.free;
    end;
    result := #1+ZCompressStr(s.DataString) + #0 + categories.encode;
  finally
    s.Free;
  end;
end;

function TFhirOperation.EncodeResourceForStorage(resource: TFHIRResource; categories : TFHIRAtomCategoryList): String;
var
  s, sSum : TStringStream;
  xml : TFHIRXmlComposer;
begin
  s := TStringStream.create('');
  sSum := TStringStream.create('');
  try
    if resource is TFhirBinary then
    begin
      TFhirBinary(resource).Content.SaveToStream(s);
      result := #2+ZCompressStr(TFhirBinary(resource).ContentType + #0 + categories.encode+#0+s.DataString);
    end
    else
    begin
      xml := TFHIRXmlComposer.create(lang);
      try
        xml.Compose(s, '', '', resource, false);
        if Resource.hasASummary then
        begin
          xml.SummaryOnly := true;
          xml.Compose(sSum, '', '', resource, false);
          result := #4+sSum.DataString+#0+s.DataString + #0 + categories.encode;
        end
        else
          result := #3+s.DataString + #0 + categories.encode;
      finally
        xml.free;
      end;
    end;
  finally
    s.Free;
  end;
end;
*)


function TFhirOperation.EncodeFeed(r: TFHIRAtomFeed): TBytes;
var
  b : TBytesStream;
  xml : TFHIRXmlComposer;
begin
  b :=  TBytesStream.Create;
  try
    xml := TFHIRXmlComposer.Create('en');
    try
      xml.Compose(b, r, false);
    finally
      xml.Free;
    end;
    result := b.Bytes;
  finally
    b.free;
  end;
end;

function TFhirOperation.EncodeResource(r: TFhirResource): TBytes;
var
  b : TBytesStream;
  xml : TFHIRXmlComposer;
begin
  b :=  TBytesStream.Create;
  try
    xml := TFHIRXmlComposer.Create('en');
    try
      xml.Compose(b, '0', '0', r, false);
    finally
      xml.Free;
    end;
    result := copy(b.Bytes, 0, b.size);
  finally
    b.free;
  end;
end;

function TFhirOperation.EncodeResourceSummary(r: TFhirResource): TBytes;
var
  b : TBytesStream;
  xml : TFHIRXmlComposer;
begin
  b :=  TBytesStream.Create;
  try
    xml := TFHIRXmlComposer.Create('en');
    try
      xml.SummaryOnly := true;
      xml.Compose(b, '0', '0', r, false);
    finally
      xml.Free;
    end;
    result := copy(b.Bytes, 0, b.size);
  finally
    b.free;
  end;
end;

Function TFhirOperation.Execute : String;
begin
 // assert(FConnection.InTransaction);
  result := Request.Id;
  case request.CommandType of
    fcmdMailbox : ExecuteMailBox(request, response);
    fcmdRead : ExecuteRead(request, response);
    fcmdUpdate : ExecuteUpdate(nil, false, request, response);
    fcmdVersionRead : ExecuteVersionRead(request, response);
    fcmdDelete : ExecuteDelete(request, response);
    fcmdValidate : ExecuteValidation(nil, request, response, '');
    fcmdHistoryInstance, fcmdHistoryType, fcmdHistorySystem : ExecuteHistory(request, response);
    fcmdSearch : ExecuteSearch(request, response);
    fcmdCreate : result := ExecuteCreate(nil, false, request, response, idNoNew, 0);
    fcmdConformanceStmt : ExecuteConformanceStmt(request, response);
    fcmdUpload : ExecuteUpload(request, response);
    fcmdTransaction : ExecuteTransaction(false, request, response);
    fcmdGetTags : ExecuteGetTags(request, response);
    fcmdUpdateTags : ExecuteUpdateTags(request, response);
    fcmdDeleteTags : ExecuteDeleteTags(request, response);
  else
    Raise Exception.Create(GetFhirMessage('MSG_UNKNOWN_OPERATION', lang));
  End;
end;
{
procedure TFhirStorage.addParam(srch : TConformanceRestResourceSearch; html : TAdvStringBuilder; n, d : String; t : TSearchParamType; r : TSearchRepeatBehavior);
var
  param : TConformanceRestResourceSearchParam;
begin
  param := TConformanceRestResourceSearchParam.create;
  try
    param.name := TFHIRString.create(n);
    param.documentation := TFHIRString.create(d);
    param.type_ := t;
    param.repeats := r;
    srch.ParamList.add(param.link);
  finally
    param.free;
  end;
  html.append('<li>'+n+' : '+FormatTextToHTML(d)+'</li>');
end;
}

procedure TFhirOperation.ExecuteConformanceStmt(request: TFHIRRequest; response: TFHIRResponse);
var
  oConf : TFhirConformance;
  res : TFhirConformanceRestResource;
  a : TFHIRResourceType;
  html : TAdvStringBuilder;
begin
  try
    response.HTTPCode := 200;
    oConf := TFhirConformance.Create;
    response.Resource := oConf;
    oConf.date := TFhirDateTime.create(TDateAndTime.CreateUTC(UniversalDateTime));
    oConf.publisherST := 'Health Intersections'; //
    oConf.software := TFhirConformanceSoftware.Create;
    oConf.software.name := TFhirString.create('Reference Server');
    oConf.software.version := TFhirString.create(FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION);
    oConf.software.releaseDate := TFhirDateTime.create(TDateAndTime.createHL7(RELEASE_DATE));
    oConf.fhirVersionST := FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION;
    oConf.restList.add(TFhirConformanceRest.Create);
    oConf.restList[0].modeST := RestfulConformanceModeServer;
    oConf.text := TFhirNarrative.create;
    oConf.text.statusST := NarrativeStatusGenerated;

    html := TAdvStringBuilder.Create;
    try
      html.append('<div><div id="div-cnt" class="content"><h2>FHIR Reference Server Conformance Statement</h2><p>FHIR v'+FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION+' released '+RELEASE_DATE+'. '+
       'RESTful server based on FHIR '+FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION+'</p><table class="grid"><tr><th>Resource Type</th><th>Profile</th><th>Read</th><th>V-Read</th><th>Search</th><th>Update</th><th>Updates</th><th>Create</th><th>Delete</th><th>History</th></tr>'+#13#10);

      for a := Low(TFHIRResourceType) to High(TFHIRResourceType) do
      begin
        if FRepository.ResConfig[a].Supported and (a <> frtMessageHeader) then
        begin
          if a = frtBinary then
            html.append('<tr><td>'+CODES_TFHIRResourceType[a]+'</td>'+
            '<td>--</td>')
          else
            html.append('<tr><td>'+CODES_TFHIRResourceType[a]+'</td>'+
            '<td><a href="'+request.baseUrl+'profile/'+lowercase(CODES_TFHIRResourceType[a])+'?format=text/html">'+lowercase(CODES_TFHIRResourceType[a])+'</a></td>');
          res := TFhirConformanceRestResource.create;
          try
            res.type_ := TFhirCode.create(CODES_TFHIRResourceType[a]);
            if a <> frtBinary then
              res.profile := FFactory.makeReference(request.baseUrl+'Profile/'+lowercase(CODES_TFHIRResourceType[a]));
            if a <> frtMessageHeader Then
            begin
              html.append('<td align="middle"><img src="http://www.healthintersections.com.au/tick.png"/></td><td align="middle"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              res.operationList.Append.codeST := TypeRestfulOperationRead;
              res.operationList.Append.codeST := TypeRestfulOperationVread;
              res.readHistory := TFhirBoolean.create(true);
              if FRepository.ResConfig[a].cmdSearch then
              begin
                res.operationList.Append.codeST := TypeRestfulOperationSearchType;
                html.append('<td align="middle"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
              if FRepository.ResConfig[a].cmdUpdate then
              begin
                res.operationList.Append.codeST := TypeRestfulOperationUpdate;
                html.append('<td align="middle"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
              if FRepository.ResConfig[a].cmdHistoryType then
              begin
                res.operationList.Append.codeST := TypeRestfulOperationHistoryType;
                html.append('<td align="middle"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
              if FRepository.ResConfig[a].cmdCreate then
              begin
                res.operationList.Append.codeST := TypeRestfulOperationCreate;
                html.append('<td align="middle"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
              if FRepository.ResConfig[a].cmdDelete then
              begin
                res.operationList.Append.codeST := TypeRestfulOperationDelete;
                html.append('<td align="middle"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
              if FRepository.ResConfig[a].cmdHistoryInstance then
              begin
                res.operationList.Append.codeST := TypeRestfulOperationHistoryInstance;
                html.append('<td align="middle"><img src="http://www.healthintersections.com.au/tick.png"/></td>');
              end
              else
                html.append('<td></td>');
              // yet to do    res.validate := FConnection.ColIntegerByName['cmdValidate'] > 0;
  {seraching - to move elsewhere
                res.search := TConformanceRestResourceSearch.create;
                res.search.documentation := TFHIRString.create('Implemented as per the specification');
                html.append('<br/>search</td><td><ul>');
                for i := 0 to ndx(a).Indexes.count - 1 do
                  addParam(res.search, html, ndx(a).Indexes[i].Name, ndx(a).Indexes[i].Description, ndx(a).Indexes[i].SearchType, ndx(a).Indexes[i].RepeatBehavior);
                addParam(res.search, html, 'originalId', 'Original Id if one was provided when the resource was created', SearchParamTypeToken, tsrbSingle);
                html.append('</ul>');                                                                                                                               }
  //            end;
            end;
            html.append('</tr>');

              //<th>Search/Updates Params</th>
              // html.append('n : offset<br/>');
              // html.append('count : # resources per request<br/>');
              // html.append(m.Indexes[i]+' : ?<br/>');
            oConf.restList[0].resourceList.add(res.Link);
          finally
            res.free;
          end;
        end;
      end;
      FConnection.Terminate;
      html.append('</table></div></div>');
      oConf.text.div_ := ParseXhtml(lang, html.asString);
    finally
      html.free;
    end;
    AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, 500, '', e.message);
      raise;
    end;
  end;
end;

Function TFhirOperation.ExecuteCreate(context: TFHIRValidatorContext; upload : boolean; request: TFHIRRequest; response: TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String;
var
  sId : String;
  resourceKey, i : Integer;
  key : Integer;
  tnow : TDateTime;
  tags : TFHIRAtomCategoryList;
  ok : boolean;
begin
  try
    ok := true;
    if not check(response, opAllowed(request.ResourceType, fcmdCreate), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
      ok := false;

    if ok and not check(response, request.Resource <> nil, 400, lang, GetFhirMessage('MSG_RESOURCE_REQUIRED', lang)) or not check(response, request.ResourceType = request.resource.ResourceType, 400, lang, GetFhirMessage('MSG_RESOURCE_TYPE_MISMATCH', lang)) then
      ok := false;

    if ok and FValidate and not upload and (request.Session <> nil) then
    begin
      if not ExecuteValidation(context, request, response, 'Create Resource '+CODES_TFHIRResourceType[request.ResourceType]+'/'+request.Id+' ('+request.originalId+')') then
        ok := false
      else
        response.Resource := nil;
    end;

    if not ok then
      // nothing
    else if (idState = idMaybeNew) and (request.Id <> '') then
    begin
      sId := request.Id;
      if not check(response, (Length(sId) <= 36) and AddNewResourceId(request.resourceType, sId, resourceKey), 404, lang, GetFhirMessage('MSG_INVALID_ID', lang)) then
        ok := false;
    end
    else if (idState = idIsNew) then
    begin
      sid := request.id;
      resourceKey := iAssignedKey;
    end
    else if not check(response, GetNewResourceId(request.ResourceType, sId, resourceKey), 404, lang, StringFormat(GetFhirMessage('MSG_DUPLICATE_ID', lang), [sId, CODES_TFHIRResourceType[request.resourceType]])) then
       ok := false;

    if ok then
    begin
      tags := TFHIRAtomCategoryList.create;
      try
        tags.CopyTags(request.categories);
        result := sId;
        request.id := sId;
        tnow := UniversalDateTime;
        key := FRepository.NextVersionKey;
        for i := 0 to tags.count - 1 do
          FRepository.RegisterTag(tags[i], FConnection);

        FConnection.sql := 'insert into Versions (ResourceVersionKey, ResourceKey, StatedDate, TransactionDate, VersionId, Format, Deleted, SessionKey, Tags, Content, Summary) values (:k, :rk, :sd, :td, :v, :f, 0, :s, :tb, :cb, :sb)';
        FConnection.prepare;
        try
          FConnection.BindInteger('k', key);
          FConnection.BindInteger('rk', resourceKey);
          FConnection.BindTimeStamp('sd', DateTimeToTS(tnow));
          FConnection.BindTimeStamp('td', DateTimeToTS(tnow));
          request.SubId := '1';
          FConnection.BindString('v', '1');
          if request.Session <> nil then
            FConnection.BindInteger('s', request.Session.Key)
          else
            FConnection.BindInteger('s', 0);
          if request.Resource <> nil then
          begin
            FConnection.BindInteger('f', 2);
            FConnection.BindBlobFromBytes('tb', tags.json);
            FConnection.BindBlobFromBytes('cb', EncodeResource(request.Resource));
            FConnection.BindBlobFromBytes('sb', EncodeResourceSummary(request.Resource));
          end
          else
          begin
            FConnection.BindInteger('f', 1);
            FConnection.BindBlobFromBytes('tb', tags.json);
            FConnection.BindBlobFromBytes('cb', EncodeFeed(request.Feed));
            FConnection.BindNull('sb');
            Raise Exception.Create('not supported at this time');
          end;
          FConnection.Execute;
          CommitTags(tags, key);
        finally
          FConnection.Terminate;
        end;
        FConnection.ExecSQL('update Ids set MostRecent = '+inttostr(key)+' where ResourceKey = '+inttostr(resourceKey));
        CheckCompartments(FIndexer.execute(resourceKey, sId, request.resource, request.categories), request.compartments);
        FRepository.SeeResource(resourceKey, sId, request.Resource);
        if request.resourceType = frtPatient then
          FConnection.execSQL('update Compartments set CompartmentKey = '+inttostr(resourceKey)+' where Id = '''+sid+''' and CompartmentKey is null');
        response.HTTPCode := 201;
        response.Message := 'Created';
        response.ContentLocation := request.baseUrl+CODES_TFhirResourceType[request.ResourceType]+'/'+sId+'/_history/1';
        response.Location := request.baseUrl+CODES_TFhirResourceType[request.ResourceType]+'/'+sId+'/_history/1';
        response.Resource := FFactory.makeSuccessfulOperation;  // don't need to return anything, but we return this anyway
        response.categories.CopyTags(tags);
      finally
        tags.free;
      end;
    end;
    if request.ResourceType <> frtSecurityEvent then // else you never stop
      AuditRest(request.session, request.ip, request.ResourceType, sid, '1', request.CommandType, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      if request.ResourceType <> frtSecurityEvent then // else you never stop
        AuditRest(request.session, request.ip, request.ResourceType, sid, '1', request.CommandType, 500, '', e.message);
      raise;
    end;
  end;
end;

procedure TFhirOperation.ExecuteDelete(request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : Integer;
  key, nvid, i : Integer;
  originalId : String;
  tnow : TDateTime;
  tags : TFHIRAtomCategoryList;
  ok : boolean;
begin
  nvid := 0;
  try
    ok := true;
    if not check(response, opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
      ok := false;

    if ok and (not check(response, request.Resource = nil, 400, lang, GetFhirMessage('MSG_RESOURCE_NOT_ALLOWED', lang)) or
       not check(response, length(request.id) <= 36, 400, lang, StringFormat(GetFhirMessage('MSG_ID_TOO_LONG', lang), [request.id])) or
       not FindResource(request.ResourceType, request.Id, false, resourceKey, originalId, response, request.compartments)) then
      ok := false;

    if ok and FTestServer and not check(response, request.id <> 'example', 400, lang, GetFhirMessage('MSG_RESOURCE_EXAMPLE_PROTECTED', lang)) then
      ok := false;

    if ok then
    begin
      tnow := UniversalDateTime;
      key := FRepository.NextVersionKey;
      nvid := FConnection.CountSQL('Select Max(Cast(VersionId as '+DBIntType(FConnection.Owner.Platform)+')) from Versions where ResourceKey = '+IntToStr(resourceKey)) + 1;

      tags := TFHIRAtomCategoryList.create;
      try
        tags.CopyTags(request.categories);
        LoadTags(tags, ResourceKey);
        for i := 0 to tags.count - 1 do
          FRepository.RegisterTag(tags[i], FConnection);

        FConnection.sql := 'insert into Versions (ResourceVersionKey, ResourceKey, StatedDate, TransactionDate, VersionId, Format, Deleted, SessionKey, Content) values '+
                                                             '(:k,:rk, :sd, :td, :v, :f, 1, :s, :c)';
        FConnection.prepare;
        try
          FConnection.BindInteger('k', key);
          FConnection.BindInteger('rk', resourceKey);
          FConnection.BindTimeStamp('sd', DateTimeToTS(tnow));
          FConnection.BindTimeStamp('td', DateTimeToTS(tnow));
          FConnection.BindString('v', inttostr(nvid));
          Request.SubId := inttostr(nvid);
          if request.Session = nil then
            FConnection.BindInteger('s', 0)
          else
            FConnection.BindInteger('s', request.Session.Key);
          FConnection.BindInteger('f', 0);
          FConnection.BindBlobFromBytes('c', tags.json);
          FConnection.Execute;
        finally
          FConnection.Terminate;
        end;
        FConnection.ExecSQL('update Ids set MostRecent = null where ResourceKey = '+inttostr(resourceKey));
        CommitTags(tags, key);
        FConnection.execSQL('Delete from IndexEntries where ResourceKey = '+IntToStr(resourceKey));
        FRepository.DropResource(ResourceKey, request.Id, request.ResourceType);
        response.HTTPCode := 204;
        response.Message := GetFhirMessage('MSG_DELETED_DONE', lang);
        response.categories.CopyTags(tags);
      finally
        tags.free;
      end;
    end;
    AuditRest(request.session, request.ip, request.ResourceType, request.id, inttostr(nvid), request.CommandType, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, inttostr(nvid), request.CommandType, 500, '', e.message);
      raise;
    end;
  end;
end;

procedure TFhirOperation.ExecuteHistory(request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : Integer;
  feed : TFHIRAtomFeed;
  originalId : String;
  since, prior : TDateTime;
  i, offset, count : integer;
  ok : boolean;
  base : String;
  cmp : String;
begin
  try
    ok := true;
    since := MIN_DATE;
    prior := MIN_DATE;
    offset := 0;
    count := 20;

    if (request.compartments <> '') then
      cmp := ' and Ids.ResourceKey in (select ResourceKey from Compartments where CompartmentType = 1 and Id in ('+request.compartments+'))'
    else
      cmp := '';

    feed := TFHIRAtomFeed.Create;
    try
      feed.isSearch := true;
      feed.fhirBaseUrl := request.baseUrl;
      case request.CommandType of
        fcmdHistoryInstance :
          begin
            TypeNotFound(request, response);
            if not check(response, opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
              ok := false
            else if (length(request.id) > 36) or not FindResource(request.ResourceType, request.Id, true, resourceKey, originalId, response, request.compartments) then
              ok := false
            else
              FConnection.SQL := 'Select Ids.Id, VersionId, ResourceTypeKey, StatedDate, Sessions.Name, OriginalId, Deleted, Tags, Content '+
               'from Versions, Ids, Sessions '+
               'where Versions.ResourceKey = '+inttostr(resourceKey)+' '+cmp+' and Versions.ResourceKey = Ids.ResourceKey and Versions.SessionKey = Sessions.SessionKey';
            feed.title := StringFormat(GetFhirMessage('MSG_HISTORY', lang), [CODES_TFHIRResourceType[request.ResourceType]+' '+GetFhirMessage('NAME_RESOURCE', lang)+' '+request.Id]);
            base := request.baseUrl+''+CODES_TFhirResourceType[request.ResourceType]+'/'+request.id+'/_history?';
          end;
        fcmdHistoryType :
          begin
            TypeNotFound(request, response);
            if not check(response, opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
              ok := false
            else
            begin
              FConnection.SQL := 'Select Ids.Id, VersionId, ResourceTypeKey, StatedDate, Sessions.Name, OriginalId, Deleted, Tags, Content from Versions, Ids, Sessions '+
              'where Ids.ResourceTypeKey = '+inttostr(FRepository.ResConfig[request.ResourceType].key)+' '+cmp+' and Ids.MostRecent = Versions.ResourceVersionKey and Versions.SessionKey = Sessions.SessionKey';
              feed.title := StringFormat(GetFhirMessage('MSG_HISTORY', lang), [CODES_TFHIRResourceType[request.ResourceType]]);
              base := request.baseUrl+''+CODES_TFhirResourceType[request.ResourceType]+'/_history?';
            end;
          end;
        fcmdHistorySystem :
          begin
            FConnection.SQL := 'Select Ids.Id, VersionId, ResourceTypeKey, StatedDate, Sessions.Name, OriginalId, Deleted, Tags, Content from Versions, Ids, Sessions '+
              'where Ids.MostRecent = Versions.ResourceVersionKey '+cmp+' and Versions.SessionKey = Sessions.SessionKey';
            feed.title := StringFormat(GetFhirMessage('MSG_HISTORY', lang), [GetFhirMessage('NAME_SYSTEM', lang)]);
            base := request.baseUrl+'_history?';
          end;
      else
        FConnection.SQL := '';
      end;

      if ok then
      begin
        for i := 0 to request.Parameters.getItemCount - 1 do
          if request.Parameters.VarName(i) = '_offset' then
            offset := StrToIntDef(request.Parameters.Value[request.Parameters.VarName(i)], 0)
          else if request.Parameters.VarName(i) = '_count' then
            count := StrToIntDef(request.Parameters.Value[request.Parameters.VarName(i)], 0)
          else if request.Parameters.VarName(i) = '_since' then
          begin
            since := DTReadDateTZ('yyyy-mm-ddThh:nn:ss.zzzzzz', request.Parameters.Value[request.Parameters.VarName(i)], false);
            FConnection.SQL := FConnection.SQL + ' and StatedDate >= :since ';
            base := base +'_since='+request.Parameters.Value[request.Parameters.VarName(i)];
          end
          else if request.Parameters.VarName(i) = '_prior' then
          begin
            prior := DTReadDateTZ('yyyy-mm-ddThh:nn:ss.zzzzzz', request.Parameters.Value[request.Parameters.VarName(i)], false);
            FConnection.SQL := FConnection.SQL + ' and StatedDate <= :prior ';
            base := base +'_prior='+request.Parameters.Value[request.Parameters.VarName(i)];
          end;
        if (prior = MIN_DATE) then
          base := base +'_prior='+FormatDateTime('yyyy-mm-dd''T''hh:nn:ss''Z''', UniversalDateTime, FormatSettings);

        if count < 1 then count := 1;
        if offset < 0 then offset := 0;
        FConnection.SQL := FConnection.SQL +' order by ResourceVersionKey Desc OFFSET '+inttostr(offset)+' ROWS FETCH NEXT '+IntToStr(count+1)+' ROWS ONLY';
        FConnection.Prepare;
        try
          if since <> MIN_DATE then
            FConnection.BindTimeStamp('since', DateTimeToTS(since));
          if prior <> MIN_DATE then
            FConnection.BindTimeStamp('prior', DateTimeToTS(prior));

          FConnection.Execute;
          i := 0;
          while FConnection.FetchNext do
          begin
            inc(i);
            if i > count then
              feed.links.rel['next'] := base + '&_offset='+inttostr(offset+count)+'&_count='+inttostr(count)
            else if Connection.ColIntegerByName['Deleted'] = 1 then
              AddDeletedResourceToFeed(feed, FConnection.ColStringByName['Id'], CODES_TFhirResourceType[TypeForKey(FConnection.ColIntegerByName['ResourceTypeKey'])], request.baseURL, FConnection.ColStringByName['OriginalId'])
            else
              AddResourceToFeed(feed, FConnection.ColStringByName['Id'], CODES_TFhirResourceType[TypeForKey(FConnection.ColIntegerByName['ResourceTypeKey'])], request.baseURL, FConnection.ColStringByName['OriginalId'], false);
          end;
        finally
          FConnection.Terminate;
        end;
        feed.updated := NowUTC;
        feed.id := 'urn:uuid:'+FhirGUIDToString(CreateGUID);
        feed.links.rel['self'] := base + '&_offset='+inttostr(offset)+'&_count='+inttostr(count);
        feed.links.rel['first'] := base + '&_offset=0&_count='+inttostr(count);
        feed.sql := FConnection.sql;
        response.HTTPCode := 200;
        response.Message := 'OK';
        response.Body := '';
        response.Feed := feed.Link;
      end;
    finally
      feed.Free;
    end;
    AuditRest(request.session, request.ip, request.ResourceType, request.id, '', request.CommandType, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, '', request.CommandType, 500, '', e.message);
      raise;
    end;
  end;
end;

procedure TFhirOperation.ExecuteRead(request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : integer;
  originalId : String;
begin
  try
    NotFound(request, response);
    if check(response, opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if (length(request.id) <= 36) and FindResource(request.ResourceType, request.Id, false, resourceKey, originalId, response, request.compartments) then
      begin
        FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc';
        FConnection.Prepare;
        try
          FConnection.Execute;
          if FConnection.FetchNext then
          Begin
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
            response.versionId := FConnection.GetColStringByName('VersionId');
            response.originalId := originalId;
            response.LastModifiedDate := TSToDateTime(FConnection.ColTimeStampByName['StatedDate']);
            response.ContentLocation := request.baseUrl+CODES_TFhirResourceType[request.ResourceType]+'/'+request.id+'/_history/'+response.versionId;
            processBlob(Response, false);
          end;
        finally
          FConnection.Terminate;
        end;
      end;
    end;
    AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, response.versionId, request.CommandType, 500, '', e.message);
      raise;
    end;
  end;
end;


Function TFhirOperation.processParam(types : TFHIRResourceTypeSet; code : String; abbrev : TAbbreviationServer; name : String; value : String; nested : boolean; var bFirst : Boolean; var bHandled : Boolean; var wantSummary : boolean) : String;
var
  key, i : integer;
  left, right, op, modifier : String;
  f, dummy : Boolean;
  ts : TStringList;
  sc : String;
  pfx, sfx : String;
  date : TDateAndTime;
  a : TFHIRResourceType;
  type_ : TFhirSearchParamType;
begin
  date := nil;
  result := '';
  op := '';
  bHandled := false;
  if (value = '') then
    exit;

  if (name = '_summary') and (value = 'true') then
  begin
    bHandled := true;
    wantSummary := true;
  end
  else if pos('.', name) > 0 then
  begin
    StringSplit(name, '.', left, right);
    if (pos(':', left) > 0) then
    begin
      StringSplit(left, ':', left, modifier);
      if not StringArrayExistsSensitive(CODES_TFHIRResourceType, modifier) then
        raise Exception.create(StringFormat(GetFhirMessage('MSG_UNKNOWN_TYPE', lang), [modifier]));
      a := TFHIRResourceType(StringArrayIndexOfSensitive(CODES_TFHIRResourceType, modifier));
      types := [a];
    end
    else
      modifier := '';
    key := FIndexer.GetKeyByName(types, left);
    if key = 0 then
      raise Exception.create(StringFormat(GetFhirMessage('MSG_PARAM_CHAINED', lang), [left]));
    f := true;
    sc := abbrev.next;
    if modifier <> '' then
      result := result + '(IndexKey = '+inttostr(Key)+' and target in (select ResourceKey from IndexEntries where (ResourceKey in (select ResourceKey from Ids where ResourceTypeKey = '+inttostr(FRepository.ResConfig[a].key)+')) and ('+processParam(FIndexer.GetTargetsByName(types, left), sc, abbrev, right, lowercase(value), true, f, bHandled, dummy)+')))'
    else
      result := result + '(IndexKey = '+inttostr(Key)+' and target in (select ResourceKey from IndexEntries where '+processParam(FIndexer.GetTargetsByName(types, left), sc, abbrev, right, lowercase(value), true, f, bHandled, dummy)+'))';
    bHandled := true;
  end
  else
  begin
    //remove the modifier:
    if (pos(':', name) > 0) then
      StringSplit(name, ':', name, modifier);

    if name = 'originalId' then
    begin
      bHandled := true;
      if modifier <> '' then
        raise exception.create('modifier "'+modifier+'" not handled on originalId');
      result :=  '(originalId = '''+sqlwrapString(value)+''')';
    end
    else if name = 'tag' then
    begin
      bHandled := true;
      if modifier = 'partial' then
        result :=  '(MostRecent in (Select ResourceVersionKey from VersionTags where TagKey in (Select TagKey from Tags where SchemeUri = '''+FHIR_TAG_SCHEME+''' and TermUri like '''+SQLWrapString(value)+'%'')))'
      else if modifier = 'text' then
        result :=  '(MostRecent in (Select ResourceVersionKey from VersionTags where Label like ''%'+SQLWrapString(value)+'%''))'
      else if modifier = '' then
        result :=  '(MostRecent in (Select ResourceVersionKey from VersionTags where TagKey = '''+intToStr(FRepository.KeyForTag(FHIR_TAG_SCHEME, value))+'''))'
      else
        raise exception.create('modifier "'+modifier+'" not handled on tag');
    end
    else
    begin
      key := FIndexer.GetKeyByName(types, name);
      if key > 0 then
      begin
        type_ := FIndexer.GetTypeByName(types, name);
        if modifier = 'missing' then
        begin
          bHandled := true;
          if StrToBoolDef(value, false) then
            result := result + '((Select count(*) from IndexEntries where IndexKey = '+inttostr(Key)+' and '+code+'.ResourceKey = z.ResourceKey) = 0)'
          else
            result := result + '((Select count(*) from IndexEntries where IndexKey = '+inttostr(Key)+' and '+code+'.ResourceKey = z.ResourceKey) > 0)';
        end
        else
        begin
          ts := TStringlist.create;
          try
            ts.CommaText := value;
            if (ts.count > 1) then
              result := '(';
            for i := 0 to ts.count - 1 do
            begin
              if i > 0 then
                result := result +') or (';
              value := ts[i];
              if i > 0 then
                result := result + op;
              bHandled := true;
              case type_ of
                SearchParamTypeDate:
                  begin
                    CheckDateFormat(value);
                    date := TDateAndTime.CreateXML(value);
                    try
                      if modifier = '' then
                        result := result + '(IndexKey = '+inttostr(Key)+' and Value >= '''+date.AsUTCDateTimeMinHL7+''' and Value2 <= '''+date.AsUTCDateTimeMaxHL7+''')'
                      else if modifier = 'before' then
                        result := result + '(IndexKey = '+inttostr(Key)+' and Value <= '''+date.AsUTCDateTimeMaxHL7+''')'
                      else if modifier = 'after' then
                        result := result + '(IndexKey = '+inttostr(Key)+' and Value2 >= '''+date.AsUTCDateTimeMinHL7+''')'
                      else
                        raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [modifier]));
                    finally
                      date.free;
                      date := nil;
                    end;
                  end;
                SearchParamTypeString:
                  begin
                    if name = 'phonetic' then
                     value := EncodeNYSIIS(value);
                    if (modifier = 'partial') or (modifier = '') then
                    begin
                      pfx := 'like ''%';
                      sfx := '%''';
                    end
                    else if (modifier = 'exact') then
                    begin
                      pfx := '= ''';
                      sfx := '''';
                    end
                    else
                      raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [modifier]));
                    result := result + '(IndexKey = '+inttostr(Key)+' and Value '+pfx+sqlwrapString(value)+sfx+')';
                  end;
                SearchParamTypeToken:
                  begin
                  // _id is a special case
                  if (name = '_id') or (FIndexer.GetTypeByName(types, name) = SearchParamTypeToken) then
                    result := result + '(IndexKey = '+inttostr(Key)+' and Value = '''+sqlwrapString(value)+''')'
                  else if modifier = 'text' then
                    result := result + '(IndexKey = '+inttostr(Key)+' and Value2 like ''%'+sqlwrapString(value)+'%'')'
                  else if (modifier = 'code') or ((modifier = '') and (pos('/', value) > 0)) then
                  begin
                    StringSplitRight(value, '/', left, right);
                    result := result + '(IndexKey = '+inttostr(Key)+' and SpaceKey = (Select SpaceKey from Spaces where Space = '''+sqlwrapstring(left)+''') and Value = '''+sqlwrapString(right)+''')';
                  end
                  else if modifier = 'anyns' then
                    result :=  result + '(IndexKey = '+inttostr(Key)+' and Value = '''+sqlwrapString(value)+''')'
                  else
                    raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [modifier]))
                  end;
                SearchParamTypeReference :
                  begin
                  // _id is a special case
                  if (name = '_id') or (FIndexer.GetTypeByName(types, name) = SearchParamTypeToken) then
                    result := result + '(IndexKey = '+inttostr(Key)+' and Value = '''+sqlwrapString(value)+''')'
                  else if modifier = 'text' then
                    result := result + '(IndexKey = '+inttostr(Key)+' and Value2 like ''%'+sqlwrapString(value)+'%'')'
                  else if (modifier = 'anyns') or (modifier = '') then
                    result :=  result + '(IndexKey = '+inttostr(Key)+' and Value = '''+sqlwrapString(value)+''')'
                  else
                    raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [modifier]))
                  end;
              else
                raise exception.create('not done yet: type = '+CODES_TFhirSearchParamType[type_]);
              end;
      {
      todo: renable for quantity with right words etc + ucum
              else if (right = 'before') or (right = 'after') then
              begin
                bHandled := true;
                if (right = 'before') then
                  op := '<='
                else if (right = 'after') then
                  op := '>='
                else
                  op := '=';
                result := result + '(IndexKey = '+inttostr(Key)+' and Value '+op+' '''+sqlwrapString(value)+''')';
              end
      }
            end;
          if ts.count > 1 then
            result := result + ')';
          finally
            ts.free;
          end;
        end;
      end;
    end;
  end;

  if result <> '' then
  begin
    if not nested and (name <> 'tag') then
      result := 'Ids.ResourceKey in (select ResourceKey from IndexEntries where '+result+')';
    if not bfirst then
      result := ' and '+result;
  end;
  bfirst := bfirst and (result = '');
end;


function TFhirOperation.FindSavedSearch(const sId : String; var key, link, sql : String; var total : Integer; var wantSummary : boolean): boolean;
begin
  if sId = '' then
    result := false
  else
  begin
    FConnection.sql := 'Select SearchKey, Count, Summary, Link, SqlCode  from Searches where Id = :s';
    FConnection.Prepare;
    try
      FConnection.BindString('s', sId);
      FConnection.Execute;
      result := FConnection.FetchNext;
      if result then
      begin
        key := FConnection.GetColStringByName('SearchKey');
        total := FConnection.GetColIntegerByName('Count');
        link := FConnection.GetColStringByName('Link');
        sql := FConnection.GetColStringByName('SqlCode');
        wantSummary := FConnection.GetColIntegerByName('Summary') = 1;
      end;
    finally
      FConnection.Terminate;
    end;
    FConnection.ExecSQL('update Searches set date = '+DBGetDate(FConnection.Owner.Platform)+' where SearchKey = '+key);
  end;
end;

function TFhirOperation.BuildSearchResultSet(typekey : integer; aType : TFHIRResourceType; params : TParseMap; compartments, compartmentId : String; var link, sql : String; var total : Integer; var wantSummary : boolean):String;
var
  id : string;
begin
  id := FhirGUIDToString(CreateGuid);
  result := inttostr(FRepository.NextSearchKey);
  if params.VarExists('_query') then
  begin
    raise exception.create('no defined queries are implemented at this time');
  end
  else
    ProcessDefaultSearch(typekey, aType, params, compartments, compartmentId, id, result, link, sql, total, wantSummary);
end;

function booleanToSQL(b : boolean): string;
begin
  if b then
    result := '1'
  else
    result := '0';
end;

procedure TFhirOperation.ProcessDefaultSearch(typekey : integer; aType : TFHIRResourceType; params : TParseMap; compartments, compartmentId : String; id, key : string; var link, sql : String; var total : Integer; var wantSummary : boolean);
var
  s, ss : String;
  first : boolean;
  handled : boolean;
  i, j : integer;
  ix : TFhirIndex;
  abbrev : TAbbreviationServer;
  ts : TStringList;
  code : String;
begin
  FConnection.ExecSQL('insert into Searches (SearchKey, Id, Count, Type, Date, Summary) values ('+key+', '''+id+''', 0, 1, '+DBGetDate(FConnection.Owner.Platform)+', '+booleanToSQl(wantSummary)+')');

  if typekey = 0 then
    s := 'Ids.MasterResourceKey is null'
  else
    s := 'Ids.MasterResourceKey is null and Ids.ResourceTypeKey = '+inttostr(typekey);

  if (compartmentId <> '') then
    s := s +' and Ids.ResourceKey in (select ResourceKey from Compartments where CompartmentType = 1 and Id = '''+compartmentId+''')';

  if (compartments <> '') then
    s := s +' and Ids.ResourceKey in (select ResourceKey from Compartments where CompartmentType = 1 and Id in ('+compartments+'))';

  link := SEARCH_PARAM_NAME_ID+'='+id;
  first := false;
  abbrev := TAbbreviationServer.create;
  ts := TStringList.create;
  try
    code := abbrev.next;
    for i := 0 to params.Count - 1 do
    begin
      ts.Clear;
      ts.assign(params.List(i));
      for j := ts.count - 1 downto 0 do
        if ts[j] = '' then
          ts.delete(j);
      for j := 0 to ts.count - 1 do
      begin
        handled := false;
        s := s + processParam([aType], code, abbrev, params.VarName(i), lowercase(ts[j]), false, first, handled, wantSummary);
        if handled then
          link := link + '&'+params.VarName(i)+'='+EncodeMIME(ts[j]);
      end;
    end;
  finally
    ts.free;
    abbrev.Free;
  end;

  if params.VarExists(SEARCH_PARAM_NAME_SORT) and (params.Value[SEARCH_PARAM_NAME_SORT] <> '_id') then
  begin
    ix := FIndexer.Indexes.getByName(aType, params.Value[SEARCH_PARAM_NAME_SORT]);
    if (ix = nil) then
      Raise Exception.create(StringFormat(GetFhirMessage('MSG_SORT_UNKNOWN', lang), [params.Value[SEARCH_PARAM_NAME_SORT]]));
    ss :='(SELECT Min(Value) FROM IndexEntries WHERE IndexEntries.ResourceKey = Ids.ResourceKey and IndexKey = '+inttostr(ix.Key)+')';
    link := link+'&'+SEARCH_PARAM_NAME_SORT+'='+ix.Name;
  end
  else
  begin
    ss := 'Id';
    link := link+'&'+SEARCH_PARAM_NAME_SORT+'=_id';
  end;

  s := 'Insert into SearchEntries Select '+key+', ResourceKey, '+ss+' from Ids where '+s+' order by ResourceKey DESC';
  FConnection.ExecSQL(s);
  sql := s;
  total := FConnection.CountSQL('Select count(*) from SearchEntries where SearchKey = '+key);
  FConnection.Sql := 'update Searches set Link = :l, SqlCode = :s, count = '+inttostr(total)+', Summary = '+booleanToSQL(wantSummary)+' where SearchKey = '+key;
  FConnection.Prepare;
  try
    FConnection.BindBlobFromString('l', link);
    FConnection.BindBlobFromString('s', s);
    FConnection.Execute;
  finally
    FConnection.Terminate;
  end;
end;


procedure TFhirOperation.ExecuteSearch(request: TFHIRRequest; response: TFHIRResponse);
var
  feed : TFHIRAtomFeed;
  entry : TFHIRAtomEntry;
  includes : TReferenceList;
  id, link, base, sql : String;
  i, total, t : Integer;
  key : integer;
  offset, count : integer;
  ok, wantsummary : boolean;
begin
  try
    ok := true;
    wantsummary := false;
    count := 0;
    offset := 0;
    { todo:
     conformance
     quantity searches
    }
    if not check(response, opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
      // ok := false
    else if (request.Parameters.getItemCount = 0) and (response.Format = ffXhtml) and (request.compartmentId = '') then
      BuildSearchForm(request, response)
    else
    begin
      TypeNotFound(request, response);
      if request.resourceType <> frtNull then
      begin
        key := FConnection.CountSQL('select ResourceTypeKey from Types where supported = 1 and ResourceName = '''+CODES_TFHIRResourceType[request.resourceType]+'''');
        if not check(response, key > 0, 404, lang, 'Resource Type '+CODES_TFHIRResourceType[request.resourceType]+' not known') then
          ok := false;
      end
      else
        key := 0;

      if ok then
      begin
        feed := TFHIRAtomFeed.Create;
        includes := TReferenceList.create;
        try
          feed.fhirBaseUrl := request.baseUrl;
          feed.isSearch := true;
          // special query support
          if (request.ResourceType = frtValueSet) and (request.Parameters.GetVar('_query') = 'expand') then
            ProcessValueSetExpansion(request.resource, request.parameters, feed, includes, request.baseUrl) // need to set feed title, self link, search total. may add includes
          else if (request.ResourceType = frtValueSet) and (request.Parameters.GetVar('_query') = 'validate') then
            ProcessValueSetValidation(request.resource, request.parameters, feed, includes, request.baseUrl, request.Lang) // need to set feed title, self link, search total. may add includes
          else if (request.ResourceType = frtConceptMap) and (request.Parameters.GetVar('_query') = 'translate') then
            ProcessConceptMapTranslation(request.resource, request.parameters, feed, includes, request.baseUrl, request.Lang) // need to set feed title, self link, search total. may add includes
          else
          // standard query
          begin
            if FindSavedSearch(request.parameters.value[SEARCH_PARAM_NAME_ID], id, link, sql, total, wantSummary) then
              link := SEARCH_PARAM_NAME_ID+'='+request.parameters.value[SEARCH_PARAM_NAME_ID]
            else
              id := BuildSearchResultSet(key, request.resourceType, request.Parameters, request.compartments, request.compartmentId, link, sql, total, wantSummary);

            feed.SearchTotal := total;
            feed.SearchOffset := offset;
            feed.SearchCount := count;
            feed.sql := sql;
            base := AppendForwardSlash(Request.baseUrl)+CODES_TFhirResourceType[request.ResourceType]+'/_search?';
            if response.Format <> ffAsIs then
              base := base + '_format='+MIMETYPES_TFHIRFormat[response.Format]+'&';
            feed.links.AddValue(base+link, 'self');

            offset := StrToIntDef(request.Parameters.getVar(SEARCH_PARAM_NAME_OFFSET), 0);
            if request.Parameters.getVar(SEARCH_PARAM_NAME_COUNT) = 'all' then
              count := SUMMARY_SEARCH_PAGE_LIMIT
            else
              count := StrToIntDef(request.Parameters.getVar(SEARCH_PARAM_NAME_COUNT), 0);
            if (count < 2) then
              count := SEARCH_PAGE_DEFAULT
            else if wantsummary and (Count > SUMMARY_SEARCH_PAGE_LIMIT) then
              count := SUMMARY_SEARCH_PAGE_LIMIT
            else if (Count > SEARCH_PAGE_LIMIT) then
              count := SEARCH_PAGE_LIMIT;

            if (offset > 0) or (Count < total) then
            begin
              feed.links.AddValue(base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'=0&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count), 'first');
              if offset - count >= 0 then
                feed.links.AddValue(base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset - count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count), 'previous');
              if offset + count < total then
                feed.links.AddValue(base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset + count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count), 'next');
              feed.links.AddValue(base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(total - count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count), 'last');
            end;

            FConnection.SQL := 'Select Ids.Id, VersionId, StatedDate, Name, originalId, Tags, Content, Summary from Versions, Ids, Sessions, SearchEntries '+
                'where Ids.MostRecent = Versions.ResourceVersionKey and Versions.SessionKey = Sessions.SessionKey and SearchEntries.ResourceKey = Ids.ResourceKey and SearchEntries.SearchKey = '+id+' '+
                'order by SortValue ASC';
            FConnection.Prepare;
            try
              FConnection.Execute;
              i := 0;
              t := 0;
              while FConnection.FetchNext do
              Begin
                inc(i);
                if (i > offset) then
                begin
                  entry := AddResourceToFeed(feed, '', CODES_TFHIRResourceType[request.ResourceType], request.baseUrl, FConnection.colstringByName['originalId'], wantsummary);
                  if request.Parameters.VarExists('_include') then
                    CollectIncludes(includes, entry.resource, request.Parameters.GetVar('_include'));
                  inc(t);
                end;
                if (t = count) then
                  break;
              End;
            finally
              FConnection.Terminate;
            end;
            if request.resourceType = frtNull then
              feed.title := GetFhirMessage('MSG_SEARCH_RESULTS_ALL', lang)
            else
              feed.title := StringFormat(GetFhirMessage('MSG_SEARCH_RESULTS', lang), [CODES_TFHIRResourceType[request.resourceType]]);
          end;
          //now, add the includes
          if includes.Count > 0 then
          begin
            FConnection.SQL := 'Select ResourceTypeKey, Ids.Id, VersionId, StatedDate, Name, originalId, Tags, Content from Versions, Sessions, Ids '+
                'where Ids.MostRecent = Versions.ResourceVersionKey and Versions.SessionKey = Sessions.SessionKey '+
                'and Ids.ResourceKey in (select ResourceKey from IndexEntries where '+includes.asSql+') order by ResourceVersionKey DESC';
            FConnection.Prepare;
            try
              FConnection.Execute;
              while FConnection.FetchNext do
                AddResourceToFeed(feed, '', CODES_TFHIRResourceType[TypeForKey(FConnection.ColIntegerByName['ResourceTypeKey'])], request.baseUrl, FConnection.ColStringByName['originalId'], wantSummary);
            finally
              FConnection.Terminate;
            end;
          end;

          feed.updated := NowUTC;
          feed.id := 'urn:uuid:'+FhirGUIDToString(CreateGUID);
          //feed.links['self'] := request.url;
          response.HTTPCode := 200;
          response.Message := 'OK';
          response.Body := '';
          response.Feed := feed.Link;
        finally
          includes.free;
          feed.Free;
        end;
      end;
    end;
    AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, response.httpCode, request.Parameters.Source, response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, 500, request.Parameters.Source, e.message);
      raise;
    end;
  end;
end;


function TFhirOperation.ExecuteUpdate(context: TFHIRValidatorContext; upload : boolean; request: TFHIRRequest; response: TFHIRResponse) : boolean;
var
  resourceKey : Integer;
  key, nvid, i : Integer;
  s, originalId : String;
  tnow : TDateTime;
  tags : TFHIRAtomCategoryList;
  ok : boolean;
begin
  nvid := 0;
  key := 0;
  try
    ok := true;

    if ok and (not check(response, request.Resource <> nil, 400, lang, GetFhirMessage('MSG_RESOURCE_REQUIRED', lang)) or
       not check(response, request.ResourceType = request.resource.ResourceType, 400, lang, GetFhirMessage('MSG_RESOURCE_TYPE_MISMATCH', lang)) or
       not check(response, length(request.id) <= 36, 400, lang, StringFormat(GetFhirMessage('MSG_ID_TOO_LONG', lang), [request.id]))) then
      ok := false;

    if ok and FValidate and not upload then
    begin
      if not ExecuteValidation(context, request, response, 'Update Resource '+CODES_TFHIRResourceType[request.ResourceType]+'/'+request.Id) then
        ok := false
      else
        response.Resource := nil;
    end;

    result := true;

    if ok and not FindResource(request.ResourceType, request.Id, true, resourceKey, originalId, response, request.compartments) Then
    begin
      ExecuteCreate(context, upload, request, response, idMaybeNew, 0);
      result := false;
      ok := false;
    end;

    if ok and not check(response, opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
      ok := false;

    if ok then
    begin
      key := FRepository.NextVersionKey;
      nvid := FConnection.CountSQL('Select Max(Cast(VersionId as '+DBIntType(FConnection.Owner.Platform)+')) from Versions where ResourceKey = '+IntToStr(resourceKey)) + 1;

      if FRepository.ResConfig[request.ResourceType].versionUpdates then
      begin
        s := request.contentLocation;
        if not check(response, s <> '', 412, lang, GetFhirMessage('MSG_VERSION_AWARE', lang)) or
           not check(response, StringStartsWith(s, request.baseUrl), 412, lang, GetFhirMessage('MSG_VERSION_AWARE_URL', lang)) or
           not check(response, IsId(copy(s, pos('/_history/', s) + 10, $FFFF)), 412, lang, GetFhirMessage('MSG_VERSION_AWARE_URL', lang)) then
          exit;
        s := copy(s, pos('/_history/', s) + 10, $FFFF);
        if not check(response, s = inttostr(nvid-1), 412, lang, StringFormat(GetFhirMessage('MSG_VERSION_AWARE_CONFLICT', lang), [inttostr(nvid-1), s])) then
          ok := false;
      end;
    end;

    if ok then
    begin
      tags := TFHIRAtomCategoryList.create;
      try
        tags.CopyTags(request.categories);
        LoadTags(tags, ResourceKey);
        for i := 0 to tags.count - 1 do
          FRepository.RegisterTag(tags[i], FConnection);

        FConnection.execSQL('Delete from IndexEntries where ResourceKey = '+IntToStr(resourceKey));

        // check whether originalId matches?
        // to do: merging

        tnow := UniversalDateTime;
        FConnection.sql := 'insert into Versions (ResourceVersionKey, ResourceKey, TransactionDate, StatedDate, Format, VersionId, Deleted, SessionKey, Tags, Content, Summary) values (:k, :rk, :td, :sd, :f, :v, 0, :s, :tb, :cb, :sb)';
        FConnection.prepare;
        try
          FConnection.BindInteger('k', key);
          FConnection.BindInteger('rk', resourceKey);
          FConnection.BindTimeStamp('td', DateTimeToTS(tnow));
          FConnection.BindTimeStamp('sd', DateTimeToTS(tnow));
          FConnection.BindString('v', inttostr(nvid));
          request.SubId := inttostr(nvid);
          if request.Session = nil then
            FConnection.BindNull('s')
          else
            FConnection.BindInteger('s', request.Session.Key);
          if request.Resource <> nil then
          begin
            FConnection.BindInteger('f', 2);
            FConnection.BindBlobFromBytes('tb', tags.json);
            FConnection.BindBlobFromBytes('cb', EncodeResource(request.Resource));
            FConnection.BindBlobFromBytes('sb', EncodeResourceSummary(request.Resource));
          end
          else
          begin
            FConnection.BindInteger('f', 1);
            FConnection.BindBlobFromBytes('tb', tags.json);
            FConnection.BindBlobFromBytes('cb', EncodeFeed(request.Feed));
            FConnection.BindNull('sb');
          end;
          FConnection.Execute;
        finally
          FConnection.Terminate;
        end;
        FConnection.ExecSQL('update Ids set MostRecent = '+inttostr(key)+' where ResourceKey = '+inttostr(resourceKey));
        CommitTags(tags, key);
        FIndexer.execute(resourceKey, request.id, request.resource, request.categories);
        FRepository.SeeResource(resourceKey, request.id, request.Resource);

        if response.Feed <> nil then
          response.Feed.entries.add(request.Resource.Link)
        else
          response.Resource := request.Resource.Link;

        response.categories.CopyTags(tags);
      finally
        tags.free;
      end;
      response.HTTPCode := 200;
      response.Message := 'OK';
      response.Resource := FFactory.makeSuccessfulOperation; // don't need to return anything, but we return this anyway
      response.ContentLocation := request.baseUrl+CODES_TFhirResourceType[request.ResourceType]+'/'+request.Id+'/_history/'+inttostr(nvid);
    end;
    AuditRest(request.session, request.ip, request.ResourceType, request.id, inttostr(nvid), request.CommandType, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, inttostr(nvid), request.CommandType, 500, '', e.message);
      raise;
    end;
  end;
end;


function TFhirOperation.ExecuteValidation(context: TFHIRValidatorContext; request: TFHIRRequest; response: TFHIRResponse; opDesc : String) : boolean;
var
  outcome : TFhirOperationOutcome;
  i : integer;
  buffer : TAdvBuffer;
  mem : TAdvMemoryStream;
  xml : TFHIRComposer;
  vcl : TVclStream;
  profileId : String;
  profile : TFHIRProfile;
begin
  profileId := '';
  profile := nil;
  try
    for i := 0 to request.categories.Count - 1 do
      if request.categories[i].scheme = TAG_FHIR_SCHEME_PROFILE then
        profileId := request.categories[i].term;

    if StringStartsWith(ProfileId, 'http://localhost/profile/') then
      profile := GetProfileById(copy(ProfileId, 27, $FF))
    else if StringStartsWith(ProfileId, request.baseUrl) then
      profile := GetProfileById(copy(ProfileId, length(request.baseUrl), $FF));

    if opDesc = '' then
      if Profile = nil then
        opDesc := 'Validate resource '+request.id
      else
        opDesc := 'Validate resource '+request.id+' against profile '+profileId;

    if request.resourceType = frtBinary then
    begin
      outcome := TFhirOperationOutcome.create;
    end
    else if (request.Source <> nil) and (request.PostFormat = ffXml) then
      outcome := FRepository.validator.validateInstance(context, request.Source, opDesc, profile)
    else
    begin
      buffer := TAdvBuffer.create;
      try
        mem := TAdvMemoryStream.create;
        vcl := TVclStream.create;
        try
          mem.Buffer := buffer.Link;
          vcl.stream := mem.Link;
          xml := TFHIRXmlComposer.create(request.Lang);
          try
            if request.Resource <> nil then
              xml.Compose(vcl, '', '', request.resource, true)
            else
              xml.Compose(vcl, request.feed, true);
          finally
            xml.free;
          end;
        finally
          vcl.free;
          mem.free;
        end;
        outcome := FRepository.validator.validateInstance(context, buffer, opDesc, profile);
      finally
        buffer.free;
      end;
    end;

    // todo: check version id integrity
    // todo: check version integrity

    response.Resource := outcome;
    result := true;
    for i := 0 to outcome.issueList.count - 1 do
      result := result and (outcome.issueList[i].severityST in [IssueSeverityInformation, IssueSeverityWarning]);
    if result then
      response.HTTPCode := 200
    else
      response.HTTPCode := 400;
    if request.ResourceType <> frtSecurityEvent then
      AuditRest(request.session, request.ip, request.ResourceType, request.id, '', request.CommandType, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      if request.ResourceType <> frtSecurityEvent then
        AuditRest(request.session, request.ip, request.ResourceType, request.id, '', request.CommandType, 500, '', e.message);
      raise;
    end;
  end;
end;

procedure TFhirOperation.ExecuteVersionRead(request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : integer;
  originalId : String;
begin
  try
    NotFound(request, response);
    if check(response, opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
    begin
      if (length(request.id) <= 36) and (length(request.subid) <= 36) and FindResource(request.ResourceType, request.Id, false, resourceKey, originalId, response, request.compartments) then
      begin
        VersionNotFound(request, response);
        FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(resourceKey)+' and VersionId = :v';
        FConnection.Prepare;
        try
          FConnection.BindString('v', request.SubId);
          FConnection.Execute;
          if FConnection.FetchNext then
          Begin
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
            response.versionId := FConnection.GetColStringByName('VersionId');
            response.originalId := originalId;
            response.LastModifiedDate := TSToDateTime(FConnection.ColTimeStampByName['StatedDate']);
            processBlob(response, false);
          end;
        finally
          FConnection.Terminate;
        end;
      end;
    end;
    AuditRest(request.session, request.ip, request.ResourceType, request.id, request.SubId, request.CommandType, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, request.SubId, request.CommandType, 500, '', e.message);
      raise;
    end;
  end;
end;

function TFhirOperation.FindResource(aType : TFHIRResourceType; sId: String; bAllowDeleted : boolean; var resourceKey: integer; var originalId : String; response : TFhirResponse; compartments : String): boolean;
var
  cmp : String;
begin
  if (compartments <> '') then
    cmp := ' and Ids.ResourceKey in (select ResourceKey from Compartments where CompartmentType = 1 and Id in ('+compartments+'))'
  else
    cmp := '';

//  if bAllowDeleted then
  FConnection.sql := 'select ResourceKey, OriginalId, MostRecent from Ids, Types where id = :s '+cmp+' and Ids.ResourceTypeKey = Types.ResourceTypeKey and Supported = 1 and ResourceName = :n';
//  else
//    FConnection.sql := 'select ResourceKey, OriginalId from Ids, Types where id = :s and Ids.ResourceTypeKey = Types.ResourceTypeKey and Supported = 1 and not MostRecent is null and ResourceName = :n';
  FConnection.Prepare;
  FConnection.BindString('s', sId);
  FConnection.BindString('n', CODES_TFHIRResourceType[aType]);
  FConnection.execute;
  result := FConnection.FetchNext;
  if result then
  begin
    if bAllowDeleted or not FConnection.ColNullByName['MostRecent'] then
    begin
      resourceKey := FConnection.ColIntegerByName['ResourceKey'];
      originalId := FConnection.ColStringByName['OriginalId'];
    end
    else
    begin
      if response <> nil then
        check(response, false, 410, lang, StringFormat(GetFhirMessage('MSG_DELETED_ID', lang), [request.id]));
      result := false;
    end;
  end
  else if response <> nil then
    check(response, false, 404 , lang, StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [CODES_TFHIRResourceType[request.ResourceType]+'/'+request.id]));
  FConnection.terminate;
end;

function TFhirOperation.AddNewResourceId(aType : TFHIRResourceType; id : string; var resourceKey : integer) : Boolean;
var
  itype : integer;
begin
  iType := FConnection.CountSQL('select ResourceTypeKey from Types where supported = 1 and ResourceName = '''+CODES_TFHIRResourceType[aType]+'''');
  result := iType > 0;
  if result then
  begin
    resourceKey := FRepository.NextResourceKey;
    FConnection.SQL := 'insert into Ids (ResourceKey, ResourceTypeKey, Id, MostRecent) values (:k, :r, :i, null)';
    FConnection.Prepare;
    FConnection.BindInteger('k', resourceKey);
    FConnection.BindInteger('r', itype);
    FConnection.BindString('i', id);
    FConnection.Execute;
    FConnection.Terminate;
    if IsNumericString(id) then
      FConnection.ExecSQL('update Types set LastId = '+id+' where ResourceTypeKey = '+inttostr(iType)+' and LastId < '+id);
    FConnection.ExecSQL('update IndexEntries set target = '+inttostr(resourceKey)+' where SpaceKey = '+inttostr(iType)+' and value = '''+id+'''');
  end;
End;

function TFhirOperation.getNewResourceId(aType: TFHIRResourceType; var id: string; var key: integer): Boolean;
var
  itype : integer;
  guid : boolean;
begin
  guid := false;
  iType := 0;
  FConnection.SQL := 'select ResourceTypeKey, LastId, IdGuids from Types where supported = 1 and ResourceName = '''+CODES_TFHIRResourceType[aType]+'''';
  FConnection.Prepare;
  FConnection.Execute;
  if FConnection.FetchNext then
  begin
    iType := FConnection.ColIntegerByName['ResourceTypeKey'];
    id := inttostr(FConnection.ColIntegerByName['LastId']+1);
    guid := FConnection.ColIntegerByName['IdGuids'] = 1;
  end;
  FConnection.Terminate;
  result := iType > 0;
  if result then
  begin
    if guid then
      id := FhirGUIDToString(CreateGUID)
    else
      FConnection.ExecSQL('update Types set LastId = '+id+' where ResourceTypeKey = '+inttostr(iType));
    key := FRepository.NextResourceKey;
    FConnection.SQL := 'insert into Ids (ResourceKey, ResourceTypeKey, Id, MostRecent) values (:k, :r, :i, null)';
    FConnection.Prepare;
    FConnection.BindInteger('k', key);
    FConnection.BindInteger('r', itype);
    FConnection.BindString('i', id);
    FConnection.Execute;
    FConnection.Terminate;
    FConnection.ExecSQL('update IndexEntries set target = '+inttostr(key)+' where SpaceKey = '+inttostr(iType)+' and value = '''+id+'''');
  end;
end;

procedure TFhirOperation.NotFound(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [CODES_TFHIRResourceType[request.ResourceType]+':'+request.Id]);
  response.Body := response.Message;
  response.Resource := BuildOperationOutcome(lang, response.Message);
end;

procedure TFhirOperation.VersionNotFound(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [CODES_TFHIRResourceType[request.ResourceType]+':'+request.Id+'/_history/'+request.subId]);
  response.Body := response.Message;
  response.Resource := BuildOperationOutcome(lang, response.Message);
end;

function TFhirOperation.GetPatientId(): String;
begin

end;

procedure TFhirOperation.TypeNotFound(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_UNKNOWN_TYPE', lang), [CODES_TFHIRResourceType[request.ResourceType]]);
  response.Body := response.Message;
  response.Resource := BuildOperationOutcome(lang, response.Message);
end;


function describeResourceTypes(aTypes : TFHIRResourceTypeSet) : String;
var
  a : TFHIRResourceType;
begin
  result := '';
  for a := High(TFHIRResourceType) downto low(TFHIRResourceType) do
    if a in aTypes then
      if result = '' then
        result := CODES_TFHIRResourceType[a]
      else
        result := result+' / '+CODES_TFHIRResourceType[a];
end;

procedure TFhirOperation.BuildSearchForm(request: TFHIRRequest; response : TFHIRResponse);
var
  i, j : integer;
  s, pfx, desc : String;
  ix, ix2 : TFhirIndex;
  types : TFHIRResourceTypeSet;
  m : TStringList;
begin
  response.HTTPCode := 200;
  response.ContentType := 'text/html';
  s :=
'<?xml version="1.0" encoding="UTF-8"?>'#13#10+
'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10+
'       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10+
''#13#10+
'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10+
'<head>'#13#10+
'    <title>FHIR RESTful Server - FHIR v'+FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION+'</title>'#13#10+
TFHIRXhtmlComposer.PageLinks+
FHIR_JS+
'</head>'#13#10+
''#13#10+
'<body>'#13#10+
''#13#10+
TFHIRXhtmlComposer.Header(request.Session, request.baseUrl, request.lang)+
'<h2>'+GetFhirMessage('SEARCH_TITLE', lang)+'</h2>'#13#10+
'</p>'#13#10;
if Request.DefaultSearch then
s := s +
'<form action="'+CODES_TFhirResourceType[request.ResourceType]+'/_search" method="GET">'#13#10+
'<table class="lines">'#13#10
else
s := s +
'<form action="_search" method="GET">'#13#10+
'<table class="lines">'#13#10;

  if request.ResourceType = frtNull then
  begin
    m := TStringList.Create;
    try
      for i := 0 to FIndexer.Indexes.Count - 1 Do
        if m.IndexOf(FIndexer.Indexes[i].Name) = -1 then
          m.addObject(FIndexer.Indexes[i].Name, FIndexer.Indexes[i]); // first one wins!
      m.Sort;
      for i := 0 to m.count - 1 do
      begin
        ix := TFhirIndex(m.objects[i]);
        if (ix.TargetTypes = []) then
          if ix.SearchType = SearchParamTypeDate then
          begin
            s := s + '<tr><td align="left">'+ix.Name+' </td><td><input type="text" name="'+ix.Name+'"></td><td>(Date)</td>'+
            '<td><select title="Missing?" size="1" name="'+ix.Name+'-missing"><option/><option value="1">absent</option><option name="0">present</option></select></td></tr>'#13#10;
            s := s + '<tr><td align="right"> (before)</td><td><input type="text" name="'+ix.Name+':before"></td><td> (before given date)</td><td></td></tr>'#13#10;
            s := s + '<tr><td align="right"> (after)</td><td><input type="text" name="'+ix.Name+':after"></td><td> (after given date)</td><td></td></tr>'#13#10
          end
          else
            s := s + '<tr><td align="left">'+ix.Name+'</td><td><input type="text" name="'+ix.Name+'"></td><td></td>'+
            '<td><select title="Missing?" size="1" name="'+ix.Name+':missing"><option/><option value="1">absent</option><option name="0">present</option></select></td></tr>'#13#10


      end;
    finally
      m.free;
    end;
  end
  else
  begin
    s := s +'<tr><td colspan="4"><b>'+CODES_TFHIRResourceType[request.ResourceType]+':</b></td></tr>'+#13#10;
    for i := 0 to FIndexer.Indexes.Count - 1 Do
    begin
      ix := FIndexer.Indexes[i];
      if (ix.ResourceType = request.ResourceType) and (ix.TargetTypes = []) then
      begin
        desc := FormatTextToHTML(GetFhirMessage('ndx-'+CODES_TFHIRResourceType[request.ResourceType]+'-'+ix.name, lang, ix.Description));
        if ix.SearchType = SearchParamTypeDate then
        begin
          s := s + '<tr><td align="left">'+ix.Name+' </td><td><input type="text" name="'+ix.Name+'"></td><td> '+desc+' on given date (yyyy-mm-dd)</td>'+
          '<td><select title="Missing?" size="1" name="'+ix.Name+':missing"><option/><option value="1">absent</option><option name="0">present</option></select></td></tr>'#13#10;
          s := s + '<tr><td align="right"> (before)</td><td><input type="text" name="'+ix.Name+':before"></td><td> (before given date)</td><td></td></tr>'#13#10;
          s := s + '<tr><td align="right"> (after)</td><td><input type="text" name="'+ix.Name+':after"></td><td> (after given date)</td><td></td></tr>'#13#10
        end
        else
          s := s + '<tr><td align="left">'+ix.Name+'</td><td><input type="text" name="'+ix.Name+'"></td><td> '+desc+'</td>'+
          '<td><select title="Missing?" size="1" name="'+ix.Name+':missing"><option/><option value="1">absent</option><option name="0">present</option></select></td></tr>'#13#10
      end;
    end;

    for i := 0 to FIndexer.Indexes.Count - 1 Do
    begin
      ix := FIndexer.Indexes[i];
      if (ix.ResourceType = request.ResourceType) and (ix.TargetTypes <> []) then
      begin
        pfx := ix.Name;
        types := ix.TargetTypes;
        s := s +'<tr><td colspan="4"><b>'+ix.Name+'</b> ('+describeResourceTypes(types)+')<b>:</b></td></tr>'+#13#10;
        m := TStringList.create;
        try
          for j := 0 to FIndexer.Indexes.Count - 1 Do
          begin
            ix2 := FIndexer.Indexes[j];
            if (ix2.ResourceType in types) and (m.IndexOf(ix2.Name) = -1) then
            begin
              desc := FormatTextToHTML(GetFhirMessage('ndx-'+CODES_TFHIRResourceType[request.ResourceType]+'-'+ix2.name, lang, ix2.Description));
              if (ix2.searchType = SearchParamTypeDate) then
              begin
                s := s + '<tr>&nbsp;&nbsp;<td align="left">'+ix2.Name+' (exact)</td><td><input type="text" name="'+pfx+'.'+ix2.Name+'"></td><td> '+desc+'</td>'+
          '<td><select title="Missing?" size="1" name="'+pfx+'.'+ix2.Name+':missing"><option/><option value="1">absent</option><option name="0">present</option></select></td></tr>'#13#10;
                s := s + '<tr>&nbsp;&nbsp;<td align="right">  (before)</td><td><input type="text" name="'+pfx+'.'+ix2.Name+':before"></td><td> (before given date) </td></tr>'#13#10;
                s := s + '<tr>&nbsp;&nbsp;<td align="right">  (after)</td><td><input type="text" name="'+pfx+'.'+ix2.Name+':after"></td><td> (after given date) </td></tr>'#13#10
              end
              else
                s := s + '<tr>&nbsp;&nbsp;<td align="left">'+ix2.Name+'</td><td><input type="text" name="'+pfx+'.'+ix2.Name+'"></td><td> '+desc+'</td>'+
          '<td><select title="Missing?" size="1" name="'+pfx+'.'+ix2.Name+':missing"><option/><option value="1">absent</option><option name="0">present</option></select></td></tr>'#13#10;
              m.add(ix2.Name);
            end;
          end;
        finally
          m.Free;
        end;
      end;
    end;
  end;

  s := s +
'<tr><td colspan="2"><hr/></td></tr>'#13#10+
'<tr><td align="right">'+GetFhirMessage('SEARCH_REC_OFFSET', lang)+'</td><td><input type="text" name="'+SEARCH_PARAM_NAME_OFFSET+'"></td><td> '+GetFhirMessage('SEARCH_REC_OFFSET_COMMENT', lang)+'</td></tr>'#13#10+
'<tr><td align="right">'+GetFhirMessage('SEARCH_REC_COUNT', lang)+'</td><td><input type="text" name="'+SEARCH_PARAM_NAME_COUNT+'"></td><td> '+StringFormat(GetFhirMessage('SEARCH_REC_COUNT_COMMENT', lang), [SEARCH_PAGE_LIMIT])+'</td></tr>'#13#10+
'<tr><td align="right">'+GetFhirMessage('SEARCH_SORT_BY', lang)+'</td><td><select size="1" name="'+SEARCH_PARAM_NAME_SORT+'">'+#13#10+
'<tr><td align="right">'+GetFhirMessage('SEARCH_SUMMARY', lang)+'</td><td><input type="checkbox" name="'+SEARCH_PARAM_NAME_SUMMARY+'" value="true"></td><td> '+GetFhirMessage('SEARCH_SUMMARY_COMMENT', lang)+'</td></tr>'#13#10;
  for i := 0 to FIndexer.Indexes.Count - 1 Do
  begin
    ix := FIndexer.Indexes[i];
    if (ix.ResourceType = request.ResourceType) then
      s := s + '<option value="'+ix.Name+'">'+ix.Name+'</option>';
  end;
  s := s + '</select></td><td></td></tr>'#13#10+
'</table>'#13#10+
'<p><input type="submit"/></p>'#13#10+
'</form>'#13#10+
''#13#10+
'<p>'+
TFHIRXhtmlComposer.Footer(request.baseUrl);
  response.Body := s;
end;


procedure TFhirOperation.SetConnection(const Value: TKDBConnection);
begin
  if (Fconnection <> nil) and (Value = nil) then
    StoreAudits;
  FConnection := Value;

  if (Value <> nil) then
  begin
    FSpaces := TFHIRIndexSpaces.Create(Value);
    FIndexer := TFHIRIndexManager.Create(FSpaces);
    FIndexer.Ucum := FRepository.TerminologyServer.Ucum.Link;
    FIndexer.Bases := FRepository.Bases;
    FIndexer.KeyEvent := FRepository.GetNextKey;
  end;
end;

procedure TFhirOperation.ExecuteUpload(request: TFHIRRequest; response: TFHIRResponse);
var
  s : String;
  ok : boolean;
begin
  try
    ok := true;
    if not check(response, opAllowed(request.ResourceType, request.CommandType), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
      ok := false;

    if ok and not check(response, (request.Resource <> nil) or ((request.feed <> nil) and (request.feed.entries.count > 0)), 400, lang, GetFhirMessage('MSG_RESOURCE_REQUIRED', lang)) then
      ok := false;
    if ok and (request.Resource <> nil) then
      if not check(response, request.ResourceType = request.resource.ResourceType, 400, lang, GetFhirMessage('MSG_RESOURCE_TYPE_MISMATCH', lang)) then
        ok := false;
    // todo: check version id integrity
    // todo: check version integrity

    if not ok then
      // do nothing
    else if request.Feed <> nil then
    begin
      ExecuteTransaction(true, request, response);
      exit;
    end
    else
    begin
      ExecuteCreate(nil, true, request, response, idMaybeNew, 0);
      s := '1 new resource created @'+request.id;
    end;

    if ok then
    begin
      response.HTTPCode := 202;
      response.Message := 'Accepted';
      response.Body :=
    '<?xml version="1.0" encoding="UTF-8"?>'#13#10+
    '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10+
    '       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10+
    ''#13#10+
    '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10+
    '<head>'#13#10+
    '    <title>'+StringFormat(GetFhirMessage('UPLOAD_DONE', lang), [CODES_TFHIRResourceType[request.resource.ResourceType]])+'</title>'#13#10+
    '    <link rel="Stylesheet" href="/css/fhir.css" type="text/css" media="screen" />'#13#10+
    FHIR_JS+
    '</head>'#13#10+
    ''#13#10+
    '<body>'#13#10+
    ''#13#10+
    '<div class="header">'#13#10+
    '  <a href="http://www.hl7.org/fhir" title="'+GetFhirMessage('MSG_HOME_PAGE_TITLE', lang)+'"><img src="/img/flame16.png" style="vertical-align: text-bottom"/> <b>FHIR</b></a>'#13#10+
    ''#13#10+
    '  &copy; HL7.org 2011-2013'#13#10+
    '  &nbsp;'#13#10+
    '  HL7Connect FHIR '+GetFhirMessage('NAME_IMPLEMENTATION', lang)+#13#10+
    '  &nbsp;'#13#10+
    '  '+GetFhirMessage('NAME_VERSION', lang)+' '+FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION+#13#10;

    if request.session <> nil then
      response.Body := response.Body +'&nbsp;&nbsp;'+FormatTextToXml(request.Session.Name);

    response.Body := response.Body +
    '  &nbsp;<a href="'+request.baseUrl+'">'+GetFhirMessage('MSG_BACK_HOME', lang)+'</a>'#13#10+
    '</div>'#13#10+
    ''#13#10+
    '<div id="div-cnt" class="content">'#13#10+
    '<h2>'+StringFormat(GetFhirMessage('UPLOAD_DONE', lang), [CODES_TFHIRResourceType[request.resource.ResourceType]])+'</h2>'#13#10+
    '<p>'+s+'</p>'#13#10+
    ''#13#10+
    '<p><br/><a href="'+request.baseUrl+'">'+GetFhirMessage('MSG_BACK_HOME', lang)+'</a></p>'+
    '</div>'#13#10+
    '</body>'#13#10+
    '</html>'#13#10+
    ''#13#10;
      response.ContentType:= 'text/html';
    end;
    AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, 500, '', e.message);
      raise;
    end;
  end;
end;


Function IsTypeAndId(s : String; var id : String):Boolean;
var
  l, r : String;
begin
  StringSplit(s, '/', l, r);
  id := r;
  if (l <> '') and (l[1] = '/') then
    delete(l, 1, 1);
  result := (StringArrayIndexOfSensitive(CODES_TFHIRResourceType, l) > -1) and IsId(r);
end;

Function TFhirOperation.ResolveSearchId(atype : TFHIRResourceType; params : String) : String;
var
  p : TParseMap;
  key, i , j: integer;
  s : String;
  first, h : Boolean;
  abbrev : TAbbreviationServer;
  ts : TStringList;
  code : String;
  wantSummary : boolean;
begin
  key := FConnection.CountSQL('select ResourceTypeKey from Types where supported = 1 and ResourceName = '''+CODES_TFHIRResourceType[aType]+'''');
  assert(key > 0);
  p := TParseMap.createSmart(params);
  abbrev := TAbbreviationServer.create;
  ts := TStringList.create;
  try
    s := '';
    first := true;
    code := abbrev.next;
    for i := 0 to p.getItemCount - 1 do
    begin
      ts.Clear;
      ts.assign(p.List(i));
      for j := ts.count - 1 downto 0 do
        if ts[j] = '' then
          ts.delete(j);
      for j := ts.count - 1 downto 0 do
        s := s + processParam([aType], code, abbrev, p.VarName(i), lowercase(ts[j]), false, first, h, wantSummary);
    end;
    if s = '' then
      raise Exception.create(StringFormat(GetFhirMessage('SEARCH_NONE', lang), [CODES_TFHIRResourceType[aType], params]));
    FConnection.SQL := 'Select Id from Ids where ResourceTypeKey = '+inttostr(key)+' and MasterResourceKey is null and Ids.ResourceKey in (select ResourceKey from IndexEntries where '+s+')';
    FConnection.prepare;
    try
      FConnection.Execute;
      if FConnection.FetchNext then
      begin
        result := FConnection.ColStringByName['Id'];
        if FConnection.FetchNext then
          raise Exception.create(StringFormat(GetFhirMessage('SEARCH_MULTIPLE', lang), [CODES_TFHIRResourceType[aType], params]));
      end
      else
        result := '';
    finally
      FConnection.terminate;
    end;
  finally
    p.free;
    abbrev.Free;
  end;
end;

procedure TFhirOperation.scanId(entry : TFHIRAtomEntry; ids : TFHIRTransactionEntryList);
var
  match : boolean;
  id : TFHIRTransactionEntry;
  i, k : integer;
  sId : String;
begin
  if not FRepository.ResConfig[entry.resource.ResourceType].cmdUpdate and not FRepository.ResConfig[entry.resource.ResourceType].cmdCreate then
    Raise Exception.create('Resource '+CODES_TFHIRResourceType[entry.resource.ResourceType]+' is not supported in Transactiones');

  id := TFHIRTransactionEntry.create;
  try
    id.Name := entry.id;
    if id.name = '' then
      raise Exception.create(GetFhirMessage('MSG_TRANSACTION_MISSING_ID', lang));
    id.ResType := CODES_TFHIRResourceType[entry.resource.ResourceType];
    if ids.ExistsByName(id.Name) then
      raise Exception.create(StringFormat(GetFhirMessage('MSG_Transaction_DUPLICATE_ID', lang), [id.Name]));
    ids.add(id.link);

    // processing the id
    if IsId(entry.id) then // this is actually illegal...
      raise Exception.create('plain id on a resource - illegal ("'+entry.id+'")');

    if entry.id.StartsWith(request.baseUrl, true) and IsId(copy(entry.id, length(request.baseUrl)+1, $FFFF)) then
    begin
      id.id := copy(entry.id, length(request.baseUrl)+1, $FFFF);
      id.originalId := '';
    end
    else if entry.id.StartsWith(AppendForwardSlash(request.baseUrl)+CODES_TFHIRResourceType[entry.resource.ResourceType]+'/_search?', true) or
      entry.id.StartsWith('http://localhost/'+CODES_TFHIRResourceType[entry.resource.ResourceType]+'/_search?', true)  then
    begin
      id.id := ResolveSearchId(entry.resource.ResourceType, copy(entry.id, pos('?', entry.id)+1, $FFF));
    end
    else if entry.id.StartsWith('cid:', true) then
    begin
      // don't set this - get a new id below!  id.id :=
    end
    else if entry.id.StartsWith('urn:uuid:', true) and IsGuid(copy(entry.id, 10, $FFFF)) then
    begin
      // todo: do we care if GUIDs aren't allowed?
      id.id := copy(entry.id, 10, $FFFF);
      id.originalId := '';
    end
    else
    begin
      match := false;
      for i := 0 to FRepository.Bases.count - 1 do
       if entry.id.StartsWith(FRepository.Bases[i], true) and IsTypeAndId(copy(entry.id, length(FRepository.Bases[i])+2, $FFFF), sId) then
       begin
         id.id := sId;
         id.originalId := '';
         match := true;
       end;
     if not match then
     begin
       id.originalId := entry.id;
       id.id := '';
     end;
    end;

    If not entry.deleted and (id.id <> '') then
    begin
      if ids.ExistsByTypeAndId(id) and not FTestServer then
        raise Exception.create(StringFormat(GetFhirMessage('MSG_Transaction_DUPLICATE_ID', lang), [id.Name]));

      if ids.ExistsByTypeAndId(id) then
      begin
        i := 0;
        sId := id.id;
        repeat
          inc(i);
          id.id := sId + inttostr(i);
        until not ids.ExistsByTypeAndId(id);
      end;

      AddNewResourceId(entry.resource.resourceType, id.id, k);
      id.key := k;
    end;
  finally
    id.free;
  end;
end;

procedure TFhirOperation.confirmId(entry : TFHIRAtomEntry; ids : TFHIRTransactionEntryList);
var
  id : TFHIRTransactionEntry;
  sId : String;
  k : integer;
begin
  id := ids.getByName(entry.id) as TFHIRTransactionEntry;

  if not entry.deleted // doesn't matter if id is unknown
    and (id.id = '') then
  begin
    id.new := true;
    if not GetNewResourceId(entry.resource.resourceType,  sId, k) then
      raise exception.create(GetFhirMessage('MSG_RESOURCE_ID_FAIL', lang));
    id.id := sId;
    id.key := k;
  end;
end;


procedure FixXhtmlUrls(lang, base : String; ids : TFHIRTransactionEntryList; node : TFhirXHtmlNode);
var
  i, j : integer;
  s : string;
begin
  if (node <> nil) and (node.NodeType = fhntElement) then
  begin
    if (node.Name = 'a') and (node.Attributes.Get('href') <> '') then
    begin
      s := node.Attributes.Get('href');
      j := ids.IndexByName(s);
      if (j > -1) then
        node.Attributes.SetValue('href', base+ids[j].resType+'/'+ids[j].id)
      else if s.StartsWith('cid:', true) then
        Raise Exception.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [s]));
    end;
    if (node.Name = 'img') and (node.Attributes.Get('src') <> '') then
    begin
      s := node.Attributes.Get('src');
      j := ids.IndexByName(s);
      if (j > -1) then
        node.Attributes.SetValue('src', base+ids[j].resType+'/'+ids[j].id)
      else if s.StartsWith('cid:', true) then
        Raise Exception.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [s]));
    end;
    for i := 0 to node.ChildNodes.count - 1 do
      FixXhtmlUrls(lang, base, ids, node.childNodes[i]);
  end;
end;


procedure TFhirOperation.adjustReferences(base : String; entry : TFHIRAtomEntry; ids : TFHIRTransactionEntryList);
var
  refs : TFhirResourceReferenceList;
  ref : TFhirResourceReference;
  i, j, k : integer;
  attachments : TFhirAttachmentList;
  attachment : TFhirAttachment;
  extension : TFhirExtension;
begin
  refs := TFhirResourceReferenceList.create;
  try
    listReferences(entry.resource, refs);
    for i := 0 to refs.count - 1 do
    begin
      ref := refs[i];
      j := ids.IndexByName(ref.referenceST);
      k := 0;
      while (j = -1) and (k < FRepository.Bases.Count - 1) do
      begin
        j := ids.IndexByName(FRepository.Bases[k]+ref.referenceST);
        inc(k);
      end;
      if (j <> -1) then
      begin
        ref.referenceST :=  ids[j].resType+'/'+ids[j].id; // todo: make local?
      end;
    end;
  finally
    refs.free;
  end;
  attachments := TFhirAttachmentList.create;
  try
    ListAttachments(entry.resource, attachments);
    for i := 0 to attachments.count - 1 do
    begin
      attachment := attachments[i];
      j := ids.IndexByName(attachment.urlST);
      if (j > -1) then
        attachment.urlST := lowercase(ids[j].resType)+'/'+ids[j].id
      else if attachment.urlST.StartsWith('cid:', true) then
        Raise Exception.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [attachment.urlST]));
    end;
  finally
    attachments.free;
  end;
  for i := 0 to entry.resource.extensionList.count - 1 do
  begin
    extension := entry.resource.extensionList[i];
    j := ids.IndexByName(extension.urlST);
    if (j > -1) then
      extension.url := TFhirUri.create(base+ids[j].resType+'/'+ids[j].id)
    else if extension.urlST.StartsWith('cid:', true) then
      Raise Exception.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [extension.urlST]));
  end;
  // special case: XdsEntry
  if (entry.resource.resourceType = frtDocumentReference) and (TFhirDocumentReference(entry.resource).location <> nil) then
  begin
    j := ids.IndexByName(TFhirDocumentReference(entry.resource).locationST);
    if (j > -1) then
      TFhirDocumentReference(entry.resource).locationST := base+ids[j].resType+'/'+ids[j].id
    else if TFhirDocumentReference(entry.resource).locationST.StartsWith('cid:', true) then
      Raise Exception.create(StringFormat(GetFhirMessage('MSG_LOCAL_FAIL', lang), [TFhirDocumentReference(entry.resource).locationST]));
  end;

  if entry.resource.text <> nil then
    FixXhtmlUrls(lang, base, ids, entry.resource.text.div_);
end;

function TFhirOperation.commitResource(context: TFHIRValidatorContext; upload : boolean; entry: TFHIRAtomEntry; id: TFHIRTransactionEntry; session : TFHIRSession; resp : TFHIRAtomFeed) : Boolean;
var
  ne : TFHIRAtomEntry;
begin
  request.Id := id.id;
  request.originalId := id.originalId;
  request.SubId := '';
  request.ResourceType := entry.resource.ResourceType;
  request.resource := entry.resource.link;
  //todo: check session?
  //      raise exception.create('special post conditions on provenance and patient');
  if id.deleted then
  begin
    if id.id <> '' then
      ExecuteDelete(request, Response);
  end
  else if id.new then
    ExecuteCreate(context, upload, request, response, idIsNew, id.key)
  else
    ExecuteUpdate(context, upload, request, response);
  result := check(response, response.HTTPCode < 300, response.HTTPCode, lang, response.Message);
  if result then
  begin
    ne := TFHIRAtomEntry.create;
    try
      // server assigned id
      ne.id := request.baseUrl+CODES_TFHIRResourceType[response.resource.resourceType]+'/'+request.id;
      // specific version reference
      ne.links['self'] := request.baseUrl+CODES_TFHIRResourceType[response.resource.resourceType]+'/'+request.id+'/_history/'+request.SubId;
      // client assigned id
      ne.links['related'] := entry.id;
      // original id supported by this server, not part of the spec:
      ne.originalId := request.originalId;
      // other standards metadata
      ne.updated := NowUTC;
      ne.published_ := ne.updated.Link;
      ne.title := 'Resource "'+request.id+'"';
      ne.authorName := request.Session.Name;
      // no content or summary
      resp.entries.add(ne.Link);
    finally
      ne.free;
    end;
  end;
end;


procedure TFhirOperation.ExecuteTransaction(upload : boolean; request: TFHIRRequest; response: TFHIRResponse);
var
//  match : boolean;
  i : integer;
  resp : TFHIRAtomFeed;
  ids : TFHIRTransactionEntryList;
  ok : boolean;
  context: TFHIRValidatorContext;
begin
  try
    ok := true;
    if not check(response, FRepository.SupportTransaction, 405, lang, 'Transaction Operations not allowed') then
      ok := false;
    if ok and not check(response, request.feed <> nil, 400, lang, 'A bundle is required for a Transaction operation') then
      ok := false;

    if ok then
    begin
      request.Source := nil; // ignore that now
      resp := TFHIRAtomFeed.create;
      ids := TFHIRTransactionEntryList.create;
      context := FRepository.Validator.AcquireContext;
      try
        resp.fhirBaseUrl := request.baseUrl;
        ids.SortedByName;
        resp.title := 'Transaction results';
        resp.updated := NowUTC;
        resp.id := 'urn:uuid:'+FhirGUIDToString(CreateGuid);

        // first pass: scan ids
        for i := 0 to request.feed.entries.count - 1 do
          scanId(request.feed.entries[i], ids);

        // second pass: confirm ids
        for i := 0 to request.feed.entries.count - 1 do
          confirmId(request.feed.entries[i], ids);

        // third pass: reassign references
        for i := 0 to request.feed.entries.count - 1 do
          adjustReferences(request.baseUrl, request.feed.entries[i], ids);

        // four pass: commit resources
        for i := 0 to request.feed.entries.count - 1 do
        begin
          writeln(inttostr(i)+': '+ids.GetByName(request.feed.entries[i].id).summary);
          if not commitResource(context, FTestServer and (request.feed.entries.count > 50), request.feed.entries[i], ids.GetByName(request.feed.entries[i].id), request.Session, resp) then
          begin
            Abort;
          end;
        end;

        response.HTTPCode := 202;
        response.Message := 'Accepted';
        response.feed := resp.Link;
      finally
        FRepository.Validator.YieldContext(context);
        ids.free;
        resp.free;
      end;
    end;
    AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, 500, '', e.message);
      raise;
    end;
  end;
end;

procedure TFhirOperation.ExecuteMailBox(request: TFHIRRequest; response: TFHIRResponse);
var
  ok : boolean;
  msg, resp : TFhirMessageHeader;
begin
  try
    ok := true;
    if ok and not check(response, (request.Resource <> nil) or ((request.feed <> nil) and (request.feed.entries.count > 0)), 400, lang, GetFhirMessage('MSG_RESOURCE_REQUIRED', lang)) then
      ok := false;

    if ok then
    begin
      if request.Feed <> nil then
      begin
        if request.Feed.hasTag(FHIR_TAG_SCHEME, 'http://hl7.org/fhir/tag/message') then
        begin
          if (request.feed.entries.Count = 0) or not (request.feed.entries[0].resource is TFhirMessageHeader) then
            raise Exception.Create('Invalid Message - first resource must be message header');
          response.Feed := TFHIRAtomFeed.create;
          response.feed.fhirBaseUrl := request.baseUrl;
          response.HTTPCode := 200;
          msg := TFhirMessageHeader(request.feed.entries[0].resource);
          response.Feed.title := 'Response to Message Envelope '+ request.Feed.id;
          response.Feed.id := NewGuidURN;
          response.Feed.links.AddValue(NewGUIDUrn, 'self'); // todo - refer to cache
          response.Feed.updated := NowUTC;
          // todo: check message and envelope ids
          resp := BuildResponseMessage(request, msg);
          response.Feed.addEntry('Message Response', resp.timestampST, NewGuidURN, NewGuidURN, resp);
          ProcessMessage(request, msg, resp, response.Feed);
        end
        else if request.Feed.hasTag(FHIR_TAG_SCHEME, 'http://hl7.org/fhir/tag/document') then
          Raise Exception.create('Document Handling not implemented yet')
        else
          Raise Exception.create('Unknown content for mailbox');
      end
      else
        Raise Exception.create('Unknown content for mailbox');
      response.HTTPCode := 202;
      response.Message := 'Accepted';
      AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, response.httpCode, '', response.message);
    end;
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, '', '', request.CommandType, 500, '', e.message);
      raise;
    end;
  end;
end;

procedure TFhirOperation.CollectIncludes(includes: TReferenceList; resource: TFHIRResource; xpath: String);
var
  s : String;
  matches : TFHIRObjectList;
  i : integer;
begin
  while xpath <> '' do
  begin
    StringSplit(xpath, ';', s, xpath);
    matches := resource.PerformQuery(s);
    try
      for i := 0 to matches.count - 1 do
        if (matches[i] is TFhirResourceReference) and (TFhirResourceReference(matches[i]).reference <> nil) then
          includes.seeReference(TFhirResourceReference(matches[i]).referenceST);
    finally
      matches.free;
    end;
  end;
end;

function TFhirOperation.LoadBinaryResource(stream: TMemoryStream): TFhirBinary;
var
  s, l : TBytes;
begin
  result := TFhirBinary.create;
  try
    s := ZDecompressBytes(copy(StreamToBytes(stream), 2, $FFFFFFF));
    BytesSplit(s, 0, l, s);
    result.ContentType := BytesAsAnsiString(l);
    BytesSplit(s, 0, l, s);
    // l is categories....
    result.Content.AsBytes := s;
    result.text :=  TFhirNarrative.create;
    result.text.statusST := NarrativeStatusGenerated;
    result.text.div_ := ParseXhtml(lang, '<div>'+GetFhirMessage('NAME_BINARY', lang)+' ('+result.ContentType+', '+DescribeBytes(result.Content.Size)+')</div>');
    result.Link;
  finally
    result.free;
  end;
end;

function TFhirOperation.opAllowed(resource: TFHIRResourceType; command: TFHIRCommandType): Boolean;
begin
  case command of
    fcmdUnknown : result := false;
    fcmdMailbox : result := false;
    fcmdRead : result := FRepository.ResConfig[resource].Supported;
    fcmdVersionRead : result := FRepository.ResConfig[resource].Supported;
    fcmdUpdate : result := FRepository.ResConfig[resource].Supported and FRepository.ResConfig[resource].cmdUpdate;
    fcmdDelete : result := FRepository.ResConfig[resource].Supported and FRepository.ResConfig[resource].cmdDelete;
    fcmdHistoryInstance : result := FRepository.ResConfig[resource].Supported and FRepository.ResConfig[resource].cmdHistoryInstance;
    fcmdHistoryType : result := FRepository.ResConfig[resource].Supported and FRepository.ResConfig[resource].cmdHistoryType;
    fcmdHistorySystem : result := FRepository.SupportSystemHistory;
    fcmdValidate : result := FRepository.ResConfig[resource].Supported and FRepository.ResConfig[resource].cmdValidate;
    fcmdSearch : result := FRepository.ResConfig[resource].Supported and FRepository.ResConfig[resource].cmdSearch;
    fcmdCreate : result := FRepository.ResConfig[resource].Supported and FRepository.ResConfig[resource].cmdCreate;
    fcmdConformanceStmt : result := true;
    fcmdUpload, fcmdTransaction : result := FRepository.SupportTransaction;
  else
    result := false;
  end;
end;

function TFhirOperation.TypeForKey(key: integer): TFHIRResourceType;
var
  a : TFHIRResourceType;
  b : boolean;
begin
  result := frtNull;
  b := false;
  for a := low(TFHIRResourceType) to high(TFHIRResourceType) do
    if  FRepository.ResConfig[a].key = key then
    begin
      result := a;
      b := true;
    end;
  if not b then
    raise exception.create('key '+inttostr(key)+' not found');
end;

procedure TFhirOperation.CommitTags(tags: TFHIRAtomCategoryList; key: integer);
var
  i : integer;
begin
  if (tags.Count = 0) then
    exit;

  FConnection.sql := 'Insert into VersionTags (ResourceTagKey, ResourceVersionKey, TagKey, Label) values (:k, '+inttostr(key)+', :t, :l)';
  FConnection.prepare;
  try
    for i := 0 to tags.count - 1 do
    begin
      FConnection.BindInteger('k', FRepository.NextTagVersionKey);
      FConnection.BindInteger('t', tags[i].TagKey);
      FConnection.BindString('l', tags[i].label_);
      FConnection.Execute;
    end;
  finally
    FConnection.terminate;
  end;
end;

procedure TFhirOperation.ProcessBlob(response: TFHIRResponse; wantSummary : boolean);
var
  parser : TFHIRParser;
  stream : TStream;
begin
  if request.ResourceType = frtBinary then
    response.resource := LoadBinaryResource(FConnection.ColMemoryByName['Content'])
  else
  begin
    response.categories.DecodeJson(FConnection.ColMemoryByName['Tags']);
    if wantSummary then
      stream := FConnection.ColMemoryByName['Summary']
    else
      stream := FConnection.ColMemoryByName['Content'];
    stream.position := 0;
    parser := MakeParser(lang, ffXml, stream);
    try
      response.Resource := parser.resource.Link;
    finally
      parser.free;
    end;
  end;
end;

procedure TFhirOperation.ProcessConceptMapTranslation(resource : TFHIRResource; params: TParseMap; feed: TFHIRAtomFeed; includes: TReferenceList; base, lang: String);
var
  src : TFhirValueset;
  coding : TFhirCoding;
  op : TFhirOperationOutcome;
  used : string;
  dest : String;
  cacheId : String;
begin
  used := '_query=translate';

  src := IdentifyValueset(nil, params, base, used, cacheId, true);
  try
    coding := nil;
    try
      if params.VarExists('coding') then
        coding := TFHIRJsonParser.parseFragment(params.GetVar('coding'), 'Coding', lang) as TFhirCoding
      else
      begin
        coding := TFhirCoding.Create;
        coding.systemST := params.GetVar('system');
        coding.versionST := params.GetVar('version');
        coding.codeST := params.GetVar('code');
        coding.displayST := params.GetVar('display');
        coding.valueSet := FFactory.makeReference(params.GetVar('display'));
      end;

      dest := params.GetVar('dest');
      feed.title := 'Concept Translation';
      feed.links.Rel['self'] := used;
      feed.SearchTotal := 1;
      op := FRepository.TerminologyServer.translate(src, coding, dest);
      try
        AddResourceToFeed(feed, NewGuidURN, 'Translation Outcome', 'Translation Outcome', '??', 'Health Intersections', '??base', now, op, '', nil, false);
      finally
        op.free;
      end;
    finally
      coding.Free;
    end;
  finally
    src.free;
  end;
end;

procedure TFhirOperation.LoadTags(tags: TFHIRAtomCategoryList; ResourceKey: integer);
var
  t : TFhirTag;
  lbl : String;
begin
  FConnection.SQL := 'Select * from VersionTags where ResourceVersionKey = (select MostRecent from Ids where ResourceKey = '+inttostr(resourcekey)+')';
  FConnection.Prepare;
  FConnection.Execute;
  while FConnection.FetchNext do
  begin
    t := FRepository.GetTagByKey(FConnection.ColIntegerByName['TagKey']);
    if FConnection.ColStringByName['Label'] <> '' then
      lbl := FConnection.ColStringByName['Label']
    else
      lbl := t.Label_;

    if not tags.HasTag(t.Scheme, t.Term) then
      tags.AddValue(t.Scheme, t.Term, lbl).TagKey := t.Key
    else if tags.GetTag(t.Scheme, t.Term).Label_ = '' then
      tags.GetTag(t.Scheme, t.Term).Label_ := lbl;
  end;
  FConnection.Terminate;
end;

procedure TFhirOperation.ExecuteDeleteTags(request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : Integer;
  resourceVersionKey : Integer;
  currentResourceVersionKey : Integer;
  originalId : String;
  i, n : integer;
  tags : TFHIRAtomCategoryList;
  t : String;
  ok : boolean;
  stream, s : TMemoryStream;
  parser : TFHIRParser;
begin
  try
    ok := true;
    if not check(response, opAllowed(request.ResourceType, fcmdUpdate), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
      ok := false;
    if ok then
      NotFound(request, response);
    if ok and not FindResource(request.ResourceType, request.Id, false, resourceKey, originalId, response, request.compartments) then
      ok := false;
    if not ok then
      // nothing
    else
    begin
      currentResourceVersionKey := StrToInt(FConnection.Lookup('Ids', 'ResourceKey', inttostr(Resourcekey), 'MostRecent', '0'));
      if request.SubId <> '' then
      begin
        if not check(response, opAllowed(request.ResourceType, fcmdVersionRead {todo}), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
          exit;
        if Not FindResourceVersion(request.ResourceType, request.Id, request.SubId, false, resourceVersionKey, response) then
          exit;
      end
      else
        ResourceVersionKey := currentResourceVersionKey;
    end;

    if ok then
    begin
      tags := TFHIRAtomCategoryList.create;
      try
        LoadTags(tags, ResourceKey);
        for i := 0 to request.categories.Count - 1 do
          if (tags.HasTag(request.categories[i].scheme, request.categories[i].term, n)) then
            tags.DeleteByIndex(n);
        tags.Clear;
        t := request.categories.AsHeader;

        FConnection.SQL := 'update Versions set Tags = :cnt where ResourceVersionKey = '+inttostr(resourceVersionKey);
        FConnection.Prepare;
        FConnection.BindBlobFromBytes('cnt', tags.Json);
        FConnection.Execute;
        FConnection.Terminate;

        FConnection.ExecSQL('delete from VersionTags where ResourceVersionKey = '+inttostr(resourceVersionKey));
        CommitTags(tags, resourceVersionKey);

        if resourceVersionKey = currentResourceVersionKey then
        begin
          // changing the tags might change the indexing
          stream := TMemoryStream.Create;
          try
            FConnection.SQL := 'Select Content from Versions where ResourceVersionKey = '+inttostr(resourceVersionKey);
            FConnection.prepare;
            FConnection.Execute;
            if not FConnection.FetchNext then
              raise Exception.Create('Internal Error fetching current content');
            s := FConnection.ColMemoryByName['Content'];
            s.Position := 0;
            stream.CopyFrom(s, s.Size);
            stream.Position := 0;
            FConnection.Terminate;
            parser := MakeParser('en', ffXml, stream);
            try
              FIndexer.execute(resourceKey, request.Id, parser.resource, tags);
            finally
              parser.free;
            end;
          finally
            stream.free;
          end;
        end;

        response.categories.CopyTags(tags);
      finally
        tags.free;
      end;
      response.HTTPCode := 200;
      response.Message := 'OK';
      response.resource := nil; // clear the error message
    end;
    AuditRest(request.session, request.ip, request.ResourceType, request.id, request.subid, request.CommandType, response.httpCode, t, response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, request.subid, request.CommandType, 500, t, e.message);
      raise;
    end;
  end;
end;

procedure TFhirOperation.ExecuteGetTags(request: TFHIRRequest;  response: TFHIRResponse);
var
  tag : TFHIRAtomCategory;
  ok : boolean;
begin
  try
    ok := true;
    if request.ResourceType = frtNull then
    begin
    // well, this operation is always allowed?
      FConnection.sql := 'Select SchemeUri, TermUri, Label from Tags order by SchemeUri, TermUri'
    end
    else if request.Id = '' then
    begin
      if not check(response, FRepository.ResConfig[request.ResourceType].Supported, 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
        ok := false
      else
        FConnection.sql := 'Select SchemeUri, TermUri, Label from Tags where TagKey in (Select TagKey from Versions where Deleted = 0 and ResourceVersionKey in (select MostRecent from Ids where ResourceTypeKey = '+inttostr(FRepository.ResConfig[request.ResourceType].Key)+')) order by SchemeUri, TermUri'
    end
    else if request.SubId = '' then
    begin
      if not check(response, opAllowed(request.ResourceType, fcmdRead), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
         Ok := false
      else
        FConnection.sql := 'Select SchemeUri, TermUri, VersionTags.Label from Tags, VersionTags '+
        'where Tags.TagKey = VersionTags.TagKey and VersionTags.ResourceVersionKey in (Select ResourceVersionKey from Versions where Deleted = 0 and ResourceVersionKey in (select MostRecent from Ids where Id = :id)) order by SchemeUri, TermUri'
    end
    else
    begin
      if not check(response, opAllowed(request.ResourceType, fcmdVersionRead), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
        ok := false
      else
        FConnection.sql := 'Select SchemeUri, TermUri, VersionTags.Label from Tags, VersionTags '+
        'where Tags.TagKey = VersionTags.TagKey and VersionTags.ResourceVersionKey in (Select ResourceVersionKey from Versions where VersionId = :vid and Deleted = 0 and ResourceKey in (select ResourceKey from Ids where Id = :id)) order by SchemeUri, TermUri';
    end;

    if ok then
    begin
      FConnection.Prepare;
      if request.Id <> '' then
      begin
        FConnection.BindString('id', request.Id);
        if request.SubId <> '' then
          FConnection.BindString('vid', request.SubId);
      end;
      FConnection.execute;
      while FConnection.FetchNext do
      begin
        tag := TFHIRAtomCategory.create;
        try
          tag.scheme := FConnection.ColStringByName['SchemeUri'];
          tag.term := FConnection.ColStringByName['TermUri'];
          tag.label_ := FConnection.ColStringByName['Label'];
          response.Categories.add(tag.Link);
        finally
          tag.free;
        end;
      end;
      FConnection.terminate;
      response.HTTPCode := 200;
      response.Message := 'OK';
      response.Body := '';
    end;
    AuditRest(request.session, request.ip, request.ResourceType, request.id, request.subid, request.CommandType, response.httpCode, request.Parameters.Source, response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, request.subid, request.CommandType, 500, request.Parameters.Source, e.message);
      raise;
    end;
  end;
end;

procedure TFhirOperation.ExecuteUpdateTags(request: TFHIRRequest; response: TFHIRResponse);
var
  resourceKey : Integer;
  resourceVersionKey : Integer;
  currentResourceVersionKey : Integer;
  originalId : String;
  i : integer;
  tags : TFHIRAtomCategoryList;
  t : string;
  ok : boolean;
  stream, s : TMemoryStream;
  parser : TFHIRParser;
begin
  try
    ok := true;
    if not check(response, opAllowed(request.ResourceType, fcmdUpdate), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
      ok := false;
    if ok then
      NotFound(request, response);
    if ok and not FindResource(request.ResourceType, request.Id, false, resourceKey, originalId, response, request.compartments) then
      ok := false;
    if not ok then
      // nothing
    else
    begin
      CurrentResourceVersionKey := StrToInt(FConnection.Lookup('Ids', 'ResourceKey', inttostr(Resourcekey), 'MostRecent', '0'));
      if request.SubId <> '' then
      begin
        if not check(response, opAllowed(request.ResourceType, fcmdVersionRead {todo}), 400, lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', lang), [CODES_TFHIRCommandType[request.CommandType], CODES_TFHIRResourceType[request.ResourceType]])) then
          ok := false;
       if ok and Not FindResourceVersion(request.ResourceType, request.Id, request.SubId, false, resourceVersionKey, response) then
         ok := false;
      end
      else
        resourceVersionKey := currentResourceVersionKey;
    end;

    if ok then
    begin
      tags := TFHIRAtomCategoryList.create;
      try
        tags.CopyTags(request.categories);
        LoadTags(tags, ResourceKey);
        t := request.categories.AsHeader;
        for i := 0 to tags.count - 1 do
          FRepository.RegisterTag(tags[i], FConnection);

        FConnection.SQL := 'update Versions set Tags = :cnt where ResourceVersionKey = '+inttostr(resourceVersionKey);
        FConnection.Prepare;
        FConnection.BindBlobFromBytes('cnt', tags.Json);
        FConnection.Execute;
        FConnection.Terminate;

        FConnection.ExecSQL('delete from VersionTags where ResourceVersionKey = '+inttostr(resourceVersionKey));
        CommitTags(tags, resourceVersionKey);
        response.categories.CopyTags(tags);

        if resourceVersionKey = currentResourceVersionKey then
        begin
          stream := TMemoryStream.Create;
          try
            FConnection.SQL := 'Select Content from Versions where ResourceVersionKey = '+inttostr(resourceVersionKey);
            FConnection.prepare;
            FConnection.Execute;
            if not FConnection.FetchNext then
              raise Exception.Create('Internal Error fetching current content');
            s := FConnection.ColMemoryByName['Content'];
            s.Position := 0;
            stream.CopyFrom(s, s.Size);
            stream.Position := 0;
            FConnection.Terminate;
            parser := MakeParser('en', ffXml, stream);
            try
              FIndexer.execute(resourceKey, request.Id, parser.resource, tags);
            finally
              parser.free;
            end;
          finally
            stream.free;
          end;
        end;
      finally
        tags.free;
      end;
      response.HTTPCode := 200;
      response.Message := 'OK';
      response.resource := nil; // clear the error message
    end;
    AuditRest(request.session, request.ip, request.ResourceType, request.id, request.subid, request.CommandType, response.httpCode, t, response.message);
  except
    on e: exception do
    begin
      AuditRest(request.session, request.ip, request.ResourceType, request.id, request.subid, request.CommandType, 500, t, e.message);
      raise;
    end;
  end;
end;



function TFhirOperation.FindResourceVersion(aType: TFHIRResourceType; sId, sVersionId: String; bAllowDeleted: boolean; var resourceVersionKey: integer; response: TFHIRResponse): boolean;
begin
  FConnection.sql := 'select ResourceVersionKey from Ids, Types, Versions '+
    'where Ids.Id = :id and Ids.ResourceTypeKey = Types.ResourceTypeKey '+
    ' and Supported = 1 and Types.ResourceName = :n and Ids.ResourceKey = Versions.ResourceKey and Versions.Deleted = 0 and Versions.VersionId = :vid';
  FConnection.Prepare;
  FConnection.BindString('id', sId);
  FConnection.BindString('vid', sVersionId);
  FConnection.BindString('n', CODES_TFHIRResourceType[aType]);
  FConnection.execute;
  result := FConnection.FetchNext;
  if result then
    resourceVersionKey := FConnection.ColIntegerByName['ResourceVersionKey']
  else
    check(response, false, 404 , lang, StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [request.subid]));
  FConnection.terminate;
end;

function TFhirOperation.IdentifyValueset(resource : TFHIRResource; params: TParseMap; base : String; var used, cacheId : string; allowNull : boolean = false) : TFHIRValueSet;
begin
  cacheId := '';
  if (resource <> nil) and (resource is TFHIRValueSet) then
    result := resource.Link as TFhirValueSet
  else if params.VarExists('_id') then
  begin
    result := GetValueSetById(params.getvar('_id'), base);
    cacheId := result.identifierST;
    used := used+'&_id='+params.getvar('_id')
  end
  else if params.VarExists('identifier') then
  begin
    if not FRepository.TerminologyServer.isKnownValueSet(params.getvar('identifier'), result) then
      result := GetValueSetByIdentity(params.getvar('identifier'));
    cacheId := result.identifierST;
    used := used+'&identifier='+params.getvar('identifier')
  end
  else
    result := constructValueSet(params, used, allowNull);
end;

// need to set feed title, self link, search total. may add includes
procedure TFhirOperation.ProcessValueSetExpansion(resource : TFHIRResource; params: TParseMap; feed: TFHIRAtomFeed; includes: TReferenceList; base : String);
var
  src : TFhirValueset;
  dst : TFhirValueset;
  used : string;
  cacheId : String;
begin
  used := '_query=expand';

  src := IdentifyValueset(resource, params, base, used, cacheId);
  try
    feed.title := 'Expanded ValueSet';
    feed.links.Rel['self'] := used;
    feed.SearchTotal := 1;
    dst := FRepository.TerminologyServer.expandVS(src, cacheId, params.getVar('filter'));
    try
      AddResourceToFeed(feed, NewGuidURN, 'ValueSet', feed.title, '??', 'Health Intersections', '??base', now, dst, '', nil, false);
    finally
      dst.free;
    end;
  finally
    src.free;
  end;
end;

// need to set feed title, self link, search total. may add includes
procedure TFhirOperation.ProcessValueSetValidation(resource : TFHIRResource; params: TParseMap; feed: TFHIRAtomFeed; includes: TReferenceList; base, lang : String);
var
  src : TFhirValueset;
  coding : TFhirCoding;
  coded : TFhirCodeableConcept;
  op : TFhirOperationOutcome;
  used, cacheId : string;
begin
  used := '_query=validate';

  src := IdentifyValueset(resource, params, base, used, cacheId);
  try
    coding := nil;
    coded := nil;
    try
      if params.VarExists('coding') then
        coding := TFHIRJsonParser.parseFragment(params.GetVar('coding'), 'Coding', lang) as TFhirCoding
      else if params.VarExists('codeableconcept') then
        coded := TFHIRJsonParser.parseFragment(params.GetVar('codeableconcept'), 'CodeableConcept', lang) as TFhirCodeableConcept
      else
      begin
        coding := TFhirCoding.Create;
        coding.systemST := params.GetVar('system');
        coding.versionST := params.GetVar('version');
        coding.codeST := params.GetVar('code');
        coding.displayST := params.GetVar('display');
        coding.valueSet := FFactory.makeReference(params.GetVar('display'));
      end;


      feed.title := 'ValueSet Validation';
      feed.links.Rel['self'] := used;
      feed.SearchTotal := 1;
      if coding = nil then
        op := FRepository.TerminologyServer.validate(src, coded)
      else
        op := FRepository.TerminologyServer.validate(src, coding);
      try
        AddResourceToFeed(feed, NewGuidURN, 'Validation Outcome', 'Validation Outcome', '??', 'Health Intersections', '??base', now, op, '', nil, false);
      finally
        op.free;
      end;
    finally
      coding.Free;
      coded.Free;
    end;
  finally
    src.free;
  end;
end;

function TFhirOperation.GetValueSetById(id, base: String): TFHIRValueSet;
var
  resourceKey : integer;
  originalId : String;
  parser : TFHIRParser;
  b : String;
  s : TMemoryStream;
begin
  if id.StartsWith(base) then
    id := id.Substring(base.Length)
  else for b in FRepository.Bases do
    if id.StartsWith(b) then
      id := id.Substring(b.Length);
  if id.startsWith('ValueSet/') then
    id := id.Substring(9);

  if (length(request.id) <= 36) and FindResource(frtValueSet, id, false, resourceKey, originalId, nil, '') then
  begin
    FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc';
    FConnection.Prepare;
    try
      FConnection.Execute;
      if FConnection.FetchNext then
      begin
        s := FConnection.ColMemoryByName['Content'];
        s.position := 0;
        parser := MakeParser(lang, ffXml, s);
        try
          result := parser.resource.Link as TFHIRValueSet;
        finally
          parser.free;
        end;
      end
      else
        raise Exception.Create('Unable to find value set '+id);
    finally
      FConnection.Terminate;
    end;
  end
  else
    raise Exception.create('Unknown Value Set '+id);
end;

function TFhirOperation.GetValueSetByIdentity(id: String): TFHIRValueSet;
var
  stream : TMemoryStream;
  parser : TFHIRParser;
begin
  FConnection.SQL := 'Select * from Versions where ResourceKey in (select ResourceKey from IndexEntries where IndexKey in (Select IndexKey from Indexes where name = ''identifier'') and Value = :id) order by ResourceVersionKey desc';
  FConnection.Prepare;
  try
    FConnection.BindString('id', id);
    FConnection.Execute;
    if not FConnection.FetchNext then
      raise Exception.create('Unknown Value Set '+id);
    stream := FConnection.ColMemoryByName['Content'];
    stream.position := 0;
    parser := MakeParser(lang, ffXml, stream);
    try
      result := parser.resource.Link as TFHIRValueSet;
      try
        if FConnection.FetchNext then
          raise Exception.create('Found multiple matches for ValueSet '+id+'. Pick one by the resource id');
        result.link;
      finally
        result.free;
      end;
    finally
      parser.free;
    end;
  finally
    FConnection.Terminate;
  end;
end;

function ReadOperator(s : String) : TFhirFilterOperator;
begin
  s := LowerCase(s);
  if s = '' then
    result := FilterOperatorIsA
  else if (s = '=') or (s = 'equals') then
    result := FilterOperatorEqual
  else if (s = 'nota') then
    result := FilterOperatorIsNotA
  else if (s = '<=') or (s = 'isa') then
    result := FilterOperatorIsA
  else if (s = 'regex') then
    result := FilterOperatorRegex
  else
    raise Exception.create('Unhandled filter operator value "'+s+'"');
end;

function TFhirOperation.constructValueSet(params: TParseMap; var used: String; allowNull : Boolean): TFhirValueset;
var
  empty : boolean;
  function UseParam(name : String; var value : String) : boolean; overload;
  begin
    result := params.VarExists(name);
    value := params.GetVar(name);
    used := used + '&'+name+'='+EncodeMime(value);
    empty := value <> '';
  end;
  function UseParam(name : String): String; overload;
  begin
    result := params.GetVar(name);
    used := used + '&'+name+'='+EncodeMime(result);
    empty := result <> '';
  end;
var
  s, l : String;
  inc : TFhirValueSetComposeInclude;
  filter : TFhirValueSetComposeIncludeFilter;
begin
  empty := true;

  result := TFhirValueSet.create;
  try
    result.NameST := useParam('name');
    result.identifierST := useParam('vs-identifier');
    if result.identifierST = '' then
      result.identifierST := NewGuidURN;
    result.versionST := useParam('vs-version');
    result.compose := TFhirValueSetCompose.create;
    if useParam('import', s) then
      result.compose.importList.Append.value := s;
    if UseParam('system', s) then
    begin
      inc := result.compose.includeList.append;
      if (s = 'snomed') then
        inc.systemST := 'http://snomed.info/sct'
      else if (s = 'loinc') then
        inc.systemST := 'http://loinc.org'
      else
        inc.systemST := s;
      if UseParam('code', s) then
      begin
        while (s <> '') do
        begin
          StringSplit(s, ',', l, s);
          inc.codeList.Append.value := l;
        end;
      end;
      s := useParam('property');
      l := useParam('value');
      if (s <> '') or (l <> '') then
      begin
        filter := inc.filterList.Append;
        filter.property_ST := s;
        if filter.property_ST = '' then
          filter.property_ST := 'concept';
        filter.valueST := l;
        filter.opST := ReadOperator(UseParam('op'));
      end;
    end;
    if not empty then
      result.link
    else if not allowNull then
      raise Exception.Create('Not value set details found');
  finally
    result.free;
  end;
  if empty then
    result := nil;
end;

procedure TFhirOperation.AuditRest(session: TFhirSession; ip: string; resourceType: TFhirResourceType; id, ver: String; op: TFHIRCommandType; httpCode: Integer; name, message: String);
var
  se : TFhirSecurityEvent;
  c : TFhirCoding;
  p : TFhirSecurityEventParticipant;
  o : TFhirSecurityEventObject;
  procedure event(t, ts, td, s, sc : String; a : TFhirSecurityEventAction);
  begin
    se.event.type_ := TFhirCodeableConcept.create;
    c := se.event.type_.codingList.Append;
    c.codeST := t;
    c.systemST := ts;
    c.displayST := td;
    c := se.event.subtypeList.append.codingList.Append;
    c.codeST := s;
    c.systemST := sc;
    c.displayST := s;
    se.event.actionST := a;
  end;
begin
  if not FRepository.DoAudit then
    exit;
  se := TFhirSecurityEvent.create;
  try
    se.event := TFhirSecurityEventEvent.create;
    case op of
      fcmdMailbox :        event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'mailbox', 'http://hl7.org/fhir/restful-operation', SecurityEventActionE);
      fcmdRead:            event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'read',    'http://hl7.org/fhir/restful-operation', SecurityEventActionR);
      fcmdVersionRead:     event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'vread',   'http://hl7.org/fhir/restful-operation', SecurityEventActionR);
      fcmdUpdate:          event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'update',  'http://hl7.org/fhir/restful-operation', SecurityEventActionU);
      fcmdDelete:          event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'delete',  'http://hl7.org/fhir/restful-operation', SecurityEventActionD);
      fcmdHistoryInstance: event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'history-instance', 'http://hl7.org/fhir/restful-operation', SecurityEventActionR);
      fcmdCreate:          event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'create',  'http://hl7.org/fhir/restful-operation', SecurityEventActionC);
      fcmdSearch:          event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'search',  'http://hl7.org/fhir/restful-operation', SecurityEventActionE);
      fcmdHistoryType:     event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'history-type', 'http://hl7.org/fhir/restful-operation', SecurityEventActionR);
      fcmdValidate:        event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'validate', 'http://hl7.org/fhir/restful-operation', SecurityEventActionE);
      fcmdConformanceStmt: event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'conformance',    'http://hl7.org/fhir/restful-operation', SecurityEventActionE);
      fcmdTransaction:     event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'transaction', 'http://hl7.org/fhir/restful-operation', SecurityEventActionE);
      fcmdHistorySystem:   event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'history-system', 'http://hl7.org/fhir/restful-operation', SecurityEventActionR);
      fcmdUpload:          event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'upload', 'http://hl7.org/fhir/restful-operation', SecurityEventActionE);
      fcmdGetTags:         event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'tags-get', 'http://hl7.org/fhir/restful-operation', SecurityEventActionR);
      fcmdUpdateTags:      event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'tags-update', 'http://hl7.org/fhir/restful-operation', SecurityEventActionU);
      fcmdDeleteTags:     event('rest', 'http://hl7.org/fhir/security-event-type', 'Restful Operation', 'tags-delete', 'http://hl7.org/fhir/restful-operation', SecurityEventActionD);
    else // fcmdUnknown
      raise exception.create('unknown operation');
    end;
    if httpCode < 300 then
      se.event.outcomeST := SecurityEventOutcome0
    else if httpCode < 500 then
      se.event.outcomeST := SecurityEventOutcome4
    else
      se.event.outcomeST := SecurityEventOutcome8; // no way we are going down...
    se.event.dateTimeST := NowUTC;

    se.source := TFhirSecurityEventSource.create;
    se.source.siteST := 'Cloud';
    se.source.identifierST := 'HL7Connect';
    c := se.source.type_List.Append;
    c.codeST := '3';
    c.displayST := 'Web Server';
    c.systemST := 'http://hl7.org/fhir/security-source-type';

    // participant - the web browser / user proxy
    p := se.participantList.Append;
    p.userIdST := inttostr(session.Key);
    p.altIdST := session.Id;
    p.nameST := session.Name;
    p.requestorST := true;
    p.network := TFhirSecurityEventParticipantNetwork.create;
    p.network.identifierST := ip;
    p.network.type_ST := NetworkType2;

    if resourceType <> frtNull then
    begin
      o := se.object_List.Append;
      o.reference := TFhirResourceReference.create;
      if ver <> '' then
        o.reference.referenceST := CODES_TFHIRResourceType[resourceType]+'/'+id+'/_history/'+ver
      else if id <> '' then
        o.reference.referenceST := CODES_TFHIRResourceType[resourceType]+'/'+id;
      o.type_ST := ObjectType2;
      case op of
        fcmdMailbox :        o.lifecycleST := ObjectLifecycle6;
        fcmdRead:            o.lifecycleST := ObjectLifecycle6;
        fcmdVersionRead:     o.lifecycleST := ObjectLifecycle6;
        fcmdUpdate:          o.lifecycleST := ObjectLifecycle3;
        fcmdDelete:          o.lifecycleST := ObjectLifecycle14;
        fcmdHistoryInstance: o.lifecycleST := ObjectLifecycle9;
        fcmdCreate:          o.lifecycleST := ObjectLifecycle1;
        fcmdSearch:          o.lifecycleST := ObjectLifecycle6;
        fcmdHistoryType:     o.lifecycleST := ObjectLifecycle9;
        fcmdValidate:        o.lifecycleST := ObjectLifecycle4;
        fcmdConformanceStmt: o.lifecycleST := ObjectLifecycle6;
        fcmdTransaction:     o.lifecycleST := ObjectLifecycle3;
        fcmdHistorySystem:   o.lifecycleST := ObjectLifecycle9;
        fcmdUpload:          o.lifecycleST := ObjectLifecycle9;
        fcmdGetTags:         o.lifecycleST := ObjectLifecycle6;
        fcmdUpdateTags:      o.lifecycleST := ObjectLifecycle3;
        fcmdDeleteTags:      o.lifecycleST := ObjectLifecycle14;
      end;
      if op = fcmdSearch then
        o.queryST := name
      else
        o.nameST := name;
    end;
    FAudits.add(se.link);
  finally
    se.Free;
  end;
end;

procedure TFhirOperation.storeAudits;
var
  i : integer;
begin
  Connection.StartTransact;
  try
    // cut us off from the external request
    request := TFHIRRequest.create;
     response := TFHIRResponse.create;
    for i := 0 to FAudits.count - 1 do
    begin
      request.ResourceType := frtSecurityEvent;
      request.CommandType := fcmdCreate;
      request.Resource := FAudits[i].link;
      request.lastModifiedDate := TFhirSecurityEvent(FAudits[i]).event.dateTimeST.AsUTCDateTime;
      request.Session := nil;
      Execute;
    end;
    Connection.Commit;
  except
    on e : Exception do
    begin
      Connection.Rollback;
      raise;
    end;
  end;
end;


procedure TFhirOperation.ReIndex;
var
  list : TStringList;
  i : integer;
  r : TFHirResource;
  parser : TFHIRParser;
  m : TFHIRIndexManager;
  k : integer;
  tags : TFHIRAtomCategoryList;
begin
  Connection.ExecSQL('delete from SearchEntries');
  Connection.ExecSQL('delete from Searches');
  Connection.ExecSQL('delete from IndexEntries');

  k := Connection.CountSQL('select Max(IndexKey) from Indexes');
  m := TFHIRIndexManager.create(nil);
  try
    m.Ucum := FRepository.TerminologyServer.Ucum.Link;
    m.KeyEvent := FRepository.GetNextKey;
    for i := 0 to m.Indexes.count - 1 do
    begin
      if Connection.CountSQL('select Count(IndexKey) from Indexes where Name = '''+ m.indexes[i].Name+'''') = 0 then
      begin
        Connection.Sql := 'insert into Indexes (IndexKey, Name) values (:k, :d)';
        Connection.prepare;
        Connection.bindInteger('k', k);
        Connection.bindString('d', m.indexes[i].Name);
        Connection.execute;
        inc(k);
        Connection.terminate;
      end;
    end;
  finally
    m.free;
  end;

  list := TStringList.create;
  try
    Connection.SQL := 'select * from Ids where ResourceTypeKey in (Select ResourceTypeKey from Types where ResourceName != ''Binary'')';
    Connection.prepare;
    Connection.execute;
    while Connection.fetchnext do
      list.addObject(Connection.ColStringByName['Id'], TObject(connection.ColIntegerByName['ResourceKey']));
    Connection.terminate;
    for i := 0 to list.count - 1 do
    begin
      FConnection.sql := 'select Tags, Content from Versions where Deleted != 1 and resourceVersionkey in (Select MostRecent from  Ids where ResourceKey = '+inttostr(Integer(list.objects[i]))+')';
      FConnection.prepare;
      FConnection.execute;
      if FConnection.FetchNext then
      begin
        tags := TFHIRAtomCategoryList.create;
        try
          tags.DecodeJson(Connection.ColMemoryByName['Tags']);
          parser := MakeParser('en', ffXml, Connection.ColMemoryByName['Content']);
          try
            r := parser.resource;
            FConnection.terminate;
            Connection.StartTransact;
            try
              FIndexer.execute(Integer(list.objects[i]), list[i], r, tags);
              Connection.Commit;
            except
              Connection.Rollback;
              raise;
            end;
          finally
            parser.free;
          end;
        finally
          tags.free;
        end;
      end
      else
        FConnection.terminate;
    end;
  finally
    list.free;
  end;
end;


procedure TFhirOperation.clear(a: TFhirResourceTypeSet);
var
  i : TFhirResourceType;
  k, l : string;
begin
  Connection.ExecSQL('delete from SearchEntries');
  Connection.ExecSQL('delete from Searches');
  Connection.ExecSQL('delete from IndexEntries');

  for i := Low(TFhirResourceType) to High(TFhirResourceType) do
  begin
    if i in a then
    begin
      k := inttostr(Connection.CountSQL('select ResourceTypeKey from Types where ResourceName = '''+CODES_TFhirResourceType[i]+''''));
      // first thing we need to do is to make a list of all the resources that are going to be deleted
      // the list includes any resources of type k
      // any contained resources of type k
      l := '';
      connection.sql := 'select ResourceKey from Ids where ResourceTypeKey = '+k+' or ResourceKey in (select MasterResourceKey from Ids where ResourceTypeKey = '+k+')';
      connection.prepare;
      connection.Execute;
      while Connection.FetchNext do
        CommaAdd(l, connection.ColStringByName['ResourceKey']);
      connection.terminate;
      if (l <> '') then
      begin
        // now, any resources contained by either of those
        connection.sql := 'select ResourceKey from Ids where MasterResourceKey in ('+l+')';
        connection.prepare;
        connection.Execute;
        while Connection.FetchNext do
          CommaAdd(l, connection.ColStringByName['ResourceKey']);
        connection.terminate;

        Connection.ExecSQL('update ids set MostRecent = null where ResourceKey in ('+l+')');
        Connection.ExecSQL('delete from VersionTags where ResourceVersionKey in (select ResourceVersionKey from Versions where ResourceKey in ('+l+'))');
        Connection.ExecSQL('delete from Versions where ResourceKey in ('+l+')');
        Connection.ExecSQL('delete from Compartments where ResourceKey in ('+l+')');
        Connection.ExecSQL('delete from Compartments where CompartmentKey in ('+l+')');
        Connection.ExecSQL('delete from ids where ResourceKey in ('+l+')');
      end;
    end;
  end;
  Reindex;
end;

function TFhirOperation.GetProfileById(id: String): TFHIRProfile;
var
  resourceKey : integer;
  originalId : String;
  parser : TFHIRParser;
begin
  if (length(request.id) <= 36) and FindResource(frtProfile, id, false, resourceKey, originalId, nil, '') then
  begin
    FConnection.SQL := 'Select * from Versions where ResourceKey = '+inttostr(resourceKey)+' order by ResourceVersionKey desc';
    FConnection.Prepare;
    try
      FConnection.Execute;
      FConnection.FetchNext;
      parser := MakeParser(lang, ffXml, FConnection.ColMemoryByName['Content']);
      try
        result := parser.resource.Link as TFHIRProfile;
      finally
        parser.free;
      end;
    finally
      FConnection.Terminate;
    end;
  end
  else
    raise Exception.create('Unknown Profile '+id);
end;

procedure TFhirOperation.CheckCompartments(actual, allowed: String);
var
  act, all : TStringList;
  i : integer;
begin
  if allowed <> '' then
  begin
    act := TStringList.create;
    all := TStringList.create;
    try
      act.CommaText := actual;
      all.CommaText := allowed;
      for i := 0 to act.count - 1 do
        if all.IndexOf(act[i]) < 0 then
          raise Exception.create('Compartment error: no access to compartment for patient '+act[i]);
    finally
      act.free;
      all.free;
    end;
  end;
end;

procedure TFhirOperation.ProcessMsgQuery(request: TFHIRRequest; response: TFHIRResponse; feed : TFHIRAtomFeed);
//var
//  query
begin
  raise exception.create('query-response is not yet supported');
end;

function TFhirOperation.BuildResponseMessage(request: TFHIRRequest; incoming: TFhirMessageHeader): TFhirMessageHeader;
var
  dst : TFhirMessageHeaderDestination;
begin
  result := TFhirMessageHeader.create;
  try
    result.identifierST := GUIDToString(CreateGUID);
    result.timestampST := NowUTC;
    result.event := incoming.event.Clone;
    result.response := TFhirMessageHeaderResponse.create;
    result.response.identifierST := GUIDToString(CreateGUID);;
    result.response.codeST := ResponseCodeOk;
    dst := result.destinationList.Append;
    if incoming.source <> nil then
    begin
      dst.nameST := incoming.source.nameST;
      dst.endpointST := incoming.source.endpointST;
    end
    else
    begin
      dst.nameST := 'No Source Provided';
      dst.endpointST := 'http://example.com/unknown';
    end;
    result.source := TFhirMessageHeaderSource.create;
    result.source.endpointST := request.baseUrl+'/mailbox';
    result.source.nameST := 'Health Intersections';
    result.source.softwareST := 'HL7Connect';
    result.source.versionST := FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION;
    result.source.contact := FFactory.makeContact('email', 'grahame@healthintersections.com.au', '');
    result.link;
  finally
    result.free;
  end;
end;

procedure TFhirOperation.ProcessMessage(request: TFHIRRequest; msg, resp: TFhirMessageHeader; feed: TFHIRAtomFeed);
var
  s : String;
begin
  try
    s := msg.event.codeST;
    if s = 'MedicationAdministration-Complete' then
      raise exception.create('MedicationAdministration-Complete is not yet supported')
    else if s = 'MedicationAdministration-Nullification' then
      raise exception.create('MedicationAdministration-Nullification is not yet supported')
    else if s = 'MedicationAdministration-Recording' then
      raise exception.create('MedicationAdministration-Recording is not yet supported')
    else if s = 'MedicationAdministration-Update' then
      raise exception.create('MedicationAdministration-Update is not yet supported')
    else if s = 'admin-notify' then
      raise exception.create('admin-notify is not yet supported')
    else if s = 'diagnosticreport-provide' then
      raise exception.create('diagnosticreport-provide is not yet supported')
    else if s = 'observation-provide' then
      raise exception.create('observation-provide is not yet supported')
    else if s = 'query' then
      ProcessMsgQuery(request, response, feed)
    else if s = 'query-response' then
      raise exception.create('query-response is not yet supported')
//    else if s = 'make-claim' then
//      ProcessMsgClaim(request, msg, resp, request.feed, feed)
    else
      raise exception.create('Unknown message event: "'+s+'"');

  except
    on e:exception do
    begin
      resp.response.codeST := ResponseCodeFatalError;
      resp.response.details := FFactory.makeReferenceText(e.message);
    end;
  end;
end;

{
procedure TFhirOperation.ProcessMsgClaim(request : TFHIRRequest; incoming, outgoing : TFhirMessageHeader; infeed, outfeed: TFHIRAtomFeed);
var
  id : string;
  claim : TFhirClaim;
  rem : TFhirRemittance;
  i : integer;
  svc : TFhirRemittanceService;
  entry : TFHIRAtomEntry;
  utc : TDateAndTime;
  context : TFHIRValidatorContext;
begin
  context := FRepository.Validator.AcquireContext;
  try
    claim := GetResourceFromFeed(infeed, incoming.dataList[0]) as TFhirClaim;
    id := MessageCreateResource(context, request, claim);
    rem := TFhirRemittance.create;
    try
      rem.identifier := FFactory.makeIdentifier('urn:ietf:rfc:3986', NewGuidURN);
      for i := 0 to claim.serviceList.count - 1 do
      begin
        svc := rem.serviceList.Append;
        svc.instanceST := claim.serviceList[i].instanceST;
        svc.rateST := '0.8';
        svc.benefitST := floatToStr(0.8 * StrToFloat(claim.serviceList[i].instanceST));
      end;
      id := request.BaseURL+'/remittance/'+MessageCreateResource(context, request, rem);
      outgoing.dataList.add(FFactory.makeReference(id));
      utc := NowUTC;
      try
        entry := outfeed.addEntry('remittance', utc, id, id, rem);
      finally
        utc.free;
      end;
    finally
      rem.free;
    end;
  finally
    FRepository.Validator.YieldContext(context);
  end;
end;
}

function TFhirOperation.MessageCreateResource(context : TFHIRValidatorContext; request : TFHIRRequest; res: TFHIRResource): string;
var
  req : TFHIRRequest;
  resp : TFHIRResponse;
begin
  req := TFHIRRequest.create;
  try
    req.Session := request.session.Link;
    req.CommandType := fcmdCreate;
    req.ResourceType := res.ResourceType;
    req.Resource := res.Link;
    req.baseUrl := request.BaseUrl;
    resp := TFHIRResponse.create;
    try
      ExecuteCreate(context, false, req, resp, idNoNew, 0);
      result := req.Id;
    finally
      resp.free;
    end;
  finally
    req.free;
  end;
end;                       


procedure TFhirOperation.Inspect();
begin
  // if command is validation, and content wasn't valid...
end;

procedure TFhirOperation.PreCheck();
begin
  FFactory := TFHIRFactory.create(lang);
end;

procedure TFhirOperation.SetRequest(const Value: TFHIRRequest);
begin
  FRequest.Free;
  FRequest := Value;
end;

procedure TFhirOperation.SetResponse(const Value: TFHIRResponse);
begin
  FResponse.Free;
  FResponse := Value;
end;

function TFhirOperation.AddDeletedResourceToFeed(feed: TFHIRAtomFeed; sId, sType, base, originalId: String): TFHIRAtomEntry;
var
  entry : TFHIRAtomEntry;
begin
  entry := TFHIRAtomEntry.Create;
  try
    entry.title := GetFhirMessage('NAME_RESOURCE', lang)+' '+sId+' '+GetFhirMessage('NAME_VERSION', lang)+' '+FConnection.ColStringByName['VersionId']+' ('+GetFhirMessage('NAME_DELETED', lang)+')';
    entry.deleted := true;
    entry.links['self'] := request.baseUrl+lowercase(sType)+'/'+sId+'/_history/'+FConnection.ColStringByName['VersionId'];
    entry.updated := TDateAndTime.CreateUTC(TSToDateTime(FConnection.ColTimeStampByName['StatedDate']));
    entry.id := base+sId;
    entry.published_ := NowUTC;
    entry.authorName := FConnection.ColStringByName['Name'];
    entry.originalId := originalId;
    entry.categories.decodeJson(FConnection.ColMemoryByName['Tags']);
    feed.entries.add(entry.Link);
    result := entry;
  finally
    entry.Free;
  end;
end;

procedure TFhirOperation.checkDateFormat(s: string);
var
  ok : boolean;
begin
  ok := false;
  if (length(s) = 4) and StringIsCardinal16(s) then
    ok := true
  else if (length(s) = 7) and (s[5] = '-') and
          StringIsCardinal16(copy(s, 1, 4)) and StringIsCardinal16(copy(s, 5, 2)) then
    ok := true
  else if (length(s) = 10) and (s[5] = '-') and (s[8] = '-') and
          StringIsCardinal16(copy(s, 1, 4)) and StringIsCardinal16(copy(s, 6, 2)) and StringIsCardinal16(copy(s, 9, 2)) then
    ok := true
  else if (length(s) > 11) and (s[5] = '-') and (s[8] = '-') and (s[11] = 'T') and
          StringIsCardinal16(copy(s, 1, 4)) and StringIsCardinal16(copy(s, 6, 2)) and StringIsCardinal16(copy(s, 9, 2)) then
  begin
    if (length(s) = 16) and (s[14] = '-') and StringIsCardinal16(copy(s, 12, 2)) and StringIsCardinal16(copy(s, 15, 2)) then
      ok := true
    else if (length(s) = 19) and (s[14] = '-') and (s[17] = '-') and
          StringIsCardinal16(copy(s, 12, 2)) and StringIsCardinal16(copy(s, 15, 2)) and StringIsCardinal16(copy(s, 18, 2)) then
      ok := true;
  end;
  if not ok then
    raise exception.create(StringFormat(GetFhirMessage('MSG_DATE_FORMAT', lang), [s]));
end;

{ TReferenceList }

function TReferenceList.asSql: String;
var
  i, j : Integer;
  s : String;
  st : TStringList;
begin
  result := '';
  for i := 0 to Count - 1 Do
  begin
    if i > 0 then
      result := result + ' or ';
    s := '';
    st := TStringList(Objects[i]);
    for j := 0 to st.count - 1 do
      s := s + ', '''+st[j]+'''';
    result := result + '((ResourceTypeKey = (Select ResourceTypeKey from Types where ResourceName = '''+SQLWrapString(Strings[i])+''')) and (Ids.Id in ('+copy(s, 3, $FFFF)+')))';
  end;
end;

procedure TReferenceList.seeReference(id: String);
var
  i : Integer;
  t : TStringList;
  st, si : String;
begin
  StringSplit(id, '/', st, si);
  if (length(si) = 0) then
    exit;

  i := IndexOf(st);
  if i = -1 then
  begin
    t := TStringList.Create;
    t.Sorted := true;
    i := AddObject(st, t);
  end
  else
    t := TStringList(Objects[i]);

  if not t.find(si, i) then
    t.add(si);
end;

{ TAbbreviationServer }

function TAbbreviationServer.next: String;
begin
  result := char(ord('a')+FIndex);
  inc(FIndex);
end;

{ TFHIRTransactionEntryList }

function TFHIRTransactionEntryList.ExistsByTypeAndId(entry : TFHIRTransactionEntry):boolean;
var
  i : integer;
begin
  result := false;
  i := 0;
  while not result and (i < Count) do
  begin
    result := (entries[i].resType = entry.resType) and (entries[i].id = entry.id) and (entries[i] <> entry);
    inc(i);
  end;
end;

function TFHIRTransactionEntryList.GetByName(oName: String): TFHIRTransactionEntry;
begin
  result := TFHIRTransactionEntry(inherited GetByName(oName));
end;

function TFHIRTransactionEntryList.GetEntry(iIndex: Integer): TFHIRTransactionEntry;
begin
  result := TFHIRTransactionEntry(ObjectByIndex[iIndex]);
end;

{ TFHIRTransactionEntry }

function TFHIRTransactionEntry.summary: string;
begin
  result := name;
end;

end.

