unit VocabPocServerCore;

interface

uses
  SysUtils, Classes,
  DateSupport, HashSupport, GuidSupport,
  AdvJson,
  FHIRBase, FHIRSupport, FHIRTypes, FHIRResources, SCIMObjects, FHIRSecurity, FHIRUtilities,
  FHIRStorageService, FHIRUserProvider, TerminologyServer, FHIRServerContext;

const
  TX_SEARCH_PAGE_DEFAULT = 10;
  TX_SEARCH_PAGE_LIMIT = 20;


type
  TTerminologyServerUserProvider = class (TFHIRUserProvider)
  public
    Function loadUser(key : integer) : TSCIMUser; overload; override;
    Function loadUser(id : String; var key : integer) : TSCIMUser; overload; override;
    function CheckLogin(username, password : String; var key : integer) : boolean; override;
    function CheckId(id : String; var username, hash : String) : boolean; override;
    function loadOrCreateUser(id, name, email : String; var key : integer) : TSCIMUser; override;
    function allowInsecure : boolean; override;
  end;

  TTerminologyServerOperationEngine = class (TFHIROperationEngine)
  private
    FServer : TTerminologyServer;
    function ExecuteCreate(context: TOperationContext; request: TFHIRRequest;
      response: TFHIRResponse; idState: TCreateIdState;
      iAssignedKey: Integer): String;
    function ExecuteUpdate(context: TOperationContext; request: TFHIRRequest;
      response: TFHIRResponse): Boolean;
  protected
    procedure StartTransaction; override;
    procedure CommitTransaction; override;
    procedure RollbackTransaction; override;

    function ExecuteRead(request: TFHIRRequest; response : TFHIRResponse; ignoreHeaders : boolean) : boolean; override;
    procedure ExecuteHistory(request: TFHIRRequest; response : TFHIRResponse); override;
    procedure ExecuteSearch(request: TFHIRRequest; response : TFHIRResponse); override;
  public
    Constructor create(server : TTerminologyServer; ServerContext : TFHIRServerContext; lang : String);
    Destructor Destroy; override;
  end;

  TTerminologyServerStorage = class (TFHIRStorageService)
  private
    FServer : TTerminologyServer;
    FServerContext : TFHIRServerContext;
  protected
    function GetTotalResourceCount: integer; override;
  public
    Constructor create(server : TTerminologyServer);
    Destructor Destroy; override;

    // no OAuth Support

    // server total counts:
    function FetchResourceCounts(comps : String) : TStringList; override;

    procedure RecordFhirSession(session: TFhirSession); override;
    procedure CloseFhirSession(key: integer); override;
    procedure QueueResource(r: TFhirResource); overload; override;
    procedure QueueResource(r: TFhirResource; dateTime: TDateTimeEx); overload; override;
    procedure RegisterAuditEvent(session: TFhirSession; ip: String); override;

    function ProfilesAsOptionList : String; override;

    procedure ProcessSubscriptions; override;
    procedure ProcessObservations; override;
    procedure RunValidation; override;

    function createOperationContext(lang : String) : TFHIROperationEngine; override;
    Procedure Yield(op : TFHIROperationEngine; exception : Exception); override;

    Property Server : TTerminologyServer read FServer;
    Property ServerContext : TFHIRServerContext read FServerContext write FServerContext; // no own
  end;

implementation

{ TTerminologyServerStorage }

procedure TTerminologyServerStorage.CloseFhirSession(key: integer);
begin
  // nothing;
end;

constructor TTerminologyServerStorage.create(server : TTerminologyServer);
begin
  inherited Create;
  FServer := server;
end;

function TTerminologyServerStorage.createOperationContext(lang: String): TFHIROperationEngine;
begin
  result := TTerminologyServerOperationEngine.create(FServer.Link, FServerContext, lang);
end;

destructor TTerminologyServerStorage.Destroy;
begin
  FServer.Free;
  inherited;
end;

function TTerminologyServerStorage.FetchResourceCounts(comps: String): TStringList;
begin
  result := TStringList.create;
  result.AddObject('ValueSet', TObject(FServer.ValueSetCount));
  result.AddObject('CodeSystem', TObject(FServer.CodeSystemCount));
end;

function TTerminologyServerStorage.GetTotalResourceCount: integer;
begin
  result := 0;
end;

procedure TTerminologyServerStorage.ProcessObservations;
begin
  // nothing
end;

procedure TTerminologyServerStorage.ProcessSubscriptions;
begin
  // nothing
end;

function TTerminologyServerStorage.ProfilesAsOptionList: String;
begin
  // nothing
end;

procedure TTerminologyServerStorage.QueueResource(r: TFhirResource; dateTime: TDateTimeEx);
begin
  // nothing
end;

procedure TTerminologyServerStorage.QueueResource(r: TFhirResource);
begin
  // nothing
end;

procedure TTerminologyServerStorage.RecordFhirSession(session: TFhirSession);
begin
  // nothing
end;

procedure TTerminologyServerStorage.RegisterAuditEvent(session: TFhirSession; ip: String);
begin
  // nothing
end;

procedure TTerminologyServerStorage.RunValidation;
begin
  // nothing
end;

procedure TTerminologyServerStorage.Yield(op: TFHIROperationEngine; exception: Exception);
begin
  op.Free;
end;

{ TTerminologyServerUserProvider }

function TTerminologyServerUserProvider.allowInsecure: boolean;
begin
  result := true;
end;

function TTerminologyServerUserProvider.CheckId(id: String; var username, hash: String): boolean;
begin
  if (id = 'user') then
  begin
    result := false;
    userName := 'User';
    hash := inttostr(HashStringToCode32('User'));
  end
  else
    result := false;
end;

function TTerminologyServerUserProvider.CheckLogin(username, password: String; var key: integer): boolean;
begin
  result := (username = 'user') and (HashStringToCode32('Password') = HashStringToCode32(password));
  if result then
    Key := 1;
end;

function TTerminologyServerUserProvider.loadOrCreateUser(id, name, email: String; var key: integer): TSCIMUser;
begin
  key := 1;
  result := loadUser(key);
end;

function TTerminologyServerUserProvider.loadUser(key: integer): TSCIMUser;
begin
  result := TSCIMUser.Create(TJsonObject.Create);
  result.userName := 'User';
  result.formattedName := 'User';
  result.addEntitlement(SCIM_SMART_PREFIX+'user/*.*');
end;

function TTerminologyServerUserProvider.loadUser(id: String; var key: integer): TSCIMUser;
begin
  key := 1;
  result := LoadUser(key);
end;

{ TTerminologyServerOperationEngine }

procedure TTerminologyServerOperationEngine.CommitTransaction;
begin
  // nothing
end;

constructor TTerminologyServerOperationEngine.create(server: TTerminologyServer; ServerContext : TFHIRServerContext; lang : String);
begin
  inherited Create(ServerContext, lang);
  FServer := server;
end;

destructor TTerminologyServerOperationEngine.Destroy;
begin
  FServer.Free;
  inherited;
end;

function TTerminologyServerOperationEngine.ExecuteCreate(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse; idState: TCreateIdState; iAssignedKey: Integer): String;
begin
  raise Exception.Create('Not Done Yet');
end;

procedure TTerminologyServerOperationEngine.ExecuteHistory(request: TFHIRRequest; response: TFHIRResponse);
var
  offset, count, i : integer;
  bundle : TFHIRBundle;
  ok : boolean;
//  id : String;
//  dummy : TFHIRSummaryOption;
//  link : string;
//  sql : String;
//  title : String;
  base : String;
////  keys : TStringList;
//  total : integer;
//  field : String;
//  comp : TFHIRParserClass;
//  needsObject : boolean;
//  type_ : String;
  list : TFHIRObjectList;
  o : TFHIRObject;
  res : TFhirMetadataResource;
  be : TFhirBundleEntry;
begin
  offset := 0;
  count := 50;
  for i := 0 to request.Parameters.getItemCount - 1 do
    if request.Parameters.VarName(i) = SEARCH_PARAM_NAME_OFFSET then
      offset := StrToIntDef(request.Parameters.Value[request.Parameters.VarName(i)], 0)
    else if request.Parameters.VarName(i) = '_count' then
      count := StrToIntDef(request.Parameters.Value[request.Parameters.VarName(i)], 0);
  if (count < 2) then
    count := TX_SEARCH_PAGE_DEFAULT
  else if (Count > TX_SEARCH_PAGE_LIMIT) then
    count := TX_SEARCH_PAGE_LIMIT;
  if offset < 0 then
    offset := 0;
  base:= '';

  bundle := TFHIRBundle.Create(BundleTypeHistory);
  try
    if response.Format <> ffUnspecified then
      base := base + '&_format='+MIMETYPES_TFHIRFormat[response.Format]+'&';
    bundle.meta := TFHIRMeta.create;
    bundle.meta.lastUpdated := TDateTimeEx.makeUTC;
    bundle.link_List.AddRelRef('self', base);
    bundle.id := FhirGUIDToString(CreateGUID);

    if request.ResourceName = 'CodeSystem' then
      list := FServer.GetCodeSystemList
    else if request.ResourceName = 'ValueSet' then
      list := FServer.GetValueSetList
    else
      raise Exception.Create('Unsupported Resource Type');
    try
      bundle.total := inttostr(list.count);
      if (offset > 0) or (Count < list.count) then
      begin
        bundle.link_List.AddRelRef('first', base+'&'+SEARCH_PARAM_NAME_OFFSET+'=0&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
        if offset - count >= 0 then
          bundle.link_List.AddRelRef('previous', base+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset - count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
        if offset + count < list.count then
          bundle.link_List.AddRelRef('next', base+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset + count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
        if count < list.count then
          bundle.link_List.AddRelRef('last', base+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr((list.count div count) * count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
      end;

      for o in list do
      begin
        res := o as TFhirMetadataResource;
        be := bundle.entryList.Append;
        be.fullUrl := res.url;
        be.resource := res.Link;
      end;

    finally
      list.Free;
    end;

    response.HTTPCode := 200;
    response.Message := 'OK';
    response.Body := '';
    response.bundle := bundle.Link;
  finally
    bundle.Free;
  end;
end;

function TTerminologyServerOperationEngine.ExecuteRead(request: TFHIRRequest; response: TFHIRResponse; ignoreHeaders: boolean): boolean;
var
  res : TFhirMetadataResource;
begin
  result := false;
  if request.ResourceName = 'CodeSystem' then
    res := FServer.getCodeSystemById(request.Id)
  else if request.ResourceName = 'ValueSet' then
    res := FServer.getValueSetById(request.Id)
  else
    res := nil;
  try
    if res <> nil then
    begin
      response.HTTPCode := 200;
      response.Message := 'OK';
      response.Resource := res.link;
      result := true;
    end
    else
    begin
      response.HTTPCode := 404;
      response.Message := 'Not Found';
      response.Resource := BuildOperationOutcome(lang, 'not found', IssueTypeUnknown);
    end;
  finally
    res.Free;
  end;
end;

procedure TTerminologyServerOperationEngine.ExecuteSearch(request: TFHIRRequest; response: TFHIRResponse);
//var
//  bundle : TFHIRBundle;
//  id, link, base, sql, field : String;
//  i, total, t : Integer;
//  key : integer;
//  offset, count : integer;
//  ok, reverse : boolean;
//  summaryStatus : TFHIRSummaryOption;
//  title: string;
//  keys : TKeyList;
//  comp : TFHIRParserClass;
//  needsObject : boolean;
//  op : TFHIROperationOutcome;
//  type_ : String;
//  be : TFhirBundleEntry;
begin
  if (request.Parameters.getItemCount = 0) and (response.Format = ffXhtml) and (request.compartmentId = '') then
    BuildSearchForm(request, response)
  else
  begin
    TypeNotFound(request, response);
//      if request.resourceName <> '' then
//      begin
//        key := FConnection.CountSQL('select ResourceTypeKey from Types where supported = 1 and ResourceName = '''+request.ResourceName+'''');
//        if not check(response, key > 0, 404, lang, 'Resource Type '+request.ResourceName+' not known', IssueTypeNotSupported) then
//            ok := false;
//      end
//      else
//        key := 0;
//
//      if ok then
//      begin
//        bundle := TFHIRBundle.Create(BundleTypeSearchset);
//        op := TFhirOperationOutcome.Create;
//        keys := TKeyList.Create;
//        try
////          bundle.base := request.baseUrl;
//          bundle.meta := TFhirMeta.Create;
//          bundle.meta.lastUpdated := TDateTimeEx.makeUTC;
//
//          summaryStatus := request.Summary;
//          if FindSavedSearch(request.parameters.value[SEARCH_PARAM_NAME_ID], request.Session, 1, id, link, sql, title, base, total, summaryStatus, request.strictSearch, reverse) then
//            link := SEARCH_PARAM_NAME_ID+'='+request.parameters.value[SEARCH_PARAM_NAME_ID]
//          else
//            id := BuildSearchResultSet(key, request.Session, request.resourceName, request.Parameters, request.baseUrl, request.compartments, request.compartmentId, op, link, sql, total, summaryStatus, request.strictSearch, reverse);
//
//          bundle.total := inttostr(total);
//          bundle.Tags['sql'] := sql;
//
//          base := AppendForwardSlash(Request.baseUrl)+request.ResourceName+'?';
//          if response.Format <> ffUnspecified then
//            base := base + '_format='+MIMETYPES_TFHIRFormat[response.Format]+'&';
//          bundle.link_List.AddRelRef('self', base+link);
//
//          offset := StrToIntDef(request.Parameters.getVar(SEARCH_PARAM_NAME_OFFSET), 0);
//          if request.Parameters.getVar(SEARCH_PARAM_NAME_COUNT) = 'all' then
//            count := SUMMARY_SEARCH_PAGE_LIMIT
//          else
//            count := StrToIntDef(request.Parameters.getVar(SEARCH_PARAM_NAME_COUNT), 0);
//          if (count = 0) and request.Parameters.VarExists(SEARCH_PARAM_NAME_COUNT) then
//            summaryStatus := soCount;
//
//          if (summaryStatus <> soCount) then
//          begin
//            if (count < 1) then
//              count := SEARCH_PAGE_DEFAULT
//            else if (summaryStatus = soSummary) and (Count > SUMMARY_SEARCH_PAGE_LIMIT) then
//              count := SUMMARY_SEARCH_PAGE_LIMIT
//            else if (summaryStatus = soText) and (Count > SUMMARY_TEXT_SEARCH_PAGE_LIMIT) then
//              count := SUMMARY_TEXT_SEARCH_PAGE_LIMIT
//            else if (Count > SEARCH_PAGE_LIMIT) then
//              count := SEARCH_PAGE_LIMIT;
//
//            if (offset > 0) or (Count < total) then
//            begin
//              bundle.link_List.AddRelRef('first', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'=0&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
//              if offset - count >= 0 then
//                bundle.link_List.AddRelRef('previous', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset - count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
//              if offset + count < total then
//                bundle.link_List.AddRelRef('next', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset + count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
//              if count < total then
//                bundle.link_List.AddRelRef('last', base+link+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr((total div count) * count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
//            end;
//
//            chooseField(response.Format, summaryStatus, request.loadObjects, field, comp, needsObject);
//            if (not needsObject) and not request.Parameters.VarExists('__wantObject') then // param __wantObject is for internal use only
//              comp := nil;
//
//            FConnection.SQL := 'Select Ids.ResourceKey, Types.ResourceName, Ids.Id, VersionId, Secure, StatedDate, Name, Versions.Status, Score1, Score2, Tags, '+field+' from Versions, Ids, Sessions, SearchEntries, Types '+
//                'where Ids.Deleted = 0 and SearchEntries.ResourceVersionKey = Versions.ResourceVersionKey and Types.ResourceTypeKey = Ids.ResourceTypeKey and '+'Versions.SessionKey = Sessions.SessionKey and SearchEntries.ResourceKey = Ids.ResourceKey and SearchEntries.SearchKey = '+id;
//            if reverse then
//              FConnection.SQL := FConnection.SQL + ' order by SortValue DESC'
//            else
//              FConnection.SQL := FConnection.SQL + ' order by SortValue ASC';
//            FConnection.Prepare;
//            try
//              FConnection.Execute;
//              i := 0;
//              t := 0;
//              while FConnection.FetchNext do
//              Begin
//                inc(i);
//                if (i > offset) then
//                begin
//                  AddResourceTobundle(bundle, request.secure, request.baseUrl, field, comp, SearchEntryModeMatch, false, type_);
//                  keys.Add(TKeyPair.Create(type_, FConnection.ColStringByName['ResourceKey']));
//                  inc(t);
//                end;
//                if (t = count) then
//                  break;
//              End;
//            finally
//              FConnection.Terminate;
//            end;
//
//            processIncludes(request.session, request.secure, request.Parameters.GetVar('_include'), request.Parameters.GetVar('_revinclude'), bundle, keys, field, comp);
//          end;
//
//          bundle.id := FhirGUIDToString(CreateGUID);
//          if (op.issueList.Count > 0) then
//          begin
//            be := bundle.entryList.Append;
//            be.resource := op.Link;
//            be.search := TFhirBundleEntrySearch.Create;
//            be.search.mode := SearchEntryModeOutcome;
//          end;
//
//          //bundle.link_List['self'] := request.url;
//          response.HTTPCode := 200;
//          response.Message := 'OK';
//          response.Body := '';
//          response.Resource := nil;
//          response.bundle := bundle.Link;
//        finally
//          keys.Free;
//          bundle.Free;
//          op.Free;
//        end;
//      end;
  end;
end;

function TTerminologyServerOperationEngine.ExecuteUpdate(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse): Boolean;
begin
  raise Exception.Create('Not Done Yet');
end;

procedure TTerminologyServerOperationEngine.RollbackTransaction;
begin
  // nothing
end;

procedure TTerminologyServerOperationEngine.StartTransaction;
begin
  // nothing
end;

end.
