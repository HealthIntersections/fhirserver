unit VocabPocServerCore;

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
  DateSupport, HashSupport, GuidSupport, StringSupport,
  AdvGenerics, AdvJson,
  FHIRBase, FHIRSupport, FHIRTypes, FHIRResources, SCIMObjects, FHIRSecurity, FHIRUtilities, FHIRSearch, FHIRPath, FHIRLang,
  FHIRStorageService, FHIRUserProvider, TerminologyServer, FHIRServerContext, TerminologyOperations;

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
    FEngine : TFHIRPathEngine;
    function compareDate(base, min, max : TDateTimeEx; value : String; prefix : TFHIRSearchParamPrefix) : boolean;

    function matches(resource : TFhirResource; sp : TSearchParameter) : boolean;
    function matchesObject(obj : TFhirObject; sp : TSearchParameter) : boolean;
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

    function FindResource(aType, sId : String; options : TFindResourceOptions; var resourceKey, versionKey : integer; request: TFHIRRequest; response: TFHIRResponse; compartments : TAdvList<TFHIRCompartmentId>): boolean; override;
    function GetResourceById(request: TFHIRRequest; aType : String; id, base : String; var needSecure : boolean) : TFHIRResource; override;
    function getResourceByUrl(aType : TFhirResourceType; url, version : string; allowNil : boolean; var needSecure : boolean): TFHIRResource; override;
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
    function FetchResourceCounts(compList : TAdvList<TFHIRCompartmentId>) : TStringList; override;

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

function TTerminologyServerStorage.FetchResourceCounts(compList : TAdvList<TFHIRCompartmentId>): TStringList;
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

function TTerminologyServerOperationEngine.compareDate(base, min, max: TDateTimeEx; value: String; prefix: TFHIRSearchParamPrefix): boolean;
var
  v, vmin, vmax : TDateTimeEx;
begin
  v := TDateTimeEx.fromXML(value);
  vmin := v.Min;
  vmax := v.Max;
  case prefix of
    sppNull: result := v.equal(base);
    sppNotEqual: result := not v.Equal(base);
    sppGreaterThan: result := max.after(vmax, false);
    sppLessThan: result := min.before(vmin, false);
    sppGreaterOrEquals: result := not min.before(vmin, false);
    sppLesserOrEquals: result := not max.after(vmax, false);
    sppStartsAfter: result := min.after(vmax, false);
    sppEndsBefore: result := max.before(vmin, false);
    sppAproximately:
      begin
        min := base.lessPrecision.Min;
        max := base.lessPrecision.Max;
        vmin := v.lessPrecision.Min;
        vmax := v.lessPrecision.Max;
        result := min.between(vmin, vmax, true) or max.between(vmin, vmax, true);
      end;
  end;
end;

constructor TTerminologyServerOperationEngine.create(server: TTerminologyServer; ServerContext : TFHIRServerContext; lang : String);
begin
  inherited Create(ServerContext, lang);
  FServer := server;
  FEngine := TFHIRPathEngine.create(ServerContext.ValidatorContext.Link);
  FOperations.add(TFhirExpandValueSetOperation.create(ServerContext.TerminologyServer.Link));
  FOperations.add(TFhirLookupCodeSystemOperation.create(ServerContext.TerminologyServer.Link));
  FOperations.add(TFhirValueSetValidationOperation.create(ServerContext.TerminologyServer.Link));
  FOperations.add(TFhirConceptMapTranslationOperation.create(ServerContext.TerminologyServer.Link));
  FOperations.add(TFhirSubsumesOperation.create(ServerContext.TerminologyServer.Link));
  FOperations.add(TFhirCodeSystemComposeOperation.create(ServerContext.TerminologyServer.Link));
//   FOperations.add(TFhirConceptMapClosureOperation.create(ServerContext.TerminologyServer.Link, connection)); // uses storage...
end;

destructor TTerminologyServerOperationEngine.Destroy;
begin
  FEngine.Free;
  FServer.Free;
  inherited;
end;


procedure TTerminologyServerOperationEngine.ExecuteHistory(request: TFHIRRequest; response: TFHIRResponse);
var
  offset, count, i : integer;
  bundle : TFHIRBundle;
  base : String;
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
var
  search : TAdvList<TSearchParameter>;
  list, l : TFhirObjectList;
  filtered : TAdvList<TFHIRMetadataResource>;
  o : TFHIRObject;
  res : TFhirMetadataResource;
  bundle : TFHIRBundle;
  base : String;
  isMatch : boolean;
  sp : TSearchParameter;
  i, t, offset, count : integer;
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

  if (request.Parameters.getItemCount = 0) and (response.Format = ffXhtml) and not request.hasCompartments then
    BuildSearchForm(request, response)
  else
  begin
    TypeNotFound(request, response);
    search := TSearchParser.parse(TFHIRServerContext(FServerContext).Indexes, request.ResourceName, request.Parameters);
    try
      base := TSearchParser.buildUrl(search);

      bundle := TFHIRBundle.Create(BundleTypeSearchset);
      try
        bundle.meta := TFHIRMeta.create;
        bundle.meta.lastUpdated := TDateTimeEx.makeUTC;
        bundle.link_List.AddRelRef('self', base);
        bundle.id := FhirGUIDToString(CreateGUID);

        if request.ResourceName = 'CodeSystem' then
          list := FServer.GetCodeSystemList
        else if request.ResourceName = 'ValueSet' then
          list := FServer.GetValueSetList
        else if (request.ResourceName = '') and (request.Parameters.GetVar('_type').Contains('ValueSet') or request.Parameters.GetVar('_type').Contains('CodeSystem')) then
        begin
          list := TFhirObjectList.Create;
          if request.Parameters.GetVar('_type').Contains('CodeSystem') then
          begin
            l := FServer.GetCodeSystemList;
            try
              list.AddAll(l);
            finally
              l.Free;
            end;
          end;
          if request.Parameters.GetVar('_type').Contains('ValueSet') then
          begin
            l := FServer.GetValueSetList;
            try
              list.AddAll(l);
            finally
              l.Free;
            end;
          end;

        end
        else
          raise Exception.Create('Unsupported Resource Type '+request.resourceName);

        try
          filtered := TAdvList<TFHIRMetadataResource>.create;
          try
            for o in list do
            begin
              res := o as TFhirMetadataResource;
              isMatch := true;
              for sp in search do
                if isMatch and not matches(res, sp) then
                  isMatch := false;
              if isMatch then
              filtered.add(res.link);
            end;

            bundle.total := inttostr(filtered.count);
            if (offset > 0) or (Count < filtered.count) then
            begin
              bundle.link_List.AddRelRef('first', base+'&'+SEARCH_PARAM_NAME_OFFSET+'=0&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
              if offset - count >= 0 then
                bundle.link_List.AddRelRef('previous', base+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset - count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
              if offset + count < list.count then
                bundle.link_List.AddRelRef('next', base+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset + count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
              if count < list.count then
                bundle.link_List.AddRelRef('last', base+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr((filtered.count div count) * count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
            end;

            i := 0;
            t := 0;
            for res in filtered do
            begin
              inc(i);
              if (i > offset) then
              begin
                be := bundle.entryList.Append;
                be.fullUrl := res.url;
                be.resource := res.Link;
                inc(t);
                if (t = count) then
                  break;
              end;
            end;
          finally
            filtered.free;
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
    finally
      search.free;
    end;


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

function TTerminologyServerOperationEngine.FindResource(aType, sId : String; options : TFindResourceOptions; var resourceKey, versionKey : integer; request: TFHIRRequest; response: TFHIRResponse; compartments : TAdvList<TFHIRCompartmentId>): boolean;
var
  res : TFhirMetadataResource;
begin
  result := false;
  if aType = 'CodeSystem' then
  begin
    res := FServer.getCodeSystemById(sId);
    try
      if check(response, res <> nil, 410, lang, StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [aType+'/'+request.id]), IssueTypeNotFound) then
      begin
        result := true;
        resourceKey := res.tagInt;
        versionKey := 1;
      end;
    finally
      res.Free;
    end;
  end
  else if aType = 'ValueSet' then
  begin
    res := FServer.getValueSetById(sId);
    try
      if check(response, res <> nil, 410, lang, StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [aType+'/'+request.id]), IssueTypeNotFound) then
      begin
        result := true;
        resourceKey := res.tagInt;
        versionKey := 2;
      end;
    finally
      res.Free;
    end;
  end
  else
    check(response, false, 404 , lang, StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [aType+'/'+sId]), IssueTypeNotFound);
end;

function TTerminologyServerOperationEngine.GetResourceById(request: TFHIRRequest; aType, id, base: String; var needSecure: boolean): TFHIRResource;
begin
  needSecure := false;
  if aType = 'CodeSystem' then
    result := FServer.getCodeSystemById(id)
  else if aType = 'ValueSet' then
    result := FServer.getValueSetById(id)
  else
    result := nil;
end;

function TTerminologyServerOperationEngine.getResourceByUrl(aType: TFhirResourceType; url, version: string; allowNil: boolean; var needSecure: boolean): TFHIRResource;
begin
  needSecure := false;
  if aType = frtCodeSystem then
    result := FServer.getCodeSystem(url)
  else if aType = frtValueSet then
    result := FServer.getValueSetByUrl(url)
  else
    result := nil;
end;

function TTerminologyServerOperationEngine.matches(resource: TFhirResource; sp: TSearchParameter): boolean;
var
  selection : TFHIRSelectionList;
  so : TFHIRSelection;
  parser : TFHIRPathParser;
begin
  if sp.index.expression = nil then
  begin
    parser := TFHIRPathParser.Create;
    try
      sp.index.expression := parser.parse(sp.index.Path);
    finally
      parser.Free;
    end;
  end;

  selection := FEngine.evaluate(resource, resource, resource, sp.index.expression);
  try
    if sp.modifier = spmMissing then
    begin
      if sp.value = 'true' then
        result := selection.Empty
      else if sp.value = 'false' then
        result := not selection.Empty
      else
        raise Exception.Create('Error Processing search parameter (:missing, value = '+sp.value+')');
    end
    else if selection.Empty then
      result := false
    else
    begin
      result := false;
      for so in selection do
        result := result or matchesObject(so.value, sp);
    end;
  finally
    selection.Free;
  end;
end;

function TTerminologyServerOperationEngine.matchesObject(obj: TFhirObject; sp: TSearchParameter): boolean;
begin
  case sp.index.SearchType of
    SearchParamTypeNull: raise Exception.Create('param.type = null');
    SearchParamTypeNumber: raise Exception.Create('not done yet');
//      if obj.isPrimitive then
//        result := compareNumber(obj.primitiveValue, sp.value, sp.prefix)
//      else
//        result := false;
    SearchParamTypeDate:
      if obj is TFHIRDate then
        result := compareDate(TFHIRDate(obj).value, TFHIRDate(obj).value.Min, TFHIRDate(obj).value.Max, sp.value, sp.prefix)
      else if obj is TFHIRDateTime then
        result := compareDate(TFHIRDateTime(obj).value, TFHIRDateTime(obj).value.Min, TFHIRDateTime(obj).value.Max, sp.value, sp.prefix)
      else if obj is TFHIRInstant then
        result := compareDate(TFHIRInstant(obj).value, TFHIRInstant(obj).value.Min, TFHIRInstant(obj).value.Max, sp.value, sp.prefix)
      else
        result := false;
    SearchParamTypeString:
      if not obj.isPrimitive then
        result := false
      else if sp.modifier = spmNull then
        result := RemoveAccents(obj.primitiveValue.ToLower).StartsWith(RemoveAccents(sp.value.ToLower))
      else if sp.modifier = spmContains then
        result := RemoveAccents(obj.primitiveValue.ToLower).contains(RemoveAccents(sp.value.ToLower))
      else if sp.modifier = spmExact then
        result := obj.primitiveValue = sp.value
      else if sp.modifier = spmExact then
        raise Exception.Create('Modifier is not supported');
    SearchParamTypeToken: raise Exception.Create('not done yet');
    SearchParamTypeReference: raise Exception.Create('not done yet');
    SearchParamTypeComposite: raise Exception.Create('not done yet');
    SearchParamTypeQuantity: raise Exception.Create('not done yet');
    SearchParamTypeUri:
      if not obj.isPrimitive then
        result := false
      else if sp.modifier = spmNull then
        result := obj.primitiveValue = sp.value
      else if sp.modifier = spmAbove then
        result := sp.value.StartsWith(obj.primitiveValue)
      else if sp.modifier = spmBelow then
        result := obj.primitiveValue.StartsWith(sp.value)
      else if sp.modifier = spmExact then
        raise Exception.Create('Modifier is not supported');
  end;
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
