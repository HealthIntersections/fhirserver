unit SearchProcessor;

interface

uses
  SysUtils, Classes, Generics.Collections, System.Character,
  ParseMap,
  StringSupport, EncodeSupport,
  AdvObjects, DateAndTime, DecimalSupport,
  FHIRBase, FHIRResources, FHIRLang, FHIRConstants, FHIRTypes,
  FHIRIndexManagers, FHIRDataStore, FHIRUtilities, FHIRSearchSyntax, FHIRSupport, ServerUtilities,
  UcumServices;

const
  SEARCH_PARAM_NAME_ID = 'search-id';
  HISTORY_PARAM_NAME_ID = 'history-id';
  SEARCH_PARAM_NAME_OFFSET = 'search-offset';
  SEARCH_PARAM_NAME_TEXT = '_text';
  SEARCH_PARAM_NAME_COUNT = '_count';
  SEARCH_PARAM_NAME_SORT = '_sort';
  SEARCH_PARAM_NAME_SUMMARY = '_summary';
  SEARCH_PARAM_NAME_FILTER = '_filter';
  SEARCH_PAGE_DEFAULT = 50;
  SEARCH_PAGE_LIMIT = 1000;
  SUMMARY_SEARCH_PAGE_LIMIT = 10000;
  SUMMARY_TEXT_SEARCH_PAGE_LIMIT = 10000;


type
  TQuantityOperation = (qopEqual, qopNotEqual, qopLess, qopLessEqual, qopGreater, qopGreaterEqual, qopStartsAfter, qopEndsBefore, qopApproximate);

  TSearchProcessor = class (TAdvObject)
  private
    FResConfig: TConfigArray;
    FLink: String;
    FSort: String;
    FFilter: String;
    FTypeKey: integer;
    FCompartmentId: String;
    FCompartments: String;
    FParams: TParseMap;
    FType: TFHIRResourceType;
    FBaseURL: String;
    FIndexes: TFhirIndexInformation;
    FLang: String;
    FRepository: TFHIRDataStore;
    FSession : TFhirSession;
//    FLeftOpen: boolean;
    FcountAllowed: boolean;
    FReverse: boolean;

    function processValueSetMembership(vs : String) : String;
    function BuildFilter(filter : TFSFilter; parent : char; issuer : TFSCharIssuer; types : TFHIRResourceTypeSet) : String;
    function BuildFilterParameter(filter : TFSFilterParameter; path : TFSFilterParameterPath; parent : char; issuer : TFSCharIssuer; types : TFHIRResourceTypeSet) : String;
    function BuildFilterLogical  (filter : TFSFilterLogical;   parent : char; issuer : TFSCharIssuer; types : TFHIRResourceTypeSet) : String;
    Function ProcessSearchFilter(value : String) : String;
    Function ProcessParam(types : TFHIRResourceTypeSet; name : String; value : String; nested : boolean; var bFirst : Boolean; var bHandled : Boolean) : String;
    procedure SetIndexes(const Value: TFhirIndexInformation);
    procedure SetRepository(const Value: TFHIRDataStore);
    procedure SplitByCommas(value: String; list: TStringList);
    function findPrefix(var value: String; subst: String): boolean;
    procedure checkDateFormat(s : string);
    function BuildParameterNumber(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
    function BuildParameterString(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
    function buildParameterDate(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
    function buildParameterToken(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
    function buildParameterReference(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
    procedure replaceNames(paramPath : TFSFilterParameterPath; components : TDictionary<String, String>); overload;
    procedure replaceNames(filter : TFSFilter; components : TDictionary<String, String>); overload;
    procedure processQuantityValue(name, lang: String; parts: TArray<string>; op: TQuantityOperation; var minv, maxv, space, mincv, maxcv, spaceC: String);
    procedure processNumberValue(value : TSmartDecimal; op : TQuantityOperation; var minv, maxv : String);
    procedure SetSession(const Value: TFhirSession);
    function filterTypes(types: TFHIRResourceTypeSet): TFHIRResourceTypeSet;
    procedure ProcessDateParam(date: TDateAndTime; var Result: string; name, modifier, value: string; key: Integer; types : TFHIRResourceTypeSet);
    procedure ProcessStringParam(var Result: string; name, modifier, value: string; key: Integer; var pfx: string; var sfx: string; types : TFHIRResourceTypeSet);
    procedure ProcessUriParam(var Result: string; name, modifier, value: string; key: Integer; types : TFHIRResourceTypeSet);
    procedure ProcessTokenParam(var Result: string; name, modifier, value: string; key: Integer; types : TFHIRResourceTypeSet);
    procedure ProcessReferenceParam(var Result: string; name, modifier, value: string; key: Integer; types : TFHIRResourceTypeSet);
    procedure ProcessQuantityParam(var Result: string; name, modifier, value: string; key: Integer; types : TFHIRResourceTypeSet);
    procedure ProcessNumberParam(var Result: string; name, modifier, value: string; key: Integer; types : TFHIRResourceTypeSet);
  public
    constructor create(ResConfig: TConfigArray);
    Destructor Destroy; override;
    procedure Build;
//procedure TFhirOperation.ProcessDefaultSearch(typekey : integer; aType : TFHIRResourceType; params : TParseMap; baseURL, compartments, compartmentId : String; id, key : string; var link, sql : String; var total : Integer; var wantSummary : boolean);

    // inbound
    property typekey : integer read FTypeKey write FTypeKey;
    property type_ : TFHIRResourceType read FType write FType;
    property compartmentId : String read FCompartmentId write FCompartmentId;
    property compartments : String read FCompartments write FCompartments;
    property baseURL : String read FBaseURL write FBaseURL;
    property lang : String read FLang write FLang;
    property params : TParseMap read FParams write FParams;
    property indexes : TFhirIndexInformation read FIndexes write SetIndexes;
    property repository : TFHIRDataStore read FRepository write SetRepository;
    property session : TFhirSession read FSession write SetSession;
    property countAllowed : boolean read FcountAllowed write FcountAllowed;

    // outbound
    property link_ : String read FLink write FLink;
    property sort : String read FSort write FSort;
    property filter : String read FFilter write FFilter;
    property reverse : boolean read FReverse write FReverse;
  end;


implementation


function lt(name, value : String) :String;
begin
  if value.StartsWith('-') then
    result := '(Left('+name+', 1) = ''-'' and '+name+' > '''+value+''')'
  else
    result := name+' < '''+value+'''';
end;

function gt(name, value : String) :String;
begin
  if value.StartsWith('-') then
    result := '((Left('+name+', 1) = ''-'' and '+name+' < '''+value+''')) or ((Left('+name+', 1) <> ''-'' and '+name+' > '''+value+'''))'
  else
    result := name+' > '''+value+'''';
end;

function lte(name, value : String) :String;
begin
  if value.StartsWith('-') then
    result := '(Left('+name+', 1) = ''-'' and '+name+' >= '''+value+''')'
  else
    result := name+' <= '''+value+'''';
end;

function gte(name, value : String) :String;
begin
  if value.StartsWith('-') then
    result := '((Left('+name+', 1) = ''-'' and '+name+' <= '''+value+''')) or ((Left('+name+', 1) <> ''-'' and '+name+' > '''+value+'''))'
  else
    result := name+' >= '''+value+'''';
end;


{ TSearchProcessor }

procedure TSearchProcessor.Build;
var
  first : boolean;
  handled : boolean;
  i, j : integer;
  ix : TFhirIndex;
  ts : TStringList;
begin
  if typekey = 0 then
    filter := 'Ids.MasterResourceKey is null'
  else
    filter := 'Ids.MasterResourceKey is null and Ids.ResourceTypeKey = '+inttostr(typekey);

  if (compartmentId <> '') then
    filter := filter +' and Ids.ResourceKey in (select ResourceKey from Compartments where TypeKey = '+inttostr(FResConfig[frtPatient].key)+' and Id = '''+compartmentId+''')';

  if (compartments <> '') then
    filter := filter +' and Ids.ResourceKey in (select ResourceKey from Compartments where TypeKey = '+inttostr(FResConfig[frtPatient].key)+' and Id in ('+compartments+'))';

  link_ := '';
  first := false;
  ts := TStringList.create;
  try
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
        filter := filter + processParam([type_], params.VarName(i), ts[j], false, first, handled);
        if handled then
          link_ := link_ + '&'+params.VarName(i)+'='+EncodeMIME(ts[j]);
      end;
    end;
  finally
    ts.free;
  end;

  if params.VarExists(SEARCH_PARAM_NAME_SORT) and (params.Value[SEARCH_PARAM_NAME_SORT] <> '_id') then
  begin
    ix := FIndexes.Indexes.getByName(type_, params.Value[SEARCH_PARAM_NAME_SORT]);
    if (ix = nil) then
      Raise Exception.create(StringFormat(GetFhirMessage('MSG_SORT_UNKNOWN', lang), [params.Value[SEARCH_PARAM_NAME_SORT]]));
    sort :='(SELECT Min(Value) FROM IndexEntries WHERE Flag <> 2 and IndexEntries.ResourceKey = Ids.ResourceKey and IndexKey = '+inttostr(ix.Key)+')';
    link_ := link_+'&'+SEARCH_PARAM_NAME_SORT+':asc='+ix.Name;
  end
  else if params.VarExists(SEARCH_PARAM_NAME_SORT+':asc') and (params.Value[SEARCH_PARAM_NAME_SORT+':asc'] <> '_id') then
  begin
    ix := FIndexes.Indexes.getByName(type_, params.Value[SEARCH_PARAM_NAME_SORT+':asc']);
    if (ix = nil) then
      Raise Exception.create(StringFormat(GetFhirMessage('MSG_SORT_UNKNOWN', lang), [params.Value[SEARCH_PARAM_NAME_SORT]]));
    sort :='(SELECT Min(Value) FROM IndexEntries WHERE Flag <> 2 and IndexEntries.ResourceKey = Ids.ResourceKey and IndexKey = '+inttostr(ix.Key)+')';
    link_ := link_+'&'+SEARCH_PARAM_NAME_SORT+':asc='+ix.Name;
  end
  else if params.VarExists(SEARCH_PARAM_NAME_SORT+':desc') and (params.Value[SEARCH_PARAM_NAME_SORT+':desc'] <> '_id') then
  begin
    ix := FIndexes.Indexes.getByName(type_, params.Value[SEARCH_PARAM_NAME_SORT+':desc']);
    if (ix = nil) then
      Raise Exception.create(StringFormat(GetFhirMessage('MSG_SORT_UNKNOWN', lang), [params.Value[SEARCH_PARAM_NAME_SORT]]));
    sort :='(SELECT Max(Value) FROM IndexEntries WHERE Flag <> 2 and IndexEntries.ResourceKey = Ids.ResourceKey and IndexKey = '+inttostr(ix.Key)+')';
    link_ := link_+'&'+SEARCH_PARAM_NAME_SORT+':desc='+ix.Name;
    FReverse := true;
  end
  else
  begin
    sort := 'Id';
    link_ := link_+'&'+SEARCH_PARAM_NAME_SORT+'=_id';
  end;
end;

procedure TSearchProcessor.ProcessQuantityParam(var Result: string; name, modifier, value: string; key: Integer; types : TFHIRResourceTypeSet);
var
  qop: TQuantityOperation;
  v1: string;
  v2: string;
  sp: string;
  v1c: string;
  v2c: string;
  spC: string;
begin
  if modifier <> '' then
    raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [name + ':' + modifier]));
  qop := qopEqual;
  if findPrefix(value, 'eq')      then qop := qopEqual
  else if findPrefix(value, 'ne') then qop := qopNotEqual
  else if findPrefix(value, 'gt') then qop := qopGreater
  else if findPrefix(value, 'lt') then qop := qopLess
  else if findPrefix(value, 'ge') then qop := qopGreaterEqual
  else if findPrefix(value, 'le') then qop := qopLessEqual
  else if findPrefix(value, 'sa') then qop := qopStartsAfter
  else if findPrefix(value, 'eb') then qop := qopEndsBefore
  else if findPrefix(value, 'ap') then qop := qopApproximate;

  processQuantityValue(name, lang, value.Split(['|']), qop, v1, v2, sp, v1C, v2C, spC);
  if spc <> '' then
  begin
    case qop of
      qopEqual:       result := result + '((IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(sp) + ''') and ' + gte('Value', v1) + ' and ' + lte('Value2', v2) + ') or ' + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 1 and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(spC) + ''') and ' + gte('Value', v1C) + ' and ' + lte('Value2', v2C) + '))';
      qopNotEqual:    result := result + '((IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(sp) + ''') and ' + gte('Value', v1) + ' and ' + lte('Value2', v2) + ') or ' + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 1 and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(spC) + ''') and (' + lt('Value', v1C) + ' or ' + gt('Value2', v2C) + ')))';
      qopLess: result := result + '((IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(sp) + ''') and ' + lt('Value', v2) + ') or ' + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 1 and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(spC) + ''') and ' + lt('Value', v2C) + '))';
      qopLessEqual: result := result + '((IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(sp) + ''') and ' + lt('Value', v2) + ') or ' + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 1 and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(spC) + ''') and ' + lt('Value', v2C) + '))';
      qopGreater: result := result + '((IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(sp) + ''') and ' + gt('Value2', v1) + ') or ' + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 1 and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(spC) + ''') and ' + gt('Value2', v1C) + '))';
      qopGreaterEqual: result := result + '((IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(sp) + ''') and ' + gte('Value2', v1) + ') or ' + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 1 and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(spC) + ''') and ' + gte('Value2', v1C) + '))';
      qopStartsAfter: result := result + '((IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(sp) + ''') and ' + gte('Value2', v1) + ') or ' + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 1 and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(spC) + ''') and ' + gte('Value2', v1C) + '))';
      qopEndsBefore: result := result + '((IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(sp) + ''') and ' + gte('Value2', v1) + ') or ' + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 1 and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(spC) + ''') and ' + gte('Value2', v1C) + '))';
      qopApproximate: result := result + '((IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(sp) + ''') and ' + gt('Value', v1) + ' and ' + lt('Value2', v2) + ') or ' + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 1 and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(spC) + ''') and ' + gt('Value', v1C) + ' and ' + lt('Value2', v2C) + '))';
    end;
  end
  else
  begin
    if sp <> '' then
      sp := 'and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(sp) + ''') ';
    case qop of
      qopEqual: result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 ' + sp + 'and ' + gte('Value', v1) + ' and ' + lte('Value2', v2) + ')';
      qopNotEqual: result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 ' + sp + 'and (' + lt('Value', v1) + ' or ' + gt('Value2', v2) + '))';
      qopLess: result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 ' + sp + 'and ' + lt('Value', v2) + ')';
      qopLessEqual: result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 ' + sp + 'and ' + lt('Value', v2) + ')';
      qopGreater: result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 ' + sp + 'and ' + gt('Value2', v1) + ')';
      qopGreaterEqual: result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 ' + sp + 'and ' + gte('Value2', v1) + ')';
      qopStartsAfter: result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 ' + sp + 'and ' + gte('Value2', v1) + ')';
      qopEndsBefore: result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 ' + sp + 'and ' + gte('Value2', v1) + ')';
      qopApproximate: result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 ' + sp + 'and ' + gt('Value', v1) + ' and ' + lt('Value2', v2) + ')';
    end;
  end;
end;

procedure TSearchProcessor.ProcessReferenceParam(var Result: string; name,modifier,value: string; key: Integer;types : TFHIRResourceTypeSet);
var
  parts: TArray<String>;
  targets : TFHIRResourceTypeSet;
  i : integer;
  a : TFHIRResourceType;
begin
  // _id is a special case
  if (name = '_id') or (name = 'id') then  //  ?? what's this? or (FIndexer.GetTypeByName(types, name) = SearchParamTypeToken) then
    result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value = ''' + sqlwrapString(value) + ''')'

// not part of the spec right now.
//  else if modifier = 'text' then
//    result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value2 like ''%' + sqlwrapString(lowercase(RemoveAccents(value))) + '%'')'

  else if (modifier = '') then
  begin
    if IsId(value) then
    begin
      targets := Findexes.GetTargetsByName(types, name);
      i := 0;
      for a := low(TFHIRResourceType) to HIgh(TFHIRResourceType) do
        if a in targets then
          inc(i);
      if (i <> 1) then
        raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_INVALID_TARGETTYPE', lang), [name, i]))
      else
        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value = ''' + sqlwrapString(value) + ''')'
    end
    else
    begin
      if value.StartsWith(baseUrl) then
        value := value.Substring(baseURL.Length);
      parts := value.Split(['/']);
      if Length(parts) = 2 then
      begin
        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(parts[0]) + ''')  and Value = ''' + sqlwrapString(parts[1]) + ''')';
      end
      else if value.startsWith('http:') or value.startsWith('https:') then
        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value = ''' + sqlwrapString(value) + ''')'
      else
        raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_INVALID', lang), [name]));
    end
  end
  else if isResourceName(modifier, true) then
  begin
    if IsId(value) then
      result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(modifier) + ''') and Value = ''' + sqlwrapString(value) + ''')'
    else if value.StartsWith(baseUrl) then
    begin
      parts := value.Substring(baseURL.Length).Split(['/']);
      if Length(parts) = 2 then
      begin
        if modifier <> parts[0] then
          raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_MODIFIER_INVALID', lang), [name]));
        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(parts[0]) + ''') and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(modifier) + ''')  and Value = ''' + sqlwrapString(parts[1]) + ''')';
      end
      else
        raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_INVALID', lang), [name]));
    end
    else
      raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_MODIFIER_INVALID', lang), [modifier]));
  end
  else
    raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_MODIFIER_INVALID', lang), [modifier]));
end;

procedure TSearchProcessor.ProcessTokenParam(var Result: string; name, modifier, value: string; key: Integer; types : TFHIRResourceTypeSet);
var
  system, code : String;
begin
  system := '--';
  if (name = '_id') or (name = 'id') then
    result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value = ''' + sqlwrapString(value) + ''')'
  else if modifier = 'text' then
    result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value2 like ''' + sqlwrapString(lowercase(RemoveAccents(value))) + '%'')'
  else if modifier = 'in' then
    result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and ' + processValueSetMembership(value) + ')'
  else if modifier = 'not-in' then
    result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and not (' + processValueSetMembership(value) + '))'
  else
  begin
    if value.Contains('|') then
      StringSplit(value, '|', system, code)
    else
      code := value;
    code := code.ToLower;
    if modifier = '' then
    begin
      if (system = '--') then
        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value = ''' + sqlwrapString(value) + ''')'
      else if (system = '') then
        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and SpaceKey = null and Value = ''' + sqlwrapString(code) + ''')'
      else if (code = '') then // this variation (no code, only system) is not described in the spec
        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(system) + '''))'
      else
        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(system) + ''') and Value = ''' + sqlwrapString(code) + ''')';
    end
    else if modifier = 'not' then
    begin
      if (system = '--') then
        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and not (Value = ''' + sqlwrapString(value) + '''))'
      else if (system = '') then
        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and not (SpaceKey = null and Value = ''' + sqlwrapString(code) + '''))'
      else if (code = '') then // this variation (no code, only system) is not described in the spec
        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and not (SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(system) + ''')))'
      else
        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and not (SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(system) + ''') and Value = ''' + sqlwrapString(code) + '''))';
    end
    else if modifier = 'above' then
      raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_MODIFIER_INVALID', lang), [modifier]))
    else if modifier = 'below' then
      raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_MODIFIER_INVALID', lang), [modifier]))
    else
      raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_MODIFIER_INVALID', lang), [modifier]));
  end;
end;

procedure TSearchProcessor.ProcessUriParam(var Result: string; name, modifier, value: string; key: Integer; types : TFHIRResourceTypeSet);
var
  pfx: string;
  sfx: string;
begin
  if (modifier = '') then
  begin
    pfx := '= ''';
    sfx := '''';
  end
  else if (modifier = 'above') then
  begin
    pfx := '= Left(''';
    sfx := ''', LEN(Value))';
  end
  else if (modifier = 'below') then
  begin
    pfx := 'like ''';
    sfx := '%''';
  end
  else
    raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_MODIFIER_INVALID', lang), [modifier]));
  result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value ' + pfx + sqlwrapString(value) + sfx + ')';
end;

function RemoveSpacers(s : String):String;
var
  i, c : integer;
begin
  SetLength(result, length(s));
  c := 0;
  for i := 1 to length(s) do
  begin
    if s[i].IsLetter() or s[i].IsDigit() then
    begin
      inc(c);
      result[c] := s[i];
    end;
  end;
  SetLength(result, c);
end;

procedure TSearchProcessor.ProcessStringParam(var Result: string; name, modifier, value: string; key: Integer; var pfx: string; var sfx: string; types : TFHIRResourceTypeSet);
var
  v : String;
begin
  if name = 'phonetic' then
    v := lowercase(EncodeNYSIIS(value))
  else if name = 'telecom' then
    v := lowercase(RemoveSpacers(RemoveAccents(value)))
  else
    v := lowercase(RemoveAccents(value));

  if (modifier = '') then
  begin
    pfx := 'like ''';
    sfx := '%''';
  end
  else if (modifier = 'contains') then
  begin
    pfx := 'like ''%';
    sfx := '%''';
  end
  else if (modifier = 'exact') then
  begin
    pfx := '= ''';
    sfx := '''';
    v := value;
  end
  else
    raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_MODIFIER_INVALID', lang), [modifier]));
  result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value ' + pfx + sqlwrapString(value) + sfx + ')';
end;

procedure TSearchProcessor.ProcessDateParam(date: TDateAndTime; var Result: string; name, modifier, value: string; key: Integer; types : TFHIRResourceTypeSet);
var
  qop: TQuantityOperation;
begin
  if modifier <> '' then
    raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [name + ':' + modifier]));
  qop := qopEqual;
  if findPrefix(value, 'eq')      then qop := qopEqual
  else if findPrefix(value, 'ne') then qop := qopNotEqual
  else if findPrefix(value, 'gt') then qop := qopGreater
  else if findPrefix(value, 'lt') then qop := qopLess
  else if findPrefix(value, 'ge') then qop := qopGreaterEqual
  else if findPrefix(value, 'le') then qop := qopLessEqual
  else if findPrefix(value, 'sa') then qop := qopStartsAfter
  else if findPrefix(value, 'eb') then qop := qopEndsBefore
  else if findPrefix(value, 'ap') then qop := qopApproximate;
  CheckDateFormat(value);
  date := TDateAndTime.CreateXML(value);
  try
    case qop of
      qopEqual:        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value >= ''' + date.AsUTCDateTimeMinHL7 + ''' and Value2 <= ''' + date.AsUTCDateTimeMaxHL7 + ''')';
      qopNotEqual:     result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and (Value < ''' + date.AsUTCDateTimeMinHL7 + ''' or Value2 > ''' + date.AsUTCDateTimeMaxHL7 + '''))';
      qopLess:         result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value <= ''' + date.AsUTCDateTimeMinHL7 + ''')';
      qopLessEqual:    result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value <= ''' + date.AsUTCDateTimeMaxHL7 + ''')';
      qopGreater:      result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value2 >= ''' + date.AsUTCDateTimeMaxHL7 + ''')';
      qopGreaterEqual: result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value2 >= ''' + date.AsUTCDateTimeMinHL7 + ''')';
      qopStartsAfter:  result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value2 >= ''' + date.AsUTCDateTimeMinHL7 + ''')';
      qopEndsBefore:   result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value2 >= ''' + date.AsUTCDateTimeMinHL7 + ''')';
      qopApproximate:  result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value <= ''' + date.AsUTCDateTimeMaxHL7 + ''' and Value2 >= ''' + date.AsUTCDateTimeMinHL7 + ''')';
    end;
  finally
    date.free;
    date := nil;
  end;
end;

procedure TSearchProcessor.ProcessNumberParam(var Result: string; name, modifier, value: string; key: Integer; types: TFHIRResourceTypeSet);
var
  qop: TQuantityOperation;
  v1: string;
  v2: string;
begin
  if modifier <> '' then
    raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [name + ':' + modifier]));

  qop := qopEqual;
  if findPrefix(value, 'eq')      then qop := qopEqual
  else if findPrefix(value, 'ne') then qop := qopNotEqual
  else if findPrefix(value, 'gt') then qop := qopGreater
  else if findPrefix(value, 'lt') then qop := qopLess
  else if findPrefix(value, 'ge') then qop := qopGreaterEqual
  else if findPrefix(value, 'le') then qop := qopLessEqual
  else if findPrefix(value, 'sa') then qop := qopStartsAfter
  else if findPrefix(value, 'eb') then qop := qopEndsBefore
  else if findPrefix(value, 'ap') then qop := qopApproximate;

  processNumberValue(TSmartDecimal.ValueOf(value), qop, v1, v2);

  case qop of
    qopEqual:        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 and ' + gte('Value', v1) + ' and ' + lte('Value2', v2) + ')';
    qopNotEqual:     result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 and (' + lt('Value', v1) + ' or ' + gt('Value2', v2) + '))';
    qopLess:         result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 and ' + lt('Value', v2) + ')';
    qopLessEqual:    result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 and ' + lt('Value', v2) + ')';
    qopGreater:      result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 and ' + gt('Value2', v1) + ')';
    qopGreaterEqual: result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 and ' + gte('Value2', v1) + ')';
    qopStartsAfter:  result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 and ' + gt('Value2', v1) + ')';
    qopEndsBefore:   result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 and ' + lt('Value', v2) + ')';
    qopApproximate:  result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and flag = 0 and ' + gt('Value', v1) + ' and ' + lt('Value2', v2) + ')';
  end;
end;

function TSearchProcessor.buildParameterReference(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
var
  parts: TArray<String>;
begin
  case op of
    fscoEQ:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value = ''' + SQLWrapString(Value) + '''' + j + ')';
    fscoNE:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value <> ''' + SQLWrapString(Value) + '''' + j + ')';
    fscoCO:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value2 like ''%' + SQLWrapString(Value) + '%''' + j + ')';
    fscoSW:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value2 like ''' + SQLWrapString(Value) + '%''' + j + ')';
    fscoEW:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value2 like ''%' + SQLWrapString(Value) + '''' + j + ')';
    fscoRE:
      begin
        if value.StartsWith(baseURL) then
          value := value.Substring(baseURL.Length);
        if value.StartsWith('http:') or value.StartsWith('http:') then
          // external reference - treat as equals
          result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value = ''' + SQLWrapString(value) + '''' + j + ')'
        else
        begin
          parts := value.Split(['/']);
          if length(parts) = 1 then
            result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value = ''' + SQLWrapString(value) + '''' + j + ')'
          else
            result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(parts[0]) + ''') and ' + n + '.Value = ''' + SQLWrapString(parts[1]) + '''' + j + ')';
        end;
      end;
  else
    //
    raise Exception.Create('The operation ''' + CODES_CompareOperation[op] + ''' is not supported for parameter types of reference');
  end;
end;

function TSearchProcessor.buildParameterToken(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
var
  ref: string;
  like: Boolean;
begin
  begin
    like := false;
    if value.Contains('|') then
    begin
      StringSplit(value, '|', ref, value);
      if (ref = '') then
        ref := n + '.SpaceKey is null and '
      else if (ref = 'loinc') then
        ref := n + '.SpaceKey = (Select SpaceKey from Spaces where Space = ''http://loinc.org'') and '
      else if (ref = 'snomed') then
        ref := n + '.SpaceKey = (Select SpaceKey from Spaces where Space = ''http://snomed.info/sct'') and '
      else if (ref = 'rxnorm') then
        ref := n + '.SpaceKey = (Select SpaceKey from Spaces where Space = ''http://www.nlm.nih.gov/research/umls/rxnorm'') and '
      else if (ref = 'ucum') then
        ref := n + '.SpaceKey = (Select SpaceKey from Spaces where Space = ''http://unitsofmeasure.org'') and '
      else
        ref := n + '.SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(ref) + ''') and ';
    end
    else
      ref := '';
    like := (Name = '_language');
    case op of
      fscoEQ:
        if like then
          result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' /*' + name + '*/ and ' + ref + ' ' + n + '.Value like ''' + sqlwrapString(value) + '''%)'
        else
          result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' /*' + Name + '*/ and ' + ref + ' ' + n + '.Value = ''' + sqlwrapString(value) + ''')';
      fscoNE:
        if like then
          result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' /*' + name + '*/ and ' + ref + ' ' + n + '.Value <> ''' + sqlwrapString(value) + ''')'
        else
          result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' /*' + name + '*/ and ' + ref + ' not (' + n + '.Value like ''' + sqlwrapString(value) + '''%))';
      fscoSS:
        raise Exception.Create('Not implemented yet');
      fscoSB:
        raise Exception.Create('Not implemented yet');
      fscoIN:
        raise Exception.Create('Not implemented yet');
      fscoCO:
        result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value2 like ''%' + SQLWrapString(value) + '%''' + j + ')';
      fscoSW:
        result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value2 like ''' + SQLWrapString(value) + '%''' + j + ')';
      fscoEW:
        result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value2 like ''%' + SQLWrapString(value) + '''' + j + ')';
    else
      // fscoGT, fscoLT, fscoGE, fscoLE, fscoPO, fscoRE
      raise Exception.Create('The operation ''' + CODES_CompareOperation[op] + ''' is not supported for parameter types of token');
    end;
  end;
end;

function TSearchProcessor.buildParameterDate(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
var
  date: TDateAndTime;
begin
  begin
    date := TDateAndTime.CreateXML(value);
    try
      case op of
        fscoEQ:
          result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value = ''' + date.AsUTCDateTimeMinHL7 + ''' and ' + n + '.Value2 = ''' + date.AsUTCDateTimeMaxHL7 + '''' + j + ')';
        fscoNE:
          result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and (' + n + '.Value <> ''' + date.AsUTCDateTimeMinHL7 + ''' or ' + n + '.Value2 <> ''' + date.AsUTCDateTimeMaxHL7 + ''')' + j + ')';
        fscoGT:
          result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value2 >= ''' + date.AsUTCDateTimeMaxHL7 + '''' + j + ')';
        fscoLT:
          result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value <= ''' + date.AsUTCDateTimeMinHL7 + '''' + j + ')';
        fscoGE:
          result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value2 >= ''' + date.AsUTCDateTimeMinHL7 + '''' + j + ')';
        fscoLE:
          result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value <= ''' + date.AsUTCDateTimeMaxHL7 + '''' + j + ')';
        fscoPO:
          result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and (' + n + '.Value >= ''' + date.AsUTCDateTimeMaxHL7 + ''' or ' + n + '.Value2 <= ''' + date.AsUTCDateTimeMinHL7 + ''')' + j + ')';
      else
        // fscoSS, fscoSB, fscoIN, fscoRE, fscoCO, fscoSW, fscoEW
        raise Exception.Create('The operation ''' + CODES_CompareOperation[op] + ''' is not supported for parameter types of date');
      end;
    finally
      date.free;
    end;
  end;
end;

function TSearchProcessor.BuildParameterString(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
begin
  case op of
    fscoEQ:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value = ''' + SQLWrapString(Value) + '''' + j + ')';
    fscoNE:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value <> ''' + SQLWrapString(Value) + '''' + j + ')';
    fscoCO:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value like ''%' + SQLWrapString(Value) + '%''' + j + ')';
    fscoSW:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value like ''' + SQLWrapString(Value) + '%''' + j + ')';
    fscoEW:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value like ''%' + SQLWrapString(Value) + '''' + j + ')';
    fscoGT:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value > ''' + SQLWrapString(Value) + '''' + j + ')';
    fscoLT:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value < ''' + SQLWrapString(Value) + '''' + j + ')';
    fscoGE:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value >= ''' + SQLWrapString(Value) + '''' + j + ')';
    fscoLE:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value <= ''' + SQLWrapString(Value) + '''' + j + ')';
  else
    // fscoPO, fscoSS, fscoSB, fscoIN, fscoRE
    raise Exception.Create('The operation ''' + CODES_CompareOperation[op] + ''' is not supported for parameter types of string');
  end;
end;

function TSearchProcessor.BuildParameterNumber(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
  function CheckInteger(s : String):String;
  begin
    if StringIsInteger32(s) then
      result := s
    else
      raise Exception.Create('not a valid number');
  end;
begin
  case op of
    fscoEQ:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and Cast(' + n + '.Value as int) = ' + CheckInteger(Value) + j + ')';
    fscoNE:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and Cast(' + n + '.Value as int) <> ' + CheckInteger(Value) + j + ')';
    fscoGT:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and Cast(' + n + '.Value as int) > ' + CheckInteger(Value) + j + ')';
    fscoLT:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and Cast(' + n + '.Value as int) < ' + CheckInteger(Value) + j + ')';
    fscoGE:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and Cast(' + n + '.Value as int) >= ' + CheckInteger(Value) + j + ')';
    fscoLE:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and Cast(' + n + '.Value as int) <= ' + CheckInteger(Value) + j + ')';
  else
    // fscoCO, fscoSW, fscoEW, fscoPO, fscoSS, fscoSB, fscoIN, fscoRE
    raise Exception.Create('The operation ''' + CODES_CompareOperation[op] + ''' is not supported for parameter types of number');
  end;
end;

procedure TSearchProcessor.processNumberValue(value : TSmartDecimal; op : TQuantityOperation; var minv, maxv : String);
begin
  // work out the numerical limits
  case op of
    qopEqual :
      begin
      minv := normaliseDecimal(value.lowerBound.AsDecimal);
      maxv := normaliseDecimal(value.upperBound.AsDecimal);
      end;
    qopLess :
      maxv := normaliseDecimal(value.lowerBound.AsDecimal);
    qopLessEqual :
      maxv := normaliseDecimal(value.immediateLowerBound.AsDecimal);
    qopGreater :
      minv := normaliseDecimal(value.UpperBound.AsDecimal);
    qopGreaterEqual :
      minv := normaliseDecimal(value.immediateUpperBound.AsDecimal);
    qopApproximate :
      begin
      if value.IsNegative then
      begin
        minv := normaliseDecimal(value.Multiply(TSmartDecimal.ValueOf('1.2')).lowerBound.AsDecimal);
        maxv := normaliseDecimal(value.Multiply(TSmartDecimal.ValueOf('0.8')).upperBound.AsDecimal);
      end
      else
      begin
        minv := normaliseDecimal(value.Multiply(TSmartDecimal.ValueOf('0.8')).lowerBound.AsDecimal);
        maxv := normaliseDecimal(value.Multiply(TSmartDecimal.ValueOf('1.2')).upperBound.AsDecimal);
      end;
      end;
  end;


end;


procedure TSearchProcessor.processQuantityValue(name, lang : String; parts : TArray<string>; op : TQuantityOperation; var minv, maxv, space, mincv, maxcv, spaceC : String);
var
  value : TSmartDecimal;
  ns, s : String;
  specified, canonical : TUcumPair;
  v, u : String;
  i : integer;
begin
  minv := '';
  maxv := '';
  space := '';
  mincv := '';
  maxcv := '';
  spaceC := '';

  // [number]|[namespace]|[code]
  if Length(parts) = 0 then
    raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [name]));
  if TSmartDecimal.CheckValue(parts[0]) then
    v := parts[0]
  else
  begin
    i := 1;
    s := parts[0];
    while (i <= length(s)) and CharInSet(s[i], ['0'..'9', '.', '-']) do
      inc(i);
    if i = 1 then
      raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [name]));
    v := parts[0].Substring(0, i-1);
    u := parts[0].Substring(i-1);
  end;
  value := TSmartDecimal.valueOf(v);

  // work out the numerical limits
  case op of
    qopEqual :
      begin
      minv := normaliseDecimal(value.lowerBound.AsDecimal);
      maxv := normaliseDecimal(value.upperBound.AsDecimal);
      end;
    qopLess :
      maxv := normaliseDecimal(value.lowerBound.AsDecimal);
    qopLessEqual :
      maxv := normaliseDecimal(value.immediateLowerBound.AsDecimal);
    qopGreater :
      minv := normaliseDecimal(value.UpperBound.AsDecimal);
    qopGreaterEqual :
      minv := normaliseDecimal(value.immediateUpperBound.AsDecimal);
    qopApproximate :
      begin
      if value.IsNegative then
      begin
        minv := normaliseDecimal(value.Multiply(TSmartDecimal.valueOf('1.2')).lowerBound.AsDecimal);
        maxv := normaliseDecimal(value.Multiply(TSmartDecimal.valueOf('0.8')).upperBound.AsDecimal);
      end
      else
      begin
        minv := normaliseDecimal(value.Multiply(TSmartDecimal.valueOf('0.8')).lowerBound.AsDecimal);
        maxv := normaliseDecimal(value.Multiply(TSmartDecimal.valueOf('1.2')).upperBound.AsDecimal);
      end;
      end;
  end;

  if (length(parts) = 1) then
    space := u
  else if length(parts) = 2 then
  begin
    if u = '' then
      space := parts[1]
    else
    begin
      space := u;
      ns := parts[1];
    end;
  end
  else if (length(parts) > 3) or (u <> '') then
    raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [name]))
  else
  begin
    if (parts[2] = 'ucum') or (parts[2] = 'snomed') or (parts[2].StartsWith('http:')) then
    begin
      // 2 is namespace
      space := parts[1];
      ns := parts[2];
    end
    else if (parts[1] = 'ucum') or (parts[1] = 'snomed') or (parts[1].StartsWith('http:')) then
    begin
      // 1 is namespace (per spec)
      space := parts[2];
      ns := parts[1];
    end;
  end;

  if (ns = 'ucum') then
    ns := 'http://unitsofmeasure.org'
  else if ns  = 'snomed' then
    ns := 'http://snomed.info/sct';

  if (ns = 'http://unitsofmeasure.org') then
  begin
    specified := TUcumPair.create;
    try
      specified.Value := value;
      specified.UnitCode := space;
      canonical := repository.TerminologyServer.Ucum.getCanonicalForm(specified);
      try
        mincv := normaliseDecimal(canonical.Value.lowerBound.AsDecimal);
        maxcv := normaliseDecimal(canonical.Value.upperBound.AsDecimal);
        spaceC := 'urn:ucum-canonical#'+canonical.UnitCode;
      finally
        canonical.free;
      end;
    finally
      specified.free;
    end;
  end;

  if (ns <> '') then
    space := ns+'#'+space;
end;

Function TSearchProcessor.filterTypes(types : TFHIRResourceTypeSet) : TFHIRResourceTypeSet;
var
  a : TFHIRResourceType;
begin
  result := [];
  for a in types do
    if session.canRead(a) then
      result := result + [a];
end;

function isCommonSearchParameter(name : String): boolean;
begin
  result := StringArrayExistsSensitive(['_id', '_lastUpdated', '_tag', '_profile', '_security', '_text', '_content', '_list', '_query'], name);
end;

Function TSearchProcessor.processParam(types : TFHIRResourceTypeSet; name : String; value : String; nested : boolean; var bFirst : Boolean; var bHandled : Boolean) : String;
var
  key, i : integer;
  left, right, op, modifier, tl : String;
  f : Boolean;
  ts : TStringList;
  pfx, sfx : String;
  date : TDateAndTime;
  a : TFHIRResourceType;
  type_ : TFhirSearchParamTypeEnum;
begin
  a := frtNull;
  date := nil;
  result := '';
  op := '';
  bHandled := false;
  if (value = '') then
    exit;

  if (name = '_include') or (name = '_revinclude') then
    bHandled := true
  else if (name = '_summary') then
  begin
    bHandled := true;
  end
  else if (name = '_filter') and not nested then
  begin
    bHandled := true;
    result:= processSearchFilter(value);
  end
  else if (name = '_text') then
  begin
    result := '(IndexKey = '+inttostr(FIndexes.NarrativeIndex)+' and CONTAINS(Xhtml, '''+SQLWrapString(value)+'''))';
  end
  else if pos('.', name) > 0 then
  begin
    StringSplit(name, '.', left, right);
    if (pos(':', left) > 0) then
    begin
      StringSplit(left, ':', left, modifier);
      if not StringArrayExistsInSensitive(CODES_TFHIRResourceType, modifier) then
        raise Exception.create(StringFormat(GetFhirMessage('MSG_UNKNOWN_TYPE', lang), [modifier]));
      a := TFHIRResourceType(StringArrayIndexOfInSensitive(CODES_TFHIRResourceType, modifier));
      types := filterTypes([a]);
    end
    else
    begin
      types := filterTypes(FIndexes.GetTargetsByName(types, left));
    end;
    key := FIndexes.GetKeyByName(left);
    if key = 0 then
      raise Exception.create(StringFormat(GetFhirMessage('MSG_PARAM_CHAINED', lang), [left]));
    f := true;
    tl := '';
    for a in types do
      tl := tl+','+inttostr(FRepository.ResConfig[a].key);
    if (tl <> '') then
      tl := tl.Substring(1);
    if (tl = '') then
    begin
      result := ''; // cannot match because the chain cannot be executed
      bHandled := false;
    end
    else
    begin
      result := result + '(IndexKey = '+inttostr(Key)+' /*'+left+'*/ and target in (select ResourceKey from IndexEntries where Flag <> 2 and (ResourceKey in '+
        '(select ResourceKey from Ids where ResourceTypeKey in ('+tl+')) and ('+processParam(types, right, lowercase(value), true, f, bHandled)+'))))';
      bHandled := true;
    end;
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
    else
    begin
      key := FIndexes.GetKeyByName(name);
      if (types = [frtNull]) and not isCommonSearchParameter(name) then
        key := 0;

      if key > 0 then
      begin
        type_ := FIndexes.GetTypeByName(types, name);
        if modifier = 'missing' then
        begin
          bHandled := true;
          if StrToBoolDef(value, false) then
            result := result + '((Select count(*) from IndexEntries where Flag <> 2 and IndexKey = '+inttostr(Key)+' /*'+name+'*/ and IndexEntries.ResourceKey = Ids.ResourceKey) = 0)'
          else
            result := result + '((Select count(*) from IndexEntries where Flag <> 2 and IndexKey = '+inttostr(Key)+' /*'+name+'*/ and IndexEntries.ResourceKey = Ids.ResourceKey) > 0)';
        end
        else
        begin
          ts := TStringlist.create;
          try
            SplitByCommas(value, ts);
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
                SearchParamTypeDate: ProcessDateParam(date, Result, name, modifier, value, key, types);
                SearchParamTypeString: ProcessStringParam(Result, name, modifier, value, key, pfx, sfx, types);
                SearchParamTypeUri: ProcessUriParam(Result, name, modifier, value, key, types);
                SearchParamTypeToken: ProcessTokenParam(Result, name, modifier, value, key, types);
                SearchParamTypeReference : ProcessReferenceParam(Result, name, modifier, value, key, types);
                SearchParamTypeQuantity : ProcessQuantityParam(Result, name, modifier, value, key, types);
                SearchParamTypeNumber : ProcessNumberParam(Result, name, modifier, value, key, types);
              else if type_ <> SearchParamTypeNull then
                raise exception.create('not done yet: type = '+CODES_TFhirSearchParamTypeEnum[type_]);
              end;
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
      result := 'Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and '+result+')';
    if not bfirst then
      result := ' and '+result;
  end;
  bfirst := bfirst and (result = '');
end;



function TSearchProcessor.ProcessSearchFilter(value: String): String;
var
  filter : TFSFilter;
  issuer : TFSCharIssuer;
begin
  issuer := TFSCharIssuer.Create;
  try
    filter := TFSFilterParser.parse(value);
    try
      result := '('+BuildFilter(filter, ' ', issuer, [FType])+')';
    finally
      filter.Free;
    end;
  finally
    issuer.Free;
  end;
end;

function TSearchProcessor.processValueSetMembership(vs: String): String;
var
  vso : TFHIRValueSet;
begin
  // firstly, the vs can be a logical reference or a literal reference
  if (vs.StartsWith('valueset/')) then
  begin
    vso := FRepository.TerminologyServer.getValueSetByUrl(vs);
    try
      if vso = nil then
        vs := 'not-found'
      else
        vs := vso.url;
    finally
      vso.Free;
    end;
  end;
  result := 'Concept in (select ConceptKey from ValueSetMembers where ValueSetKey in (select ValueSetKey from ValueSets where URL = '''+sqlWrapString(vs)+'''))';
end;

procedure TSearchProcessor.replaceNames(paramPath: TFSFilterParameterPath; components: TDictionary<String, String>);
begin
  if components.ContainsKey(paramPath.Name) then
    paramPath.Name := components[paramPath.Name]
  else
    raise Exception.Create('Unknown Search Parameter Name "'+paramPath.Name+'"');
end;

procedure TSearchProcessor.replaceNames(filter: TFSFilter; components: TDictionary<String, String>);
begin
  if (filter = nil) then
    exit
  else if filter.FilterItemType = fsitLogical then
  begin
    replaceNames(TFSFilterLogical(filter).Filter1, components);
    replaceNames(TFSFilterLogical(filter).Filter2, components);
  end
  else
    replaceNames(TFSFilterParameter(filter).ParamPath, components);
end;

procedure TSearchProcessor.SetIndexes(const Value: TFhirIndexInformation);
begin
  FIndexes.Free;
  FIndexes := Value;
end;

procedure TSearchProcessor.SetRepository(const Value: TFHIRDataStore);
begin
  FRepository.Free;
  FRepository := Value;
end;



procedure TSearchProcessor.SetSession(const Value: TFhirSession);
begin
  FSession.Free;
  FSession := Value;
end;

procedure TSearchProcessor.SplitByCommas(value : String; list : TStringList);
var
  s : String;
begin
  for s in value.Split([',']) do
    list.add(s);
end;

function TSearchProcessor.findPrefix(var value : String; subst : String) : boolean;
begin
  result := value.StartsWith(subst);
  if result then
    value := value.Substring(subst.Length);
end;

function TSearchProcessor.BuildFilter(filter: TFSFilter; parent: char; issuer: TFSCharIssuer; types : TFHIRResourceTypeSet): String;
begin
  case filter.FilterItemType of
    fsitParameter : result := BuildFilterParameter(filter as TFSFilterParameter, TFSFilterParameter(filter).ParamPath, parent, issuer, types);
    fsitLogical :   result := BuildFilterLogical(filter as TFSFilterLogical, parent, issuer, types);
  else
    raise Exception.Create('Unknown type');
  end;
end;

function TSearchProcessor.BuildFilterLogical(filter: TFSFilterLogical; parent: char; issuer: TFSCharIssuer; types : TFHIRResourceTypeSet): String;
begin
  if filter.Operation = fsloNot then
    result := '(Not '+BuildFilter(filter.Filter2, parent, issuer, types)+')'
  else if filter.Operation = fsloOr then
    result := '('+BuildFilter(filter.Filter1, parent, issuer, types)+' or '+BuildFilter(filter.Filter2, parent, issuer, types)+')'
  else // filter.Operation = fsloAnd
    result := '('+BuildFilter(filter.Filter1, parent, issuer, types)+' and '+BuildFilter(filter.Filter2, parent, issuer, types)+')';

end;


function TSearchProcessor.BuildFilterParameter(filter: TFSFilterParameter; path : TFSFilterParameterPath; parent: char; issuer: TFSCharIssuer; types : TFHIRResourceTypeSet): String;
var
  index : integer;
  n : char;
  j : string;
  stype : TFhirSearchParamTypeEnum;
  comp : TFhirComposite;
begin
  n := issuer.next;
  if parent = ' ' then
    j := ''
  else
    j := ' and '+n+'.parent = '+parent+'.EntryKey';
  index := FIndexes.GetKeyByName(path.Name);

  if path.Next <> nil then
  begin
    comp := FIndexes.getComposite(types, path.Name, types);
    if (comp <> nil) then
    begin
      if (filter = nil) then
        raise Exception.Create('Parameter ("'+path.Name+'") is missing a filter - it is required');
      // first, scan the filter - there must be one - and rename them. They must match
      replaceNames(path.Filter, comp.Components);
      replaceNames(path.next, comp.components);
      result := 'ResourceKey in (select ResourceKey from IndexEntries as '+n+' where Flag <> 2 and (indexKey = '''+inttostr(index)+''' and '+BuildFilter(path.Filter, n, issuer, types) +')'+j;
      // ok, that's the filter. Now we process the content
      result := result + ' and '+BuildFilterParameter(filter, path.Next, n, issuer, types);
      result := result +')';
    end
    else
    begin
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index);
      if path.filter <> nil then
        raise Exception.Create('Not handled yet');
     //   + ' and ' + n + '.SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(parts[0]) + ''')'
    // result := result +'  and ' + n + '.target in ()' + j + ')';
      result := result + ' and '+n+'.target in (select ResourceKey from IndexEntries where Flag <> 2 and ('+BuildFilterParameter(filter, path.Next, parent, issuer, FIndexes.GetTargetsByName(types, path.name))+')))'+j;
    end;
  end
  else
  begin
    // do we recognise the attribute path?
    assert(path.Filter = nil); // not allowed in the grammar
    stype := FIndexes.GetTypeByName(types, path.Name);

    if filter.Operation = fscoPR then
    begin
      if (filter.value = 'true') then
        result := 'ResourceKey in (select ResourceKey from IndexEntries as '+n+' where Flag <> 2 and '+n+'.IndexKey = '+inttostr(index)+j+')'
      else
        result := 'not (ResourceKey in (select ResourceKey from IndexEntries as '+n+' where Flag <> 2 and '+n+'.IndexKey = '+inttostr(index)+j+'))';
    end
    else case stype of
      SearchParamTypeNull:
        raise Exception.Create('The search type could not be determined');
      SearchParamTypeNumber:    result := BuildParameterNumber(index, n, j, path.Name, filter.Operation, filter.Value);
      SearchParamTypeString :   result := BuildParameterString(index, n, j, path.Name, filter.Operation, filter.Value);
      SearchParamTypeDate:      result := buildParameterDate(index, n, j, path.Name, filter.Operation, filter.Value);
      SearchParamTypeToken:     result := buildParameterToken(index, n, j, path.Name, filter.Operation, filter.Value);
      SearchParamTypeReference: result := buildParameterReference(index, n, j, path.Name, filter.Operation, filter.Value);
      SearchParamTypeComposite: raise Exception.Create('Composite indexes cannot the direct target of an operation criteria (except for operation "pr")');
      SearchParamTypeQuantity:  raise Exception.Create('Quantity searching is not handled yet');
    end;
  end;
end;

procedure TSearchProcessor.checkDateFormat(s: string);
var
  ok : boolean;
  tz : String;
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
    tz := '';
    if s.EndsWith('Z') then
      s := s.Substring(0, s.length-1)
    else if (s.Substring(11).Contains('-')) then
    begin
      tz := s.Substring(s.LastIndexOf('-'));
      s := s.Substring(0, s.LastIndexOf('-'))
    end
    else if (s.Substring(11).Contains('+')) then
    begin
      tz := s.Substring(s.LastIndexOf('+'));
      s := s.Substring(0, s.LastIndexOf('+'))
    end;
    if (tz <> '') then
     if (tz.Length <> 6) or (tz[4] <> ':') or not StringIsCardinal16(copy(tz, 2, 2)) or not StringIsCardinal16(copy(tz, 5, 2)) then
       raise exception.create(StringFormat(GetFhirMessage('MSG_DATE_FORMAT', lang), [s]));

    if (length(s) = 16) and (s[14] = ':') and StringIsCardinal16(copy(s, 12, 2)) and StringIsCardinal16(copy(s, 15, 2)) then
      ok := true
    else if (length(s) = 19) and (s[14] = ':') and (s[17] = ':') and
          StringIsCardinal16(copy(s, 12, 2)) and StringIsCardinal16(copy(s, 15, 2)) and StringIsCardinal16(copy(s, 18, 2)) then
      ok := true;
  end;
  if not ok then
    raise exception.create(StringFormat(GetFhirMessage('MSG_DATE_FORMAT', lang), [s]));
end;

constructor TSearchProcessor.create(ResConfig: TConfigArray);
begin
  Inherited Create;
  FResConfig := ResConfig;
end;

destructor TSearchProcessor.Destroy;
begin
  FRepository.Free;
  FSession.Free;
  FIndexes.Free;
  inherited;
end;

end.

