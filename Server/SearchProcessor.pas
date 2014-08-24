unit SearchProcessor;

interface

uses
  SysUtils, Classes,
  ParseMap,
  StringSupport, EncodeSupport,
  AdvObjects, DateAndTime,
  FHIRResources, FHIRLang, FHIRConstants, FHIRComponents, FHIRTypes,
  FHIRIndexManagers, FHIRDataStore, FHIRUtilities;

const
  SEARCH_PARAM_NAME_ID = 'search-id';
  HISTORY_PARAM_NAME_ID = 'history-id';
  SEARCH_PARAM_NAME_OFFSET = 'search-offset';
  SEARCH_PARAM_NAME_TEXT = '_text';
  SEARCH_PARAM_NAME_COUNT = '_count';
  SEARCH_PARAM_NAME_SORT = 'search-sort';
  SEARCH_PARAM_NAME_SUMMARY = '_summary';
  SEARCH_PAGE_DEFAULT = 50;
  SEARCH_PAGE_LIMIT = 1000;
  SUMMARY_SEARCH_PAGE_LIMIT = 10000;
  SUMMARY_TEXT_SEARCH_PAGE_LIMIT = 10000;


type
  TDateOperation = (dopEqual, dopLess, dopLessEqual, dopGreater, dopGreaterEqual);
  TFHIRSearchSummary = (ssFull, ssSummary, ssText);

  TSearchProcessor = class (TAdvObject)
  private
    FLink: String;
    FSort: String;
    FFilter: String;
    FTypeKey: integer;
    FCompartmentId: String;
    FCompartments: String;
    FParams: TParseMap;
    FType: TFHIRResourceType;
    FBaseURL: String;
    FWantSummary: TFHIRSearchSummary;
    FIndexer: TFhirIndexManager;
    FLang: String;
    FRepository: TFHIRDataStore;
    Function ProcessParam(types : TFHIRResourceTypeSet; name : String; value : String; nested : boolean; var bFirst : Boolean; var bHandled : Boolean) : String;
    procedure SetIndexer(const Value: TFhirIndexManager);
    procedure SetRepository(const Value: TFHIRDataStore);
    function SchemeForName(name: String): string;
    procedure SplitByCommas(value: String; list: TStringList);
    function findPrefix(var value: String; subst: String): boolean;
    procedure checkDateFormat(s : string);
  public
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
    property indexer : TFhirIndexManager read FIndexer write SetIndexer;
    property repository : TFHIRDataStore read FRepository write SetRepository;


    // outbound
    property link : String read FLink write FLink;
    property sort : String read FSort write FSort;
    property filter : String read FFilter write FFilter;
    property wantSummary : TFHIRSearchSummary read FWantSummary write FWantSummary;
  end;


implementation



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
    filter := filter +' and Ids.ResourceKey in (select ResourceKey from Compartments where CompartmentType = 1 and Id = '''+compartmentId+''')';

  if (compartments <> '') then
    filter := filter +' and Ids.ResourceKey in (select ResourceKey from Compartments where CompartmentType = 1 and Id in ('+compartments+'))';

  link := '';
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
          link := link + '&'+params.VarName(i)+'='+EncodeMIME(ts[j]);
      end;
    end;
  finally
    ts.free;
  end;

  if params.VarExists(SEARCH_PARAM_NAME_SORT) and (params.Value[SEARCH_PARAM_NAME_SORT] <> '_id') then
  begin
    ix := FIndexer.Indexes.getByName(type_, params.Value[SEARCH_PARAM_NAME_SORT]);
    if (ix = nil) then
      Raise Exception.create(StringFormat(GetFhirMessage('MSG_SORT_UNKNOWN', lang), [params.Value[SEARCH_PARAM_NAME_SORT]]));
    sort :='(SELECT Min(Value) FROM IndexEntries WHERE IndexEntries.ResourceKey = Ids.ResourceKey and IndexKey = '+inttostr(ix.Key)+')';
    link := link+'&'+SEARCH_PARAM_NAME_SORT+'='+ix.Name;
  end
  else
  begin
    sort := 'Id';
    link := link+'&'+SEARCH_PARAM_NAME_SORT+'=_id';
  end;
end;



Function TSearchProcessor.processParam(types : TFHIRResourceTypeSet; name : String; value : String; nested : boolean; var bFirst : Boolean; var bHandled : Boolean) : String;
var
  key, i : integer;
  left, right, op, modifier : String;
  f, dummy : Boolean;
  ts : TStringList;
  pfx, sfx : String;
  date : TDateAndTime;
  a : TFHIRResourceType;
  type_ : TFhirSearchParamType;
  parts : TArray<string>;
  dop : TDateOperation;
begin
  date := nil;
  result := '';
  op := '';
  bHandled := false;
  if (value = '') then
    exit;

  if (name = '_include') or (name = '_reverseInclude') then
    bHandled := true
  else if (name = '_summary') and (value = 'true') then
  begin
    bHandled := true;
    wantSummary := ssSummary;
  end
  else if (name = '_summary') and (value = 'text') then
  begin
    bHandled := true;
    wantSummary := ssText;
  end
  else if (name = '_text') then
  begin
    result := '(IndexKey = '+inttostr(FIndexer.NarrativeIndex)+' and CONTAINS(Xhtml, '''+SQLWrapString(value)+'''))';
  end
  else if pos('.', name) > 0 then
  begin
    StringSplit(name, '.', left, right);
    if (pos(':', left) > 0) then
    begin
      StringSplit(left, ':', left, modifier);
      if not StringArrayExistsInSensitive(CODES_TFHIRResourceType, modifier) then
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
    if modifier <> '' then
      result := result + '(IndexKey = '+inttostr(Key)+' /*'+left+'*/ and target in (select ResourceKey from IndexEntries where (ResourceKey in (select ResourceKey from Ids where ResourceTypeKey = '+inttostr(FRepository.ResConfig[a].key)+')) and ('+processParam(FIndexer.GetTargetsByName(types, left), right, lowercase(value), true, f, bHandled)+')))'
    else
      result := result + '(IndexKey = '+inttostr(Key)+' /*'+left+'*/ and target in (select ResourceKey from IndexEntries where '+processParam(FIndexer.GetTargetsByName(types, left), right, lowercase(value), true, f, bHandled)+'))';
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
    else if (name = '_tag') or (name = 'profile') or (name = '_security') then
    begin
      bHandled := true;
      if modifier = 'partial' then
        result :=  '(MostRecent in (Select ResourceVersionKey from VersionTags where TagKey in (Select TagKey from Tags where SchemeUri = '''+SchemeForName(name)+''' and TermUri like '''+SQLWrapString(value)+'%'')))'
      else if modifier = 'text' then
        result :=  '(MostRecent in (Select ResourceVersionKey from VersionTags where Label like ''%'+SQLWrapString(value)+'%''))'
      else if modifier = '' then
        result :=  '(MostRecent in (Select ResourceVersionKey from VersionTags where TagKey = '''+intToStr(FRepository.KeyForTag(SchemeForName(name), value))+'''))'
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
            result := result + '((Select count(*) from IndexEntries where IndexKey = '+inttostr(Key)+' /*'+name+'*/ and IndexEntries.ResourceKey = Ids.ResourceKey) = 0)'
          else
            result := result + '((Select count(*) from IndexEntries where IndexKey = '+inttostr(Key)+' /*'+name+'*/ and IndexEntries.ResourceKey = Ids.ResourceKey) > 0)';
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
                SearchParamTypeDate:
                  begin
                    if modifier <> '' then
                      raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [name+':'+modifier]));
                    dop := dopEqual;
                    if findPrefix(value, '<=') then
                      dop := dopLessEqual
                    else if findPrefix(value, '<') then
                      dop := dopLess
                    else if findPrefix(value, '>=') then
                      dop := dopGreaterEqual
                    else if findPrefix(value, '>') then
                      dop := dopGreater;
                    CheckDateFormat(value);
                    date := TDateAndTime.CreateXML(value);
                    try
                      case dop of
                        dopEqual:        result := result + '(IndexKey = '+inttostr(Key)+' /*'+name+'*/ and Value >= '''+date.AsUTCDateTimeMinHL7+''' and Value2 <= '''+date.AsUTCDateTimeMaxHL7+''')';
                        dopLess:         result := result + '(IndexKey = '+inttostr(Key)+' /*'+name+'*/ and Value <= '''+date.AsUTCDateTimeMinHL7+''')';
                        dopLessEqual:    result := result + '(IndexKey = '+inttostr(Key)+' /*'+name+'*/ and Value <= '''+date.AsUTCDateTimeMaxHL7+''')';
                        dopGreater:      result := result + '(IndexKey = '+inttostr(Key)+' /*'+name+'*/ and Value2 >= '''+date.AsUTCDateTimeMaxHL7+''')';
                        dopGreaterEqual: result := result + '(IndexKey = '+inttostr(Key)+' /*'+name+'*/ and Value2 >= '''+date.AsUTCDateTimeMinHL7+''')';
                      end;
                    finally
                      date.free;
                      date := nil;
                    end;
                  end;
                SearchParamTypeString:
                  begin
                    value := lowercase(value);
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
                    result := result + '(IndexKey = '+inttostr(Key)+' /*'+name+'*/ and Value '+pfx+sqlwrapString(lowercase(RemoveAccents(value)))+sfx+')';
                  end;
                SearchParamTypeToken:
                  begin
                  value := lowercase(value);
                  // _id is a special case
                  if (name = '_id') then
                    result := result + '(IndexKey = '+inttostr(Key)+' /*'+name+'*/ and Value = '''+sqlwrapString(value)+''')'
                  else if (name = '_language') then
                    result := result + '(IndexKey = '+inttostr(Key)+' /*'+name+'*/ and Value like '''+sqlwrapString(value)+'%'')'
                  else if modifier = 'text' then
                    result := result + '(IndexKey = '+inttostr(Key)+' /*'+name+'*/ and Value2 like ''%'+sqlwrapString(lowercase(RemoveAccents(value)))+'%'')'
                  else if value.Contains('|') then
                  begin
                    StringSplit(value, '|', left, right);
                    result := result + '(IndexKey = '+inttostr(Key)+' /*'+name+'*/ and SpaceKey = (Select SpaceKey from Spaces where Space = '''+sqlwrapstring(left)+''') and Value = '''+sqlwrapString(right)+''')';
                  end
                  else
                    result :=  result + '(IndexKey = '+inttostr(Key)+' /*'+name+'*/ and Value = '''+sqlwrapString(value)+''')'
//
//                  !
//                  // _id is a special case
//                  if (name = '_id') or (FIndexer.GetTypeByName(types, name) = SearchParamTypeToken) then
//                    result := result + '(IndexKey = '+inttostr(Key)+' and Value = '''+sqlwrapString(value)+''')'
//                  else
//                  else if (modifier = 'code') or ((modifier = '') and (pos('/', value) > 0)) then
//                  begin
//                    StringSplitRight(value, '/', left, right);
//                    result := result + '(IndexKey = '+inttostr(Key)+' and SpaceKey = (Select SpaceKey from Spaces where Space = '''+sqlwrapstring(left)+''') and Value = '''+sqlwrapString(right)+''')';
//                  end
//                  else if modifier = 'anyns' then
//                    result :=  result + '(IndexKey = '+inttostr(Key)+' and Value = '''+sqlwrapString(value)+''')'
//                  else
//                    raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [modifier]))
                  end;
                SearchParamTypeReference :
                  begin
                  // _id is a special case
                  if (name = '_id') or (FIndexer.GetTypeByName(types, name) = SearchParamTypeToken) then
                    result := result + '(IndexKey = '+inttostr(Key)+' /*'+name+'*/ and Value = '''+sqlwrapString(value)+''')'
                  else if modifier = 'text' then
                    result := result + '(IndexKey = '+inttostr(Key)+' /*'+name+'*/ and Value2 like ''%'+sqlwrapString(lowercase(RemoveAccents(value)))+'%'')'
                  else if (modifier = 'anyns') or (modifier = '') then
                  begin
                    if IsId(value) then
                      result :=  result + '(IndexKey = '+inttostr(Key)+' /*'+name+'*/ and Value = '''+sqlwrapString(value)+''')'
                    else if value.StartsWith(baseUrl) then
                    begin
                      parts := value.Substring(baseURL.Length).Split(['/']);
                      if Length(parts) = 2 then
                      begin
                        result :=  result + '(IndexKey = '+inttostr(Key)+' /*'+name+'*/ and SpaceKey = (Select SpaceKey from Spaces where Space = '''+sqlwrapstring(parts[0])+''')  and Value = '''+sqlwrapString(parts[1])+''')'
                      end
                      else
                        raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [name]))
                    end
                    else
                      raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [modifier]))
                  end
                  else
                  begin
                    if IsId(value) then
                      result :=  result + '(IndexKey = '+inttostr(Key)+' /*'+name+'*/ and SpaceKey = (Select SpaceKey from Spaces where Space = '''+sqlwrapstring(modifier)+''') and Value = '''+sqlwrapString(value)+''')'
                    else if value.StartsWith(baseUrl) then
                    begin
                      parts := value.Substring(baseURL.Length).Split(['/']);
                      if Length(parts) = 2 then
                      begin
                        result :=  result + '(IndexKey = '+inttostr(Key)+' /*'+name+'*/ and SpaceKey = (Select SpaceKey from Spaces where Space = '''+sqlwrapstring(parts[0])+''') and SpaceKey = (Select SpaceKey from Spaces where Space = '''+sqlwrapstring(modifier)+''')  and Value = '''+sqlwrapString(parts[1])+''')'
                      end
                      else
                        raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [name]))
                    end
                    else
                      raise exception.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [modifier]))
                  end
                  end;
                SearchParamTypeQuantity :
                  raise exception.create('not done yet: type = '+CODES_TFhirSearchParamType[type_]);
              else if type_ <> SearchParamTypeNull then
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
                result := result + '(IndexKey = '+inttostr(Key)+' /*'+name+'*/ and Value '+op+' '''+sqlwrapString(value)+''')';
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



procedure TSearchProcessor.SetIndexer(const Value: TFhirIndexManager);
begin
  FIndexer := Value;
end;

procedure TSearchProcessor.SetRepository(const Value: TFHIRDataStore);
begin
  FRepository := Value;
end;

function TSearchProcessor.SchemeForName(name : String) : string;
begin
  if name = '_tag' then
    result := 'http://hl7.org/fhir/tag'
  else if result = '_profile' then
    result :='http://hl7.org/fhir/tag/profile'
  else
    result := 'http://hl7.org/fhir/tag/security';
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

procedure TSearchProcessor.checkDateFormat(s: string);
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
    if (length(s) = 16) and (s[14] = ':') and StringIsCardinal16(copy(s, 12, 2)) and StringIsCardinal16(copy(s, 15, 2)) then
      ok := true
    else if (length(s) = 19) and (s[14] = ':') and (s[17] = ':') and
          StringIsCardinal16(copy(s, 12, 2)) and StringIsCardinal16(copy(s, 15, 2)) and StringIsCardinal16(copy(s, 18, 2)) then
      ok := true;
  end;
  if not ok then
    raise exception.create(StringFormat(GetFhirMessage('MSG_DATE_FORMAT', lang), [s]));
end;




end.

