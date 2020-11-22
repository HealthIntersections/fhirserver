unit search;

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
  SysUtils, Classes, Generics.Collections, Character,
  fsl_http,
  fsl_base, fsl_utilities, fsl_fpc,
  fhir_objects,  fhir_utilities, fhir_common, fhir_factory, fhir_client,
  fdb_manager, fdb_dialects,
  fhir_indexing,
  indexing, search_syntax, session, utilities, server_context, server_constants,
  ftx_ucum_services;

type
  TQuantityOperation = (qopEqual, qopNotEqual, qopLess, qopLessEqual, qopGreater, qopGreaterEqual, qopStartsAfter, qopEndsBefore, qopApproximate);

  TSearchProcessor = class (TFHIRServerWorker)
  private
    FLink: String;
    FSort: String;
    FFilter: String;
    FTypeKey: integer;
    FCompartment: TFHIRCompartmentId;
    FSessionCompartments : TFslList<TFHIRCompartmentId>;
    FParams: THTTPParameters;
    FType: String;
    FBaseURL: String;
    FIndexes: TFhirIndexInformation;
    FLang: THTTPLanguages;
    FSession : TFhirSession;
//    FLeftOpen: boolean;
    FcountAllowed: boolean;
    FReverse: boolean;
    FStrict : boolean;
    FConnection: TFDBConnection;
    FWarnings : TFslList<TFhirOperationOutcomeIssueW>;
    FResConfig : TFslMap<TFHIRResourceConfig>;

    function factory : TFHIRFactory;
    function order(s : String) : String;
    function fetchGroup(id : String) : TFHIRResourceV;
    function processValueSetMembership(code, vs : String) : String;
    function BuildFilter(filter : TFSFilter; parent : char; issuer : TFSCharIssuer; types : TArray<String>) : String;
    function BuildFilterParameter(filter : TFSFilterParameter; path : TFSFilterParameterPath; parent : char; issuer : TFSCharIssuer; types : TArray<String>) : String;
    function BuildFilterLogical  (filter : TFSFilterLogical;   parent : char; issuer : TFSCharIssuer; types : TArray<String>) : String;
    Function ProcessSearchFilter(value : String) : String;
    Function ProcessParam(types : TArray<String>; name : String; value : String; nested : boolean; var bFirst : Boolean; var bHandled : Boolean) : String;
    procedure SetIndexes(const Value: TFhirIndexInformation);
    procedure SplitByCommas(value: String; list: TStringList);
    function findPrefix(var value: String; subst: String): boolean;
    procedure checkDateFormat(s : string);
    function BuildParameterNumber(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
    function BuildParameterString(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
    function buildParameterDate(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
    function buildParameterToken(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
    function buildParameterReference(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
    procedure replaceNames(paramPath : TFSFilterParameterPath; components : TFslStringDictionary); overload;
    procedure replaceNames(filter : TFSFilter; components : TFslStringDictionary); overload;
    procedure processQuantityValue(name : String; const lang : THTTPLanguages; parts: TArray<string>; op: TQuantityOperation; var minv, maxv, space, mincv, maxcv, spaceC: String);
    procedure processNumberValue(value : TFslDecimal; op : TQuantityOperation; var minv, maxv : String);
    procedure SetSession(const Value: TFhirSession);
    function filterTypes(types: TArray<String>): TArray<String>;
    procedure ProcessDateParam(date: TFslDateTime; var Result: string; name, modifier, value: string; key: Integer; types : TArray<String>);
    procedure ProcessStringParam(var Result: string; name, modifier, value: string; key: Integer; var pfx: string; var sfx: string; types : TArray<String>);
    procedure ProcessUriParam(var Result: string; name, modifier, value: string; key: Integer; types : TArray<String>);
    procedure ProcessTokenParam(var Result: string; name, modifier, value: string; key: Integer; types : TArray<String>);
    procedure ProcessReferenceParam(var Result: string; name, modifier, value: string; key: Integer; types : TArray<String>);
    procedure ProcessQuantityParam(var Result: string; name, modifier, value: string; key: Integer; types : TArray<String>);
    procedure ProcessNumberParam(var Result: string; name, modifier, value: string; key: Integer; types : TArray<String>);
    procedure SetConnection(const Value: TFDBConnection);
    procedure SetCompartment(const Value: TFHIRCompartmentId);
    procedure SetSessionCompartments(const Value: TFslList<TFHIRCompartmentId>);
    procedure SetResConfig(const Value: TFslMap<TFHIRResourceConfig>);
  public
    constructor Create(serverContext : TFslObject);
    destructor Destroy; override;
    procedure Build;
//procedure TFhirOperation.ProcessDefaultSearch(typekey : integer; aType : TFHIRResourceType; params : THTTPParameters; baseURL, compartments, compartmentId : String; id, key : string; var link, sql : String; var total : Integer; var wantSummary : boolean);

    // inbound
    property resConfig : TFslMap<TFHIRResourceConfig> read FResConfig write SetResConfig;
    property typekey : integer read FTypeKey write FTypeKey;
    property type_ : String read FType write FType;
    property compartment : TFHIRCompartmentId read FCompartment write SetCompartment;
    property sessionCompartments : TFslList<TFHIRCompartmentId> read FSessionCompartments write SetSessionCompartments;
    property baseURL : String read FBaseURL write FBaseURL;
    property lang : THTTPLanguages read FLang write FLang;
    property params : THTTPParameters read FParams write FParams;
    property indexes : TFhirIndexInformation read FIndexes write SetIndexes;
    property session : TFhirSession read FSession write SetSession;
    property countAllowed : boolean read FcountAllowed write FcountAllowed;
    property Connection : TFDBConnection read FConnection write SetConnection;

    // outbound
    property link_ : String read FLink write FLink;
    property sort : String read FSort write FSort;
    property filter : String read FFilter write FFilter;
    property reverse : boolean read FReverse write FReverse;
    property strict : boolean read FStrict write FStrict;
    property Warnings : TFslList<TFhirOperationOutcomeIssueW> read FWarnings;
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


function knownParam(name : String) : boolean;
begin
  result := StringArrayExistsSensitive(['_id', '_lastUpdated', '_tag', '_profile', '_security', '_text', '_content', '_list', '_has', '_type', '_query', '_sort', '_count', '_include', '_revinclude', '_summary', '_elements', '_contained', '_containedType'], name);
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
  begin
    filter := 'Ids.MasterResourceKey is null';
    type_ := '*';
  end
  else
    filter := 'Ids.MasterResourceKey is null and Ids.ResourceTypeKey = '+inttostr(typekey);

  filter := filter + buildCompartmentsSQL(resConfig, compartment, sessionCompartments);

  link_ := '';
  first := false;
  ts := TStringList.create;
  try
    i := 0;
    while i < params.Count do
    begin
      ts.Clear;
      ts.assign(params.List(i));
      for j := ts.count - 1 downto 0 do
        if ts[j] = '' then
          ts.delete(j);
      for j := 0 to ts.count - 1 do
      begin
        handled := false;
        filter := filter + processParam(TArray<String>.create(type_), params.Name[i], ts[j], false, first, handled);
        if handled then
          link_ := link_ + '&'+params.Name[i]+'='+EncodeMIME(ts[j])
        else if strict and not (knownParam(params.Name[i])) then
          raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [params.Name[i]]));
      end;
      inc(i);
    end;
  finally
    ts.free;
  end;

  if params.has(SEARCH_PARAM_NAME_SORT) and (params.Value[SEARCH_PARAM_NAME_SORT] <> '_id') then
  begin
    ix := FIndexes.Indexes.getByName(type_, params.Value[SEARCH_PARAM_NAME_SORT]);
    if (ix = nil) then
      raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_SORT_UNKNOWN', lang), [params.Value[SEARCH_PARAM_NAME_SORT]]));
    sort :='(SELECT Min(Value) FROM IndexEntries WHERE Flag <> 2 and IndexEntries.ResourceKey = Ids.ResourceKey and IndexKey = '+inttostr(ix.Key)+')';
    link_ := link_+'&'+SEARCH_PARAM_NAME_SORT+':asc='+ix.Name;
  end
  else if params.has(SEARCH_PARAM_NAME_SORT+':asc') and (params.Value[SEARCH_PARAM_NAME_SORT+':asc'] <> '_id') then
  begin
    ix := FIndexes.Indexes.getByName(type_, params.Value[SEARCH_PARAM_NAME_SORT+':asc']);
    if (ix = nil) then
      raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_SORT_UNKNOWN', lang), [params.Value[SEARCH_PARAM_NAME_SORT]]));
    sort :='(SELECT Min(Value) FROM IndexEntries WHERE Flag <> 2 and IndexEntries.ResourceKey = Ids.ResourceKey and IndexKey = '+inttostr(ix.Key)+')';
    link_ := link_+'&'+SEARCH_PARAM_NAME_SORT+':asc='+ix.Name;
  end
  else if params.has(SEARCH_PARAM_NAME_SORT+':desc') and (params.Value[SEARCH_PARAM_NAME_SORT+':desc'] <> '_id') then
  begin
    ix := FIndexes.Indexes.getByName(type_, params.Value[SEARCH_PARAM_NAME_SORT+':desc']);
    if (ix = nil) then
      raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_SORT_UNKNOWN', lang), [params.Value[SEARCH_PARAM_NAME_SORT]]));
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

procedure TSearchProcessor.ProcessQuantityParam(var Result: string; name, modifier, value: string; key: Integer; types : TArray<String>);
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
    raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [name + ':' + modifier]));
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

procedure TSearchProcessor.ProcessReferenceParam(var Result: string; name,modifier,value: string; key: Integer;types : TArray<String>);
var
  parts: TArray<String>;
  targets : TArray<String>;
  i, index : integer;
begin
  i := 0;
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
      if (length(targets) <> 1) then
      begin
        // special case for bug in R4...
        index := StringArrayIndexOfInsensitive(targets, name);
        if index = -1 then
          raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_INVALID_TARGETTYPE', lang), [name, i]))
        else
          result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value = ''' + sqlwrapString(value) + ''')'
      end
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
        raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_INVALID', lang), [name]));
    end
  end
  else if StringArrayExistsSensitive(factory.ResourceNames, modifier) then
  begin
    if IsId(value) then
      result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(modifier) + ''') and Value = ''' + sqlwrapString(value) + ''')'
    else if value.StartsWith(baseUrl) then
    begin
      parts := value.Substring(baseURL.Length).Split(['/']);
      if Length(parts) = 2 then
      begin
        if modifier <> parts[0] then
          raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_MODIFIER_INVALID', lang), [name]));
        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(parts[0]) + ''') and SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(modifier) + ''')  and Value = ''' + sqlwrapString(parts[1]) + ''')';
      end
      else
        raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_INVALID', lang), [name]));
    end
    else
      raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_MODIFIER_INVALID', lang), [modifier]));
  end
  else
    raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_MODIFIER_INVALID', lang), [modifier]));
end;

procedure TSearchProcessor.ProcessTokenParam(var Result: string; name, modifier, value: string; key: Integer; types : TArray<String>);
var
  system, code : String;
begin
  system := '--';
  if (name = '_id') or (name = 'id') then
    result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value = ''' + sqlwrapString(value) + ''')'
  else if modifier = 'text' then
    result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value2 like ''' + sqlwrapString(lowercase(RemoveAccents(value))) + '%'')'
  else if modifier = 'in' then
    result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and ' + processValueSetMembership(name, value) + ')'
  else if modifier = 'not-in' then
    result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and not (' + processValueSetMembership(name, value) + '))'
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
        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and not (Value = ''' + sqlwrapString(value) + ''') and not (value = ''''))'
      else if (system = '') then
        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and not (SpaceKey = null and Value = ''' + sqlwrapString(code) + ''') and not (value = ''''))'
      else if (code = '') then // this variation (no code, only system) is not described in the spec
        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and not (SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(system) + ''')) and not (value = ''''))'
      else
        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and not (SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(system) + ''') and Value = ''' + sqlwrapString(code) + ''') and not (value = ''''))';
    end
    else if modifier = 'excludes' then
    begin
      if (system = '--') then
        result := result + 'not (IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and (Value = ''' + sqlwrapString(value) + '''))'
      else if (system = '') then
        result := result + 'not (IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and (SpaceKey = null and Value = ''' + sqlwrapString(code) + '''))'
      else if (code = '') then // this variation (no code, only system) is not described in the spec
        result := result + 'not (IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and (SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(system) + ''')))'
      else
        result := result + 'not (IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and (SpaceKey = (Select SpaceKey from Spaces where Space = ''' + sqlwrapstring(system) + ''') and Value = ''' + sqlwrapString(code) + '''))';
    end
    else if modifier = 'above' then
      raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_MODIFIER_INVALID', lang), [modifier]))
    else if modifier = 'below' then
      raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_MODIFIER_INVALID', lang), [modifier]))
    else
      raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_MODIFIER_INVALID', lang), [modifier]));
  end;
end;

procedure TSearchProcessor.ProcessUriParam(var Result: string; name, modifier, value: string; key: Integer; types : TArray<String>);
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
    raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_MODIFIER_INVALID', lang), [modifier]));
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

procedure TSearchProcessor.ProcessStringParam(var Result: string; name, modifier, value: string; key: Integer; var pfx: string; var sfx: string; types : TArray<String>);
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
    raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_MODIFIER_INVALID', lang), [modifier]));
  result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value ' + pfx + sqlwrapString(v) + sfx + ')';
end;

procedure TSearchProcessor.ProcessDateParam(date: TFslDateTime; var Result: string; name, modifier, value: string; key: Integer; types : TArray<String>);
var
  qop: TQuantityOperation;
begin
  if modifier <> '' then
    raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [name + ':' + modifier]));
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
  date := TFslDateTime.fromXml(value);
  case qop of
    qopEqual:        result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value >= ''' + date.Min.UTC.toHL7 + ''' and Value2 <= ''' + date.Max.UTC.toHL7 + ''')';
    qopNotEqual:     result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and (Value < ''' + date.Min.UTC.toHL7 + ''' or Value2 > ''' + date.Max.UTC.toHL7 + '''))';
    qopLess:         result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value <= ''' + date.Min.UTC.toHL7 + ''')';
    qopLessEqual:    result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value <= ''' + date.Max.UTC.toHL7 + ''')';
    qopGreater:      result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value2 >= ''' + date.Max.UTC.toHL7 + ''')';
    qopGreaterEqual: result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value2 >= ''' + date.Min.UTC.toHL7 + ''')';
    qopStartsAfter:  result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value2 >= ''' + date.Min.UTC.toHL7 + ''')';
    qopEndsBefore:   result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value2 >= ''' + date.Min.UTC.toHL7 + ''')';
    qopApproximate:  result := result + '(IndexKey = ' + inttostr(Key) + ' /*' + name + '*/ and Value <= ''' + date.Max.UTC.toHL7 + ''' and Value2 >= ''' + date.Min.UTC.toHL7 + ''')';
  end;
end;

procedure TSearchProcessor.ProcessNumberParam(var Result: string; name, modifier, value: string; key: Integer; types: TArray<String>);
var
  qop: TQuantityOperation;
  v1: string;
  v2: string;
begin
  if modifier <> '' then
    raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [name + ':' + modifier]));

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

  processNumberValue(TFslDecimal.ValueOf(value), qop, v1, v2);

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
    raise EFHIRException.create('The operation ''' + CODES_CompareOperation[op] + ''' is not supported for parameter types of reference');
  end;
end;

function TSearchProcessor.buildParameterToken(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
var
  ref: string;
  like: Boolean;
begin
  begin
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
        raise EFHIRException.create('Not implemented yet');
      fscoSB:
        raise EFHIRException.create('Not implemented yet');
      fscoIN:
        raise EFHIRException.create('Not implemented yet');
      fscoCO:
        result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value2 like ''%' + SQLWrapString(value) + '%''' + j + ')';
      fscoSW:
        result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value2 like ''' + SQLWrapString(value) + '%''' + j + ')';
      fscoEW:
        result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value2 like ''%' + SQLWrapString(value) + '''' + j + ')';
    else
      // fscoGT, fscoLT, fscoGE, fscoLE, fscoPO, fscoRE
      raise EFHIRException.create('The operation ''' + CODES_CompareOperation[op] + ''' is not supported for parameter types of token');
    end;
  end;
end;

function TSearchProcessor.buildParameterDate(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
var
  date: TFslDateTime;
begin
  begin
    date := TFslDateTime.fromXml(value);
    case op of
      fscoEQ:
        result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value = ''' + date.Min.UTC.toHL7 + ''' and ' + n + '.Value2 = ''' + date.Max.UTC.toHL7 + '''' + j + ')';
      fscoNE:
        result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and (' + n + '.Value <> ''' + date.Min.UTC.toHL7 + ''' or ' + n + '.Value2 <> ''' + date.Max.UTC.toHL7 + ''')' + j + ')';
      fscoGT:
        result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value2 >= ''' + date.Max.UTC.toHL7 + '''' + j + ')';
      fscoLT:
        result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value <= ''' + date.Min.UTC.toHL7 + '''' + j + ')';
      fscoGE:
        result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value2 >= ''' + date.Min.UTC.toHL7 + '''' + j + ')';
      fscoLE:
        result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value <= ''' + date.Max.UTC.toHL7 + '''' + j + ')';
      fscoPO:
        result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and (' + n + '.Value >= ''' + date.Max.UTC.toHL7 + ''' or ' + n + '.Value2 <= ''' + date.Min.UTC.toHL7 + ''')' + j + ')';
    else
      // fscoSS, fscoSB, fscoIN, fscoRE, fscoCO, fscoSW, fscoEW
      raise EFHIRException.create('The operation ''' + CODES_CompareOperation[op] + ''' is not supported for parameter types of date');
    end;
  end;
end;

function TSearchProcessor.BuildParameterString(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
begin
  case op of
    fscoEQ:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value = ''' + SQLWrapString(RemoveAccents(Value)) + '''' + j + ')';
    fscoNE:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value <> ''' + SQLWrapString(RemoveAccents(Value)) + '''' + j + ')';
    fscoCO:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value like ''%' + SQLWrapString(RemoveAccents(Value)) + '%''' + j + ')';
    fscoSW:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value like ''' + SQLWrapString(RemoveAccents(Value)) + '%''' + j + ')';
    fscoEW:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value like ''%' + SQLWrapString(RemoveAccents(Value)) + '''' + j + ')';
    fscoGT:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value > ''' + SQLWrapString(RemoveAccents(Value)) + '''' + j + ')';
    fscoLT:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value < ''' + SQLWrapString(RemoveAccents(Value)) + '''' + j + ')';
    fscoGE:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value >= ''' + SQLWrapString(RemoveAccents(Value)) + '''' + j + ')';
    fscoLE:
      result := 'ResourceKey in (select ResourceKey from IndexEntries as ' + n + ' where Flag <> 2 and ' + n + '.IndexKey = ' + inttostr(index) + ' and ' + n + '.Value <= ''' + SQLWrapString(RemoveAccents(Value)) + '''' + j + ')';
  else
    // fscoPO, fscoSS, fscoSB, fscoIN, fscoRE
    raise EFHIRException.create('The operation ''' + CODES_CompareOperation[op] + ''' is not supported for parameter types of string');
  end;
end;

function TSearchProcessor.BuildParameterNumber(index: Integer; n: Char; j: string; name : String; op : TFSCompareOperation; value: string) : String;
  function CheckInteger(s : String):String;
  begin
    if StringIsInteger32(s) then
      result := s
    else
      raise EFHIRException.create('not a valid number');
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
    raise EFHIRException.create('The operation ''' + CODES_CompareOperation[op] + ''' is not supported for parameter types of number');
  end;
end;

procedure TSearchProcessor.processNumberValue(value : TFslDecimal; op : TQuantityOperation; var minv, maxv : String);
begin
  // work out the numerical limits
  case op of
    qopEqual :
      begin
      minv := value.lowerBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, false);
      maxv := value.upperBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      end;
    qopLess :
      maxv := value.lowerBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, false);
    qopLessEqual :
      maxv := value.immediateLowerBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, false);
    qopGreater :
      minv := value.UpperBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
    qopGreaterEqual :
      minv := value.immediateUpperBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
    qopApproximate :
      begin
      if value.IsNegative then
      begin
        minv := value.Multiply(TFslDecimal.ValueOf('1.2')).lowerBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, false);
        maxv := value.Multiply(TFslDecimal.ValueOf('0.8')).upperBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      end
      else
      begin
        minv := value.Multiply(TFslDecimal.ValueOf('0.8')).lowerBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, false);
        maxv := value.Multiply(TFslDecimal.ValueOf('1.2')).upperBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      end;
      end;
  end;
end;


procedure TSearchProcessor.processQuantityValue(name : String; const lang : THTTPLanguages; parts : TArray<string>; op : TQuantityOperation; var minv, maxv, space, mincv, maxcv, spaceC : String);
var
  value : TFslDecimal;
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
    raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [name]));
  if TFslDecimal.CheckValue(parts[0]) then
    v := parts[0]
  else
  begin
    i := 1;
    s := parts[0];
    while (i <= length(s)) and CharInSet(s[i], ['0'..'9', '.', '-']) do
      inc(i);
    if i = 1 then
      raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [name]));
    v := parts[0].Substring(0, i-1);
    u := parts[0].Substring(i-1);
  end;
  value := TFslDecimal.valueOf(v);

  // work out the numerical limits
  case op of
    qopEqual :
      begin
      minv := value.lowerBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, false);
      maxv := value.upperBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      end;
    qopLess :
      maxv := value.lowerBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, false);
    qopLessEqual :
      maxv := value.immediateLowerBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, false);
    qopGreater :
      minv := value.UpperBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
    qopGreaterEqual :
      minv := value.immediateUpperBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
    qopApproximate :
      begin
      if value.IsNegative then
      begin
        minv := value.Multiply(TFslDecimal.valueOf('1.2')).lowerBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, false);
        maxv := value.Multiply(TFslDecimal.valueOf('0.8')).upperBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      end
      else
      begin
        minv := value.Multiply(TFslDecimal.valueOf('0.8')).lowerBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, false);
        maxv := value.Multiply(TFslDecimal.valueOf('1.2')).upperBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
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
    raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_UNKNOWN', lang), [name]))
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
      canonical := TFHIRServerContext(ServerContext).TerminologyServer.CommonTerminologies.Ucum.getCanonicalForm(specified);
      try
        mincv := canonical.Value.lowerBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, false);
        maxcv := canonical.Value.upperBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
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

function TSearchProcessor.factory: TFHIRFactory;
begin
  result := TFHIRServerContext(ServerContext).Factory;
end;

function TSearchProcessor.fetchGroup(id: String): TFHIRResourceV;
var
  client : TFhirClientV;
begin
  client := TFHIRServerContext(ServerContext).Storage.createClient(lang, ServerContext, TFHIRServerContext(ServerContext).ValidatorContext.link, FSession.link);
  try
    result := client.readResourceV('Group', id);
    if result = nil then
      raise EFHIRException.create('Unable to find group '+id);
  finally
    client.Free;
  end;
end;

Function TSearchProcessor.filterTypes(types : TArray<String>) : TArray<String>;
var
  a : string;
  res : TStringList;
begin
  res := TStringList.Create;
  try
  for a in types do
    if session.canRead(a) then
        res.add(a);
    result := res.ToStringArray;
  finally
    res.free;
  end;
end;

function isCommonSearchParameter(name : String): boolean;
begin
  result := StringArrayExistsSensitive(['_id', '_lastUpdated', '_tag', '_profile', '_security', '_text', '_content', '_list', '_query'], name);
end;

Function TSearchProcessor.processParam(types : TArray<String>; name : String; value : String; nested : boolean; var bFirst : Boolean; var bHandled : Boolean) : String;
var
  key, i : integer;
  left, right, op, modifier, tl : String;
  f, isReverse : Boolean;
  ts : TStringList;
  pfx, sfx, n : String;
  date : TFslDateTime;
  a : String;
  type_ : TFhirSearchParamType;
  group : TFHIRGroupW;
  characteristic : TFhirGroupCharacteristicW;
begin
  isReverse := false;
  a := '';
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
  else if (name = '_type') then
  begin
    types := value.Split([',']);
    for a in types do
      tl := tl+','+inttostr(TFHIRServerContext(ServerContext).ResConfig[a].key);
    if (tl <> '') then
      tl := tl.Substring(1);
    result := result + '(ResourceTypeKey in ('+tl+'))';
    bHandled := true;
  end
  else if (name = '_list') then
  begin
    result := result + '(IndexKey = '+inttostr(FIndexes.ListItemIndex)+' /*'+left+'*/ and ResourceKey in (select ResourceKey from Ids where ResourceTypeKey = '+inttostr(TFHIRServerContext(ServerContext).ResConfig['List'].key)+' and Id = '''+SQLWrapString(value)+'''))';
    bHandled := true;
    isReverse :=true;
  end
  else if (name = '_group') then
  begin
    group := factory.wrapGroup(fetchGroup(value).Link);
    try
      if group.hasMembers then
      begin
        result := result + '(IndexKey = '+inttostr(FIndexes.GroupMemberIndex)+' /*'+left+'*/ and ResourceKey in (select ResourceKey from Ids where ResourceTypeKey = '+inttostr(TFHIRServerContext(ServerContext).ResConfig['Group'].key)+' and Id = '''+SQLWrapString(value)+'''))';
        bHandled := true;
        isReverse := true;
      end
      else if group.hasCharacteristics then
      begin
        for characteristic in group.characteristics.forEnum do
        begin
          n := characteristic.code.fromSystem(['http://hl7.org/fhir/StructureDefinition/Patient', 'http://hl7.org/fhir/StructureDefinition/Practitioner', 'http://hl7.org/fhir/StructureDefinition/Device', 'http://hl7.org/fhir/StructureDefinition/Medication', 'http://hl7.org/fhir/StructureDefinition/Substance']);
          if n = 'Patient.gender' then
            params.add('gender', characteristic.value.fromSystem('http://hl7.org/fhir/ValueSet/administrative-gender', true));
        end;
      end
      else
        raise EFHIRException.create('Unable to process group "'+group.name+'": it has no members or characteristics');
    finally
      group.Free;
    end;
  end
  else if (name = '_text') then
  begin
    if Connection.owner.platform = kdbSQLServer then
      result := '(IndexKey = '+inttostr(FIndexes.NarrativeIndex)+' and CONTAINS(Xhtml, '''+SQLWrapString(value)+'''))';
    if Connection.owner.platform = kdbMySQL then
      result := '(IndexKey = '+inttostr(FIndexes.NarrativeIndex)+' and MATCH (Xhtml) AGAINST ('''+SQLWrapString(value)+'''))';
  end
  else if pos('.', name) > 0 then
  begin
    StringSplit(name, '.', left, right);
    if (pos(':', left) > 0) then
    begin
      StringSplit(left, ':', left, modifier);
      if not StringArrayExistsInSensitive(factory.ResourceNames, modifier) then
        raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_UNKNOWN_TYPE', lang), [modifier]));
      types := filterTypes(TArray<String>.create(modifier));
    end
    else
    begin
      types := filterTypes(FIndexes.GetTargetsByName(types, left));
    end;
    key := FIndexes.GetKeyByName(left);
    if key = 0 then
      raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_PARAM_CHAINED', lang), [left]));
    f := true;
    tl := '';
    for a in types do
      tl := tl+','+inttostr(TFHIRServerContext(ServerContext).ResConfig[a].key);
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
        raise EFHIRException.create('modifier "'+modifier+'" not handled on originalId');
      result :=  '(originalId = '''+sqlwrapString(value)+''')';
    end
    else
    begin
      key := FIndexes.GetKeyByName(name);
      if (length(types) = 0) and not isCommonSearchParameter(name) then
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
                sptDate: ProcessDateParam(date, Result, name, modifier, value, key, types);
                sptString: ProcessStringParam(Result, name, modifier, value, key, pfx, sfx, types);
                sptUri: ProcessUriParam(Result, name, modifier, value, key, types);
                sptToken: ProcessTokenParam(Result, name, modifier, value, key, types);
                sptReference : ProcessReferenceParam(Result, name, modifier, value, key, types);
                sptQuantity : ProcessQuantityParam(Result, name, modifier, value, key, types);
                sptNumber : ProcessNumberParam(Result, name, modifier, value, key, types);
                sptNull : if (name = '_id') then
                  ProcessTokenParam(Result, name, modifier, value, key, types);
              else if type_ <> sptNull then
                raise EFHIRTodo.create('type = '+CODES_TFhirSearchParamType[type_]);
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
      if isReverse then
      // This last ORDER BY is to workaround MariaDB Issue where a subquery in an IN clause may not give the results if there is an ORDER BY.
        result := 'Ids.ResourceKey in (select Target from IndexEntries where Flag <> 2 and '+result+order(' order by Target DESC')+')'
      else
        result := 'Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and '+result+order(' order by resourcekey DESC')+')';
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
      result := '('+BuildFilter(filter, ' ', issuer, TArray<String>.create(FType))+')';
    finally
      filter.Free;
    end;
  finally
    issuer.Free;
  end;
end;

function TSearchProcessor.processValueSetMembership(code, vs: String): String;
var
  vso : TFHIRValueSetW;
  key : integer;
begin
  // firstly, the vs can be a logical reference or a literal reference
  if (vs.StartsWith('valueset/')) then
  begin
    vso := TFHIRServerContext(ServerContext).TerminologyServer.getValueSetByUrl(vs);
    try
      if vso = nil then
        vs := 'not-found'
      else
        vs := vso.url;
    finally
      vso.Free;
    end;
  end;
  key := FConnection.CountSQL('select ValueSetKey from ValueSets where URL = '''+sqlWrapString(vs)+'''');
  if key = 0 then
    FWarnings.Add(factory.makeIssue(isWarning, itNotFound, 'http.url.'+code, 'The value set "'+vs+'" is not indexed, and cannot be searched on'));
  result := 'Concept in (select ConceptKey from ValueSetMembers where ValueSetKey = '+inttostr(key)+')';
end;

procedure TSearchProcessor.replaceNames(paramPath: TFSFilterParameterPath; components: TFslStringDictionary);
begin
  if components.ContainsKey(paramPath.Name) then
    paramPath.Name := components[paramPath.Name]
  else
    raise EFHIRException.create('Unknown Search Parameter Name "'+paramPath.Name+'"');
end;

procedure TSearchProcessor.replaceNames(filter: TFSFilter; components: TFslStringDictionary);
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

procedure TSearchProcessor.SetCompartment(const Value: TFHIRCompartmentId);
begin
  FCompartment.Free;
  FCompartment := Value;
end;

procedure TSearchProcessor.SetConnection(const Value: TFDBConnection);
begin
  FConnection.Free;
  FConnection := Value;
end;

procedure TSearchProcessor.SetIndexes(const Value: TFhirIndexInformation);
begin
  FIndexes.Free;
  FIndexes := Value;
end;


procedure TSearchProcessor.SetResConfig(const Value: TFslMap<TFHIRResourceConfig>);
begin
  FResConfig.Free;
  FResConfig := Value;
end;

procedure TSearchProcessor.SetSession(const Value: TFhirSession);
begin
  FSession.Free;
  FSession := Value;
end;

procedure TSearchProcessor.SetSessionCompartments(const Value: TFslList<TFHIRCompartmentId>);
begin
  FSessionCompartments.Free;
  FSessionCompartments := Value;
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

function TSearchProcessor.order(s: String): String;
begin
  if Connection.owner.platform = kdbMySQL then
    result := s
  else
    result := '';
end;

function TSearchProcessor.BuildFilter(filter: TFSFilter; parent: char; issuer: TFSCharIssuer; types : TArray<String>): String;
begin
  case filter.FilterItemType of
    fsitParameter : result := BuildFilterParameter(filter as TFSFilterParameter, TFSFilterParameter(filter).ParamPath, parent, issuer, types);
    fsitLogical :   result := BuildFilterLogical(filter as TFSFilterLogical, parent, issuer, types);
  else
    raise EFHIRException.create('Unknown type');
  end;
end;

function TSearchProcessor.BuildFilterLogical(filter: TFSFilterLogical; parent: char; issuer: TFSCharIssuer; types : TArray<String>): String;
begin
  if filter.Operation = fsloNot then
    result := '(Not '+BuildFilter(filter.Filter2, parent, issuer, types)+')'
  else if filter.Operation = fsloOr then
    result := '('+BuildFilter(filter.Filter1, parent, issuer, types)+' or '+BuildFilter(filter.Filter2, parent, issuer, types)+')'
  else // filter.Operation = fsloAnd
    result := '('+BuildFilter(filter.Filter1, parent, issuer, types)+' and '+BuildFilter(filter.Filter2, parent, issuer, types)+')';

end;


function TSearchProcessor.BuildFilterParameter(filter: TFSFilterParameter; path : TFSFilterParameterPath; parent: char; issuer: TFSCharIssuer; types : TArray<String>): String;
var
  index : integer;
  n : char;
  j : string;
  stype : TFhirSearchParamType;
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
        raise EFHIRException.create('Parameter ("'+path.Name+'") is missing a filter - it is required');
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
        raise EFHIRException.create('Not handled yet');
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
      sptNull: raise EFHIRException.create('The search type could not be determined for '+path.Name);
      sptNumber:    result := BuildParameterNumber(index, n, j, path.Name, filter.Operation, filter.Value);
      sptString :   result := BuildParameterString(index, n, j, path.Name, filter.Operation, filter.Value);
      sptDate:      result := buildParameterDate(index, n, j, path.Name, filter.Operation, filter.Value);
      sptToken:     result := buildParameterToken(index, n, j, path.Name, filter.Operation, filter.Value);
      sptReference: result := buildParameterReference(index, n, j, path.Name, filter.Operation, filter.Value);
      sptComposite: raise EFHIRException.create('Composite indexes cannot the direct target of an operation criteria (except for operation "pr")');
      sptQuantity:  raise EFHIRException.create('Quantity searching is not handled yet');
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
       raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_DATE_FORMAT', lang), [s]));

    if (length(s) = 16) and (s[14] = ':') and StringIsCardinal16(copy(s, 12, 2)) and StringIsCardinal16(copy(s, 15, 2)) then
      ok := true
    else if (length(s) = 19) and (s[14] = ':') and (s[17] = ':') and
          StringIsCardinal16(copy(s, 12, 2)) and StringIsCardinal16(copy(s, 15, 2)) and StringIsCardinal16(copy(s, 18, 2)) then
      ok := true
    else if (length(s) > 20) and (length(s) < 29)  then
      ok := (s[14] = ':') and (s[17] = ':') and (s[20] = '.') and StringisNumeric(copy(s, 21, 9));
  end;
  if not ok then
    raise EFHIRException.create(StringFormat(GetFhirMessage('MSG_DATE_FORMAT', lang), [s]));
end;

constructor TSearchProcessor.create(serverContext : TFslObject);
begin
  Inherited Create(serverContext);
  FStrict := false;
  FWarnings := TFslList<TFhirOperationOutcomeIssueW>.create;
end;

destructor TSearchProcessor.Destroy;
begin
  FResConfig.Free;
  FSessionCompartments.Free;
  FCompartment.Free;
  FWarnings.Free;
  FConnection.Free;
  FSession.Free;
  FIndexes.Free;
  inherited;
end;

end.

