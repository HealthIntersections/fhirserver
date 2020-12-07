unit fsl_http;

{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

interface

uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  Classes, Generics.Collections, Generics.Defaults,
  fsl_base, fsl_utilities;

const
  HTTPUtilAnonymousItemName = 'ANONYMOUS';
  { TMultiValList: any parsed items found without names will be collected under this name }

type
  TMultiValList = class(TFslObject)
  Private
    fItemList: TStringList;

    procedure addItem(itemname: String; const itemvalue: String);
    //      procedure StripEntries(starter : string);
    function retrieveItem(const itemname: String; index: Integer; var itemval: String): Boolean;
    function retrieveNameIndex(itemname: String; var itemnum: Integer): Boolean;
    procedure ParseAddItem(p: PChar; decodeflag: Boolean; itemnamestart, itemnamelen, itemvalstart, itemvallen: Integer);
    procedure Parse(const instr: String; decodeflag: Boolean; delimchar: Char);
  protected
    FSource: String;
    function getItemCount: Integer;
    function VarName(index: Integer): String;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;

    function getValueCount(itemnum: Integer): Integer;
    function retrieveNumberedItem(itemnum: Integer; index: Integer; var itemval: String): Boolean;
    function list(index : integer):TStringList;
  end;

  THTTPParameters = class(TMultiValList)
  private
    function GetVar(n: String): String;
  Public
    constructor Create(const s: String; MimeDecode: Boolean = True);
    function Link : THTTPParameters; overload;

    function has(const n: String): Boolean;

    property Count : Integer read getItemCount;
    property Value[Name: String]: String Read GetVar; default;
    property Name[index : integer]: String Read VarName;
    property Source: String Read FSource;
    procedure add(sName: String; const sValue: String);
  end;

  TMimeContentType = class (TFslObject)
  private
    FSource : String;
    FParams: TFslStringDictionary;
    FBase: String;
    function GetMain: String;
    function GetSub: String;
    procedure SetMain(const Value: String);
    procedure SetSub(const Value: String);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function link : TMimeContentType; overload;

    class function parseSingle(s : String) : TMimeContentType;
    class function parseList(s : String) : TFslList<TMimeContentType>;

    property source : String read FSource;
    property base : String read FBase write FBase;
    property main : String read GetMain write SetMain;
    property sub : String read GetSub write SetSub;

    function isValid : boolean;

    property Params : TFslStringDictionary read FParams;
    function hasParam(name : String) : boolean;
  end;


  THTTPLanguages = record
  private
    FSource: String;
    FCodes : TArray<String>;

    function GetCodes: TArray<String>;
    function codeMatches(code, spec : String) : boolean;
  public
    constructor Create(hdr : String);

    property codes : TArray<String> read GetCodes; // in order...
    property header : String read FSource;

    function matches(code: String): boolean;
    function prefLang : String;
    function sizeInBytes : cardinal;
  end;

  //end;

implementation

uses
  SysUtils;

const
  unitname = 'fsl_http';

  {------------------------------------------------------------------------------}
constructor TMultiValList.Create;
begin
  inherited Create;
  FItemList := TStringList.Create;
  FItemList.CaseSensitive := false;
end;

destructor TMultiValList.Destroy;
var
  vallist: TStringList;
  i: Integer;
begin
  if FItemList <> NIL then
    begin
    for i := 0 to FItemList.Count - 1 do
      begin
      vallist := FItemList.Objects[i] as TStringList;
      if vallist <> NIL then
        vallist.Free;
      end;
    FItemList.Free;
    FItemList := NIL;
    end;
  inherited Destroy;
end;

procedure TMultiValList.addItem(itemname: String; const itemvalue: String);
var
  vallist: TStringList;
  tempint: Integer;
begin
  if not retrieveNameIndex(itemname, tempint) then
    begin
    itemname := itemname;
    vallist := TStringList.Create;
    FItemList.AddObject(itemname, vallist);
    end
  else
    vallist := FItemList.Objects[tempint] as TStringList;
  vallist.Add(itemvalue);
end;

function TMultiValList.retrieveItem(const itemname: String;
  index: Integer;
  var itemval: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if retrieveNameIndex(itemname, i) then
    Result := retrieveNumberedItem(i, index, itemval);
end;


function TMultiValList.retrieveNumberedItem(itemnum: Integer;
  index: Integer;
  var itemval: String): Boolean;
var
  vallist: TStringList;
begin
  Result := False;
  try
    vallist := FItemList.Objects[itemnum] as TStringList;
    if vallist <> NIL then
      begin
      if vallist.Count >= index then
        begin
        itemval := vallist.Strings[index];
        Result := True;
        end;
      end;
  except
    on e:
    Exception do
      Result := False;
    end;
end;

function TMultiValList.retrieveNameIndex(itemname: String; var itemnum: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;

  i := FItemList.indexOf(itemname);
  if i<>-1 then
    begin
    itemnum := i;
    Result := True;
    end;
end;

function TMultiValList.getItemCount: Integer;
begin
  Result := FItemList.Count;
end;

function TMultiValList.getValueCount(itemnum: Integer): Integer;
var
  vallist: TStringList;
begin
  Result := 0;

  vallist := FItemList.Objects[itemnum] as TStringList;
  if vallist <> NIL then
    Result := vallist.Count;
end;

procedure TMultiValList.ParseAddItem(p: PChar; decodeflag: Boolean;
  itemnamestart, itemnamelen, itemvalstart, itemvallen: Integer);
var
  itemname, itemvalue: String;
  temppchar: PChar;
  tempint: Integer;
begin
  tempint := itemnamelen;
  if itemvallen > tempint then
    tempint := itemvallen;
  GetMem(temppchar, (tempint + 1) * 2);

  try
    { set the name string: if we didn't get a name,
      use the anonymous string }
    if (itemnamelen > 0) then
      begin
      StrLCopy(temppchar, p + itemnamestart,
        itemnamelen);
      itemname := temppchar;
      end
    else
      itemname := HTTPUtilAnonymousItemName;

    if decodeflag then
      { when handling cookies, the name may
        be preceded by white space }
      itemname := DecodeMIME(TrimLeft(itemname))
    else
      itemname := itemname;

    { set the value string: if we didn't get
      a name, use an empty string }
    if (itemvallen > 0) then
      begin
      StrLCopy(temppchar, p + itemvalstart, itemvallen);
      itemvalue := temppchar;
      end
    else
      itemvalue := '';

    if decodeflag then
      { trim off the trailing newline that some browsers
        send. Can't do this with parameters, because
        the user might want this stuff }
      itemvalue := DecodeMimeURL(itemvalue)
    else
      itemvalue := itemvalue;

    addItem(itemname, itemvalue);

  finally
    Freemem(temppchar);
    end;
end;


procedure TMultiValList.Parse(const instr: String; decodeflag: Boolean; delimchar: Char);
var
  cursor, len, itemnamestart, itemnamelen, itemvalstart, itemvallen: Integer;
begin
  FSource := inStr;

  len := Length(instr);
  cursor := 0;
  itemnamestart := 0;
  itemvalstart := 0;
  itemnamelen := 0;

  while cursor < len do
    begin
    if PChar(instr)[cursor] = delimchar then
      begin
      if itemNameLen = 0 then
        begin
        itemnamelen := cursor - itemnamestart;
        itemvalstart := cursor;
        end;
      itemvallen := cursor - itemvalstart;
      ParseaddItem(PChar(instr), decodeflag, itemnamestart, itemnamelen, itemvalstart, itemvallen);
      cursor := cursor + 1;
      { next item starts at the *next* character }
      itemnamestart := cursor;
      itemnamelen := 0;
      itemvalstart := cursor;
      end
    else if PChar(instr)[cursor] = '=' then
      begin
        { this signals the end of the item name;
          compute its length. The value starts at
          the *next* character }
      itemnamelen := cursor - itemnamestart;
      cursor := cursor + 1;
      itemvalstart := cursor;
      end
    else
      begin
      { examine the next character }
      cursor := cursor + 1;
      end;
    end;

  { the last item should have have ended without a
    delimiter. As in that case above, calculate the
    length of the value, and add the item. However,
    if zero items were present in the request, we
    don't want to add a spurious item; that's
    what the if statement is checking for }
  if cursor > itemvalstart then
    begin
    itemvallen := cursor - itemvalstart;
    ParseaddItem(PChar(instr), decodeflag, itemnamestart,
      itemnamelen, itemvalstart, itemvallen);
    end;
end;

function TMultiValList.VarName(index: Integer): String;
begin
  Result := FItemList[index];
end;

{-----------------------------------------------------------------------------}

function TMultiValList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, fItemList.sizeInBytes);
  inc(result, (FSource.length * sizeof(char)) + 12);
end;

procedure THTTPParameters.add(sname: String; const svalue: String);
begin
  addItem(sname, svalue);
end;

constructor THTTPParameters.Create(const s: String; MimeDecode: Boolean = True);
begin
  inherited Create;
  Parse(StringTrimWhitespaceRight(s), MimeDecode, '&');
end;

function THTTPParameters.has(const n: String): Boolean;
var
  i: Integer;
begin
  Result := retrieveNameIndex(n, i);
end;

// only return first variable
function THTTPParameters.getVar(n: String): String;
var
  i, c: Integer;
  s: String;
begin
  Result := '';
  if retrieveNameIndex(n, i) then
    begin
    c := getValueCount(i);
    for i := 0 to c - 1 do
      begin
      retrieveitem(n, i, s);
      if Result = '' then
        Result := s
      else
        Result := Result + ';' + s;
      end;
    end;
end;

function THTTPParameters.Link: THTTPParameters;
begin
  result := THTTPParameters(inherited Link);
end;

function TMultiValList.list(index: integer): TStringList;
begin
  result := TStringList(FItemList.Objects[index]);
end;

{ TMimeContentType }

constructor TMimeContentType.Create;
begin
  inherited;
  FParams := TFslStringDictionary.create;
end;

destructor TMimeContentType.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TMimeContentType.GetMain: String;
begin
  if FBase.Contains('/') then
    result := FBase.Substring(0, FBase.IndexOf('/'))
  else
    result := FBase;
end;

function TMimeContentType.GetSub: String;
begin
  if FBase.Contains('/') then
    result := FBase.Substring(FBase.IndexOf('/')+1)
  else
    result := '';
end;

function TMimeContentType.hasParam(name: String): boolean;
begin
  result := FParams.ContainsKey(name);
end;

function TMimeContentType.isValid: boolean;
begin
  result := (StringArrayExistsSensitive(['application', 'audio', 'font', 'example', 'image', 'message', 'model', 'multipart', 'text', 'video'], main) or main.StartsWith('x-'))
    and (sub <> '');
end;

function TMimeContentType.link: TMimeContentType;
begin
  result := TMimeContentType(inherited link);
end;

procedure TMimeContentType.SetMain(const Value: String);
begin
  if FBase.Contains('/') then
    FBase := Value+'/'+sub
  else
    FBase := Value;
end;

procedure TMimeContentType.SetSub(const Value: String);
begin
  if FBase.Contains('/') then
    FBase := Main+'/'+value
  else
    FBase := 'application/'+Value;
end;

function TMimeContentType.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FSource.length * sizeof(char)) + 12);
  inc(result, FParams.sizeInBytes);
  inc(result, (FBase.length * sizeof(char)) + 12);
end;

class function TMimeContentType.parseList(s : String): TFslList<TMimeContentType>;
var
  e : String;
begin
  result := TFslList<TMimeContentType>.create;
  try
    for e in s.Split([',']) do
      result.add(parseSingle(e));
    result.link;
  finally
    result.Free;
  end;
end;


class function TMimeContentType.parseSingle(s : String): TMimeContentType;
var
  p : string;
begin
  result := TMimeContentType.Create;
  try
    result.FSource := s;
    for p in s.Split([';']) do
    begin
      if result.FBase = '' then
      begin
        if (p = 'xml') then
          result.FBase := 'application/fhir+xml'
        else if (p = 'json') then
          result.FBase := 'application/fhir+json'
        else if (p = 'ttl') then
          result.FBase := 'application/fhir+ttl'
        else
          result.FBase := p;
      end
      else
        result.FParams.Add(p.Substring(0, p.IndexOf('=')), p.Substring(p.IndexOf('=')+1));
    end;
    result.link;
  finally
    result.Free;
  end;
end;




//       StringSplit(lang, [';', ','], l, lang);

type
  TLanguageSpec = class (TFslObject)
  private
    FCode : String;
    FValue : Double;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor create(code : String; value : Double);
  end;

{ TLanguageSpec }

constructor TLanguageSpec.create(code: String; value: Double);
begin
  inherited Create;
  FCode := code;
  FValue := value;
end;

type
  TLanguageSpecComparer = class (TFslComparer<TLanguageSpec>)
  public
    function Compare(const l, r: TLanguageSpec): Integer; override;
  end;

function TLanguageSpec.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCode.length * sizeof(char)) + 12);
end;

function TLanguageSpecComparer.Compare(const l, r : TLanguageSpec) : integer;
begin
  if l.FValue > r.FValue then
    result := 1
  else if r.FValue > l.FValue then
    result := -1
  else
    result := 0;
end;

{ THTTPLanguages }

constructor THTTPLanguages.Create(hdr: String);
var
  list : TFslList<TLanguageSpec>;
  i : integer;
  s, l, r : String;
begin
  FSource := hdr;
  list := TFslList<TLanguageSpec>.create;
  try
    for s in hdr.Split([',']) do
    begin
      if s.Contains(';') then
      begin
        StringSplit(s, ';', l, r);
        list.Add(TLanguageSpec.Create(l, StrToFloatDef(r, 0.5)));
      end
      else
        list.Add(TLanguageSpec.Create(s, 1));
    end;
    if (list.count > 1) then
    begin
      list.Sort(TLanguageSpecComparer.create);
    end;
    SetLength(FCodes, list.Count);
    for i := 0 to list.Count - 1 do
      FCodes[i] := list[i].FCode;
  finally
    list.Free;
  end;
end;

function THTTPLanguages.GetCodes: TArray<String>;
begin
  result := FCodes;
end;

function THTTPLanguages.codeMatches(code, spec : String) : boolean;
var
  c, s : TArray<String>;
  i, j : integer;
  ok : boolean;
begin
  if (code = '') then
    exit(false);
  if (code = spec) then
    exit(true);
  c := code.split(['-']);
  s := spec.split(['-']);
  result := true;
  if c[0] <> s[0] then
    exit(false);
  if length(c) < length(s) then
    exit(false);
  for i := 1 to length(s) - 1 do
  begin
    ok := false;
    for j := 1 to length(c) - 1 do
      if s[i] = c[j] then
        ok := true;
    if (not ok) then
      exit(false);
  end;
end;

function THTTPLanguages.matches(code : String) : boolean;
var
  s : String;
begin
  result := false;
  for s in FCodes do
    if codeMatches(code, s) then
      exit(true);
end;


function THTTPLanguages.prefLang: String;
begin
  if (Length(FCodes) = 0) then
    result := 'en'
  else
    result := FCodes[0];
end;

function THTTPLanguages.sizeInBytes: cardinal;
var
  s : String;
begin
  result := sizeof(self);
  inc(result, (FSource.Length * SizeOf(char)) + 12);
  for s in FCodes do
    inc(result, (s.Length * SizeOf(char)) + 12);
end;

end.
