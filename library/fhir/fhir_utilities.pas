unit fhir_utilities;

{
Copyright (c) 2018+, Health Intersections Pty Ltd
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
  SysUtils, {$IFDEF FPC} zstream, {$ELSE} AnsiStrings, {$ENDIF} Classes, ZLib, Generics.Collections,
  fsl_base, fsl_utilities, fsl_stream, fsl_json, fsl_fpc, fsl_http, fsl_fetcher, fsl_versions, fsl_collections, fsl_logging,
  fhir_objects, fhir_uris;

function mimeTypeToFormat(mt : String; hack : boolean; def : TFHIRFormat = ffUnspecified) : TFHIRFormat;
function mimeTypeListToFormat(mt : String; hack : boolean; def : TFHIRFormat = ffUnspecified) : TFHIRFormat;
Function RecogniseFHIRFormat(Const sName : String; langList : THTTPLanguageList): TFHIRFormat;
Function FhirGUIDToString(aGuid : TGuid):String;
function IsId(s : String) : boolean;
function isHistoryURL(url : String) : boolean;
function isVersionUrl(url, t : String) : boolean;
procedure splitHistoryUrl(var url : String; var history : String);
procedure RemoveBOM(var s : String);
function CustomResourceNameIsOk(name : String) : boolean;
function Path(const parts : array of String) : String;
function UrlPath(const parts : array of String) : String;

function fullResourceUri(base: String; aType : string; id : String) : String; overload;
function fullResourceUri(base: String; url : String) : String; overload;

function hasProp(props : TArray<String>; name : String; def : boolean) : boolean;

type

  { TFHIRVersions }

  TFHIRVersions = class (TSemanticVersion)
  public
  //  class function getMajMinFromFHIRVersion(v : TFHIRVersion) : String; overload;
    class function readVersion(s : String) : TFHIRVersion;

    class function versionMatches(va : TFHIRVersionAlgorithm; criteria, candidate: String): boolean;
    class function guessVersionFormat(v : String) : TFHIRVersionAlgorithm;
    class function isLaterVersion(va1, va2 : TFHIRVersionAlgorithm; v1, v2 : String) : boolean;
    class function isSubset(ver1, ver2 : String) : boolean;
  end;

  TResourceWithReference = class (TFslObject)
  private
    FReference: String;
    FResource: TFHIRResourceV;
    procedure SetResource(const Value: TFHIRResourceV);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(reference : String; resource : TFHIRResourceV);
    destructor Destroy; override;
    property Reference : String read FReference write FReference;
    property Resource : TFHIRResourceV read FResource write SetResource;
  end;

const
  MIN_DATE = DATETIME_MIN;
  MAX_DATE = DATETIME_MAX;

function DetectFormat(oContent : TStream) : TFHIRFormat; overload;
function DetectFormat(bytes : TBytes) : TFHIRFormat; overload;
function DetectFormat(oContent : TFslBuffer) : TFHIRFormat; overload;
function csName(url : string) : String;

function csUriForProperty(code : String) : String;

type
  { TFslWordStemmer }

  TFslWordStemmer = class (TFslObject)
  private
    // FStem : TYuStemmer;
  public
    constructor Create(lang : String);
    destructor Destroy; override;
    function stem(word : String) : String;

    procedure stems(content : String; stems : TFslStringList; lang, defLang : String);
  end;




implementation

function mimeTypeListToFormat(mt : String; hack : boolean; def : TFHIRFormat = ffUnspecified) : TFHIRFormat;
var
  ctl : TFslList<TMimeContentType>;
  ct : TMimeContentType;
begin
  result := ffUnspecified;
  ctl := TMimeContentType.parseList(mt, hack);
  try
    for ct in ctl do
    begin
      if      (ct.base = 'application/json') or (ct.base = 'application/fhir+json') or (ct.base = 'application/json+fhir') then result := ffJson
      else if (ct.base = 'application/xml') or (ct.base = 'application/fhir+xml') or (ct.base = 'application/xml+fhir') then result := ffXml
      else if (ct.base = 'application/jquery') then result := ffJQuery
      else if (ct.base = 'application/x-ndjson') or (ct.base = 'application/fhir+ndjson') then result := ffNDJson
      else if (ct.base = 'text/turtle') or (ct.base = 'application/fhir+turtle') then result := ffTurtle

      else if (ct.base = 'text/json') then result := ffJson
      else if (ct.base = 'text/html') then result := ffXhtml
      else if (ct.base = 'text/xml') then result := ffXml
      else if (ct.base = 'application/x-zip-compressed') or (ct.base = 'application/zip') then result := ffXhtml
      else if (ct.base = 'text/plain') then result := ffText

      else if StringExistsInsensitive(ct.base, 'json') then result := ffJson
      else if StringExistsInsensitive(ct.base, 'jquery') then result := ffJQuery
      else if StringExistsInsensitive(ct.base, 'xml') then result := ffXml
      else if StringExistsInsensitive(ct.base, 'html') then result := ffXhtml
      else if StringExistsInsensitive(ct.base, 'text') then result := ffText
      else if StringExistsInsensitive(ct.base, 'rdf') then result := ffTurtle
      else if StringExistsInsensitive(ct.base, 'turtle') then result := ffTurtle
      else if StringExistsSensitive(ct.base, '*/*') Then result := ffXhtml;
      if result <> ffUnspecified then
        break;
    end;
  finally
    ctl.free;
  end;
  if result = ffUnspecified then
    result := def;
end;

function mimeTypeToFormat(mt : String; hack : boolean; def : TFHIRFormat = ffUnspecified) : TFHIRFormat;
var
  ct : TMimeContentType;
begin
  result := def;
  ct := TMimeContentType.parseSingle(mt, hack);
  try
    if      (ct.base = 'application/json') or (ct.base = 'application/fhir+json') or (ct.base = 'application/json+fhir') then result := ffJson
    else if (ct.base = 'application/xml') or (ct.base = 'application/fhir+xml') or (ct.base = 'application/xml+fhir') then result := ffXml
    else if (ct.base = 'application/x-ndjson') or (ct.base = 'application/fhir+ndjson') then result := ffNDJson
    else if (ct.base = 'text/turtle') or (ct.base = 'application/fhir+turtle') then result := ffTurtle

    else if (ct.base = 'text/json') then result := ffJson
    else if (ct.base = 'text/html') then result := ffXhtml
    else if (ct.base = 'text/xml') then result := ffXml
    else if (ct.base = 'application/x-zip-compressed') or (ct.base = 'application/zip') then result := ffXhtml
    else if (ct.base = 'text/plain') then result := ffText

    else if StringExistsInsensitive(ct.base, 'json') then result := ffJson
    else if StringExistsInsensitive(ct.base, 'xml') then result := ffXml
    else if StringExistsInsensitive(ct.base, 'html') then result := ffXhtml
    else if StringExistsInsensitive(ct.base, 'text') then result := ffText
    else if StringExistsInsensitive(ct.base, 'rdf') then result := ffTurtle
    else if StringExistsInsensitive(ct.base, 'turtle') then result := ffTurtle
    else if StringExistsSensitive(ct.base, '*/*') Then result := ffXhtml;
  finally
    ct.free;
  end;
end;

Function RecogniseFHIRFormat(Const sName : String; langList : THTTPLanguageList): TFHIRFormat;
Begin
  if (sName = '.xml') or (sName = 'xml') or (sName = '.xsd') or (sName = 'xsd') Then
    result := ffXml
  else if (sName = '.json') or (sName = 'json') then
    result := ffJson
  else if sName = '' then
    result := ffUnspecified
  else
    raise ERestfulException.Create('fhir_objects.RecogniseFHIRFormat', HTTP_ERR_BAD_REQUEST, itStructure, 'Unknown format '+sName, langList);
End;

Function FhirGUIDToString(aGuid : TGuid):String;
begin
  result := Copy(GUIDToString(aGuid), 2, 34).ToLower;
end;


function IsId(s : String) : boolean;
var
  i : integer;
begin
  result := length(s) in [1..ID_LENGTH];
  if result then
    for i := 1 to length(s) do
      result := result and CharInset(s[i], ['0'..'9', 'a'..'z', 'A'..'Z', '-', '.', '_']);
end;



function isHistoryURL(url : String) : boolean;
begin
  result := url.Contains('/_history/') and IsId(url.Substring(url.IndexOf('/_history/')+10));
end;

procedure splitHistoryUrl(var url : String; var history : String);
begin
  history := url.Substring(url.IndexOf('/_history/')+10);
  url := url.Substring(0, url.IndexOf('/_history/'));
end;

procedure RemoveBOM(var s : String);
begin
  if s.startsWith(#$FEFF) then
    s := s.substring(1);
end;

function CustomResourceNameIsOk(name : String) : boolean;
var
  fetcher : TInternetFetcher;
  json : TJsonObject;
  stream : TFileStream;
  n : TJsonNode;
begin
  result := false;
  fetcher := TInternetFetcher.Create;
  try
    fetcher.URL := 'http://www.healthintersections.com.au/resource-policy.json';
    fetcher.Fetch;
//    fetcher.Buffer.SaveToFileName(filePath(['[tmp]', 'test.json']));
    stream := TFileStream.Create(filePath(['[tmp]', 'test.json']), fmOpenRead + fmShareDenyWrite);
    try
//      fetcher.Buffer.SaveToStream(stream);
//      stream.Position := 0;
      json := TJSONParser.Parse(stream);
      try
        for n in json.arr['prefixes'] do
          if name.StartsWith(TJsonString(n).value) then
            exit(true);
        for n in json.arr['names'] do
          if name = TJsonString(n).value then
            exit(true);
      finally
        json.free;
      end;
    finally
      stream.free;
    end;
  finally
    fetcher.free;
  end;
end;


function Path(const parts : array of String) : String;
var
  i, j : integer;
  p : TArray<String>;
begin
  if length(parts) = 0 then
    result := ''
  else
    result := parts[0];
  for i := 1 to high(parts) do
  begin
    if parts[i].contains('/') or parts[i].contains('\') then
    begin
      p := parts[i].split(['\', '/']);
      for j := 0 to high(p) do
        result := IncludeTrailingPathDelimiter(result) + p[j];
    end
    else
      result := IncludeTrailingPathDelimiter(result) + parts[i];
  end;
end;

function IsSlash(const S: string; Index: Integer): Boolean;
begin
  Result := (Index >= Low(string)) and (Index <= High(S)) and (S[Index] = '/') and (ByteType(S, Index) = mbSingleByte);
end;

function IncludeTrailingSlash(const S: string): string;
begin
  Result := S;
  if not IsSlash(Result, High(Result)) then
    Result := Result + PathDelim;
end;

function IncludeTrailingURLSlash(const S: string): string;
begin
  Result := S;
  if not IsSlash(Result, High(Result)) then
    Result := Result + '/';
end;

function RemoveLeadingURLSlash(s : String) : String;
begin
  if s.StartsWith('/') then
    result := s.Substring(1)
  else
    result := s;
end;

function UrlPath(const parts : array of String) : String;
var
  i : integer;
begin
  if length(parts) = 0 then
    result := ''
  else
    result := parts[0];
  for i := 1 to high(parts) do
    result := IncludeTrailingURLSlash(result) + RemoveLeadingURLSlash(parts[i]);
end;


function isVersionUrl(url, t : String) : boolean;
begin
  result := (url.Length = (44 + t.Length)) and
    url.StartsWith('http://hl7.org/fhir/') and
    url.EndsWith('/StructureDefinition/'+t) and
    StringArrayExistsSensitive(PF_CONST, url.Substring(20, 3));
end;

function hasProp(props : TArray<String>; name : String; def : boolean) : boolean;
begin
  if (props = nil) or (length(props) = 0) then
    result := def
  else
    result := StringArrayExistsSensitive(props, name);
end;

{ TResourceWithReference }

constructor TResourceWithReference.Create(reference: String; resource: TFHIRResourceV);
begin
  inherited Create;
  self.Reference := reference;
  self.Resource := resource;

end;

destructor TResourceWithReference.Destroy;
begin
  FResource.free;
  inherited;
end;

procedure TResourceWithReference.SetResource(const Value: TFHIRResourceV);
begin
  FResource.free;
  FResource := Value;
end;

function fullResourceUri(base: String; aType : string; id : String) : String;
begin
  if (base = 'urn:oid:') then
  begin
    if isOid(id) then
      result := base+id
    else
      raise EFHIRException.Create('The resource id "'+'" has a base of "urn:oid:" but is not a valid OID');
  end
  else if (base = 'urn:uuid:') then
  begin
    if isGuid(id) then
      result := base+id
    else
      raise EFHIRException.Create('The resource id "'+id+'" has a base of "urn:uuid:" but is not a valid UUID');
  end
  else if not base.StartsWith('http://') and not base.StartsWith('https://')  then
    raise EFHIRException.Create('The resource base of "'+base+'" is not understood')
  else
    result := AppendForwardSlash(base)+aType+'/'+id;
end;

function fullResourceUri(base: String; url : String) : String; overload;
begin
  if url = '' then
    result := ''
  else if url.StartsWith('urn:oid:') or url.StartsWith('urn:uuid:') or url.StartsWith('http://') or url.StartsWith('https://') then
    result := url
  else if not base.StartsWith('http://') and not base.StartsWith('https://')  then
    raise EFHIRException.Create('The resource base of "'+base+'" is not understood')
  else
    result := AppendForwardSlash(base)+url;
end;



function DetectFormat(oContent : TStream) : TFHIRFormat; overload;
var
  i : integer;
  s : AnsiString;
begin
  i := oContent.Position;
  setlength(s, ocontent.Size - oContent.Position);
  ocontent.Read(s[1], length(s));
  oContent.Position := i;
  if ({$IFNDEF FPC}{$IFNDEF FPC}AnsiStrings.{$ENDIF}{$ENDIF}AnsiPos('<', s) > 0) and (({$IFNDEF FPC}AnsiStrings.{$ENDIF}AnsiPos('<', s) < 10)) then
    result := ffXml
  else if ({$IFNDEF FPC}AnsiStrings.{$ENDIF}AnsiPos('{', s) > 0) and (({$IFNDEF FPC}AnsiStrings.{$ENDIF}AnsiPos('{', s) < 10)) then
    result := ffJson
  else if ({$IFNDEF FPC}AnsiStrings.{$ENDIF}AnsiPos('@', s) > 0) and (({$IFNDEF FPC}AnsiStrings.{$ENDIF}AnsiPos('@', s) < 10)) then
    result := ffTurtle
  else
    result := ffUnspecified;
end;

function DetectFormat(bytes : TBytes) : TFHIRFormat; overload;
var
  sa : AnsiString;
  s : String;
begin
  setlength(sa, length(bytes));
  move(bytes[0], sa[1], length(sa));
  s := String(sa);
  if (pos('<', s) > 0) and ((pos('<', s) < 10)) then
    result := ffXml
  else
    result := ffJson;
end;


function DetectFormat(oContent : TFslBuffer) : TFHIRFormat; overload;
var
  s : String;
begin
  s := oContent.AsText;
  if (pos('<', s) > 0) and ((pos('<', s) < 10)) then
    result := ffXml
  else
    result := ffJson;

end;

function TResourceWithReference.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, (FReference.length * sizeof(char)) + 12);
  inc(result, FResource.sizeInBytes(magic));
end;

class function TFHIRVersions.readVersion(s: String): TFHIRVersion;
begin
  if (s.contains('(')) then
    s := s.substring(0, s.indexof('(')).trim;
  s := s.ToLower;
  if (s.length > 3) then
    s := s.substring(0, 3);
  if (s = 'r5') or s.startsWith('5.0') then
    result := fhirVersionRelease5
  else if (s = 'r4b') or s.startsWith('4.3') then
      result := fhirVersionRelease5
  else if (s = 'r4') or s.startsWith('4.0') then
    result := fhirVersionRelease4
  else if (s = 'r3') or s.startsWith('3.0') then
    result := fhirVersionRelease3
  else if (s = 'r2') or s.startsWith('1.0') then
    result := fhirVersionRelease2
  else
    result := fhirVersionUnknown;
end;

class function TFHIRVersions.guessVersionFormat(v : String) : TFHIRVersionAlgorithm;
var
  i, dotCount, dashCount, digitCount, letterCount: Integer;
  hasDigits, hasLetters, hasDots, hasDashes: Boolean;
  firstChar, lastChar: Char;
  yearPart, monthPart, dayPart: string;
  tempStr: string;

  function IsDigit(c: Char): Boolean;
  begin
    Result := (c >= '0') and (c <= '9');
  end;

  function IsLetter(c: Char): Boolean;
  begin
    Result := ((c >= 'A') and (c <= 'Z')) or ((c >= 'a') and (c <= 'z'));
  end;

  function IsValidDatePart(const part: string; minVal, maxVal: Integer): Boolean;
  var
    v, code, i: Integer;
  begin
    Result := False;
    if (Length(part) = 0) or (Length(part) > 4) then Exit;

    // Check all digits
    for i := 1 to Length(part) do
      if not IsDigit(part[i]) then Exit;

    Val(part, v, code);
    Result := (code = 0) and (v >= minVal) and (v <= maxVal);
  end;

  function CheckDateFormat: Boolean;
  var
    dashPos1, dashPos2, i: Integer;
  begin
    Result := False;

    // Look for YYYY-MM-DD format
    if (Length(v) >= 4) and (dashCount > 0) then
    begin
      dashPos1 := Pos('-', v);
      if dashPos1 = 5 then // Year should be 4 digits
      begin
        yearPart := Copy(v, 1, 4);
        if IsValidDatePart(yearPart, 1000, 9999) then
        begin
          if Length(v) = 4 then // Just YYYY
            Result := True
          else if dashPos1 = Length(v) then // YYYY- (partial)
            Result := True
          else
          begin
            dashPos2 := Pos('-', v, dashPos1 + 1);
            if dashPos2 = 0 then
            begin
              // YYYY-MM format
              monthPart := Copy(v, 6, Length(v) - 5);
              Result := IsValidDatePart(monthPart, 1, 12);
            end
            else if dashPos2 = dashPos1 + 3 then // YYYY-MM-DD
            begin
              monthPart := Copy(v, 6, 2);
              dayPart := Copy(v, 9, Length(v) - 8);
              Result := IsValidDatePart(monthPart, 1, 12) and IsValidDatePart(dayPart, 1, 31);
            end;
          end;
        end;
      end;
    end
    // Check YYYYMMDD format
    else if (dashCount = 0) and (Length(v) >= 4) and (Length(v) <= 8) then
    begin
      // All digits for date format
      for i := 1 to Length(v) do
        if not IsDigit(v[i]) then Exit;

      if Length(v) = 4 then // YYYY
        Result := IsValidDatePart(v, 1000, 9999)
      else if Length(v) = 6 then // YYYYMM
      begin
        yearPart := Copy(v, 1, 4);
        monthPart := Copy(v, 5, 2);
        Result := IsValidDatePart(yearPart, 1000, 9999) and IsValidDatePart(monthPart, 1, 12);
      end
      else if Length(v) = 8 then // YYYYMMDD
      begin
        yearPart := Copy(v, 1, 4);
        monthPart := Copy(v, 5, 2);
        dayPart := Copy(v, 7, 2);
        Result := IsValidDatePart(yearPart, 1000, 9999) and
                  IsValidDatePart(monthPart, 1, 12) and
                  IsValidDatePart(dayPart, 1, 31);
      end;
    end;
  end;

  function CheckSemverFormat: Boolean;
  var
    parts: array[1..3] of string;
    partIndex, partStart, j, i: Integer;
    currentPart: string;
  begin
    Result := False;

    // Must have exactly 2 dots for basic semver (major.minor.patch)
    if dotCount <> 2 then Exit;

    // Split by dots and validate each part
    partIndex := 1;
    partStart := 1;

    for i := 1 to Length(v) do
    begin
      if (v[i] = '.') or (i = Length(v)) then
      begin
        if i = Length(v) then
          currentPart := Copy(v, partStart, i - partStart + 1)
        else
          currentPart := Copy(v, partStart, i - partStart);

        if partIndex > 3 then Exit; // Too many parts

        // Each part should be numeric (allowing leading zeros for now), or a wildcard
        if Length(currentPart) = 0 then Exit;
        if not (StringArrayExists(['*', 'x', 'X'], currentPart)) then
        begin
          for j := 1 to Length(currentPart) do
            if not IsDigit(currentPart[j]) then Exit;
        end;

        parts[partIndex] := currentPart;
        Inc(partIndex);
        partStart := i + 1;
      end;
    end;

    Result := (partIndex = 4); // Should have exactly 3 parts
  end;

begin
  Result := vaUnknown;

  if Length(v) = 0 then Exit;

  // Initialize counters
  dotCount := 0;
  dashCount := 0;
  digitCount := 0;
  letterCount := 0;
  hasDigits := False;
  hasLetters := False;
  hasDots := False;
  hasDashes := False;

  firstChar := v[1];
  lastChar := v[Length(v)];

  // Count character types
  for i := 1 to Length(v) do
  begin
    if IsDigit(v[i]) then
    begin
      Inc(digitCount);
      hasDigits := True;
    end
    else if IsLetter(v[i]) then
    begin
      Inc(letterCount);
      hasLetters := True;
    end
    else if v[i] = '.' then
    begin
      Inc(dotCount);
      hasDots := True;
    end
    else if v[i] = '-' then
    begin
      Inc(dashCount);
      hasDashes := True;
    end;
  end;

  // Check for plain integer first (simplest case)
  if (digitCount = Length(v)) and (Length(v) > 0) then
  begin
    Result := vaInteger;
    Exit;
  end;

  // Check for date format
  if CheckDateFormat then
  begin
    Result := vaDate;
    Exit;
  end;

  // Check for semver format
  if CheckSemverFormat then
  begin
    Result := vaSemver;
    Exit;
  end;

  // Check for natural version (contains digits and has some version-like structure)
  if hasDigits and ((hasDots and (dotCount <= 4)) or
     (hasLetters and (letterCount <= digitCount * 2))) then
  begin
    // Basic heuristic: looks like it could be a natural version
    // Contains digits, maybe some dots, maybe some letters but not too many
    Result := vaNatural;
    Exit;
  end;

  // Default case
  Result := vaUnknown;
end;

class function TFHIRVersions.isLaterVersion(va1, va2: TFHIRVersionAlgorithm; v1, v2: String): boolean;
var
  i1, i2 : integer;
begin
  if va1 = vaUnknown then
    va1 := guessVersionFormat(v1);
  if va2 = vaUnknown then
    va2 := guessVersionFormat(v2);
  if (va1 <> va2) then
    va1 := vaNatural;
  case va1 of
    vaSemver : result := TSemanticVersion.isMoreRecent(v2, v1);
    vaInteger :
      begin
        result := false;
        i1 := StrToIntDef(v1, 0);
        i2 := StrToIntDef(v2, 0);
        result := i2 > i1;
      end;
  else
    // vaDate, vaNatural, vaUnknown, vaAlpha
    result := StringCompare(v1, v2) < 0;
  end;

end;

type
  TSemVer = record
    Major, Minor, Patch: Integer;
    PreRelease, Build: string;
    HasMinor, HasPatch: Boolean;
  end;

function Trim(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] = ' ') do Inc(I);
  if I > L then
    Result := ''
  else
  begin
    while (L > 0) and (S[L] = ' ') do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function StrToIntDef(const S: string; Default: Integer): Integer;
var
  Code: Integer;
begin
  Val(S, Result, Code);
  if Code <> 0 then
    Result := Default;
end;

function ParseSemVer(const Version: string): TSemVer;
var
  WorkStr, Part: string;
  DotPos, DashPos, PlusPos: Integer;
  Parts: array[0..2] of string;
  PartCount, I: Integer;
begin
  // Initialize result
  Result.Major := 0;
  Result.Minor := 0;
  Result.Patch := 0;
  Result.PreRelease := '';
  Result.Build := '';
  Result.HasMinor := False;
  Result.HasPatch := False;

  WorkStr := Trim(Version);
  if WorkStr = '' then Exit;

  // Extract build metadata (after +)
  PlusPos := Pos('+', WorkStr);
  if PlusPos > 0 then
  begin
    Result.Build := Copy(WorkStr, PlusPos + 1, Length(WorkStr));
    WorkStr := Copy(WorkStr, 1, PlusPos - 1);
  end;

  // Extract prerelease (after -)
  DashPos := Pos('-', WorkStr);
  if DashPos > 0 then
  begin
    Result.PreRelease := Copy(WorkStr, DashPos + 1, Length(WorkStr));
    WorkStr := Copy(WorkStr, 1, DashPos - 1);
  end;

  // Parse version parts (major.minor.patch)
  PartCount := 0;
  I := 1;
  while (I <= Length(WorkStr)) and (PartCount < 3) do
  begin
    DotPos := Pos('.', Copy(WorkStr, I, Length(WorkStr)));
    if DotPos > 0 then
    begin
      Parts[PartCount] := Copy(WorkStr, I, DotPos - 1);
      I := I + DotPos;
      Inc(PartCount);
    end
    else
    begin
      Parts[PartCount] := Copy(WorkStr, I, Length(WorkStr));
      Inc(PartCount);
      Break;
    end;
  end;

  // Assign parsed parts - only set HasMinor/HasPatch if actually present in string
  if PartCount >= 1 then
    Result.Major := StrToIntDef(Parts[0], 0);
  if PartCount >= 2 then
  begin
    Result.HasMinor := True;
    if (Parts[1] <> '*') and (Parts[1] <> 'x') and (Parts[1] <> 'X') then
      Result.Minor := StrToIntDef(Parts[1], 0);
  end;
  if PartCount >= 3 then
  begin
    Result.HasPatch := True;
    if (Parts[2] <> '*') and (Parts[2] <> 'x') and (Parts[2] <> 'X') then
      Result.Patch := StrToIntDef(Parts[2], 0);
  end;
end;

function IsWildcard(const S: string): Boolean;
begin
  Result := (S = '*') or (S = 'x') or (S = 'X');
end;

function HasQuestionMark(const Version: string): Boolean;
begin
  Result := Pos('?', Version) > 0;
end;

function GetVersionPrefix(const Version: string): string;
var
  QPos: Integer;
begin
  QPos := Pos('?', Version);
  if QPos > 0 then
    Result := Copy(Version, 1, QPos - 1)
  else
    Result := Version;
end;

function StartsWith(const S, Prefix: string): Boolean;
begin
  Result := (Length(S) >= Length(Prefix)) and
            (Copy(S, 1, Length(Prefix)) = Prefix);
end;

function GetVersionPart(const Version: string; PartIndex: Integer): string;
var
  WorkStr, Part: string;
  DotPos, DashPos, PlusPos: Integer;
  PartCount, I: Integer;
begin
  Result := '';
  WorkStr := Trim(Version);

  // Remove build metadata and prerelease for parsing core version
  PlusPos := Pos('+', WorkStr);
  if PlusPos > 0 then
    WorkStr := Copy(WorkStr, 1, PlusPos - 1);
  DashPos := Pos('-', WorkStr);
  if DashPos > 0 then
    WorkStr := Copy(WorkStr, 1, DashPos - 1);

  // Parse version parts
  PartCount := 0;
  I := 1;
  while (I <= Length(WorkStr)) and (PartCount <= PartIndex) do
  begin
    DotPos := Pos('.', Copy(WorkStr, I, Length(WorkStr)));
    if DotPos > 0 then
    begin
      Part := Copy(WorkStr, I, DotPos - 1);
      if PartCount = PartIndex then
      begin
        Result := Part;
        Exit;
      end;
      I := I + DotPos;
      Inc(PartCount);
    end
    else
    begin
      Part := Copy(WorkStr, I, Length(WorkStr));
      if PartCount = PartIndex then
        Result := Part;
      Break;
    end;
  end;
end;

function SemVerVersionMatches(const Criteria, Candidate: string): Boolean;
var
  CriteriaSV, CandidateSV: TSemVer;
  CriteriaPrefix: string;
  MajorPart, MinorPart, PatchPart: string;
begin
  Result := False;

  // Handle question mark wildcard - prefix matching
  if HasQuestionMark(Criteria) then
  begin
    CriteriaPrefix := GetVersionPrefix(Criteria);
    Result := StartsWith(Candidate, CriteriaPrefix);
    Exit;
  end;

  // Parse both versions
  CriteriaSV := ParseSemVer(Criteria);
  CandidateSV := ParseSemVer(Candidate);

  // Get original string parts for wildcard checking
  MajorPart := GetVersionPart(Criteria, 0);
  MinorPart := GetVersionPart(Criteria, 1);
  PatchPart := GetVersionPart(Criteria, 2);

  // Check major version - must always match if not wildcard
  if IsWildcard(MajorPart) then
  begin
    Result := True; // Major wildcard matches everything
    Exit;
  end;

  if CriteriaSV.Major <> CandidateSV.Major then
    Exit;

  // Check minor version
  if not CriteriaSV.HasMinor then
  begin
    // Criteria has no minor part (e.g., "1" vs "1.0.0")
    // This should only match if candidate also has no minor part
    Result := not CandidateSV.HasMinor;
    Exit;
  end;

  if IsWildcard(MinorPart) then
  begin
    // Minor wildcard (e.g., "2.*" or "2.x.x")
    if not CriteriaSV.HasPatch then
    begin
      // "2.*" matches "2.0", "2.1" but not "2.0.0"
      Result := CandidateSV.HasMinor and not CandidateSV.HasPatch;
    end
    else
    begin
      // "2.x.x" matches "2.0.1", "2.1.0" but not "2.0"
      Result := CandidateSV.HasMinor and CandidateSV.HasPatch and
                (CandidateSV.PreRelease = '') and (CandidateSV.Build = '');
    end;
    Exit;
  end;

  // Criteria has minor, candidate must also have minor and they must match
  if not CandidateSV.HasMinor then
    Exit;

  if CriteriaSV.Minor <> CandidateSV.Minor then
    Exit;

  // Check patch version
  if not CriteriaSV.HasPatch then
  begin
    // Criteria has no patch part (e.g., "2.0" vs "2.0.0")
    // This should only match if candidate also has no patch part
    Result := not CandidateSV.HasPatch;
    Exit;
  end;

  if IsWildcard(PatchPart) then
  begin
    // Patch wildcard (e.g., "2.0.*")
    Result := CandidateSV.HasPatch and
              (CandidateSV.PreRelease = '') and (CandidateSV.Build = '');
    Exit;
  end;

  // Criteria has patch, candidate must also have patch and they must match
  if not CandidateSV.HasPatch then
    Exit;

  if CriteriaSV.Patch <> CandidateSV.Patch then
    Exit;

  // Check prerelease
  if Pos('-*', Criteria) > 0 then
  begin
    // Prerelease wildcard (e.g., "2.0.0-*")
    Result := (CandidateSV.PreRelease <> '') and (CandidateSV.Build = '');
    Exit;
  end;

  if CriteriaSV.PreRelease <> CandidateSV.PreRelease then
    Exit;

  // Check build metadata
  if Pos('+*', Criteria) > 0 then
  begin
    // Build wildcard (e.g., "2.0.0+*")
    Result := (CandidateSV.Build <> '') and (CandidateSV.PreRelease = '');
    Exit;
  end;

  // Exact match
  Result := CriteriaSV.Build = CandidateSV.Build;
end;

class function TFHIRVersions.versionMatches(va : TFHIRVersionAlgorithm; criteria, candidate: String): boolean;
begin
  if va = vaUnknown then
    va := guessVersionFormat(candidate);
  case va of
    vaSemver : result := SemVerVersionMatches(criteria, candidate);
    vaInteger : result := false;
  else
    // vaUnknown, vaAlpha, vaDate, vaNatural :
    Result := candidate.startsWith(criteria);
  end;
end;

class function TFHIRVersions.isSubset(ver1, ver2: String): boolean;
var
  V1SV, V2SV: TSemVer;
  V1Major, V1Minor, V1Patch: string;
  V2Major, V2Minor, V2Patch: string;
  V1HasQuestion, V2HasQuestion: Boolean;
  V1Prefix, V2Prefix: string;
  V1HasPreWild, V1HasBuildWild: Boolean;
  V2HasPreWild, V2HasBuildWild: Boolean;
begin
  Result := False;

  // Handle question mark wildcards first
  V1HasQuestion := HasQuestionMark(Ver1);
  V2HasQuestion := HasQuestionMark(Ver2);

  if V1HasQuestion and V2HasQuestion then
  begin
    // Both have question marks - ver2 subset of ver1 if ver2 prefix starts with ver1 prefix
    V1Prefix := GetVersionPrefix(Ver1);
    V2Prefix := GetVersionPrefix(Ver2);
    Result := StartsWith(V2Prefix, V1Prefix);
    Exit;
  end;

  if V1HasQuestion and not V2HasQuestion then
  begin
    // Ver1 has question mark, ver2 doesn't - ver2 subset if it starts with ver1 prefix
    V1Prefix := GetVersionPrefix(Ver1);
    Result := StartsWith(Ver2, V1Prefix);
    Exit;
  end;

  if not V1HasQuestion and V2HasQuestion then
  begin
    // Ver1 doesn't have question mark, ver2 does - can't be subset (ver2 is broader)
    Result := False;
    Exit;
  end;

  // Neither has question marks - parse normally
  V1SV := ParseSemVer(Ver1);
  V2SV := ParseSemVer(Ver2);

  V1Major := GetVersionPart(Ver1, 0);
  V1Minor := GetVersionPart(Ver1, 1);
  V1Patch := GetVersionPart(Ver1, 2);
  V2Major := GetVersionPart(Ver2, 0);
  V2Minor := GetVersionPart(Ver2, 1);
  V2Patch := GetVersionPart(Ver2, 2);

  // Check for prerelease and build wildcards
  V1HasPreWild := Pos('-*', Ver1) > 0;
  V1HasBuildWild := Pos('+*', Ver1) > 0;
  V2HasPreWild := Pos('-*', Ver2) > 0;
  V2HasBuildWild := Pos('+*', Ver2) > 0;

  // Major version check
  if IsWildcard(V1Major) then
  begin
    // Ver1 major is wildcard - matches everything, so ver2 is always subset
    Result := True;
    Exit;
  end;

  if IsWildcard(V2Major) then
  begin
    // Ver2 major is wildcard, Ver1 is not - ver2 is broader, not subset
    Result := False;
    Exit;
  end;

  // Both have specific major versions - they must match
  if V1SV.Major <> V2SV.Major then
  begin
    Result := False;
    Exit;
  end;

  // Minor version check
  if not V1SV.HasMinor then
  begin
    // Ver1 has no minor (e.g., "1") - only matches exact "1"
    // Ver2 can only be subset if it's also exact "1"
    Result := not V2SV.HasMinor;
    Exit;
  end;

  if IsWildcard(V1Minor) then
  begin
    // Ver1 minor is wildcard
    if not V1SV.HasPatch then
    begin
      // Ver1 is "X.*" - matches "X.Y" but not "X.Y.Z"
      if IsWildcard(V2Minor) then
      begin
        // Ver2 is also "X.*" - subset
        Result := not V2SV.HasPatch;
      end
      else
      begin
        // Ver2 is specific minor - subset if it has no patch
        Result := V2SV.HasMinor and not V2SV.HasPatch;
      end;
    end
    else
    begin
      // Ver1 is "X.x.x" - matches "X.Y.Z"
      if IsWildcard(V2Minor) then
      begin
        // Ver2 is also wildcard minor
        if V2SV.HasPatch then
        begin
          Result := IsWildcard(V2Patch); // Both "X.x.x"
        end
        else
        begin
          Result := False; // Ver2 "X.*" is broader than Ver1 "X.x.x"
        end;
      end
      else
      begin
        // Ver2 has specific minor.patch - subset
        Result := V2SV.HasMinor and V2SV.HasPatch;
      end;
    end;
    Exit;
  end;

  if not V2SV.HasMinor then
  begin
    // Ver1 has minor, Ver2 doesn't - Ver2 is broader
    Result := False;
    Exit;
  end;

  if IsWildcard(V2Minor) then
  begin
    // Ver1 has specific minor, Ver2 has wildcard - Ver2 is broader
    Result := False;
    Exit;
  end;

  // Both have specific minor versions - they must match
  if V1SV.Minor <> V2SV.Minor then
  begin
    Result := False;
    Exit;
  end;

  // Patch version check
  if not V1SV.HasPatch then
  begin
    // Ver1 has no patch (e.g., "1.2") - only matches exact "1.2"
    Result := not V2SV.HasPatch;
    Exit;
  end;

  if IsWildcard(V1Patch) then
  begin
    // Ver1 patch is wildcard "X.Y.*"
    if IsWildcard(V2Patch) then
    begin
      Result := True; // Both "X.Y.*"
    end
    else
    begin
      Result := V2SV.HasPatch; // Ver2 is specific patch - subset
    end;
    Exit;
  end;

  if not V2SV.HasPatch then
  begin
    // Ver1 has patch, Ver2 doesn't - Ver2 is broader
    Result := False;
    Exit;
  end;

  if IsWildcard(V2Patch) then
  begin
    // Ver1 has specific patch, Ver2 has wildcard - Ver2 is broader
    Result := False;
    Exit;
  end;

  // Both have specific patch versions - they must match
  if V1SV.Patch <> V2SV.Patch then
  begin
    Result := False;
    Exit;
  end;

  // Prerelease check
  if V1HasPreWild and V2HasPreWild then
  begin
    Result := True; // Both match any prerelease
    Exit;
  end;

  if V1HasPreWild and not V2HasPreWild then
  begin
    Result := V2SV.PreRelease <> ''; // Ver2 subset if it has specific prerelease
    Exit;
  end;

  if not V1HasPreWild and V2HasPreWild then
  begin
    Result := False; // Ver2 is broader
    Exit;
  end;

  // Build metadata check
  if V1HasBuildWild and V2HasBuildWild then
  begin
    Result := True; // Both match any build
    Exit;
  end;

  if V1HasBuildWild and not V2HasBuildWild then
  begin
    Result := V2SV.Build <> ''; // Ver2 subset if it has specific build
    Exit;
  end;

  if not V1HasBuildWild and V2HasBuildWild then
  begin
    Result := False; // Ver2 is broader
    Exit;
  end;

  // Exact prerelease and build match
  Result := (V1SV.PreRelease = V2SV.PreRelease) and (V1SV.Build = V2SV.Build);
end;


function csName(url : string) : String;
begin
  if url = '' then
    result := '[not stated]'
  else if url = URI_SNOMED then
    result := 'SNOMED CT'
  else if url = URI_LOINC then
    result := 'LOINC'
  else if url = 'http://www.nlm.nih.gov/research/umls/rxnorm"))' then
    result := 'RxNorm'
  else if url = URI_ICD9 then
    result := 'ICD-9'
  else if url = URI_ICD10 then
    result := 'ICD-10'
  else if url = 'http://id.who.int/icd/release/11/mms' then
    result := 'ICD-11'
  else if url = URI_DICOM then
    result := 'DICOM'
  else if url = URI_UCUM then
    result := 'UCUM'
  else if url = URI_BCP47 then
    result := 'lang'
  else if url = URI_BCP13 then
    result := 'mimetypes'
  else if url = URI_11073 then
    result := '11073'
  else if url = URI_DICOM then
    result := 'dicom'
  else if url = URI_CVX then
    result := 'CVX'
  else if url = URI_GTIN then
    result := 'GTIN'
  else if url = 'https://www.humanservices.gov.au/organisations/health-professionals/enablers/air-vaccine-code-formats' then
    result := 'AIR'
  else if url = 'http://www.whocc.no/atc' then
    result := 'ATC/DDD'

  else if url.StartsWith('http://hl7.org/fhir/v2') then
    result := 'V2-'+url.Substring(22)
  else if url.StartsWith('http://hl7.org/fhir/v3') then
    result := 'V3-'+url.Substring(22)
  else if url.StartsWith('http://hl7.org/fhir') then
    result := 'FHIR-'+url.Substring(19)
  else if url.StartsWith('urn:iso:std:iso:') then
    result := 'iso'+url.substring(16).replace(':', '')
  else if url.StartsWith('http://terminology.hl7.org/CodeSystem/') then
    result := url.substring(38).replace('/', '')
  else if url.StartsWith('http://hl7.org/fhir/') then
    result := url.substring(20).replace('/', '')

  else
    result := url;

end;

function csUriForProperty(code : String) : String;
begin
  if (code = 'status') then               result := 'http://hl7.org/fhir/concept-properties#status'
  else if (code = 'inactive') then        result := 'http://hl7.org/fhir/concept-properties#inactive'
  else if (code = 'effectiveDate') then   result := 'http://hl7.org/fhir/concept-properties#effectiveDate'
  else if (code = 'deprecationDate') then result := 'http://hl7.org/fhir/concept-properties#deprecationDate'
  else if (code = 'retirementDate') then  result := 'http://hl7.org/fhir/concept-properties#retirementDate'
  else if (code = 'notSelectable') then   result := 'http://hl7.org/fhir/concept-properties#notSelectable'
  else if (code = 'parent') then          result := 'http://hl7.org/fhir/concept-properties#parent'
  else if (code = 'child') then           result := 'http://hl7.org/fhir/concept-properties#child'
  else if (code = 'partOf') then          result := 'http://hl7.org/fhir/concept-properties#partOf'
  else if (code = 'synonym') then         result := 'http://hl7.org/fhir/concept-properties#synonym'
  else if (code = 'comment') then         result := 'http://hl7.org/fhir/concept-properties#comment'
  else if (code = 'itemWeight') then      result := 'http://hl7.org/fhir/concept-properties#itemWeight'
  else
    result := '';
end;


{ TFslWordStemmer }

constructor TFslWordStemmer.Create(lang: String);
begin
  inherited Create;
//  FStem := GetStemmer(lang);
end;

destructor TFslWordStemmer.Destroy;
begin
//  FStem.free;
  inherited;
end;

function TFslWordStemmer.stem(word: String): String;
begin
  result := EncodeNYSIIS(word); // temporary hack
  // result := FStem.Stem(word);
end;

procedure TFslWordStemmer.stems(content: String; stems: TFslStringList; lang, defLang: String);
var
  s, t : string;
begin
  content := content.replace('''', '');
  for s in content.Split([',', ' ', ':', '.', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '{', '}', '[', ']', '|', '\', ';', '"', '<', '>', '?', '/', '~', '`', '-', '_', '-', '+', '=']) do
  begin
    if (s <> '') And not StringIsInteger64(s) and (s.length > 2) Then
    begin
      t := Stem(s);
      if (t <> '') then
        stems.add(t.toLower);
    end;
  End;
end;

end.
