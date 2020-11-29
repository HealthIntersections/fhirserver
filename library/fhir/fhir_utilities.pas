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
  fsl_base, fsl_utilities, fsl_stream, fsl_json, fsl_fpc, fsl_http, fsl_fetcher,
  fhir_objects;

function mimeTypeToFormat(mt : String; def : TFHIRFormat = ffUnspecified) : TFHIRFormat;
function mimeTypeListToFormat(mt : String; def : TFHIRFormat = ffUnspecified) : TFHIRFormat;
Function RecogniseFHIRFormat(Const sName : String; const lang : THTTPLanguages): TFHIRFormat;
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
  TFHIRVersions = class (TSemVer)
  public
    class function getMajMin(v : TFHIRVersion) : String; overload;
  end;

  TResourceWithReference = class (TFslObject)
  private
    FReference: String;
    FResource: TFHIRResourceV;
    procedure SetResource(const Value: TFHIRResourceV);
  protected
    function sizeInBytesV : cardinal; override;
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


implementation

function mimeTypeListToFormat(mt : String; def : TFHIRFormat = ffUnspecified) : TFHIRFormat;
var
  ctl : TFslList<TMimeContentType>;
  ct : TMimeContentType;
begin
  result := ffUnspecified;
  ctl := TMimeContentType.parseList(mt);
  try
    for ct in ctl do
    begin
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
      if result <> ffUnspecified then
        break;
    end;
  finally
    ctl.Free;
  end;
  if result = ffUnspecified then
    result := def;
end;

function mimeTypeToFormat(mt : String; def : TFHIRFormat = ffUnspecified) : TFHIRFormat;
var
  ct : TMimeContentType;
begin
  result := def;
  ct := TMimeContentType.parseSingle(mt);
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
    ct.Free;
  end;
end;

Function RecogniseFHIRFormat(Const sName : String; const lang : THTTPLanguages): TFHIRFormat;
Begin
  if (sName = '.xml') or (sName = 'xml') or (sName = '.xsd') or (sName = 'xsd') Then
    result := ffXml
  else if (sName = '.json') or (sName = 'json') then
    result := ffJson
  else if sName = '' then
    result := ffUnspecified
  else
    raise ERestfulException.create('fhir_objects.RecogniseFHIRFormat', HTTP_ERR_BAD_REQUEST, itStructure, 'Unknown format '+sName, lang);
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
  fetcher := TInternetFetcher.create;
  try
    fetcher.URL := 'http://www.healthintersections.com.au/resource-policy.json';
    fetcher.Fetch;
//    fetcher.Buffer.SaveToFileName('c:\temp\test.json');
    stream := TFileStream.Create('c:\temp\test.json', fmOpenRead + fmShareDenyWrite);
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
        json.Free;
      end;
    finally
      stream.Free;
    end;
  finally
    fetcher.Free;
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
      raise EFHIRException.create('The resource id "'+'" has a base of "urn:oid:" but is not a valid OID');
  end
  else if (base = 'urn:uuid:') then
  begin
    if isGuid(id) then
      result := base+id
    else
      raise EFHIRException.create('The resource id "'+id+'" has a base of "urn:uuid:" but is not a valid UUID');
  end
  else if not base.StartsWith('http://') and not base.StartsWith('https://')  then
    raise EFHIRException.create('The resource base of "'+base+'" is not understood')
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
    raise EFHIRException.create('The resource base of "'+base+'" is not understood')
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

function TResourceWithReference.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FReference.length * sizeof(char)) + 12);
  inc(result, FResource.sizeInBytes);
end;

class function TFHIRVersions.getMajMin(v: TFHIRVersion): String;
begin
  result := getMajMin(FHIR_VERSIONS[v])
end;


function csName(url : string) : String;
begin
  if url.StartsWith('http://hl7.org/fhir/v2') then
    result := 'V2-'+url.Substring(22)
  else if url.StartsWith('http://hl7.org/fhir/v3') then
    result := 'V3-'+url.Substring(22)
  else if url.StartsWith('http://hl7.org/fhir') then
    result := 'FHIR'+url.Substring(19)
  else if url = 'http://snomed.info/sct' then
    result := 'SNOMED CT'
  else if url = 'http://loinc.org' then
    result := 'LOINC'
  else
    result := url;
end;


end.
