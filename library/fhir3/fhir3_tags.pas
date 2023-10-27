unit fhir3_tags;

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
  SysUtils, Classes,
  fsl_base, fsl_json, fsl_stream,
  fhir_objects, fhir_uris,
  fhir3_types;

const
  TAG_FHIR_SYSTEM = 'http://healthintersections.com.au/fhir/tags';
  TAG_TEST_SYSTEM = TAG_FHIR_SYSTEM;
  TAG_TEST_CODE = 'for-testing';
//  TAG_FHIR_SYSTEM_PROFILES = 'http://healthintersections.com.au/fhir/profiles'; // code will be a uri

//  TAG_READONLY = 'read-only';
//  TAG_SUMMARY = 'summary';


  TAG_COMPARTMENT_IN = 'patient-compartment';
  TAG_COMPARTMENT_OUT = 'patient-compartment-not';
//  TAG_USER_COMPARTMENT = 'patient-compartment-user';

type
  TFHIRTagCategory = (tcTag, tcSecurity, tcProfile);

  TFHIRTag = class (TFHIRCoding)
  private
    FKey : integer;
    FCategory : TFHIRTagCategory;
    FTransactionId: String;
    FConfirmedStored: boolean;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    function Link : TFHIRTag;
    property Key : integer read Fkey write FKey;
    property Category : TFHIRTagCategory read FCategory write FCategory;

    // operational stuff to do with transaction scope management
    property TransactionId : String read FTransactionId write FTransactionId;
    property ConfirmedStored : boolean read FConfirmedStored write FConfirmedStored;
    function describe : String; override;
  end;

  TFHIRTagList = class (TFslObject)
  private
    FList : TFslList<TFHIRTag>;
    function GetCount: Integer;
    function GetTag(index: integer): TFHIRTag;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TFHIRTagList;
    procedure readTags(meta : TFhirMeta);
    procedure writeTags(meta : TFhirMeta);
    procedure deleteTags(meta : TFhirMeta);
    procedure removeTags(meta : TFhirMeta);
    Property Count : Integer read GetCount;
    Property Tag[index : integer] : TFHIRTag read GetTag; default;
    function json : TArray<byte>;
    function findTag(category : TFHIRTagCategory; system, code : String) : TFHIRTag;
    procedure removeTag(category : TFHIRTagCategory; system, code : String);
    function hasTag(category : TFHIRTagCategory; system, code : String) : boolean;
    function addTag(key : integer; kind : TFHIRTagCategory; system, code, display : String) : TFHIRTag;
    procedure add(tag : TFHIRTag);
    function hasTestingTag : boolean;
    procedure forceTestingTag;
    function asHeader : String;
    function describe : String;
  end;

implementation

uses
  fhir3_utilities;

{ TFHIRTag }

function TFHIRTag.describe: String;
begin
  result := inttostr(ord(FCategory))+':'+system+'::'+code;
end;

function TFHIRTag.Link: TFHIRTag;
begin
  result := TFHIRTag(inherited Link);
end;

function TFHIRTag.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, (FTransactionId.length * sizeof(char)) + 12);
end;

{ TFHIRTagList }

Constructor TFHIRTagList.Create;
begin
  inherited;
  FList := TFslList<TFHIRTag>.Create;
end;


procedure TFHIRTagList.deleteTags(meta: TFhirMeta);
var
  t : TFHIRTag;
begin
  for t in FList do
    case t.Category of
      tcTag: meta.tagList.RemoveCoding(t.system, t.code);
      tcSecurity: meta.securityList.RemoveCoding(t.system, t.code);
      tcProfile: meta.profileList.removeUri(t.code);
    end;
end;

function TFHIRTagList.describe: String;
var
  item : TFHIRTag;
begin
  result := '';
  for item in FList do
    result := result +', '+item.describe;
end;

Destructor TFHIRTagList.Destroy;
begin
  FList.free;
  inherited;
end;


function TFHIRTagList.Link: TFHIRTagList;
begin
  result := TFHIRTagList(inherited Link);
end;


procedure TFHIRTagList.add(tag: TFHIRTag);
begin
  FList.Add(tag);
end;

function TFHIRTagList.addTag(key: integer; kind: TFHIRTagCategory; system, code, display: String) : TFHIRTag;
var
  tag : TFHIRTag;
begin
  tag := TFHIRTag.Create;
  try
    tag.Key := Key;
    tag.Category := kind;
    tag.system := system;
    tag.code := code;
    tag.display := display;
    FList.Add(tag.Link);
    result := tag;
  finally
    tag.free;
  end;
end;

function TFHIRTagList.asHeader: String;
begin
end;

function TFHIRTagList.GetCount: Integer;
begin
  result := FList.Count;
end;

function TFHIRTagList.findTag(category : TFHIRTagCategory; system, code: String): TFHIRTag;
var
  t : TFHIRTag;
begin
  result := nil;
  for t in FList do
    if (t.Category = category) and (t.system = system) and (t.code = code) then
    begin
      result := t;
      exit;
    end;
end;

procedure TFHIRTagList.forceTestingTag;
begin
  if not hasTestingTag then
    addTag(0, tcTag, TAG_TEST_SYSTEM, TAG_TEST_CODE, 'For Testing Only');
end;

function TFHIRTagList.GetTag(index: integer): TFHIRTag;
begin
  result := FList[index];
end;

function TFHIRTagList.hasTag(category : TFHIRTagCategory; system, code: String): boolean;
begin
  result := findTag(category, system, code) <> nil;
end;

function TFHIRTagList.hasTestingTag: boolean;
begin
  result := hasTag(tcTag, TAG_TEST_SYSTEM, TAG_TEST_CODE);
end;

function TFHIRTagList.json: TArray<byte>;
var
  json : TJSONWriter;
  s : TBytesStream;
  vs : TFslVCLStream;
  t : TFHIRTag;
begin
  s := TBytesStream.Create;
  try
    vs := TFslVCLStream.Create;
    try
      vs.Stream := s;
      json := TJSONWriterDirect.Create;
      try
        json.Stream := vs.link;
        json.Start(true);
        json.HasWhitespace := false;
        json.ValueArray('tags');
        for t in FList do
        begin
          json.ValueObject();
          json.Value('key', t.Key);
          json.Value('category', ord(t.Category));
          json.Value('system', t.system);
          json.Value('code', t.code);
          json.Value('display', t.display);
          json.FinishObject;
        end;
        json.FinishArray;
        json.Finish(true);
      finally
        json.free;
      end;
    finally
      vs.free;
    end;
    result := s.Bytes;
  finally
    s.free;
  end;
end;

procedure TFHIRTagList.readTags(meta: TFhirMeta);
var
  c : TFHIRCoding;
  u : TFhirUri;
begin
  for c in meta.tagList do
    if not hasTag(tcTag, c.system, c.code) then
      addTag(0, tcTag, c.system, c.code, c.display);
  for c in meta.securityList do
    if not hasTag(tcSecurity, c.system, c.code) then
      addTag(0, tcSecurity, c.system, c.code, c.display);
  for u in meta.profileList do
    if not hasTag(tcProfile, URI_URIs, u.value) then
      addTag(0, tcProfile, URI_URIs, u.value, '');
end;

procedure TFHIRTagList.removeTag(category: TFHIRTagCategory; system, code: String);
var
  i : integer;
begin
  for i := FList.count - 1 downto 0 do
    if (flist[i].Category = category) and (flist[i].system = system) and (flist[i].code = code) then
      flist.Delete(i);
end;

procedure TFHIRTagList.removeTags(meta: TFhirMeta);
var
  c : TFHIRCoding;
  u : TFhirUri;
begin
  for c in meta.tagList do
    removeTag(tcTag, c.system, c.code);
  for c in meta.securityList do
    removeTag(tcSecurity, c.system, c.code);
  for u in meta.profileList do
    removeTag(tcProfile, URI_URIs, u.value);
end;

procedure TFHIRTagList.writeTags(meta: TFhirMeta);
var
  t : TFHIRTag;
begin
  meta.tagList.Clear;
  meta.securityList.Clear;
  meta.profileList.Clear;
  for t in FList do
    case t.Category of
      tcTag: meta.tagList.addCoding(t.system, t.code, t.display);
      tcSecurity: meta.securityList.addCoding(t.system, t.code, t.display);
      tcProfile: meta.profileList.Append.value := t.code;
    end;
end;


function TFHIRTagList.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FList.sizeInBytes(magic));
end;

end.
