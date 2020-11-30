unit fsl_npm_client;

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
   SysUtils, Classes,
   fsl_base, fsl_utilities, fsl_json, fsl_fetcher;

const
  PACKAGE_SERVER_PRIMARY = 'http://packages.fhir.org';
  PACKAGE_SERVER_BACKUP = 'https://packages2.fhir.org/packages';
  PACKAGE_SERVER_CIBUILD = 'https://build.fhir.org/ig/qas.json';

type

  { TFHIRPackageInfo }

  TFHIRPackageInfo = class (TFslObject)
  private
    FId : String;
    FSize: integer;
    FVersion : String;
    FFhirVersion : String;
    FDescription : String;
    FUrl : String;
    FDate: TDateTime;
    FCanonical : String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(id, version, fhirVersion, description, canonical, url : String); overload;
    constructor Create(id, version, fhirVersion, description, canonical, url : String; date : TDateTime); overload;
    function link : TFHIRPackageInfo; overload;
    property id : String read FId;
    property version : String read FVersion;
    property fhirVersion : String read FFhirVersion;
    property description : String read FDescription;
    property url : String read FUrl;
    property canonical : String read FCanonical;
    property date : TDateTime read FDate write FDate;
    function presentDate : String;
    property size : integer read FSize write FSize;
  end;

  TFHIRPackageClient = class (TFslObject)
  private
    FAddress : String;
    function readDate(df: String): TDateTime;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(address : String);
    property address : String read FAddress;
    function exists(id, ver : String) : boolean;
    function fetch(id, ver : String) : TBytes; overload;
    function fetch(info : TFHIRPackageInfo) : TBytes; overload;
    function fetchNpm(id, ver : String) : TBytes;
    function getVersions(id : String) : TFslList<TFHIRPackageInfo>;
    function search(name, canonical, fhirVersion : String; preRelease : boolean) : TFslList<TFHIRPackageInfo>;  overload;
    function search(lastCalled : TFslDateTime; packages : TFslList<TFHIRPackageInfo>) : TFslDateTime; overload;

    function fetchFromCIBuild : TFslList<TFHIRPackageInfo>;

    class procedure loadPackages(list : TFslList<TFHIRPackageInfo>; server : String; id : String);
    class procedure loadPackagesForVersion(list : TFslList<TFHIRPackageInfo>; server : String; ver : String);
  end;

implementation

{ TFHIRPackageClient }

constructor TFHIRPackageClient.Create(address: String);
begin
  inherited Create;
  FAddress := address;
end;

function TFHIRPackageClient.exists(id, ver: String): boolean;
begin
  result := false;
end;

function TFHIRPackageClient.fetch(info: TFHIRPackageInfo): TBytes;
begin
  result := TInternetFetcher.fetchUrl(URLPath([address, info.id, info.version]));
end;

function TFHIRPackageClient.fetchFromCIBuild: TFslList<TFHIRPackageInfo>;
var
  arr : TJsonArray;
  obj : TJsonObject;
  s : String;
begin
  result := TFslList<TFHIRPackageInfo>.create;
  try
    arr := TInternetFetcher.fetchJsonArray(FAddress);
    try
      for obj in arr.asObjects.forEnum do
      begin
        if obj['repo'].contains('/master') then
        begin
          s := obj['repo'];
          s := s.substring(0, s.indexof('/branches'));
          result.Add(TFHIRPackageInfo.Create(obj['package-id'], 'current'{obj['ig-ver']}, obj['version'], 'github: '+ obj['repo'], '', URLPath(['https://build.fhir.org/ig', s, 'package.tgz']), readDate(obj['date'])));
        end;
      end;
    finally
      arr.free;
    end;
    result.Link;
  finally
    result.free;
  end;
end;

function TFHIRPackageClient.fetch(id, ver: String): TBytes;
begin
  result := TInternetFetcher.fetchUrl(URLPath([address, id, ver]));
end;

function TFHIRPackageClient.fetchNpm(id, ver: String): TBytes;
begin
  result := TInternetFetcher.fetchUrl(URLPath([address, id, '-', id+'-'+ver+'.tgz']));
end;

function TFHIRPackageClient.getVersions(id: String): TFslList<TFHIRPackageInfo>;
var
  json, versions, obj : TJsonObject;
  s : String;
begin
  result := TFslList<TFHIRPackageInfo>.create;
  try
    try
      json := TInternetFetcher.fetchJson(URLPath([address, id]));
      try
        versions := json.obj['versions'];
        if (versions <> nil) then
        begin
          for s in versions.properties.SortedKeys do
          begin
            obj := versions.obj[s];
            result.add(TFHIRPackageInfo.create(obj['name'], obj['version'], obj['FhirVersion'], obj['description'], obj['canonical'], obj['url']));
          end;
        end;
      finally
        json.free;
      end;
    except
      on e : Exception do
      begin
        if not e.Message.Contains('Not Found') then
          raise;
      end;
    end;
    result.Link;
  finally
    result.free;
  end;
end;

function TFHIRPackageClient.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FAddress.length * sizeof(char)) + 12);
end;

class procedure TFHIRPackageClient.loadPackages(list : TFslList<TFHIRPackageInfo>; server, id: String);
var
  this : TFHIRPackageClient;
  l : TFslList<TFHIRPackageInfo>;
begin
  this := TFHIRPackageClient.Create(server);
  try
    try
      if (server.Contains('build.fhir.org')) then
        l := this.fetchFromCIBuild
      else if (id = '') then
        l := this.search('', '', '', false)
      else
        l := this.getVersions(id);
      try
        list.AddAll(l);
      finally
        l.free;
      end;
    except
      // suppress for now
    end;
  finally
    this.Free;
  end;
end;

class procedure TFHIRPackageClient.loadPackagesForVersion(list: TFslList<TFHIRPackageInfo>; server, ver: String);
var
  this : TFHIRPackageClient;
  l : TFslList<TFHIRPackageInfo>;
  i, t : TFHIRPackageInfo;
  found : boolean;
begin
  this := TFHIRPackageClient.Create(server);
  try
    try
      l := this.search('', '', ver, false);
      try
        for i in l do
        begin
          found := false;
          for t in list do
            if t.id = i.id then
              found := true;
          if not found then
            list.Add(i.link);
        end;
      finally
        l.free;
      end;
    except
      // suppress for now
    end;
  finally
    this.Free;
  end;
end;

function TFHIRPackageClient.search(name, canonical, fhirVersion: String; preRelease: boolean): TFslList<TFHIRPackageInfo>;
var
  b : TCommaBuilder;
  json : TJsonArray;
  e : TJsonNode;
  obj : TJsonObject;
begin
  b := TCommaBuilder.Create('&');
  try
    if (name <> '') then
      b.add('name='+name);
    if (canonical <> '') then
      b.add('canonical='+canonical);
    if (fhirVersion <> '') then
      b.add('fhirversion='+fhirVersion);
    if (preRelease) then
      b.add('prerelease='+BooleanToString(preRelease));
    result := TFslList<TFHIRPackageInfo>.create;
    try
      json := TInternetFetcher.fetchJsonArray(URLPath([address, 'catalog?'])+b.asString());
      try
        for e in json do
        begin
          obj := e as TJsonObject;
          result.add(TFHIRPackageInfo.create(obj.str2('Name', 'name'), obj.str2('Version', 'version'), obj.str2('FhirVersion', 'fhirVersion'),
                     obj.str2('Description', 'description'), obj['canonical'], obj['url']));
        end;
      finally
        json.free;
      end;
      result.link;
    finally
      result.Free;
    end;
  finally
    b.Free;
  end;
end;

function TFHIRPackageClient.search(lastCalled: TFslDateTime; packages: TFslList<TFHIRPackageInfo>): TFslDateTime;
begin
  result := TFslDateTime.makeNull;
end;

function TFHIRPackageClient.readDate(df: String): TDateTime;
begin
  result := trunc(TFslDateTime.fromFormat('xxx, dd mmm, yyyy hh:NN:ss', df).DateTime);
end;

{ TFHIRPackageInfo }

constructor TFHIRPackageInfo.Create(id, version, fhirVersion, description, canonical, url: String);
begin
  Inherited Create;
  FId := id;
  FVersion := version;
  FFhirVersion := fhirVersion;
  FDescription := description;
  FCanonical := canonical;
  FUrl := url;
end;

constructor TFHIRPackageInfo.Create(id, version, fhirVersion, description, canonical, url: String; date: TDateTime);
begin
  Inherited Create;
  FId := id;
  FVersion := version;
  FFhirVersion := fhirVersion;
  FDescription := description;
  FCanonical := canonical;
  FUrl := url;
  FDate := date;
end;

function TFHIRPackageInfo.link: TFHIRPackageInfo;
begin
  result := TFHIRPackageInfo(inherited link);
end;

function TFHIRPackageInfo.presentDate: String;
begin
  if FDate = 0 then
    result := ''
  else
    result := FormatDateTime('c', FDate)
end;

function TFHIRPackageInfo.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FId.length * sizeof(char)) + 12);
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, (FFhirVersion.length * sizeof(char)) + 12);
  inc(result, (FDescription.length * sizeof(char)) + 12);
  inc(result, (FUrl.length * sizeof(char)) + 12);
  inc(result, (FCanonical.length * sizeof(char)) + 12);
end;

end.
