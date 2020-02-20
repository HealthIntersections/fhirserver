unit FHIR.Cache.PackageClient;

interface

uses
   SysUtils, Classes,
   FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Json;

type
  TFHIRPackageInfo = class (TFslObject)
  private
    FId : String;
    FVersion : String;
    FFhirVersion : String;
    FDescription : String;
    FUrl : String;
  public
    constructor Create(id, version, fhirVersion, description, url : String);
    property id : String read FId;
    property version : String read FVersion;
    property fhirVersion : String read FFhirVersion;
    property description : String read FDescription;
    property url : String read FUrl;
  end;

  TFHIRPackageClient = class (TFslObject)
  private
    FAddress : String;
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
    function url(id, v : String) : string;
  end;

implementation

{ TFHIRPackageClient }

constructor TFHIRPackageClient.Create(address: String);
begin

end;

function TFHIRPackageClient.exists(id, ver: String): boolean;
begin
  result := false;
end;

function TFHIRPackageClient.fetch(info: TFHIRPackageInfo): TBytes;
begin

end;

function TFHIRPackageClient.fetch(id, ver: String): TBytes;
begin

end;

function TFHIRPackageClient.fetchNpm(id, ver: String): TBytes;
begin

end;

function TFHIRPackageClient.getVersions(id: String): TFslList<TFHIRPackageInfo>;
begin
  result := nil;
end;

function TFHIRPackageClient.search(lastCalled: TFslDateTime;
  packages: TFslList<TFHIRPackageInfo>): TFslDateTime;
begin

end;

function TFHIRPackageClient.search(name, canonical, fhirVersion: String;
  preRelease: boolean): TFslList<TFHIRPackageInfo>;
begin
  result := nil;
end;

function TFHIRPackageClient.url(id, v: String): string;
begin

end;

{ TFHIRPackageInfo }

constructor TFHIRPackageInfo.Create(id, version, fhirVersion, description, url: String);
begin

end;

end.
