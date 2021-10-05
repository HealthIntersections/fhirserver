unit ftk_store_http;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  IdUri,
  fsl_fetcher,
  fhir_client,
  ftk_store;

{
 this implementation share addresses with the server store.
 that will be used wherever there's a matching registered server for an http:
 else it falls back to this which is read-only
}

type
  { THTTPStorageService }

  THTTPStorageService = class (TStorageService)
  public
    function schemes : TArray<String>; override;
    function inScope(url : String) : boolean; override;

    function CheckTimes : boolean; override;
    function CurrencyCheckFrequency : integer; override;
    function canSave : boolean; override;
    function load(address : String; doException : boolean) : TLoadedBytes; override;
    function save(address : String; bytes : TBytes) : TDateTime; override;
    function CaptionForAddress(address : String) : String; override;
    function describe(address : String) : String; override;
    procedure delete(address : String); override;
    function openDlg(out newName : String) : boolean; override;
    function saveDlg(existing : String; suggestedExtension : String; out newName : String) : boolean; override;
    function MakeFilename(address : String) : String; override;
    function clientForAddress(address : String) : TFHIRClientV; override;
    procedure forceLocation(address : String); override;
    function getName(address : String; mode : TNameMode) : String; override;
  end;

implementation

{ THTTPStorageService }

function THTTPStorageService.schemes: TArray<String>;
begin
  result := ['http', 'https', 'ftp'];
end;

function THTTPStorageService.inScope(url: String): boolean;
begin
  result := false;
end;

function THTTPStorageService.CheckTimes: boolean;
begin
  result := true;
end;

function THTTPStorageService.CurrencyCheckFrequency: integer;
begin
  result := 5 * 60;
end;

function THTTPStorageService.canSave: boolean;
begin
  result := false;
end;

function THTTPStorageService.load(address: String; doException: boolean): TLoadedBytes;
var
  fetcher : TInternetFetcher;
begin
  fetcher := TInternetFetcher.create;
  try
    fetcher.URL := address;
    fetcher.Fetch;
    result.content := fetcher.Buffer.AsBytes;
    result.mimeType := fetcher.ContentType;
    result.timestamp := fetcher.LastModified;
  finally
    fetcher.free;
  end;
end;

function THTTPStorageService.save(address: String; bytes: TBytes): TDateTime;
begin
  raise Exception.create('This store cannot save');
end;

function THTTPStorageService.CaptionForAddress(address: String): String;
var
  uri : TIdUri;
begin
  uri := TIdURI.create(address);
  try
    result := uri.document;
  finally
    uri.free;
  end;
end;

function THTTPStorageService.describe(address: String): String;
begin
  result := CaptionForAddress((address));
end;

procedure THTTPStorageService.delete(address: String);
begin
  raise Exception.create('This store cannot delete');
end;

function THTTPStorageService.openDlg(out newName: String): boolean;
begin
  raise Exception.create('Not supported by this store');
end;

function THTTPStorageService.saveDlg(existing: String; suggestedExtension: String; out newName: String): boolean;
begin
  raise Exception.create('Not supported by this store');
end;

function THTTPStorageService.MakeFilename(address: String): String;
begin
  result := CaptionForAddress(address);
end;

function THTTPStorageService.clientForAddress(address: String): TFHIRClientV;
begin
  raise Exception.create('Not supported by this store');
end;

procedure THTTPStorageService.forceLocation(address: String);
begin
  raise Exception.create('Not supported by this store');
end;

function THTTPStorageService.getName(address: String; mode: TNameMode): String;
begin
  raise Exception.create('Not supported by this store');
end;

end.

