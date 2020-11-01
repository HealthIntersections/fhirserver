unit FHIR.Toolkit.FileStore;

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  FHIR.Support.Utilities, FHIR.Support.Stream,
  FHIR.Toolkit.Context;

type

  { TFileStorageService }

  TFileStorageService = class (TStorageService)
  public
    function scheme : String; override;
    procedure save(address : String; bytes : TBytes); override;
    function CaptionForAddress(address : String) : String; override;
    function makeSession(address : String) : TToolkitEditSession; override;
  end;

implementation

{ TFileStorageService }

function TFileStorageService.scheme: String;
begin
  result := 'file';
end;

procedure TFileStorageService.save(address: String; bytes: TBytes);
begin
  BytesToFile(bytes, address.Substring(5));
end;

function TFileStorageService.CaptionForAddress(address: String): String;
begin
  result := ExtractFileName(address.Substring(5));
end;

function TFileStorageService.makeSession(address: String) : TToolkitEditSession;
begin
  result := TToolkitEditSession.create;
  result.Guid := NewGuidId;
  result.Address := address;
  result.Caption := ExtractFileName(address.Substring(5));
end;

end.

