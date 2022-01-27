unit fhir_package_upload;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base,
  fhir_client, fhir_factory;

type

  { TFHIRPackageUploader }

  TFHIRPackageUploader = class (TFslObject)
  private
    FClient: TFHIRClientV;
    FFactory: TFHIRFactory;
    procedure SetClient(AValue: TFHIRClientV);
    procedure SetFactory(AValue: TFHIRFactory);
  public
    destructor Destroy; override;

    procedure uploadPackage(packageId, version : String);

    property client : TFHIRClientV read FClient write SetClient;
    property factory : TFHIRFactory read FFactory write SetFactory;
  end;

implementation

{ TFHIRPackageUploader }

procedure TFHIRPackageUploader.SetClient(AValue: TFHIRClientV);
begin
  FClient.free;
  FClient := AValue;
end;

procedure TFHIRPackageUploader.SetFactory(AValue: TFHIRFactory);
begin
  FFactory.free;
  FFactory := AValue;
end;

destructor TFHIRPackageUploader.Destroy;
begin
  FClient.free;
  FFactory.free;
  inherited Destroy;
end;

procedure TFHIRPackageUploader.uploadPackage(packageId, version: String);
begin
  // todo
end;

end.

