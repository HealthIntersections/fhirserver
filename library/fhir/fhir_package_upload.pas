unit fhir_package_upload;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_logging, fsl_lang, fsl_npm_client, fsl_npm_cache,
  fhir_client, fhir_factory;

type

  { TFHIRPackageUploader }

  TFHIRPackageUploader = class (TFslObject)
  private
    FAPIResources: boolean;
    FBulkImport: boolean;
    FClient: TFHIRClientV;
    FDefinitionResources: boolean;
    FDependencies: boolean;
    FExampleResources: boolean;
    FFactory: TFHIRFactory;
    FNoErrorIfExists: boolean;
    FOnLog: TLogEvent;
    FProfileResources: boolean;
    FQuestionnaireResources: boolean;
    FStopForErrors: boolean;
    FTerminologyResources: boolean;
    procedure SetClient(AValue: TFHIRClientV);
    procedure SetFactory(AValue: TFHIRFactory);
  public
    destructor Destroy; override;

    procedure uploadPackage(packageId, version : String);

    property client : TFHIRClientV read FClient write SetClient;
    property factory : TFHIRFactory read FFactory write SetFactory;

    // settings:
    property TerminologyResources : boolean read FTerminologyResources write FTerminologyResources;
    property ProfileResources : boolean read FProfileResources write FProfileResources;
    property APIResources : boolean read FAPIResources write FAPIResources;
    property QuestionnaireResources : boolean read FQuestionnaireResources write FQuestionnaireResources;
    property DefinitionResources : boolean read FDefinitionResources write FDefinitionResources;
    property ExampleResources : boolean read FExampleResources write FExampleResources;
    property Dependencies : boolean read FDependencies write FDependencies;
    property BulkImport : boolean read FBulkImport write FBulkImport;
    property StopForErrors : boolean read FStopForErrors write FStopForErrors;
    property NoErrorIfExists : boolean read FNoErrorIfExists write FNoErrorIfExists;

    Property OnLog : TLogEvent read FOnLog write FOnLog;
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
  raise EFSLException.create('Still to be done');
end;

end.

