unit FHIR.Ndc.Services;

interface

uses
  FHIR.Support.Base,
  FHIR.Database.Manager;

type
  TNdcImporter = class (TFslObject)
  private
    FSource: String;
    FDatabase: TFslDBManager;
    procedure SetDatabase(const Value: TFslDBManager);
  public
    constructor Create(source : String);
    destructor Destroy; override;

    property Database : TFslDBManager read FDatabase write SetDatabase;
    property source : String read FSource write FSource;
  end;

implementation

{ TNdcImporter }

constructor TNdcImporter.Create(source: String);
begin
  inherited create;
  FSource := source;
end;

destructor TNdcImporter.Destroy;
begin
  FDatabase.Free;
  inherited;
end;

procedure TNdcImporter.SetDatabase(const Value: TFslDBManager);
begin
  FDatabase := Value;
end;

end.
