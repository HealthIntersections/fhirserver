unit FHIR.Ndc.Services;

interface

uses
  FHIR.Support.Base,
  FHIR.Database.Manager;

type
  TNdcImporter = class (TFslObject)
  private
    FSource: String;
    FDatabase: TKDBManager;
    procedure SetDatabase(const Value: TKDBManager);
  public
    constructor Create(source : String);
    destructor Destroy; override;

    property Database : TKDBManager read FDatabase write SetDatabase;
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

procedure TNdcImporter.SetDatabase(const Value: TKDBManager);
begin
  FDatabase := Value;
end;

end.
