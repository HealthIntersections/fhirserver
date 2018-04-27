unit FHIR.Cql.Engine;

interface

uses
  FHIR.Support.Objects;


Type
  TCqlEngine = class (TAdvObject)
  private
  public
    Constructor Create; override;
    Destructor Destroy; override;

  end;

implementation

{ TCqlEngine }

constructor TCqlEngine.Create;
begin
  inherited;

end;

destructor TCqlEngine.Destroy;
begin

  inherited;
end;

end.
