unit FHIRServerContext;

interface

uses
  AdvObjects,
  FHIRStorageService;

Type
  TFHIRServerContext = class (TAdvObject)
  private
    FStorage : TFHIRStorageService;
  public
    Constructor Create(storage : TFHIRStorageService);
    Destructor Destroy; override;

    property Storage : TFHIRStorageService read FStorage;
  end;

implementation

{ TFHIRServerContext }

constructor TFHIRServerContext.Create(storage: TFHIRStorageService);
begin
  Inherited Create;
  FStorage := storage;
end;

destructor TFHIRServerContext.Destroy;
begin
  FStorage.Free;
  inherited;
end;

end.
