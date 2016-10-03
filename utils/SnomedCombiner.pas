unit SnomedCombiner;

interface

uses
  AdvGenerics, AdvObjects,
  SnomedServices;
{
// for major.concept
//   add to combination
//   add descriptions
//   add relationships

// for each other
//   for each concept
//     is it in combo?
//     are it's descriptions in combo?
//     are its relationships in combo?

// write the combo to RF2 format

}
Type
  TSnomedCombiner = class (TAdvObject)
  private
    FModuleId: int64;
    FBase: TSnomedServices;
    FOthers: TAdvMap<TSnomedServices>;
    procedure SetBase(const Value: TSnomedServices);
  public
    Constructor Create; override;
    Destructor Destroy; override;

    property base : TSnomedServices read FBase write SetBase;
    property others : TAdvMap<TSnomedServices> read FOthers;
    property moduleId : int64 read FModuleId write FModuleId;
  end;

implementation

{ TSnomedCombiner }

constructor TSnomedCombiner.Create;
begin
  inherited;
  FOthers := TAdvMap<TSnomedServices>.create;
end;

destructor TSnomedCombiner.Destroy;
begin
  FOthers.free;
  FBase.free;
  inherited;
end;

procedure TSnomedCombiner.SetBase(const Value: TSnomedServices);
begin
  FBase.free;
  FBase := Value;
end;

end.
