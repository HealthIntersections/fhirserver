unit SnomedCombiner;

interface

uses
  AdvGenerics, AdvObjects,
  SnomedServices;

Type
  TSnomedCombinedConcept = class;

  TSnomedCombinedDescription = class (TAdvObject)
  private
    FId : Int64;
    FDate : TSnomedDate;
    FModule : byte;
    FKind : TSnomedCombinedConcept; // not linked
    FFlags : byte;
  public
  end;

  TSnomedCombinedRelationship = class (TAdvObject)
  private
    FId : Int64;

  public
  end;

  TSnomedCombinedConcept = class (TAdvObject)
  private
    FId : Int64;
    FDescriptions : TAdvList<TSnomedCombinedDescription>;
    FFlags : byte;
    FModule : byte;
    FDate : TSnomedDate;
    FParents : TAdvList<TSnomedCombinedConcept>;
    FChildren : TAdvList<TSnomedCombinedConcept>;
    FInbounds : TAdvList<TSnomedCombinedRelationship>;
    FOutbounds : TAdvList<TSnomedCombinedRelationship>;
  public
  end;
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

// classify

// write the combo to RF2 format

}
Type
  TSnomedCombiner = class (TAdvObject)
  private
    FModuleId: int64;
    FInternational: TSnomedServices;
    FOthers: TAdvMap<TSnomedServices>;
    procedure SetInternational(const Value: TSnomedServices);
  public
    Constructor Create; override;
    Destructor Destroy; override;

    procedure Execute;

    property international : TSnomedServices read FInternational write SetInternational;
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
  FInternational.free;
  inherited;
end;

procedure TSnomedCombiner.Execute;
begin

end;

procedure TSnomedCombiner.SetInternational(const Value: TSnomedServices);
begin
  FInternational.free;
  FInternational := Value;
end;


end.
