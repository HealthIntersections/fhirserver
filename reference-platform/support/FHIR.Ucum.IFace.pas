unit FHIR.Ucum.IFace;

interface

uses
  SysUtils, Classes,
  FHIR.Support.Decimal,
  FHIR.Support.Objects;

Type
  EUCUMServices = class (Exception);

  TUcumPair = class (TFslObject)
  private
    FUnitCode: String;
    FValue: TSmartDecimal;
  Public
    Constructor Create(oValue : TSmartDecimal; sUnitCode : String); Overload;

    Property Value : TSmartDecimal read FValue write FValue;
    Property UnitCode : String read FUnitCode write FUnitCode;
  End;

  TUcumServiceInterface = class (TFslObject)
  public
    Function multiply(o1, o2 : TUcumPair) : TUcumPair; virtual;
    function getCanonicalForm(value : TUcumPair) : TUcumPair; virtual;
  end;

implementation

{ TUcumPair }

constructor TUcumPair.Create(oValue: TSmartDecimal; sUnitCode: String);
begin
  Create;
  Value := oValue;
  UnitCode := sUnitCode;
end;


{ TUcumServiceInterface }

function TUcumServiceInterface.getCanonicalForm(value: TUcumPair): TUcumPair;
begin
  raise Exception.Create('Must override TUcumServiceInterface.getCanonicalForm');
end;

function TUcumServiceInterface.multiply(o1, o2: TUcumPair): TUcumPair;
begin
  raise Exception.Create('Must override TUcumServiceInterface.multiply');
end;

end.
