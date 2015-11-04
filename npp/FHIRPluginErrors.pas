unit FHIRPluginErrors;

interface

uses
  AdvObjects, AdvGenerics, System.Generics.Defaults;

const
  INDIC_INFORMATION = 28;
  INDIC_WARNING = 28;
  INDIC_ERROR = 28;

type
  TValidationError = class (TAdvObject)
  private
    FLevel: integer;
    FMessage: String;
    FStop: integer;
    FStart: integer;
  public
    Constructor create(level : integer; start, stop : integer; message : String); overload;

    property level : integer read FLevel write FLevel;
    property start : integer read FStart write FStart;
    property stop : integer read FStop write FStop;
    property message : String read FMessage write FMessage;
  end;

  TValidationErrorComparer = class (TAdvObject, IComparer<TValidationError>)
  public
    function Compare(const Left, Right: TValidationError): Integer;
  end;



implementation


{ TValidationErrorComparer }

function TValidationErrorComparer.Compare(const Left, Right: TValidationError): Integer;
begin
  if (left.FStart < Right.FStart) then
    result := -1
  else if (left.FStart > Right.FStart) then
    result := 1
  else if (left.FStop < Right.FStop) then
    result := -1
  else if (left.FStop > Right.FStop) then
    result := 1
  else if (left.FLevel < Right.FLevel) then
    result := -1
  else if (left.FLevel > Right.FLevel) then
    result := 1
  else
    result := 0;
end;

{ TValidationError }

constructor TValidationError.create(level: integer; start, stop: integer; message: String);
begin
  Create;
  self.level := level;
  self.start := start;
  self.stop := stop;
  self.message := message;
end;

end.
