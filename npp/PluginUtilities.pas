unit PluginUtilities;

interface

uses
  AdvObjects, System.Generics.Defaults, FHIRBase;

type
  TTreeDataPointer = record
    expr : TFHIRExpressionNode;
    op : boolean;
  end;
  PTreeDataPointer = ^TTreeDataPointer;

  TFHIRAnnotationLevel = (alError, alWarning, alHint, alMatch);

  TFHIRAnnotation = class (TAdvObject)
  private
    FLevel: TFHIRAnnotationLevel;
    FMessage: String;
    FLine : integer;
    FStop: integer;
    FStart: integer;
    FDescription: String;
  public
    Constructor create(level : TFHIRAnnotationLevel; line : integer; start, stop : integer; message, description : String); overload;

    property level : TFHIRAnnotationLevel read FLevel write FLevel;
    property start : integer read FStart write FStart;
    property stop : integer read FStop write FStop;
    property message : String read FMessage write FMessage;
    property description : String read FDescription write FDescription;
    property line : integer read FLine write FLine;
  end;

  TFHIRAnnotationComparer = class (TAdvObject, IComparer<TFHIRAnnotation>)
  public
    function Compare(const Left, Right: TFHIRAnnotation): Integer;
  end;

implementation

{ TFHIRAnnotation }

constructor TFHIRAnnotation.create(level: TFHIRAnnotationLevel; line, start, stop: integer; message, description: String);
begin
  Create;
  self.level := level;
  self.line := line;
  self.start := start;
  self.stop := stop;
  self.message := message;
  if description <> '' then
    self.description := description
  else
    self.description := message;
end;

{ TFHIRAnnotationComparer }

function TFHIRAnnotationComparer.Compare(const Left, Right: TFHIRAnnotation): Integer;
begin
  if (left.Start < Right.Start) then
    result := -1
  else if (left.Start > Right.Start) then
    result := 1
  else if (left.Stop < Right.Stop) then
    result := -1
  else if (left.Stop > Right.Stop) then
    result := 1
  else if (left.Level < Right.Level) then
    result := -1
  else if (left.Level > Right.Level) then
    result := 1
  else
    result := 0;
end;



end.
