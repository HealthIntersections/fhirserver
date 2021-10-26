unit time_tracker;

interface

uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_logging;

type
  TTimeTracker = class (TFslObject)
  private
    FStart : int64;
    FLast : Int64;
//    FPoints : TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure step(name : String);
    function total : integer;
    function log : String;
  end;

implementation

{ TTimeTracker }

constructor TTimeTracker.Create;
begin
  inherited;
  FStart := GetTickCount64;
  FLast := FStart;
//  FPoints := TStringList.Create;
end;

destructor TTimeTracker.Destroy;
begin
//  FPoints.Free;
  inherited;
end;

function TTimeTracker.log : String;
var
  s : String;
begin
  result := '';
//  for s in FPoints do
//    CommaAdd(result, s);
//  Logging.log('~~~ '+v);
end;

procedure TTimeTracker.step(name: String);
var
  t : int64;
begin
  t := GetTickCount64;
//  FPoints.Add(name+': '+StringPadLeft(inttostr(t - FLast), ' ', 5));
  FLast := t;
end;

function TTimeTracker.total: integer;
begin
  result := GetTickCount64 - FStart;
end;

end.
