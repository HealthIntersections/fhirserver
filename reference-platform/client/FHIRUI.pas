unit FHIRUI;

interface

uses
  SysUtils,
  Classes,
  AdvObjects;

type
  TCommaBuilder = class (TAdvObject)
  private
    list : TStringList;
    FIgnoreDuplicates: Boolean;
  public
    Constructor Create;
    Destructor Destroy; override;

    procedure add(s : String);
    function asString : string;

    property ignoreDuplicates : Boolean read FIgnoreDuplicates write FIgnoreDuplicates;
  end;

implementation

{ TCommaBuilder }

procedure TCommaBuilder.add(s: String);
begin
  if s <> '' then
    if not FIgnoreDuplicates or (list.IndexOf(s) = -1) then
      list.add(s);
end;

function TCommaBuilder.asString: string;
var
  i : integer;
begin
  if list.Count = 0 then
    result := ''
  else
  begin
    result := list[0];
    for i := 1 to list.Count - 1 do
      result := result + ', '+list[i];
  end;
end;

constructor TCommaBuilder.Create;
begin
  inherited create;
  list := TStringList.Create;
end;

destructor TCommaBuilder.Destroy;
begin
  list.Free;
  inherited;
end;

end.
