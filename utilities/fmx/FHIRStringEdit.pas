unit FHIRStringEdit;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls,
  FMX.Controls.Presentation, FMX.Edit,
  fhir4_types;

type
  TFHIRStringEdit = class(TEdit)
  private
    fFHIRString: tFHIRString;
    fOnChange : TNotifyEvent;
    procedure MyOnChange(Sender : TObject);
  protected
    function GeTFHIRStringValue: String;
    procedure SeTFHIRStringValue(AValue: string);
    function GeTFHIRString: TFHIRString;
    procedure SeTFHIRString(AValue: TFHIRstring);
  public
    constructor Create(AOwner: TComponent); override;
    property FHIRPropertyValue: String read GeTFHIRStringValue write seTFHIRStringValue;
    property FHIRProperty: TFHIRString read GeTFHIRString write seTFHIRString;
    procedure load;
    function associate(AValue: TFHIRstring): TFHIRString;
    procedure useObject(var AValue: TFHIRstring);
  end;



implementation

constructor TFHIRStringEdit.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     Inherited OnChange := MyOnChange;
end;

procedure TFHIRStringEdit.MyOnChange(Sender : TObject);
begin
  if fFHIRString <> nil then
    fFHIRstring.value:=text;
end;

procedure TFHIRStringEdit.load;
begin
  if fFHIRString<>nil then
  text:=fFHIRString.value;
end;



function TFHIRStringEdit.GeTFHIRStringValue: string;
begin
  if fFHIRString <> nil then
  begin
    result := fFHIRString.Value;
  end
  else
  begin
    result := 'No external object attached';
  end;
end;

procedure TFHIRStringEdit.SeTFHIRStringValue(AValue: String);
begin
  if fFHIRString = nil then
  begin
    fFHIRString := TFHIRString.Create;
  end;
  fFHIRString.Value := AValue;
end;

function TFHIRStringEdit.GeTFHIRString: TFHIRString;
begin
  result := fFHIRString;
  if fFHIRString <> nil then
    text:=fFHIRString.value;
end;

procedure TFHIRStringEdit.SeTFHIRString(AValue: TFHIRString);
begin
  if fFHIRString = nil then
  begin
    fFHIRString := TFHIRString.Create;
    text:='';
  end
  else begin
    fFHIRString := AValue;
    text:=fFHIRString.value;
  end;

end;

function TFHIRStringEdit.associate(AValue: TFHIRString): TFHIRString;
begin
  if fFHIRString = nil then
  begin
    fFHIRString := TFHIRString.Create;
    text:='';
  end
  else begin
    fFHIRString := AValue;
    text:=fFHIRString.value;
  end;
  result:=fFhirString;
end;


procedure TFHIRStringEdit.useObject(var AValue: TFHIRString);
begin
aValue:= associate(aValue);
end;

end.
