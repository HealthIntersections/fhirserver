unit FHIR.FMX.Ctrls;

{$DEFINE USEFHIROBJ}


interface

uses
  System.SysUtils, System.Classes, System.types, System.RTLConsts, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.TreeView, FMX.Controls.Presentation, FMX.Edit , FMX.Layouts, FMX.StdCtrls
{$IFDEF USEFHIROBJ}
, FHIR.R4.Types
{$ENDIF}
  ;

  type

  TFHIRStringEdit = class(TStyledControl)
  private
    pGroup: TScrollBox;
    pLabel: TScrollBox;
    lbl: TLabel;
    pContent: TScrollBox;
    edt: TEdit;
    pBtn: TScrollBox;
{$IFDEF USEFHIROBJ}
    fFHIRString:TFHIRString;
{$ELSE}
    fFHIRStringList:TStringList;
{$ENDIF}
    btn: TButton;
    fmultiple:boolean;
    fOnChange : TNotifyEvent;
    fOnClick : TNotifyEvent;
    flabelText:String;
    fPropertyName:string;
    procedure onEditChange(Sender: TObject);
    procedure onBtnClick(Sender: TObject);
  protected

{$IFDEF USEFHIROBJ}
    function GeTFHIRString: TFHIRString;
    procedure SeTFHIRString(AValue: TFHIRString);
{$ELSE}
    function GeTFHIRString: TStringList;
    procedure SeTFHIRString(AValue: TstringList);
{$ENDIF}

    procedure setPropertyName(propName:string);
    procedure setMultiple(mult:boolean);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure load;
{$IFDEF USEFHIROBJ}
    property FHIRProperty: TFHIRString read GeTFHIRString write seTFHIRString;
    function associate(AValue: TFHIRString): TFHIRString;
{$ELSE}
    property FHIRProperty: TStringList read GeTFHIRString write seTFHIRString;
    function associate(AValue: TstringList): TStringList;
{$ENDIF}
{$IFNDEF USEFHIROBJ}
    property FHIRStringList: TStringList read fFHIRStringList write fFHIRStringList;
{$ENDIF}

  published
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnClick: TNotifyEvent read fOnClick write fOnClick;
    property FHIRPropertyName: String read fPropertyName write setPropertyName;
    property Multiple: Boolean read fMultiple write setMultiple;
    property Align default TAlignLayout.Top;
  end;


procedure Register;

implementation


procedure Register;
begin
  RegisterComponents('FHIR', [TFHIRStringEdit]);
end;

constructor TFHIRStringEdit.Create(Owner: TComponent);
begin
  inherited;
  Align := TAlignLayout.Top;
  FOnChange := nil;
  FOnClick := nil;
  flabelTExt:='Label';
  height:=37;
  width := 230;
  pGroup := TScrollBox.Create(Self);
  pGroup.Parent := self;
  pGroup.Height := 31;
  pGroup.Margins.Top := 5;
  pGroup.Margins.bottom := 5;
  pgroup.Stored:=false;
  pgroup.ShowScrollBars:=false;
  pGroup.Align := tAlignLayout.Client;

  pLabel := TScrollBox.Create(pGroup);
  pLabel.Parent := pGroup;
  pLabel.Height := 17;
  pLabel.Align := tAlignLayout.Left;
  pLabel.Stored:=false;
  plabel.ShowScrollBars:=false;
  pLabel.Margins.Left := 7;

  lbl := TLabel.Create(self);
  lbl.Parent := pLabel;
  lbl.TextSettings.WordWrap := False;
  lbl.AutoSize := true;
  lbl.Align := tAlignLayout.Left;
  lbl.position.x := 7;
  lbl.text := flabeltext;
  lbl.Stored:=false;

  pContent := TScrollBox.Create(pGroup);
  pContent.Parent := pGroup;
  pContent.Align := tAlignLayout.Client;
  pContent.Margins.Top := 2;
  pContent.Margins.Left := 7;
  pContent.Margins.Right := 7;
  pContent.ShowScrollBars:=false;
  pContent.Margins.bottom := 2;
  pContent.Stored:=false;

  pBtn := TScrollBox.Create(pGroup);
  pBtn.Parent := pContent;
  pBtn.Align := tAlignLayout.Right;
  pBtn.size.width := 47;
  pBtn.size.width := 23;
  pBtn.Margins.Top := 0;
  pBtn.Margins.Left := 7;
  pBtn.Margins.Right := 2;
  pBtn.Margins.bottom := 0;
  pBtn.ShowScrollBars:=false;
  pBtn.Stored:=false;

  Btn:=TButton.create(pBtn);

  Btn.Parent := pBtn;
  Btn.text:='+';
  Btn.Align := tAlignLayout.Client;
  Btn.Stored:=false;
  Btn.Onclick := onBtnClick;

  edt := TEdit.Create(pContent);
  edt.Parent := pContent;
  edt.Stored:=false;
  edt.Align := tAlignLayout.Client;
  edt.OnChange := onEditChange;
{$IFNDEF USEFHIROBJ}
  FFHIRStringList:=TStringList.Create;
  FFHIRStringList.add('');
{$ENDIF}

end;



destructor TFHIRStringEdit.Destroy;
begin
{$IFNDEF USEFHIROBJ}
  FFHIRStringList.Destroy;
{$ENDIF}
  edt.Destroy;
  Btn.Destroy;
  pBtn.Destroy;
  pContent.Destroy;
  lbl.Destroy;
  pLabel.Destroy;
  pGroup.Destroy;
  inherited;

end;

procedure TFHIRStringEdit.OnEditChange(Sender : TObject);
begin
{$IFDEF USEFHIROBJ}
  if fFHIRString <> nil then
    fFHIRstring.value:=edt.text;
{$ELSE}
  fFHIRStringList[0]:=edt.Text;
{$ENDIF}
  if(Assigned(FOnChange))then FOnChange(Self);

end;

procedure TFHIRStringEdit.OnBtnClick(Sender : TObject);
begin
//Internal stuff to do when clicking.
  if(Assigned(FOnClick))then FOnClick(Self);
end;


procedure TFHIRStringEdit.SetPropertyName(propName : String);
begin
  fPropertyName:=propName;
  btn.Hint:= 'Add a new ' + fpropertyName +'.';
  if fPropertyName <> '' then btn.ShowHint:=true else btn.ShowHint:=false;
  lbl.text:= PropName;
  pLabel.Width := lbl.Canvas.TextWidth(lbl.Text) ;

end;

procedure TFHIRStringEdit.setMultiple(mult:boolean);
begin
  fmultiple:=mult;
  if fmultiple then pBtn.Visible:=true else pBtn.Visible:=false;
  pBtn.Repaint;


end;


procedure TFHIRStringEdit.load;
begin
{$IFDEF USEFHIROBJ}
  if fFHIRString <>nil then
  edt.text:= fFHIRString.value;
{$ELSE}
  if fFHIRStringList<>nil then
  edt.text:=fFHIRStringList[0];
{$ENDIF}

end;

{$IFDEF USEFHIROBJ}
function TFHIRStringEdit.GetFHIRString: TFHIRString;
begin
  result := fFHIRString;
  if fFHIRString <> nil then
  edt.text:=fFHIRString.value;
end;
{$ELSE}

function TFHIRStringEdit.GetFHIRString: TStringList;
begin
  result := fFHIRStringList;
  if fFHIRStringList <> nil then
  edt.text:=fFHIRStringList[0];
end;
{$ENDIF}


{$IFDEF USEFHIROBJ}
procedure TFHIRStringEdit.SetFHIRString(AValue: TFHIRString);
begin
  if fFHIRString = nil then
  begin
    fFHIRString := TFHIRString.Create;
    edt.text:='';
  end
  else begin
    fFHIRString := AValue;
    edt.text:=fFHIRString.value;
  end;
end;


{$ELSE}
procedure TFHIRStringEdit.SetFHIRString(AValue: TStringList);
begin
  if fFHIRStringList = nil then
  begin
    fFHIRStringList := TStringList.Create;
    edt.text:='';
  end
  else begin
    fFHIRStringList := AValue;
    edt.text:=fFHIRStringList[0];
  end;
end;
{$ENDIF}


{$IFDEF USEFHIROBJ}
function TFHIRStringEdit.associate(AValue: TFHIRString): TFHIRString;
begin
  if aValue = nil then
  begin
    fFHIRString.CREATE;//     := TFHIRString.Create;
    edt.text:='';
  end
  else begin
    fFHIRString := AValue;                  // THIS IS NOT REALLY WORKING WELL. mUST CHECK AND REDO
    fFHIRstring.value:=AValue.value;
    edt.text:=fFHIRString.value;
    edt.Repaint;
    fFHIRString := AValue;                  // THIS IS NOT REALLY WORKING WELL. mUST CHECK AND REDO
  end;
  result:=fFhirString;
end;

{$ELSE}
function TFHIRStringEdit.associate(AValue: TStringList): TStringList;
begin
  if aValue = nil then
  begin
    fFHIRStringList := TStringList.Create;
    edt.text:='';
  end
  else begin
    fFHIRStringList := AValue;
    edt.text:=fFHIRStringList[0];
  end;
  result:=fFhirStringList;
end;


{$ENDIF}

{
procedure TFHIRStringEdit.useObject(var AValue: TFHIRString);
begin
aValue:= associate(aValue);
end;

}















end.


