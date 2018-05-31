unit FHIR.FMX.Ctrls;

interface

uses
  System.SysUtils, System.Classes, System.types, System.RTLConsts, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.TreeView, FMX.Controls.Presentation, FMX.Edit , FMX.Layouts, FMX.StdCtrls
//, FHIR.R4.Types

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
//    fFHIRString:String; ///TO RENAME AFTER GETTING FHIR CLASSES       //////////////////////////////////////
    fFHIRStringList:TStringList; ///TO RENAME AFTER GETTING FHIR CLASSES       //////////////////////////////////////
    btn: TButton;
    fmultiple:boolean;
    fOnChange : TNotifyEvent;
    fOnClick : TNotifyEvent;
    flabelText:String;
    fPropertyName:string;
    procedure onEditChange(Sender: TObject);
    procedure onBtnClick(Sender: TObject);
  protected

    function GeTFHIRString: TStringList;
    procedure SeTFHIRString(AValue: TstringList);

    procedure setPropertyName(propName:string);
    procedure setMultiple(mult:boolean);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure load;
{
    property FHIRPropertyValue: String read GeTFHIRStringValue write seTFHIRStringValue; ///TO RESTORE AFTER GETTING FHIR CLASSES
    property FHIRProperty: TFHIRString read GeTFHIRString write seTFHIRString;
    procedure useObject(var AValue: TFHIRstring);
}
    function associate(AValue: TstringList): TStringList;

  published
    property FHIRStringList: TStringList read fFHIRStringList write fFHIRStringList;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnClick: TNotifyEvent read fOnClick write fOnClick;
    property FHIRPropertyName: String read fPropertyName write setPropertyName;
    property Multiple: Boolean read fMultiple write setMultiple;
    property Align default TAlignLayout.Top;
  end;


procedure Register;

implementation

{$R FHIR.FMX.Ctrls.res}

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
  FFHIRStringList:=TStringList.Create;
  FFHIRStringList.add('');

end;



destructor TFHIRStringEdit.Destroy;
begin
  FFHIRStringList.Destroy;
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
//  fFHIRString := edt.text;
  fFHIRStringList[0]:=edt.Text;

  if(Assigned(FOnChange))then FOnChange(Self);
//  if fFHIRString <> nil then                ///TO RESTORE AFTER GETTING FHIR CLASSES
//    fFHIRstring.value:=edt.text;
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
  if fFHIRStringList<>nil then                                                          ///TO RESTORE AFTER GETTING FHIR CLASSES
  edt.text:=fFHIRStringList[0];
end;

{
function TFHIRStringEdit.GeTFHIRStringValue: string;                     ///TO RESTORE AFTER GETTING FHIR CLASSES
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

}
function TFHIRStringEdit.GetFHIRString: TStringList;
begin
  result := fFHIRStringList;
  if fFHIRStringList <> nil then
    edt.text:=fFHIRStringList[0];
end;

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

function TFHIRStringEdit.associate(AValue: TStringList): TStringList;
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
  result:=fFhirStringList;
end;


{
procedure TFHIRStringEdit.useObject(var AValue: TFHIRString);
begin
aValue:= associate(aValue);
end;

}















end.

