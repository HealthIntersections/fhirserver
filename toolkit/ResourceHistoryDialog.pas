unit ResourceHistoryDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.ImageList, FMX.ImgList, FMX.ScrollBox, FMX.Memo, FMX.Edit,
  FMX.DateTimeCtrls, FMX.StdCtrls, FMX.Controls.Presentation,
  FHIR.Support.Utilities,
  FHIR.Version.Types, FHIR.Version.Utilities;

type
  THistoryField = (hfDate, hfId, hfAuthor, hfOnBehalfOf, hfSubst, hfBreaking, hfNotes);
  THistoryFieldSet = set of THistoryField;

  TResourceHistoryForm = class(TForm)
    Panel1: TPanel;
    btnOk: TButton;
    Button2: TButton;
    btnAsChild: TButton;
    lblDate: TLabel;
    lblId: TLabel;
    LblOnBehalfOf: TLabel;
    LblAuthor: TLabel;
    lblNotes: TLabel;
    edtDate: TDateEdit;
    edtId: TEdit;
    edtOnBehalfOf: TEdit;
    edtAuthor: TEdit;
    memNotes: TMemo;
    cbSubst: TCheckBox;
    cbBreaking: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure edtDateChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    FExtension : TFhirExtension;
    procedure SetExtension(const Value: TFhirExtension);
  public
    destructor Destroy; override;
    procedure Adapt(fields : THistoryFieldSet);
    property Extension : TFhirExtension read FExtension write SetExtension;
  end;

var
  ResourceHistoryForm: TResourceHistoryForm;

implementation

{$R *.fmx}

{ TForm1 }

procedure TResourceHistoryForm.Adapt(fields: THistoryFieldSet);
  procedure status(lbl : TLabel; cntrl : TStyledControl; enabled : boolean);
  begin
    if enabled then
    begin
      if lbl <> nil then
        lbl.TextSettings.FontColor := TAlphaColorRec.Black;
      cntrl.enabled := true;
    end
    else
    begin
      if lbl <> nil then
        lbl.TextSettings.FontColor := TAlphaColorRec.Grey;
      cntrl.enabled := false;
    end;
  end;
begin
  status(lblDate, edtDate, hfDate in fields);
  status(lblId, edtId, hfId in fields);
  status(LblAuthor, edtAuthor, hfAuthor in fields);
  status(LblOnBehalfOf, edtOnBehalfOf, hfOnBehalfOf in fields);
  status(nil, cbSubst, hfSubst in fields);
  status(nil, cbBreaking, hfBreaking in fields);
  status(lblNotes, memNotes, hfNotes in fields);
end;

procedure TResourceHistoryForm.btnOkClick(Sender: TObject);
begin
  if edtDate.Enabled then
    FExtension.setExtensionDate('date', TDateTimeEx.make(edtDate.Date, dttzUnknown).truncToDay.toXML);
  if edtId.Enabled then
    FExtension.setExtensionString('id', edtId.Text);
  if edtAuthor.enabled then
    FExtension.setExtensionString('author', edtAuthor.Text);
  if edtOnBehalfOf.enabled then
    FExtension.setExtensionString('onBehalfOf', edtOnBehalfOf.Text);
  if cbSubst.enabled then
    FExtension.setExtensionBoolean('substantive', cbSubst.IsChecked);
  if cbBreaking.enabled then
    FExtension.setExtensionBoolean('backwardCompatible', not cbBreaking.IsChecked);
  if memNotes.enabled then
    FExtension.setExtensionString('notes', memNotes.Text);
end;

destructor TResourceHistoryForm.Destroy;
begin
  FExtension.Free;
  inherited;
end;

procedure TResourceHistoryForm.edtDateChange(Sender: TObject);
begin
  btnOk.Enabled := (not edtDate.enabled or (edtDate.Date < now + 2)) and (not edtId.enabled or (edtId.Text <> '')) and (not edtAuthor.enabled or (edtAuthor.Text <> ''));
end;

procedure TResourceHistoryForm.FormShow(Sender: TObject);
begin
  if FExtension.hasExtension('date') then
    edtDate.Date := FExtension.getExtensionByUrl('date').value.dateValue.DateTime
  else
    edtDate.Date := 0;
  edtId.Text := FExtension.getExtensionString('id');
  edtAuthor.Text := FExtension.getExtensionString('author');
  edtOnBehalfOf.Text := FExtension.getExtensionString('onBehalfOf');
  cbSubst.IsChecked := FExtension.getExtensionBoolean('substantive');
  cbBreaking.IsChecked := not FExtension.getExtensionBoolean('backwardCompatible');
  memNotes.Text := FExtension.getExtensionString('notes');
end;

procedure TResourceHistoryForm.SetExtension(const Value: TFhirExtension);
begin
  FExtension.Free;
  FExtension := Value;
end;

end.
