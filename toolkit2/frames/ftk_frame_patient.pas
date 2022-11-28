unit ftk_frame_patient;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Graphics,
  Menus, ExtDlgs, IntfGraphics, Buttons, DateTimePicker, FPImage, FPWriteBMP,
  HtmlView,
  fsl_utilities, fsl_json, fsl_crypto,
  fhir_objects, fhir_parser, fhir_healthcard, fhir_common, fui_lcl_managers,
  ftk_context, ftk_constants,
  ftk_frame_resource;

type
  TFrame = TResourceDesignerFrame;
  TPatientFrame = class;

  { THealthcardManager }

  THealthcardManager = class (TListManager<THealthcareCard>)
  private
    FFrame : TPatientFrame;
  public
    function canSort : boolean; override;
    function doubleClickEdit : boolean; override;
    function allowedOperations(item : THealthcareCard) : TNodeOperationSet; override;
    function loadList : boolean; override;

    procedure buildMenu; override;
    function getCellText(item : THealthcareCard; col : integer) : String; override;
    function getSummaryText(item : THealthcareCard) : String; override;
    function compareItem(left, right : THealthcareCard; col : integer) : integer; override;
    function filterItem(item : THealthcareCard; s : String) : boolean; override;

    function executeItem(item : THealthcareCard; mode : String) : boolean; override;
  end;

  { TGenderIdentityManager }

  TGenderIdentityManager = class (TListManager<TFHIRExtensionW>)
  private
    FFrame : TPatientFrame;
  public
    function canSort : boolean; override;
    function allowedOperations(item : TFHIRExtensionW) : TNodeOperationSet; override;
    function loadList : boolean; override;

    function getCellText(item : TFHIRExtensionW; col : integer) : String; override;
    function getSummaryText(item : TFHIRExtensionW) : String; override;
    function compareItem(left, right : TFHIRExtensionW; col : integer) : integer; override;
  end;

  { TPatientFrame }

  TPatientFrame = class(TFrame)
    btnAddGenderId: TBitBtn;
    btnAddPronoun: TBitBtn;
    btnAddSFCU: TBitBtn;
    btnAddRecord: TBitBtn;
    btnDeleteGenderId: TBitBtn;
    btnDeletePronoun: TBitBtn;
    btnDeleteSFCU: TBitBtn;
    btnDeleteRecord: TBitBtn;
    btnEditGenderid: TBitBtn;
    btnEditPronoun: TBitBtn;
    btnEditSFCU: TBitBtn;
    btnEditRecord: TBitBtn;
    btnFetchHealthCards: TButton;
    cbCovidOnly: TCheckBox;
    cbActive: TCheckBox;
    cbxGender: TComboBox;
    edtDoB: TDateTimePicker;
    edtNameSummary: TEdit;
    gbGenderIdentity: TGroupBox;
    gbPronouns: TGroupBox;
    gbSexForClinicalUse: TGroupBox;
    gbRecordedSexOrGender: TGroupBox;
    htmlCard: THtmlViewer;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    lvGenderId: TListView;
    lvPronouns: TListView;
    ListView3: TListView;
    ListView4: TListView;
    lvCards: TListView;
    mnuSaveQR: TMenuItem;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    pnlAdminGender: TPanel;
    pnlPatientDetails: TPanel;
    pbCard: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlHealthcardsOutome: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    pmImage: TPopupMenu;
    sd: TSavePictureDialog;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    procedure btnFetchHealthCardsClick(Sender: TObject);
    procedure mnuSaveQRClick(Sender: TObject);
    procedure Panel4Resize(Sender: TObject);
    procedure pbCardPaint(Sender: TObject);
    procedure TabSheet4Resize(Sender: TObject);
  private
    FCardManager : THealthcardManager;
    FGenderIdManager : TGenderIdentityManager;
    FCards : TFHIRParametersW;
    FPatient : TFhirPatientW;

    procedure DoSelectCard(sender : TObject);
  public
    destructor Destroy; override;

    procedure initialize; override;
    procedure bind; override;
    procedure saveStatus; override;
  end;

implementation

{$R *.lfm}

{ TGenderIdentityManager }

function TGenderIdentityManager.canSort: boolean;
begin
  Result := true
end;

function TGenderIdentityManager.allowedOperations(item: TFHIRExtensionW): TNodeOperationSet;
begin
  result := [opAdd, opDelete, opEdit];
end;

function TGenderIdentityManager.loadList: boolean;
var
  ext : TFHIRObject;
begin
  !
  for ext in FFrame.FPatient.extensions('http://hl7.org/fhir/StructureDefinition/individual-genderIdentity') do
    FData.add(FFrame.Context.factory(ext.fhirObjectVersion).WrapExtension(ext.link));
end;

function TGenderIdentityManager.getCellText(item: TFHIRExtensionW; col: integer): String;
var
  ext : TFHIRObject;
  ew : TFhirExtensionW;
begin
  case col of
    0: ext := item.extension('value');
    1: ext := item.extension('period');
    2: ext := item.extension('comment');
  end;
  if (ext <> nil) then
  begin
    ew := FFrame.Context.factory(ext.fhirObjectVersion).WrapExtension(ext);
    try
      result := ew.renderText;
    finally
      ew.free;
    end;
  end
  else
    result := '';
end;


function TGenderIdentityManager.getSummaryText(item: TFHIRExtensionW): String;
begin
  Result:=inherited getSummaryText(item);
end;

function TGenderIdentityManager.compareItem(left, right: TFHIRExtensionW; col: integer): integer;
begin
  Result:=inherited compareItem(left, right, col);
end;

{ THealthcardManager }

function THealthcardManager.canSort: boolean;
begin
  Result := true;
end;

function THealthcardManager.doubleClickEdit: boolean;
begin
  Result := false;
end;

function THealthcardManager.allowedOperations(item: THealthcareCard): TNodeOperationSet;
begin
  result := [opExecute];
end;

function THealthcardManager.loadList: boolean;
var
  p, pp : TFhirParametersParameterW;
  utils : THealthcareCardUtilities;
begin
  if FFrame.FCards <> nil then
  begin
    utils := THealthcareCardUtilities.create;
    try
      utils.JWKList := TJWKList.create;
      utils.Factory := FFrame.sync.Factory.link;
      for p in FFrame.FCards.parameterList do
        if p.name = 'verifiableCredential' then
          Data.add(utils.verify(p.valueString));
    finally
      utils.free;
    end;
  end;
end;

procedure THealthcardManager.buildMenu;
begin
  inherited buildMenu;
  registerMenuEntry('Open as Bundle', ICON_OPEN, copExecute);
  registerMenuEntry('Open as JWT', ICON_SIG, copExecute, 'jwt');
end;

function THealthcardManager.getCellText(item: THealthcareCard; col: integer): String;
begin
  case col of
    0: result := item.issueDate.toString;
    1: result := item.issuer;
    2: result := item.cardTypesSummary;
    3: if item.isValid then result := 'Yes' else result := 'No';
    4: result := item.summary;
  end;
end;

function THealthcardManager.getSummaryText(item: THealthcareCard): String;
begin
  Result := item.summary;
end;

function THealthcardManager.compareItem(left, right: THealthcareCard; col: integer): integer;
begin
  case col of
    0: result := left.IssueDate.compare(right.IssueDate);
    1: result := String.compare(left.Issuer, right.Issuer);
    2: result := String.compare(left.cardTypesSummary, right.cardTypesSummary);
    3: result := ord(left.isValid) - ord(right.isValid);
    4: result := String.compare(left.summary, right.summary);
  else
    result := 0;
  end;
end;

function THealthcardManager.filterItem(item: THealthcareCard; s: String): boolean;
begin
  Result := item.issuer.contains(s) or item.cardTypesSummary.contains(s) or item.summary.contains(s);
end;

function THealthcardManager.executeItem(item: THealthcareCard; mode: String): boolean;
begin
  if mode = 'jwt' then
    FFrame.context.OnOpenSource(self, TEncoding.ASCII.getBytes(item.jws), sekJWT)
  else
    FFrame.context.OnOpenResourceObj(self, item.bundle);
end;

{ TPatientFrame }

destructor TPatientFrame.Destroy;
begin
  FPatient.free;
  FCardManager.Free;
  FCards.free;
  inherited;
end;

procedure TPatientFrame.initialize;
begin
  lvCards.SmallImages := Context.images;

  FCardManager := THealthcardManager.create;
  FCardManager.Settings := Context.Settings;
  FCardManager.FFrame := self;
  FCardManager.Images := Context.images;
  FCardManager.List := lvCards;
  FCardManager.OnSetFocus := DoSelectCard;

  FGenderIdManager := TGenderIdentityManager.Create;
  FGenderIdManager.Settings := Context.Settings;
  FGenderIdManager.FFrame := self;
  FGenderIdManager.Images := Context.images;
  FGenderIdManager.List := lvGenderId;
end;

procedure TPatientFrame.bind;
begin
  FPatient := sync.Factory.wrapPatient(resource.link);
  if Client = nil then
  begin
    TabSheet2.Visible := false;
    PageControl1.ShowTabs := false;
  end;
  cbxGender.itemIndex := StringArrayIndexOf(['male', 'female', 'other', 'unknown'], FPatient.gender);
  case StringArrayIndexOf(['', 'true', 'false'], FPatient.activeStr) of
    0: cbActive.State := cbGrayed;
    1: cbActive.State := cbChecked;
    2: cbActive.State := cbUnchecked;
  end;
  edtNameSummary.text := FPatient.nameSummary;
  edtDoB.Date := TFslDateTime.fromXml(FPatient.dob).DateTime;
  FGenderIdManager.doLoad;
end;

procedure TPatientFrame.saveStatus;
begin
  inherited saveStatus;
  FCardManager.saveStatus;
  FGenderIdManager.saveStatus;
end;

procedure TPatientFrame.btnFetchHealthCardsClick(Sender: TObject);
var
  p : TFHIRParametersW;
  r : TFHIRResourceV;
  s : String;
  t : UInt64;
begin
  cursor := crHourGlass;
  try
    pnlHealthcardsOutome.caption := '  Fetching Healthcare Cards';
    Application.ProcessMessages;
    try
      FCards.Free;
      FCards := nil;
      FCardManager.doLoad;
      p := sync.Factory.makeParameters;
      try
        p.addParam('credentialType', sync.Factory.makeUri('https://smarthealth.cards#health-card'));
        if cbCovidOnly.checked then
          p.addParam('credentialType', sync.Factory.makeUri('https://smarthealth.cards#covid19'));
        t := GetTickCount64;
        r := client.operationV('Patient', FPatient.id, 'health-cards-issue', p.Resource);
        try
          FCards := sync.Factory.wrapParams(r.link);
        finally
          r.free;
        end;
        t := GetTickCount64 - t;
        FCardManager.doLoad;
        s := '  Health Cards: '+inttostr(FCardManager.Data.count)+' found';
        s := s + ' as of '+TFslDateTime.makeLocal.toXML+' (local)';
        s := s + ' ('+inttostr(t)+'ms)';
        pnlHealthcardsOutome.caption := s;
      finally
        p.free;
      end;
    except
      on e : Exception do
      begin
        pnlHealthcardsOutome.caption := '  Error: '+e.message;
        raise;
      end;
    end;
  finally
    Cursor := crDefault;
  end;
end;

procedure TPatientFrame.mnuSaveQRClick(Sender: TObject);
var
  bmp : TBitmap;
begin
  if sd.execute then
  begin
    bmp := TBitmap.create;
    try
      FCardManager.Focus.toBmp(bmp);
      bmp.SaveToFile(sd.filename);
    finally
      bmp.free;
    end;
  end;
end;

procedure TPatientFrame.Panel4Resize(Sender: TObject);
begin
end;

procedure TPatientFrame.pbCardPaint(Sender: TObject);
var
  bmp : TBitMap;
  scale : double;
begin
  pbCard.Canvas.Brush.Color := clWhite;
  pbCard.Canvas.FillRect(Rect(0, 0, pbCard.Width, pbCard.Height));
  if FCaRdManager.Focus <> nil then
  begin
    bmp := TBitmap.create;
    try
      FCardManager.Focus.toBmp(bmp);
      if (pbCard.Width < pbCard.Height) then
        scale := pbCard.Width / bmp.Width
      else
        scale := pbCard.Height / bmp.Height;
      pbCard.Canvas.StretchDraw(Rect(0, 0, Trunc(scale * bmp.Width), Trunc(scale * bmp.Height)), bmp);
    finally
      bmp.free;
    end;
  end;
end;

procedure TPatientFrame.TabSheet4Resize(Sender: TObject);
begin
  gbGenderIdentity.Height := (tabSheet4.Height - pnlAdminGender.height) div 4;
  gbPronouns.Height := (tabSheet4.Height - pnlAdminGender.height) div 4;
  gbSexForClinicalUse.Height := (tabSheet4.Height - pnlAdminGender.height) div 4;
end;

procedure TPatientFrame.DoSelectCard(sender: TObject);
begin
  if FCardManager.Focus = nil then
  begin
    htmlCard.Clear;
    mnuSaveQR.Enabled := false;
  end
  else
  begin
    htmlCard.LoadFromString(FCardManager.Focus.htmlReport(context.TxServers.defaultServer));
    mnuSaveQR.Enabled := true;
  end;
  pbCard.Invalidate;
end;

end.

