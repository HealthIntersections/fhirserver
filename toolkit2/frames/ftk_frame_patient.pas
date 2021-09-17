unit ftk_frame_patient;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Graphics,
  Menus, ExtDlgs, IntfGraphics, FPImage, FPWritePNG,
  DelphiZXingQRCode, HtmlView,
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

  { TPatientFrame }

  TPatientFrame = class(TFrame)
    btnFetchHealthCards: TButton;
    cbCovidOnly: TCheckBox;
    htmlCard: THtmlViewer;
    lvCards: TListView;
    mnuSaveQR: TMenuItem;
    PageControl1: TPageControl;
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
    procedure btnFetchHealthCardsClick(Sender: TObject);
    procedure mnuSaveQRClick(Sender: TObject);
    procedure Panel4Resize(Sender: TObject);
    procedure pbCardPaint(Sender: TObject);
  private
    FCardManager : THealthcardManager;
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
end;

procedure TPatientFrame.bind;
begin
  FPatient := sync.Factory.wrapPatient(resource.link);
  if Client = nil then
  begin
    TabSheet2.Visible := false;
    PageControl1.ShowTabs := false;
  end;
end;

procedure TPatientFrame.saveStatus;
begin
  inherited saveStatus;
  FCardManager.saveStatus;
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
  mem : TMemoryStream;
  picture : TPicture;
begin
  if sd.execute then
  begin
    mem := TMemoryStream.create;
    try
      bmp := TBitmap.create;
      try
        makeQRCode(bmp, qrAlphanumeric, FCardManager.Focus.qrSource);
        bmp.SaveToStream(mem);
      finally
        bmp.free;
      end;
      mem.position := 0;
      picture := TPicture.create;
      try
        picture.LoadFromStreamWithFileExt(mem, '.bmp');
        picture.SaveToFile(sd.FileName);
      finally
        picture.Free;
      end;
    finally
      mem.free;
    end;
  end;
end;

procedure TPatientFrame.Panel4Resize(Sender: TObject);
begin
end;

procedure TPatientFrame.pbCardPaint(Sender: TObject);
var
  bmp : TBitmap;
  scale : double;
begin
  pbCard.Canvas.Brush.Color := clWhite;
  pbCard.Canvas.FillRect(Rect(0, 0, pbCard.Width, pbCard.Height));
  if FCardManager.Focus <> nil then
  begin
    bmp := TBitmap.create;
    try
      makeQRCode(bmp, qrAlphanumeric, FCardManager.Focus.qrSource);
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

procedure TPatientFrame.DoSelectCard(sender: TObject);
begin
  if FCardManager.Focus = nil then
  begin
    htmlCard.Clear;
    mnuSaveQR.Enabled := false;
  end
  else
  begin
    htmlCard.LoadFromString(FCardManager.Focus.htmlReport(context.TerminologyService));
    mnuSaveQR.Enabled := true;
  end;
  pbCard.Invalidate;
end;

end.

