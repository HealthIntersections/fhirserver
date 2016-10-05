unit UtilitiesForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, FileSupport, SystemSupport, Inifiles;

type
  TForm4 = class(TForm)
    Panel1: TPanel;
    ListView1: TListView;
    ImageList1: TImageList;
    dlgSource: TFileOpenDialog;
    dlgDestination: TFileSaveDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel2: TPanel;
    Panel3: TPanel;
    btnSnomedImportClose: TBitBtn;
    btnImportSnomed: TBitBtn;
    btnSnomedImportStop: TBitBtn;
    Panel4: TPanel;
    lblSCTAmount: TLabel;
    lblSCTAction: TLabel;
    prgSnomedImport: TProgressBar;
    Panel5: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    btnSource: TSpeedButton;
    btnDestination: TSpeedButton;
    edtSource: TEdit;
    cbxEdition: TComboBox;
    edtDate: TDateTimePicker;
    edtDestination: TEdit;
    Panel6: TPanel;
    Panel7: TPanel;
    Label6: TLabel;
    Panel8: TPanel;
    Panel9: TPanel;
    Label7: TLabel;
    Panel10: TPanel;
    Panel11: TPanel;
    lblLoincAmount: TLabel;
    lblLoincAction: TLabel;
    prgLoincImport: TProgressBar;
    Panel12: TPanel;
    btnCloseLoinc: TBitBtn;
    btnImportLoinc: TBitBtn;
    btnLoincImportStop: TBitBtn;
    Panel13: TPanel;
    Label10: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    btnLoincSource: TSpeedButton;
    btnLoincDest: TSpeedButton;
    edtLoincSource: TEdit;
    edtLoincDest: TEdit;
    edtLoincVersion: TEdit;
    TabSheet3: TTabSheet;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Label1: TLabel;
    Panel17: TPanel;
    Label8: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    edtUMLSServer: TEdit;
    edtUMLSUsername: TEdit;
    edtUMLSDatabase: TEdit;
    Panel18: TPanel;
    lblUMLSAmount: TLabel;
    lblUMLSAction: TLabel;
    prgUMLSImport: TProgressBar;
    Panel19: TPanel;
    btnUMLSClose: TBitBtn;
    btnProcessUMLS: TBitBtn;
    btnUMLSStop: TBitBtn;
    Label16: TLabel;
    edtUMLSPassword: TEdit;
    procedure btnDestinationClick(Sender: TObject);
    procedure btnSourceClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnImportSnomedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure btnLoincSourceClick(Sender: TObject);
    procedure btnLoincDestClick(Sender: TObject);
    procedure btnImportLoincClick(Sender: TObject);
    procedure btnProcessUMLSClick(Sender: TObject);
  private
    { Private declarations }
    ini : TIniFile;
    wantStop : boolean;
    function getSnomedModule: String;
    procedure sctCallback(pct: Integer; action: String);
    procedure loincCallback(pct: Integer; action: String);
    procedure umlsCallback(pct: Integer; action: String);
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

Uses
  EncodeSupport, DateSupport,
  KDBManager, KDBOdbcExpress,
  SnomedImporter, LoincImporter, RxNormServices;

procedure TForm4.FormCreate(Sender: TObject);
var
  page : integer;
begin
  ini := TIniFile.Create(IncludeTrailingBackslash(SystemTemp)+'FhirServerUtils.ini');
  edtSource.text := ini.ReadString('snomed-import', 'source', '');
  cbxEdition.ItemIndex := ini.ReadInteger('snomed-import', 'edition', -1);
  edtDate.Date := ini.ReadInteger('snomed-import', 'date', trunc(now));
  edtDestination.text := ini.ReadString('snomed-import', 'dest', '');

  edtLoincSource.text := ini.ReadString('loinc-import', 'source', '');
  edtLoincVersion.Text := ini.ReadString('loinc-import', 'date', '');
  edtLoincDest.text := ini.ReadString('loinc-import', 'dest', '');

  edtUMLSServer.text := ini.ReadString('umls-process', 'server', '');
  edtUMLSDatabase.text := ini.ReadString('umls-process', 'database', '');
  edtUMLSUsername.text := ini.ReadString('umls-process', 'username', '');
  edtUMLSPassword.text := strDecrypt(ini.ReadString('umls-process', 'password', ''), GetCryptKey('umls encryption key'));

  for page := 0 to PageControl1.PageCount - 1 do
    PageControl1.Pages[page].TabVisible := false;
  ListView1.ItemIndex := 0;
  PageControl1.ActivePageIndex := 0;
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  ini.Free;
end;

procedure TForm4.ListView1Click(Sender: TObject);
begin
  PageControl1.ActivePageIndex := ListView1.ItemIndex;
end;

procedure TForm4.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm4.btnStopClick(Sender: TObject);
begin
  wantStop := true;
end;

// snomed module ---------------------------------------------------------------

function TForm4.getSnomedModule : String;
begin
  case cbxEdition.itemindex of
    0 { International } : result := '900000000000207008';
    1 { US } :  result := '731000124108';
    2 { Australia } : result := '32506021000036107';
    3 { Spanish } : result := '449081005';
    4 { Denmark } : result := '554471000005108';
    5 { Netherlands } : result := '11000146104';
    6 { Sweden } : result := '45991000052106';
    7 { UK } : result := '999000041000000102';
  end;
end;

procedure TForm4.sctCallback(pct: Integer; action: String);
begin
  prgSnomedImport.Position := pct;
  lblSCTAction.Caption := action;
  lblSCTAmount.Caption := inttostr(pct)+'%';
  prgSnomedImport.Update;
  lblSCTAction.Update;
  lblSCTAmount.Update;
  Application.ProcessMessages;
  if (wantStop)  then
    abort;
end;

procedure TForm4.btnImportSnomedClick(Sender: TObject);
var
  module, version : String;
  start : TDateTime;
begin
  if not FolderExists(edtSource.Text) then
    ShowMessage('Folder "'+edtSource.Text+'" not found')
  else if edtDestination.Text = '' then
    ShowMessage('Please Choose a Destination')
  else if cbxEdition.ItemIndex = -1 then
    ShowMessage('Please Choose an Edition')
  else if not FileExists(edtDestination.Text) or (MessageDlg('Overwrite "'+edtDestination.Text+'"?', mtConfirmation, mbYesNo, 0) = mrYes) then
  begin
    ini.WriteString('snomed-import', 'source', edtSource.text);
    ini.WriteInteger('snomed-import', 'edition', cbxEdition.ItemIndex);
    ini.WriteInteger('snomed-import', 'date', trunc(edtDate.Date));
    ini.WriteString('snomed-import', 'dest', edtDestination.text);
    module := getSnomedModule;
    version := FormatDateTime('yyyymmdd', edtDate.Date);
    wantStop := false;
    btnSnomedImportStop.Visible := true;
    cursor := crHourGlass;
    listview1.enabled := false;
    edtSource.enabled := false;
    cbxEdition.enabled := false;
    edtDate.enabled := false;
    edtDestination.enabled := false;
    btnImportSnomed.enabled := false;
    btnSnomedImportClose.enabled := false;
    btnSource.enabled := false;
    btnDestination.enabled := false;
    try
      start := now;
      importSnomedRF2(edtSource.text, edtDestination.text, 'http://snomed.info/sct/'+module+'/version/'+version, sctCallback);
    finally
      cursor := crDefault;
      btnSnomedImportStop.Visible := false;
      listview1.enabled := true;
      edtSource.enabled := true;
      cbxEdition.enabled := true;
      edtDate.enabled := true;
      edtDestination.enabled := true;
      btnImportSnomed.enabled := true;
      btnSnomedImportClose.enabled := true;
      btnSource.enabled := true;
      btnDestination.enabled := true;
      sctCallback(0, '');
    end;
    MessageDlg('Successfully Imported SNOMED CT in '+DescribePeriod(now - start), mtInformation, [mbok], 0);
  end;
end;

procedure TForm4.btnDestinationClick(Sender: TObject);
begin
  if dlgDestination.Execute then
    edtDestination.text := dlgDestination.filename;
end;

procedure TForm4.btnSourceClick(Sender: TObject);
begin
  dlgSource.Title := 'Choose SNOMED CT RF2 Snapshot Folder';
  if dlgSource.Execute then
    edtSource.text := dlgSource.filename;
end;

// LOINC module ----------------------------------------------------------------

procedure TForm4.btnLoincSourceClick(Sender: TObject);
begin
  dlgSource.Title := 'Choose LOINC Content Folder';
  if dlgSource.Execute then
    edtLoincSource.text := dlgSource.filename;
end;

procedure TForm4.btnLoincDestClick(Sender: TObject);
begin
  if dlgDestination.Execute then
    edtLoincDest.text := dlgDestination.filename;
end;

procedure TForm4.loincCallback(pct: Integer; action: String);
begin
  prgLoincImport.Position := pct;
  lblLoincAction.Caption := action;
  lblLoincAmount.Caption := inttostr(pct)+'%';
  prgLoincImport.Update;
  lblLoincAction.Update;
  lblLoincAmount.Update;
  Application.ProcessMessages;
  if (wantStop)  then
    abort;
end;

procedure TForm4.btnImportLoincClick(Sender: TObject);
var
  start : TDateTime;
begin
  if not FolderExists(edtloincSource.Text) then
    ShowMessage('Folder "'+edtSource.Text+'" not found')
  else if edtDestination.Text = '' then
    ShowMessage('Please Choose a Destination')
  else if (Length(edtLoincVersion.Text) <> 4) then
    ShowMessage('Please provide a version in the form X.YY')
  else if not FileExists(edtLoincDest.Text) or (MessageDlg('Overwrite "'+edtLoincDest.Text+'"?', mtConfirmation, mbYesNo, 0) = mrYes) then
  begin
    ini.WriteString('loinc-import', 'source', edtLoincSource.text);
    ini.WriteString('loinc-import', 'date', edtLoincVersion.Text);
    ini.WriteString('loinc-import', 'dest', edtLoincDest.text);

    wantStop := false;
    btnLoincImportStop.Visible := true;
    cursor := crHourGlass;
    listview1.enabled := false;
    edtLoincSource.enabled := false;
    edtLoincVersion.enabled := false;
    edtLoincDest.enabled := false;
    btnImportLoinc.enabled := false;
    btnCloseLoinc.enabled := false;
    btnLoincSource.enabled := false;
    btnLoincDest.enabled := false;
    try
      importLoinc(edtloincSource.Text, edtLoincVersion.Text, edtLoincDest.text, loincCallBack);
    finally
      cursor := crDefault;
      btnUMLSStop.Visible := false;
      listview1.enabled := true;
      edtLoincSource.enabled := true;
      edtLoincVersion.enabled := true;
      edtLoincDest.enabled := true;
      btnImportLoinc.enabled := true;
      btnCloseLoinc.enabled := true;
      btnLoincSource.enabled := true;
      btnLoincDest.enabled := true;
      loincCallback(0, '');
    end;
    MessageDlg('Successfully Imported LOINC in '+DescribePeriod(now - start), mtInformation, [mbok], 0);
  end;
end;


// RxNorm module ---------------------------------------------------------------

procedure TForm4.btnProcessUMLSClick(Sender: TObject);
var
  db : TKDBManager;
  start : TDateTime;
begin
  if edtUMLSServer.Text = '' then
    ShowMessage('No Server specified')
  else if edtUMLSDatabase.Text = '' then
    ShowMessage('No Database specified')
  else if (edtUMLSUsername.Text = '') xor (edtUMLSPassword.Text = '') then
    ShowMessage('Plase specify both a username and password, or neither')
  else
  begin
    ini.WriteString('umls-process', 'server', edtUMLSServer.text);
    ini.WriteString('umls-process', 'database', edtUMLSDatabase.text);
    ini.WriteString('umls-process', 'username', edtUMLSUsername.text);
    ini.WriteString('umls-process', 'password', strEncrypt(edtUMLSPassword.text, GetCryptKey('umls encryption key')));

    wantStop := false;
    btnUMLSStop.Visible := true;
    cursor := crHourGlass;
    listview1.enabled := false;
    edtUMLSServer.enabled := false;
    edtUMLSDatabase.enabled := false;
    edtUMLSUsername.enabled := false;
    edtUMLSPassword.enabled := false;
    btnProcessUMLS.enabled := false;
    btnUMLSClose.enabled := false;
    try
      db := TKDBOdbcDirect.create('umls', 4, 0, 'SQL Server Native Client 11.0', edtUMLSServer.text, edtUMLSDatabase.Text, edtUMLSUsername.Text, edtUMLSPassword.Text);
      generateRxStems(db, umlsCallback);
    finally
      cursor := crDefault;
      btnUMLSStop.Visible := false;
      listview1.enabled := true;
      edtUMLSServer.enabled := true;
      edtUMLSDatabase.enabled := true;
      edtUMLSUsername.enabled := true;
      edtUMLSPassword.enabled := true;
      btnProcessUMLS.enabled := true;
      btnUMLSClose.enabled := true;
      umlsCallback(0, '');
    end;
    MessageDlg('Successfully Process UMLS Entries in '+DescribePeriod(now - start), mtInformation, [mbok], 0);
  end;
end;

procedure TForm4.umlsCallback(pct: Integer; action: String);
begin
  prgUMLSImport.Position := pct;
  lblUMLSAction.Caption := action;
  lblUMLSAmount.Caption := inttostr(pct)+'%';
  prgUMLSImport.Update;
  lblUMLSAction.Update;
  lblUMLSAmount.Update;
  Application.ProcessMessages;
  if (wantStop)  then
    abort;
end;





end.
