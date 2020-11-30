unit UtilitiesForm;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.strUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ImgList, fsl_utilities, Inifiles, fsl_shell,
  Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, fhir_diff, Registry, fsl_npm_cacheDialog;

type
  TForm4 = class(TForm)
    Panel1: TPanel;
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
    TabSheet4: TTabSheet;
    Panel20: TPanel;
    btnCloseCombine: TBitBtn;
    btnCombineGo: TBitBtn;
    btnStopCombine: TBitBtn;
    Panel21: TPanel;
    lblCombineAmount: TLabel;
    lblCombineAction: TLabel;
    prgCombine: TProgressBar;
    Panel22: TPanel;
    Panel23: TPanel;
    Label17: TLabel;
    Panel24: TPanel;
    Panel25: TPanel;
    Label18: TLabel;
    Label19: TLabel;
    btnInternational: TSpeedButton;
    edtInternational: TEdit;
    lbEditions: TListBox;
    btnAddEdition: TSpeedButton;
    btnDeleteEdition: TSpeedButton;
    dlgOpenCache: TFileOpenDialog;
    pnlPackageManagerLink: TPanel;
    Image1: TImage;
    pnlLoincImport: TPanel;
    Image2: TImage;
    pnlCombineSnomed: TPanel;
    Image3: TImage;
    pnlSnomedImport: TPanel;
    Image4: TImage;
    Label14: TLabel;
    edtCombinedDestination: TEdit;
    btnCombinedDestination: TSpeedButton;
    Label15: TLabel;
    edtCombinedStore: TEdit;
    btnCombinedStore: TSpeedButton;
    Label20: TLabel;
    cbUMLSDriver: TComboBox;
    cbUMLSType: TComboBox;
    Label21: TLabel;
    Label22: TLabel;
    edtLoincDate: TEdit;
    pnlProcessUMLS: TPanel;
    Image5: TImage;
    TabSheet5: TTabSheet;
    pnlPackageManager: TPanel;
    Panel26: TPanel;
    procedure btnDestinationClick(Sender: TObject);
    procedure btnSourceClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnImportSnomedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnLoincSourceClick(Sender: TObject);
    procedure btnLoincDestClick(Sender: TObject);
    procedure btnImportLoincClick(Sender: TObject);
    procedure btnProcessUMLSClick(Sender: TObject);
    procedure btnInternationalClick(Sender: TObject);
    procedure btnAddEditionClick(Sender: TObject);
    procedure lbEditionsClick(Sender: TObject);
    procedure btnDeleteEditionClick(Sender: TObject);
    procedure btnCombineGoClick(Sender: TObject);
    procedure pnlSnomedImportClick(Sender: TObject);
    procedure pnlCombineSnomedClick(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure btnCombinedDestinationClick(Sender: TObject);
    procedure btnCombinedStoreClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function GetODBCDriversList: TStrings;
    procedure cbUMLSDriverChange(Sender: TObject);
    procedure pnlPackageManagerLinkClick(Sender: TObject);
  private
    { Private declarations }
    ini : TIniFile;
    wantStop : boolean;
    running : boolean;
    function getSnomedModule: String;
    procedure sctCallback(pct: Integer; action: String);
    procedure cmbCallback(pct: Integer; action: String);
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
  fdb_manager, fdb_odbc,
  ftx_sct_importer, SnomedCombiner, ftx_sct_services,
  ftx_loinc_importer, FHIR.Tx.RxNorm;

procedure TForm4.FormCreate(Sender: TObject);
var
  page : integer;
begin
  ini := TIniFile.Create(Path([SystemTemp, 'FhirServerUtils.ini']));
  edtSource.text := ini.ReadString('snomed-import', 'source', '');
  cbxEdition.ItemIndex := ini.ReadInteger('snomed-import', 'edition', -1);
  edtDate.Date := ini.ReadInteger('snomed-import', 'date', trunc(now));
  edtDestination.text := ini.ReadString('snomed-import', 'dest', '');

  edtLoincSource.text := ini.ReadString('loinc-import', 'source', '');
  edtLoincVersion.Text := ini.ReadString('loinc-import', 'date', ''); // should not be date for is for legacy reasons
  edtLoincDest.text := ini.ReadString('loinc-import', 'dest', '');
  edtLoincDate.Text := ini.ReadString('loinc-import', 'tdate', '');

  edtUMLSServer.text := ini.ReadString('umls-process', 'server', '');
  edtUMLSDatabase.text := ini.ReadString('umls-process', 'database', '');
  edtUMLSUsername.text := ini.ReadString('umls-process', 'username', '');
  edtUMLSPassword.text := strDecrypt(ini.ReadString('umls-process', 'password', ''), GetCryptKey('umls encryption key'));

  edtInternational.text := ini.ReadString('snomed-combine', 'base', '');
  lbEditions.Items.CommaText := ini.ReadString('snomed-combine', 'editions', '');
  edtCombinedDestination.text := ini.ReadString('snomed-combine', 'dest', '');
  edtCombinedStore.text := ini.ReadString('snomed-combine', 'store', '');

  if lbEditions.Items.Count > 0 then
    lbEditions.Itemindex := 0;

  for page := 0 to PageControl1.PageCount - 1 do
    PageControl1.Pages[page].TabVisible := false;
  pnlSnomedImport.Color := rgb(217, 240, 247);
  pnlLoincImport.color := clWhite;
  pnlPackageManagerLink.color := clWhite;
  pnlCombineSnomed.color := clWhite;
  pnlProcessUMLS.color := clWhite;
  PackageCacheForm := TPackageCacheForm.create(self);
  PackageCacheForm.Parent := pnlPackageManager;
  PackageCacheForm.Align := alClient;
  PackageCacheForm.Visible := True;
  PackageCacheForm.Caption := '';
  PackageCacheForm.BorderStyle := bsNone;
  PackageCacheForm.UserMode := false;

  PageControl1.ActivePageIndex := 0;
  lbEditionsClick(self);
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  ini.Free;
end;



procedure TForm4.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm4.btnStopClick(Sender: TObject);
begin
  wantStop := true;
end;

procedure TForm4.pnlCombineSnomedClick(Sender: TObject);
begin
  if running then
    exit;
  pnlCombineSnomed.Color := rgb(217, 240, 247);
  pnlPackageManagerLink.color := clWhite;
  pnlLoincImport.color := clWhite;
  pnlSnomedImport.color := clWhite;
  pnlProcessUMLS.color := clWhite;
  PageControl1.ActivePageIndex := 1;
end;

procedure TForm4.pnlPackageManagerLinkClick(Sender: TObject);
begin
  if running then
    exit;
  pnlPackageManagerLink.Color := rgb(217, 240, 247);
  pnlLoincImport.color := clWhite;
  pnlSnomedImport.color := clWhite;
  pnlProcessUMLS.color := clWhite;
  pnlCombineSnomed.color := clWhite;
  PageControl1.ActivePageIndex := 4;
end;

procedure TForm4.pnlSnomedImportClick(Sender: TObject);
begin
  if running then
    exit;
  pnlSnomedImport.Color := rgb(217, 240, 247);
  pnlLoincImport.color := clWhite;
  pnlPackageManagerLink.color := clWhite;
  pnlCombineSnomed.color := clWhite;
  pnlProcessUMLS.color := clWhite;
  PageControl1.ActivePageIndex := 0;
end;

procedure TForm4.Image1Click(Sender: TObject);
begin
  if running then
    exit;
  pnlProcessUMLS.Color := rgb(217, 240, 247);
  pnlLoincImport.color := clWhite;
  pnlPackageManagerLink.color := clWhite;
  pnlCombineSnomed.color := clWhite;
  pnlSnomedImport.color := clWhite;
  PageControl1.ActivePageIndex := 3;
end;

procedure TForm4.Image2Click(Sender: TObject);
begin
  if running then
    exit;
  pnlLoincImport.Color := rgb(217, 240, 247);
  pnlPackageManagerLink.color := clWhite;
  pnlSnomedImport.color := clWhite;
  pnlCombineSnomed.color := clWhite;
  pnlProcessUMLS.color := clWhite;
  PageControl1.ActivePageIndex := 2;
end;

// snomed module ---------------------------------------------------------------

function TForm4.getSnomedModule : String;
begin
  case cbxEdition.itemindex of
    0 { International } : result := '900000000000207008';
    1 { US } :  result := '731000124108';
    2 { Australia } : result := '32506021000036107';
    3 { Canada } : result := '20611000087101';
    4 { Spanish } : result := '449081005';
    5 { Denmark } : result := '554471000005108';
    6 { Netherlands } : result := '11000146104';
    7 { Sweden } : result := '45991000052106';
    8 { UK } : result := '999000041000000102';
    9 { } : result := inttostr(COMBINED_MODULE_ID);
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

procedure TForm4.btnCombinedStoreClick(Sender: TObject);
begin
  if (edtCombinedStore.text <> '') then
  begin
    dlgDestination.filename := edtCombinedStore.text;
    dlgDestination.DefaultFolder := ExtractFilePath(dlgSource.filename);
  end;
  dlgDestination.Title := 'Choose Combined Files Persistent Store';
  if dlgDestination.Execute then
    edtCombinedStore.text := dlgDestination.filename;
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
    running := true;
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
      running := false;
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
    if MessageDlg('Successfully Imported SNOMED CT in '+DescribePeriod(now - start)+'. Do you want to Zip it?', mtInformation, [mbYes, mbNo], 0) = mrYes then
    begin
      ExecuteLaunch('open', 'c:\program files\7-zip\7z.exe', 'a -mx9 '+changeFileExt(edtDestination.text, '.zip')+' '+edtDestination.text, true, false);
    end;
  end;
end;

procedure TForm4.btnDestinationClick(Sender: TObject);
begin
  if (edtDestination.text <> '') then
  begin
    dlgDestination.filename := edtDestination.text;
    dlgDestination.DefaultFolder := ExtractFilePath(dlgDestination.filename);
  end;
  if dlgDestination.Execute then
    edtDestination.text := dlgDestination.filename;
end;

procedure TForm4.btnSourceClick(Sender: TObject);
begin
  if (edtSource.text <> '') then
  begin
    dlgSource.filename := edtSource.text;
    dlgSource.DefaultFolder := ExtractFilePath(dlgSource.filename);
  end;
  dlgSource.Title := 'Choose SNOMED CT RF2 Snapshot Folder';
  if dlgSource.Execute then
    edtSource.text := dlgSource.filename;
end;

// Snomed Combiner  ------------------------------------------------------------

procedure TForm4.btnInternationalClick(Sender: TObject);
begin
  if (edtInternational.text <> '') then
    dlgOpenCache.filename := edtInternational.text;
  dlgOpenCache.Title := 'Choose International SNOMED cache file';
  if dlgOpenCache.Execute then
    edtInternational.text := dlgOpenCache.filename;
end;

procedure TForm4.btnAddEditionClick(Sender: TObject);
begin
  dlgOpenCache.Title := 'Choose National SNOMED Edition cache file';
  if dlgOpenCache.Execute then
  begin
    lbEditions.Items.Add(dlgOpenCache.filename);
    lbEditions.ItemIndex := lbEditions.Items.Count - 1;
    lbEditionsClick(nil);
  end;
end;

procedure TForm4.lbEditionsClick(Sender: TObject);
begin
  btnDeleteEdition.Enabled := lbEditions.ItemIndex <> -1;
end;

procedure TForm4.btnDeleteEditionClick(Sender: TObject);
begin
  if (lbEditions.ItemIndex > -1) then
    lbEditions.Items.Delete(lbEditions.ItemIndex);
end;


procedure TForm4.cmbCallback(pct: Integer; action: String);
begin
  prgCombine.Position := pct;
  lblCombineAction.Caption := action;
  lblCombineAmount.Caption := inttostr(pct)+'%';
  prgCombine.Update;
  lblCombineAction.Update;
  lblCombineAmount.Update;
  Application.ProcessMessages;
  if (wantStop)  then
    abort;
end;

procedure TForm4.btnCombinedDestinationClick(Sender: TObject);
begin
  if (edtCombinedDestination.text <> '') then
  begin
    dlgSource.filename := edtCombinedDestination.text;
    dlgSource.DefaultFolder := ExtractFilePath(dlgSource.filename);
  end;
  dlgSource.Title := 'Choose Combined Files Destination';
  if dlgSource.Execute then
    edtCombinedDestination.text := dlgSource.filename;
end;

procedure TForm4.btnCombineGoClick(Sender: TObject);
var
  i : integer;
  start : TDateTime;
  combiner : TSnomedCombiner;
  svc : TSnomedServices;
begin
  if not FileExists(edtInternational.Text) then
    ShowMessage('International File "'+edtInternational.Text+'" not found')
  else if lbEditions.Items.Count = 0 then
    ShowMessage('Please provide some other editions')
  else
    for i := 0 to lbEditions.Items.Count - 1 do
      if not FileExists(lbEditions.Items[i]) then
      begin
        ShowMessage('Edition File "'+lbEditions.Items[i]+'" not found');
        exit;
      end;

//  else if not FileExists(edtDestination.Text) or (MessageDlg('Overwrite "'+edtDestination.Text+'"?', mtConfirmation, mbYesNo, 0) = mrYes) then
    ini.WriteString('snomed-combine', 'base', edtInternational.text);
    ini.WriteString('snomed-combine', 'editions', lbEditions.Items.CommaText);
    ini.WriteString('snomed-combine', 'dest', edtCombinedDestination.text);
    ini.WriteString('snomed-combine', 'store', edtCombinedStore.text);

    wantStop := false;
    btnStopCombine.Visible := true;
    cursor := crHourGlass;
    running := true;
    edtInternational.enabled := false;
    lbEditions.enabled := false;
    btnCombineGo.enabled := false;
    btnInternational.enabled := false;
    btnAddEdition.enabled := false;
    btnDeleteEdition.enabled := false;
    btnCloseCombine.enabled := false;
    btnCombinedDestination.enabled := false;
    edtCombinedDestination.enabled := false;
    btnCombinedStore.enabled := false;
    edtCombinedStore.enabled := false;
    try
      start := now;
      cmbCallback(0, 'Loading Editions');
      combiner := TSnomedCombiner.Create;
      try
        combiner.international := TSnomedServices.Create;
        combiner.international.Load(edtInternational.Text);
        for i := 0 to lbEditions.Items.Count - 1 do
        begin
          svc := TSnomedServices.create;
          combiner.others.Add(svc);
          svc.load(lbEditions.Items[i]);
        end;
        combiner.callback := cmbCallBack;
        combiner.destination := edtCombinedDestination.text;
        combiner.store := edtCombinedStore.text;
        combiner.Execute;
        combiner.issues.SaveToFile('c:\temp\snomed-combination-notes.txt');
        MessageDlg('Successfully Combined SNOMED CT editions in '+DescribePeriod(now - start)+':'+#13#10+combiner.summary.Text, mtInformation, [mbok], 0);
      finally
        combiner.free;
      end;
    finally
      btnStopCombine.Visible := false;
      cursor := crDefault;
      running := false;
      edtInternational.enabled := true;
      lbEditions.enabled := true;
      btnCombineGo.enabled := true;
      btnInternational.enabled := true;
      btnAddEdition.enabled := true;
      btnDeleteEdition.enabled := true;
      btnCloseCombine.enabled := true;
      btnCombinedDestination.enabled := true;
      edtCombinedDestination.enabled := true;
      btnCombinedStore.enabled := true;
      edtCombinedStore.enabled := true;
      cmbCallback(0, '');
    end;
end;

// LOINC module ----------------------------------------------------------------

procedure TForm4.btnLoincSourceClick(Sender: TObject);
begin
  if (edtLoincSource.text <> '') then
    dlgSource.filename := edtLoincSource.text;
  dlgSource.Title := 'Choose LOINC Content Folder';
  if dlgSource.Execute then
    edtLoincSource.text := dlgSource.filename;
end;

procedure TForm4.btnLoincDestClick(Sender: TObject);
begin
  if (edtLoincDest.text <> '') then
    dlgDestination.filename := edtLoincDest.text;
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
  ini.WriteString('loinc-import', 'source', edtLoincSource.text);
  ini.WriteString('loinc-import', 'date', edtLoincVersion.Text);
  ini.WriteString('loinc-import', 'tdate', edtLoincDate.Text);
  ini.WriteString('loinc-import', 'dest', edtLoincDest.text);
  if not FolderExists(edtloincSource.Text) then
    ShowMessage('Folder "'+edtSource.Text+'" not found')
  else if edtLoincDest.Text = '' then
    ShowMessage('Please Choose a Destination')
  else if (Length(edtLoincVersion.Text) <> 4) then
    ShowMessage('Please provide a version in the form X.YY')
  else if (edtLoincDate.Text = '') then
    ShowMessage('Please provide a date')
  else if not FileExists(edtLoincDest.Text) or (MessageDlg('Overwrite "'+edtLoincDest.Text+'"?', mtConfirmation, mbYesNo, 0) = mrYes) then
  begin
    start := now;

    wantStop := false;
    btnLoincImportStop.Visible := true;
    cursor := crHourGlass;
    running := true;
    edtLoincSource.enabled := false;
    edtLoincVersion.enabled := false;
    edtLoincDate.enabled := false;
    edtLoincDest.enabled := false;
    btnImportLoinc.enabled := false;
    btnCloseLoinc.enabled := false;
    btnLoincSource.enabled := false;
    btnLoincDest.enabled := false;
    try
      importLoinc(edtloincSource.Text, edtLoincVersion.Text, edtLoincDate.Text, edtLoincDest.text, loincCallBack);
    finally
      cursor := crDefault;
      btnUMLSStop.Visible := false;
      running := false;
      edtLoincSource.enabled := true;
      edtLoincVersion.enabled := true;
      edtLoincDate.enabled := true;
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
  db : TFDBManager;
  start : TDateTime;
begin
  if cbUMLSDriver.Text = '' then
    ShowMessage('No Database Driver specified') else
  if not MatchText(cbUMLSType.Text, ['mssql', 'mysql']) then
    ShowMessage('No valid Server Type specified') else
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
    running := true;
    edtUMLSServer.enabled := false;
    edtUMLSDatabase.enabled := false;
    edtUMLSUsername.enabled := false;
    edtUMLSPassword.enabled := false;
    btnProcessUMLS.enabled := false;
    btnUMLSClose.enabled := false;
    try

      db := TFDBOdbcManager.create('umls', 4, 0, cbUMLSDriver.Text, edtUMLSServer.text, edtUMLSDatabase.Text, edtUMLSUsername.Text, edtUMLSPassword.Text);
      generateRxStems(db, umlsCallback);
    finally
      cursor := crDefault;
      btnUMLSStop.Visible := false;
      running := false;
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


procedure TForm4.cbUMLSDriverChange(Sender: TObject);
begin
//
cbUMLSType.itemindex:=-1;
if (pos('MySQL',cbUMLSDriver.text)<>0) then cbUMLSType.itemIndex:=1;
if (pos('SQL Server',cbUMLSDriver.text)<>0) then cbUMLSType.itemIndex:=0;

end;


procedure TForm4.FormShow(Sender: TObject);
begin
cbUMLSDriver.items.Assign(GetODBCDriversList);
end;


function TForm4.GetODBCDriversList: TStrings;
var
  aStringlist   : TStringlist;
  aRegistry   : TRegistry;
Begin
  aStringlist:= Tstringlist.Create;
  aRegistry:= TRegistry.Create;
  Result:= Tstringlist.Create;

  with aRegistry do
  Begin
    rootkey:= HKEY_LOCAL_MACHINE;
    OpenKey('Software\ODBC\ODBCINST.INI\ODBC Drivers',False);
    GetValueNames(aStringlist);
  End;
  aRegistry.Free;
  aStringlist.Sort;
  result.AddStrings(aStringlist);
  aStringlist.Free;
End;




end.
