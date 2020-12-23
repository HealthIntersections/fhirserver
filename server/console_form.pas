unit console_form;

{
Copyright (c) 2020+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF WINDOWS}Windows, {$ENDIF}
  Classes, SysUtils, StrUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, Registry, Types,
  ExtCtrls, Menus, ActnList, StdActns, Buttons, DateTimePicker, Interfaces,
  IniFiles, Math,
  IdTelnet, IdGlobal,
  fsl_base, fsl_threads, fsl_fpc,  fsl_utilities, fsl_logging, fsl_npm_client, fsl_openssl,
  fdb_odbc_fpc, fdb_manager, fdb_odbc, fdb_dialects,
  ftx_sct_combiner, ftx_sct_services, ftx_sct_importer, ftx_loinc_importer, tx_ndc, tx_rxnorm,
  fui_lcl_managers,
  server_config,
  console_managers;

const
   DEF_PASSWORD = 'AA8FF8CC-81C8-41D7-93BA-26AD5E89A1C1';

type
  TConnectionStatus = (csDiconnected, csUsername, csPassword, csConnected, csEnhanced);

  { TConnectingThread }

  TConnectingThread = class (TFslThread)
  protected
    procedure Initialise; override;
    procedure Execute; override;
  end;

  { TPackageClientThread }

  TPackageClientThread = class (TFslThread)
  protected
    procedure Execute; override;
  end;

  { TServerSession }

  TServerSession = class (TFslObject)
  private
    FLog : TStringList;
    FStart : int64;
    FLocal : TDateTime;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TServerSessionStatistics }

  TServerSessionStatistics = class (TFslObject)
  private
    FCursor : integer;
    FStarts : array [0..1000] of int64;
    FLengths : array [0..1000] of int64;
    FTotal : integer;
    FLength : int64;
    FCounts : array [0..9] of integer;
    FStart : int64;
  public
    constructor Create;
    procedure recordSession(start, length : int64);
    function report : String;
  end;


  { TMainConsoleForm }

  TMainConsoleForm = class(TForm)
    BitBtn1: TBitBtn;
    btnTxImport: TBitBtn;
    btnIDAdd: TBitBtn;
    btnIDDelete: TBitBtn;
    btnTxAdd: TBitBtn;
    btnEPAdd: TBitBtn;
    btnTxDelete: TBitBtn;
    btnAddEdition: TSpeedButton;
    btnBase: TSpeedButton;
    btnCombinedDestination: TSpeedButton;
    btnCombinedStore: TSpeedButton;
    btnCombineGo: TBitBtn;
    btnDeleteEdition: TSpeedButton;
    btnDestination: TSpeedButton;
    btnFetchObjects: TButton;
    btnFetchObjectsPlus: TButton;
    btnImportLoinc: TBitBtn;
    btnImportSnomed: TBitBtn;
    btnInternational: TSpeedButton;
    btnLoincDest: TSpeedButton;
    btnLoincImportStop: TBitBtn;
    btnLoincSource: TSpeedButton;
    btnSnomedImportStop: TBitBtn;
    btnSource: TSpeedButton;
    btnStopCombine: TBitBtn;
    btnEPDelete: TBitBtn;
    btnEPInstall: TBitBtn;
    btnFetchThreads: TButton;
    btnClearCache: TButton;
    cbxEdition: TComboBox;
    chkWebMode: TCheckBox;
    edtCACert: TEdit;
    edtConfigFile: TEdit;
    edtBase: TEdit;
    edtCombinedDestination: TEdit;
    edtCombinedStore: TEdit;
    edtDate: TDateTimePicker;
    edtDestination: TEdit;
    edtAdminSCIMSalt: TEdit;
    edtTelnetPassword: TEdit;
    edtInternational: TEdit;
    edtLoincDate: TEdit;
    edtLoincDest: TEdit;
    edtLoincSource: TEdit;
    edtLoincVersion: TEdit;
    edtPrivateKey: TEdit;
    edtSource: TEdit;
    edtSSLCert: TEdit;
    edtSSLPassword: TEdit;
    edtSSLPort: TEdit;
    edtHostName: TEdit;
    edtWebPort: TEdit;
    edtGoogleId: TEdit;
    edtAdminEmail: TEdit;
    edtAdminOrganization: TEdit;
    edtAdminSMS: TEdit;
    FileNewAction: TAction;
    ActionList1: TActionList;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditDelete1: TEditDelete;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    edtFilter: TEdit;
    FileExit1: TFileExit;
    FileOpenAction: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox6: TGroupBox;
    HelpContents1: THelpContents;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    ImageList1: TImageList;
    lblDoco: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    lblSomething: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lbEditions: TListBox;
    lblCombineAction: TLabel;
    lblCombineAmount: TLabel;
    lblLoincAction: TLabel;
    lblLoincAmount: TLabel;
    lblSCTAction: TLabel;
    lblSCTAmount: TLabel;
    lvPackages: TListView;
    lvID: TListView;
    lvTx: TListView;
    lvEP: TListView;
    MainMenu1: TMainMenu;
    mConsole: TMemo;
    MenuItem10: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    N1: TMenuItem;
    mThreads: TMemo;
    MenuItem1: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    mStats: TMemo;
    N4: TMenuItem;
    N8: TMenuItem;
    dlgOpen: TOpenDialog;
    dlgConfig: TOpenDialog;
    pgMain: TPageControl;
    pgManage: TPageControl;
    Panel1: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel29: TPanel;
    Panel30: TPanel;
    Panel33: TPanel;
    Panel35: TPanel;
    Panel37: TPanel;
    Panel38: TPanel;
    Panel39: TPanel;
    pgTerminologies: TPageControl;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel2: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel22: TPanel;
    Panel23: TPanel;
    Panel24: TPanel;
    Panel25: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pnlCombineSnomed: TPanel;
    pnlLoincImport: TPanel;
    pnlProcessUMLS: TPanel;
    pnlSnomedImport: TPanel;
    prgCombine: TProgressBar;
    prgLoincImport: TProgressBar;
    prgSnomedImport: TProgressBar;
    dlgFolder: TSelectDirectoryDialog;
    dlgSave: TSaveDialog;
    sBar: TStatusBar;
    btnCert: TSpeedButton;
    btnCACert: TSpeedButton;
    btnCertKey: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    Splitter5: TSplitter;
    tbWebSettings: TTabSheet;
    tbUserAdmin: TTabSheet;
    tbEndPoints: TTabSheet;
    tbTermload: TTabSheet;
    tbConsole: TTabSheet;
    tbStatistics: TTabSheet;
    tbManage: TTabSheet;
    tbTerminologies: TTabSheet;
    tbThreads: TTabSheet;
    tbSnomed: TTabSheet;
    tbSnomedCombine: TTabSheet;
    tbLoinc: TTabSheet;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure BitBtn1Click(Sender: TObject);
    procedure btnAddEditionClick(Sender: TObject);
    procedure btnBaseClick(Sender: TObject);
    procedure btnCACertClick(Sender: TObject);
    procedure btnCertClick(Sender: TObject);
    procedure btnCertKeyClick(Sender: TObject);
    procedure btnClearCacheClick(Sender: TObject);
    procedure btnCombinedDestinationClick(Sender: TObject);
    procedure btnCombinedStoreClick(Sender: TObject);
    procedure btnCombineGoClick(Sender: TObject);
    procedure btnDeleteEditionClick(Sender: TObject);
    procedure btnDestinationClick(Sender: TObject);
    procedure btnFetchObjectsClick(Sender: TObject);
    procedure btnFetchObjectsPlusClick(Sender: TObject);
    procedure btnImportLoincClick(Sender: TObject);
    procedure btnImportSnomedClick(Sender: TObject);
    procedure btnInternationalClick(Sender: TObject);
    procedure btnLoincDestClick(Sender: TObject);
    procedure btnLoincImportStopClick(Sender: TObject);
    procedure btnLoincSourceClick(Sender: TObject);
    procedure btnNDCClick(Sender: TObject);
    procedure btnSnomedImportStopClick(Sender: TObject);
    procedure btnSourceClick(Sender: TObject);
    procedure btnStopCombineClick(Sender: TObject);
    procedure btnUMLSStopClick(Sender: TObject);
    procedure btnFetchThreadsClick(Sender: TObject);
    procedure cbUMLSDriverChange(Sender: TObject);
    procedure cbxEditionChange(Sender: TObject);
    procedure chkWebModeChange(Sender: TObject);
    procedure edtAdminEmailChange(Sender: TObject);
    procedure edtAdminOrganizationChange(Sender: TObject);
    procedure edtAdminSCIMSaltChange(Sender: TObject);
    procedure edtAdminSMSChange(Sender: TObject);
    procedure edtCACertChange(Sender: TObject);
    procedure edtConfigFileChange(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    procedure edtGoogleIdChange(Sender: TObject);
    procedure edtHostNameChange(Sender: TObject);
    procedure edtPrivateKeyChange(Sender: TObject);
    procedure edtSSLCertChange(Sender: TObject);
    procedure edtSSLPasswordChange(Sender: TObject);
    procedure edtSSLPortChange(Sender: TObject);
    procedure edtTelnetPasswordChange(Sender: TObject);
    procedure edtWebPortChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure Image3Click(Sender: TObject);
    procedure Image4Click(Sender: TObject);
    procedure Image5Click(Sender: TObject);
    procedure lbEditionsClick(Sender: TObject);
    procedure lvPackagesItemChecked(Sender: TObject; Item: TListItem);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure pnlSnomedImportClick(Sender: TObject);
    procedure tbConsoleContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure tbTerminologiesContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
  private
    FLock : TFslLock;
    FTelnet: TIdTelnet;
    FConnected : boolean;
    FIncoming : TStringList;
    FThreads : TStringList;
    FServerStatus : String;
    FLines : TStringList;
    FStatistics : TServerSessionStatistics;
    FLastIncoming : TDateTime;
    FStatus : TConnectionStatus;
    FThread : TConnectingThread;
    FPackageThread : TPackageClientThread;
    FAddress : String;
    FPassword : String;
    FFilter : String;
    FIni : TIniFile;
    FConfig : TFHIRServerConfigFile;
    FWantStop : boolean;
    FRunning : boolean;
    FLoading : boolean;

    FTxManager : TTxManager;
    FEPManager : TEndPointManager;
    FIDManager : TIdentityProviderManager;
    FPackages : TFslList<TFHIRPackageInfo>;
    FThreadPackages : TFslList<TFHIRPackageInfo>;

    procedure recordSessionLength(start, length : int64);
    procedure DoIncoming(Sender: TIdTelnet; const Buffer: TIdBytes);
    procedure DoConnected(Sender: TObject);
    procedure DoDisconnected(Sender: TObject);
    procedure processIncomingLine(line: String);
    function passesFilter(line: String) : boolean;
    function handleCommand(line: String) : boolean;
    procedure Connect;
    procedure sctCallback(pct: Integer; action: String);
    procedure cmbCallback(pct: Integer; action: String);
    procedure loincCallback(pct: Integer; action: String);
    Procedure SetUpTerminologyPage;
    function getSnomedModule: String;
    procedure connectToServer(server : String);
    procedure GetODBCDriversList(list : TStrings);
    procedure SetConfigEditable;
    procedure SetConfigReadonly;
    procedure EPFocusChange(sender : TObject);
    procedure updateDoco;
  public
    property Packages : TFslList<TFHIRPackageInfo> read FPackages;
  end;

var
  MainConsoleForm: TMainConsoleForm;

implementation

{$R *.lfm}

uses
  console_server_form;

{ TPackageClientThread }

procedure TPackageClientThread.Execute;
var
  client : TFHIRPackageClient;
  list : TFslList<TFHIRPackageInfo>;
begin
  client := TFHIRPackageClient.create(PACKAGE_SERVER_BACKUP);
  try
    list := client.search('', '', '', false);
    MainConsoleForm.Flock.Lock;
    try
      MainConsoleForm.FThreadPackages := list;
    finally
      MainConsoleForm.Flock.UnLock;
    end;
  finally
    client.Free;
  end;
end;

{ TServerSessionStatistics }

constructor TServerSessionStatistics.Create;
begin
  inherited Create;
  FStart := GetTickCount64;
end;

procedure TServerSessionStatistics.recordSession(start, length: int64);
begin
  inc(FCursor);
  if (FCursor = 1000) then
    FCursor := 0;
  FStarts[FCursor] := start;
  FLengths[FCursor] := length;
  inc(FTotal);
  inc(FLength, length);
  if      (length <=   100) then inc(FCounts[0])
  else if (length <=   500) then inc(FCounts[1])
  else if (length <=  1000) then inc(FCounts[2])
  else if (length <=  2000) then inc(FCounts[3])
  else if (length <=  4000) then inc(FCounts[4])
  else if (length <=  8000) then inc(FCounts[5])
  else if (length <= 16000) then inc(FCounts[6])
  else if (length <= 32000) then inc(FCounts[7])
  else if (length <= 48000) then inc(FCounts[8])
  else {if (length <= 100) then} inc(FCounts[9])
end;

function TServerSessionStatistics.report: String;
var
  i, t, c : integer;
  latest, span, length : int64;

begin
  result := 'Total Requests: '+inttostr(FTotal)+#13#10;
  if (FTotal > 0) then
  begin
    result := result + 'Avg Length: '+FloatToStrF(FLength / FTotal, ffFixed, 2, 2)+'ms'+#13#10;
    t := FCursor;
    latest := FStarts[t];
    length := 0;
  end;
  if (FTotal > 1) then
  begin
    c := Math.Min(20, FTotal);
    result := result + 'Last '+inttostr(c)+' Requests: '+#13#10;
    for i := 1 to c do
    begin
      length := length + FLengths[t];
      span := latest - FStarts[t];
      dec(t);
      if t < 0 then
        t := 999;
    end;
    result := result + '  Frequency: '+FloatToStrF((c * 1000) / span , ffFixed, 2, 2)+'hz'+#13#10;
    result := result + '  Avg Length: '+FloatToStrF(length / c, ffFixed, 2, 2)+'ms'+#13#10;
  end;
  result := result + 'Frequency (total): '+FloatToStrF((FTotal * 1000)/ (GetTickCount64 - FStart), ffFixed, 2, 2)+'hz'+#13#10;
  result := result + #13#10+'Histogram (seconds): '+#13#10;
  result := result + '  0 - 0.1: '+inttostr(FCounts[0])+#13#10;
  result := result + '0.1 - 0.5: '+inttostr(FCounts[1])+#13#10;
  result := result + '0.5 - 1.0: '+inttostr(FCounts[2])+#13#10;
  result := result + '  1 -  2: '+inttostr(FCounts[3])+#13#10;
  result := result + '  2 -  4: '+inttostr(FCounts[4])+#13#10;
  result := result + '  4 -  8: '+inttostr(FCounts[5])+#13#10;
  result := result + '  8 - 16: '+inttostr(FCounts[6])+#13#10;
  result := result + ' 16 - 32: '+inttostr(FCounts[7])+#13#10;
  result := result + ' 32 - 48: '+inttostr(FCounts[8])+#13#10;
  result := result + ' >48    : '+inttostr(FCounts[9])+#13#10;
end;

{ TServerSession }

constructor TServerSession.Create;
begin
  inherited Create;
  FLog := TStringList.create;
  FLocal := Now;
end;

destructor TServerSession.Destroy;
begin
  FLog.Free;
  inherited Destroy;
end;

{ TConnectingThread }

procedure TConnectingThread.Initialise;
begin
  TimePeriod := 50;
end;

procedure TConnectingThread.Execute;
begin
  try
    MainConsoleForm.Connect;
  except
  end;
end;


{ TMainConsoleForm }

procedure TMainConsoleForm.FormCreate(Sender: TObject);
var
  s : String;
begin
  GBackgroundTasks.start;

  s := getAppConfigDir(false);
  FIni := TIniFile.create(path([s, 'FHIRConsole.ini']));
  FAddress := FIni.ReadString('console', 'address', 'Localhost');
  FPassword := FIni.ReadString('console', 'password', DEF_PASSWORD); // this password only works from localhost

  FTelnet := TIdTelnet.create(nil);
  FTelnet.Port := 44123;
  FTelnet.ThreadedEvent := true;
  FTelnet.OnConnected := DoConnected;
  FTelnet.onDisconnected := DoDisconnected;
  FTelnet.OnDataAvailable := DoIncoming;

  setupTerminologyPage;
  FStatus := csDiconnected;
  FIncoming := TStringList.create;
  FThreads := TStringList.create;;
  FLines := TStringList.create;
  FStatistics := TServerSessionStatistics.create;
  FLock := TFslLock.create('incoming');
  FThread := TConnectingThread.create;
  FThread.Start;

  FPackages := TFslList<TFHIRPackageInfo>.create;
  FPackageThread := TPackageClientThread.create;
  FPackageThread.Start;

  FTxManager := TTxManager.create;
  FTxManager.Settings := FIni;
  FTxManager.Images := ImageList1;
  FTxManager.List := lvTx;
  FTxManager.registerControl(btnTxAdd, copAdd);
  FTxManager.registerControl(btnTxDelete, copDelete);
  FTxManager.registerControl(btnTxImport, copExecute);

  FEPManager := TEndPointManager.create;
  FEPManager.Settings := FIni;
  FEPManager.Images := ImageList1;
  FEPManager.List := lvEP;
  FEPManager.registerControl(btnEPAdd, copAdd);
  FEPManager.registerControl(btnEPDelete, copDelete);
  FEPManager.registerControl(btnEPInstall, copExecute);
  FEPManager.OnSetFocus := EPFocusChange;

  FIDManager := TIdentityProviderManager.create;
  FIDManager.Settings := FIni;
  FIDManager.Images := ImageList1;
  FIDManager.List := lvID;
  FIDManager.registerControl(btnIDAdd, copAdd);
  FIDManager.registerControl(btnIDDelete, copDelete);

  edtConfigFile.text := FIni.ReadString('config', 'filename', '');
  edtConfigFileChange(self);

  Timer1.enabled := true;
end;

procedure TMainConsoleForm.FormDestroy(Sender: TObject);
begin
  GBackgroundTasks.stopAll;
  FThread.StopAndWait(40);
  FThread.Free;
  GBackgroundTasks.Wait(2000);
  FPackageThread.Stop;
  FPackageThread.Free;
  FTxManager.Free;
  FEPManager.Free;
  FIDManager.Free;
  FConfig.free;
  FPackages.free;
  FTelnet.Free;
  FIncoming.Free;
  FThreads.Free;
  FLines.Free;
  FStatistics.Free;
  FLock.Free;
  FIni.Free;
end;

procedure TMainConsoleForm.FormShow(Sender: TObject);
var
  fn : string;
begin
  if getCommandLineParam('installer', fn) then
  begin
    edtConfigFile.text := fn;
    edtConfigFileChange(self);
    if edtAdminSCIMSalt.text = '' then
      edtAdminSCIMSalt.text := NewGuidId;
    pgMain.ActivePage := tbManage;
  end
  else
    pgMain.ActivePage := tbConsole;
  pgManage.ActivePage := tbWebSettings;
end;

procedure TMainConsoleForm.Image2Click(Sender: TObject);
begin
  if FRunning then
    exit;
  pnlLoincImport.Color := rgb(217, 240, 247);
  pnlSnomedImport.color := clWhite;
  pnlCombineSnomed.color := clWhite;
  pnlProcessUMLS.color := clWhite;
  pgTerminologies.ActivePageIndex := 2;
end;

procedure TMainConsoleForm.Image3Click(Sender: TObject);
begin
  if FRunning then
    exit;
  pnlCombineSnomed.Color := rgb(217, 240, 247);
  pnlLoincImport.color := clWhite;
  pnlSnomedImport.color := clWhite;
  pnlProcessUMLS.color := clWhite;
  pgTerminologies.ActivePageIndex := 1;
end;

procedure TMainConsoleForm.Image4Click(Sender: TObject);
begin
  if FRunning then
    exit;
  pnlSnomedImport.Color := rgb(217, 240, 247);
  pnlLoincImport.color := clWhite;
  pnlCombineSnomed.color := clWhite;
  pnlProcessUMLS.color := clWhite;
  pgTerminologies.ActivePageIndex := 0;
end;

procedure TMainConsoleForm.Image5Click(Sender: TObject);
begin
  if FRunning then
    exit;
  pnlProcessUMLS.Color := rgb(217, 240, 247);
  pnlLoincImport.color := clWhite;
  pnlCombineSnomed.color := clWhite;
  pnlSnomedImport.color := clWhite;
  pgTerminologies.ActivePageIndex := 3;
end;

procedure TMainConsoleForm.lbEditionsClick(Sender: TObject);
begin
  btnDeleteEdition.Enabled := lbEditions.ItemIndex <> -1;
end;

procedure TMainConsoleForm.lvPackagesItemChecked(Sender: TObject; Item: TListItem);
var
  i : integer;
  ts : TStringList;
begin
  if FEPManager.Focus <> nil then
  begin
    ts := FEPManager.Focus.prop['packages'].values;
    i := ts.IndexOf(item.Caption);
    if item.Checked then
    begin
      if i = -1 then
        ts.add(item.caption);
    end
    else if (i > -1) then
      ts.Delete(i);
    FConfig.save;
  end;

end;

procedure TMainConsoleForm.MenuItem4Click(Sender: TObject);
begin
  ServerConnectionForm.edtServer.Text := FAddress;
  ServerConnectionForm.edtServer.ReadOnly:= false;
  ServerConnectionForm.edtPassword.Text := FPassword;
  if ServerConnectionForm.ShowModal = mrOk then
  begin
    FAddress := ServerConnectionForm.edtServer.Text;
    FPassword := ServerConnectionForm.edtPassword.Text;
    FIni.WriteString('console', 'address', FAddress);
    if FPassword = '' then
      FPassword := DEF_PASSWORD;
    FIni.WriteString('console', 'password', FPassword);
    FIni.WriteString('servers', FAddress, FPassword);
    FLines.clear;
    mConsole.Lines.Clear;
    if FConnected then
      FTelnet.Disconnect;
    FStatus := csDiconnected;
  end;
end;

procedure TMainConsoleForm.MenuItem6Click(Sender: TObject);
begin

end;

procedure TMainConsoleForm.connectToServer(server : String);
var
  pwd : String;
begin
  if (server = 'localhost') then
    pwd := DEF_PASSWORD
  else
    pwd := FIni.ReadString('servers', FAddress, '');
  if (pwd = '') then
  begin
    ServerConnectionForm := TServerConnectionForm.create(self);
    try
      ServerConnectionForm.edtServer.Text := server;
      ServerConnectionForm.edtServer.ReadOnly:= true;
      ServerConnectionForm.edtPassword.Text := pwd;
      if ServerConnectionForm.ShowModal = mrOk then
        pwd := ServerConnectionForm.edtPassword.Text
    finally
      FreeAndNil(ServerConnectionForm);
    end;
  end;
  if pwd <> '' then
  begin
    FAddress := server;
    FPassword := ServerConnectionForm.edtPassword.Text;
    FIni.WriteString('console', 'address', FAddress);
    FIni.WriteString('console', 'password', FPassword);
    FIni.WriteString('servers', FAddress, FPassword);
    FLines.clear;
    mConsole.Lines.Clear;
    if FConnected then
      FTelnet.Disconnect;
    FStatus := csDiconnected;
  end;
end;

procedure TMainConsoleForm.GetODBCDriversList(list : TStrings);
var
  aStringlist   : TStringlist;
  aRegistry   : TRegistry;
Begin
  aStringlist:= Tstringlist.Create;
  try
    aRegistry:= TRegistry.Create;
    try
      aRegistry.rootkey:= HKEY_LOCAL_MACHINE;
      aRegistry.OpenKeyReadOnly('SOFTWARE\ODBC\ODBCINST.INI\ODBC Drivers');
      aRegistry.GetValueNames(aStringlist);

      list.assign(aStringlist);
    finally
      aRegistry.Free;
    end;
  finally
    aStringlist.Sort;
  end;
end;

procedure TMainConsoleForm.SetConfigEditable;
begin
  if (FConfig <> nil) and (FConfig.filename = edtConfigFile.text) then
    exit;
  if FConfig <> nil then
    FConfig.Free;
  FConfig := TFHIRServerConfigFile.create(edtConfigFile.text);
  FTxManager.ConfigFile := FConfig.link;
  FEPManager.ConfigFile := FConfig.link;
  FIDManager.ConfigFile := FConfig.link;

  FLoading := true;
  try
    edtHostName.Text := FConfig.web['host'].value;
    edtHostName.Enabled := true;
    edtWebPort.Text := FConfig.web['http'].value;
    edtWebPort.Enabled := true;
    chkWebMode.Checked := FConfig.web['plain-mode'].value = 'redirect';
    chkWebMode.Enabled := true;
    edtSSLPort.Text := FConfig.web['https'].value;
    edtSSLPort.Enabled := true;
    edtSSLCert.Text := FConfig.web['certname'].value;
    edtSSLCert.Enabled := true;
    edtCACert.Text := FConfig.web['cacertname'].value;
    edtCACert.Enabled := true;
    edtPrivateKey.Text := FConfig.web['certkey'].value;
    edtPrivateKey.Enabled := true;
    edtSSLPassword.Text := FConfig.web['password'].value;
    edtSSLPassword.Enabled := true;
    edtGoogleId.Text := FConfig.web['googleid'].value;
    edtGoogleId.Enabled := true;
    edtTelnetPassword.Text := FConfig.web['telnet-password'].value;
    edtTelnetPassword.Enabled := true;

    edtAdminEmail.Text := FConfig.admin['email'].value;
    edtAdminEmail.Enabled := true;
    edtAdminOrganization.Text := FConfig.admin['ownername'].value;
    edtAdminOrganization.Enabled := true;
    edtAdminSMS.Text := FConfig.admin['owner-sms'].value;
    edtAdminSMS.Enabled := true;
    edtAdminSCIMSalt.Text := FConfig.admin['scim-salt'].value;
    edtAdminSCIMSalt.Enabled := true;

    lvPackages.Enabled := true;
  finally
    FLoading := false;
  end;
end;

procedure TMainConsoleForm.SetConfigReadonly;
begin
  FConfig.Free;
  FConfig := nil;
  FTxManager.ConfigFile := nil;
  FEPManager.ConfigFile := nil;
  FIDManager.ConfigFile := nil;

  FLoading := true;
  try
    edtAdminEmail.Text := '';
    edtAdminEmail.Enabled := false;
    edtAdminOrganization.Text := '';
    edtAdminOrganization.Enabled := false;
    edtAdminSMS.Text := '';
    edtAdminSMS.Enabled := false;
    edtAdminSCIMSalt.Text := '';
    edtAdminSCIMSalt.Enabled := false;
    edtHostName.Text := '';
    edtHostName.Enabled := false;
    edtWebPort.Text := '';
    edtWebPort.Enabled := false;
    chkWebMode.checked := false;
    chkWebMode.Enabled := false;
    edtSSLPort.Text := '';
    edtSSLPort.Enabled := false;
    edtSSLCert.Text := '';
    edtSSLCert.Enabled := false;
    edtCACert.Text := '';
    edtCACert.Enabled := false;
    edtPrivateKey.Text := '';
    edtPrivateKey.Enabled := false;
    edtSSLPassword.Text := '';
    edtSSLPassword.Enabled := false;
    edtGoogleId.Text := '';
    edtGoogleId.Enabled := false;
    edtTelnetPassword.Text := '';
    edtTelnetPassword.Enabled := false;
    lvPackages.Enabled := true;
  finally
    FLoading := false;
  end;
end;

function matchesVersion(ep, pi, piv : String):boolean;
begin
  if ep = 'r2' then
    result := SameText(pi, 'DSTU2') or SameText(pi, 'STU2') or pi.StartsWith('1.0') or piv.StartsWith('1.0')
  else if ep = 'r3' then
    result := SameText(pi, 'STU3') or pi.StartsWith('3.0') or piv.StartsWith('3.0')
  else if ep = 'r4' then
    result := SameText(pi, 'R4') or pi.StartsWith('4.0') or piv.StartsWith('4.0')
  else if ep = 'r5' then
    result := SameText(pi, 'R5') or pi.StartsWith('4.5') or piv.StartsWith('4.5')
  else
    result := false;
end;

function isAutomatic(ep : String; pi : TFHIRPackageInfo):boolean;
begin
  if pi.id = 'hl7.fhir.core' then
    result := true
  else if pi.id = 'hl7.fhir.'+ep+'.core' then
    result := true
  else if pi.id = 'hl7.terminology' then
    result := true
  else
    result := false;
end;

procedure TMainConsoleForm.EPFocusChange(sender : TObject);
var
  ep : TFHIRServerConfigSection;
  pi : TFHIRPackageInfo;
  li : TListItem;
  ts : TStringList;
begin
  lvPackages.Items.clear;
  ep := FEPManager.Focus;
  if (ep <> nil) then
  begin
    ts := ep['packages'].values;
    for pi in FPackages do
    begin
      if matchesversion(ep['type'].value, pi.fhirVersion, pi.version) and not isAutomatic(ep['type'].value, pi) then
      begin
        li := lvPackages.items.Add;
        li.Caption := pi.id+'#'+pi.version;
        li.Checked := ts.IndexOf(pi.id+'#'+pi.version) > -1;
      end;
    end;
  end;
  lvPackages.ViewStyle := vsIcon;
  lvPackages.ViewStyle := vsList;
end;

procedure TMainConsoleForm.updateDoco;
begin
  if ActiveControl = chkWebMode then
    lblDoco.caption := 'If this is selected, then any requests on the non-secure port will immediately be redirected to the SSL port. Note that end-points may have their own security rules for SSL, but won''t have any restrictions on open (other than not working)'
  else if ActiveControl = edtCACert then
    lblDoco.caption := 'The CA certificate for the SSL certificate, in DER format'
  else if ActiveControl = edtAdminSCIMSalt then
    lblDoco.caption := 'The Salt used when hashing passwords. Note that changing the salt will invalidate all passwords'
  else if ActiveControl = edtTelnetPassword then
    lblDoco.caption := 'The password required to connect to telnet when connecting from an external computer (not localhost). Default is no password required. Note that the telnet interface can make no changes on the server'
  else if ActiveControl = edtPrivateKey then
    lblDoco.caption := 'The private key file for the SSL certificate'
  else if ActiveControl = edtSSLCert then
    lblDoco.caption := 'The SSL certificate, as issued by a CA. In DER format'
  else if ActiveControl = edtSSLPassword then
    lblDoco.caption := 'The password for the SSL private key'
  else if ActiveControl = edtSSLPort then
    lblDoco.caption := 'The port to use for SSL services'
  else if ActiveControl = edtHostName then
    lblDoco.caption := 'The host name by which clients know this server (normally, the server uses the Host details provided by the client, but there are places in the OAuth process and others where this is not available'
  else if ActiveControl = edtWebPort then
    lblDoco.caption := 'The post to use for plain (unsecured) web services'
  else if ActiveControl = edtGoogleId then
    lblDoco.caption := 'The google id to use for reporting hits to the geolocating device'
  else if ActiveControl = edtAdminEmail then
    lblDoco.caption := 'The administrator email for this server'
  else if ActiveControl = edtAdminOrganization then
    lblDoco.caption := 'The organization that owns the server (typically, would match the SSL certificate, but this is not required)'
  else if ActiveControl = edtAdminSMS then
    lblDoco.caption := 'The SMS of the owner. If an SMS destination (twilio) is set up, the server will get SMS messages when the server starts and stops'
  else if ActiveControl = lvPackages then
    lblDoco.caption := 'Packages loaded when this end-point starts (typically, these are used when validating or providing terminology services)'
  else if ActiveControl = lvID then
    lblDoco.caption := 'identity providers supported for this server. These need external registration, and specific API support - consult the #pascal channel on chat.fhir.org for advice to use these'
  else if ActiveControl = lvTx then
    lblDoco.caption := 'Terminologies loaded when the server loads. These terminologies are available on all end=points for terminoogy services and reasoning. Most of the terminologies require some kind of import process). Note that some terminologies are loaded internally to the server, and are not subject to user configuration. All servers SHOULD have UCUM loaded for normal functionality'
  else if ActiveControl = lvEP then
    lblDoco.caption := 'A list of end-points available on the server. Each end-point represents a set of functional web services available to the client. Most of the end-points require a database, and some kind of install process'
  else
    lblDoco.caption := ''

end;

procedure TMainConsoleForm.MenuItem7Click(Sender: TObject);
begin
  connectToServer((Sender as TMenuItem).Caption);
end;

procedure TMainConsoleForm.MenuItem8Click(Sender: TObject);
begin

end;

procedure TMainConsoleForm.pnlSnomedImportClick(Sender: TObject);
begin

end;

procedure TMainConsoleForm.tbConsoleContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TMainConsoleForm.tbTerminologiesContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TMainConsoleForm.edtFilterChange(Sender: TObject);
var
  s : String;
begin
  FFilter := edtFilter.Text;
  FFilter := FFilter.ToLower;
  mConsole.lines.BeginUpdate;
  try
    mConsole.Lines.clear;
    for s in FLines do
      if passesFilter(s) then
        mConsole.Lines.add(s);
  finally
    mConsole.Lines.EndUpdate;
  end;
  mConsole.SelStart := mConsole.Lines.Text.Length-1;
end;

procedure TMainConsoleForm.edtGoogleIdChange(Sender: TObject);
begin
  if not FLoading then
  begin
    FConfig.web['googleid'].value := edtGoogleId.Text;
    FConfig.Save;
  end;
end;

procedure TMainConsoleForm.edtHostNameChange(Sender: TObject);
begin
  if not FLoading then
  begin
    FConfig.web['host'].value := edtHostName.Text;
    FConfig.Save;
  end;
end;

procedure TMainConsoleForm.edtPrivateKeyChange(Sender: TObject);
begin
  if not FLoading then
  begin
    FConfig.web['certkey'].value := edtPrivateKey.Text;
    FConfig.Save;
  end;
end;

procedure TMainConsoleForm.edtSSLCertChange(Sender: TObject);
begin
  if not FLoading then
  begin
    FConfig.web['certname'].value := edtSSLCert.Text;
    FConfig.Save;
  end;
end;

procedure TMainConsoleForm.edtSSLPasswordChange(Sender: TObject);
begin
  if not FLoading then
  begin
    FConfig.web['password'].value := edtSSLPassword.Text;
    FConfig.Save;
  end;
end;

procedure TMainConsoleForm.edtSSLPortChange(Sender: TObject);
begin
  if not FLoading then
  begin
    FConfig.web['https'].value := edtSSLPort.Text;
    FConfig.Save;
  end;
end;

procedure TMainConsoleForm.edtTelnetPasswordChange(Sender: TObject);
begin
  if not FLoading then
  begin
    FConfig.web['telnet-password'].value := edtGoogleId.Text;
    FConfig.Save;
  end;
end;

procedure TMainConsoleForm.edtWebPortChange(Sender: TObject);
begin
  if not FLoading then
  begin
    FConfig.web['http'].value := edtWebPort.Text;
    FConfig.Save;
  end;
end;

procedure TMainConsoleForm.btnSourceClick(Sender: TObject);
begin
   if (edtSource.text <> '') then
    dlgFolder.filename := edtSource.text;
  dlgFolder.Title := 'Choose SNOMED CT RF2 Snapshot Folder';
  if dlgFolder.Execute then
    edtSource.text := dlgFolder.filename;
end;

procedure TMainConsoleForm.btnStopCombineClick(Sender: TObject);
begin
  FWantStop := true;
end;

procedure TMainConsoleForm.btnUMLSStopClick(Sender: TObject);
begin
  FWantStop := true;
end;

procedure TMainConsoleForm.btnFetchThreadsClick(Sender: TObject);
begin
  try
    if FConnected then
      FTelnet.SendString('@threads'+#10);
  except
  end;
end;

procedure TMainConsoleForm.cbUMLSDriverChange(Sender: TObject);
begin
end;

procedure TMainConsoleForm.btnBaseClick(Sender: TObject);
begin
  if (edtSource.text <> '') then
  begin
    dlgFolder.filename := edtBase.text;
  end;
  dlgFolder.Title := 'Choose SNOMED CT RF2 International Snapshot Folder';
  if dlgFolder.Execute then
    edtBase.text := dlgFolder.filename;
end;

procedure TMainConsoleForm.btnCACertClick(Sender: TObject);
begin
  dlgOpen.filename := edtCACert.text;
  if dlgOpen.Execute then;
    edtCACert.text := dlgOpen.filename;
end;

procedure TMainConsoleForm.btnCertClick(Sender: TObject);
begin
  dlgOpen.filename := edtSSLCert.text;
  if dlgOpen.Execute then;
    edtSSLCert.text := dlgOpen.filename;
end;

procedure TMainConsoleForm.btnCertKeyClick(Sender: TObject);
begin
  dlgOpen.filename := edtPrivateKey.text;
  if dlgOpen.Execute then;
    edtPrivateKey.text := dlgOpen.filename;
end;

procedure TMainConsoleForm.btnClearCacheClick(Sender: TObject);
begin
  try
    if FConnected then
      FTelnet.SendString('@cache'+#10);
  except
  end;
end;

procedure TMainConsoleForm.btnCombinedDestinationClick(Sender: TObject);
begin
  if (edtCombinedDestination.text <> '') then
  begin
    dlgFolder.filename := edtCombinedDestination.text;
  end;
  dlgFolder.Title := 'Choose Combined Files Destination';
  if dlgFolder.Execute then
    edtCombinedDestination.text := dlgFolder.filename;
end;

procedure TMainConsoleForm.btnCombinedStoreClick(Sender: TObject);
begin
  if (edtCombinedStore.text <> '') then
  begin
    dlgSave.filename := edtCombinedStore.text;
  end;
  dlgSave.Title := 'Choose Combined Files Persistent Store';
  if dlgSave.Execute then
    edtCombinedStore.text := dlgSave.filename;
end;

procedure TMainConsoleForm.btnCombineGoClick(Sender: TObject);
var
  i : integer;
  start : TDateTime;
  combiner : TSnomedCombiner;
  svc : TSnomedServices;
begin
  raise Exception.create('not done yet');
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
    FIni.WriteString('snomed-combine', 'base', edtInternational.text);
    FIni.WriteString('snomed-combine', 'editions', lbEditions.Items.CommaText);
    FIni.WriteString('snomed-combine', 'dest', edtCombinedDestination.text);
    FIni.WriteString('snomed-combine', 'store', edtCombinedStore.text);

    FWantStop := false;
    btnStopCombine.Visible := true;
    cursor := crHourGlass;
    FRunning := true;
    edtInternational.enabled := false;
    lbEditions.enabled := false;
    btnCombineGo.enabled := false;
    btnInternational.enabled := false;
    btnAddEdition.enabled := false;
    btnDeleteEdition.enabled := false;
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
        combiner.international.Load(edtInternational.Text, true);
        for i := 0 to lbEditions.Items.Count - 1 do
        begin
          svc := TSnomedServices.create;
          combiner.others.Add(svc);
          svc.load(lbEditions.Items[i], true);
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
      FRunning := false;
      edtInternational.enabled := true;
      lbEditions.enabled := true;
      btnCombineGo.enabled := true;
      btnInternational.enabled := true;
      btnAddEdition.enabled := true;
      btnDeleteEdition.enabled := true;
      btnCombinedDestination.enabled := true;
      edtCombinedDestination.enabled := true;
      btnCombinedStore.enabled := true;
      edtCombinedStore.enabled := true;
      cmbCallback(0, '');
    end;
end;

procedure TMainConsoleForm.btnDeleteEditionClick(Sender: TObject);
begin
  if (lbEditions.ItemIndex > -1) then
    lbEditions.Items.Delete(lbEditions.ItemIndex);
end;

procedure TMainConsoleForm.btnAddEditionClick(Sender: TObject);
begin
  dlgOpen.Title := 'Choose National SNOMED Edition cache file';
  if dlgOpen.Execute then
  begin
    lbEditions.Items.Add(dlgOpen.filename);
    lbEditions.ItemIndex := lbEditions.Items.Count - 1;
    lbEditionsClick(nil);
  end;
end;

procedure TMainConsoleForm.BitBtn1Click(Sender: TObject);
begin
  dlgConfig.FileName := edtConfigFile.text;
  if dlgConfig.Execute then
  begin
    edtConfigFile.text := dlgConfig.filename;
    FIni.WriteString('config', 'filename', edtConfigFile.text);
  end;
end;

procedure TMainConsoleForm.btnDestinationClick(Sender: TObject);
begin
  if (edtDestination.text <> '') then
  begin
    dlgSave.filename := edtDestination.text;
  end;
  if dlgSave.Execute then
    edtDestination.text := dlgSave.filename;
end;

procedure TMainConsoleForm.btnFetchObjectsClick(Sender: TObject);
begin
  try
    if FConnected then
      FTelnet.SendString('@classes'+#10);
  except
  end;
end;

procedure TMainConsoleForm.btnFetchObjectsPlusClick(Sender: TObject);
begin
   try
    if FConnected then
      FTelnet.SendString('@classes+'+#10);
  except
  end;
end;

procedure TMainConsoleForm.btnImportLoincClick(Sender: TObject);
var
  start : TDateTime;
begin
  FIni.WriteString('loinc-import', 'source', edtLoincSource.text);
  FIni.WriteString('loinc-import', 'date', edtLoincVersion.Text);
  FIni.WriteString('loinc-import', 'tdate', edtLoincDate.Text);
  FIni.WriteString('loinc-import', 'dest', edtLoincDest.text);
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

    FWantStop := false;
    btnLoincImportStop.Visible := true;
    cursor := crHourGlass;
    FRunning := true;
    edtLoincSource.enabled := false;
    edtLoincVersion.enabled := false;
    edtLoincDate.enabled := false;
    edtLoincDest.enabled := false;
    btnImportLoinc.enabled := false;
    btnLoincSource.enabled := false;
    btnLoincDest.enabled := false;
    try
      importLoinc(edtloincSource.Text, edtLoincVersion.Text, edtLoincDate.Text, edtLoincDest.text, loincCallBack);
    finally
      cursor := crDefault;
      FRunning := false;
      edtLoincSource.enabled := true;
      edtLoincVersion.enabled := true;
      edtLoincDate.enabled := true;
      edtLoincDest.enabled := true;
      btnImportLoinc.enabled := true;
      btnLoincSource.enabled := true;
      btnLoincDest.enabled := true;
      loincCallback(0, '');
    end;
    MessageDlg('Successfully Imported LOINC in '+DescribePeriod(now - start), mtInformation, [mbok], 0);
  end;
end;

procedure TMainConsoleForm.btnImportSnomedClick(Sender: TObject);
var
  module, version : String;
  start : TDateTime;
  nb : boolean;
begin
  if not FolderExists(edtSource.Text) then
    ShowMessage('Folder "'+edtSource.Text+'" not found')
  else if edtDestination.Text = '' then
    ShowMessage('Please Choose a Destination')
  else if cbxEdition.ItemIndex = -1 then
    ShowMessage('Please Choose an Edition')
  else if not FileExists(edtDestination.Text) or (MessageDlg('Overwrite "'+edtDestination.Text+'"?', mtConfirmation, mbYesNo, 0) = mrYes) then
  begin
    FIni.WriteString('snomed-import', 'source', edtSource.text);
    FIni.WriteString('snomed-import', 'base', edtBase.text);
    FIni.WriteInteger('snomed-import', 'edition', cbxEdition.ItemIndex);
    FIni.WriteInteger('snomed-import', 'date', trunc(edtDate.Date));
    FIni.WriteString('snomed-import', 'dest', edtDestination.text);
    module := getSnomedModule;
    nb := needsBaseForImport(module);
    if nb and not FolderExists(edtBase.Text) then
      ShowMessage('Base Folder "'+edtSource.Text+'" not found')
    else
    begin
      version := FormatDateTime('yyyymmdd', edtDate.Date);
      FWantStop := false;
      btnSnomedImportStop.Visible := true;
      cursor := crHourGlass;
      FRunning := true;
      edtSource.enabled := false;
      cbxEdition.enabled := false;
      edtDate.enabled := false;
      edtDestination.enabled := false;
      btnImportSnomed.enabled := false;
      btnSource.enabled := false;
      btnDestination.enabled := false;
      try
        start := now;
        if nb then
          importSnomedRF2(edtSource.text, edtBase.text, edtDestination.text, 'http://snomed.info/sct/'+module+'/version/'+version, sctCallback)
        else
          importSnomedRF2(edtSource.text, '', edtDestination.text, 'http://snomed.info/sct/'+module+'/version/'+version, sctCallback);
      finally
        cursor := crDefault;
        btnSnomedImportStop.Visible := false;
        FRunning := false;
        edtSource.enabled := true;
        cbxEdition.enabled := true;
        edtDate.enabled := true;
        edtDestination.enabled := true;
        btnImportSnomed.enabled := true;
        btnSource.enabled := true;
        btnDestination.enabled := true;
        sctCallback(0, '');
      end;
    end;
  end;
end;

procedure TMainConsoleForm.btnInternationalClick(Sender: TObject);
begin
  if (edtInternational.text <> '') then
     dlgOpen.filename := edtInternational.text;
   dlgOpen.Title := 'Choose International SNOMED cache file';
   if dlgOpen.Execute then
     edtInternational.text := dlgOpen.filename;
end;

procedure TMainConsoleForm.btnLoincDestClick(Sender: TObject);
begin
   if (edtLoincDest.text <> '') then
     dlgSave.filename := edtLoincDest.text;
   if dlgSave.Execute then
     edtLoincDest.text := dlgSave.filename;
end;

procedure TMainConsoleForm.btnLoincImportStopClick(Sender: TObject);
begin
  FWantStop := true;
end;

procedure TMainConsoleForm.btnLoincSourceClick(Sender: TObject);
begin
   if (edtLoincSource.text <> '') then
      dlgFolder.filename := edtLoincSource.text;
    dlgFolder.Title := 'Choose LOINC Content Folder';
    if dlgFolder.Execute then
      edtLoincSource.text := dlgFolder.filename;
end;


function dbtype(i : integer) : TFDBPlatform;
begin
  if (i = 1) then
    result := kdbMySQL
  else
    result := kdbSqlServer;
end;


procedure TMainConsoleForm.btnNDCClick(Sender: TObject);
//var
//  start : TDateTime;
//  ndc : TNdcImporter;
begin
  //if dlgFolder.execute then
  //begin
  //  start := now;
  //  if cbUMLSDriver.Text = '' then
  //    ShowMessage('No Database Driver specified') else
  //  if not AnsiMatchText(cbUMLSType.Text, ['mssql', 'mysql']) then
  //    ShowMessage('No valid Server Type specified') else
  //  if edtUMLSServer.Text = '' then
  //    ShowMessage('No Server specified')
  //  else if edtUMLSDatabase.Text = '' then
  //    ShowMessage('No Database specified')
  //  else if (edtUMLSUsername.Text = '') xor (edtUMLSPassword.Text = '') then
  //    ShowMessage('Plase specify both a username and password, or neither')
  //  else
  //  begin
  //    FIni.WriteString('umls-process', 'driver', cbUMLSDriver.text);
  //    FIni.WriteString('umls-process', 'type', cbUMLSType.text);
  //    FIni.WriteString('umls-process', 'server', edtUMLSServer.text);
  //    FIni.WriteString('umls-process', 'database', edtUMLSDatabase.text);
  //    FIni.WriteString('umls-process', 'username', edtUMLSUsername.text);
  //    FIni.WriteString('umls-process', 'password', strEncrypt(edtUMLSPassword.text, GetCryptKey('umls encryption key')));
  //
  //    ndc := TNDCImporter.create(dlgFolder.FileName);
  //    try
  //      ndc.Database := TFDBOdbcManager.create('ndc', dbtype(cbUMLSType.itemIndex), 4, 0, cbUMLSDriver.Text, edtUMLSServer.text, edtUMLSDatabase.Text, edtUMLSUsername.Text, edtUMLSPassword.Text);
  //      FWantStop := false;
  //      btnUMLSStop.Visible := true;
  //      cursor := crHourGlass;
  //      FRunning := true;
  //      edtUMLSServer.enabled := false;
  //      edtUMLSDatabase.enabled := false;
  //      edtUMLSUsername.enabled := false;
  //      edtUMLSPassword.enabled := false;
  //      btnProcessUMLS.enabled := false;
  //      btnNDC.Enabled := false;
  //      try
  //        ndc.process(umlsCallback);
  //      finally
  //        cursor := crDefault;
  //        btnNDC.Enabled := true;
  //        btnUMLSStop.Visible := false;
  //        FRunning := false;
  //        edtUMLSServer.enabled := true;
  //        edtUMLSDatabase.enabled := true;
  //        edtUMLSUsername.enabled := true;
  //        edtUMLSPassword.enabled := true;
  //        btnProcessUMLS.enabled := true;
  //        umlsCallback(0, '');
  //      end;
  //    finally
  //      ndc.Free;
  //    end;
  //    MessageDlg('Successfully Upload NDC in '+DescribePeriod(now - start), mtInformation, [mbok], 0);
  //  end;
  //end;
end;

procedure TMainConsoleForm.btnSnomedImportStopClick(Sender: TObject);
begin
  FWantStop := true;
end;

procedure TMainConsoleForm.cbxEditionChange(Sender: TObject);
var
  module : string;
  b : boolean;
begin
  module := getSnomedModule;
  b := needsBaseForImport(module);
  edtBase.Enabled := b;
  btnBase.Enabled := b;
end;

procedure TMainConsoleForm.chkWebModeChange(Sender: TObject);
begin
  if not FLoading then
  begin
    if chkWebMode.Checked then
      FConfig.web['plain-mode'].value := 'redirect'
    else
      FConfig.web['plain-mode'].value := 'serve';
    FConfig.save;
  end;
end;

procedure TMainConsoleForm.edtAdminEmailChange(Sender: TObject);
begin
  if not FLoading and (FConfig <> nil) then
  begin
    FConfig.admin['email'].value := edtAdminEmail.Text;
    FConfig.Save;
  end;
end;

procedure TMainConsoleForm.edtAdminOrganizationChange(Sender: TObject);
begin
  if not FLoading and (FConfig <> nil) then
  begin
    FConfig.admin['ownername'].value := edtAdminOrganization.Text;
    FConfig.Save;
  end;
end;

procedure TMainConsoleForm.edtAdminSCIMSaltChange(Sender: TObject);
begin
  if not FLoading and (FConfig <> nil) then
  begin
    FConfig.admin['scim-salt'].value := edtAdminSCIMSalt.Text;
    FConfig.Save;
  end;
end;

procedure TMainConsoleForm.edtAdminSMSChange(Sender: TObject);
begin
  if not FLoading and (FConfig <> nil) then
  begin
    FConfig.admin['owner-sms'].value := edtAdminSMS.Text;
    FConfig.Save;
  end;
end;

procedure TMainConsoleForm.edtCACertChange(Sender: TObject);
begin
  if not FLoading and (FConfig <> nil) then
  begin
    FConfig.web['cacertname'].value := edtCACert.Text;
    FConfig.Save;
  end;
end;

procedure TMainConsoleForm.edtConfigFileChange(Sender: TObject);
begin
  if (edtConfigFile.text <> '') and FileExists(edtConfigFile.text) then
  begin
    edtConfigFile.Color := clWhite;
    SetConfigEditable;
  end
  else
  begin
    edtConfigFile.Color := HTML_COLOUR_VALUES[hcSalmon];
    SetConfigReadOnly;
  end;
end;

procedure TMainConsoleForm.Timer1Timer(Sender: TObject);
var
  ts, tsl, tsd, tsth : TStringList;
  s, ss, rs : String;
  st : TConnectionStatus;
  id : String;
  session : TServerSession;
  i : integer;
  d : TDateTime;
begin
  ts := TStringList.create;
  tsl := TStringList.create;
  tsd := TStringList.create;
  tsth := TStringList.create;
  try
    d := 0;
    Flock.Lock;
    try
      st := FStatus;
      ss := FServerStatus;
      ts.assign(FIncoming);
      FIncoming.clear;
      tsth.assign(FThreads);
      FThreads.clear;
      if FThreadPackages <> nil then
      begin
        FPackages.Clear;
        FPackages.AddAll(FThreadPackages);
        FThreadPackages.Free;
        FThreadPackages := nil;
      end;
      rs := FStatistics.report;
    finally
      FLock.Unlock;
    end;
    case st of
      csDiconnected :
        begin
          mConsole.Color := $00EFEFEF;
          Caption := 'FHIR Console - Connecting to '+FAddress;
          sBar.Panels[0].Text := 'Connecting';
        end;
      csUsername :
        begin
          mConsole.Color := $00EFFFEF;
          Caption := 'FHIR Console - Authenticating to '+FAddress;
          sBar.Panels[0].Text := 'Authenticating (U)';
        end;
      csPassword :
        begin
          mConsole.Color := $00EFFFFF;
          Caption := 'FHIR Console - Authenticating to '+FAddress;
          sBar.Panels[0].Text := 'Authenticating (P)';
        end;
      csConnected, csEnhanced:
        begin
          mConsole.Color := $00FFFFFF;
          Caption := 'FHIR Console - Connected to '+FAddress;
          sBar.Panels[0].Text := 'Connected';
          if not FTelnet.connected then
          begin
            Flock.Lock;
            try
              FStatus := csDiconnected;
            finally
              FLock.Unlock;
            end;
          end;
        end;
    end;

    if tsth.Count > 0 then
      mThreads.lines.Assign(tsth);

    for s in ts do
    begin
      FLines.add(s);
      if passesFilter(s) then
        mConsole.lines.add(s);
    end;
    while (mConsole.lines.count > 1000) and (edtFilter.Text = '') do
    begin
      FLines.delete(0);
      mConsole.lines.delete(0);
    end;
    mConsole.SelStart := mConsole.Lines.Text.Length-1;
    mStats.Text := rs;
  finally
    ts.free;
    tsth.free;
    tsl.free;
    tsd.free;
  end;
  GBackgroundTasks.primaryThreadCheck;
  if st = csDiconnected then
  begin
    sBar.Panels[1].Text := 'n/a';
    if (ss = '') then
      sBar.Panels[4].Text := ''
    else
      sBar.Panels[4].Text := 'Last Server: '+ss;
  end
  else
  begin
    sBar.Panels[1].Text := DescribePeriodNoMsec(now - FLastIncoming);
    sBar.Panels[4].Text := 'Server: '+ss;
  end;
  sBar.Panels[2].Text := inttostr(mConsole.lines.count) + ' '+StringPlural('Line', mConsole.lines.count);
  sBar.Panels[3].Text := Logging.MemoryStatus;
  updateDoco;
end;

procedure TMainConsoleForm.ToolButton1Click(Sender: TObject);
begin
  mConsole.lines.clear;
end;

procedure TMainConsoleForm.ToolButton3Click(Sender: TObject);
begin
  ServerConnectionForm.edtServer.Text := FAddress;
  ServerConnectionForm.edtPassword.Text := FPassword;
  if ServerConnectionForm.ShowModal = mrOk then
  begin
    FAddress := ServerConnectionForm.edtServer.Text;
    FPassword := ServerConnectionForm.edtPassword.Text;
    FIni.WriteString('console', 'address', FAddress);
    if FPassword = '' then
      FPassword := DEF_PASSWORD;
    FIni.WriteString('console', 'password', FPassword);
    FLines.clear;
    mConsole.Lines.Clear;
    FTelnet.Disconnect;
    FStatus := csDiconnected;
  end;
end;

procedure TMainConsoleForm.recordSessionLength(start, length: int64);
begin
  FStatistics.recordSession(start, length);
end;

function ignoreLine(s : String) : boolean;
var
  ch : char;
begin
  s := trim(s);
  if (s = '') or (s = '.') or (s = 'console') then
    result := true
  else
  begin
    result := true;
    for ch in s do
      if ch <> '*' then
         exit(false);
  end;
end;

procedure TMainConsoleForm.processIncomingLine(line : String);
var
  reply : String;
begin
  reply := '';
  FLock.Lock;
  try
    case FStatus of
      csDiconnected :
        FIncoming.add('!!'+line); // this is a timing issue if it does happen but it should not
      csUsername:
        if (line = 'Username: ') then
        begin
          reply := 'console';
          FStatus := csPassword;
        end
        else
          ; // ignore line
      csPassword:
        if (line = 'Password: ') then
        begin
          reply := FPassword;
          FStatus := csConnected;
          FIncoming.add('----------------------------------------------------------');
        end
        else
          ; // ignore line
      csConnected:
        begin
          if not handleCommand(line) then
            if not ignoreLine(line) then
              FIncoming.add(line);
          reply := '@console';
          FStatus := csEnhanced;
        end;
      csEnhanced:
        if not handleCommand(line) then
          if not ignoreLine(line) then
            FIncoming.add(line);
    end;
  finally
    FLock.Unlock;
  end;
  if (reply <> '') then
    FTelnet.SendString(reply+#10);
end;

function TMainConsoleForm.passesFilter(line: String): boolean;
begin
  result := (FFilter = '') or line.ToLower.Contains(FFilter);
end;

function TMainConsoleForm.handleCommand(line: String): boolean;
begin
  result := false;
  if (line.startsWith('$@')) then
  begin
    if line.startsWith('$@ping: ') then
    begin
      FLock.Lock;
      try
        FServerStatus := line.Substring(8);
      finally
        FLock.unLock;
      end;
      exit(true);
    end;
    if line.startsWith('$@threads') then
    begin
      FLock.Lock;
      try
        FThreads.Text := line.subString(10).replace('|', #13#10).trim();
      finally
        FLock.unLock;
      end;
      exit(true);
    end;
  end;
  if line.startsWith('$@classes') then
  begin
    FLock.Lock;
    try
      FThreads.Text := line.subString(10).replace('|', #13#10).trim();
      FThreads.Sort;
    finally
      FLock.unLock;
    end;
    exit(true);
  end;
end;

procedure TMainConsoleForm.Connect;
begin
  if FStatus = csDiconnected then
  begin
    FTelnet.Host := FAddress;
    FTelnet.Connect;
  end;
end;

procedure TMainConsoleForm.sctCallback(pct: Integer; action: String);
begin
  prgSnomedImport.Position := pct;
  lblSCTAction.Caption := action;
  lblSCTAmount.Caption := inttostr(pct)+'%';
  prgSnomedImport.Update;
  lblSCTAction.Update;
  lblSCTAmount.Update;
  Application.ProcessMessages;
  if (FWantStop) then
    abort;
end;

procedure TMainConsoleForm.cmbCallback(pct: Integer; action: String);
begin
  prgCombine.Position := pct;
  lblCombineAction.Caption := action;
  lblCombineAmount.Caption := inttostr(pct)+'%';
  prgCombine.Update;
  lblCombineAction.Update;
  lblCombineAmount.Update;
  Application.ProcessMessages;
  if (FWantStop) then
    abort;
end;

procedure TMainConsoleForm.loincCallback(pct: Integer; action: String);
begin
  prgLoincImport.Position := pct;
  lblLoincAction.Caption := action;
  lblLoincAmount.Caption := inttostr(pct)+'%';
  prgLoincImport.Update;
  lblLoincAction.Update;
  lblLoincAmount.Update;
  Application.ProcessMessages;
  if (FWantStop) then
    abort;
end;

procedure TMainConsoleForm.SetUpTerminologyPage;
begin
  edtSource.text := FIni.ReadString('snomed-import', 'source', '');
  edtBase.text := FIni.ReadString('snomed-import', 'base', '');
  cbxEdition.ItemIndex := FIni.ReadInteger('snomed-import', 'edition', -1);
  edtDate.Date := FIni.ReadInteger('snomed-import', 'date', trunc(now));
  edtDestination.text := FIni.ReadString('snomed-import', 'dest', '');

  edtLoincSource.text := FIni.ReadString('loinc-import', 'source', '');
  edtLoincVersion.Text := FIni.ReadString('loinc-import', 'date', ''); // should not be date for is for legacy reasons
  edtLoincDest.text := FIni.ReadString('loinc-import', 'dest', '');
  edtLoincDate.Text := FIni.ReadString('loinc-import', 'tdate', '');

  edtInternational.text := FIni.ReadString('snomed-combine', 'base', '');
  lbEditions.Items.CommaText := FIni.ReadString('snomed-combine', 'editions', '');
  edtCombinedDestination.text := FIni.ReadString('snomed-combine', 'dest', '');
  edtCombinedStore.text := FIni.ReadString('snomed-combine', 'store', '');

  if lbEditions.Items.Count > 0 then
    lbEditions.Itemindex := 0;

  pnlSnomedImport.Color := rgb(217, 240, 247);
  pnlLoincImport.color := clWhite;
  pnlCombineSnomed.color := clWhite;
  pnlProcessUMLS.color := clWhite;

  pgMain.ActivePageIndex := 0;
  lbEditionsClick(self);
end;

function TMainConsoleForm.getSnomedModule: String;
begin
  case cbxEdition.itemindex of
    0 { International } : result := '900000000000207008';
    1 { US } :  result := '731000124108';
    2 { Australia } : result := '32506021000036107';
    3 { Belgium } : result := '11000172109';
    4 { Canada } : result := '20611000087101';
    5 { Spanish } : result := '449081005';
    6 { Denmark } : result := '554471000005108';
    7 { Netherlands } : result := '11000146104';
    8 { Sweden } : result := '45991000052106';
    9 { UK } : result := '999000041000000102';
    10 { } : result := inttostr(COMBINED_MODULE_ID);
  end;
end;

procedure TMainConsoleForm.DoIncoming(Sender: TIdTelnet; const Buffer: TIdBytes);
var
  s : String;
  ts : TStringList;
begin
  FLastIncoming := now;
  ts := TStringList.create;
  try
    ts.text := TEncoding.UTF8.GetAnsiString(buffer);
    for s in ts do
      processIncomingLine(s);
  finally
    ts.free;
  end;
end;

procedure TMainConsoleForm.DoConnected(Sender: TObject);
begin
  FLock.Lock;
  try
    FStatus := csUsername;
  finally
    FLock.Unlock;
  end;
  FConnected := true;
  btnClearCache.enabled := true;
  btnFetchThreads.enabled := true;
end;

procedure TMainConsoleForm.DoDisconnected(Sender: TObject);
begin
  FConnected := false;
  btnClearCache.enabled := false;
  btnFetchThreads.enabled := false;
end;


end.

