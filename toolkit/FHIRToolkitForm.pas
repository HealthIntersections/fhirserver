unit FHIRToolkitForm;

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

{$I fhir.inc}

{$IFDEF FHIR4}
{$DEFINE EXAMPLESCENARIO}
{$DEFINE IMPLEMENTATIONGUIDE}
{$ENDIF}
{$IFDEF OSX}
{$UNDEF IMPLEMENTATIONGUIDE}
{$ENDIF}

interface


uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Rtti,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Platform,
  FMX.Layouts, FMX.ListBox, FMX.TabControl, FMX.Controls.Presentation, FMX.DialogService,
  System.ImageList, FMX.ImgList, FMX.Menus, FMX.WebBrowser,
  IdSSLOpenSSLHeaders,
  FHIR.Support.Certs, fsl_threads, fsl_base, FHIR.Ui.FMX,
  fsl_logging,
  fhir_objects, fhir_factory, fhir_client, fhir_common, 
  fsl_fetcher,
  FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Resources.Base, FHIR.Version.Client, FHIR.Version.Utilities, fhir_indexing, FHIR.Version.IndexInfo, FHIR.Version.Constants,
  FHIR.Version.Context, FHIR.Version.Profiles, fsl_utilities, fsl_stream, fsl_npm_cache,
  fhir_oauth, FHIR.Smart.Login, FHIR.Client.ServerDialogFMX, FHIR.Ui.OSX,
  ValueSetEditor, NamingSystemEditor, HelpContexts, ProcessForm, SettingsDialog,
{$IFDEF EXAMPLESCENARIO} ExampleScenarioEditor, {$ENDIF}
  AboutDialog, ToolKitVersion, CodeSystemEditor, LibraryEditor,
{$IFDEF IMPLEMENTATIONGUIDE} ImplementationGuideEditor, ShellApi, {$ENDIF}
  ToolkitSettings, ServerForm, CapabilityStatementEditor, BaseResourceFrame, SourceViewer, ListSelector,
  ToolKitUtilities, UpgradeNeededDialog, QuestionnaireEditor, RegistryForm, ProviderDirectoryForm, ResourceLanguageDialog,
  PackageManagerFrame, PackageEditorFrame, ValidationFrame, TransformationFrame, DiffEngineFrame, UTGMgmtFrame,
  BaseFrame;

type
  TToolkitLogger = class(TFHIRClientLogger)
  private
    FLog: TLogger;
    function toChars(s: TStream): string;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure logExchange(verb, url, status, requestHeaders, responseHeaders: String; request, response: TStream); override;
  end;

  TVersionCheckerOutcome = class(TFslObject)
  private
    FVer: String;
  public
    constructor Create(ver: String);

    property ver: String read FVer write FVer;
  end;

  TVersionChecker = class(TBackgroundTaskEngine)
  private
  public
    function name: String; override;
    procedure execute; override;
  end;

  TMasterToolsForm = class(TForm)
    tbMain: TTabControl;
    Label2: TLabel;
    TabItem1: TTabItem;
    pnlToolbar: TPanel;
    Panel1: TPanel;
    lbServers: TListBox;
    btnConnect: TButton;
    btnAddServer: TButton;
    btnRemoveServer: TButton;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Label1: TLabel;
    lbFiles: TListBox;
    btnReopen: TButton;
    btnRemoveFile: TButton;
    btnOpen: TButton;
    btnNew: TButton;
    ToolBar1: TToolBar;
    tbnNew: TButton;
    tbnOpen: TButton;
    tbnConnect: TButton;
    odFile: TOpenDialog;
    StyleBook1: TStyleBook;
    ToolbarImages: TImageList;
    tbnSave: TButton;
    tbnSaveAs: TButton;
    tbnClose: TButton;
    Timer1: TTimer;
    sdFile: TSaveDialog;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileNew: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileSave: TMenuItem;
    mnuFileSaveAs: TMenuItem;
    mnuFileClose: TMenuItem;
    MenuItem1: TMenuItem;
    mnuFileExit: TMenuItem;
    tbnSource: TButton;
    mnuFileSource: TMenuItem;
    pnlHelp: TPanel;
    webHelp: TWebBrowser;
    tbnHelpContext: TButton;
    mnuHelp: TMenuItem;
    mnuHelpContext: TMenuItem;
    splitHelp: TSplitter;
    btnSettings: TButton;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem6: TMenuItem;
    mnuHelpAbout: TMenuItem;
    btnCopy: TButton;
    btnEditServer: TButton;
    mnuCheckVersion: TMenuItem;
    MenuItem5: TMenuItem;
    mnuRegistry: TMenuItem;
    Button1: TButton;
    MenuItem3: TMenuItem;
    mnuResourceLanguage: TMenuItem;
    mnuPackageManager: TMenuItem;
    MenuItem8: TMenuItem;
    Button2: TButton;
    Panel3: TPanel;
    Button3: TButton;
    mnuValidation: TMenuItem;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    mnuTransformation: TMenuItem;
    Button9: TButton;
    mnuSource: TMenuItem;
    MenuItem9: TMenuItem;
    btnFromUrl: TButton;
    Button10: TButton;
    MenuItem7: TMenuItem;
    MenuItem10: TMenuItem;
    btnExplorer: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    mnuProject: TMenuItem;
    mnuRepository: TMenuItem;
    mnuPublish: TMenuItem;
    MenuItem11: TMenuItem;
    mnuResourceOrganise: TMenuItem;
    ProgressBar1: TProgressBar;
    Button14: TButton;
    MenuItem12: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbServersClick(Sender: TObject);
    procedure lbFilesClick(Sender: TObject);
    procedure btnRemoveServerClick(Sender: TObject);
    procedure btnAddServerClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnReopenClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure tbnSaveClick(Sender: TObject);
    procedure tbnSaveAsClick(Sender: TObject);
    procedure tbnCloseClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnRemoveFileClick(Sender: TObject);
    procedure tbnSourceClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure tbnHelpContextClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure mnuHelpAboutClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnEditServerClick(Sender: TObject);
    procedure mnuCheckVersionClick(Sender: TObject);
    procedure mnuRegistryClick(Sender: TObject);
    procedure mnuResourceLanguageClick(Sender: TObject);
    procedure mnuPackageManagerClick(Sender: TObject);
    procedure mnuValidationClick(Sender: TObject);
    procedure mnuTransformationClick(Sender: TObject);
    procedure btnFromUrlClick(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure mnuRepositoryClick(Sender: TObject);
    procedure mnuPublishClick(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure mnuResourceOrganiseClick(Sender: TObject);
    procedure btnPasteAsNewClick(Sender: TObject);
  private
    { Private declarations }
    FSettings: TFHIRToolkitSettings;
    FShowHelp: Boolean;
    FFocus: TStyledControl;
    FIndexes: TFhirIndexList;
    FCache: TFHIRPackageManager;
    FContext: TToolkitWorkerContext;
    FLoadTaskId: integer;
    FVerCheckTaskId: integer;
    FIsStopped: Boolean;
    UpgradeOnClose: Boolean;
    FUpgradeChecked: Boolean;
    FRegistryTab: TTabItem;
    FPackageMgrTab: TTabItem;
    FValidationTab: TTabItem;
    FTransformationTab: TTabItem;
    FDiffEngineTab: TTabItem;
    FUTGTab: TTabItem;
    ToolkitLogger: TToolkitLogger;
    FServers: TFslList<TRegisteredFHIRServer>;
    FFactory: TFHIRFactory;

    procedure saveFiles;
    procedure openResourceFromFile(filename: String; res: TFHIRResource; format: TFHIRFormat; frameClass: TBaseResourceFrameClass);
    procedure OpenResourcefromClient(Sender: TObject; Client: TFHIRClient; format: TFHIRFormat; resource: TFHIRResource);
    procedure newResource(rClass: TFhirResourceClass; frameClass: TBaseResourceFrameClass); overload;
    procedure newResource(source : String); overload;
    procedure addFileToList(filename: String);
    function frameForResource(res: TFHIRResource): TBaseResourceFrameClass;
    function doSave: Boolean;
    function doSaveAs: Boolean;
    procedure updateHelpStatus;
    procedure updateHelpText;
    function processHelpContext(helpContext: String): String;
    function searchDesc(s: String): String;
    procedure fhirDefn(s: String; b: TStringBuilder);
    function GetStopped(context : pointer): Boolean;
    procedure DoIdle(out stop: Boolean);
    procedure DoOpenURL(url: String);
    procedure CheckVersionUpgradeOutome(id: integer; outcome: TFslObject);
    procedure checkVersion(reportIfCurrent: Boolean);
    procedure processVersionOoutcome(ver: String; reportIfCurrent: Boolean);
    procedure loadServers;
    procedure loadedResource(frameClass: TBaseResourceFrameClass; url: string; res: TFHIRResource);
  public
    procedure dowork(Sender: TObject; opName: String; canCancel: Boolean; proc: TWorkProc);
    procedure dowork2(sender : TObject; pct : integer; done : boolean; desc : String);
    procedure threadMonitorProc(Sender: TFhirClientV; var stop: Boolean);
  end;

var
  MasterToolsForm: TMasterToolsForm;

implementation

{$R *.fmx}

uses
{$IFDEF FHIR3} fhir3_factory; {$ENDIF}
{$IFDEF FHIR4} fhir4_factory, ProjectFilesDialog, {$IFDEF IMPLEMENTATIONGUIDE} IGPublishSettings, {$ENDIF} FDownloadForm, ScenarioRendering; {$ENDIF}

procedure TMasterToolsForm.addFileToList(filename: String);
var
  i: integer;
begin
  for i := lbFiles.Count - 1 downto 0 do
    if lbFiles.items[i] = filename then
      lbFiles.items.Delete(i);
  lbFiles.items.Insert(0, filename);
  saveFiles;
  lbFilesClick(nil);
end;

procedure TMasterToolsForm.btnAddServerClick(Sender: TObject);
var
  form: TEditRegisteredServerForm;
begin
  form := TEditRegisteredServerForm.Create(self);
  try
    form.SoftwareId := 'FHIR Toolkit';
    form.SoftwareVersion := ToolKitVersionBase + inttostr(BuildCount);
    form.Versions := FSettings.Versions.Link;
{$IFDEF FHIR3}
    form.Versions[CURRENT_FHIR_VERSION] := TFHIRFactoryR3.Create;
{$ENDIF}
{$IFDEF FHIR4}
    form.Versions[CURRENT_FHIR_VERSION] := TFHIRFactoryR4.Create;
{$ENDIF}
    form.DefaultVersion := CURRENT_FHIR_VERSION;
    form.Server := TRegisteredFHIRServer.Create;
    if ShowModalHack(form) = mrOk then
    begin
      FSettings.registerServer('', form.Server);
      loadServers;
      lbServersClick(nil);
    end;
  finally
    form.Free;
  end;
end;

procedure TMasterToolsForm.btnConnectClick(Sender: TObject);
var
  http: TFHIRClient;
  Client: TFHIRClient;
  tab: TTabItem;
  ServerForm: TServerFrame;
  cs: TFhirCapabilityStatement;
  Server: TRegisteredFHIRServer;
  Smart: TSmartAppLaunchLogin;
  ok: Boolean;
begin
  Server := TRegisteredFHIRServer(lbServers.ListItems[lbServers.ItemIndex].data);
  http := TFhirClients.makeHTTP(FContext.Link, Server.fhirEndpoint, Server.format, FSettings.Timeout * 1000, FSettings.proxy);
  try
    (http.Communicator as TFHIRHTTPCommunicator).username := Server.username;
    (http.Communicator as TFHIRHTTPCommunicator).password := Server.password;
    if Server.isSSL then
    begin
      (http.Communicator as TFHIRHTTPCommunicator).certFile := Server.SSLPublicCert;
      (http.Communicator as TFHIRHTTPCommunicator).certKey := Server.SSLPrivateKey;
      (http.Communicator as TFHIRHTTPCommunicator).certPWord := Server.SSLPassphrase;
    end;
    http.Logger := ToolkitLogger.Link;
    server.thisHost := 'localhost';
    ok := false;
    if Server.SmartAppLaunchMode <> salmNone then
    begin
      dowork(self, 'Logging in', true,
        procedure (context : pointer)
        begin
          Smart := TSmartAppLaunchLogin.Create;
          try
            Smart.Server := Server.Link;
            Smart.scopes := ['user/*.*'];
            Smart.OnIdle := DoIdle;
            Smart.OnOpenURL := DoOpenURL;
            Smart.name := 'FHIR Toolkit';
            Smart.Version := '0.0.' + inttostr(BuildCount);
            ok := Smart.Login;
            if ok then
              http.smartToken := Smart.token.Link;
          finally
            Smart.Free;
          end;
        end);
    end
    else
      ok := true;
    if not ok then
      exit;
    Client := TFhirClients.makeThreaded(nil, http.Link, threadMonitorProc);
    try
      cs := nil;
      dowork(nil, 'Connect', true,
        procedure (context : pointer)
        begin
          cs := Client.conformance(false);
        end);
      try
        if (cs <> nil) then
        begin
          tab := tbMain.Add(TTabItem);
          tab.Text := Server.name;
          tbMain.ActiveTab := tab;
          ServerForm := TServerFrame.Create(tab);
          ServerForm.Parent := tab;
          tab.TagObject := ServerForm;
          ServerForm.OnWork := dowork;
          ServerForm.TagObject := tab;
          ServerForm.tabs := tbMain;
          ServerForm.Settings := FSettings.Link;
          ServerForm.tab := tab;
          ServerForm.Align := TAlignLayout.Client;
          ServerForm.Client := Client.Link;
          ServerForm.CapabilityStatement := cs.Link;
          ServerForm.OnOpenResource := OpenResourcefromClient;
          ServerForm.OnWork := dowork;
          ServerForm.load;

          if lbServers.ItemIndex > 0 then
          begin
            lbServers.items.Exchange(0, lbServers.ItemIndex);
            FSettings.moveServerBefore('', TRegisteredFHIRServer(lbServers.ListItems[lbServers.ItemIndex].data), TRegisteredFHIRServer(lbServers.ListItems[0].data));
            lbServers.ItemIndex := 0;
          end;
        end;
      finally
        cs.Free;
      end;
    finally
      Client.Free;
    end;
  finally
    http.Free;
  end;
end;

procedure TMasterToolsForm.btnCopyClick(Sender: TObject);
var
  Svc: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
    Svc.SetClipboard(lbFiles.items[lbFiles.ItemIndex])
  else
    Beep;
end;

procedure TMasterToolsForm.btnEditServerClick(Sender: TObject);
var
  i: integer;
  form: TEditRegisteredServerForm;
begin
  form := TEditRegisteredServerForm.Create(self);
  try
    form.SoftwareId := ToolkitIdentifier;
    form.SoftwareVersion := ToolKitVersionBase + inttostr(BuildCount);
    form.Versions := TFHIRVersionFactories.Create;
    form.Server := TRegisteredFHIRServer(lbServers.ListItems[lbServers.ItemIndex].data).Link;
    if ShowModalHack(form) = mrOk then
    begin
      FSettings.updateServerInfo('', form.Server);
      loadServers;
      lbServersClick(nil);
    end;
  finally
    form.Free;
  end;
end;

procedure TMasterToolsForm.btnFromUrlClick(Sender: TObject);
var
  res: TFHIRResource;
  format: TFHIRFormat;
  Fetcher: TInternetFetcher;
begin
  InputQuery('Open From URL', ['Address'], [''],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if AResult = mrOk then
      begin
        Fetcher := TInternetFetcher.Create;
        try
          Fetcher.url := AValues[0];
          Fetcher.Accept := 'application/fhir+xml, application/fhir+json, application/xml, application/json';
          Fetcher.fetch;

          format := ffUnspecified;
          res := bytesToResource(Fetcher.buffer.asBytes, format);
          try
            if res is TFhirCapabilityStatement then
              loadedResource(TCapabilityStatementEditorFrame, Fetcher.url, res)
            else if res is TFhirValueSet then
              loadedResource(TValueSetEditorFrame, Fetcher.url, res)
            else if res is TFhirNamingSystem then
              loadedResource(TNamingSystemEditorFrame, Fetcher.url, res)
            else if res is TFhirCodeSystem then
              loadedResource(TCodeSystemEditorFrame, Fetcher.url, res)
            else if res is TFhirQuestionnaire then
              loadedResource(TQuestionnaireEditorFrame, Fetcher.url, res)
            else if res is TFhirLibrary then
              loadedResource(TLibraryEditorFrame, Fetcher.url, res)
{$IFDEF EXAMPLESCENARIO}
            else if res is TFHIRExampleScenario then
              loadedResource(TExampleScenarioEditorFrame, Fetcher.url, res)
{$ENDIF}
{$IFDEF IMPLEMENTATIONGUIDE}
            else if res is TFHIRImplementationGuide then
              loadedResource(TImplementationGuideEditorFrame, Fetcher.url, res)
{$ENDIF}
            else
              MessageDlg('Unsupported Resource Type: ' + res.fhirType, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
          finally
            res.Free;
          end;

        finally
          Fetcher.Free;
        end;
      end;
    end);
end;

procedure TMasterToolsForm.btnNewClick(Sender: TObject);
var
  form: TListSelectorForm;
begin
  form := TListSelectorForm.Create(nil);
  try
    form.ListBox1.ShowCheckboxes := false;
    form.ListBox1.items.Add('CapabilityStatement');
    form.ListBox1.items.Add('ValueSet');
    form.ListBox1.items.Add('NamingSystem');
    form.ListBox1.items.Add('CodeSystem');
    form.ListBox1.items.Add('Questionnaire');
    form.ListBox1.items.Add('Library');
{$IFDEF EXAMPLESCENARIO}
    form.ListBox1.items.Add('ExampleScenario');
{$ENDIF}
{$IFDEF IMPLEMENTATIONGUIDE}
    form.ListBox1.items.Add('ImplementationGuide');
{$ENDIF}
    form.caption := 'Create New File';
    if (ShowModalHack(form) = mrOk) then
      case form.ListBox1.ItemIndex of
        0: newResource(TFhirCapabilityStatement, TCapabilityStatementEditorFrame);
        1: newResource(TFhirValueSet, TValueSetEditorFrame);
        2: newResource(TFhirNamingSystem, TNamingSystemEditorFrame);
        3: newResource(TFhirCodeSystem, TCodeSystemEditorFrame);
        4: newResource(TFhirQuestionnaire, TQuestionnaireEditorFrame);
        5: newResource(TFhirLibrary, TLibraryEditorFrame);
{$IFDEF EXAMPLESCENARIO}
        6: newResource(TFHIRExampleScenario, TExampleScenarioEditorFrame);
{$IFDEF IMPLEMENTATIONGUIDE}
        7: newResource(TFHIRImplementationGuide, TImplementationGuideEditorFrame);
{$ENDIF}
{$ELSE}
{$IFDEF IMPLEMENTATIONGUIDE}
        6: newResource(TFHIRImplementationGuide, TImplementationGuideEditorFrame);
{$ENDIF}
{$ENDIF}
      end;
  finally
    form.Free;
  end;
end;

procedure TMasterToolsForm.btnOpenClick(Sender: TObject);
var
  res: TFHIRResource;
  format: TFHIRFormat;
  s : String;
  frame: TPackageEditorFrame;
  tab : TTabItem;
begin
  if odFile.execute then
  begin
    try
      s := odFile.fileName;
      if (s.endsWith('.tgz')) then
      begin
        tab := tbMain.Add(TTabItem);
        tbMain.ActiveTab := tab;
        tab.Text := odFile.fileName;
        frame := TPackageEditorFrame.Create(tab);
        frame.form := self;
        tab.TagObject := frame;
        frame.TagObject := tab;
        frame.Parent := tab;
        frame.tabs := tbMain;
        frame.OnWork := dowork;
        frame.Settings := FSettings.Link;
        frame.tab := tab;
        frame.Align := TAlignLayout.Client;
        frame.Source := s;
        frame.load;
      end
      else
      begin
        format := ffUnspecified;
        res := fileToResource(odFile.filename, format);
        try
          if res is TFhirCapabilityStatement then
            openResourceFromFile(odFile.filename, res, format, TCapabilityStatementEditorFrame)
          else if res is TFhirValueSet then
            openResourceFromFile(odFile.filename, res, format, TValueSetEditorFrame)
          else if res is TFhirNamingSystem then
            openResourceFromFile(odFile.filename, res, format, TNamingSystemEditorFrame)
          else if res is TFhirCodeSystem then
            openResourceFromFile(odFile.filename, res, format, TCodeSystemEditorFrame)
          else if res is TFhirQuestionnaire then
            openResourceFromFile(odFile.filename, res, format, TQuestionnaireEditorFrame)
          else if res is TFhirLibrary then
            openResourceFromFile(odFile.filename, res, format, TLibraryEditorFrame)
  {$IFDEF EXAMPLESCENARIO}
          else if res is TFHIRExampleScenario then
            openResourceFromFile(odFile.filename, res, format, TExampleScenarioEditorFrame)
  {$ENDIF}
  {$IFDEF IMPLEMENTATIONGUIDE}
          else if res is TFHIRImplementationGuide then
            openResourceFromFile(odFile.filename, res, format, TImplementationGuideEditorFrame)
  {$ENDIF}
          else
            MessageDlg('Unsupported Resource Type: ' + res.fhirType, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
        finally
          res.Free;
        end;
      end;  
    except
      on e: Exception do
        MessageDlg('Error reading Resource: ' + e.Message, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    end;
  end;
end;

procedure TMasterToolsForm.btnRemoveFileClick(Sender: TObject);
var
  i: integer;
begin
  i := lbFiles.ItemIndex;
  lbFiles.items.Delete(i);
  if i = lbFiles.items.Count then
    dec(i);
  lbFiles.ItemIndex := i;
  saveFiles;
  lbFilesClick(nil);
end;

procedure TMasterToolsForm.btnRemoveServerClick(Sender: TObject);
var
  i: integer;
begin
  i := lbServers.ItemIndex;
  FSettings.DeleteServer('', TRegisteredFHIRServer(lbServers.ListItems[lbServers.ItemIndex].data));
  lbServers.items.Delete(i);
  if i = lbServers.items.Count then
    dec(i);
  lbServers.ItemIndex := i;
  lbServersClick(nil);
end;

procedure TMasterToolsForm.btnReopenClick(Sender: TObject);
var
  res: TFHIRResource;
  fn: String;
  format: TFHIRFormat;
begin
  fn := lbFiles.items[lbFiles.ItemIndex];
  try
    format := ffUnspecified;
    res := fileToResource(fn, format);
    try
      openResourceFromFile(fn, res, format, frameForResource(res));
    finally
      res.Free;
    end;
  except
    on e: Exception do
      MessageDlg('Error reading Resource: ' + e.Message, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TMasterToolsForm.btnSettingsClick(Sender: TObject);
var
  frame: TBaseFrame;
  form: TSettingsForm;
  i: integer;
begin
  form := TSettingsForm.Create(self);
  try
    form.Settings := FSettings.Link;
    if ShowModalHack(form) = mrOk then
      for i := 0 to tbMain.TabCount - 1 do
        if tbMain.tabs[i].TagObject is TBaseFrame then
          TBaseFrame(tbMain.tabs[i].TagObject).SettingsChanged;
  finally
    form.Free;
  end;
end;

procedure TMasterToolsForm.btnStopClick(Sender: TObject);
begin
  FIsStopped := true;
end;

procedure TMasterToolsForm.btnPasteAsNewClick(Sender: TObject);
var
  Svc: IFMXClipboardService;
  text : String;
  Value: TValue;
begin
 if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
  begin
    Value := Svc.GetClipboard;
    if not Value.IsEmpty then
    begin
      if Value.IsType<string> then
      begin
        Text := Value.ToString;
        newResource(text);
      end;
    end;
  end;
end;

procedure TMasterToolsForm.tbnSaveClick(Sender: TObject);
begin
  doSave;
end;

procedure TMasterToolsForm.tbnSourceClick(Sender: TObject);
var
  form: TSourceViewerForm;
  frame: TBaseFrame;
begin
  frame := tbMain.ActiveTab.TagObject as TBaseFrame;
  if (frame <> nil) and frame.hasResource then
  begin
    form := TSourceViewerForm.Create(self);
    try
      form.Current := frame.currentResource.Link;
      form.original := frame.originalResource.Link;
      form.Factory := FFactory.Link;
      ShowModalHack(form);
    finally
      form.Free;
    end;
  end;
end;

procedure TMasterToolsForm.threadMonitorProc(Sender: TFhirClientV; var stop: Boolean);
begin
  Application.ProcessMessages;
  stop := FIsStopped;
end;

procedure TMasterToolsForm.tbnSaveAsClick(Sender: TObject);
begin
  doSaveAs;
end;

procedure TMasterToolsForm.tbnCloseClick(Sender: TObject);
var
  i: integer;
  frame: TBaseFrame;
  procedure closeit;
  begin
    i := tbMain.TabIndex;
    if tbMain.ActiveTab = FRegistryTab then
      FRegistryTab := nil;
    if tbMain.ActiveTab = FValidationTab then
      FValidationTab := nil;
    if tbMain.ActiveTab = FPackageMgrTab then
      FPackageMgrTab := nil;
    if tbMain.ActiveTab = FTransformationTab then
      FTransformationTab := nil;
    tbMain.ActiveTab.Free;
    if i > 0 then
      tbMain.TabIndex := i - 1
    else
      tbMain.TabIndex := 0;
  end;

begin
  frame := tbMain.ActiveTab.TagObject as TBaseFrame;
  if (frame <> nil) then
  begin
    if frame.isDirty then
    begin
      case MessageDlg(tbMain.ActiveTab.Text + ' contains unsaved data. Save the data first?', TMsgDlgType.mtConfirmation, mbYesNoCancel, 0) of
        mrYes:
          if doSave then
            closeit;
        mrNo:
          closeit;
        mrCancel:
          exit;
      end;
    end
    else
      closeit;
  end;
end;

procedure TMasterToolsForm.tbnHelpContextClick(Sender: TObject);
begin
  FShowHelp := not FShowHelp;
  FSettings.ShowHelp := FShowHelp;
  FFocus := nil;
  updateHelpStatus;
end;

procedure TMasterToolsForm.CheckVersionUpgradeOutome(id: integer; outcome: TFslObject);
begin
  processVersionOoutcome((outcome as TVersionCheckerOutcome).ver, false);
end;

procedure TMasterToolsForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TMasterToolsForm.DoIdle(out stop: Boolean);
begin
  Application.ProcessMessages;
  stop := GetStopped(nil);
end;

procedure TMasterToolsForm.DoOpenURL(url: String);
begin
  openURL(url);
end;

function TMasterToolsForm.GetStopped(context : pointer): Boolean;
begin
  result := FIsStopped;
end;

function TMasterToolsForm.doSave: Boolean;
var
  frame: TBaseFrame;
  ok: Boolean;
begin
  result := false;
  frame := tbMain.ActiveTab.TagObject as TBaseFrame;

  if frame <> nil then
  begin
    if frame.canSave then
    begin
      ok := false;
      frame.OnStopped := GetStopped;
      frame.work('Save', false,
        procedure (context : pointer)
        begin;
          ok := frame.save;
        end);
      result := ok;
    end
    else
      result := doSaveAs;
  end;
end;

function TMasterToolsForm.doSaveAs: Boolean;
var
  frame: TBaseFrame;
  ok: Boolean;
  fn, ext: String;
begin
  result := false;
  frame := tbMain.ActiveTab.TagObject as TBaseFrame;

  if frame <> nil then
  begin
    if sdFile.execute then
    begin
      if frame.canSaveAs then
      begin
        ok := false;
        frame.work('Save As', false,
          procedure (context : pointer)
          begin
            fn := sdFile.filename;
            ext := ExtractFileExt(fn).ToLower;
            if (ext = '.xml') then
              ok := frame.saveAs(fn, ffXml)
            else if (ext = '.json') then
              ok := frame.saveAs(fn, ffJson)
            else if (ext = '.ttl') then
              ok := frame.saveAs(fn, ffTurtle)
            else if sdFile.FilterIndex = 1 then
              ok := frame.saveAs(fn, ffXml)
            else if sdFile.FilterIndex = 2 then
              ok := frame.saveAs(fn, ffJson)
            else if sdFile.FilterIndex = 3 then
              ok := frame.saveAs(fn, ffTurtle)
            else
              raise EFHIRException.Create('Unknown format');
            addFileToList(sdFile.filename);
          end);
        result := ok;
      end
      else
        raise EFHIRException.Create('Unable to save file');
    end;
  end;
end;

procedure TMasterToolsForm.dowork(Sender: TObject; opName: String; canCancel: Boolean; proc: TWorkProc);
var
  fcs: IFMXCursorService;
  form: TProcessingForm;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    fcs := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService
  else
    fcs := nil;
  if Assigned(fcs) then
  begin
    Cursor := fcs.GetCursor;
    fcs.SetCursor(crHourGlass);
  end;
  try
    FIsStopped := false;
    if Assigned(Sender) and (Sender is TBaseFrame) then
      TBaseFrame(Sender).OnStopped := GetStopped;
    form := TProcessingForm.Create(self);
    try
      form.lblOperation.Text := opName;
      form.Button1.enabled := canCancel;
      form.Button1.OnClick := btnStopClick;
      form.proc := proc;
      ShowModalHack(form);
    finally
      form.Free;
    end;
  finally
    if Assigned(fcs) then
      fcs.SetCursor(Cursor);
  end;
end;

procedure TMasterToolsForm.dowork2(sender: TObject; pct: integer; done: boolean; desc: String);
begin
//  if done then
//  begin
//    ProgressBar1.Visible := false
//  end
//  else
//  begin
//    ProgressBar1.Visible := true;
//    ProgressBar1.Value := pct;
//  end;
end;

procedure TMasterToolsForm.fhirDefn(s: String; b: TStringBuilder);
var
  n: string;
  sd: TFhirStructureDefinition;
  ed: TFhirElementDefinition;
begin
  if s.Contains('.') then
    n := s.Substring(0, s.IndexOf('.'))
  else
    n := s;
  sd := FContext.fetchResource(frtStructureDefinition, 'http://hl7.org/fhir/StructureDefinition/' + n) as TFhirStructureDefinition;
  if sd = nil then
    b.Append('<p>uUnknown path:' + s + '</p>')
  else
  begin
    for ed in sd.snapshot.elementList do
    begin
      if ed.path = s then
      begin
        b.Append('<p><b>' + s + '</b></p>');
        b.Append('<p>Definition: ' + ed.definition + '</p>');
        if ed.comment <> '' then
          b.Append('<p>Comments: ' + ed.comment + '</p>');
        exit;
      end;
    end;
    b.Append('<p>Unknown path:' + s + '</p>')
  end;
end;

procedure TMasterToolsForm.FormActivate(Sender: TObject);
var
  Factory: TFHIRFactory;
begin
  try
    if FContext = nil then
    begin
      Factory := {$IFDEF FHIR3} TFHIRFactoryR3.Create {$ENDIF}  {$IFDEF FHIR4} TFHIRFactoryR4.Create {$ENDIF};
      try
        FCache := TFHIRPackageManager.Create(true);
        FCache.OnWork := doWork2;
        if not FCache.packageExists(Factory.corePackage, Factory.versionString) then
        begin
          FCache.autoInstallPackage(Factory.corePackage, Factory.versionString);
          // ShowMessage('The base FHIR package ' + Factory.versionString + ' is not installed; you will need to install it using the package manager and restart');
        end;
        FContext := TToolkitWorkerContext.Create(Factory.Link);
        FLoadTaskId := GBackgroundTasks.registerTaskEngine(TBackgroundContextLoader.Create(FContext.loadStructures));
        GBackgroundTasks.queueTask(FLoadTaskId, TBackgroundContextLoadingInformation.Create(FCache.Link, Factory.corePackage, Factory.versionString, FContext.Link));
        if not(IdSSLOpenSSLHeaders.load and LoadEAYExtensions(false)) then
          ShowMessage('Unable to load openSSL - SSL/Crypto functions will fail (technical details: ' + WhichFailedToLoad + ', ' + WhichFailedToLoad2 + ')');
        checkSSL; // really, this is just to init internal structures in openSSL

        FVerCheckTaskId := GBackgroundTasks.registerTaskEngine(TVersionChecker.Create(CheckVersionUpgradeOutome));
        if FSettings.CheckForUpgradesOnStart then
          GBackgroundTasks.queueTask(FVerCheckTaskId, TFslObject.Create);
      finally
        Factory.Free;
      end
    end;
  except
    on e : exception do
      showmessage(e.message);
  end;
end;

procedure TMasterToolsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  dirty: Boolean;
  i: integer;
  obj: TBaseFrame;
  form: TListSelectorForm;
begin
  dirty := false;
  form := TListSelectorForm.Create(self);
  try
    form.caption := 'Unsaved Content found. Which files do you want to save?';
    form.okWithNoneSelected := true;
    form.Button1.Text := 'Close';
    for i := 1 to tbMain.TabCount - 1 do
    begin
      obj := tbMain.tabs[i].TagObject as TBaseFrame;
      if (obj.isDirty) then
      begin
        dirty := true;
        form.ListBox1.items.AddObject(obj.nameForSaveDialog, obj)
      end;
    end;
    if not dirty then
      CanClose := true
    else
      CanClose := ShowModalHack(form) = mrOk;
    for i := 0 to form.ListBox1.items.Count - 1 do
      if form.ListBox1.ListItems[i].IsChecked then
        if not TBaseFrame(form.ListBox1.items.Objects[i]).save then
        begin
          CanClose := false;
          exit;
        end;
  finally
    form.Free;
  end;
end;

function versionSettingsString: String;
begin
  result := '';
  if (FHIR_GENERATED_PUBLICATION <> '3') then
    result := '-r' + FHIR_GENERATED_PUBLICATION;
end;

procedure TMasterToolsForm.FormCreate(Sender: TObject);
begin
  FSettings := TFHIRToolkitSettings.Create(IncludeTrailingPathDelimiter(SystemTemp) + 'fhir-toolkit-settings.json',
{$IFDEF FHIR3}
  [fhirVersionRelease3]
{$ELSE}
  [fhirVersionRelease4]
{$ENDIF}
    );
  FSettings.CacheManager := TFHIRPackageManager.Create(true);
  FSettings.Versions := TFHIRVersionFactories.Create;
{$IFDEF FHIR3}
  FSettings.Versions.Version[fhirVersionRelease3] := TFHIRFactoryR3.Create;
{$ELSE}
  FSettings.Versions.Version[fhirVersionRelease4] := TFHIRFactoryR4.Create;
{$ENDIF}
  loadServers;
  lbServers.ItemIndex := 0;
  lbServersClick(self);
  FSettings.getValues('Files' + versionSettingsString, lbFiles.items);
  if lbFiles.items.Count > 0 then
    lbFiles.ItemIndex := 0;
  lbFilesClick(self);
  Left := FSettings.getValue('Window', 'left', Left);
  Top := FSettings.getValue('Window', 'top', Top);
  Width := FSettings.getValue('Window', 'width', Width);
  Height := FSettings.getValue('Window', 'height', Height);
  FShowHelp := FSettings.ShowHelp;
  updateHelpStatus;
{$IFDEF MACOS}
  mnuFileExit.Text := '&Quit';
{$ENDIF}
  caption := 'FHIR Toolkit (R' + FHIR_GENERATED_PUBLICATION + ')';
  ToolkitLogger := TToolkitLogger.Create;
{$IFDEF FHIR2} FFactory := TFHIRFactoryR2.Create; {$ENDIF}
{$IFDEF FHIR3} FFactory := TFHIRFactoryR3.Create; {$ENDIF}
{$IFDEF FHIR4} FFactory := TFHIRFactoryR4.Create; {$ENDIF}
end;

procedure TMasterToolsForm.FormDestroy(Sender: TObject);
var
  newVersion: String;
begin
  saveFiles;
  try
    FSettings.storeValue('Window', 'left', Left);
    FSettings.storeValue('Window', 'top', Top);
    FSettings.storeValue('Window', 'width', Width);
    FSettings.storeValue('Window', 'height', Height);
    FSettings.save;
  except
  end;
  FSettings.Free;
  FIndexes.Free;
  FContext.Free;
  FServers.Free;
  FCache.Free;
  ToolkitLogger.Free;
  if UpgradeOnClose then
  begin
    dowork(self, 'Checking Version', true,
      procedure (context : pointer)
      begin
        newVersion := checkUpgrade;
      end);
    doUpgrade(newVersion);
  end;
  if FFactory <> nil then
    FFactory.Destroy; // this seemed to leak
end;

function TMasterToolsForm.frameForResource(res: TFHIRResource): TBaseResourceFrameClass;
begin
  if res is TFhirCapabilityStatement then
    result := TCapabilityStatementEditorFrame
  else if res is TFhirValueSet then
    result := TValueSetEditorFrame
  else if res is TFhirNamingSystem then
    result := TNamingSystemEditorFrame
  else if res is TFhirCodeSystem then
    result := TCodeSystemEditorFrame
  else if res is TFhirQuestionnaire then
    result := TQuestionnaireEditorFrame
  else if res is TFhirLibrary then
    result := TLibraryEditorFrame
{$IFDEF EXAMPLESCENARIO}
  else if res is TFHIRExampleScenario then
    result := TExampleScenarioEditorFrame
{$ENDIF}
{$IFDEF IMPLEMENTATIONGUIDE}
  else if res is TFHIRImplementationGuide then
    result := TImplementationGuideEditorFrame
{$ENDIF}
  else
    MessageDlg('Unsupported Resource Type: ' + res.fhirType, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
end;

procedure TMasterToolsForm.lbFilesClick(Sender: TObject);
begin
  btnReopen.enabled := lbFiles.ItemIndex >= 0;
  btnRemoveFile.enabled := lbFiles.ItemIndex >= 0;
  btnCopy.enabled := lbFiles.ItemIndex >= 0;
end;

procedure TMasterToolsForm.lbServersClick(Sender: TObject);
begin
  btnConnect.enabled := lbServers.ItemIndex >= 0;
  btnRemoveServer.enabled := lbServers.ItemIndex >= 0;
  btnEditServer.enabled := lbServers.ItemIndex >= 0;
end;

procedure TMasterToolsForm.loadServers;
var
  i: integer;
begin
  if FServers = nil then
    FServers := TFslList<TRegisteredFHIRServer>.Create
  else
    FServers.Clear;
  lbServers.items.Clear;
  FSettings.ListServers('', FServers);
  for i := 0 to FServers.Count - 1 do
  begin
    lbServers.items.Add(FServers[i].name + ': ' + FServers[i].fhirEndpoint);
    lbServers.ListItems[i].data := FServers[i];
  end;
end;

procedure TMasterToolsForm.MenuItem10Click(Sender: TObject);
begin
  MessageDlg('test', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbOK], 0);
end;

procedure TMasterToolsForm.MenuItem11Click(Sender: TObject);
var
  frame: TUTGManagementFrame;
begin
  if FUTGTab <> nil then
    tbMain.ActiveTab := FUTGTab
  else
  begin
    FUTGTab := tbMain.Add(TTabItem);
    tbMain.ActiveTab := FUTGTab;
    FUTGTab.Text := 'UTG Management';
    frame := TUTGManagementFrame.Create(FDiffEngineTab);
    frame.form := self;
    FUTGTab.TagObject := frame;
    frame.TagObject := FUTGTab;
    frame.Parent := FUTGTab;
    frame.tabs := tbMain;
    frame.OnWork := dowork;
    frame.Settings := FSettings.Link;
    frame.tab := FDiffEngineTab;
    frame.Align := TAlignLayout.Client;
    frame.load;
  end;

end;

procedure TMasterToolsForm.mnuResourceOrganiseClick(Sender: TObject);
var
  frame: TBaseFrame;
begin
  frame := tbMain.ActiveTab.TagObject as TBaseFrame;
  frame.organise;
end;

procedure TMasterToolsForm.MenuItem7Click(Sender: TObject);
var
  frame: TDiffEngineEngineFrame;
begin
  if FDiffEngineTab <> nil then
    tbMain.ActiveTab := FDiffEngineTab
  else
  begin
    FDiffEngineTab := tbMain.Add(TTabItem);
    tbMain.ActiveTab := FDiffEngineTab;
    FDiffEngineTab.Text := 'Diff Engine';
    frame := TDiffEngineEngineFrame.Create(FDiffEngineTab);
    frame.form := self;
    FDiffEngineTab.TagObject := frame;
    frame.TagObject := FDiffEngineTab;
    frame.Parent := FDiffEngineTab;
    frame.tabs := tbMain;
    frame.OnWork := dowork;
    frame.Settings := FSettings.Link;
    frame.tab := FDiffEngineTab;
    frame.Align := TAlignLayout.Client;
    frame.load;
  end;
end;

procedure TMasterToolsForm.mnuResourceLanguageClick(Sender: TObject);
var
  ResourceLanguageForm: TResourceLanguageForm;
  frame: TBaseFrame;
begin
  frame := tbMain.ActiveTab.TagObject as TBaseFrame;
  if (frame <> nil) and frame.hasResource then
  begin
    ResourceLanguageForm := TResourceLanguageForm.Create(self);
    try
      ResourceLanguageForm.resource := frame.currentResource.Link;
      if ShowModalHack(ResourceLanguageForm) = mrOk then
        frame.reload;
    finally
      ResourceLanguageForm.Free;
    end;
  end;
end;

procedure TMasterToolsForm.mnuTransformationClick(Sender: TObject);
var
  frame: TTransformationEngineFrame;
begin
  if FTransformationTab <> nil then
    tbMain.ActiveTab := FTransformationTab
  else
  begin
    FTransformationTab := tbMain.Add(TTabItem);
    tbMain.ActiveTab := FTransformationTab;
    FTransformationTab.Text := 'General Transformation';
    frame := TTransformationEngineFrame.Create(FTransformationTab);
    frame.form := self;
    FTransformationTab.TagObject := frame;
    frame.TagObject := FTransformationTab;
    frame.Parent := FTransformationTab;
    frame.tabs := tbMain;
    frame.OnWork := dowork;
    frame.Settings := FSettings.Link;
    frame.tab := FTransformationTab;
    frame.Align := TAlignLayout.Client;
    frame.load;
  end;
end;

procedure TMasterToolsForm.mnuValidationClick(Sender: TObject);
var
  frame: TValidationEngineFrame;
begin
  if FValidationTab <> nil then
    tbMain.ActiveTab := FValidationTab
  else
  begin
    FValidationTab := tbMain.Add(TTabItem);
    tbMain.ActiveTab := FValidationTab;
    FValidationTab.Text := 'General Validation';
    frame := TValidationEngineFrame.Create(FValidationTab);
    frame.form := self;
    FValidationTab.TagObject := frame;
    frame.TagObject := FValidationTab;
    frame.Parent := FValidationTab;
    frame.tabs := tbMain;
    frame.OnWork := dowork;
    frame.Settings := FSettings.Link;
    frame.tab := FValidationTab;
    frame.Align := TAlignLayout.Client;
    frame.load;
  end;
end;

procedure TMasterToolsForm.newResource(source: String);
var
  format : TFHIRFormat;
  res : TFHIRResource;
  tab: TTabItem;
  frame: TBaseResourceFrame;
  fcs: IFMXCursorService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    fcs := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService
  else
    fcs := nil;
  if Assigned(fcs) then
  begin
    Cursor := fcs.GetCursor;
    fcs.SetCursor(crHourGlass);
  end;
  try
    format := ffUnspecified;
    res := stringToResource(source, format);
    try
      tab := tbMain.Add(TTabItem);
      tbMain.ActiveTab := tab;
      frame := frameForResource(res).Create(tab);
      tab.TagObject := frame;
      tab.Text := 'New ' + res.fhirType;
      tab.Hint := tab.Text;
      tab.ShowHint := true;
      frame.TagObject := tab;
      frame.Parent := tab;
      frame.tabs := tbMain;
      frame.OnWork := dowork;
      frame.Settings := FSettings.Link;
      frame.tab := tab;
      frame.Align := TAlignLayout.Client;
      frame.filename := '$$';
      frame.resource := res.link;
      frame.format := format;
      frame.load;
    finally
      res.free;
    end;
  finally
    if Assigned(fcs) then
      fcs.SetCursor(Cursor);
  end;
end;

procedure TMasterToolsForm.mnuCheckVersionClick(Sender: TObject);
begin
  checkVersion(true);
end;

procedure TMasterToolsForm.checkVersion(reportIfCurrent: Boolean);
var
  newVersion: String;
begin
  dowork(self, 'Checking Version', true,
    procedure (context : pointer)
    begin
      newVersion := checkUpgrade;
    end);
  processVersionOoutcome(newVersion, reportIfCurrent);
end;

procedure TMasterToolsForm.processVersionOoutcome(ver: String; reportIfCurrent: Boolean);
var
  upg: TUpgradeNeededForm;
begin
  if (ver <> '') and (ver <> '0.0.' + inttostr(BuildCount)) then
  begin
    upg := TUpgradeNeededForm.Create(self);
    try
      upg.Settings := FSettings.Link;
      upg.lblVersion.Text := 'The current version is ' + ver + ', you are running 0.0.' + inttostr(BuildCount) + '. Upgrade?';
      case ShowModalHack(upg) of
        mrContinue:
          UpgradeOnClose := true;
        mrYes:

          begin
            doUpgrade(ver);
            Close;
          end;
      end;
    finally
      upg.Free;
    end;
  end
  else if reportIfCurrent then
    ShowMessage('The FHIR Toolkit is up to date');
end;

procedure TMasterToolsForm.mnuHelpAboutClick(Sender: TObject);
var
  form: TAboutForm;
begin
  form := TAboutForm.Create(self);
  try
    ShowModalHack(form);
  finally
    form.Free;
  end;
end;

procedure TMasterToolsForm.mnuPackageManagerClick(Sender: TObject);
var
  frame: TPackageManagerFrame;
begin
  if FPackageMgrTab <> nil then
    tbMain.ActiveTab := FPackageMgrTab
  else
  begin
    FPackageMgrTab := tbMain.Add(TTabItem);
    tbMain.ActiveTab := FPackageMgrTab;
    FPackageMgrTab.Text := 'Package Manager';
    frame := TPackageManagerFrame.Create(FPackageMgrTab);
    frame.form := self;
    FPackageMgrTab.TagObject := frame;
    frame.TagObject := FPackageMgrTab;
    frame.Parent := FPackageMgrTab;
    frame.tabs := tbMain;
    frame.OnWork := dowork;
    frame.Settings := FSettings.Link;
    frame.tab := FPackageMgrTab;
    frame.Align := TAlignLayout.Client;
    frame.load;
  end;
end;

procedure TMasterToolsForm.mnuPublishClick(Sender: TObject);
var
  frame: TBaseResourceFrame;
  res: TFHIRResource;
  FSettings: TForm;
  errorstring, str: string;

begin
{$IFDEF IMPLEMENTATIONGUIDE}
  frame := tbMain.ActiveTab.TagObject as TBaseResourceFrame;
  if ((frame.resource is TFHIRImplementationGuide) or (frame.resource is TFHIRExampleScenario)) then
  begin
    if frame.filename = '' then
      ShowMessage('File is not saved. Please save the file first.')
    else

      if (frame.resource is TFHIRImplementationGuide) then

      with frame as TImplementationGuideEditorFrame do
      begin
        IGRootFolder := ExtractFileDir(ExcludeTrailingBackslash(ExtractFileDir(frame.filename)));
        IGFileName := extractfilename(frame.filename);

        ShellExecute(0, 'open', 'C:\HL7\server\fhirserver\utilities\publisher\Win32\Release\IGPublisher.exe ', PChar(IGRootFolder), nil, 1);

{
// Opening the publisher from code
        PublisherForm:=TPublisherForm.create(self);
        PublisherForm.IGtoPublish:=IGRootFolder;
        PublisherForm.ShowModal;
        PublisherForm.Destroy;
}


      end

    else if (frame.resource is TFHIRExampleScenario) then

      with frame as TExampleScenarioEditorFrame do
      begin
        ESRootFolder := extractfiledir(frame.filename);
        ESFileName := extractfilename(frame.filename);

        ESPublishForm := TESPublishForm.Create(self);
        ESPublishForm.Edit1.Text := ESPublisherFolder;

        ESPublishForm.ESRootFolder := ESRootFolder;
        ESPublishForm.ESPublisherFolder := ESPublisherFolder;
        ESPublishForm.ESFileName := ESFileName;

        ESPublishForm.ShowModal;

        // if IGSettingsForm.ModalResult = mrOK then
        begin
          ESRootFolder := ESPublishForm.ESRootFolder;
          ESPublisherFolder := ESPublishForm.ESPublisherFolder;
          str := ESPublisherFolder;

          // pandocfolder := IGSettingsForm.pandocFolder;
        end;
        ESPublishForm.Destroy;

      end
  end;
{$ELSE}
    raise Exception.Create('This is not supported in R3');
{$ENDIF}
end;

procedure TMasterToolsForm.mnuRegistryClick(Sender: TObject);
var
  frame: TRegistryFrame;
begin
  if FRegistryTab <> nil then
    tbMain.ActiveTab := FRegistryTab
  else
  begin
    FRegistryTab := tbMain.Add(TTabItem);
    tbMain.ActiveTab := FRegistryTab;
    FRegistryTab.Text := 'registry.fhir.org';
    frame := TRegistryFrame.Create(FRegistryTab);
    FRegistryTab.TagObject := frame;
    frame.TagObject := FRegistryTab;
    frame.Parent := FRegistryTab;
    frame.tabs := tbMain;
    frame.OnWork := dowork;
    frame.OnOpenResource := OpenResourcefromClient;
    frame.Settings := FSettings.Link;
    frame.tab := FRegistryTab;
    frame.Align := TAlignLayout.Client;
    frame.load;
  end;
end;

procedure TMasterToolsForm.mnuRepositoryClick(Sender: TObject);
{$IFDEF FHIR4}
var
  filename: string;
  projectFolder: string;
  ProjectFilesDialog: TProjectDialog;
  frame: TBaseResourceFrame;
begin
  frame := tbMain.ActiveTab.TagObject as TBaseResourceFrame;

  if extractfiledir(frame.filename) = '' then
    ShowMessage('File is not saved. Please save the file first.')
  else
  begin
    ProjectFilesDialog := TProjectDialog.Create(self);
    ProjectFilesDialog.Edit4.Text := extractfiledir(frame.filename);
    ProjectFilesDialog.ShowModal;
    ProjectFilesDialog.Destroy;
  end;
end;
{$ELSE}

begin
  raise Exception.Create('Not supported in R3');
end;
{$ENDIF}

procedure TMasterToolsForm.loadedResource(frameClass: TBaseResourceFrameClass; url: string; res: TFHIRResource);
var
  tab: TTabItem;
  frame: TBaseResourceFrame;
  fcs: IFMXCursorService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    fcs := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService
  else
    fcs := nil;
  if Assigned(fcs) then
  begin
    Cursor := fcs.GetCursor;
    fcs.SetCursor(crHourGlass);
  end;
  try
    tab := tbMain.Add(TTabItem);
    tbMain.ActiveTab := tab;
    tab.Text := url;
    tab.Hint := tab.Text;
    tab.ShowHint := true;
    frame := frameClass.Create(tab);
    tab.TagObject := frame;
    frame.TagObject := tab;
    frame.Parent := tab;
    frame.tabs := tbMain;
    frame.OnWork := dowork;
    frame.Settings := FSettings.Link;
    frame.tab := tab;
    frame.Align := TAlignLayout.Client;
    frame.resource := res.Link;
    frame.load;
  finally
    if Assigned(fcs) then
      fcs.SetCursor(Cursor);
  end;
end;

procedure TMasterToolsForm.newResource(rClass: TFhirResourceClass; frameClass: TBaseResourceFrameClass);
var
  tab: TTabItem;
  frame: TBaseResourceFrame;
  fcs: IFMXCursorService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    fcs := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService
  else
    fcs := nil;
  if Assigned(fcs) then
  begin
    Cursor := fcs.GetCursor;
    fcs.SetCursor(crHourGlass);
  end;
  try
    tab := tbMain.Add(TTabItem);
    tbMain.ActiveTab := tab;
    tab.Text := 'New ' + rClass.ClassName.Substring(5);
    tab.Hint := tab.Text;
    tab.ShowHint := true;
    frame := frameClass.Create(tab);
    tab.TagObject := frame;
    frame.TagObject := tab;
    frame.Parent := tab;
    frame.tabs := tbMain;
    frame.OnWork := dowork;
    frame.Settings := FSettings.Link;
    frame.tab := tab;
    frame.Align := TAlignLayout.Client;
    frame.resource := rClass.Create;
    frame.load;
  finally
    if Assigned(fcs) then
      fcs.SetCursor(Cursor);
  end;
end;

procedure TMasterToolsForm.openResourceFromFile(filename: String; res: TFHIRResource; format: TFHIRFormat; frameClass: TBaseResourceFrameClass);
var
  tab: TTabItem;
  frame: TBaseResourceFrame;
  fcs: IFMXCursorService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    fcs := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService
  else
    fcs := nil;
  if Assigned(fcs) then
  begin
    Cursor := fcs.GetCursor;
    fcs.SetCursor(crHourGlass);
  end;
  try
    tab := tbMain.Add(TTabItem);
    tbMain.ActiveTab := tab;
    tab.Text := extractfilename(filename);
    tab.Hint := filename;
    tab.ShowHint := true;
    frame := frameClass.Create(tab);
    tab.TagObject := frame;
    frame.TagObject := tab;
    frame.Parent := tab;
    frame.tabs := tbMain;
    frame.OnWork := dowork;
    frame.Settings := FSettings.Link;
    frame.tab := tab;
    frame.Align := TAlignLayout.Client;
    frame.filename := filename;
    frame.resource := res.Link;
    frame.format := format;
    frame.load;
    addFileToList(filename);
  finally
    if Assigned(fcs) then
      fcs.SetCursor(Cursor);
  end;
end;

procedure TMasterToolsForm.OpenResourcefromClient(Sender: TObject; Client: TFHIRClient; format: TFHIRFormat; resource: TFHIRResource);
var
  tab: TTabItem;
  frame: TBaseResourceFrame;
  fcs: IFMXCursorService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    fcs := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService
  else
    fcs := nil;
  if Assigned(fcs) then
  begin
    Cursor := fcs.GetCursor;
    fcs.SetCursor(crHourGlass);
  end;
  try
    tab := tbMain.Add(TTabItem);
    tbMain.ActiveTab := tab;
    tab.Text := resource.fhirType + '/' + resource.id;
    tab.Hint := Client.address + '/' + resource.fhirType + '/' + resource.id;
    tab.ShowHint := true;
    frame := frameForResource(resource).Create(tab);
    tab.TagObject := frame;
    frame.TagObject := tab;
    frame.Parent := tab;
    frame.tabs := tbMain;
    frame.OnWork := dowork;
    frame.Settings := FSettings.Link;
    frame.tab := tab;
    frame.Align := TAlignLayout.Client;
    frame.Client := Client.Link;
    frame.filename := '$$';
    frame.resource := resource.clone;
    frame.format := format;
    frame.load;
  finally
    if Assigned(fcs) then
      fcs.SetCursor(Cursor);
  end;
end;

procedure TMasterToolsForm.saveFiles;
var
  s: String;
begin
  try
    FSettings.storeValues('Files' + versionSettingsString, lbFiles.items);
    FSettings.save;
  except
    // nothing we can do
  end;
end;

function TMasterToolsForm.searchDesc(s: String): String;
var
  builder: TFHIRIndexBuilder;
  comps: TFHIRCompartmentList;
  index: TFHIRIndex;
  parts: TArray<string>;
begin
  if FIndexes = nil then
  begin
    FIndexes := TFhirIndexList.Create(TFHIRFactoryX.Create);
    comps := TFHIRCompartmentList.Create;
    builder := TFHIRIndexBuilderX.Create;
    try
      builder.registerIndexes(FIndexes, comps);
    finally
      builder.Free;
      comps.Free;
    end;
  end;
  result := '';
  parts := s.Split(['.']);
  index := FIndexes.getByName(parts[0], parts[1]);
  if index <> nil then
    result := '<p>' + FormatTextToHTML(index.Description) + '</p>' + '<p>' + index.name + ' : ' + CODES_TFhirSearchParamType[index.SearchType] + '</p>';
end;

procedure TMasterToolsForm.Timer1Timer(Sender: TObject);
var
  frame: TBaseFrame;
begin
  ModalHackUPdateStatus;
  frame := tbMain.ActiveTab.TagObject as TBaseFrame;
  tbnSave.enabled := false;
  tbnSaveAs.enabled := false;
  tbnClose.enabled := false;
  tbnSource.enabled := false;
  mnuResourceLanguage.enabled := false;

  if (frame <> nil) then
  begin
    tbnClose.enabled := true;
    tbnSave.enabled := frame.canSave;
    tbnSaveAs.enabled := frame.canSaveAs;
    tbnSource.enabled := frame.hasResource;
    mnuResourceLanguage.enabled := frame.hasResource;
  end;
  updateHelpText;
  GBackgroundTasks.primaryThreadCheck;
end;

function template(fragment: String): String;
begin
  result := '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">' + #13#10 + '<head>' + #13#10 + '  <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />' + #13#10 +
    ' <style>' + #13#10 + '  body { background-color: rgb(255, 254, 245);' + #13#10 + '  margin: 0px;' + #13#10 + '  padding: 0px;' + #13#10 + '  height: 100%;' + #13#10 + '  font-size: 10px;' + #13#10
    + '  font-family: verdana;' + #13#10 + '}' + #13#10 + ' </style>' + #13#10 + '</head>' + #13#10 + fragment + #13#10 + '<body>' + #13#10 + '' + #13#10 + '</body>' + #13#10 + '</html>' + #13#10;
end;

procedure TMasterToolsForm.updateHelpStatus;
begin
  if FShowHelp then
  begin
    pnlHelp.visible := FShowHelp;
    splitHelp.visible := FShowHelp;
  end
  else
  begin
    splitHelp.visible := FShowHelp;
    pnlHelp.visible := FShowHelp;
  end;

  if (pnlHelp.visible) then
    webHelp.LoadFromStrings(template(''), 'my.html');
end;

procedure TMasterToolsForm.updateHelpText;
var
  focus: IControl;
  obj: TStyledControl;
  s: String;
begin
  if FShowHelp then
  begin
    obj := nil;
    focus := Focused;
    if (focus <> nil) and (focus.GetObject is TStyledControl) then
      obj := focus.GetObject as TStyledControl;
    if (focus <> nil) and (focus.GetObject = webHelp) then
      exit;

    if obj <> FFocus then
    begin
      FFocus := obj;
      if (obj = nil) or (obj.helpContext = 0) then
        webHelp.LoadFromStrings(template(''), 'my.html')
      else
      begin
        s := Help_Strings[obj.helpContext];
        if s <> '' then
          webHelp.LoadFromStrings(template(processHelpContext(s)), 'my.html')
        else
          webHelp.LoadFromStrings(template(''), 'my.html');
      end;
    end;
  end;
end;

function TMasterToolsForm.processHelpContext(helpContext: String): String;
var
  b: TStringBuilder;
  parts: TArray<String>;
  s: String;
begin
  b := TStringBuilder.Create;
  try
    parts := helpContext.Split([',']);
    for s in parts do
      if s.StartsWith('ui:') then
        b.Append('<p>' + FormatTextToHTML(s.Substring(3).trim) + '</p>')
      else if s.StartsWith('search:') then
        b.Append(searchDesc(s.Substring(7)))
      else if s.StartsWith('fhir:') then
        fhirDefn(s.Substring(5), b)
      else
        b.Append('<p>Unknown context ' + s + '</p>');
    result := b.ToString;
  finally
    b.Free;
  end;
end;

{ TToolkitLogger }

constructor TToolkitLogger.Create;
begin
  inherited;
  if DirectoryExists('c:\temp') then
    FLog := TLogger.Create('c:\temp\toolkit.fhir.log')
  else
    FLog := TLogger.Create(IncludeTrailingPathDelimiter(SystemTemp) + 'toolkit.fhir.log')
end;

destructor TToolkitLogger.Destroy;
begin
  FLog.Free;
  inherited;
end;

procedure TToolkitLogger.logExchange(verb, url, status, requestHeaders, responseHeaders: String; request, response: TStream);
begin
  FLog.WriteToLog('=================================='#13#10);
  FLog.WriteToLog(verb + ' ' + url + ' HTTP/1.0'#13#10);
  FLog.WriteToLog(requestHeaders + #13#10);
  if request <> nil then
    FLog.WriteToLog(toChars(request) + #13#10);
  FLog.WriteToLog('----------------------------------'#13#10);
  FLog.WriteToLog(status + ' HTTP/1.0'#13#10);
  FLog.WriteToLog(responseHeaders + #13#10);
  if response <> nil then
    FLog.WriteToLog(toChars(response) + #13#10);
end;

function TToolkitLogger.toChars(s: TStream): string;
var
  b: TBytes;
begin
  s.Position := 0;
  setLength(b, s.Size);
  s.Read(b[0], s.Size);
  s.Position := 0;
  result := TEncoding.ANSI.GetString(b);
end;

{ TVersionChecker }

procedure TVersionChecker.execute;
begin
  response := TVersionCheckerOutcome.Create(checkUpgrade);
end;

function TVersionChecker.name: String;
begin
  result := 'Auto Upgrade checker';
end;

{ TVersionCheckerOutcome }

constructor TVersionCheckerOutcome.Create(ver: String);
begin
  inherited Create;
  FVer := ver;
end;

end.
