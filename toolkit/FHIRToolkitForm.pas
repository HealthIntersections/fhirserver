unit FHIRToolkitForm;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Platform,
  FMX.Layouts, FMX.ListBox, FMX.TabControl, FMX.Controls.Presentation, FMX.DialogService,
  System.ImageList, FMX.ImgList, FMX.Menus, FMX.WebBrowser,
  IdSSLOpenSSLHeaders, libeay32,
  SystemSupport, TextUtilities, Logging,
  FHIRBase, FHIRTypes, FHIRResources, FHIRClient, FHIRUtilities, FHIRIndexBase, FHIRIndexInformation, FHIRSupport,
  FHIRContext, FHIRProfileUtilities,
  SmartOnFHIRUtilities, EditRegisteredServerDialogFMX, OSXUIUtils,
  ToolkitSettings, ServerForm, CapabilityStatementEditor, BaseResourceFrame, BaseFrame, SourceViewer, ListSelector,
  ValueSetEditor, HelpContexts, ProcessForm, SettingsDialog, AboutDialog, ToolKitVersion, CodeSystemEditor, LibraryEditor,
  ToolKitUtilities, UpgradeNeededDialog, QuestionnaireEditor, RegistryForm, ProviderDirectoryForm, ResourceLanguageDialog;

type
  TToolkitLogger = class (TFHIRClientLogger)
  private
    FLog : TLogger;
    function toChars(s : TStream) : string;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure logExchange(verb, url, status, requestHeaders, responseHeaders : String; request, response : TStream); override;
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
  private
    { Private declarations }
    FSettings : TFHIRToolkitSettings;
    FShowHelp : boolean;
    FFocus : TStyledControl;
    FIndexes : TFhirIndexList;
    FContext : TBaseWorkerContext;
    FIsStopped : boolean;
    UpgradeOnClose : boolean;
    FUpgradeChecked : boolean;
    FRegistryTab : TTabItem;
    ToolkitLogger : TToolkitLogger;

    procedure saveFiles;
    procedure openResourceFromFile(filename : String; res : TFHIRResource; format : TFHIRFormat; frameClass : TBaseResourceFrameClass);
    procedure OpenResourcefromClient(sender : TObject; client : TFHIRClient; format : TFHIRFormat; resource : TFHIRResource);
    procedure newResource(rClass : TFhirResourceClass; frameClass : TBaseResourceFrameClass);
    procedure addFileToList(filename : String);
    function frameForResource(res : TFhirResource) : TBaseResourceFrameClass;
    function doSave : boolean;
    function doSaveAs : boolean;
    procedure updateHelpStatus;
    procedure updateHelpText;
    function processHelpContext(helpContext : String) : String;
    function searchDesc(s : String) : String;
    procedure fhirDefn(s : String; b : TStringBuilder);
    function GetStopped: boolean;
    procedure DoIdle(out stop : boolean);
    procedure DoOpenURL(url : String);
    procedure checkVersion(reportIfCurrent: boolean);
  public
    procedure dowork(Sender : TObject; opName : String; canCancel : boolean; proc : TWorkProc);
    procedure threadMonitorProc(sender : TFhirClient; var stop : boolean);
  end;

var
  MasterToolsForm: TMasterToolsForm;

implementation

{$R *.fmx}

procedure TMasterToolsForm.addFileToList(filename: String);
var
  i : integer;
begin
  for i := lbFiles.Count - 1 downto 0 do
    if lbFiles.items[i] = filename then
      lbFiles.Items.Delete(i);
  lbFiles.Items.Insert(0, filename);
  saveFiles;
  lbFilesClick(nil);
end;

procedure TMasterToolsForm.btnAddServerClick(Sender: TObject);
var
  form : TEditRegisteredServerForm;
begin
  form := TEditRegisteredServerForm.create(self);
  try
    form.SoftwareId := 'FHIR Toolkit';
    form.SoftwareVersion := ToolKitVersionBase+inttostr(BuildCount);
    form.Server := TRegisteredFHIRServer.Create;
    if form.ShowModal = mrOk then
    begin
      FSettings.registerServer('', form.Server);
      FSettings.ListServers('', lbServers.Items);
      lbServersClick(nil);
    end;
  finally
    form.Free;
  end;
end;

procedure TMasterToolsForm.btnConnectClick(Sender: TObject);
var
  http: TFhirHTTPClient;
  client : TFhirClient;
  tab : TTabItem;
  serverForm : TServerFrame;
  cs : TFhirCapabilityStatement;
  server : TRegisteredFHIRServer;
  smart : TSmartAppLaunchLogin;
  ok : boolean;
begin
  server := FSettings.serverInfo('', lbServers.ItemIndex);
  try
    http := TFhirHTTPClient.Create(nil, server.fhirEndpoint, false, FSettings.Timeout* 1000, FSettings.proxy);
    try
      http.username := server.username;
      http.password := server.password;
      if server.isSSL then
      begin
        http.certFile := server.SSLPublicCert;
        http.certKey := server.SSLPrivateKey;
        http.certPWord := server.SSLPassphrase;
      end;
      http.Logger := ToolkitLogger.Link;
      ok := false;
      if server.SmartAppLaunchMode <> salmNone then
      begin
        dowork(self, 'Logging in', true,
          procedure
          begin
            smart := TSmartAppLaunchLogin.Create;
            try
              smart.server := server.Link;
              smart.scopes := ['user/*.*'];
              smart.OnIdle := DoIdle;
              smart.OnOpenURL := DoOpenURL;
              smart.name := 'FHIR Toolkit';
              smart.version := '0.0.'+inttostr(BuildCount);
              ok := smart.login;
              if ok then
                http.smartToken := smart.token.link;
            finally
              smart.Free;
            end;
          end);
      end
      else
        ok := true;
      if not ok then
        exit;
      client := TFhirThreadedClient.create(http.link, threadMonitorProc);
      try
        cs := nil;
        doWork(nil, 'Connect', true,
          procedure
          begin
            cs := client.conformance(false);
          end);
        try
          if (cs <> nil) then
          begin
            tab := tbMain.Add(TTabItem);
            tab.Text := server.name;
            tbMain.ActiveTab := tab;
            serverForm := TServerFrame.create(tab);
            serverForm.Parent := tab;
            tab.TagObject := serverForm;
            serverForm.OnWork := dowork;
            serverForm.TagObject := tab;
            serverForm.tabs := tbMain;
            serverForm.Settings := FSettings.Link;
            serverForm.Tab := tab;
            serverForm.Align := TAlignLayout.Client;
            serverForm.Client := client.link;
            serverForm.CapabilityStatement := cs.link;
            serverForm.OnOpenResource := OpenResourcefromClient;
            serverForm.OnWork :=  dowork;
            serverForm.load;

            if lbServers.ItemIndex > 0 then
            begin
              lbServers.Items.Exchange(0, lbServers.ItemIndex);
              FSettings.moveServer('', lbServers.ItemIndex, -lbServers.ItemIndex);
              lbServers.ItemIndex := 0;
            end;
          end;
        finally
          cs.free;
        end;
      finally
        client.Free;
      end;
    finally
      http.Free;
    end;
  finally
    server.free;
  end;
end;

procedure TMasterToolsForm.btnCopyClick(Sender: TObject);
var
  Svc: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
    Svc.SetClipboard(lbFiles.Items[lbFiles.ItemIndex])
  else
    Beep;
end;

procedure TMasterToolsForm.btnEditServerClick(Sender: TObject);
var
  i : integer;
  form : TEditRegisteredServerForm;
begin
  form := TEditRegisteredServerForm.create(self);
  try
    form.SoftwareId := ToolkitIdentifier;
    form.SoftwareVersion := ToolKitVersionBase+inttostr(BuildCount);
    form.Server := FSettings.serverInfo('', lbServers.ItemIndex);
    if form.ShowModal = mrOk then
    begin
      FSettings.updateServerInfo('', lbServers.ItemIndex, form.Server);
      FSettings.ListServers('', lbServers.Items);
      lbServersClick(nil);
    end;
  finally
    form.Free;
  end;
end;

procedure TMasterToolsForm.btnNewClick(Sender: TObject);
var
  form : TListSelectorForm;
begin
  form := TListSelectorForm.create(nil);
  try
    form.ListBox1.ShowCheckboxes := false;
    form.ListBox1.items.Add('CapabilityStatement');
    form.ListBox1.items.Add('ValueSet');
    form.ListBox1.items.Add('CodeSystem');
    form.ListBox1.items.Add('Questionnaire');
    form.ListBox1.items.Add('Library');
    form.caption := 'Create New File';
    if (form.ShowModal = mrOk) then
      case form.ListBox1.ItemIndex of
        0 : newResource(TFhirCapabilityStatement, TCapabilityStatementEditorFrame);
        1 : newResource(TFhirValueSet, TValueSetEditorFrame);
        2 : newResource(TFhirCodeSystem, TCodeSystemEditorFrame);
        3 : newResource(TFhirQuestionnaire, TQuestionnaireEditorFrame);
        4 : newResource(TFhirLibrary, TLibraryEditorFrame);
      end;
  finally
    form.Free;
  end;
end;

procedure TMasterToolsForm.btnOpenClick(Sender: TObject);
var
  res : TFhirResource;
  format : TFHIRFormat;
begin
  if odFile.Execute then
  begin
    try
      format := ffUnspecified;
      res := fileToResource(odFile.Filename, format);
      try
        if res is TFhirCapabilityStatement then
          openResourceFromFile(odFile.Filename, res, format, TCapabilityStatementEditorFrame)
        else if res is TFhirValueSet then
          openResourceFromFile(odFile.Filename, res, format, TValueSetEditorFrame)
        else if res is TFhirCodeSystem then
          openResourceFromFile(odFile.Filename, res, format, TCodeSystemEditorFrame)
        else if res is TFhirQuestionnaire then
          openResourceFromFile(odFile.Filename, res, format, TQuestionnaireEditorFrame)
        else if res is TFhirLibrary then
          openResourceFromFile(odFile.Filename, res, format, TLibraryEditorFrame)
        else
          MessageDlg('Unsupported Resource Type: '+res.fhirType, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
      finally
        res.free;
      end;
    except
      on e : Exception do
        MessageDlg('Error reading Resource: '+e.Message, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    end;
  end;
end;

procedure TMasterToolsForm.btnRemoveFileClick(Sender: TObject);
var
  i : integer;
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
  i : integer;
begin
  i := lbServers.ItemIndex;
  FSettings.DeleteServer('', lbServers.ItemIndex);
  lbServers.items.Delete(i);
  if i = lbServers.items.Count then
    dec(i);
  lbServers.ItemIndex := i;
  lbServersClick(nil);
end;

procedure TMasterToolsForm.btnReopenClick(Sender: TObject);
var
  res : TFhirResource;
  fn : String;
  format : TFHIRFormat;
begin
  fn := lbFiles.Items[lbFiles.ItemIndex];
  try
    format := ffUnspecified;
    res := fileToResource(fn, format);
    try
      openResourceFromFile(fn, res, format, frameForResource(res));
    finally
      res.free;
    end;
  except
    on e : Exception do
      MessageDlg('Error reading Resource: '+e.Message, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TMasterToolsForm.btnSettingsClick(Sender: TObject);
var
  form : TSettingsForm;
begin
  form := TSettingsForm.create(self);
  try
    form.Settings := FSettings.link;
    form.showmodal;
  finally
    form.free;
  end;
end;

procedure TMasterToolsForm.btnStopClick(Sender: TObject);
begin
  FIsStopped := true;
end;

procedure TMasterToolsForm.tbnSaveClick(Sender: TObject);
begin
  doSave;
end;

procedure TMasterToolsForm.tbnSourceClick(Sender: TObject);
var
  form : TSourceViewerForm;
  frame : TBaseFrame;
begin
  frame := tbMain.ActiveTab.TagObject as TBaseFrame;
  if (frame <> nil) and frame.hasResource then
  begin
    form := TSourceViewerForm.create(self);
    try
      form.current := frame.currentResource.Link;
      form.original := frame.originalResource.Link;
      form.ShowModal;
    finally
      form.Free;
    end;
  end;
end;

procedure TMasterToolsForm.threadMonitorProc(sender: TFhirClient; var stop: boolean);
begin
  Application.ProcessMessages;
  stop :=  FIsStopped;
end;

procedure TMasterToolsForm.tbnSaveAsClick(Sender: TObject);
begin
  doSaveAs;
end;

procedure TMasterToolsForm.tbnCloseClick(Sender: TObject);
var
  i : integer;
  frame : TBaseFrame;
  procedure closeit;
  begin
    i := tbMain.TabIndex;
    if tbMain.ActiveTab = FRegistryTab then
      FRegistryTab := nil;
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
      case MessageDlg(tbMain.ActiveTab.Text+' contains unsaved data. Save the data first?', TMsgDlgType.mtConfirmation, mbYesNoCancel, 0) of
        mrYes :
          if doSave then
            closeit;
        mrNo : closeit;
        mrCancel : exit;
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

procedure TMasterToolsForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TMasterToolsForm.DoIdle(out stop: boolean);
begin
  Application.ProcessMessages;
  stop := GetStopped;
end;

procedure TMasterToolsForm.DoOpenURL(url: String);
begin
  openURL(url);
end;

function TMasterToolsForm.GetStopped: boolean;
begin
  result := FIsStopped;
end;


function TMasterToolsForm.doSave: boolean;
var
  frame : TBaseFrame;
  ok : boolean;
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
        procedure
        begin;
          ok := frame.save;
        end);
      result := ok;
    end
    else
      result := doSaveAs;
  end;
end;

function TMasterToolsForm.doSaveAs: boolean;
var
  frame : TBaseFrame;
  ok : boolean;
  fn, ext : String;
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
          procedure
          begin
            fn := sdFile.Filename;
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
              raise Exception.Create('Unknown format');
            addFileToList(sdFile.FileName);
          end);
        result := ok;
      end
      else
        raise Exception.create('Unable to save file');
    end;
  end;
end;

procedure TMasterToolsForm.dowork(Sender: TObject; opName : String; canCancel : boolean; proc: TWorkProc);
var
  fcs : IFMXCursorService;
  form : TProcessingForm;
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
    if assigned(sender) and (sender is TBaseFrame) then
      TBaseFrame(sender).OnStopped := GetStopped;
    form := TProcessingForm.Create(self);
    try
      form.lblOperation.text := opName;
      form.Button1.enabled := canCancel;
      form.Button1.OnClick := btnStopClick;
      form.proc := proc;
      form.ShowModal;
    finally
      form.Free;
    end;
  finally
    if Assigned(fCS) then
      fcs.setCursor(Cursor);
  end;
end;

procedure TMasterToolsForm.fhirDefn(s: String; b : TStringBuilder);
var
  n : string;
  sd : TFhirStructureDefinition;
  ed : TFhirElementDefinition;
begin
  if s.Contains('.') then
    n := s.Substring(0, s.IndexOf('.'))
  else
    n := s;
  sd := FContext.fetchResource(frtStructureDefinition, 'http://hl7.org/fhir/StructureDefinition/'+n) as TFhirStructureDefinition;
  if sd = nil then
    b.Append('<p>uUnknown path:' +s+'</p>')
  else
  begin
    for ed in sd.snapshot.elementList do
    begin
      if ed.path = s then
      begin
        b.Append('<p><b>'+s+'</b></p>');
        b.Append('<p>Definition: '+ed.definition+'</p>');
        if ed.comment <> '' then
          b.Append('<p>Comments: '+ed.comment+'</p>');
        exit;
      end;
    end;
    b.Append('<p>Unknown path:' +s+'</p>')
  end;
end;

procedure TMasterToolsForm.FormActivate(Sender: TObject);
begin
  if FContext = nil then
    doWork(nil, 'Loading Data', false,
      procedure
      begin
        FContext := TBaseWorkerContext.Create;
        FContext.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'profiles-types.xml');
        FContext.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'profiles-resources.xml');
        if not (IdSSLOpenSSLHeaders.load and LoadEAYExtensions) then
          raise Exception.Create('Unable to load openSSL - SSL/Crypto functions will fail');
      end);
  if not FUpgradeChecked and FSettings.CheckForUpgradesOnStart then
  begin
    FUpgradeChecked := true;
    checkVersion(false);
  end;
end;

procedure TMasterToolsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  dirty : boolean;
  i : integer;
  obj : TBaseFrame;
  form : TListSelectorForm;
begin
  dirty := false;
  form := TListSelectorForm.Create(self);
  try
    form.Caption := 'Unsaved Content found. Which files do you want to save?';
    form.okWithNoneSelected := true;
    form.button1.Text := 'Close';
    for i := 1 to tbMain.TabCount - 1 do
    begin
      obj := tbMain.Tabs[i].TagObject as TBaseFrame;
      if (obj.isDirty) then
      begin
        dirty := true;
        form.ListBox1.Items.AddObject(obj.nameForSaveDialog, obj)
      end;
    end;
    if not dirty then
      CanClose := true
    else
      CanClose := form.ShowModal = mrOk;
      for i := 0 to form.ListBox1.Items.Count - 1 do
        if form.ListBox1.ListItems[i].IsChecked then
          if not TBaseFrame(form.ListBox1.Items.Objects[i]).save then
          begin
            CanClose := false;
            exit;
          end;
  finally
    form.Free;
  end;
end;

procedure TMasterToolsForm.FormCreate(Sender: TObject);
begin
  FSettings := TFHIRToolkitSettings.Create(IncludeTrailingPathDelimiter(SystemTemp) + 'fhir-toolkit-settings.json');
  FSettings.ListServers('', lbServers.Items);
  lbServers.ItemIndex := 0;
  lbServersClick(self);
  FSettings.getValues('Files', lbFiles.Items);
  if lbFiles.Items.count > 0 then
    lbFiles.ItemIndex := 0;
  lbFilesClick(self);
  Left := FSettings.getValue('Window', 'left', left);
  Top := FSettings.getValue('Window', 'top', top);
  Width := FSettings.getValue('Window', 'width', width);
  Height := FSettings.getValue('Window', 'height', height);
  FShowHelp := FSettings.ShowHelp;
  updateHelpStatus;
  {$IFDEF MACOS}
  mnuFileExit.Text := '&Quit';
  {$ENDIF}
  ToolkitLogger := TToolkitLogger.create;
end;

procedure TMasterToolsForm.FormDestroy(Sender: TObject);
var
  newVersion : String;
begin
  saveFiles;
  try
    FSettings.storeValue('Window', 'left', left);
    FSettings.storeValue('Window', 'top', top);
    FSettings.storeValue('Window', 'width', width);
    FSettings.storeValue('Window', 'height', height);
    FSettings.Save;
  except
  end;
  FSettings.Free;
  FIndexes.Free;
  FContext.Free;
  ToolkitLogger.Free;
  if UpgradeOnClose then
  begin
    doWork(self, 'Checking Version', true,
    procedure
    begin
      newVersion := checkUpgrade;
    end);
    doUpgrade(newVersion);
  end;
end;

function TMasterToolsForm.frameForResource(res: TFhirResource): TBaseResourceFrameClass;
begin
  if res is TFhirCapabilityStatement then
    result := TCapabilityStatementEditorFrame
  else if res is TFhirValueSet then
    result := TValueSetEditorFrame
  else if res is TFhirCodeSystem then
    result := TCodeSystemEditorFrame
  else if res is TFhirQuestionnaire then
    result := TQuestionnaireEditorFrame
  else if res is TFhirLibrary then
    result := TLibraryEditorFrame
  else
    MessageDlg('Unsupported Resource Type: '+res.fhirType, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
end;

procedure TMasterToolsForm.lbFilesClick(Sender: TObject);
begin
  btnReopen.Enabled := lbFiles.ItemIndex >= 0;
  btnRemoveFile.Enabled := lbFiles.ItemIndex >= 0;
  btnCopy.Enabled := lbFiles.ItemIndex >= 0;
end;

procedure TMasterToolsForm.lbServersClick(Sender: TObject);
begin
  btnConnect.Enabled := lbServers.ItemIndex >= 0;
  btnRemoveServer.Enabled := lbServers.ItemIndex >= 0;
  btnEditServer.Enabled := lbServers.ItemIndex >= 0;
end;

procedure TMasterToolsForm.mnuResourceLanguageClick(Sender: TObject);
var
  ResourceLanguageForm : TResourceLanguageForm;
  frame : TBaseFrame;
begin
  frame := tbMain.ActiveTab.TagObject as TBaseFrame;
  if (frame <> nil) and frame.hasResource then
  begin
    ResourceLanguageForm := TResourceLanguageForm.Create(self);
    try
      ResourceLanguageForm.Resource := frame.currentResource.Link;
      if ResourceLanguageForm.ShowModal = mrOk then
        frame.reload;
    finally
      ResourceLanguageForm.Free;
    end;
  end;
end;

procedure TMasterToolsForm.mnuCheckVersionClick(Sender: TObject);
begin
  checkVersion(true);
end;

procedure TMasterToolsForm.checkVersion(reportIfCurrent : boolean);
var
  newVersion : String;
  upg : TUpgradeNeededForm;
begin
  doWork(self, 'Checking Version', true,
    procedure
    begin
      newVersion := checkUpgrade;
    end);
  if (newVersion <> '') and (newVersion <> '0.0.'+inttostr(buildCount)) then
  begin
    upg := TUpgradeNeededForm.Create(self);
    try
      upg.Settings := FSettings.link;
      upg.lblVersion.Text := 'The current version is '+newVersion+', you are running 0.0.'+inttostr(buildCount)+'. Upgrade?';
      case upg.ShowModal of
        mrContinue : UpgradeOnClose := true;
        mrYes:

          begin
          doUpgrade(newVersion);
          close;
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
  form : TAboutForm;
begin
  form := TAboutForm.Create(self);
  try
    form.ShowModal;
  finally
    form.Free;
  end;
end;

procedure TMasterToolsForm.mnuRegistryClick(Sender: TObject);
var
  frame : TFrame;
begin
  if FRegistryTab <> nil then
    tbMain.ActiveTab := FRegistryTab
  else
  begin
    FRegistryTab := tbMain.Add(TTabItem);
    tbMain.ActiveTab := FRegistryTab;
    FRegistryTab.Text := 'registry.fhir.org';
    frame := TRegistryFrame.create(FRegistryTab);
    FRegistryTab.TagObject := frame;
    frame.TagObject := FRegistryTab;
    frame.Parent := FRegistryTab;
    frame.tabs := tbMain;
    frame.OnWork := dowork;
    frame.OnOpenResource := OpenResourcefromClient;
    frame.Settings := FSettings.link;
    frame.Tab := FRegistryTab;
    frame.Align := TAlignLayout.Client;
    frame.load;
  end;
end;

procedure TMasterToolsForm.newResource(rClass : TFhirResourceClass; frameClass : TBaseResourceFrameClass);
var
  tab : TTabItem;
  frame : TBaseResourceFrame;
  fcs : IFMXCursorService;
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
    tab.Text := 'New '+rClass.ClassName.Substring(5);
    tab.Hint := tab.Text;
    tab.ShowHint := true;
    frame := frameClass.create(tab);
    tab.TagObject := frame;
    frame.TagObject := tab;
    frame.Parent := tab;
    frame.tabs := tbMain;
    frame.OnWork := dowork;
    frame.Settings := FSettings.link;
    frame.Tab := tab;
    frame.Align := TAlignLayout.Client;
    frame.resource := rClass.create;
    frame.load;
  finally
    if Assigned(fCS) then
      fcs.setCursor(Cursor);
  end;
end;

procedure TMasterToolsForm.openResourceFromFile(filename: String; res: TFHIRResource; format : TFHIRFormat; frameClass: TBaseResourceFrameClass);
var
  tab : TTabItem;
  frame : TBaseResourceFrame;
  fcs : IFMXCursorService;
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
    tab.Text := ExtractFileName(filename);
    tab.Hint := filename;
    tab.ShowHint := true;
    frame := frameClass.create(tab);
    tab.TagObject := frame;
    frame.TagObject := tab;
    frame.Parent := tab;
    frame.tabs := tbMain;
    frame.OnWork := dowork;
    frame.Settings := FSettings.link;
    frame.Tab := tab;
    frame.Align := TAlignLayout.Client;
    frame.Filename := filename;
    frame.resource := res.link;
    frame.format := format;
    frame.load;
    addFileToList(filename);
  finally
    if Assigned(fCS) then
      fcs.setCursor(Cursor);
  end;
end;

procedure TMasterToolsForm.OpenResourceFromClient(sender : TObject; client : TFHIRClient; format : TFHIRFormat; resource : TFHIRResource);
var
  tab : TTabItem;
  frame : TBaseResourceFrame;
  fcs : IFMXCursorService;
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
    tab.Text := resource.fhirType+'/'+resource.id;
    tab.Hint := client.address+'/'+resource.fhirType+'/'+resource.id;
    tab.ShowHint := true;
    frame := frameForResource(resource).create(tab);
    tab.TagObject := frame;
    frame.TagObject := tab;
    frame.Parent := tab;
    frame.tabs := tbMain;
    frame.OnWork := dowork;
    frame.Settings := FSettings.link;
    frame.Tab := tab;
    frame.Align := TAlignLayout.Client;
    frame.Client := client.link;
    frame.Filename := '$$';
    frame.resource := resource.clone;
    frame.format := format;
    frame.load;
  finally
    if Assigned(fCS) then
      fcs.setCursor(Cursor);
  end;
end;

procedure TMasterToolsForm.saveFiles;
var
  s : String;
begin
  try
    FSettings.storeValues('Files', lbFiles.Items);
    FSettings.save;
  except
    // nothing we can do
  end;
end;


function TMasterToolsForm.searchDesc(s: String): String;
var
  builder : TFHIRIndexBuilder;
  comps : TFHIRCompartmentList;
  index : TFHIRIndex;
  parts : TArray<string>;
begin
  if FIndexes = nil then
  begin
    FIndexes := TFhirIndexList.Create;
    comps := TFHIRCompartmentList.Create;
    builder := TFHIRIndexBuilder.Create;
    try
      builder.registerIndexes(Findexes, comps);
    finally
      builder.Free;
      comps.free;
    end;
  end;
  result := '';
  parts := s.Split(['.']);
  index := FIndexes.getByName(parts[0], parts[1]);
  if index <> nil then
    result := '<p>'+FormatTextToHTML(index.Description)+'</p>'+'<p>'+index.Name+' : '+CODES_TFhirSearchParamTypeEnum[index.SearchType]+'</p>';
end;

procedure TMasterToolsForm.Timer1Timer(Sender: TObject);
var
  frame : TBaseFrame;
begin
  frame := tbMain.ActiveTab.TagObject as TBaseFrame;
  tbnSave.Enabled := false;
  tbnSaveAs.Enabled := false;
  tbnClose.Enabled := false;
  tbnSource.Enabled := false;
  mnuResourceLanguage.Enabled := false;

  if (frame <> nil) then
  begin
    tbnClose.Enabled := true;
    tbnSave.Enabled := frame.canSave;
    tbnSaveAs.Enabled := frame.canSaveAs;
    tbnSource.Enabled := frame.hasResource;
    mnuResourceLanguage.Enabled := frame.hasResource;
  end;
  updateHelpText;
end;

function template(fragment : String) : String;
begin
result :=
'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'+#13#10+
'<head>'+#13#10+
'  <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />'+#13#10+
' <style>'+#13#10+
'  body { background-color: rgb(255, 254, 245);'+#13#10+
'	margin: 0px;'+#13#10+
'	padding: 0px;'+#13#10+
'	height: 100%;'+#13#10+
'	font-size: 10px;'+#13#10+
'	font-family: verdana;'+#13#10+
'}'+#13#10+
' </style>'+#13#10+
'</head>'+#13#10+
fragment+#13#10+
'<body>'+#13#10+
''+#13#10+
'</body>'+#13#10+
'</html>'+#13#10;
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
  focus : IControl;
  obj : TStyledControl;
  s : String;
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
      if (obj = nil) or (obj.HelpContext = 0) then
        webHelp.LoadFromStrings(template(''), 'my.html')
      else
      begin
        s := Help_Strings[obj.HelpContext];
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
  b : TStringBuilder;
  parts : TArray<String>;
  s : String;
begin
  b := TStringBuilder.Create;
  try
    parts := helpContext.Split([',']);
    for s in parts do
      if s.StartsWith('ui:') then
        b.Append('<p>'+FormatTextToHTML(s.Substring(3).trim)+'</p>')
      else if s.StartsWith('search:') then
        b.Append(searchDesc(s.Substring(7)))
      else if s.StartsWith('fhir:') then
        fhirDefn(s.Substring(5), b)
      else
        b.Append('<p>Unknown context '+s+'</p>');
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
    FLog := TLogger.Create(IncludeTrailingPathDelimiter(SystemTemp)+ 'toolkit.fhir.log')
end;

destructor TToolkitLogger.Destroy;
begin
  FLog.Free;
  inherited;
end;

procedure TToolkitLogger.logExchange(verb, url, status, requestHeaders, responseHeaders : String; request, response : TStream);
begin
  FLog.WriteToLog('=================================='#13#10);
  FLog.WriteToLog(verb+' '+url+' HTTP/1.0'#13#10);
  FLog.WriteToLog(requestHeaders+#13#10);
  if request <> nil then
    Flog.WriteToLog(toChars(request)+#13#10);
  FLog.WriteToLog('----------------------------------'#13#10);
  FLog.WriteToLog(status+' HTTP/1.0'#13#10);
  FLog.WriteToLog(responseHeaders+#13#10);
  if response <> nil then
    Flog.WriteToLog(toChars(response)+#13#10);
end;

function TToolkitLogger.toChars(s: TStream): string;
var
  b : TBytes;
begin
  s.Position := 0;
  setLength(b, s.Size);
  s.Read(b[0], s.Size);
  s.Position := 0;
  result := TEncoding.ANSI.GetString(b);
end;

end.


