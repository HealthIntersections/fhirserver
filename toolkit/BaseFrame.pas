unit BaseFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox, FMX.Edit, FMX.TabControl, FMX.TreeView, FMX.Layouts,
  FMX.Controls.Presentation, FMX.Platform,
  IdComponent,
  IniFiles,
  FHIRBase, FHIRResources, FHIRClient;

type
  TOnOpenResourceEvent = procedure (sender : TObject; client : TFHIRClient; format : TFHIRFormat; resource : TFHIRResource) of object;

  TIsStoppedFunction = reference to function : boolean;
  TWorkProc = reference to procedure(isStopped : TIsStoppedFunction);
  TWorkEvent = procedure (Sender : TObject; proc : TWorkProc) of object;

  TBaseFrame = class(TFrame)
  private
    FTabs : TTabControl;
    FTab  : TTabItem;
    FIni: TIniFile;
    FOnOpenResource : TOnOpenResourceEvent;
    FOnWork : TWorkEvent;
    FStopped: TIsStoppedFunction;
    function StoppedStub : boolean;
  public
    property Tabs : TTabControl read FTabs write FTabs;
    property Tab : TTabItem read FTab write FTab;
    property Ini : TIniFile read FIni write FIni;
    property OnOpenResource : TOnOpenResourceEvent read FOnOpenResource write FOnOpenResource;
    property OnWork : TWorkEvent read FOnWork write FOnWork;
    property Stopped : TIsStoppedFunction read FStopped write FStopped;

    procedure load; virtual;
    procedure Close;

    function canSave : boolean; virtual;
    function canSaveAs : boolean; virtual;
    function isDirty : boolean; virtual;
    function nameForSaveDialog : String; virtual;
    function save : boolean; virtual;
    function saveAs(filename : String; format : TFHIRFormat) : boolean; virtual;
    function hasResource : boolean; virtual;
    function currentResource : TFHIRResource; virtual;
    function originalResource : TFHIRResource; virtual;

    procedure ClientWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure work(proc : TWorkProc);
  end;

implementation

{ TBaseFrame }

function TBaseFrame.canSave: boolean;
begin
  result := false;
end;

function TBaseFrame.canSaveAs: boolean;
begin
  result := false;
end;

procedure TBaseFrame.ClientWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  if Stopped then
    abort;
end;

procedure TBaseFrame.Close;
var
  i : integer;
begin
  i := tabs.TabIndex;
  tab.Free;
  if i > 0 then
    tabs.TabIndex := i - 1
  else
    tabs.TabIndex := 0;
end;

function TBaseFrame.currentResource: TFHIRResource;
begin
  result := nil;
end;

function TBaseFrame.hasResource: boolean;
begin
  result := false;
end;

function TBaseFrame.isDirty: boolean;
begin
  result := false;
end;

procedure TBaseFrame.load;
begin

end;


function TBaseFrame.nameForSaveDialog: String;
begin
  result := '';
end;

function TBaseFrame.originalResource: TFHIRResource;
begin
  result := nil;
end;

function TBaseFrame.save : boolean;
begin
  raise Exception.Create('Not implemented');
end;

function TBaseFrame.saveAs(filename: String; format: TFHIRFormat): boolean;
begin
  raise Exception.Create('Not implemented');
end;

function TBaseFrame.StoppedStub: boolean;
begin
  result := false;
end;

procedure TBaseFrame.work(proc: TWorkProc);
begin
  OnWork(self, proc);
  FStopped := StoppedStub;
end;

end.
