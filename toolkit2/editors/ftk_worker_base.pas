unit ftk_worker_base;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Math, Forms,
  Graphics, Controls, ExtCtrls, ComCtrls, Menus,
  fsl_base, fsl_utilities, fsl_stream, fsl_fpc, fsl_logging, fsl_json,
  ftk_context, ftk_store;

{
this is the base class for a series of 'editors' that don't edit files
directly - they provide views with context e.g. open server. There's no editing
the source directly, and they may not be able to be saved as files directly

The UI comes from a frame. The state is stored as json source
}

type
  TBaseWorker = class;

  { TBaseWorkerFrame }

  TBaseWorkerFrame = class abstract (TFrame)
  protected
    FContext : TToolkitContext;
    FWorker : TBaseWorker;
    procedure save(json : TJsonObject); virtual; abstract;
    procedure init(json : TJsonObject); virtual; abstract;
    procedure finalise(); virtual;
    procedure changed;
  public
    destructor Destroy; override;
    property Context : TToolkitContext read FContext;
    procedure saveStatus; virtual; // called before shut down because shut down order isn't always predictable
  end;

  { TBaseWorker }

  TBaseWorker = class abstract (TToolkitEditor)
  protected
    FFrame : TBaseWorkerFrame;
    function makeFrame(owner : TComponent) : TBaseWorkerFrame; virtual; abstract;
  public
    constructor Create(context : TToolkitContext; session : TToolkitEditSession; store : TStorageService); override;
    destructor Destroy; override;

    function GetCanBeSaved : boolean; override;
    procedure newContent(); override;
    function GetBytes: TBytes; override;
    procedure LoadBytes(bytes: TBytes); override;
    procedure bindToTab(tab : TTabSheet); override;
    procedure locate(location : TSourceLocation); override;
    function location : String; override;
    procedure redo; override;
    procedure updateToolbarButtons; virtual;
    procedure getFocus(content : TMenuItem); override;
    procedure loseFocus(); override;
    procedure EditPause; override;
    procedure MovePause; override;
    procedure ChangeSideBySideMode; override;
    function hasTextTab : boolean; override;
    function hasDesigner : boolean; override;
    function IsShowingDesigner : boolean; override;
    procedure showDesigner; override;
    procedure showTextTab; override;
    procedure BeginEndSelect; override;
    procedure updateFont; override;
    function getSource : String; override;
    procedure resizeControls; override;
    procedure saveStatus; override;
  end;

implementation

{ TBaseWorkerFrame }

destructor TBaseWorkerFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TBaseWorkerFrame.saveStatus;
begin
  FContext.Free;
  FContext := nil;
end;

procedure TBaseWorkerFrame.finalise();
begin
  // nothing
end;

procedure TBaseWorkerFrame.changed;
begin
  FWorker.Session.NeedsSaving := true;
  FWorker.lastChange := GetTickCount64;
  FWorker.lastChangeChecked := false;
end;

{ TBaseWorker }

constructor TBaseWorker.Create(context: TToolkitContext; session: TToolkitEditSession; store: TStorageService);
begin
  inherited Create(context, session, store);
end;

destructor TBaseWorker.Destroy;
begin
  inherited Destroy;
end;

function TBaseWorker.GetCanBeSaved: boolean;
begin
  result := false;
end;

procedure TBaseWorker.newContent();
var
  json : TJsonObject;
begin
  json := TJsonObject.create;
  try
     FFrame.init(json);
  finally
    json.free;
  end;
end;

function TBaseWorker.GetBytes: TBytes;
var
  json : TJsonObject;
begin
  json := TJsonObject.create;
  try
    FFrame.save(json);
    result := TJsonWriter.writeObject(json, true);
  finally
    json.Free;
  end;
end;

procedure TBaseWorker.LoadBytes(bytes: TBytes);
var
  json : TJsonObject;
begin
  json := TJSONParser.Parse(bytes);
  try
    FFrame.init(json);
  finally
    json.free;
  end;
end;

procedure TBaseWorker.bindToTab(tab: TTabSheet);
begin
  inherited bindToTab(tab);
  FFrame := makeFrame(tab);
  FFrame.parent := tab;
  FFrame.Align := alClient;
  FFrame.FContext := Context.link;
  FFrame.FWorker := self;
end;

procedure TBaseWorker.locate(location: TSourceLocation);
begin

end;

function TBaseWorker.location: String;
begin

end;

procedure TBaseWorker.redo;
begin

end;

procedure TBaseWorker.updateToolbarButtons;
begin

end;

procedure TBaseWorker.getFocus(content: TMenuItem);
begin

end;

procedure TBaseWorker.loseFocus();
begin

end;

procedure TBaseWorker.EditPause;
begin

end;

procedure TBaseWorker.MovePause;
begin

end;

procedure TBaseWorker.ChangeSideBySideMode;
begin

end;

function TBaseWorker.hasTextTab: boolean;
begin
  result := false;
end;

function TBaseWorker.hasDesigner: boolean;
begin

end;

function TBaseWorker.IsShowingDesigner: boolean;
begin
  result := true;
end;

procedure TBaseWorker.showDesigner;
begin

end;

procedure TBaseWorker.showTextTab;
begin
  abort;
end;

procedure TBaseWorker.BeginEndSelect;
begin
  abort;
end;

procedure TBaseWorker.updateFont;
begin
  abort;
end;

function TBaseWorker.getSource: String;
begin

end;

procedure TBaseWorker.resizeControls;
begin

end;

procedure TBaseWorker.saveStatus;
begin
  inherited saveStatus;
  FFrame.saveStatus;
end;

end.


