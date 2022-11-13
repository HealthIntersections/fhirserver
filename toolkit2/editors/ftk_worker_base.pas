unit ftk_worker_base;

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
    procedure updateSettings;  virtual;
    procedure inspect; virtual;
  public
    destructor Destroy; override;
    property Context : TToolkitContext read FContext;
    procedure getFocus; virtual;
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
    procedure bindToTab(tab : TTabSheet; pnl : TPanel); override;
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
    procedure updateSettings; override;
    function getSource : String; override;
    procedure resizeControls; override;
    procedure saveStatus; override;
    function FileExtension : String; override;
    function pollForInspector : boolean; override;
    procedure inspect; override;
  end;

implementation

{ TBaseWorkerFrame }

destructor TBaseWorkerFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TBaseWorkerFrame.getFocus;
begin
  // nothing
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

procedure TBaseWorkerFrame.updateSettings;
begin
  // nothing
end;

procedure TBaseWorkerFrame.inspect;
begin

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

procedure TBaseWorker.bindToTab(tab: TTabSheet; pnl : TPanel);
begin
  inherited bindToTab(tab, pnl);
  FFrame := makeFrame(pnl);
  FFrame.parent := pnl;
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
  FFrame.getFocus;
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
  result := false;
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

procedure TBaseWorker.updateSettings;
begin
  FFrame.updateSettings;
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

function TBaseWorker.FileExtension: String;
begin
  result := 'bin';
end;

function TBaseWorker.pollForInspector: boolean;
begin
  Result := true;
end;

procedure TBaseWorker.inspect;
begin
  inherited inspect;
  FFrame.inspect;
end;

end.


