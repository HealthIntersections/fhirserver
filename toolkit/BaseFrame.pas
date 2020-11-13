unit BaseFrame;

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
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox, FMX.Edit, FMX.TabControl, FMX.TreeView, FMX.Layouts,
  FMX.Controls.Presentation, FMX.Platform,
  IdComponent,
  fsl_base,
  fhir_objects, 
  FHIR.Version.Resources, FHIR.Version.Resources.Base, FHIR.Version.Client,
  ToolkitSettings;

type
  TWorkProc = reference to procedure (context : pointer);
  TWorkEvent = procedure (Sender : TObject; opName : String; canCancel : boolean; proc : TWorkProc) of object;
  TOnOpenResourceEvent = procedure (sender : TObject; client : TFHIRClient; format : TFHIRFormat; resource : TFHIRResource) of object;
  TIsStoppedEvent = reference to function (context : pointer): boolean;

  TUTGRepositoryBase = class abstract (TFslObject)
  private
  public
    procedure update(res : TFHIRResource); virtual; abstract;
  end;

  TBaseFrame = class(TFrame)
  private
    FTabs : TTabControl;
    FTab  : TTabItem;
    FSettings: TFHIRToolkitSettings;
    FOnOpenResource : TOnOpenResourceEvent;
    FOnWork : TWorkEvent;
    FOnStopped: TIsStoppedEvent;
    FForm : TForm;
    FUTGRepo: TUTGRepositoryBase;
    procedure SetSettings(const Value: TFHIRToolkitSettings);
    procedure SetUTGRepo(const Value: TUTGRepositoryBase);
  public
    destructor Destroy; override;

    property Form : TForm read FForm write FForm;
    property Tabs : TTabControl read FTabs write FTabs;
    property Tab : TTabItem read FTab write FTab;
    property Settings : TFHIRToolkitSettings read FSettings write SetSettings;
    property OnOpenResource : TOnOpenResourceEvent read FOnOpenResource write FOnOpenResource;
    property OnWork : TWorkEvent read FOnWork write FOnWork;
    property OnStopped : TIsStoppedEvent read FOnStopped write FOnStopped;
    property UTGRepo : TUTGRepositoryBase read FUTGRepo write SetUTGRepo;


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
    procedure reload; virtual;
    procedure SettingsChanged; virtual;
    procedure organise; virtual;
    function canOrganise : boolean; virtual;
    procedure ClientWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure work(opName : String; canCancel : boolean; proc : TWorkProc);

    function canOpenResourceType(t : TFHIRResourceType) : boolean;
  end;

implementation

{ TBaseFrame }

function TBaseFrame.canOpenResourceType(t: TFHIRResourceType): boolean;
begin
  if t = frtCapabilityStatement then
    result := true
  else if t = frtValueSet then
    result := true
  else if t = frtCodeSystem then
    result := true
  else if t = frtQuestionnaire then
    result := true
  else if t = frtLibrary then
    result := true
{$IFDEF EXAMPLESCENARIO}
  else if t = frtExampleScenario then
    result := true
{$ENDIF}
{$IFDEF IMPLEMENTATIONGUIDE}
  else if t = frtImplementationGuide then
    result := true
{$ENDIF}
  else
    result := false;
end;

function TBaseFrame.canOrganise: boolean;
begin
  result := false;
end;

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
  if assigned(OnStopped) and OnStopped(nil) then
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

destructor TBaseFrame.Destroy;
begin
  FUTGRepo.Free;
  FSettings.Free;
  inherited;
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

procedure TBaseFrame.organise;
begin
  // nothing
end;

function TBaseFrame.originalResource: TFHIRResource;
begin
  result := nil;
end;

procedure TBaseFrame.reload;
begin
  load;
end;

function TBaseFrame.save : boolean;
begin
  raise EFHIRException.create('Not implemented');
end;

function TBaseFrame.saveAs(filename: String; format: TFHIRFormat): boolean;
begin
  raise EFHIRException.create('Not implemented');
end;

procedure TBaseFrame.SetSettings(const Value: TFHIRToolkitSettings);
begin
  FSettings.Free;
  FSettings := Value;
end;

procedure TBaseFrame.SettingsChanged;
begin
  // nothing
end;

procedure TBaseFrame.SetUTGRepo(const Value: TUTGRepositoryBase);
begin
  FUTGRepo.Free;
  FUTGRepo := Value;
end;

procedure TBaseFrame.work(opName : String; canCancel : boolean; proc: TWorkProc);
begin
  OnWork(self, opname, canCancel, proc);
end;

end.
