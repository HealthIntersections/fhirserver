unit FHIR.Ui.ColorSB;

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

interface

uses
  SysUtils, Classes, Messages, Graphics, Types, Controls, Buttons;

uses Messages, Windows, SysUtils, CommCtrl, Classes, Controls, StdCtrls, ExtCtrls;

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StrUtils, ExtCtrls;
  
type
  TColorSpeedButton = class(TSpeedButton)
  private
    FColor: TColor;
    procedure SetColor(const Value: TColor);
  protected
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  public
    constructor Create(owner : TComponent); override;
  published
    property Color : TColor read FColor write SetColor;
  end;


type
  TLookAheadEdit = class(StdCtrls.TComboBox)
  private
    FStoredItems: TStringList;
    procedure FilterItems;
    procedure StoredItemsChange(Sender: TObject);
    procedure SetStoredItems(const Value: TStringList);
    procedure CNCommand(var AMessage: TWMCommand); message CN_COMMAND;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property StoredItems: TStringList read FStoredItems write SetStoredItems;
  end;


Procedure Register;


  const
  PBS_MARQUEE = $08;
  PBM_SETMARQUEE = (WM_USER+10);

type

{ TProgressBar }

  TDProgressRange = Integer; // for backward compatibility

  TDProgressBarOrientation = (pbHorizontal, pbVertical);

  TDProgressBar = class(TWinControl)
  private
    F32BitMode: Boolean;
    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;
    FStep: Integer;
    FOrientation: TDProgressBarOrientation;
    FSmooth: Boolean;
    FAnimating: Boolean;
    FTimer: TTimer;
    FInterval: Cardinal;
    function GetMin: Integer;
    function GetMax: Integer;
    function GetPosition: Integer;
    procedure SetParams(AMin, AMax: Integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetStep(Value: Integer);
    procedure SetOrientation(Value: TDProgressBarOrientation);
    procedure SetSmooth(Value: Boolean);
//    procedure SetMarquee(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetIsAnimating(Value: Boolean);
    procedure DoAnimate(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure StepIt;
    procedure StepBy(Delta: Integer);
  published
    property Align;
    property Animate: Boolean read FAnimating write SetIsAnimating default False;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Hint;
    property Min: Integer read GetMin write SetMin;
    property Max: Integer read GetMax write SetMax;
    property Orientation: TDProgressBarOrientation read FOrientation
      write SetOrientation default pbHorizontal;
    property ParentShowHint;
    property PopupMenu;
    property Position: Integer read GetPosition write SetPosition default 0;
    property Smooth: Boolean read FSmooth write SetSmooth default False;
    property Step: Integer read FStep write SetStep default 10;
    property Speed: Cardinal read FInterval write SetInterval default 500;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;


function InitCommonControl(CC: Integer): Boolean;
procedure CheckCommonControl(CC: Integer);

const
  ComCtlVersionIE3 = $00040046;
  ComCtlVersionIE4 = $00040047;
  ComCtlVersionIE401 = $00040048;

function GetComCtlVersion: Integer;

procedure Register;


implementation

Procedure Register;
Begin
  RegisterComponents('Misc', [TColorSpeedButton]);
End;



{ TColorSpeedButton }

constructor TColorSpeedButton.Create(owner: TComponent);
begin
  inherited;
  Color := clBtnFace;
end;

procedure TColorSpeedButton.SetColor(const Value: TColor);
begin
  FColor := Value;
  Invalidate;
end;

procedure TColorSpeedButton.WMPaint(var Message: TWMPaint);
begin
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(Rect(2, 2, Width-2, Height-2));
  inherited;
end;


procedure Register;
begin
  RegisterComponents('Mine', [TLookAheadEdit]);
end;

constructor TLookAheadEdit.Create(AOwner: TComponent);
begin
  inherited;
  AutoComplete := False;
  FStoredItems := TStringList.Create;
  FStoredItems.OnChange := StoredItemsChange;
end;

destructor TLookAheadEdit.Destroy;
begin
  FStoredItems.Free;
  inherited;
end;

procedure TLookAheadEdit.CNCommand(var AMessage: TWMCommand);
begin
  inherited;
  if AMessage.NotifyCode = CBN_EDITUPDATE then
    FilterItems;
end;

procedure TLookAheadEdit.FilterItems;
type
  TSelection = record
    StartPos, EndPos: Integer;
  end;
var
  I: Integer;
  Selection: TSelection;
  xText: string;
begin
  // store the current combo edit selection
  SendMessage(Handle, CB_GETEDITSEL, WPARAM(@Selection.StartPos), LPARAM(@Selection.EndPos));

  // begin with the items update
  Items.BeginUpdate;
  try
    if Text <> '' then
    begin
      // clear all items
      Items.Clear;
      for I := 0 to FStoredItems.Count - 1 do
        if ContainsText(FStoredItems[I], Text) then
//        if Pos( lowercase(Text), lowercase(FStoredItems[I]))>0 then begin
          Items.Add(FStoredItems[I]);
    end
    else
    begin
      // else the combo edit is empty
      // so then we'll use all what we have in the FStoredItems
      Items.Assign(FStoredItems)
    end;
  finally
    // finish the items update
    Items.EndUpdate;
  end;

  // restore the last combo edit selection
  xText := Text;
  SendMessage(Handle, CB_SHOWDROPDOWN, Integer(True), 0);
  SendMessage(Handle, WM_SETCURSOR, 0, 0); // work around for cursor display bug
  if (Items<>nil) and (Items.Count>0) then begin
    ItemIndex := 0;
  end else begin
    ItemIndex := -1;
  end;
  Text := xText;
  SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(Selection.StartPos, Selection.EndPos));
end;

procedure TLookAheadEdit.StoredItemsChange(Sender: TObject);
begin
//  if Assigned(FStoredItems) then
//    FilterItems;
end;

procedure TLookAheadEdit.SetStoredItems(const Value: TStringList);
begin
  if Assigned(FStoredItems) then
    FStoredItems.Assign(Value)
  else
    FStoredItems := Value;
end;



{$R resources\FHIR.Ui.Progress.res}

uses Consts, ComStrs;

const
  SectionSizeArea = 8;
  ShellDllName = 'shell32.dll';
  ComCtlDllName = 'comctl32.dll';

var
  ShellModule: THandle;
  ComCtlVersion: Integer;

function InitCommonControl(CC: Integer): Boolean;
var
  ICC: TInitCommonControlsEx;
begin
  ICC.dwSize := SizeOf(TInitCommonControlsEx);
  ICC.dwICC := CC;
  Result := InitCommonControlsEx(ICC);
  if not Result then InitCommonControls;
end;

procedure CheckCommonControl(CC: Integer);
begin
  if not InitCommonControl(CC) then
    raise EComponentError.Create(SInvalidComCtl32);
end;

function GetShellModule: THandle;
var
  OldError: Longint;
begin
  if ShellModule = 0 then
  begin
    OldError := SetErrorMode(SEM_NOOPENFILEERRORBOX);
    ShellModule := GetModuleHandle(ShellDllName);
    if ShellModule < HINSTANCE_ERROR then
      ShellModule := LoadLibrary(ShellDllName);
    if (ShellModule > 0) and (ShellModule < HINSTANCE_ERROR) then
      ShellModule := 0;
    SetErrorMode(OldError);
  end;
  Result := ShellModule;
end;

function GetComCtlVersion: Integer;
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  if ComCtlVersion = 0 then
  begin
    FileName := ComCtlDllName;
    InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
    if InfoSize <> 0 then
    begin
      GetMem(VerBuf, InfoSize);
      try
        if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
          if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
            ComCtlVersion := FI.dwFileVersionMS;
      finally
        FreeMem(VerBuf);
      end;
    end;
  end;
  Result := ComCtlVersion;
end;

procedure SetComCtlStyle(Ctl: TWinControl; Value: Integer; UseStyle: Boolean);
var
  Style: Integer;
begin
  if Ctl.HandleAllocated then
  begin
    Style := GetWindowLong(Ctl.Handle, GWL_STYLE);
    if not UseStyle then Style := Style and not Value
    else Style := Style or Value;
    SetWindowLong(Ctl.Handle, GWL_STYLE, Style);
  end;
end;

{ TProgressBar }

const
  Limit16 = 65535;

procedure Register;
begin
  RegisterComponents('Dreamsoft', [TDProgressBar]);
end;

procedure ProgressLimitError;
begin
  raise ELibraryException.createFmt(SOutOfRange, [0, Limit16]);
end;

constructor TDProgressBar.Create(AOwner: TComponent);
begin
  F32BitMode := InitCommonControl(ICC_PROGRESS_CLASS);
  inherited Create(AOwner);
  Width := 150;
  Height := GetSystemMetrics(SM_CYVSCROLL);
  FMin := 0;
  FMax := 100;
  FStep := 100;
  FInterval := 50;
  FAnimating := false;
  FOrientation := pbHorizontal;
end;

procedure TDProgressBar.CreateParams(var Params: TCreateParams);
begin
  if not F32BitMode then InitCommonControls;
  inherited CreateParams(Params);
  CreateSubClass(Params, PROGRESS_CLASS);
  with Params do
  begin
 //   if FOrientation = pbVertical then Style := Style or PBS_VERTICAL;
 //   if FSmooth then Style := Style or PBS_SMOOTH;
  end;
  SetPosition(FPosition);
  Params.Style := Params.Style or PBS_MARQUEE;
end;

procedure TDProgressBar.CreateWnd;
begin
  inherited CreateWnd;
  if F32BitMode then SendMessage(Handle, PBM_SETRANGE32, FMin, FMax)
  else SendMessage(Handle, PBM_SETRANGE, 0, MakeLong(FMin, FMax));
  SendMessage(Handle, PBM_SETSTEP, FStep, 0);
  Position := FPosition;
end;

procedure TDProgressBar.DoAnimate(Sender: TObject);
begin
Position := 100;
end;

function TDProgressBar.GetMin: Integer;
begin
  if HandleAllocated and F32BitMode then
    Result := SendMessage(Handle, PBM_GetRange, 1, 0)
  else
    Result := FMin;
end;

function TDProgressBar.GetMax: Integer;
begin
  if HandleAllocated and F32BitMode then
    Result := SendMessage(Handle, PBM_GetRange, 0, 0)
  else
    Result := FMax;
end;

function TDProgressBar.GetPosition: Integer;
begin
  if HandleAllocated then
  begin
    if F32BitMode then Result := SendMessage(Handle, PBM_GETPOS, 0, 0)
    else Result := SendMessage(Handle, PBM_DELTAPOS, 0, 0)
  end
  else Result := FPosition;
end;

procedure TDProgressBar.SetParams(AMin, AMax: Integer);
begin
  if AMax < AMin then
    raise EInvalidOperation.CreateFmt(SPropertyOutOfRange, [Self.Classname]);
  if not F32BitMode and ((AMin < 0) or (AMin > Limit16) or (AMax < 0) or
    (AMax > Limit16)) then ProgressLimitError;
  if (FMin <> AMin) or (FMax <> AMax) then
  begin
    if HandleAllocated then
    begin
      if F32BitMode then SendMessage(Handle, PBM_SETRANGE32, AMin, AMax)
      else SendMessage(Handle, PBM_SETRANGE, 0, MakeLong(AMin, AMax));
      if FMin > AMin then // since Windows sets Position when increase Min..
        SendMessage(Handle, PBM_SETPOS, AMin, 0); // set it back if decrease
    end;
    FMin := AMin;
    FMax := AMax;
  end;
end;

procedure TDProgressBar.SetMin(Value: Integer);
begin
  SetParams(Value, FMax);
end;

procedure TDProgressBar.SetMax(Value: Integer);
begin
  SetParams(FMin, Value);
end;

procedure TDProgressBar.SetPosition(Value: Integer);
begin
  if not F32BitMode and ((Value < 0) or (Value > Limit16)) then
    ProgressLimitError;
  if HandleAllocated then SendMessage(Handle, PBM_SETPOS, Value, 0)
  else FPosition := Value;
end;

procedure TDProgressBar.SetStep(Value: Integer);
begin
  if Value <> FStep then
  begin
    FStep := Value;
    if HandleAllocated then
      SendMessage(Handle, PBM_SETSTEP, FStep, 0);
  end;
end;

procedure TDProgressBar.StepIt;
begin
  if HandleAllocated then
    SendMessage(Handle, PBM_STEPIT, 0, 0);
end;

procedure TDProgressBar.StepBy(Delta: Integer);
begin
  if HandleAllocated then
    SendMessage(Handle, PBM_DELTAPOS, Delta, 0);
end;

procedure TDProgressBar.SetOrientation(Value: TDProgressBarOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    RecreateWnd;
  end;
end;

procedure TDProgressBar.SetSmooth(Value: Boolean);
begin
  if FSmooth <> Value then
  begin
    FSmooth := Value;
    RecreateWnd;
  end;
end;

//procedure TDProgressBar.SetMarquee(Value: Boolean);
//begin
//if Value = true then
//SendMessage(Handle, PBS_SETMARQUEE, 1, 200) else SendMessage(Handle, PBS_SETMARQUEE, 0, 200);
//end;

procedure TDProgressBar.SetInterval(Value: Cardinal);
begin
FInterval := Value;
if FAnimating then FTimer.Interval := Value;
end;

procedure TDProgressBar.SetIsAnimating(Value: Boolean);
begin
  if Value <> FAnimating then
  begin
    if Value then
    begin
      FTimer := TTimer.Create(Self);
      FTimer.Interval := FInterval;
      FTimer.OnTimer := DoAnimate;
      FTimer.Enabled := True;
    end
    else
    begin
      FTimer.Free;
    end;
    FAnimating := Value;
  end;
end;

initialization

finalization
  if ShellModule <> 0 then FreeLibrary(ShellModule);

end.
