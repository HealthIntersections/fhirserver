unit DropTarget;

// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite
// Component Names: TDropFileTarget, TDropTextTarget
// Module:          DropTarget
// Description:     Implements Dragging & Dropping of text and files
//                  INTO your application FROM another.
// Version:         3.7c
// Date:            19-APR-2005
// Target:          Win32, Delphi 3 - Delphi 5, C++ Builder 3, C++ Builder 4
// Authors:         Angus Johnson & Anders Melander
// Copyright        © 1997-2005 Angus Johnson & Anders Melander
// -----------------------------------------------------------------------------

interface

uses
  DropSource,
  Windows, ActiveX, Classes, Controls, CommCtrl, ExtCtrls,
  fsl_base;

{$include DragDrop.inc}

type

  TScrollDirection = (sdHorizontal, sdVertical);
  TScrollDirections = set of TScrollDirection;
  
  TDropTargetEvent = procedure(Sender: TObject;
    ShiftState: TShiftState; Point: TPoint; var Effect: Longint) of Object;

  //Note: TInterfacedComponent is declared in DropSource.pas
  TDropTarget = class(TInterfacedComponent, IDropTarget)
  private
    fDataObj: IDataObject;
    fDragTypes: TDragTypes;
    fTarget: TWinControl;
    fTargetList: TList;
    fGetDataOnEnter: boolean;
    fOnEnter: TDropTargetEvent;
    fOnDragOver: TDropTargetEvent;
    fOnLeave: TNotifyEvent;
    fOnDrop: TDropTargetEvent;
    fGetDropEffectEvent: TDropTargetEvent;

    fImages: TImageList;
    fDragImageHandle: HImageList;
    fShowImage: boolean;
    fImageHotSpot: TPoint;
    fLastPoint: TPoint; //Point where DragImage was last painted (used internally)

    fTargetScrollMargin: integer;
    fScrollDirs: TScrollDirections; //enables auto scrolling of target window during drags
    fScrollTimer: TTimer;   //and paints any drag image 'cleanly'.
    function GetTarget(pt: TPoint): TWinControl;
    procedure DoTargetScroll(Sender: TObject);
    procedure SetShowImage(Show: boolean);
  protected
    // IDropTarget methods...
    function DragEnter(const DataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HRESULT; StdCall;
    function DragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HRESULT; StdCall;
    function DragLeave: HRESULT; StdCall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HRESULT; StdCall;

    procedure DoEnter(ShiftState: TShiftState; Point: TPoint; var Effect: Longint); virtual;
    procedure DoDragOver(ShiftState: TShiftState; Point: TPoint; var Effect: Longint); virtual;
    procedure DoDrop(ShiftState: TShiftState; Point: TPoint; var Effect: Longint); virtual;
    procedure DoLeave; virtual;

    function DoGetData: boolean; Virtual; Abstract;
    procedure ClearData; Virtual; Abstract;
    function HasValidFormats: boolean; Virtual; Abstract;
    function GetValidDropEffect(ShiftState: TShiftState;
      pt: TPoint; dwEffect: LongInt): LongInt; Virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //Multiple targets can now be assigned!!!
    procedure Register(Target: TWinControl);
    procedure UnregisterAll; //Unregisters all targets
    procedure Unregister(Target: TWinControl);
    function PasteFromClipboard: longint; Virtual;
    property DataObject: IDataObject read fDataObj;
    property Target: TWinControl read fTarget;
  published
    property Dragtypes: TDragTypes read fDragTypes write fDragTypes;
    property GetDataOnEnter: Boolean read fGetDataOnEnter write fGetDataOnEnter;
    //Events...
    property OnEnter: TDropTargetEvent read fOnEnter write fOnEnter;
    property OnDragOver: TDropTargetEvent read fOnDragOver write fOnDragOver;
    property OnLeave: TNotifyEvent read fOnLeave write fOnLeave;
    property OnDrop: TDropTargetEvent read fOnDrop write fOnDrop;
    property OnGetDropEffect: TDropTargetEvent
      read fGetDropEffectEvent write fGetDropEffectEvent;
    //Drag Images...
    property ShowImage: boolean read fShowImage write SetShowImage;
  end;

  TDropFileTarget = class(TDropTarget)
  private
    fFiles: TStrings;
    fMappedNames: TStrings;
    fFileNameMapFormatEtc,
    fFileNameMapWFormatEtc: TFormatEtc;
  protected
    procedure ClearData; override;
    function DoGetData: boolean; override;
    function HasValidFormats: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PasteFromClipboard: longint; Override;
    property Files: TStrings Read fFiles;
    //MappedNames is only needed if files need to be renamed after a drag op
    //eg dragging from 'Recycle Bin'.
    property MappedNames: TStrings read fMappedNames;
  end;

  TDropTextTarget = class(TDropTarget)
  private
    fText: String;
  protected
    procedure ClearData; override;
    function DoGetData: boolean; override;
    function HasValidFormats: boolean; override;
  public
    function PasteFromClipboard: longint; Override;
    property Text: String Read fText Write fText;
  end;

  TDropDummy = class(TDropTarget)
  protected
    procedure ClearData; override;
    function DoGetData: boolean; override;
    function HasValidFormats: boolean; override;
  end;

const
  HDropFormatEtc: TFormatEtc = (cfFormat: CF_HDROP;
    ptd: nil; dwAspect: DVASPECT_CONTENT; lindex: -1; tymed: TYMED_HGLOBAL);
  TextFormatEtc: TFormatEtc = (cfFormat: CF_TEXT;
    ptd: nil; dwAspect: DVASPECT_CONTENT; lindex: -1; tymed: TYMED_HGLOBAL);

function GetFilesFromHGlobal(const HGlob: HGlobal; Files: TStrings): boolean;
function ClientPtToWindowPt(Handle: HWND; pt: TPoint): TPoint;

procedure Register;

implementation

uses
  SysUtils,
  Graphics,
  Messages,
  ShlObj,
  ClipBrd,
  Forms;

procedure Register;
begin
  RegisterComponents('DragDrop',[TDropFileTarget, TDropTextTarget, TDropDummy]);
end;

// TDummyWinControl is declared just to expose the protected property - Font -
// which is used to calculate the 'scroll margin' for the target window.
type
  TDummyWinControl = Class(TWinControl);

// -----------------------------------------------------------------------------
//      Miscellaneous functions ...
// -----------------------------------------------------------------------------

function ClientPtToWindowPt(Handle: HWND; pt: TPoint): TPoint;
var
  Rect: TRect;
begin
  ClientToScreen(Handle, pt);
  GetWindowRect(Handle, Rect);
  Result.X := pt.X - Rect.Left;
  Result.Y := pt.Y - Rect.Top;
end;
// -----------------------------------------------------------------------------

function GetFilesFromHGlobal(const HGlob: HGlobal; Files: TStrings): boolean;
var
  DropFiles: PDropFiles;
  Filename: PChar;
begin
  DropFiles := PDropFiles(GlobalLock(HGlob));
  try
    Filename := PChar(DropFiles) + DropFiles^.pFiles;
    while (Filename^ <> #0) do
    begin
      if (DropFiles^.fWide) then // -> NT4 compatibility
      begin
        Files.Add(PWideChar(FileName));
        inc(Filename, (Length(PWideChar(FileName)) + 1) * 2);
      end else
      begin
        Files.Add(Filename);
        inc(Filename, Length(Filename) + 1);
      end;
    end;
  finally
    GlobalUnlock(HGlob);
  end;
  result := (Files.count > 0);
end;

// -----------------------------------------------------------------------------
// TWinControlProxy class ...
// -----------------------------------------------------------------------------

//This (hidden) control is created whenever a control is added to
//TDropTarget's target list. TWinControlProxy is created as a child of the target
//in order to monitor when its parent's windowhandle is created and destroyed.
//This is necessary to perform the real windows dragdrop registering &
//unregistering which requires the parent windowhandle. The parent's windowhandle
//may be created and destroyed any number of times during the control's lifetime
//(eg: if its own parent is reassigned) so TWinControlProxy monitors this and
//handles the real dragdrop registering/unregistering behind the scenes.
//(Delphi's TWinControl.DestroyHandle method kindly destroys child window
//handles before the self window handle.)

//It is important to note here that when the form containing the droptarget
//is finally destroyed, parent windowhandles are destroyed before child
//windowhandles (perhaps counter-intuitively) so it is important to
//unregister all TDropTarget's target controls before destroying the form (or
//destroying a target control).
//A secondary benefit is that a target control can be 'registered' with
//TDropTarget prior to having a windowhandle.

type

  TWinControlProxy = class(TWinControl)
  private
    FCreateWndCalled: boolean;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  end;

// -----------------------------------------------------------------------------
procedure TWinControlProxy.CreateWnd;
begin
  //inherited CreateWnd; //no need to consume windows resources here.
  if FCreateWndCalled then exit;
  if (RegisterDragDrop(Parent.Handle, TDropTarget(Owner)) <> S_OK) then
    raise ELibraryException.create('Failed to Register '+ Parent.name);
  FCreateWndCalled := true;
end;
// -----------------------------------------------------------------------------

procedure TWinControlProxy.DestroyWnd;
begin
  //inherited DestroyWnd;
  if not FCreateWndCalled then exit;
  if Parent.HandleAllocated then //this should always be the case!
    if RevokeDragDrop(Parent.Handle) <> S_OK then
      raise ELibraryException.create('Failed to Unregister '+ Parent.name);
  FCreateWndCalled := false;
end;

// -----------------------------------------------------------------------------
//      TDropTarget
// -----------------------------------------------------------------------------

function TDropTarget.GetTarget(pt: TPoint): TWinControl;
var
  i: integer;
  r: TRect;
begin
  for i := 0 to fTargetList.Count-1 do
  begin
    Result := fTargetList[i];
    GetWindowRect(Result.Handle, r);
    if PtInRect(r, pt) then
      exit;
  end;
  Result := nil;
end;
// -----------------------------------------------------------------------------

function TDropTarget.DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
  pt: TPoint; var dwEffect: Longint): HRESULT;
var
  ShiftState: TShiftState;
  TargetStyles: longint;
begin
  ClearData;
  fDataObj := dataObj;
  fTarget := GetTarget(pt);
  if fTarget = nil then
  begin
    fDataObj := nil;
    Result := DROPEFFECT_NONE;
    exit;
  end;

  result := S_OK;
  pt := fTarget.ScreenToClient(pt);
  fLastPoint := pt;
  
  fDragImageHandle := 0;
  if ShowImage then
  begin
    fDragImageHandle := ImageList_GetDragImage(nil,@fImageHotSpot);
    if (fDragImageHandle <> 0) then
    begin
      //Currently we will just replace any 'embedded' cursor with our
      //blank (transparent) image otherwise we sometimes get 2 cursors ...
      ImageList_SetDragCursorImage(fImages.Handle,0,fImageHotSpot.x,fImageHotSpot.y);
      with ClientPtToWindowPt(fTarget.handle,pt) do
        ImageList_DragEnter(fTarget.handle,x,y);
    end;
  end;

  if not HasValidFormats then
  begin
    fDataObj := nil;
    dwEffect := DROPEFFECT_NONE;
    exit;
  end;

  fScrollDirs := [];

  //thanks to a suggestion by Praful Kapadia ...
  fTargetScrollMargin := abs(TDummyWinControl(fTarget).font.height);

  TargetStyles := GetWindowLong(fTarget.handle,GWL_STYLE);
  if (TargetStyles and WS_HSCROLL <> 0) then
    fScrollDirs := fScrollDirs + [sdHorizontal];
  if (TargetStyles and WS_VSCROLL <> 0) then
    fScrollDirs := fScrollDirs + [sdVertical];
  //It's generally more efficient to get data only if a drop occurs
  //rather than on entering a potential target window.
  //However - sometimes there is a good reason to get it here.
  if fGetDataOnEnter then DoGetData;

  ShiftState := KeysToShiftState(grfKeyState);
  dwEffect := GetValidDropEffect(ShiftState,Pt,dwEffect);
  DoEnter(ShiftState, pt, dwEffect);

end;
// -----------------------------------------------------------------------------

procedure TDropTarget.DoEnter(ShiftState: TShiftState; Point: TPoint; var Effect: Longint);
begin
  if Assigned(fOnEnter) then
    fOnEnter(Self, ShiftState, Point, Effect);
end;
// -----------------------------------------------------------------------------

function TDropTarget.DragOver(grfKeyState: Longint;
  pt: TPoint; var dwEffect: Longint): HResult;
var
  ShiftState: TShiftState;
  IsScrolling: boolean;
begin

  pt := fTarget.ScreenToClient(pt);

  if fDataObj = nil then
  begin
    //fDataObj = nil when no valid formats .... see DragEnter method.
    dwEffect := DROPEFFECT_NONE;
    IsScrolling := false;
  end else
  begin
    ShiftState := KeysToShiftState(grfKeyState);
    dwEffect := GetValidDropEffect(ShiftState, pt, dwEffect);
    DoDragOver(ShiftState, pt, dwEffect);

    if (fScrollDirs <> []) and (dwEffect and DROPEFFECT_SCROLL <> 0) then
    begin
      IsScrolling := true;
      fScrollTimer.enabled := true
    end else
    begin
      IsScrolling := false;
      fScrollTimer.enabled := false;
    end;
  end;

  if (fDragImageHandle <> 0) and
      ((fLastPoint.x <> pt.x) or (fLastPoint.y <> pt.y)) then
  begin
    fLastPoint := pt;
    if IsScrolling then
      //fScrollTimer.enabled := true
    else with ClientPtToWindowPt(fTarget.handle,pt) do
      ImageList_DragMove(X,Y);
  end
  else
    fLastPoint := pt;
  RESULT := S_OK;
end;
// -----------------------------------------------------------------------------

procedure TDropTarget.DoDragOver(ShiftState: TShiftState; Point: TPoint; var Effect: Longint);
begin
  if Assigned(fOnDragOver) then
    fOnDragOver(Self, ShiftState, Point, Effect);
end;
// -----------------------------------------------------------------------------

function TDropTarget.DragLeave: HResult;
begin
  ClearData;
  fScrollTimer.enabled := false;
  fDataObj := nil;
  if (fDragImageHandle <> 0) then
    ImageList_DragLeave(fTarget.handle);
  DoLeave;
  fTarget := nil;
  Result := S_OK;
end;
// -----------------------------------------------------------------------------

procedure TDropTarget.DoLeave;
begin
  if Assigned(fOnLeave) then fOnLeave(Self);
end;
// -----------------------------------------------------------------------------

function TDropTarget.Drop(const dataObj: IDataObject; grfKeyState: Longint;
  pt: TPoint; var dwEffect: Longint): HResult;
var
  ShiftState: TShiftState;
begin
  RESULT := S_OK;

  if fDataObj = nil then
  begin
    dwEffect := DROPEFFECT_NONE;
    exit;
  end;

  fScrollTimer.enabled := false;

  if (fDragImageHandle <> 0) then
    ImageList_DragLeave(fTarget.handle);

  ShiftState := KeysToShiftState(grfKeyState);
  pt := fTarget.ScreenToClient(pt);
  dwEffect := GetValidDropEffect(ShiftState, pt, dwEffect);

  if {(not fGetDataOnEnter) and} (not DoGetData) then
    dwEffect := DROPEFFECT_NONE
  else
    DoDrop(ShiftState, pt, dwEffect);

  // clean up!
  ClearData;
  fDataObj := nil;
  fTarget := nil;
end;
// -----------------------------------------------------------------------------

procedure TDropTarget.DoDrop(ShiftState: TShiftState; Point: TPoint; var Effect: Longint);
begin
  if Assigned(fOnDrop) then
    fOnDrop(Self, ShiftState, Point, Effect);
end;
// -----------------------------------------------------------------------------

constructor TDropTarget.Create( AOwner: TComponent );
var
  bm: TBitmap;
begin
   inherited Create( AOwner );
   fTargetList := TList.create;
   fScrollTimer := TTimer.create(self);
   fScrollTimer.interval := 100;
   fScrollTimer.enabled := false;
   fScrollTimer.OnTimer := DoTargetScroll;
   _AddRef;
   fGetDataOnEnter := false;

   fImages := TImageList.create(self);
   //Create a blank image for fImages which we...
   //will use to hide any cursor 'embedded' in a drag image.
   //This avoids the possibility of two cursors showing.
   bm := TBitmap.Create;
   with bm do
   begin
     Canvas.Lock;
     Try
       height := 32;
       width := 32;
       Canvas.Brush.Color:=clWindow;
       Canvas.FillRect(Rect(0,0,31,31));
       fImages.AddMasked(bm,clWindow);
     Finally
       Canvas.Unlock;
       free;
     End;
   end;
   fDataObj := nil;
   ShowImage := true;
end;
// -----------------------------------------------------------------------------

destructor TDropTarget.Destroy;
begin
  fImages.free;
  fScrollTimer.free;
  UnregisterAll;
  fTargetList.Free;
  inherited Destroy;
end;

procedure TDropTarget.Register(Target: TWinControl);
begin
  //first check if already registered...
  if (fTargetList.indexof(Target) >= 0) or (Target = nil) then exit;
  //now create TWinControlProxy which handles the real register/unregister...
  with TWinControlProxy.Create(self) do
    parent := Target;
  fTargetList.Add(Target);
end;
// -----------------------------------------------------------------------------

procedure TDropTarget.Unregister(Target: TWinControl);
var
  i: integer;
begin
  if Target = nil then //ie: if Target = nil then unregister all targets
  begin
    for i := fTargetList.count-1 downto 0 do
      Unregister(TWinControl(fTargetList[i]));
    exit;
  end;

  i := fTargetList.indexof(Target);
  if i < 0 then exit; //not in list!
  //delete Target from the target list...
  fTargetList.delete(i);
  //delete Target's TWinControlProxy control...
  for i := 0 to Target.ControlCount-1 do
   if (Target.Controls[i] is TWinControlProxy) and
      (TWinControlProxy(Target.Controls[i]).Owner = Self) then
   begin
     //DestroyWnd method must be explicitly called here because
     //no windowhandle is ever allocated for TWinControlProxy...
     TWinControlProxy(Target.Controls[i]).DestroyWnd;
     Target.Controls[i].Free;
     break;
   end;
end;
// -----------------------------------------------------------------------------

procedure TDropTarget.UnregisterAll; //Unregister all targets
begin
  Unregister(nil);
end;
// -----------------------------------------------------------------------------

procedure TDropTarget.SetShowImage(Show: boolean);
begin
  fShowImage := Show;
  if fDataObj <> nil then
    ImageList_DragShowNolock(fShowImage);
end;
// -----------------------------------------------------------------------------

function TDropTarget.GetValidDropEffect(ShiftState: TShiftState;
  pt: TPoint; dwEffect: LongInt): LongInt;
begin
  //dwEffect 'in' parameter = set of drop effects allowed by drop source.
  //Now filter out the effects disallowed by target...
  if not (dtCopy in fDragTypes) then
    dwEffect := dwEffect and not DROPEFFECT_COPY;
  if not (dtMove in fDragTypes) then
    dwEffect := dwEffect and not DROPEFFECT_MOVE;
  if not (dtLink in fDragTypes) then
    dwEffect := dwEffect and not DROPEFFECT_LINK;
  Result := dwEffect;

  //'Default' behaviour can be overriden by assigning OnGetDropEffect.
  if Assigned(fGetDropEffectEvent) then
    fGetDropEffectEvent(self, ShiftState, pt, Result)
  else
  begin
    //As we're only interested in ssShift & ssCtrl here
    //mouse buttons states are screened out ...
    ShiftState := ([ssShift, ssCtrl] * ShiftState);

    if (ShiftState = [ssShift, ssCtrl]) and
      (dwEffect and DROPEFFECT_LINK <> 0) then result := DROPEFFECT_LINK
    else if (ShiftState = [ssShift]) and
      (dwEffect and DROPEFFECT_MOVE <> 0) then result := DROPEFFECT_MOVE
    else if (dwEffect and DROPEFFECT_COPY <> 0) then result := DROPEFFECT_COPY
    else if (dwEffect and DROPEFFECT_MOVE <> 0) then result := DROPEFFECT_MOVE
    else if (dwEffect and DROPEFFECT_LINK <> 0) then result := DROPEFFECT_LINK
    else result := DROPEFFECT_NONE;
    //Add Scroll effect if necessary...
    if fScrollDirs = [] then exit;
    if (sdHorizontal in fScrollDirs) and
      ((pt.x < fTargetScrollMargin) or
          (pt.x>fTarget.ClientWidth - fTargetScrollMargin)) then
        result := result or integer(DROPEFFECT_SCROLL)
    else if (sdVertical in fScrollDirs) and
      ((pt.y < fTargetScrollMargin) or
          (pt.y>fTarget.ClientHeight - fTargetScrollMargin)) then
        result := result or integer(DROPEFFECT_SCROLL);
  end;
end;
// -----------------------------------------------------------------------------

procedure TDropTarget.DoTargetScroll(Sender: TObject);
begin
  with fTarget, fLastPoint do
  begin
    if (fDragImageHandle <> 0) then ImageList_DragLeave(handle);
    if (Y < fTargetScrollMargin) then
      Perform(WM_VSCROLL,SB_LINEUP,0)
    else if (Y>ClientHeight - fTargetScrollMargin) then
      Perform(WM_VSCROLL,SB_LINEDOWN,0);
    if (X < fTargetScrollMargin) then
      Perform(WM_HSCROLL,SB_LINEUP,0)
    else if (X > ClientWidth - fTargetScrollMargin) then
      Perform(WM_HSCROLL,SB_LINEDOWN,0);
    if (fDragImageHandle <> 0) then
      with ClientPtToWindowPt(handle,fLastPoint) do
        ImageList_DragEnter(handle,x,y);
  end;
end;
// -----------------------------------------------------------------------------

function TDropTarget.PasteFromClipboard: longint;
var
  Global: HGlobal;
  pEffect: ^DWORD;
begin
  if not ClipBoard.HasFormat(CF_PREFERREDDROPEFFECT) then
    result := DROPEFFECT_NONE
  else
  begin
    Global := Clipboard.GetAsHandle(CF_PREFERREDDROPEFFECT);
    pEffect := pointer(GlobalLock(Global)); // DROPEFFECT_COPY, DROPEFFECT_MOVE ...
    result := pEffect^;
    GlobalUnlock(Global);
  end;
end;

// -----------------------------------------------------------------------------
//      TDropFileTarget
// -----------------------------------------------------------------------------

constructor TDropFileTarget.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  fFiles := TStringList.Create;
  fMappedNames := TStringList.Create;
  with fFileNameMapFormatEtc do
  begin
    cfFormat := CF_FILENAMEMAP;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  with fFileNameMapWFormatEtc do
  begin
    cfFormat := CF_FILENAMEMAPW;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
end;
// -----------------------------------------------------------------------------

destructor TDropFileTarget.Destroy;
begin
  fFiles.Free;
  fMappedNames.Free;
  inherited Destroy;
end;
// -----------------------------------------------------------------------------

function TDropFileTarget.PasteFromClipboard: longint;
var
  Global: HGlobal;
  Preferred: longint;
begin
  result  := DROPEFFECT_NONE;
  if not ClipBoard.HasFormat(CF_HDROP) then exit;
  Global := Clipboard.GetAsHandle(CF_HDROP);
  fFiles.clear;
  if not GetFilesFromHGlobal(Global,fFiles) then exit;
  Preferred := inherited PasteFromClipboard;
  //if no Preferred DropEffect then return copy else return Preferred ...
  if (Preferred = DROPEFFECT_NONE) then
    result := DROPEFFECT_COPY else
    result := Preferred;
end;
// -----------------------------------------------------------------------------

function TDropFileTarget.HasValidFormats: boolean;
begin
  result := (fDataObj.QueryGetData(HDropFormatEtc) = S_OK);
end;
// -----------------------------------------------------------------------------

procedure TDropFileTarget.ClearData;
begin
  fFiles.clear;
  fMappedNames.clear;
end;
// -----------------------------------------------------------------------------

function TDropFileTarget.DoGetData: boolean;
var
  medium: TStgMedium;
  pFilename: pChar;
  pFilenameW: PWideChar;
  sFilename: string;
begin
  ClearData;
  result := false;

  if (fDataObj.GetData(HDropFormatEtc, medium) <> S_OK) then
    exit;

  try
    if (medium.tymed = TYMED_HGLOBAL) and
       GetFilesFromHGlobal(medium.HGlobal,fFiles) then
      result := true else
      result := false;
  finally
    //Don't forget to clean-up!
    ReleaseStgMedium(medium);
  end;

  //OK, now see if file name mapping is also used ...
  if (fDataObj.GetData(fFileNameMapFormatEtc, medium) = S_OK) then
  try
    if (medium.tymed = TYMED_HGLOBAL) then
    begin
      pFilename := GlobalLock(medium.HGlobal);
      try
        while true do
        begin
          sFilename := pFilename;
          if sFilename = '' then break;
          fMappedNames.add(sFilename);
          inc(pFilename, length(sFilename)+1);
        end;
        if fFiles.count <> fMappedNames.count then
          fMappedNames.clear;
      finally
        GlobalUnlock(medium.HGlobal);
      end;
    end;
  finally
    ReleaseStgMedium(medium);
  end
  else if (fDataObj.GetData(fFileNameMapWFormatEtc, medium) = S_OK) then
  try
    if (medium.tymed = TYMED_HGLOBAL) then
    begin
      pFilenameW := GlobalLock(medium.HGlobal);
      try
        while true do
        begin
          sFilename := WideCharToString(pFilenameW);
          if sFilename = '' then break;
          fMappedNames.add(sFilename);
          inc(pFilenameW, length(sFilename)+1);
        end;
        if fFiles.count <> fMappedNames.count then
          fMappedNames.clear;
      finally
        GlobalUnlock(medium.HGlobal);
      end;
    end;
  finally
    ReleaseStgMedium(medium);
  end;

end;

// -----------------------------------------------------------------------------
//      TDropTextTarget
// -----------------------------------------------------------------------------

function TDropTextTarget.PasteFromClipboard: longint;
var
  Global: HGlobal;
  TextPtr: pChar;
begin
  result := DROPEFFECT_NONE;
  if not ClipBoard.HasFormat(CF_TEXT) then exit;
  Global := Clipboard.GetAsHandle(CF_TEXT);
  TextPtr := GlobalLock(Global);
  fText := TextPtr;
  GlobalUnlock(Global);
  result := DROPEFFECT_COPY;
end;
// -----------------------------------------------------------------------------

function TDropTextTarget.HasValidFormats: boolean;
begin
  result := (fDataObj.QueryGetData(TextFormatEtc) = S_OK);
end;
// -----------------------------------------------------------------------------

procedure TDropTextTarget.ClearData;
begin
  fText := '';
end;
// -----------------------------------------------------------------------------

function TDropTextTarget.DoGetData: boolean;
var
  medium: TStgMedium;
  cText: pchar;
begin
  result := false;
  if fText <> '' then
    result := true // already got it!
  else if (fDataObj.GetData(TextFormatEtc, medium) = S_OK) then
  begin
    try
      if (medium.tymed <> TYMED_HGLOBAL) then exit;
      cText := PChar(GlobalLock(medium.HGlobal));
      fText := cText;
      GlobalUnlock(medium.HGlobal);
      result := true;
    finally
      ReleaseStgMedium(medium);
    end;
  end
  else
    result := false;
end;

// -----------------------------------------------------------------------------
//      TDropDummy
//      This component is designed just to display drag images over the
//      registered TWincontrol but where no drop is desired (eg a TForm).
// -----------------------------------------------------------------------------

function TDropDummy.HasValidFormats: boolean;
begin
  result := false;
end;
// -----------------------------------------------------------------------------

procedure TDropDummy.ClearData;
begin
  //abstract method override
end;
// -----------------------------------------------------------------------------

function TDropDummy.DoGetData: boolean;
begin
  result := false;
end;
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

end.

