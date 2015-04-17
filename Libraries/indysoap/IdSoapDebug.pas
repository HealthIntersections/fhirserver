{
IndySOAP: This unit tracks objects to prevent object leaks
}

{$IFDEF CLR}
  {$UNDEF OBJECT_TRACKING}
{$ENDIF}

unit IdSoapDebug;

{$I IdSoapDefines.inc}

interface

uses
  { the contents of this uses clause are controlled. Do not add any units
    to this list without first discussing with grahame@kestral.com.au

    The reason for this is that this unit is rather for finding leaks,
    and the less units it depends on, the more useful it is. Currently,
    this unit depends on classes.pas. It may be rewritten in the future to
    not depend on classes.pas. This allows for you to search for leaks
    involving classes defined there (such as TLists etc, which often
    leak (no, the TList etc classes themselves doesn't leak, but it is often
    forgotten when cleaning up....))

   }
  Classes;

{$IFDEF CLR}
  {$UNDEF OBJECT_TRACKING}
  // you just can't do object tracking meaningfully under DotNet
{$ENDIF}
Const
  ALLOW_NIL = true;
  
type
  {$IFDEF VER130}
  NativeUInt = Cardinal;
  {$ENDIF}
  
  {$M+}
  TIdBaseObject = class (TObject)
  Private
    FSerialNo: Cardinal;
  Public
    constructor Create;
    destructor Destroy; Override;

    class function GetLiveObjectCount: Cardinal;
    // if this is called, then when the object is freed, a debugger
    // breakpoint will be called. This will be raised even if the object is
    // erroneously freed as another object (but not if it is erroneously freed
    // using freemem or similiar
    procedure AskForBreakPointOnFree;

    // checks that the object is valid and in the list of current objects.
    function TestValid(AClassType: TClass = NIL; AAllowNil : boolean = false): Boolean;

    // refer to help file
    procedure TestInvariants(const AParamName: string; Const AClass : TClass; const ALoc : string);

    function Invariants(const ALoc : string; AClass : TClass) : Boolean;

    // to support hash tables (for java bigots - see IdSoapUtilities.pas
    // if you want to use hash tables, you will need to override this in
    // a descendent. The rule for the value is simple:
    // when AObj1.Equals(AObj2), then AObj1.Hash = AObj2.Hash
    function idHash : integer; virtual;

    // to support Hash Tables. You may find other uses for this
    function idIsEqual(AObj : TIdBaseObject) : boolean; virtual;

  published
    // the client can use the SerialNumber as an independent confirmation
    // that the object is the actual object. This is protection against
    // the object being freed and recreated as a different valid object
    // between usages. will be 0 if OBJECT_TRACKING is disabled. this is published
    // so a memory dump of live objects can track this for debugging purposes
    property SerialNumber: Cardinal Read FSerialNo;

  end;
  {$M-}

// direct entry points into the system - allow other object heirarchies to
// use the object tracking system
function IdObjectRegister(AObject: TObject): Cardinal;
function IdObjectTestValid(AObject: TObject; AClassType: TClass = NIL; AAllowNil : boolean = false): Boolean;
procedure IdObjectDeregister(AObject: TObject; serial : cardinal);
procedure IdObjectBreakPointOnFree(AObject: TObject);
function IdClassInstanceCount(AClassName : String) : Integer;
function IdGetThreadObjectCount : Integer;

procedure IdSoapListClassCounts(AList : TStrings);

procedure IdSetLeakDialogAppearance(AValue : Boolean);
// if you have leaks, but don't want the notification, then call this and set the value to false

var
  GIdSoapTestInvariantFormat : string = '%s (%s): %3:s [%2:s]';

implementation

uses
  { the contents of this uses clause are controlled - see above }
  IdSoapResourceStrings,
  {$IFDEF OBJECT_TRACKING}
  {$IFDEF USE_ADV}
  AdvFactories,
  AdvStringIntegerMatches,
  {$ELSE}
  IdSoapTracker,
  {$ENDIF}
  {$ENDIF}
  SysUtils,
  windows;

const
  ASSERT_UNIT = 'IdSoapDebug';

{$IFDEF OBJECT_TRACKING}
 {$IFNDEF USE_ADV}
var
  gObjectList : TIdDebugObjectList = nil;
  GIdSoapSuppressLeakDialog : boolean = false;
  GTrackingClosed : Boolean = False;
 {$ENDIF}
{$ENDIF}

threadvar
  GThreadObjectCount : integer;

{ TIdBaseObject }
constructor TIdBaseObject.Create;
begin
  inherited;
  FSerialNo := IdObjectRegister(self);
end;

destructor TIdBaseObject.Destroy;
begin
  IdObjectDeregister(self, FSerialNo);
  inherited;
end;

class function TIdBaseObject.GetLiveObjectCount: Cardinal;
begin
  result := IdClassInstanceCount(ClassName);
end;

procedure TIdBaseObject.AskForBreakPointOnFree;
begin
  IdObjectBreakPointOnFree(self);
end;

function TIdBaseObject.TestValid(AClassType: TClass = NIL; AAllowNil : boolean = false): Boolean;
begin
  result := IdObjectTestValid(self, AClassType, AAllowNil);
end;

procedure TIdBaseObject.TestInvariants(const AParamName: string; Const AClass : TClass; const ALoc : string);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TBaseObject.TestInvariants';
begin
  assert(ALoc <> '', ASSERT_LOCATION+': ALoc = ''''');
  assert(self.TestValid(AClass), Format(GIdSoapTestInvariantFormat, [ASSERT_LOCATION, ALoc, AParamName, 'self is not valid']));
end;

function TIdBaseObject.idHash: integer;
begin
  // double type case for .Net
  result := integer(TObject(self));
end;

function TIdBaseObject.idIsEqual(AObj: TIdBaseObject): boolean;
begin
  result := AObj = self;
end;

{==============================================================================}

function IdObjectRegister(AObject: TObject): Cardinal;
Const ASSERT_LOCATION = ASSERT_UNIT+'.IdObjRegister';
begin
  inc(GThreadObjectCount);
  {$IFDEF OBJECT_TRACKING}
  {$IFDEF USE_ADV}
  Factory.Construct(AObject, result);
  {$ELSE}
  assert(GTrackingClosed or assigned(gObjectList), ASSERT_LOCATION+': Attempt to use Object tracking before it is initialised');
  if GTrackingClosed then
    result := 0
  else
    result := gObjectList.Construct(AObject);
  {$ENDIF}
  {$ELSE}
  result := 0;
  {$ENDIF}
end;

procedure IdObjectDeregister(AObject: TObject; serial : cardinal);
begin
  dec(GThreadObjectCount);
  {$IFDEF OBJECT_TRACKING}
  {$IFDEF USE_ADV}
  if serial <> 0 then
    Factory.Destruct(AObject);
  {$ELSE}
  if assigned(gObjectList) then
    begin
    gObjectList.Destruct(AObject);
    end;
  {$ENDIF}
  {$ELSE}
  // nothing
  {$ENDIF}
end;

function IdObjectTestValid(AObject: TObject; AClassType: TClass = NIL; AAllowNil : boolean = false): Boolean;
const ASSERT_LOCATION = ASSERT_UNIT + '.IdObjectTestValid';
begin
  {$IFDEF OBJECT_TRACKING}
  {$IFDEF USE_ADV}
  result := ((AObject = nil) and (AAllowNil)) or Factory.Valid(AObject);
  {$ELSE}
  assert(GTrackingClosed or assigned(gObjectList), ASSERT_LOCATION+': Attempt to use Object tracking before it is initialised');
  result := GTrackingClosed  or ((AObject = nil) and (AAllowNil)) or gObjectList.exists[AObject];
  {$ENDIF}
  {$ELSE}
  Result := AAllowNil or Assigned(AObject);
  {$ENDIF}
  if Result and Assigned(AObject) and assigned(AClassType) then
    begin
    Result := AObject is AClassType;
    end;
end;

function IdClassInstanceCount(AClassName : String) : Integer;
begin
  {$IFDEF OBJECT_TRACKING}
  {$IFDEF USE_ADV}
  Factory.Lock;
  try
    result := Factory.ObjectProfiler.FrequencyCountByClassName(AClassName);
  finally
    Factory.UnLock;
  end;
  {$ELSE}
  result := gObjectList.ClassCount(AClassName);
  {$ENDIF}
  {$ELSE}
  Result := 0;
  {$ENDIF}
end;

function IdGetThreadObjectCount : Integer;
begin
  result := GThreadObjectCount;
end;

procedure IdObjectBreakPointOnFree(AObject: TObject);
begin
  {$IFDEF OBJECT_TRACKING}
  {$IFDEF USE_ADV}
  Factory.Track(AObject);
  {$ELSE}
  gObjectList.BreakPoint[AObject] := true;
  {$ENDIF}
  {$ENDIF}
end;

procedure IdSetLeakDialogAppearance(AValue : Boolean);
begin
  {$IFDEF OBJECT_TRACKING}
  {$IFDEF USE_ADV}
  Factory.Profiled := AValue;
  {$ELSE}
  GIdSoapSuppressLeakDialog := not AValue;
  {$ENDIF}
  {$ENDIF}
end;

procedure InitObjectTracking;
Const ASSERT_LOCATION = ASSERT_UNIT+'.InitObjectTracking';
begin
  {$IFDEF OBJECT_TRACKING}
  {$IFNDEF USE_ADV}
  assert(not assigned(gObjectList), ASSERT_LOCATION+': Attempt to reinitialize Object Tracking after it has already been initialised');
  gObjectList := TIdDebugObjectList.Create;
  {$ENDIF}
  {$ENDIF}
end;

{$IFDEF OBJECT_TRACKING}
{$IFNDEF USE_ADV}
procedure SystemMessage(ATitle, AContent: String);
begin
  {$IFDEF CLR}
  MessageBox(0, AContent, ATitle, mb_ok);
  {$ELSE}
  MessageBox(0, PChar(AContent), PChar(ATitle), mb_ok);
  {$ENDIF}
end;
{$ENDIF}
{$ENDIF}

procedure CloseObjectTracking;
Const ASSERT_LOCATION = ASSERT_UNIT+'.CloseObjectTracking';
begin
  {$IFDEF OBJECT_TRACKING}
  {$IFNDEF USE_ADV}
  assert(assigned(gObjectList), ASSERT_LOCATION+': Attempt to finalize Object Tracking before it has been initialised');
  if not IsLibrary and not GIdSoapSuppressLeakDialog and (gObjectList.TotalCount > 0) then
    begin
    SystemMessage(RS_ERR_DEBUG_LEAKING_OBJECTS, gObjectList.DescribeLiveObjects);
    end;
  FreeAndNil(gObjectList);
  GTrackingClosed := true;
  {$ENDIF}
  {$ENDIF}
end;

procedure IdSoapListClassCounts(AList : TStrings);
{$IFDEF OBJECT_TRACKING}
{$IFDEF USE_ADV}
var
  LList  : TAdvStringIntegerMatch;
  i : integer;
{$ENDIF}
{$ENDIF}
begin
  AList.BeginUpdate;
  try
    AList.Clear;
    {$IFDEF OBJECT_TRACKING}
    {$IFDEF USE_ADV}
    LList := TAdvStringIntegerMatch.create;
    try
      Factory.Lock;
      try
        Factory.ObjectProfiler.CollectFrequencyMatch(LList);
      finally
        Factory.Unlock;
      end;
      for i := 0 to LList.Count - 1 do
        begin
        AList.AddObject(LList.KeyByIndex[i], TObject(LList.ValueByIndex[i]));
        end;
    finally
      FreeAndNil(LList);
    end;
    {$ELSE}
    gObjectList.GetClassCounts(AList);
    {$ENDIF}
    {$ENDIF}
  finally
    AList.EndUpdate;
  end;
end;

function TIdBaseObject.Invariants(const ALoc: string; AClass: TClass) : Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TBaseObject.Invariants';
begin
  assert(ALoc <> '', ASSERT_LOCATION+': ALoc = ''''');
  assert(self.TestValid(AClass), Format(GIdSoapTestInvariantFormat, [ASSERT_LOCATION, ALoc, '', 'self is not valid']));
  result := true;
end;

initialization
  InitObjectTracking;
finalization
  CloseObjectTracking;
end.

