unit AdvJavascript;

{
  Subclasses the Javascript library so it knows about Adv library reference counting
}

interface

uses
  SysUtils, Classes, Generics.Collections,
  Javascript,
  AdvObjects, AdvObjectLists, AdvGenerics;

type
  TAdvJavascript = class (TJavascript)
  protected
    procedure freeObject(obj : TObject); override;
  end;

  TAdvObjectListManager = class (TJavascriptArrayManager)
  private
    FList : TAdvObjectList;
    FDefiner : TJavascriptDefineObjectProc;
    FFactory : TJavascriptObjectFactoryProc<TAdvObject>;
  public
    constructor Create(list : TAdvObjectList; definer : TJavascriptDefineObjectProc; factory : TJavascriptObjectFactoryProc<TAdvObject>);
    destructor Destroy; override;

    function count : integer; override;
    function item(i : integer) : JsValueRef; override;
    function push(params : PJsValueRefArray; paramCount : integer) : JsValueRef; override;
  end;

  TAdvListManager<T: class> = class (TJavascriptArrayManager)
  private
    FList : TAdvList<T>;
    FDefiner : TJavascriptDefineObjectProc;
    FFactory : TJavascriptObjectFactoryProc<T>;
  public
    constructor Create(list : TAdvList<T>; definer : TJavascriptDefineObjectProc; factory : TJavascriptObjectFactoryProc<T>);
    destructor Destroy; override;

    function count : integer; override;
    function item(i : integer) : JsValueRef; override;
    function push(params : PJsValueRefArray; paramCount : integer) : JsValueRef; override;
  end;


implementation

{ TAdvJavascript }

procedure TAdvJavascript.freeObject(obj: TObject);
begin
  if obj is TAdvObject then
    TAdvObject(obj).Free
  else
    obj.Free;
end;

{ TAdvListManager<T> }

constructor TAdvListManager<T>.create(list: TAdvList<T>; definer : TJavascriptDefineObjectProc; factory : TJavascriptObjectFactoryProc<T>);
begin
  inherited Create;
  FList := list;
  FDefiner := definer;
  FFactory := factory;
end;

destructor TAdvListManager<T>.Destroy;
begin
  FList.Free;
  inherited;
end;

function TAdvListManager<T>.count: integer;
begin
  result := FList.Count;
end;

function TAdvListManager<T>.item(i: integer): JsValueRef;
begin
  result := FManager.wrap(FList[i], FDefiner, false);
end;

function TAdvListManager<T>.push(params: PJsValueRefArray; paramCount: integer): JsValueRef;
var
  i : integer;
  o : T;
begin
  for i := 1 to paramCount - 1 do
  begin
    o := FManager.getWrapped<T>(params[i]);
    if o = nil then
      o := FFactory(FManager, params[i]);
    Flist.add(o);
  end;
  result := FManager.wrap(FList.Count);
end;

{ TAdvObjectListManager }

constructor TAdvObjectListManager.create(list: TAdvObjectList; definer : TJavascriptDefineObjectProc; factory : TJavascriptObjectFactoryProc<TAdvObject>);
begin
  inherited Create;
  FList := list;
  FDefiner := definer;
  FFactory := factory;
end;

destructor TAdvObjectListManager.Destroy;
begin
  FList.Free;
  inherited;
end;

function TAdvObjectListManager.count: integer;
begin
  result := FList.Count;
end;

function TAdvObjectListManager.item(i: integer): JsValueRef;
begin
  result := FManager.wrap(FList[i], FDefiner, false);
end;

function TAdvObjectListManager.push(params: PJsValueRefArray; paramCount: integer): JsValueRef;
var
  i : integer;
  o : TAdvObject;
begin
  for i := 1 to paramCount - 1 do
  begin
    o := FManager.getWrapped<TAdvObject>(params[i]);
    if o = nil then
      o := FFactory(FManager, params[i]);
    Flist.add(o);
  end;
  result := FManager.wrap(FList.Count);
end;

end.
