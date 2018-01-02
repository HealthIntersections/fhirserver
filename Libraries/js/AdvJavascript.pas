unit AdvJavascript;

{
  Subclasses the Javascript library so it knows about Adv library reference counting
}

interface

uses
  SysUtils, Classes, Generics.Collections, Soap.EncdDecd,
  Javascript, EncodeSupport,
  AdvObjects, AdvObjectLists, AdvGenerics;

type
  TAdvJavascript = class (TJavascript)
  protected
    procedure freeObject(obj : TObject); override;
  end;

  TAdvObjectListManager = class (TJavascriptArrayManager)
  private
    FList : TAdvObjectList;
    FClassDefinition : TJavascriptClassDefinition;
  public
    constructor Create(list : TAdvObjectList; def : TJavascriptClassDefinition);
    destructor Destroy; override;

    function count : integer; override;
    function item(i : integer) : JsValueRef; override;
    function push(this : TJsValue; params : TJsValues) : TJsValue; override;
  end;

  TAdvListManager<T: TAdvObject> = class (TJavascriptArrayManager)
  private
    FList : TAdvList<T>;
    FClassDefinition : TJavascriptClassDefinition;
  public
    constructor Create(list : TAdvList<T>; def : TJavascriptClassDefinition);
    destructor Destroy; override;

    function count : integer; override;
    function item(i : integer) : JsValueRef; override;
    function push(this : TJsValue; params : TJsValues) : TJsValue; override;
  end;

function base64AsString(value: TBytes): String;
function stringAsBase64(value: String): TBytes;


implementation

function base64AsString(value: TBytes): String;
begin
  result := String(EncodeBase64(@value[0], length(value))).replace(#13#10, '');
end;

function stringAsBase64(value: String): TBytes;
begin
  result := DecodeBase64(AnsiString(value));
end;

{ TAdvJavascript }

procedure TAdvJavascript.freeObject(obj: TObject);
begin
  if obj is TAdvObject then
    TAdvObject(obj).Free
  else
    obj.Free;
end;

{ TAdvListManager<T> }

constructor TAdvListManager<T>.create(list: TAdvList<T>; def : TJavascriptClassDefinition);
begin
  inherited Create;
  FList := list;
  FClassDefinition := def;
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
  result := FJavascript.wrap(FList[i].Link, FClassDefinition, true);
end;

function TAdvListManager<T>.push(this : TJsValue; params : TJsValues) : TJsValue;
var
  o : T;
  owns : boolean;
  pl : TJsValues;
  p : TJsValue;
begin
  setLength(pl, 1);
  for p in params do
  begin
    o := FJavascript.getWrapped<T>(p);
    if o = nil then
    begin
      pl[0] := p;
      o := FClassDefinition.Factory(FJavascript, FClassDefinition, pl, owns) as T;
    end;
    Flist.add(o.Link);
  end;
  result := FJavascript.wrap(FList.Count);
end;

{ TAdvObjectListManager }

constructor TAdvObjectListManager.create(list: TAdvObjectList; def : TJavascriptClassDefinition);
begin
  inherited Create;
  FList := list;
  FClassDefinition := def;
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
  result := FJavascript.wrap(FList[i].Link, FClassDefinition, true);
end;

function TAdvObjectListManager.push(this : TJsValue; params : TJsValues) : TJsValue;
var
  p : TJsValue;
  o : TAdvObject;
  pl : TJsValues;
  owns : boolean;
begin
  setLength(pl, 1);
  for p in params do
  begin
    o := FJavascript.getWrapped<TAdvObject>(p).Link;
    if o = nil then
    begin
      pl[0] := p;
      o := FClassDefinition.Factory(FJavascript, FClassDefinition, pl, owns) as TAdvObject;
    end;
    try
      Flist.add(o.Link);
    finally
      o.Free;
    end;
  end;
  result := FJavascript.wrap(FList.Count);
end;

end.
