unit ClosureManager;

interface

Uses
  Sysutils,
  AdvObjects,
  KDBManager,
  TerminologyServerStore;

Type
  TClosureManager = class (TAdvObject)
  private
    FName : String;
  public
    Constructor Create(name : String); overload;

    function Link : TClosureManager; overload;

    procedure Init(store : TTerminologyServerStore; conn : TKDBConnection);
  end;

implementation

{ TClosureManager }

constructor TClosureManager.Create(name: String);
begin
  inherited Create;
  FName := name;
end;

procedure TClosureManager.Init(store : TTerminologyServerStore; conn: TKDBConnection);
var
  key : integer;
begin
  // look in the database
  key := conn.CountSQL('select ClosureKey from Closures where Name = '''+FName+'''');
  if key = 0 then
  begin
    key := store.nextClosureKey;
    conn.ExecSQL('insert into Closures (ClosureKey, Name) values ('+inttostr(key)+', '''+FName+''')');
  end
  else
    conn.ExecSQL('delete from ClosureEntries where ClosureKey = '+inttostr(key));
end;

function TClosureManager.Link: TClosureManager;
begin
  result := TClosureManager(inherited Link);
end;

end.
