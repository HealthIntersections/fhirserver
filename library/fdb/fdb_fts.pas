unit fdb_fts;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_utilities,
  fdb_manager, fdb_sqlite3;

Type
  { TFDBFullTextSearchCompartment }

  TFDBFullTextSearchCompartment = class (TFslObject)
  private
    FName: String;
  public
    constructor Create(name : String);
    property name : String read FName write FName;
  end;

  { TFDBFullTextSearch }

  TFDBFullTextSearch = {abstract} class (TFslObject)
  private
    FName : string;
  public                           
    constructor Create(name : String);
    function link : TFDBFullTextSearch;

    Property name : String read FName;
    function createCompartment(name : String) : TFDBFullTextSearchCompartment; virtual; abstract;
    procedure addText(compartment : TFDBFullTextSearchCompartment; id : String; name, text : string); virtual; abstract;
    function closeCompartment(compartment : TFDBFullTextSearchCompartment) : String; virtual; abstract;

    procedure search(compartment : String; criteria : String; ids : TStringList); virtual; abstract;
  end;

  { TFDBSqlLiteFullTextSearchCompartment }

  TFDBSqlLiteFullTextSearchCompartment = class (TFDBFullTextSearchCompartment)
  private
    FConn : TFDBConnection;
  public
    constructor create(name : String; Conn : TFDBConnection);
    property Conn : TFDBConnection read FConn;
  end;

  TFDBSqlLiteFullTextSearch = class (TFDBFullTextSearch)
  private
    FDB : TFDBSQLiteManager;
  public                         
    constructor create(name : string; db : TFDBSQLiteManager);
    destructor Destroy; override;

    function createCompartment(name : String) : TFDBFullTextSearchCompartment; override;
    procedure addText(compartment : TFDBFullTextSearchCompartment; id : String; name, text : string); override;
    function closeCompartment(compartment : TFDBFullTextSearchCompartment) : String; override;

    procedure search(compartment : String; criteria : String; ids : TStringList); override;
  end;

  { TFDBFullTextSearchFactory }

  TFDBFullTextSearchFactory = class (TFslObject)
  public
    class function makeSQLiteTextSearch(name : String) : TFDBFullTextSearch;
  end;

implementation

{ TFDBFullTextSearchFactory }

class function TFDBFullTextSearchFactory.makeSQLiteTextSearch(name : String): TFDBFullTextSearch;
var
  fn : String;
begin
  fn := FilePath(['[tmp]', 'fts-'+name+'.db']);
  deleteFile(fn);
  result := TFDBSqlLiteFullTextSearch.create(name, TFDBSQLiteManager.create('fts-'+name, fn, true));
end;

{ TFDBSqlLiteFullTextSearchCompartment }

constructor TFDBSqlLiteFullTextSearchCompartment.create(name: String; Conn: TFDBConnection);
begin
  inherited Create(name);
  FConn := Conn;
end;

{ TFDBFullTextSearchCompartment }

constructor TFDBFullTextSearchCompartment.create(name: String);
begin
  inherited Create;
  FName := name;
end;

{ TFDBSqlLiteFullTextSearch }

constructor TFDBSqlLiteFullTextSearch.create(name : String; db: TFDBSQLiteManager);
begin
  inherited create(name);
  FDB := db;
end;

destructor TFDBSqlLiteFullTextSearch.Destroy;
begin
  FDB.Free;
  inherited Destroy;
end;

function TFDBSqlLiteFullTextSearch.createCompartment(name: String): TFDBFullTextSearchCompartment;
var
  conn : TFDBConnection;
begin
  conn := FDB.GetConnection('compartment');
  conn.ExecSQL('CREATE VIRTUAL TABLE '+name+' USING fts5(id, name, content);');
  conn.SQL := 'Insert into '+name+' (id, name, content) values (:id, :name, :content)';
  conn.Prepare;
  result := TFDBSqlLiteFullTextSearchCompartment.create(name, conn);
end;

procedure TFDBSqlLiteFullTextSearch.addText(compartment: TFDBFullTextSearchCompartment; id: String; name, text: string);
var
  conn : TFDBConnection;
begin
  conn := (compartment as TFDBSqlLiteFullTextSearchCompartment).Conn;
  conn.BindString('id', id);
  conn.BindString('name', name);
  conn.BindString('content', text);
  conn.Execute;
end;

function TFDBSqlLiteFullTextSearch.closeCompartment(compartment: TFDBFullTextSearchCompartment) : String;
var
  conn : TFDBConnection;
begin
  conn := (compartment as TFDBSqlLiteFullTextSearchCompartment).Conn;
  conn.Terminate;
  result := inttostr(conn.CountSQL('Select count(*) from '+compartment.name))+' Entries';
  conn.Release;
end;

procedure TFDBSqlLiteFullTextSearch.search(compartment: String; criteria: String; ids: TStringList);
begin
  // not done yet
end;

{ TFDBFullTextSearch }

constructor TFDBFullTextSearch.Create(name: String);
begin
  inherited Create;
  FName := name;
end;

function TFDBFullTextSearch.link: TFDBFullTextSearch;
begin
  result := TFDBFullTextSearch(inherited link);
end;

end.