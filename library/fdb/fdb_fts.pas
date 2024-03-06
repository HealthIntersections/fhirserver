unit fdb_fts;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  result := TFDBSqlLiteFullTextSearch.create(name, TFDBSQLiteManager.create('fts-'+name, fn, false, true));
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

