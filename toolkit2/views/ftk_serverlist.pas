unit ftk_serverlist;

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
  Classes, SysUtils, Graphics, IniFiles,
  Controls, ComCtrls,
  fsl_base, fsl_utilities, fsl_json,
  fui_lcl_managers,
  fhir_client,
  ftk_utilities, ftk_constants;

type
  TFHIRServersView = class;

  { TServerListManager }

  TServerListManager = class (TListManager<TFHIRServerEntry>)
  private
    FView : TFHIRServersView;
  public
    function canSort : boolean; override;
    function doubleClickEdit : boolean; override;
    function allowedOperations(item : TFHIRServerEntry) : TNodeOperationSet; override;
    function loadList : boolean; override;

    procedure buildMenu; override;
    function getImageIndex(item : TFHIRServerEntry) : integer; override;
    function getCellText(item : TFHIRServerEntry; col : integer) : String; override;
    function getCellColors(item : TFHIRServerEntry; col : integer; var fore, back : TColor) : boolean; override;
    function getSummaryText(item : TFHIRServerEntry) : String; override;
    function compareItem(left, right : TFHIRServerEntry; col : integer) : integer; override;
    function filterItem(item : TFHIRServerEntry; s : String) : boolean; override;

    function addItem(mode : String) : TFHIRServerEntry; override;
    function editItem(item : TFHIRServerEntry; mode : String) : boolean; override;
    function deleteItem(item : TFHIRServerEntry) : boolean; override;
    function executeItem(item : TFHIRServerEntry; mode : String) : boolean; override;

  end;

  TOpenServerEvent = procedure (sender : TObject; server : TFHIRServerEntry) of object;

  { TFHIRServersView }

  TFHIRServersView = class (TFslObject)
  private
    FControlFile: String;
    FManager : TServerListManager;
    FOnEditServer: TOpenServerEvent;
    FOnOpenServer : TOpenServerEvent;
    function GetImages: TImageList;
    function GetList: TListView;
    function GetServerList: TFslList<TFHIRServerEntry>;
    procedure SetImages(AValue: TImageList);
    procedure SetList(AValue: TListView);
  public
    constructor Create(settings : TIniFile);
    destructor Destroy; override;

    property ControlFile : String read FControlFile write FControlFile;
    property List : TListView read GetList write SetList;
    property Images : TImageList read GetImages write SetImages;
    property ServerList : TFslList<TFHIRServerEntry> read GetServerList;
    property OnOpenServer : TOpenServerEvent read FOnOpenServer write FOnOpenServer;
    property OnEditServer : TOpenServerEvent read FOnEditServer write FOnEditServer;

    procedure load;
    procedure save;
    procedure refresh;
    procedure saveStatus;

    function FetchServer(sender : TObject; name : String) : TFHIRServerEntry;

    procedure addServer(server : TFHIRServerEntry);
    procedure updateServer(server, newDetails : TFHIRServerEntry);
  end;

implementation

uses
  ftk_worker_server;

{ TServerListManager }

function TServerListManager.canSort: boolean;
begin
  Result := false;
end;

function TServerListManager.doubleClickEdit: boolean;
begin
  Result := false;
end;

function TServerListManager.allowedOperations(item: TFHIRServerEntry): TNodeOperationSet;
begin
  if item <> nil then
    result := [opDelete, opEdit, opExecute]
  else
    result := [];
end;

function TServerListManager.loadList: boolean;
var
  json, o : TJsonObject;
  arr : TJsonArray;
  i : integer;
begin
  if (FileExists(FView.FControlFile)) then
  begin
    json := TJSONParser.ParseFile(FView.FControlFile);
    try
      arr := json.forceArr['servers'];
      for i := 0 to arr.count - 1 do
        Data.Add(TFHIRServerEntry.fromJson(arr.Obj[i]));
    finally
      json.free;
    end;
  end;
end;

procedure TServerListManager.buildMenu;
begin
  inherited buildMenu;
  registerMenuEntry('Open', ICON_EXECUTE, copExecute);
  registerMenuEntry('Edit', ICON_EDIT, copEdit);
  registerMenuEntry('Delete', ICON_DELETE, copDelete);
end;

function TServerListManager.getImageIndex(item: TFHIRServerEntry): integer;
begin
  if item.workerObject <> nil then
    result := 104
  else if item.pinned then
    result := 105
  else
    result := 103;
end;

function TServerListManager.getCellText(item: TFHIRServerEntry; col: integer): String;
begin
  result := item.name;
end;

function TServerListManager.getCellColors(item: TFHIRServerEntry; col: integer; var fore, back: TColor): boolean;
begin
  Result := inherited getCellColors(item, col, fore, back);
end;

function TServerListManager.getSummaryText(item: TFHIRServerEntry): String;
begin
  Result := item.name+' ('+item.URL+')';
end;

function TServerListManager.compareItem(left, right: TFHIRServerEntry; col: integer): integer;
begin
  Result := inherited compareItem(left, right, col);
end;

function TServerListManager.filterItem(item: TFHIRServerEntry; s: String): boolean;
begin
  Result := inherited filterItem(item, s);
end;

function TServerListManager.addItem(mode: String): TFHIRServerEntry;
begin
  Result := inherited AddItem(mode);
end;

function TServerListManager.editItem(item: TFHIRServerEntry; mode: String): boolean;
begin
  FView.OnEditServer(FView, item);
end;

function TServerListManager.deleteItem(item: TFHIRServerEntry) : boolean;
begin
  result := inherited DeleteItem(item);
end;

function TServerListManager.executeItem(item: TFHIRServerEntry; mode: String): boolean;
begin
  FView.OnOpenServer(Fview, item);
  Result := true;
end;

{ TFHIRServersView }

constructor TFHIRServersView.Create(settings : TIniFile);
begin
  inherited Create;
  FManager := TServerListManager.Create;
  FManager.Settings := Settings;
  FManager.FView := self;
end;

destructor TFHIRServersView.Destroy;
begin
  FManager.free;
  inherited Destroy;
end;

function TFHIRServersView.GetList: TListView;
begin
  result := FManager.List;
end;

function TFHIRServersView.GetImages: TImageList;
begin
  result := FManager.Images;
end;

function TFHIRServersView.GetServerList: TFslList<TFHIRServerEntry>;
begin
  result := FManager.Data;
end;

procedure TFHIRServersView.SetImages(AValue: TImageList);
begin
  FManager.Images := AValue;
end;

procedure TFHIRServersView.SetList(AValue: TListView);
begin
  FManager.List := AValue;
end;

procedure TFHIRServersView.load;
begin
  try
    FManager.doLoad();
  except
  end;
end;

procedure TFHIRServersView.save;
var
  json : TJsonObject;
  arr : TJsonArray;
  server : TFHIRServerEntry;
  stream : TFileStream;
begin
  json := TJsonObject.Create;
  try
    arr := json.forceArr['servers'];
    for server in FManager.Data do
      arr.add(server.toJson);
    stream := TFileStream.Create(FControlFile, fmCreate);
    try
      TJSONWriter.writeObject(stream, json, true);
    finally
      stream.free;
    end;
  finally
    json.free;
  end;
end;

procedure TFHIRServersView.refresh;
begin
  FManager.refresh();
end;

procedure TFHIRServersView.saveStatus;
begin
  FManager.saveStatus;
end;

function TFHIRServersView.FetchServer(sender: TObject; name: String): TFHIRServerEntry;
var
  t : TFHIRServerEntry;
begin
  result := nil;
  for t in Fmanager.Data do
    if (t.Name = name) or (t.URL = name) then
      exit(t);
end;

procedure TFHIRServersView.addServer(server: TFHIRServerEntry);
begin
  server.id := NewGuidId;
  FManager.Data.Insert(0, server.link);
  save;
  FManager.doLoad();
end;

procedure TFHIRServersView.updateServer(server, newDetails: TFHIRServerEntry);
begin
  server.assign(newDetails);
  save;
  FManager.doLoad();
  if (server.workerObject <> nil) then
    (server.workerObject as TServerWorker).serverChanged;
end;

end.

