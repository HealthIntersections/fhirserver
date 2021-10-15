unit ftk_store_server;

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
  SysUtils, Classes, Dialogs,
  fsl_base, fsl_utilities, fsl_stream,
  fhir_objects, fhir_client,
  ftk_store, ftk_utilities, ftk_context;

type
  { TServerStorageService }
  TServerStorageService = class (TStorageService)
  private
    FOnConnectToServer: TConnectToServerEvent;
    FServerList : TFslList<TFHIRServerEntry>;
    procedure resolve(url : String; var server : TFHIRServerEntry; var rtype, id : String);
  public
    constructor Create(serverList : TFslList<TFHIRServerEntry>);
    destructor Destroy; override;

    function schemes : TArray<String>; override;
    function inScope(url : String) : boolean; override;

    function CheckTimes : boolean; override;
    function CurrencyCheckFrequency : integer; override;
    function canSave : boolean; override;
    function load(address : String; doException : boolean) : TLoadedBytes; override;
    function save(address : String; bytes : TBytes) : TDateTime; override;
    function CaptionForAddress(address : String) : String; override;
    function describe(address : String) : String; override;
    procedure delete(address : String); override;
    function openDlg(out newName : String) : boolean; override;
    function saveDlg(existing : String; suggestedExtension : String; out newName : String) : boolean; override;
    function MakeFilename(address : String) : String; override;
    function clientForAddress(address : String) : TFHIRClientV; override;
    procedure forceLocation(address : String); override;
    function getName(address : String; mode : TNameMode) : String; override;

    property OnConnectToServer : TConnectToServerEvent read FOnConnectToServer write FOnConnectToServer;
  end;

implementation

{ TServerStorageService }

procedure TServerStorageService.resolve(url: String; var server : TFHIRServerEntry; var rtype, id: String);
var
  e : TFHIRServerEntry;
  p : TArray<String>;
begin
  for e in FServerList do
  begin
    if url.StartsWith(e.URL) then
    begin
      p := url.subString(e.url.length).split(['/']);
      if (length(p) < 2) then
        raise EFHIRException.create('Unable to understand '+url+' for server '+e.url);
      if e.client = nil then
        OnConnectToServer(self, e);
      server := e;
      rtype := p[length(p)-2];
      id := p[length(p)-1];
      exit;
    end;
  end;
  raise EFHIRException.create('No registered server for address "'+url+'"');
end;

constructor TServerStorageService.Create(serverList: TFslList<TFHIRServerEntry>);
begin
  inherited create;
  FServerList := serverList;
end;

destructor TServerStorageService.Destroy;
begin
  FServerList.free;
  inherited Destroy;
end;

function TServerStorageService.schemes : TArray<String>;
begin
  result := ['http', 'https'];
end;

function TServerStorageService.inScope(url: String): boolean;
var
  e, server : TFHIRServerEntry;
  rtype, rId: String;
begin
  result := false;
  for e in FServerList do
  begin
    if url.StartsWith(e.URL) then
    begin
      try
        resolve(url, server, rType, rId);
        if (rType <> '') and (rId <> '') then
          exit(true);
      except
      end;
    end;
  end;
end;

function TServerStorageService.CheckTimes: boolean;
begin
  result := true;
end;

function TServerStorageService.CurrencyCheckFrequency: integer;
begin
  result := 5 * 60;
end;

function TServerStorageService.canSave: boolean;
begin
  result := true;
end;

function TServerStorageService.load(address: String; doException : boolean): TLoadedBytes;
var
  server : TFHIRServerEntry;
  rtype, id : String;
  buffer : TFslHTTPBuffer;
  headers : THTTPHeaders;
begin
  resolve(address, server, rtype, id);
  try
    buffer := server.client.customGet(rtype+'/'+id, headers);
    try
      result.content := buffer.AsBytes;
      result.timestamp := buffer.timestamp;
      result.mimeType := buffer.mimeType;
    finally
      buffer.free;
    end;
  except
    on e : EFHIRClientException do
    begin
      if doException then
        raise
      else if e.errorCode = 404 then
        result.timestamp := -1
      else
        result.timestamp := 0;
    end;
    on e : Exception do
    begin
      if doException then
        raise
      else
        result.timestamp := 0;
    end;
  end;
end;

function TServerStorageService.save(address: String; bytes: TBytes) : TDateTime;
begin
  raise EFHIRException.create('Not done yet');
end;

function TServerStorageService.CaptionForAddress(address: String): String;
var
  server : TFHIRServerEntry;
  rtype, id : String;
begin
  resolve(address, server, rtype, id);
  result := rtype+'/'+id;
end;

function TServerStorageService.describe(address: String): String;
var
  server : TFHIRServerEntry;
  rtype, id : String;
begin
  resolve(address, server, rtype, id);
  result := server.name+':'+rtype+'/'+id;
end;

procedure TServerStorageService.delete(address: String);
begin
  raise EFHIRException.create('Not done yet');
end;

function TServerStorageService.openDlg(out newName : String) : boolean;
//var
//  dlg : TOpenDialog;
begin
  raise EFHIRException.create('Not done yet');
  //dlg := TOpenDialog.create(handle);
  //try
  //  dlg.Filter := 'All Known Files|*.xml; *.json; *.ini; *.txt; *.v2; *.msg; *.hl7; *.template; *.liquid; *.js; *.md; *.htm; *.html|'+
  //    'XML|*.xml|'+
  //    'JSON|*.json|'+
  //    'Ini|*.ini|'+
  //    'V2 Messages|*.v2; *.msg; *.hl7|'+
  //    'Liquid Templates|*.template; *.liquid|'+
  //    'Javascript|*.js|'+
  //    'Markdown|*.md|'+
  //    'HTML|*.htm; *.html|'+
  //    'Text|*.txt|'+
  //    'All Fles|*.*';
  //  dlg.Options := [ofFileMustExist, ofEnableSizing, ofViewDetail];
  //  dlg.InitialDir := FIni.ReadString('file-store', 'folder', dlg.InitialDir);
  //  result := dlg.execute;
  //  if result then
  //  begin
  //    newName := 'file:'+dlg.FileName;
  //    FIni.WriteString('file-store', 'folder', ExtractFilePath(newName));
  //  end;
  //finally
  //  dlg.free;
  //end;
end;

function TServerStorageService.saveDlg(existing : String; suggestedExtension : String; out newName : String) : boolean;
//var
//  dlg : TSaveDialog;
//  fn : String;
begin
  //dlg := TSaveDialog.create(Handle);
  //try
  //  dlg.Options := [ofOverwritePrompt, ofEnableSizing, ofViewDetail];
  //  if (existing <> '') and existing.startsWith('file:') then
  //  begin
  //    fn := existing.Substring(5);
  //    dlg.InitialDir := ExtractFilePath(fn);
  //    dlg.FileName := ChangeFileExt(ExtractFileName(fn), '.'+suggestedExtension);
  //  end
  //  else
  //  begin
  //    dlg.InitialDir := FIni.ReadString('file-store', 'folder', '');
  //    dlg.FileName := 'filename.'+suggestedExtension;
  //  end;
  //  result := dlg.Execute;
  //  if result then
  //  begin
  //    newName := 'file:'+dlg.filename;
  //    FIni.WriteString('file-store', 'folder', ExtractFilePath(dlg.fileName));
  //  end;
  //finally
  //  dlg.free;
  //end;
end;

function TServerStorageService.MakeFilename(address: String): String;
begin
  raise EFHIRException.create('Not done yet');
//  result := ExtractFileName(address.Substring(5));
end;

function TServerStorageService.clientForAddress(address: String): TFHIRClientV;
var
  server : TFHIRServerEntry;
  rtype, id : String;
begin
  resolve(address, server, rtype, id);
  if server.client = nil then
    OnConnectToServer(self, server);
  result := server.client;
end;

procedure TServerStorageService.forceLocation(address: String);
begin
  // nothing
end;

function TServerStorageService.getName(address: String; mode: TNameMode): String;
begin
  case mode of
    nameModeFullPath : result := address;
    nameModeFolder : result := address.Substring(0, address.LastIndexOf('/'));
    nameModeName : result := address.Substring(address.LastIndexOf('/')+1);
  else result := '';
  end;
end;

end.
