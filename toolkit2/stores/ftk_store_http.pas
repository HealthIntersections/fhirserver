unit ftk_store_http;

{$i fhir.inc}

interface

uses
  SysUtils, Classes, Dialogs,
  fsl_base, fsl_utilities, fsl_stream,
  fhir_objects, fhir_client,
  ftk_store, ftk_utilities, ftk_context;

type

  { THTTPStorageService }
  THTTPStorageService = class (TStorageService)
  private
    FOnConnectToServer: TConnectToServerEvent;
    FServerList : TFslList<TFHIRServerEntry>;
    procedure resolve(url : String; var server : TFHIRServerEntry; var rtype, id : String);
  public
    constructor Create(serverList : TFslList<TFHIRServerEntry>);
    destructor Destroy; override;

    function schemes : TArray<String>; override;
    function CheckTimes : boolean; override;
    function load(address : String) : TLoadedBytes; override;
    function save(address : String; bytes : TBytes) : TDateTime; override;
    function CaptionForAddress(address : String) : String; override;
    function describe(address : String) : String; override;
    procedure delete(address : String); override;
    function openDlg(out newName : String) : boolean; override;
    function saveDlg(existing : String; suggestedExtension : String; out newName : String) : boolean; override;
    function MakeFilename(address : String) : String; override;
    function clientForAddress(address : String) : TFHIRClientV; override;

    property OnConnectToServer : TConnectToServerEvent read FOnConnectToServer write FOnConnectToServer;
  end;

implementation

{ THTTPStorageService }

procedure THTTPStorageService.resolve(url: String; var server : TFHIRServerEntry; var rtype, id: String);
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

constructor THTTPStorageService.Create(serverList: TFslList<TFHIRServerEntry>);
begin
  inherited create;
  FServerList := serverList;
end;

destructor THTTPStorageService.Destroy;
begin
  FServerList.free;
  inherited Destroy;
end;

function THTTPStorageService.schemes : TArray<String>;
begin
  result := ['http', 'https'];
end;

function THTTPStorageService.CheckTimes: boolean;
begin
  result := true;
end;

function THTTPStorageService.load(address: String): TLoadedBytes;
var
  server : TFHIRServerEntry;
  rtype, id : String;
  buffer : TFslHTTPBuffer;
  headers : THTTPHeaders;
begin
  resolve(address, server, rtype, id);
  buffer := server.client.customGet(rtype+'/'+id, headers);
  try
    result.content := buffer.AsBytes;
    result.timestamp := buffer.timestamp;
    result.mimeType := buffer.mimeType;
  finally
    buffer.free;
  end;
end;

function THTTPStorageService.save(address: String; bytes: TBytes) : TDateTime;
begin
  raise EFHIRException.create('Not done yet');
end;

function THTTPStorageService.CaptionForAddress(address: String): String;
var
  server : TFHIRServerEntry;
  rtype, id : String;
begin
  resolve(address, server, rtype, id);
  result := rtype+'/'+id;
end;

function THTTPStorageService.describe(address: String): String;
var
  server : TFHIRServerEntry;
  rtype, id : String;
begin
  resolve(address, server, rtype, id);
  result := server.name+':'+rtype+'/'+id;
end;

procedure THTTPStorageService.delete(address: String);
begin
  raise EFHIRException.create('Not done yet');
end;

function THTTPStorageService.openDlg(out newName : String) : boolean;
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

function THTTPStorageService.saveDlg(existing : String; suggestedExtension : String; out newName : String) : boolean;
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

function THTTPStorageService.MakeFilename(address: String): String;
begin
  raise EFHIRException.create('Not done yet');
//  result := ExtractFileName(address.Substring(5));
end;

function THTTPStorageService.clientForAddress(address: String): TFHIRClientV;
var
  server : TFHIRServerEntry;
  rtype, id : String;
begin
  resolve(address, server, rtype, id);
  if server.client = nil then
    OnConnectToServer(self, server);
  result := server.client;
end;

end.
