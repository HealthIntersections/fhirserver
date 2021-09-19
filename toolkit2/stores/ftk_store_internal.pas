unit ftk_store_internal;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_utilities,
  ftk_store;

type  
  { TInternalStorageService }
  TInternalStorageService = class (TStorageService)
  private
    function folder : String;
  public
    function schemes : TArray<String>; override;
    function CheckTimes : boolean; override;
    function CurrencyCheckFrequency : integer; override;
    function load(address : String; doException : boolean) : TLoadedBytes; override;
    function save(address : String; bytes : TBytes) : TDateTime; override;
    function CaptionForAddress(address : String) : String; override;
    function describe(address : String) : String; override;
    procedure delete(address : String); override;
    function openDlg(out newName : String) : boolean; override;
    function saveDlg(existing : String; suggestedExtension : String; out newName : String) : boolean; override;
    function MakeFilename(address : String) : String; override;
    procedure forceLocation(address : String); override;
    function getName(address : String; mode : TNameMode) : String; override;
  end;

implementation

{ TInternalStorageService }

function TInternalStorageService.folder: String;
begin
  result := ExtractFileDir(FIni.FileName);
end;

function TInternalStorageService.schemes : TArray<String>;
begin
  result := ['internal'];
end;

function TInternalStorageService.CheckTimes: boolean;
begin
  result := false;
end;

function TInternalStorageService.CurrencyCheckFrequency: integer;
begin
  result := 1;
end;

function TInternalStorageService.load(address: String; doException : boolean): TLoadedBytes;
var
  fn : String;
begin
  fn := path([folder, address.Substring(9)]);
  if (FileExists(fn)) then
  begin
    result.content := FileToBytes(fn);
    result.timestamp := FileGetModified(fn);
  end
  else
    result.timestamp := 0;
end;

function TInternalStorageService.save(address: String; bytes: TBytes): TDateTime;
var
  fn : String;
begin
  fn := path([folder, address.Substring(9)]);
  BytesToFile(bytes, fn);
  result := FileGetModified(fn);
end;

function TInternalStorageService.CaptionForAddress(address: String): String;
begin
  result := 'todo'; // we don't want to get here?
end;

function TInternalStorageService.describe(address: String): String;
begin
  result := 'internal file';
end;

procedure TInternalStorageService.delete(address: String);
var
  fn : String;
begin
  fn := path([folder, address.Substring(9)]);
  deleteFile(fn);
end;

function TInternalStorageService.openDlg(out newName: String): boolean;
begin
  abort; // we don't want to get here
end;

function TInternalStorageService.saveDlg(existing: String; suggestedExtension: String; out newName: String): boolean;
begin
  abort; // we don't want to get here
end;

function TInternalStorageService.MakeFilename(address: String): String;
begin
  abort; // we don't want to get here?
end;

procedure TInternalStorageService.forceLocation(address: String);
begin
  // nothing
end;

function TInternalStorageService.getName(address: String; mode: TNameMode): String;
begin
  case mode of
    nameModeFullPath : result := path([folder, address.Substring(9)]);
    nameModeFolder : result := folder;
    nameModeName : result := address.Substring(9);
  else result := '';
  end;
end;

end.

