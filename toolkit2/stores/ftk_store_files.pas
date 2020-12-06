unit ftk_store_files;

{$i fhir.inc}

interface

uses
  SysUtils, Classes, Dialogs,
  fsl_utilities, fsl_stream,
  ftk_store;

type

  { TFileStorageService }

  TFileStorageService = class (TStorageService)
  public
    function scheme : String; override;
    function CheckTimes : boolean; override;
    function load(address : String) : TLoadedBytes; override;
    function save(address : String; bytes : TBytes) : TDateTime; override;
    function CaptionForAddress(address : String) : String; override;
    function describe(address : String) : String; override;
    procedure delete(address : String); override;
    function openDlg(out newName : String) : boolean; override;
    function saveDlg(existing : String; suggestedExtension : String; out newName : String) : boolean; override;
    function MakeFilename(address : String) : String; override;
  end;

implementation

{ TFileStorageService }

function TFileStorageService.scheme: String;
begin
  result := 'file';
end;

function TFileStorageService.CheckTimes: boolean;
begin
  result := true;
end;

function TFileStorageService.load(address: String): TLoadedBytes;
begin
  if not address.startsWith('file:') then raise Exception.create('This is not a file address');
  result.content := FileToBytes(address.substring(5));
  result.timestamp := FileGetModified(address.substring(5));
end;

function TFileStorageService.save(address: String; bytes: TBytes) : TDateTime;
begin
  if not address.startsWith('file:') then raise Exception.create('This is not a file address');
  BytesToFile(bytes, address.Substring(5));
  result := FileGetModified(address.substring(5));
end;

function TFileStorageService.CaptionForAddress(address: String): String;
begin
  if not address.startsWith('file:') then raise Exception.create('This is not a file address');
  result := ExtractFileName(address.Substring(5));
end;

function TFileStorageService.describe(address: String): String;
begin
  if not address.startsWith('file:') then raise Exception.create('This is not a file address');
  result := 'File '+address.Substring(5);
end;

procedure TFileStorageService.delete(address: String);
begin
  if not address.startsWith('file:') then raise Exception.create('This is not a file address');
  FileDelete(address.Substring(5));
end;

function TFileStorageService.openDlg(out newName : String) : boolean;
var
  dlg : TOpenDialog;
begin
  dlg := TOpenDialog.create(handle);
  try
    dlg.Filter := 'All Known Files|*.xml; *.json; *.ini; *.txt; *.v2; *.msg; *.hl7; *.template; *.liquid; *.js; *.md; *.htm; *.html|'+
      'XML|*.xml|'+
      'JSON|*.json|'+
      'Ini|*.ini|'+
      'V2 Messages|*.v2; *.msg; *.hl7|'+
      'Liquid Templates|*.template; *.liquid|'+
      'Javascript|*.js|'+
      'Markdown|*.md|'+
      'HTML|*.htm; *.html|'+
      'Text|*.txt|'+
      'All Fles|*.*';
    dlg.Options := [ofFileMustExist, ofEnableSizing, ofViewDetail];
    dlg.InitialDir := FIni.ReadString('file-store', 'folder', dlg.InitialDir);
    result := dlg.execute;
    if result then
    begin
      newName := 'file:'+dlg.FileName;
      FIni.WriteString('file-store', 'folder', ExtractFilePath(newName));
    end;
  finally
    dlg.free;
  end;
end;

function TFileStorageService.saveDlg(existing : String; suggestedExtension : String; out newName : String) : boolean;
var
  dlg : TSaveDialog;
  fn : String;
begin
  dlg := TSaveDialog.create(Handle);
  try
    dlg.Options := [ofOverwritePrompt, ofEnableSizing, ofViewDetail];
    if (existing <> '') and existing.startsWith('file:') then
    begin
      fn := existing.Substring(5);
      dlg.InitialDir := ExtractFilePath(fn);
      dlg.FileName := ChangeFileExt(ExtractFileName(fn), '.'+suggestedExtension);
    end
    else
    begin
      dlg.InitialDir := FIni.ReadString('file-store', 'folder', '');
      dlg.FileName := 'filename.'+suggestedExtension;
    end;
    result := dlg.Execute;
    if result then
    begin
      newName := 'file:'+dlg.filename;
      FIni.WriteString('file-store', 'folder', ExtractFilePath(dlg.fileName));
    end;
  finally
    dlg.free;
  end;
end;

function TFileStorageService.MakeFilename(address: String): String;
begin
  result := ExtractFileName(address.Substring(5));
end;

end.
