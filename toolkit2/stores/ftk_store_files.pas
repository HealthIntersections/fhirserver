unit ftk_store_files;

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
  ftk_store;

type

  { TFileStorageService }

  TFileStorageService = class (TStorageService)
  public
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
    procedure forceLocation(address : String); override;
    function getName(address : String; mode : TNameMode) : String; override;
  end;

implementation

{ TFileStorageService }

function TFileStorageService.schemes : TArray<String>;
begin
  result := ['file'];
end;

function TFileStorageService.inScope(url: String): boolean;
begin
  result := true;
end;

function TFileStorageService.CheckTimes: boolean;
begin
  result := true;
end;

function TFileStorageService.CurrencyCheckFrequency: integer;
begin
  result := 1;
end;

function TFileStorageService.canSave: boolean;
begin
  result := true;
end;

function TFileStorageService.load(address: String; doException : boolean): TLoadedBytes;
var
  fn : String;
begin
  if not address.startsWith('file:') then
    raise ELibraryException.Create('This is not a file address');

  fn := address.substring(5);
  result.timestamp := 0;
  if not FileExists(fn) then
  begin
    if doException then
      raise ELibraryException.Create('The file '+fn+' does not exist')
    else
      result.timestamp := -1;
  end
  else
  begin
    try
      result.timestamp := FileGetModified(fn);
      result.content := FileToBytes(fn);
    except
      on e : Exception do
      begin
        if doException then
          raise
        else if result.timestamp = 0 then // a network drive?
          result.timestamp := 0;
      end;
    end;
  end;
end;

function TFileStorageService.save(address: String; bytes: TBytes) : TDateTime;
begin
  if not address.startsWith('file:') then raise EFslException.Create('This is not a file address');
  BytesToFile(bytes, address.Substring(5));
  result := FileGetModified(address.substring(5));
end;

function TFileStorageService.CaptionForAddress(address: String): String;
begin
  if not address.startsWith('file:') then raise EFslException.Create('This is not a file address');
  result := ExtractFileName(address.Substring(5));
end;

function TFileStorageService.describe(address: String): String;
begin
  if not address.startsWith('file:') then raise EFslException.Create('This is not a file address');
  result := 'File '+address.Substring(5);
end;

procedure TFileStorageService.delete(address: String);
begin
  if not address.startsWith('file:') then raise EFslException.Create('This is not a file address');
  FileDelete(address.Substring(5));
end;

function TFileStorageService.openDlg(out newName : String) : boolean;
var
  dlg : TOpenDialog;
begin
  dlg := TOpenDialog.Create(handle);
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
      'All Files|*.*';
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
  dlg := TSaveDialog.Create(Handle);
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

procedure TFileStorageService.forceLocation(address: String);
var
  dir : String;
begin
  if not address.startsWith('file:') then raise EFslException.Create('This is not a file address');
  dir := ExtractFileDir(address.Substring(5));
  ForceFolder(dir);
end;

function TFileStorageService.getName(address: String; mode: TNameMode): String;
var
  fn : string;
begin
  if not address.startsWith('file:') then raise EFslException.Create('This is not a file address');
  fn := address.Substring(5);
  case mode of
    nameModeFullPath : result := fn;
    nameModeFolder : result := ExtractFileDir(fn);
    nameModeName : result := ExtractFileName(fn);
    else result := '';
  end;
end;

end.
