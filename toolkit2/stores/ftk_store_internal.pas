unit ftk_store_internal;

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
  fn := FilePath([folder, address.Substring(9)]);
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
  fn := FilePath([folder, address.Substring(9)]);
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
  fn := FilePath([folder, address.Substring(9)]);
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
    nameModeFullPath : result := FilePath([folder, address.Substring(9)]);
    nameModeFolder : result := folder;
    nameModeName : result := address.Substring(9);
  else result := '';
  end;
end;

end.

