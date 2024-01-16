unit web_source;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$I fhir.inc}

interface

uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_stream, fsl_logging;

type
  TFHIRWebServerSourceProvider = class abstract (TFslObject)
  public
    function link : TFHIRWebServerSourceProvider; overload;
    function AltFile(path, base: String): String;
    function getSource(filename : String) : String; virtual; abstract;
    function exists(filename : String) : boolean; virtual; abstract;
    function asStream(filename : String) : TStream; virtual; abstract;
    function asBytes(filename : String) : TBytes; virtual; abstract;
  end;

  TFHIRWebServerSourceFolderProvider = class (TFHIRWebServerSourceProvider)
  private
    FSourcePath : String; // where to find web content
  public
    constructor Create(path : String);
//    Property SourcePath : String read FSourcePath;
    function getSource(filename : String) : String; override;
    function exists(filename : String) : boolean; override;
    function asStream(filename : String) : TStream; override;
    function asBytes(filename : String) : TBytes; override;
  end;

  TFHIRWebServerSourceZipProvider = class (TFHIRWebServerSourceProvider)
  private
    FZip : TFslMap<TFslBuffer>; // where to find web content
  public
    constructor Create(path : String);
    destructor Destroy; override;
//    Property SourcePath : String read FSourcePath;
    function getSource(filename : String) : String; override;
    function exists(filename : String) : boolean; override;
    function asStream(filename : String) : TStream; override;
    function asBytes(filename : String) : TBytes; override;
  end;

implementation

{ TFHIRWebServerSourceProvider }

function TFHIRWebServerSourceProvider.AltFile(path, base : String) : String;
begin
  if path.StartsWith(base) then
    path := path.Substring(base.Length);

  if path.StartsWith('/') then
    result := path.Substring(1).Replace('/', '\')
  else
    result := path;
end;

function TFHIRWebServerSourceProvider.link: TFHIRWebServerSourceProvider;
begin
  result := TFHIRWebServerSourceProvider(inherited Link);
end;

{ TFHIRWebServerSourceFolderProvider }

function TFHIRWebServerSourceFolderProvider.asBytes(filename: String): TBytes;
var
  fn : String;
begin
  fn := FilePath([FSourcePath, filename]);
  result := FileToBytes(fn);
end;

function TFHIRWebServerSourceFolderProvider.asStream(filename: String): TStream;
var
  fn : String;
begin
  fn := FilePath([FSourcePath, filename]);
  result := TFileStream.Create(fn, fmOpenRead + fmShareDenyWrite);
end;

constructor TFHIRWebServerSourceFolderProvider.Create(path: String);
begin
  inherited Create;
  FSourcePath := path;
end;

function TFHIRWebServerSourceFolderProvider.exists(filename: String): boolean;
var
  fn : String;
begin
  fn := FilePath([FSourcePath, filename]);
  result := FileExists(fn);
end;

function TFHIRWebServerSourceFolderProvider.getSource(filename: String): String;
var
  fn : String;
begin
  if FileExists(filename) then
    fn := filename
  else
    fn := FilePath([FSourcePath, filename]);
  result := fsl_stream.FileToString(fn, TEncoding.UTF8);
end;

{ TFHIRWebServerSourceZipProvider }

function TFHIRWebServerSourceZipProvider.asBytes(filename: String): TBytes;
var
  src : TFslBuffer;
begin
  if not FZip.TryGetValue(filename.replace('\', '/'), src) then
    raise EIOException.Create('Unable to find '+filename+ ' in archive');
  result := src.AsBytes;
end;

function TFHIRWebServerSourceZipProvider.asStream(filename: String): TStream;
var
  src : TFslBuffer;
begin
  if not FZip.TryGetValue(filename.replace('\', '/'), src) then
    raise EIOException.Create('Unable to find '+filename+ ' in archive');
  result := TMemoryStream.Create;
  src.SaveToStream(result);
  result.Position := 0;
end;

constructor TFHIRWebServerSourceZipProvider.Create(path: String);
var
  zip : TFslZipReader;
  i : integer;
begin
  inherited Create;
  FZip := TFslMap<TFslBuffer>.Create('web.source');
  zip := TFslZipReader.Create;
  try
    zip.Stream := TFslFile.Create(path, fmOpenRead + fmShareDenyWrite);
    zip.ReadZip;
    Logging.log(inttostr(zip.Parts.Count)+' files loaded');
    for i := 0 to zip.Parts.Count - 1 do
      FZip.Add(zip.Parts[i].Name, zip.Parts[i].Link);
  finally
    zip.free;
  end;
end;

destructor TFHIRWebServerSourceZipProvider.Destroy;
begin
  FZip.free;
  inherited;
end;

function TFHIRWebServerSourceZipProvider.exists(filename: String): boolean;
begin
  result := (filename <> '') and FZip.ContainsKey(filename.replace('\', '/'));
end;

function TFHIRWebServerSourceZipProvider.getSource(filename: String): String;
var
  src : TFslBuffer;
begin
  if not FZip.TryGetValue(filename.replace('\', '/'), src) then
    raise EIOException.Create('Unable to find '+filename+ ' in archive');
  result := src.AsText;
end;

end.
