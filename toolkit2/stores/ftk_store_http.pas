unit ftk_store_http;

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
  IdUri,
  fsl_fetcher,
  fhir_client,
  ftk_store;

{
 this implementation share addresses with the server store.
 that will be used wherever there's a matching registered server for an http:
 else it falls back to this which is read-only
}

type
  { THTTPStorageService }

  THTTPStorageService = class (TStorageService)
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
    function clientForAddress(address : String) : TFHIRClientV; override;
    procedure forceLocation(address : String); override;
    function getName(address : String; mode : TNameMode) : String; override;
  end;

implementation

{ THTTPStorageService }

function THTTPStorageService.schemes: TArray<String>;
begin
  result := ['http', 'https', 'ftp'];
end;

function THTTPStorageService.inScope(url: String): boolean;
begin
  result := false;
end;

function THTTPStorageService.CheckTimes: boolean;
begin
  result := true;
end;

function THTTPStorageService.CurrencyCheckFrequency: integer;
begin
  result := 5 * 60;
end;

function THTTPStorageService.canSave: boolean;
begin
  result := false;
end;

function THTTPStorageService.load(address: String; doException: boolean): TLoadedBytes;
var
  fetcher : TInternetFetcher;
begin
  fetcher := TInternetFetcher.create;
  try
    fetcher.URL := address;
    fetcher.Fetch;
    result.content := fetcher.Buffer.AsBytes;
    result.mimeType := fetcher.ContentType;
    result.timestamp := fetcher.LastModified;
  finally
    fetcher.free;
  end;
end;

function THTTPStorageService.save(address: String; bytes: TBytes): TDateTime;
begin
  raise EFHIRException.create('This store cannot save');
end;

function THTTPStorageService.CaptionForAddress(address: String): String;
var
  uri : TIdUri;
begin
  uri := TIdURI.create(address);
  try
    result := uri.document;
  finally
    uri.free;
  end;
end;

function THTTPStorageService.describe(address: String): String;
begin
  result := CaptionForAddress((address));
end;

procedure THTTPStorageService.delete(address: String);
begin
  raise EFHIRException.create('This store cannot delete');
end;

function THTTPStorageService.openDlg(out newName: String): boolean;
begin
  raise EFHIRException.create('Not supported by this store');
end;

function THTTPStorageService.saveDlg(existing: String; suggestedExtension: String; out newName: String): boolean;
begin
  raise EFHIRException.create('Not supported by this store');
end;

function THTTPStorageService.MakeFilename(address: String): String;
begin
  result := CaptionForAddress(address);
end;

function THTTPStorageService.clientForAddress(address: String): TFHIRClientV;
begin
  raise EFHIRException.create('Not supported by this store');
end;

procedure THTTPStorageService.forceLocation(address: String);
begin
  raise EFHIRException.create('Not supported by this store');
end;

function THTTPStorageService.getName(address: String; mode: TNameMode): String;
begin
  raise EFHIRException.create('Not supported by this store');
end;

end.

