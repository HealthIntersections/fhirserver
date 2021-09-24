unit ftk_store;

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
  SysUtils, Classes, IniFiles,
  fsl_base,
  fhir_client;

type

  TLoadedBytes = record
    content : TBytes;
    timestamp : TDateTime;
    mimeType : String;
  end;

  TNameMode = (nameModeFullPath, nameModeFolder, nameModeName);

  { TStorageService }

   TStorageService = class abstract (TFslObject)
   private
     FHandle : TComponent;
   protected
     FIni : TIniFile;
   public
     constructor create(handle : TComponent; ini : TIniFile);

     function link : TStorageService; overload;

     property Handle : TComponent read FHandle;

     function schemes : TArray<String>; virtual; abstract;
     function CheckTimes : boolean; virtual; abstract;
     function CurrencyCheckFrequency : integer; virtual; abstract; // number of seconds
     procedure forceLocation(address : String); virtual; abstract;
     function openDlg(out newName : String) : boolean; virtual; abstract;
     function saveDlg(existing : String; suggestedExtension : String; out newName : String) : boolean; virtual; abstract;

     function CaptionForAddress(address : String) : String; virtual; abstract;
     function describe(address : String) : String; virtual; abstract;
     function MakeFilename(address : String) : String; virtual; abstract;
     function clientForAddress(address : String) : TFHIRClientV; virtual;
     function getName(address : String; mode : TNameMode) : String; virtual; abstract;

     function load(address : String; doException : boolean) : TLoadedBytes; virtual; abstract;
     function save(address : String; bytes : TBytes) : TDateTime; virtual; abstract;
     procedure delete(address : String); virtual; abstract;
   end;


implementation

{ TStorageService }

constructor TStorageService.create(handle: TComponent; ini : TIniFile);
begin
  inherited create;
  FHandle := handle;
  FIni := ini;
end;

function TStorageService.link: TStorageService;
begin
  result := TStorageService(inherited link);
end;

function TStorageService.clientForAddress(address: String): TFHIRClientV;
begin
  result := nil;
end;

end.
