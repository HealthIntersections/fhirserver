unit ftk_terminology_service;

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
  fsl_base, fsl_utilities,
  fhir_objects, fhir_client,
  fhir4_factory, fhir4_resources, fhir4_utilities;

type

  { TToolkitTerminologyService }

  TToolkitTerminologyService = class (TFHIRTerminologyService)
  private
    FClient : TFHIRClientV;
    FCacheFile : String;
    FCache : TFslStringDictionary;
    FNextSave : TDateTime;

    procedure loadCache;
    procedure saveCache;
    procedure checkSaveCache;
  public
    constructor create(url, log, cache : String);
    destructor Destroy; override;

    function lookupCode(system_, code : String) : String; override;
  end;

implementation

{ TToolkitTerminologyService }

procedure TToolkitTerminologyService.loadCache;
var
  f : System.text;
  s, n, v : String;
begin
  if FileExists(FCacheFile) then
    try
      AssignFile(f, FCacheFile);
      reset(f);
      while not eof(f) do
      begin
        readln(f, s);
        StringSplit(s, '=', n, v);
        FCache.AddOrSetValue(n.trim, v.trim);
      end;
      closeFile(f);
    except
    end;
end;

procedure TToolkitTerminologyService.saveCache;
var
  f : System.text;
  s : String;
begin
  AssignFile(f, FCacheFile);
  rewrite(f);
  for s in FCache.Keys do
    writeln(f, s+' = '+FCache[s]);
  closeFile(f);
  FNextSave := now + 60 * DATETIME_SECOND_ONE;
end;

procedure TToolkitTerminologyService.checkSaveCache;
begin
  if FNextSave < now then
    saveCache;
end;

constructor TToolkitTerminologyService.create(url, log, cache: String);
var
  factory : TFHIRFactoryR4;
begin
  inherited create;
  FCache := TFslStringDictionary.create;
  FCacheFile := cache;
  loadCache;

  factory := TFHIRFactoryR4.create;
  try
    FClient := factory.makeClient(nil, url, fctCrossPlatform, ffJson, 5000);
    if (log <> '') then
      FClient.Logger := TTextFileLogger.create(log);
  finally
    factory.free;
  end;
end;

destructor TToolkitTerminologyService.Destroy;
begin
  saveCache;
  FCache.Free;
  FClient.Free;
  inherited Destroy;
end;

function TToolkitTerminologyService.lookupCode(system_, code: String): String;
var
  pin, pout : TFhirParameters;
begin
  if (FCache.containsKey(system_+'|'+code)) then
    exit(FCache[system_+'|'+code]);

  pin := TFhirParameters.create;
  try
    pin.AddParameter('url', system_);
    pin.AddParameter('code', code);
    pout := FClient.operationV('CodeSystem', 'validate-code', pin) as TFhirParameters;
    try
      result := pout.str['display'];
      FCache.AddOrSetValue(system_+'|'+code, result);
      checkSaveCache;
    finally
      pout.free;
    end;
  finally
    pin.free;
  end;
end;

end.

