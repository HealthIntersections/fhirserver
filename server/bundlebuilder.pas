unit bundlebuilder;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_stream, fsl_http,
  fhir_objects, fhir_common, fhir_factory, fhir_parser;

type
  TFHIRBundleBuilder = class (TFslObject)
  private
    FHasSecureOp: boolean;
  protected
    FBundle : TFHIRBundleW;
    FFactory : TFHIRFactory;
  public
    constructor Create(factory : TFHIRFactory; bundle : TFHIRBundleW);
    destructor Destroy; override;

    Property hasSecureOp : boolean read FHasSecureOp write FHasSecureOp;

    procedure setId(id : string);
    procedure setLastUpdated(dt : TFslDateTime);
    procedure setTotal(t : integer);
    procedure tag(n, v : String);
    procedure addLink(rt, url : String);
    procedure addEntry(entry : TFhirBundleEntryW; first : boolean); virtual; abstract;
    function moveToFirst(res : TFhirResourceV) : TFhirBundleEntryW; virtual; abstract;
    function getBundle : TFHIRResourceV; virtual; abstract;
    function makeEntry : TFhirBundleEntryW; virtual;
  end;

  TFHIRBundleBuilderSimple = class (TFHIRBundleBuilder)
  public
    procedure addEntry(entry : TFhirBundleEntryW; first : boolean); override;
    function moveToFirst(res : TFhirResourceV) : TFhirBundleEntryW; override;
    function getBundle : TFHIRResourceV; override;
  end;

  TFHIRBundleBuilderNDJson = class (TFHIRBundleBuilder)
  private
    FFileBase : String;
    FFiles : TFslMap<TFslFile>;
    function fileForType(rType : String) : TFslFile;
    procedure writeResource(res : TFHIRResourceV); overload;
    procedure writeResource(rType : String; cnt : TFslBuffer); overload;
  public
    constructor Create(factory : TFHIRFactory; bundle : TFHIRBundleW; fileBase : String; files : TFslMap<TFslFile>);
    destructor Destroy; override;

    procedure addEntry(entry : TFhirBundleEntryW; first : boolean); override;
    function moveToFirst(res : TFhirResourceV) : TFhirBundleEntryW; override;
    function getBundle : TFHIRResourceV; override;
  end;

implementation


{ TFHIRBundleBuilder }

constructor TFHIRBundleBuilder.Create(factory : TFHIRFactory; bundle : TFHIRBundleW);
begin
  inherited Create;
  FBundle := bundle;
  FFactory := factory;
end;

destructor TFHIRBundleBuilder.Destroy;
begin
  FBundle.Free;
  FFactory.Free;
  inherited;
end;

procedure TFHIRBundleBuilder.addLink(rt, url: String);
begin
  FBundle.links[rt] := url;
end;

procedure TFHIRBundleBuilder.setId(id: string);
begin
  FBundle.id := id;
end;

procedure TFHIRBundleBuilder.setLastUpdated(dt: TFslDateTime);
begin
  FBundle.lastUpdated := dt;
end;

procedure TFHIRBundleBuilder.setTotal(t: integer);
begin
  FBundle.total := t;
end;

procedure TFHIRBundleBuilder.tag(n, v: String);
begin
  FBundle.Tags[n] := v;
end;

function TFHIRBundleBuilder.makeEntry: TFhirBundleEntryW;
begin
  result := Ffactory.wrapBundleEntry(Ffactory.makeByName('Bundle.entry'));
end;

{ TFHIRBundleBuilderSimple }

procedure TFHIRBundleBuilderSimple.addEntry(entry: TFhirBundleEntryW; first : boolean);
begin
  FBundle.addEntry(entry, first);
end;


function TFHIRBundleBuilderSimple.getBundle: TFHIRResourceV;
begin
  result := FBundle.Resource.Link;
end;

function TFHIRBundleBuilderSimple.moveToFirst(res: TFhirResourceV): TFhirBundleEntryW;
begin
  result := FBundle.MoveToFirst(res);
end;

{ TFHIRBundleBuilderNDJson }

constructor TFHIRBundleBuilderNDJson.Create(factory : TFHIRFactory; bundle: TFHIRBundleW; fileBase: String; files : TFslMap<TFslFile>);
begin
  inherited Create(factory, bundle);
  FFileBase := fileBase;
  FFiles := files;
end;

destructor TFHIRBundleBuilderNDJson.Destroy;
begin
  writeResource(FBundle.Resource);
  Ffiles.Free;
  inherited;
end;

function TFHIRBundleBuilderNDJson.fileForType(rType: String): TFslFile;
begin
  if not FFiles.TryGetValue(rType, result) then
  begin
    result := TFslFile.Create(FFileBase+'-'+rType+'.ndjson', fmCreate);
    FFiles.Add(rType, result);
  end;
end;

procedure TFHIRBundleBuilderNDJson.addEntry(entry: TFhirBundleEntryW; first : boolean);
begin
  if (entry.resource.Tag <> nil) and (entry.resource.Tag is TFslBuffer) then
    writeResource(entry.Tags['type'], entry.Tag as TFslBuffer)
  else if entry.resource <> nil then
    writeResource(entry.resource);
  entry.Tag := nil;
  entry.resource := nil;
  FBundle.addEntry(entry, first);
end;

function TFHIRBundleBuilderNDJson.getBundle: TFHIRResourceV;
begin
  result := nil; // although we have bundle internally, we don't return it directly (only use ND-JSON is asymc mode, and return a list of files)
end;

function TFHIRBundleBuilderNDJson.moveToFirst(res: TFhirResourceV): TFhirBundleEntryW;
begin
  raise EFHIRTodo.create('TFHIRBundleBuilderNDJson.moveToFirst');
end;


procedure TFHIRBundleBuilderNDJson.writeResource(res: TFHIRResourceV);
var
  f : TFslFile;
  json : TFHIRComposer;
  b : ansichar;
begin
  f := fileForType(res.fhirType);
  if f.Size > 0 then
  begin
    b := #10;
    f.Write(b, 1);
  end;
  json := FFactory.makeComposer(nil, ffJson, THTTPLanguages.create('en'), OutputStyleNormal);
  try
    json.Compose(f, res);
  finally
    json.Free;
  end;
end;

procedure TFHIRBundleBuilderNDJson.writeResource(rType: String; cnt: TFslBuffer);
var
  f : TFslFile;
  b : ansiChar;
begin
  f := fileForType(rType);
  if f.Size > 0 then
  begin
    b := #10;
    f.Write(b, 1);
  end;
  cnt.SaveToStream(f);
end;


end.
