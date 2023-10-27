unit fhir4b_opbase;

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

{$I fhir.inc}
{$I fhir4b.inc}

interface

uses
  SysUtils,
  fsl_base, fsl_http,
  fhir4b_types, fhir4b_resources;
type

  TFHIROpExtension = class (TFslObject)
  private
    FName : String;
    FValue : TFHIRDataType;
    procedure SetValue(const Value: TFHIRDataType);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    destructor Destroy; override;

    property name : String read FName write FName;
    property value : TFHIRDataType read FValue write SetValue;
  end;

  TFHIROperationBaseObject = class (TFslObject)
  private
    FExtensions : TFslList<TFHIROpExtension>;
    function GetExtensions: TFslList<TFHIROpExtension>;
  protected
    function isKnownName(name : String) : boolean; overload; virtual;
    procedure loadExtensions(params : TFHIRParameters); overload;
    procedure loadExtensions(params : THTTPParameters); overload;
    procedure loadExtensions(params : TFhirParametersParameter); overload;
    procedure writeExtensions(params : TFHIRParameters); overload;
    procedure writeExtensions(params : TFhirParametersParameter); overload;
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    destructor Destroy; override;
    property extensions : TFslList<TFHIROpExtension> read GetExtensions;
    procedure addExtension(name : String; value : TFHIRDataType); overload;
    procedure addExtension(name : String; value : string); overload;
    procedure addExtension(name : String; value : boolean); overload;
  end;

  TFHIROperationObject = class (TFHIROperationBaseObject)
  public
    constructor Create(); overload; override;
    constructor Create(params : TFhirParametersParameter); overload; virtual;
    function asParams(name : String) : TFHIRParametersParameter; virtual;
  end;

  TFHIROperationRequest = class (TFHIROperationBaseObject)
  public
    constructor Create(); overload; override;
    procedure load(params : TFHIRParameters); overload; virtual; abstract;
    procedure load(params : THTTPParameters); overload; virtual; abstract;
    function asParams : TFHIRParameters; virtual; abstract;
  end;

  TFHIROperationResponse = class (TFHIROperationBaseObject)
  public
    constructor Create(); overload; override;
    procedure load(params : TFHIRParameters); overload; virtual; abstract;
    procedure load(params : THTTPParameters); overload; virtual; abstract;
    function asParams : TFHIRParameters; virtual; abstract;
  end;


implementation

{ TFHIROperationRequest }

constructor TFHIROperationRequest.Create;
begin
  inherited Create;
end;


{ TFHIROperationResponse }

constructor TFHIROperationResponse.Create;
begin
  inherited Create;
end;


{ TFHIROperationObject }

function TFHIROperationObject.asParams(name : String): TFHIRParametersParameter;
begin
  result := nil;
end;

constructor TFHIROperationObject.Create;
begin
  inherited;

end;

constructor TFHIROperationObject.Create(params: TFhirParametersParameter);
begin
  inherited Create;
end;


{ TFHIROperationBaseObject }

procedure TFHIROperationBaseObject.addExtension(name: String; value: TFHIRDataType);
var
  ext : TFHIROpExtension;
begin
  ext := TFHIROpExtension.Create;
  ext.name := name;
  ext.value := value;
  GetExtensions.Add(ext)
end;

procedure TFHIROperationBaseObject.addExtension(name, value: string);
var
  ext : TFHIROpExtension;
begin
  ext := TFHIROpExtension.Create;
  ext.name := name;
  ext.value := TFhirString.Create(value);
  GetExtensions.Add(ext)
end;

procedure TFHIROperationBaseObject.addExtension(name: String; value: boolean);
var
  ext : TFHIROpExtension;
begin
  ext := TFHIROpExtension.Create;
  ext.name := name;
  ext.value := TFhirBoolean.Create(value);
  GetExtensions.Add(ext)
end;

destructor TFHIROperationBaseObject.Destroy;
begin
  FExtensions.free;
  inherited;
end;

function TFHIROperationBaseObject.GetExtensions: TFslList<TFHIROpExtension>;
begin
  if FExtensions = nil then
    FExtensions := TFslList<TFHIROpExtension>.Create;
  result := FExtensions;
end;

function TFHIROperationBaseObject.IsKnownName(name: String): boolean;
begin
  result := false;
end;

procedure TFHIROperationBaseObject.loadExtensions(params: TFhirParametersParameter);
var
  p : TFhirParametersParameter;
begin
  for p in params.partList do
    if not IsKnownName(p.name) then
      addExtension(p.name, p.value.Link);
end;

procedure TFHIROperationBaseObject.loadExtensions(params: THTTPParameters);
var
  i : integer;
begin
  for i := 0 to params.Count - 1 do
    if not IsKnownName(params.Name[i]) then
      addExtension(params.Name[i], params[params.Name[i]]);
end;

procedure TFHIROperationBaseObject.loadExtensions(params: TFHIRParameters);
var
  p : TFhirParametersParameter;
begin
  for p in params.parameterList do
    if not IsKnownName(p.name) then
      addExtension(p.name, p.value.Link);
end;

procedure TFHIROperationBaseObject.writeExtensions(params: TFHIRParameters);
var
  p : TFHIROpExtension;
begin
  if FExtensions <> nil then
    for p in FExtensions do
      with params.parameterList.Append do
      begin
        name := p.name;
        value := p.value.Link;
      end;
end;

procedure TFHIROperationBaseObject.writeExtensions(params: TFhirParametersParameter);
var
  p : TFHIROpExtension;
begin
  if FExtensions <> nil then
    for p in FExtensions do
      with params.partList.Append do
      begin
        name := p.name;
        value := p.value.Link;
      end;
end;

function TFHIROperationBaseObject.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FExtensions.sizeInBytes(magic));
end;

{ TFHIROpExtension }

destructor TFHIROpExtension.Destroy;
begin
  FValue.free;
  inherited;
end;

procedure TFHIROpExtension.SetValue(const Value: TFHIRDataType);
begin
  FValue.free;
  FValue := Value;
end;

function TFHIROpExtension.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FValue.sizeInBytes(magic));
end;

end.
