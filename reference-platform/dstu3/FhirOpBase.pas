unit FhirOpBase;

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

{$IFNDEF FHIR3}
This is the dstu3 version of the FHIR code
{$ENDIF}
interface

uses
  SysUtils,
  AdvObjects, AdvGenerics, ParseMap,
  FHIRTypes, FHIRResources;
type

  TFHIROpExtension = class (TAdvObject)
  private
    FName : String;
    FValue : TFHIRType;
    procedure SetValue(const Value: TFHIRType);
  public
    destructor Destroy; override;

    property name : String read FName write FName;
    property value : TFHIRType read FValue write SetValue;
  end;

  TFHIROperationBaseObject = class (TAdvObject)
  private
    FExtensions : TAdvList<TFHIROpExtension>;
    function GetExtensions: TAdvList<TFHIROpExtension>;
  protected
    function isKnownName(name : String) : boolean; overload; virtual;
    procedure loadExtensions(params : TFHIRParameters); overload;
    procedure loadExtensions(params : TParseMap); overload;
    procedure loadExtensions(params : TFhirParametersParameter); overload;
    procedure writeExtensions(params : TFHIRParameters); overload;
    procedure writeExtensions(params : TFhirParametersParameter); overload;
  public
    destructor Destroy; override;
    property extensions : TAdvList<TFHIROpExtension> read GetExtensions;
    procedure addExtension(name : String; value : TFHIRType); overload;
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
    procedure load(params : TFHIRParameters); overload; virtual;
    procedure load(params : TParseMap); overload; virtual;
    function asParams : TFHIRParameters; virtual;
  end;

  TFHIROperationResponse = class (TFHIROperationBaseObject)
  public
    constructor Create(); overload; override;
    procedure load(params : TFHIRParameters); overload; virtual;
    procedure load(params : TParseMap); overload; virtual;
    function asParams : TFHIRParameters; virtual;
  end;


implementation

{ TFHIROperationRequest }

function TFHIROperationRequest.asParams: TFHIRParameters;
begin
  raise Exception.Create('Must be overriden');
end;

procedure TFHIROperationRequest.load(params: TFHIRParameters);
begin
end;

procedure TFHIROperationRequest.load(params: TParseMap);
begin
end;

constructor TFHIROperationRequest.create;
begin
  inherited Create;
end;


{ TFHIROperationResponse }

function TFHIROperationResponse.asParams: TFHIRParameters;
begin
  raise Exception.Create('Must be overriden');
end;

procedure TFHIROperationResponse.load(params: TFHIRParameters);
begin
end;

procedure TFHIROperationResponse.load(params: TParseMap);
begin
end;

constructor TFHIROperationResponse.create;
begin
  inherited Create;
end;


{ TFHIROperationObject }

function TFHIROperationObject.asParams(name : String): TFHIRParametersParameter;
begin
  result := nil;
end;

constructor TFHIROperationObject.create;
begin
  inherited;

end;

constructor TFHIROperationObject.Create(params: TFhirParametersParameter);
begin
  inherited create;
end;


{ TFHIROperationBaseObject }

procedure TFHIROperationBaseObject.addExtension(name: String; value: TFHIRType);
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
  FExtensions.Free;
  inherited;
end;

function TFHIROperationBaseObject.GetExtensions: TAdvList<TFHIROpExtension>;
begin
  if FExtensions = nil then
    FExtensions := TAdvList<TFHIROpExtension>.create;
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

procedure TFHIROperationBaseObject.loadExtensions(params: TParseMap);
var
  i : integer;
begin
  for i := 0 to params.Count - 1 do
    if not IsKnownName(params.VarName(i)) then
      addExtension(params.VarName(i), params.GetVar(params.VarName(i)));
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

{ TFHIROpExtension }

destructor TFHIROpExtension.Destroy;
begin
  FValue.Free;
  inherited;
end;

procedure TFHIROpExtension.SetValue(const Value: TFHIRType);
begin
  FValue.Free;
  FValue := Value;
end;

end.
