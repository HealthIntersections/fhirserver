unit fhir_features;

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

interface

uses
  SysUtils, Classes,
  fsl_base, fsl_utilities;

type
  TFHIRFeatureContextNode = class (TFslObject)
  private
    FKind: String;
    FQualifier: String;
    class function fromString(syntax : String) : TFHIRFeatureContextNode; overload;
  public
    function matches(f : TFHIRFeatureContextNode) : boolean;
    function ToString: string; override;

    property kind : String read FKind write FKind;
    property qualifier : String read FQualifier write FQualifier;
  end;

  TFHIRFeature = class (TFslObject)
  private
    FContext: TFslList<TFHIRFeatureContextNode>;
    FValue: String;
    FId: String;
    procedure Setvalue(const Value: String);
  public
    constructor Create; override;
    destructor Destroy; override;

    class function fromString(syntax, value : String) : TFHIRFeature; overload;
    class function fromString(syntax : String) : TFHIRFeature; overload;

    function ToString: string; override;

    property context : TFslList<TFHIRFeatureContextNode> read FContext;
    property id : String read FId;
    property value : String read FValue;

    function matches(f : TFHIRFeature) : boolean;
    function livesOn(f : TFHIRFeature) : boolean;
    function relativePath(f : TFHIRFeature) : string;
    function makeSubFeature(subId, value : String) : TFHIRFeature;
  end;

  TFHIRFeatureComparer = class (TFslComparer<TFHIRFeature>)
  protected
    function Compare(const l, r : TFHIRFeature) : integer; override;
  end;

  TFHIRFeatureEngine = class (TFslObject)
  private
    FCollection : TFslList<TFHIRFeature>;
    procedure sortFeatures;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure defineFeature(id, value : String); overload;
    procedure defineFeature(id : String; value : boolean); overload;

    function hasFeature(feature : TFHIRFeature) : boolean; overload;
    function hasFeature(feature : String) : boolean; overload;

    function filterFeatures(feature : TFHIRFeature) : TFslList<TFHIRFeature>;
    function sortedFeatures : TFslList<TFHIRFeature>;
  end;

implementation

{ TFHIRFeature }

constructor TFHIRFeature.Create;
begin
  inherited;
  FContext := TFslList<TFHIRFeatureContextNode>.Create;
end;

destructor TFHIRFeature.Destroy;
begin
  FContext.free;
  inherited;
end;

class function TFHIRFeature.fromString(syntax: String): TFHIRFeature;
var
  l, r : String;
begin
  if syntax.Contains('=') then
  begin
    StringSplit(syntax, '=', l, r);
    result := fromString(l, r);
  end
  else if syntax.Contains(';') then
  begin
    StringSplit(syntax, ';', l, r);
    result := fromString(l, r);
  end
  else
    result := fromString(syntax, '');
end;

class function TFHIRFeature.fromString(syntax, value: String): TFHIRFeature;
var
  s : String;
begin
  result := TFHIRFeature.Create;
  try
    result.FId := syntax.Trim;
    result.FValue := value.Trim;
    for s in syntax.split(['.']) do
      result.FContext.Add(TFHIRFeatureContextNode.fromString(s.Trim));
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFeature.makeSubFeature(subId, value: String): TFHIRFeature;
begin
  result := nil;
end;

function TFHIRFeature.matches(f: TFHIRFeature): boolean;
var
  i : integer;
begin
  if (f = nil) or (FContext.Count < f.FContext.Count) then
    result := false
  else
  begin
    for i := 0 to f.FContext.Count - 1 do
      if not FContext[i].matches(f.FContext[i]) then
        exit(false);
    result := (f.value = '') or (f.value = value);
  end;
end;

function TFHIRFeature.relativePath(f: TFHIRFeature): string;
var
  i : integer;
begin
  result := '';
  for i := f.FContext.count to FContext.count - 1 do
    if result = '' then
      result := FContext[i].ToString
    else
      result := result + '.' + FContext[i].ToString;
end;

function TFHIRFeature.livesOn(f: TFHIRFeature): boolean;
var
  i : integer;
begin
  if (f = nil) or (FContext.Count <> f.FContext.Count+1) then
    result := false
  else
  begin
    for i := 0 to f.FContext.Count - 1 do
      if not FContext[i].matches(f.FContext[i]) then
        exit(false);
    result := (f.value = '') or (f.value = value);
  end;
end;

procedure TFHIRFeature.Setvalue(const Value: String);
begin
  Fvalue := Value;
end;

function TFHIRFeature.ToString: string;
begin
  result := FId+' = '+FValue;
end;

{ TFHIRFeatureEngine }

constructor TFHIRFeatureEngine.Create;
begin
  inherited;
  FCollection := TFslList<TFHIRFeature>.Create;
end;

procedure TFHIRFeatureEngine.defineFeature(id: String; value: boolean);
begin
  if value then
    FCollection.add(TFHIRFeature.fromString(id, 'true'))
  else
    FCollection.add(TFHIRFeature.fromString(id, 'false'));
  sortFeatures;
end;

procedure TFHIRFeatureEngine.defineFeature(id, value: String);
begin
  FCollection.add(TFHIRFeature.fromString(id, value));
  sortFeatures;
end;

destructor TFHIRFeatureEngine.Destroy;
begin
  FCollection.free;
  inherited;
end;

function TFHIRFeatureEngine.filterFeatures(feature: TFHIRFeature): TFslList<TFHIRFeature>;
begin
  result := nil;
end;

function TFHIRFeatureEngine.hasFeature(feature: TFHIRFeature): boolean;
begin
  result := false;
end;

function TFHIRFeatureEngine.hasFeature(feature: String): boolean;
begin
  result := false;
end;

function TFHIRFeatureEngine.sortedFeatures: TFslList<TFHIRFeature>;
begin
  result := FCollection.link;
end;

procedure TFHIRFeatureEngine.sortFeatures;
begin
  FCollection.Sort(TFHIRFeatureComparer.create);
end;

{ TFHIRFeatureContextNode }

class function TFHIRFeatureContextNode.fromString(syntax: String): TFHIRFeatureContextNode;
begin
  result := TFHIRFeatureContextNode.Create;
  try
    if syntax.Contains(':') then
      StringSplit(syntax, ':', result.FKind, result.FQualifier)
    else
      result.FKind := syntax;
    result.Link;
  finally
    result.free;
  end;
end;

function TFHIRFeatureContextNode.matches(f: TFHIRFeatureContextNode): boolean;
begin
  result := (FKind = f.FKind) and ((f.FQualifier = '') or (f.qualifier = qualifier));
end;

function TFHIRFeatureContextNode.ToString: string;
begin
  result := FKind;
  if FQualifier <> '' then
    result := result + ':' + FQualifier
end;

{ TFHIRFeatureComparer }

function TFHIRFeatureComparer.Compare(const l, r: TFHIRFeature): integer;
begin
  result := String.Compare(l.id, r.id);
end;

end.

