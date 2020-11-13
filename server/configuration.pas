Unit configuration;

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


interface

uses
  Windows, SysUtils, Classes, IniFiles, Generics.Collections,
  fsl_base;

type
  TFHIRServerIniComplex = class (TFslObject)
  private
    FDetails : TFslStringDictionary;
    function getValue(name: String): String;
  public
    constructor Create(value : String);
    destructor Destroy; override;
    property value[name : String] : String read getValue; default;
  end;

  TFHIRServerIniFile = class (TFslObject)
  private
    FIni : TIniFile;

    FTerminologies: TFslMap<TFHIRServerIniComplex>;
    FIdentityProviders: TFslMap<TFHIRServerIniComplex>;
    FDatabases: TFslMap<TFHIRServerIniComplex>;
    FEndPoints: TFslMap<TFHIRServerIniComplex>;
    FDestinations: TFslMap<TFHIRServerIniComplex>;

    function GetFileName : String;
    function getAdminValue(name: String): String;
    function getWebValue(name: String): String;
    function GetRunNumber: integer;
    procedure SetRunNumber(const Value: integer);
    procedure readSection(name : String; map : TFslMap<TFHIRServerIniComplex>);
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    Function Link : TFHIRServerIniFile; overload;
    property FileName: string read GetFileName;

    property web[name : String] : String read getWebValue;
    property admin[name : String] : String  read getAdminValue;
    property terminologies : TFslMap<TFHIRServerIniComplex> read FTerminologies;
    property databases : TFslMap<TFHIRServerIniComplex> read FDatabases;
    property endpoints : TFslMap<TFHIRServerIniComplex> read FEndPoints;
    property identityProviders : TFslMap<TFHIRServerIniComplex> read FIdentityProviders;
    property destinations : TFslMap<TFHIRServerIniComplex> read FDestinations;

    property runNumber : integer read GetRunNumber write SetRunNumber;
  end;

implementation

{ TFHIRServerIniFile }

constructor TFHIRServerIniFile.Create(const FileName: string);
begin
  inherited create;
  FIni := TIniFile.Create(filename);
  FTerminologies := TFslMap<TFHIRServerIniComplex>.create;
  FIdentityProviders := TFslMap<TFHIRServerIniComplex>.create;
  FDatabases := TFslMap<TFHIRServerIniComplex>.create;
  FEndPoints := TFslMap<TFHIRServerIniComplex>.create;
  FDestinations := TFslMap<TFHIRServerIniComplex>.create;
  readSection('terminologies', FTerminologies);
  readSection('identity-providers', FIdentityProviders);
  readSection('endpoints', FEndPoints);
  readSection('databases', FDatabases);
  readSection('destinations', FDestinations);
end;

destructor TFHIRServerIniFile.Destroy;
begin
  FTerminologies.Free;
  FIdentityProviders.Free;
  FDatabases.Free;
  FEndPoints.Free;
  FDestinations.Free;
  FIni.Free;
  inherited;
end;

function TFHIRServerIniFile.getAdminValue(name: String): String;
begin
  result := FIni.ReadString('admin', name, '');
end;

function TFHIRServerIniFile.GetFileName: String;
begin
  result := FIni.FileName;
end;

function TFHIRServerIniFile.GetRunNumber: integer;
begin
  result := FIni.ReadInteger('server', 'run-number', 0);
end;

function TFHIRServerIniFile.getWebValue(name: String): String;
begin
  result := FIni.ReadString('web', name, '');
end;

function TFHIRServerIniFile.Link: TFHIRServerIniFile;
begin
  result := TFHIRServerIniFile(inherited Link);
end;

procedure TFHIRServerIniFile.readSection(name: String; map: TFslMap<TFHIRServerIniComplex>);
var
  ts : TStringList;
  s : String;
begin
  ts := TStringList.Create;
  try
    FIni.ReadSection(name, ts);
    for s in ts do
      map.Add(s, TFHIRServerIniComplex.create(FIni.ReadString(name, s, '')));
  finally
    ts.free;
  end;
  map.defaultValue := TFHIRServerIniComplex.create('');
end;

procedure TFHIRServerIniFile.SetRunNumber(const Value: integer);
begin
  FIni.writeInteger('server', 'run-number', value);
end;

{ TFHIRServerIniComplex }

constructor TFHIRServerIniComplex.create(value: String);
var
  sl : TArray<String>;
  s, l, r : String;
begin
  inherited Create;
  FDetails := TFslStringDictionary.Create;
  sl := value.Split([';']);
  for s in sl do
  begin
    StringSplit(s, ':', l, r);
    l := l.Trim;
    r := r.Trim;
    if r.StartsWith('"') and r.EndsWith('"') then
      r := r.Substring(1, r.Length-2);
    FDetails.Add(l, r);
  end;
end;

destructor TFHIRServerIniComplex.Destroy;
begin
  FDetails.Free;
  inherited;
end;

function TFHIRServerIniComplex.getValue(name: String): String;
begin
  if not FDetails.TryGetValue(name, result) then
    result := '';
end;

end.


