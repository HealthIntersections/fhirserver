Unit server_ini;

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
  SysUtils, Classes, IniFiles, Generics.Collections,
  fsl_base, fsl_utilities;

type

  { TFHIRConfigFileProperty }

  TFHIRConfigFileProperty = class (TFslObject)
  private
    FComment: String;
    FComments: TStringList;
    FName: String;
    FValues: TStringList;
    function GetValue: String;
    procedure SetValue(AValue: String);

    procedure save(indent : String; ts : TStringList);
  public
    constructor Create; override;
    destructor Destroy; override;


    property name : String read FName write FName;
    property comments : TStringList read FComments;
    property comment : String read FComment write FComment;
    property values : TStringList read FValues;
    property value : String read GetValue write SetValue;
    function readAsInt(def : integer) : Integer;
    function readAsBool(def : boolean) : boolean;

    class function fromLine(line : String; comments : TStringList) : TFHIRConfigFileProperty;
  end;

  { TFHIRConfigFileSection }

  TFHIRConfigFileSection = class (TFslObject)
  private
    FComment: String;
    FComments: TStringList;
    FName: String;
    FProperties: TFslList<TFHIRConfigFileProperty>;
    FSections: TFslList<TFHIRConfigFileSection>;
    function GetProperty(name : String): TFHIRConfigFileProperty;
    function GetSection(name : String): TFHIRConfigFileSection;

    procedure save(indent : String; ts : TStringList);
  public
    constructor Create; override;
    destructor Destroy; override;
    property name : String read FName write FName;
    property comments : TStringList read FComments;
    property comment : String read FComment write FComment;
    property properties : TFslList<TFHIRConfigFileProperty> read FProperties;
    property sections : TFslList<TFHIRConfigFileSection> read FSections;

    property section[name : String] : TFHIRConfigFileSection read GetSection;
    property prop[name : String] : TFHIRConfigFileProperty read GetProperty; default;

    class function fromLine(line : String; comments : TStringList) : TFHIRConfigFileSection;
  end;

  TFHIRConfigFile = class (TFslObject)
  private
    FComments: TStringList;
    FFilename: String;
    FSections: TFslList<TFHIRConfigFileSection>;
    function GetSection(name : String): TFHIRConfigFileSection;
  public
    constructor Create(filename : String);
    destructor Destroy; override;

    procedure Save;

    property FileName : String read FFilename;
    property section[name : String] : TFHIRConfigFileSection read GetSection;
    property sections : TFslList<TFHIRConfigFileSection> read FSections;
    property comments : TStringList read FComments; // at the end
  end;


  //{ TFHIRServerIniComplex }
  //
  //TFHIRServerIniComplex = class (TFslObject)
  //private
  //  FData: TFslObject;
  //  FDetails : TFslStringDictionary;
  //  FName : String;
  //  FStatus: String;
  //  FThreadStatus: String;
  //  function getValue(name: String): String;
  //  procedure SetValue(name: String; const Value: String);
  //  function save : String;
  //public
  //  constructor Create; overload; override;
  //  constructor Create(name, value : String); overload;
  //  destructor Destroy; override;
  //  function link : TFHIRServerIniComplex;
  //  function clone : TFHIRServerIniComplex;
  //
  //  procedure assign(source : TFslObject); override;
  //
  //  property value[name : String] : String read getValue write SetValue; default;
  //  property name : String read FName write FName;
  //  property status : String read FStatus write FStatus;
  //  property threadStatus : String read FThreadStatus write FThreadStatus;
  //  property data : TFslObject read FData write FData;
  //end;

  //{ TFHIRServerIniFile }
  //
  //TFHIRServerIniFile = class (TFslObject)
  //private
  //  FFile : TFHIRConfigFile;
  //  FTerminologies: TFslMap<TFHIRServerIniComplex>;
  //  FIdentityProviders: TFslMap<TFHIRServerIniComplex>;
  //  FDatabases: TFslMap<TFHIRServerIniComplex>;
  //  FEndPoints: TFslMap<TFHIRServerIniComplex>;
  //  FDestinations: TFslMap<TFHIRServerIniComplex>;
  //
  //  function GetFileName : String;
  //  function getAdminValue(name: String): String;
  //  function getWebValue(name: String): String;
  //  function getServiceValue(name: String): String;
  //  function getKernelValue(name: String): String;
  //  function GetRunNumber: integer;
  //  procedure SetRunNumber(const Value: integer);
  //  procedure readSection(name : String; map : TFslMap<TFHIRServerIniComplex>);
  //  procedure SetAdminValue(name: String; const Value: String);
  //  procedure SetWebValue(name: String; const Value: String);
  //  procedure SetServiceValue(name: String; const Value: String);
  //  procedure SetKernelValue(name: String; const Value: String);
  //public
  //  constructor Create(const FileName: string);
  //  destructor Destroy; override;
  //  Function Link : TFHIRServerIniFile; overload;
  //  property FileName: string read GetFileName;
  //
  //  procedure importFromIni(filename : String);
  //  procedure save; // used by the installer
  //
  //  property web[name : String] : String read getWebValue write SetWebValue;
  //  property admin[name : String] : String  read getAdminValue write SetAdminValue;
  //  property service[name : String] : String  read getServiceValue write SetServiceValue;
  //  property kernel[name : String] : String  read getKernelValue write SetKernelValue;
  //  property terminologies : TFslMap<TFHIRServerIniComplex> read FTerminologies;
  //  property databases : TFslMap<TFHIRServerIniComplex> read FDatabases;
  //  property endpoints : TFslMap<TFHIRServerIniComplex> read FEndPoints;
  //  property identityProviders : TFslMap<TFHIRServerIniComplex> read FIdentityProviders;
  //  property destinations : TFslMap<TFHIRServerIniComplex> read FDestinations;
  //
  //  property runNumber : integer read GetRunNumber write SetRunNumber;
  //
  //  // internal support
  //  procedure writeString(section, name, value : String);
  //end;

implementation

function countIndent(s : String) : integer;
var
  i : integer;
begin
  i := 1;
  while (i <= length(s)) and (s[i] = ' ') do
    inc(i);
  result := i div 2;
end;

function isComment(s : String) : boolean;
begin
  result := s.startsWith('//') or s.startsWith('#');
end;


{ TFHIRConfigFileProperty }

constructor TFHIRConfigFileProperty.Create;
begin
  inherited Create;
  FComments := TStringList.create;
  FValues := TStringList.create;
end;

destructor TFHIRConfigFileProperty.Destroy;
begin
  FValues.Free;
  FComments.Free;
  inherited Destroy;
end;

function TFHIRConfigFileProperty.GetValue: String;
begin
  if FValues.count = 1 then
    result := FValues[0]
  else
    result := '';
end;

procedure TFHIRConfigFileProperty.SetValue(AValue: String);
begin
  FValues.clear;
  FValues.add(aValue);
end;

procedure TFHIRConfigFileProperty.save(indent: String; ts: TStringList);
var
  s, c : String;
begin
  for s in FComments do
    ts.add(indent+'# '+s);

  if FComment = '' then
    c := ''
  else
    c := ' # '+FComment;
  if FValues.count = 0 then
    ts.add(indent+name+': '+c)
  else if FValues.count = 0 then
    ts.add(indent+name+': '+value+c)
  else
  begin
    ts.add(indent+name+': '+c);
    for s in FComments do
      ts.add(indent+' - '+s);
  end;
end;

function TFHIRConfigFileProperty.readAsInt(def: integer): Integer;
begin
  result := StrToIntDef(Value, def);
end;

function TFHIRConfigFileProperty.readAsBool(def: boolean): boolean;
begin
  result := StrToBool(Value);
end;

class function TFHIRConfigFileProperty.fromLine(line: String; comments: TStringList): TFHIRConfigFileProperty;
var
  n, v, c : String;
begin
  result := TFHIRConfigFileProperty.create;
  try
    stringSplit(line, ':', n, line);
    stringSplit(line, '#', v, c);
    result.name := n.trim();
    result.value := v.trim();
    result.comment := c.trim();
    result.comments.assign(comments);
    comments.clear;
    result.link;
  finally
    result.free;
  end;
end;

{ TFHIRConfigFileSection }

constructor TFHIRConfigFileSection.Create;
begin
  inherited Create;
  FComments := TStringList.create;
  FProperties := TFslList<TFHIRConfigFileProperty>.create;
  FSections := TFslList<TFHIRConfigFileSection>.create;
end;

destructor TFHIRConfigFileSection.Destroy;
begin
  FSections.Free;
  FProperties.Free;
  FComments.Free;
  inherited Destroy;
end;

class function TFHIRConfigFileSection.fromLine(line: String; comments: TStringList): TFHIRConfigFileSection;
var
  n, c : String;
begin
  result := TFHIRConfigFileSection.create;
  try
    stringSplit(line, '#', n, c);
    result.name := n.trim();
    result.comment := c.trim();
    result.comments.assign(comments);
    comments.clear;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRConfigFileSection.GetProperty(name : String): TFHIRConfigFileProperty;
var
  p : TFHIRConfigFileProperty;
begin
  for p in FProperties do
    if p.name = name then
      exit(p);
  result := TFHIRConfigFileProperty.create;
  result.name := name;
  FProperties.add(result);
end;

function TFHIRConfigFileSection.GetSection(name : String): TFHIRConfigFileSection;
var
  p : TFHIRConfigFileSection;
begin
  for p in FSections do
    if p.name = name then
      exit(p);
  result := TFHIRConfigFileSection.create;
  result.name := name;
  FSections.add(result);
end;

procedure TFHIRConfigFileSection.save(indent: String; ts: TStringList);
var
  s : String;
  prop : TFHIRConfigFileProperty;
  sect : TFHIRConfigFileSection;
begin
  for s in FComments do
    ts.add(indent+'# '+s);
  if FComment = '' then
    ts.add(indent+name)
  else
    ts.add(indent+name+' # '+FComment);
  for prop in FProperties do
    prop.save(indent, ts);
  if FSections.Empty then
    ts.add(indent)
  else
  begin
    for sect in FSections do
      sect.save(indent+'  ', ts);
  end;
end;

{ TFHIRConfigFile }

constructor TFHIRConfigFile.Create(filename : String);
var
  ts : TStringList;
  comments : TStringList;
  l, s : String;
  indent : integer;
  sections : TFslList<TFHIRConfigFileSection>;
  prop : TFHIRConfigFileProperty;
  section : TFHIRConfigFileSection;
begin
  inherited Create;
  FComments := TStringList.create;
  FSections := TFslList<TFHIRConfigFileSection>.create;
  FFilename := filename;
  if FileExists(filename) then
  begin
    sections := TFslList<TFHIRConfigFileSection>.create;
    comments := TStringList.create;
    ts := TStringList.create;
    try
      ts.LoadFromFile(filename);
      for l in ts do
      begin
        indent := countIndent(l);
        s := l.trim;
        if (s= '') or isComment(s) then
          comments.add(s)
        else if s.contains(':') then
        begin
          assert(sections.count > 0);
          sections[0].properties.Add(TFHIRConfigFileProperty.fromLine(s, comments));
        end
        else
        begin
          while sections.count > indent do
            sections.Delete(0);
          section := TFHIRConfigFileSection.fromLine(s, comments);
          if sections.count = 0 then
            FSections.add(section)
          else
            sections[0].FSections.add(section);
          sections.insert(0, section);
        end;
      end;
    finally
      ts.free;
      comments.free;
      sections.free;
    end;
  end;
end;

destructor TFHIRConfigFile.Destroy;
begin
  FSections.Free;
  FComments.Free;
  inherited Destroy;
end;

procedure TFHIRConfigFile.Save;
var
  ts : TStringList;
  section : TFHIRConfigFileSection;
begin
  ts := TStringList.create;
  try
    ts.add('## FHIRServer Config File');
    ts.add('');
    for section in FSections do
      section.save('', ts);
    ts.SaveToFile(FFilename);
  finally
    ts.free;
  end;
end;

function TFHIRConfigFile.GetSection(name : String): TFHIRConfigFileSection;
var
  p : TFHIRConfigFileSection;
begin
  for p in FSections do
    if p.name = name then
      exit(p);
  result := TFHIRConfigFileSection.create;
  result.name := name;
  FSections.add(result);
end;


//{ TFHIRServerIniFile }
//
//constructor TFHIRServerIniFile.Create(const FileName: string);
//begin
//  inherited create;
//  FTerminologies := TFslMap<TFHIRServerIniComplex>.create('Ini.Terminologies');
//  FIdentityProviders := TFslMap<TFHIRServerIniComplex>.create('Ini.IDProviders');
//  FDatabases := TFslMap<TFHIRServerIniComplex>.create('Ini.Databases');
//  FEndPoints := TFslMap<TFHIRServerIniComplex>.create('Ini.EndPoints');
//  FDestinations := TFslMap<TFHIRServerIniComplex>.create('Ini.Destinations');
//
//  if FileExists(filename) then
//    FFile := TFHIRConfigFile.fromFile(filename)
//  else
//    FFile := TFHIRConfigFile.create;
//
//  readSection(FFile.section['terminologies'], FTerminologies);
//  readSection(FFile.section['identity-providers'], FIdentityProviders);
//  readSection(FFile.section['endpoints'], FEndPoints);
//  readSection(FFile.section['databases'], FDatabases);
//  readSection(FFile.section['destinations'], FDestinations);
//end;
//
//destructor TFHIRServerIniFile.Destroy;
//begin
//  FTerminologies.Free;
//  FIdentityProviders.Free;
//  FDatabases.Free;
//  FEndPoints.Free;
//  FDestinations.Free;
//  FFile.Free;
//  inherited;
//end;
//
//function TFHIRServerIniFile.getAdminValue(name: String): String;
//begin
//  result := FFile.section['admin'].prop[name].value;
//end;
//
//function TFHIRServerIniFile.GetFileName: String;
//begin
//  result := FFile.FileName;
//end;
//
//function TFHIRServerIniFile.getKernelValue(name: String): String;
//begin
//  result := FIni.ReadString('kernel', name, '');
//end;
//
//function TFHIRServerIniFile.GetRunNumber: integer;
//begin
//  result := FIni.ReadInteger('server', 'run-number', 0);
//end;
//
//function TFHIRServerIniFile.getServiceValue(name: String): String;
//begin
//  result := FIni.ReadString('service', name, '');
//end;
//
//function TFHIRServerIniFile.getWebValue(name: String): String;
//begin
//  result := FIni.ReadString('web', name, '');
//end;
//
//function TFHIRServerIniFile.Link: TFHIRServerIniFile;
//begin
//  result := TFHIRServerIniFile(inherited Link);
//end;
//
//procedure TFHIRServerIniFile.importFromIni(filename: String);
//begin
////      FIni : TIniFile;
//  FIni := TIniFile.Create(filename);
//
//  readSection('terminologies', FTerminologies);
//  readSection('identity-providers', FIdentityProviders);
//  readSection('endpoints', FEndPoints);
//  readSection('databases', FDatabases);
//  readSection('destinations', FDestinations);
//end;
//
//procedure TFHIRServerIniFile.readSection(name: String; map: TFslMap<TFHIRServerIniComplex>);
//var
//  ts : TStringList;
//  s : String;
//begin
//  ts := TStringList.Create;
//  try
//    FIni.ReadSection(name, ts);
//    for s in ts do
//      map.Add(s, TFHIRServerIniComplex.create(s, FIni.ReadString(name, s, '')));
//  finally
//    ts.free;
//  end;
//  map.defaultValue := TFHIRServerIniComplex.create('', '');
//end;
//
//procedure TFHIRServerIniFile.save;
//var
//  s : String;
//  ts : TStringList;
//begin
//  ts := TStringList.Create;
//  try
//    FIni.ReadSection('databases', ts);
//    for s in ts do
//      FIni.DeleteKey('databases', s);
//    for s in databases.Keys do
//      if (s <> '') then
//        FIni.WriteString('databases', s, databases[s].save);
//    ts.Clear;
//    FIni.ReadSection('endpoints', ts);
//    for s in ts do
//      FIni.DeleteKey('endpoints', s);
//    for s in endpoints.Keys do
//      if (s <> '') then
//        FIni.WriteString('endpoints', s, endpoints[s].save);
//    ts.Clear;
//    FIni.ReadSection('terminologies', ts);
//    for s in ts do
//      FIni.DeleteKey('terminologies', s);
//    for s in terminologies.Keys do
//      if (s <> '') then
//        FIni.WriteString('terminologies', s, terminologies[s].save);
//  finally
//    ts.Free;
//  end;
//end;
//
//procedure TFHIRServerIniFile.SetAdminValue(name: String; const Value: String);
//begin
//  FIni.writeString('admin', name, value);
//end;
//
//procedure TFHIRServerIniFile.SetKernelValue(name: String; const Value: String);
//begin
//  FIni.writeString('kernel', name, value);
//end;
//
//procedure TFHIRServerIniFile.SetRunNumber(const Value: integer);
//begin
//  FIni.writeInteger('server', 'run-number', value);
//end;
//
//procedure TFHIRServerIniFile.SetServiceValue(name: String; const Value: String);
//begin
//  FIni.writeString('service', name, value);
//end;
//
//procedure TFHIRServerIniFile.SetWebValue(name: String; const Value: String);
//begin
//  FIni.writeString('web',name, value);
//end;
//
//
//procedure TFHIRServerIniFile.writeString(section, name, value: String);
//begin
//  FIni.WriteString(section, name, value);
//end;
//
//{ TFHIRServerIniComplex }
//
//constructor TFHIRServerIniComplex.Create;
//begin
//  inherited Create;
//  FDetails := TFslStringDictionary.Create;
//end;
//
//constructor TFHIRServerIniComplex.Create(name, value: String);
//var
//  sl : TArray<String>;
//  s, l, r : String;
//begin
//  Create;
//  FName := name;
//  sl := value.Split([';']);
//  for s in sl do
//  begin
//    StringSplit(s, ':', l, r);
//    l := l.Trim;
//    r := r.Trim;
//    if r.StartsWith('"') and r.EndsWith('"') then
//      r := r.Substring(1, r.Length-2);
//    FDetails.Add(l, r);
//  end;
//  FDetails.AddorSetValue('id', FName);
//end;
//
//destructor TFHIRServerIniComplex.Destroy;
//begin
//  FDetails.Free;
//  inherited;
//end;
//
//function TFHIRServerIniComplex.link: TFHIRServerIniComplex;
//begin
//  result := TFHIRServerIniComplex(inherited link);
//end;
//
//function TFHIRServerIniComplex.clone: TFHIRServerIniComplex;
//begin
//  result := TFHIRServerIniComplex(inherited clone);
//end;
//
//procedure TFHIRServerIniComplex.assign(source: TFslObject);
//begin
//  inherited;
//  FName := (source as TFHIRServerIniComplex).FName;
//  FDetails.assign((source as TFHIRServerIniComplex).FDetails);
//end;
//
//function TFHIRServerIniComplex.getValue(name: String): String;
//begin
//  if not FDetails.TryGetValue(name, result) then
//    result := '';
//end;
//
//function TFHIRServerIniComplex.save: String;
//var
//  s, v : String;
//begin
//  result := '';
//  for s in FDetails.Keys do
//  begin
//    v := FDetails[s];
//    if (v <> '') and (v <> 'id') and not v.startsWith('#') then
//    begin
//      if v.Contains(' ') then
//        result := result + '; '+s+': "'+v+'"'
//      else
//        result := result + '; '+s+': '+v;
//    end;
//  end;
//  if result.Length > 0 then
//    result := result.Substring(1);
//end;
//procedure TFHIRServerIniComplex.SetValue(name: String; const Value: String);
//begin
//  FDetails.AddOrSetValue(name, value);
//end;

end.


