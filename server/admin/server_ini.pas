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
  fsl_base, fsl_utilities, fsl_stream;

type

  { TFHIRServerConfigFileProperty }

  TFHIRServerConfigFileProperty = class (TFslObject)
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

    procedure assign(source : TFslObject); override;

    property name : String read FName write FName;
    property comments : TStringList read FComments;
    property comment : String read FComment write FComment;
    property values : TStringList read FValues;
    property value : String read GetValue write SetValue;
    function readAsInt(def : integer) : Integer;
    function readAsBool(def : boolean) : boolean;

    class function fromLine(line : String; comments : TStringList) : TFHIRServerConfigFileProperty;
  end;

  { TFHIRServerConfigFileSection }

  TFHIRServerConfigFileSection = class (TFslObject)
  private
    FComment: String;
    FComments: TStringList;
    FName: String;
    FProperties: TFslList<TFHIRServerConfigFileProperty>;
    FSections: TFslList<TFHIRServerConfigFileSection>;
    function GetProperty(name : String): TFHIRServerConfigFileProperty;
    function GetSection(name : String): TFHIRServerConfigFileSection;

    procedure save(indent : String; ts : TStringList);
  public
    constructor Create(name : String);
    destructor Destroy; override;
    function link : TFHIRServerConfigFileSection;

    procedure assign(source : TFslObject); override;

    property name : String read FName write FName;
    property comments : TStringList read FComments;
    property comment : String read FComment write FComment;
    property properties : TFslList<TFHIRServerConfigFileProperty> read FProperties;
    property sections : TFslList<TFHIRServerConfigFileSection> read FSections;

    property section[name : String] : TFHIRServerConfigFileSection read GetSection;
    property prop[name : String] : TFHIRServerConfigFileProperty read GetProperty; default;

    class function fromLine(line : String; comments : TStringList) : TFHIRServerConfigFileSection;
  end;

  TFHIRServerConfigFile = class (TFslObject)
  private
    FComments: TStringList;
    FFilename: String;
    FSections: TFslList<TFHIRServerConfigFileSection>;
    function GetSection(name : String): TFHIRServerConfigFileSection;
  public
    constructor Create(filename : String);
    destructor Destroy; override;

    procedure Save;

    property FileName : String read FFilename;
    property section[name : String] : TFHIRServerConfigFileSection read GetSection;
    property sections : TFslList<TFHIRServerConfigFileSection> read FSections;
    property comments : TStringList read FComments; // at the end
  end;


  { TFHIRServerIniComplex }

  TFHIRServerIniComplex = class (TFslObject)
  private
    FSection : TFHIRServerConfigFileSection;
    FData: TFslObject;
    FStatus: String;
    FThreadStatus: String;
    function GetName: String;
    function getValue(name: String): String;
    procedure SetName(AValue: String);
    procedure SetValue(name: String; const Value: String);
  public
    constructor Create; override;
    constructor Create(section : TFHIRServerConfigFileSection); overload;
    destructor Destroy; override;
    function link : TFHIRServerIniComplex;
    function clone : TFHIRServerIniComplex;

    procedure assign(source : TFslObject); override;

    property section : TFHIRServerConfigFileSection read FSection;

    property value[name : String] : String read getValue write SetValue; default;
    property name : String read GetName write SetName;
    property status : String read FStatus write FStatus;
    property threadStatus : String read FThreadStatus write FThreadStatus;
    property data : TFslObject read FData write FData;
  end;

  { TFHIRServerIniFile }

  TFHIRServerIniFile = class (TFslObject)
  private
    FFile : TFHIRServerConfigFile;
    FTerminologies: TFslMap<TFHIRServerIniComplex>;
    FIdentityProviders: TFslMap<TFHIRServerIniComplex>;
    FDatabases: TFslMap<TFHIRServerIniComplex>;
    FEndPoints: TFslMap<TFHIRServerIniComplex>;
    FDestinations: TFslMap<TFHIRServerIniComplex>;

    function GetFileName : String;
    function getAdminValue(name: String): String;
    function getWebValue(name: String): String;
    function getServiceValue(name: String): String;
    function GetRunNumber: integer;
    procedure SetRunNumber(const Value: integer);
    procedure readSection(section : TFHIRServerConfigFileSection; map : TFslMap<TFHIRServerIniComplex>);
    procedure SetAdminValue(name: String; const Value: String);
    procedure SetWebValue(name: String; const Value: String);
    procedure SetServiceValue(name: String; const Value: String);

    procedure readSectionFromIni(ini : TIniFile; name : String; map : TFslMap<TFHIRServerIniComplex>; section: TFHIRServerConfigFileSection); overload;
    procedure readSectionFromIni(ini : TIniFile; name : String; section : TFHIRServerConfigFileSection); overload;
    procedure importFromIni(filename : String);
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    Function Link : TFHIRServerIniFile; overload;
    property FileName: string read GetFileName;

    procedure save; // used by the installer

    property web[name : String] : String read getWebValue write SetWebValue;
    property admin[name : String] : String  read getAdminValue write SetAdminValue;
    property service[name : String] : String  read getServiceValue write SetServiceValue;
    property terminologies : TFslMap<TFHIRServerIniComplex> read FTerminologies;
    property databases : TFslMap<TFHIRServerIniComplex> read FDatabases;
    property endpoints : TFslMap<TFHIRServerIniComplex> read FEndPoints;
    property identityProviders : TFslMap<TFHIRServerIniComplex> read FIdentityProviders;
    property destinations : TFslMap<TFHIRServerIniComplex> read FDestinations;

    function addTerminology(name : String) : TFHIRServerIniComplex;
    function addDatabase(name : String) : TFHIRServerIniComplex;
    function addEndpoint(name : String) : TFHIRServerIniComplex;
    function addIdentityProvider(name : String) : TFHIRServerIniComplex;
    function addDestination(name : String) : TFHIRServerIniComplex;

    property runNumber : integer read GetRunNumber write SetRunNumber;

    // internal support
    //procedure writeString(section, name, value : String);
  end;

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

function isComment(s : String; var c : String) : boolean;
begin
  result := true;
  if s.startsWith('//') then
    c := s.Substring(2).trim
  else if s.startsWith('#') then
  c := s.Substring(1).trim
  else
    result := false;
end;

{ TFHIRServerConfigFileProperty }

constructor TFHIRServerConfigFileProperty.Create;
begin
  inherited Create;
  FComments := TStringList.create;
  FValues := TStringList.create;
end;

destructor TFHIRServerConfigFileProperty.Destroy;
begin
  FValues.Free;
  FComments.Free;
  inherited Destroy;
end;

procedure TFHIRServerConfigFileProperty.assign(source: TFslObject);
var
  src : TFHIRServerConfigFileProperty;
begin
  inherited assign(source);
  src := source as TFHIRServerConfigFileProperty;
  FComment := src.FComment;
  FName := src.FName;
  FComments.assign(src.FComments);
  FValues.assign(src.FValues);
end;

function TFHIRServerConfigFileProperty.GetValue: String;
begin
  if FValues.count = 1 then
    result := FValues[0]
  else
    result := '';
end;

procedure TFHIRServerConfigFileProperty.SetValue(AValue: String);
begin
  FValues.clear;
  if aValue <> '' then
    FValues.add(aValue);
end;

procedure TFHIRServerConfigFileProperty.save(indent: String; ts: TStringList);
var
  s, c : String;
begin
  for s in FComments do
    ts.add(indent+'  # '+s);

  if FComment = '' then
    c := ''
  else
    c := ' # '+FComment;
  if FValues.count = 0 then
    ts.add(indent+'  '+name+': '+c)
  else if FValues.count = 1 then
    ts.add(indent+'  '+name+': '+value+c)
  else
  begin
    ts.add(indent+'  '+name+': '+c);
    for s in FValues do
      ts.add(indent+'   - '+s);
  end;
end;

function TFHIRServerConfigFileProperty.readAsInt(def: integer): Integer;
begin
  result := StrToIntDef(Value, def);
end;

function TFHIRServerConfigFileProperty.readAsBool(def: boolean): boolean;
begin
  result := StrToBool(Value);
end;

class function TFHIRServerConfigFileProperty.fromLine(line: String; comments: TStringList): TFHIRServerConfigFileProperty;
var
  n, v, c : String;
begin
  result := TFHIRServerConfigFileProperty.create;
  try
    stringSplit(line, ':', n, line);
    stringSplit(line, '# ', v, c);
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

{ TFHIRServerConfigFileSection }

constructor TFHIRServerConfigFileSection.Create(name : String);
begin
  inherited Create;
  FName := name;
  FComments := TStringList.create;
  FProperties := TFslList<TFHIRServerConfigFileProperty>.create;
  FSections := TFslList<TFHIRServerConfigFileSection>.create;
end;

destructor TFHIRServerConfigFileSection.Destroy;
begin
  FSections.Free;
  FProperties.Free;
  FComments.Free;
  inherited Destroy;
end;

function TFHIRServerConfigFileSection.link: TFHIRServerConfigFileSection;
begin
  result := TFHIRServerConfigFileSection(inherited link);
end;

procedure TFHIRServerConfigFileSection.assign(source: TFslObject);
var
  src : TFHIRServerConfigFileSection;
begin
  inherited assign(source);
  src := source as TFHIRServerConfigFileSection;
  FComment := src.FComment;
  FComments.assign(src.FComments);
  FName := src.Fname;
  FProperties.Clear;
  FProperties.addAll(src.FProperties);
  FSections.Clear;
  FSections.addAll(src.FSections);
end;

class function TFHIRServerConfigFileSection.fromLine(line: String; comments: TStringList): TFHIRServerConfigFileSection;
var
  n, c : String;
begin
  stringSplit(line, '#', n, c);
  result := TFHIRServerConfigFileSection.create(n.trim);
  try
    result.comment := c.trim();
    result.comments.assign(comments);
    comments.clear;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRServerConfigFileSection.GetProperty(name : String): TFHIRServerConfigFileProperty;
var
  p : TFHIRServerConfigFileProperty;
begin
  for p in FProperties do
    if p.name = name then
      exit(p);
  result := TFHIRServerConfigFileProperty.create;
  result.name := name;
  FProperties.add(result);
end;

function TFHIRServerConfigFileSection.GetSection(name : String): TFHIRServerConfigFileSection;
var
  p : TFHIRServerConfigFileSection;
begin
  for p in FSections do
    if p.name = name then
      exit(p);
  result := TFHIRServerConfigFileSection.create(name);
  FSections.add(result);
end;

procedure TFHIRServerConfigFileSection.save(indent: String; ts: TStringList);
var
  s : String;
  prop : TFHIRServerConfigFileProperty;
  sect : TFHIRServerConfigFileSection;
begin
  for s in FComments do
    if s <> '# FHIRServer Config File' then
      ts.add(indent+'# '+s);
  if FComment = '' then
    ts.add(indent+name)
  else
    ts.add(indent+name+' # '+FComment);
  for prop in FProperties do
    prop.save(indent, ts);
  if not FSections.Empty then
  begin
    for sect in FSections do
      sect.save(indent+'  ', ts);
    ts.add(indent)
  end;
end;

{ TFHIRServerConfigFile }

constructor TFHIRServerConfigFile.Create(filename : String);
var
  ts : TStringList;
  comments : TStringList;
  l, s, c : String;
  indent, propIndent : integer;
  sections : TFslList<TFHIRServerConfigFileSection>;
  prop : TFHIRServerConfigFileProperty;
  section : TFHIRServerConfigFileSection;
begin
  inherited Create;
  FComments := TStringList.create;
  FSections := TFslList<TFHIRServerConfigFileSection>.create;
  FFilename := filename;

  if FileExists(filename) and not FileToString(filename, TEncoding.UTF8).startsWith('[') then
  begin
    sections := TFslList<TFHIRServerConfigFileSection>.create;
    comments := TStringList.create;
    ts := TStringList.create;
    try
      ts.LoadFromFile(filename);
      for l in ts do
      begin
        indent := countIndent(l);
        s := l.trim;
        if (s <> '') then
        begin
          if isComment(s, c) then
            comments.add(c)
          else if s.StartsWith('- ') then
          begin
            if propIndent = indent - 1 then
              prop.values.add(s.Substring(1).trim);
          end
          else if s.contains(':') then
          begin
            assert(sections.count > 0);
            prop := TFHIRServerConfigFileProperty.fromLine(s, comments);
            propIndent := indent;
            sections[0].properties.Add(prop);
          end
          else
          begin
            while sections.count > indent do
              sections.Delete(0);
            section := TFHIRServerConfigFileSection.fromLine(s, comments);
            if sections.count = 0 then
              FSections.add(section)
            else
              sections[0].FSections.add(section);
            sections.insert(0, section.link);
          end;
        end;
      end;
      FComments.assign(comments);
    finally
      ts.free;
      comments.free;
      sections.free;
    end;
  end;
end;

destructor TFHIRServerConfigFile.Destroy;
begin
  FSections.Free;
  FComments.Free;
  inherited Destroy;
end;

procedure TFHIRServerConfigFile.Save;
var
  ts : TStringList;
  section : TFHIRServerConfigFileSection;
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

function TFHIRServerConfigFile.GetSection(name : String): TFHIRServerConfigFileSection;
var
  p : TFHIRServerConfigFileSection;
begin
  for p in FSections do
    if p.name = name then
      exit(p);
  result := TFHIRServerConfigFileSection.create(name);
  FSections.add(result);
end;


{ TFHIRServerIniComplex }

constructor TFHIRServerIniComplex.Create(section : TFHIRServerConfigFileSection);
begin
  inherited Create;
  FSection := section;
end;

constructor TFHIRServerIniComplex.Create;
begin
  inherited Create;
  FSection := TFHIRServerConfigFileSection.create('');
end;

destructor TFHIRServerIniComplex.Destroy;
begin
  FSection.Free;
  inherited;
end;

function TFHIRServerIniComplex.link: TFHIRServerIniComplex;
begin
  result := TFHIRServerIniComplex(inherited link);
end;

function TFHIRServerIniComplex.clone: TFHIRServerIniComplex;
begin
  result := TFHIRServerIniComplex(inherited clone);
end;

procedure TFHIRServerIniComplex.assign(source: TFslObject);
var
  src : TFHIRServerIniComplex;
begin
  inherited;
  src := source as TFHIRServerIniComplex;
  FSection.assign(src.FSection);
end;

function TFHIRServerIniComplex.GetName: String;
begin
  result := FSection.Name;
end;

function TFHIRServerIniComplex.getValue(name: String): String;
begin
  result := FSection.prop[name].value;
end;

procedure TFHIRServerIniComplex.SetName(AValue: String);
begin
  FSection.name := aValue;
end;

procedure TFHIRServerIniComplex.SetValue(name: String; const Value: String);
begin
  FSection.prop[name].value := value;
end;

{ TFHIRServerIniFile }

constructor TFHIRServerIniFile.Create(const FileName: string);
begin
  inherited create;
  FTerminologies := TFslMap<TFHIRServerIniComplex>.create('Ini.Terminologies');
  FIdentityProviders := TFslMap<TFHIRServerIniComplex>.create('Ini.IDProviders');
  FDatabases := TFslMap<TFHIRServerIniComplex>.create('Ini.Databases');
  FEndPoints := TFslMap<TFHIRServerIniComplex>.create('Ini.EndPoints');
  FDestinations := TFslMap<TFHIRServerIniComplex>.create('Ini.Destinations');

  FFile := TFHIRServerConfigFile.create(filename);
  if FileExists(filename) and FileToString(filename, TEncoding.UTF8).startsWith('[') then
    importFromIni(filename)
  else
  begin
    readSection(FFile.section['terminologies'], FTerminologies);
    readSection(FFile.section['identity-providers'], FIdentityProviders);
    readSection(FFile.section['endpoints'], FEndPoints);
    readSection(FFile.section['databases'], FDatabases);
    readSection(FFile.section['destinations'], FDestinations);
  end;
end;

destructor TFHIRServerIniFile.Destroy;
begin
  FTerminologies.Free;
  FIdentityProviders.Free;
  FDatabases.Free;
  FEndPoints.Free;
  FDestinations.Free;
  FFile.Free;
  inherited;
end;

function TFHIRServerIniFile.Link: TFHIRServerIniFile;
begin
  result := TFHIRServerIniFile(inherited Link);
end;

function TFHIRServerIniFile.getAdminValue(name: String): String;
begin
  result := FFile.section['admin'].prop[name].value;
end;

function TFHIRServerIniFile.GetFileName: String;
begin
  result := FFile.FileName;
end;


function TFHIRServerIniFile.GetRunNumber: integer;
begin
  result := FFile.section['service'].prop['run-number'].readAsInt(0);
end;

function TFHIRServerIniFile.getServiceValue(name: String): String;
begin
  result := FFile.section['service'].prop[name].value;
end;

function TFHIRServerIniFile.getWebValue(name: String): String;
begin
  result := FFile.section['web'].prop[name].value;
end;

procedure TFHIRServerIniFile.readSection(section : TFHIRServerConfigFileSection; map: TFslMap<TFHIRServerIniComplex>);
var
  sect : TFHIRServerConfigFileSection;
begin
  for sect in section.sections do
    map.Add(sect.name, TFHIRServerIniComplex.create(sect.link));
end;

procedure TFHIRServerIniFile.save;
begin
  FFile.Save;
end;

function TFHIRServerIniFile.addTerminology(name: String): TFHIRServerIniComplex;
begin
  result := TFHIRServerIniComplex.create(FFile.section['terminologies'].section[name].link);
  terminologies.add(name, result);
end;

function TFHIRServerIniFile.addDatabase(name: String): TFHIRServerIniComplex;
begin
  result := TFHIRServerIniComplex.create(FFile.section['databases'].section[name].link);
  databases.add(name, result);
end;

function TFHIRServerIniFile.addEndpoint(name: String): TFHIRServerIniComplex;
begin
  result := TFHIRServerIniComplex.create(FFile.section['endpoints'].section[name].link);
  endpoints.add(name, result);
end;

function TFHIRServerIniFile.addIdentityProvider(name: String): TFHIRServerIniComplex;
begin
  result := TFHIRServerIniComplex.create(FFile.section['identity-providers'].section[name].link);
  identityProviders.add(name, result);
end;

function TFHIRServerIniFile.addDestination(name: String): TFHIRServerIniComplex;
begin
  result := TFHIRServerIniComplex.create(FFile.section['destinations'].section[name].link);
  destinations.add(name, result);
end;

procedure TFHIRServerIniFile.SetAdminValue(name: String; const Value: String);
begin
  FFile.section['admin'].prop[name].value := value;
end;

procedure TFHIRServerIniFile.SetRunNumber(const Value: integer);
begin
  FFile.section['service'].prop['run-number'].value := inttostr(value);
end;

procedure TFHIRServerIniFile.SetServiceValue(name: String; const Value: String);
begin
  FFile.section['service'].prop[name].value := value;
end;

procedure TFHIRServerIniFile.SetWebValue(name: String; const Value: String);
begin
  FFile.section['web'].prop[name].value := value;
end;

procedure TFHIRServerIniFile.importFromIni(filename: String);
var
  ini : TIniFile;
begin
  ini := TIniFile.create(filename);
  try
    readSectionFromIni(ini, 'databases', FDatabases, FFile.section['databases']);
    readSectionFromIni(ini, 'terminologies', FTerminologies, FFile.section['terminologies']);
    readSectionFromIni(ini, 'endpoints', FEndPoints, FFile.section['endpoints']);
    readSectionFromIni(ini, 'destinations', FDestinations, FFile.section['destinations']);
    readSectionFromIni(ini, 'identity-providers', FIdentityProviders, FFile.section['identity-providers']);

    readSectionFromIni(ini, 'web', FFile.section['web']);
    readSectionFromIni(ini, 'admin', FFile.section['admin']);
    readSectionFromIni(ini, 'server', FFile.section['service']);
  finally
    ini.free;
  end;
  FFile.save;
end;

procedure TFHIRServerIniFile.readSectionFromIni(ini: TIniFile; name: String; section: TFHIRServerConfigFileSection);
var
  ts : TStringList;
  s : String;
begin
  ts := TStringList.create;
  try
    ini.ReadSection(name, ts);
    for s in ts do
      section.prop[s].value := ini.ReadString(name, s, '');
  finally
    ts.free;
  end;
end;

procedure TFHIRServerIniFile.readSectionFromIni(ini: TIniFile; name: String; map: TFslMap<TFHIRServerIniComplex>; section: TFHIRServerConfigFileSection);
var
  ts : TStringList;
  s, v, l, r : String;
  sect : TFHIRServerConfigFileSection;
begin
  ts := TStringList.Create;
  try
    ini.ReadSection(name, ts);
    for s in ts do
    begin
      sect := section.section[s];
      map.Add(s, TFHIRServerIniComplex.create(sect.link));
      for v in ini.ReadString(name, s, '').split([';']) do
      begin
        StringSplit(v, ':', l, r);
        l := l.Trim;
        r := r.Trim;
        if r.StartsWith('"') and r.EndsWith('"') then
          r := r.Substring(1, r.Length-2);
        sect.prop[l].value := r;
      end;
    end;
  finally
    ts.free;
  end;
end;

end.


