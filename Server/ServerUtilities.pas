unit ServerUtilities;

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
  SysUtils, Classes, IniFiles, Generics.Collections,
  IdCustomHTTPServer,
  AdvObjects, AdvGenerics,
  FHIRResources, FHIRConstants, FHIRSupport;

type
  TProcessFileEvent = procedure (request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo; session : TFhirSession; path : String; secure : boolean; variables: TDictionary<String, String> = nil) of Object;

  TFHIRResourceConfig = class (TAdvObject)
  public
    name : String;
    enum : TFHIRResourceType;
    key: integer;
    Supported: Boolean;
    IdGuids: Boolean;
    IdClient: Boolean;
    IdServer: Boolean;
    cmdUpdate: Boolean;
    cmdDelete: Boolean;
    cmdValidate: Boolean;
    cmdHistoryInstance: Boolean;
    cmdHistoryType: Boolean;
    cmdSearch: Boolean;
    cmdCreate: Boolean;
    cmdOperation: Boolean;
    cmdVRead : boolean;
    versionUpdates: Boolean;

    lastResourceId : integer;

    constructor Create; override;
  end;

  TFHIRServerWorker = class (TAdvObject)
  private
    FServerContext : TAdvObject; // no link
  public
    Constructor Create(ServerContext : TAdvObject);
    Destructor Destroy; override;

    Property ServerContext : TAdvObject read FServerContext;
  end;

  TFHIRServerIniFileVersionOption = (voVersioningNotApplicable, voMaybeVersioned, voMustBeVersioned);

  TFHIRServerIniFile = class (TAdvObject)
  private
    FIni : TCustomIniFile;
    function GetFileName: string;
  public
    constructor Create(const FileName: string);
    Destructor Destroy; override;
    Function Link : TFHIRServerIniFile; overload;

    property FileName: string read GetFileName;

    procedure ReadSection(versioning: TFHIRServerIniFileVersionOption; const Section: string; Strings: TStrings);
    function SectionExists(versioning: TFHIRServerIniFileVersionOption; const Section: string): Boolean;

    function ReadString(versioning: TFHIRServerIniFileVersionOption; const Section, Ident, Default: string): string;
    function ReadInteger(versioning: TFHIRServerIniFileVersionOption; const Section, Ident: string; Default: Integer): Integer;
    function ReadBool(versioning: TFHIRServerIniFileVersionOption; const Section, Ident: string; Default: Boolean): Boolean;
    function ValueExists(versioning: TFHIRServerIniFileVersionOption; const Section, Ident: string): Boolean;

    procedure WriteString(const Section, Ident, Value: String);
    procedure WriteInteger(const Section, Ident: string; Value: Integer);
    procedure DeleteKey(const Section, Ident: String);
  end;

function buildCompartmentsSQL(resconfig : TAdvMap<TFHIRResourceConfig>; compartment : TFHIRCompartmentId; sessionCompartments : TAdvList<TFHIRCompartmentId>) : String;

implementation

constructor TFHIRResourceConfig.Create;
begin
  inherited;
  Supported := true;
  IdGuids := false;
  IdClient := true;
  IdServer := true;
  cmdUpdate := true;
  cmdDelete := true;
  cmdValidate := true;
  cmdHistoryInstance := true;
  cmdHistoryType := true;
  cmdSearch := true;
  cmdCreate := true;
  cmdOperation := true;
  versionUpdates := false;
  lastResourceId  := 0;
end;

{ TFHIRServerWorker }

constructor TFHIRServerWorker.Create(ServerContext: TAdvObject);
begin
  inherited Create;
  FServerContext := ServerContext;
end;

destructor TFHIRServerWorker.Destroy;
begin
  inherited;
end;


{ TFHIRServerIniFile }

constructor TFHIRServerIniFile.Create(const FileName: string);
begin
  inherited create;
  if filename <> '' then
    FIni := TIniFile.Create(filename)
  else
    FIni := TMemIniFile.Create('');
end;

destructor TFHIRServerIniFile.Destroy;
begin
  FIni.Free;
  inherited;
end;

procedure TFHIRServerIniFile.DeleteKey(const Section, Ident: String);
begin
  FIni.DeleteKey(Section, Ident);
end;

function TFHIRServerIniFile.GetFileName: string;
begin
  result := FIni.FileName;
end;

function TFHIRServerIniFile.Link: TFHIRServerIniFile;
begin
  result := TFHIRServerIniFile(inherited Link);
end;

function TFHIRServerIniFile.ReadBool(versioning: TFHIRServerIniFileVersionOption; const Section, Ident: string; Default: Boolean): Boolean;
begin
  if (versioning = voMustBeVersioned) then
    result := FIni.ReadBool(section+'-'+FHIR_GENERATED_PUBLICATION, ident, default)
  else if (versioning = voMaybeVersioned) and Fini.ValueExists(section+'-'+FHIR_GENERATED_PUBLICATION, ident) then
    result := FIni.ReadBool(section+'-'+FHIR_GENERATED_PUBLICATION, ident, default)
  else
    result := FIni.ReadBool(section, ident, default);
end;

function TFHIRServerIniFile.ReadInteger(versioning: TFHIRServerIniFileVersionOption; const Section, Ident: string; Default: Integer): Integer;
begin
  if (versioning = voMustBeVersioned) then
    result := FIni.ReadInteger(section+'-'+FHIR_GENERATED_PUBLICATION, ident, default)
  else if (versioning = voMaybeVersioned) and Fini.ValueExists(section+'-'+FHIR_GENERATED_PUBLICATION, ident) then
    result := FIni.ReadInteger(section+'-'+FHIR_GENERATED_PUBLICATION, ident, default)
  else
    result := FIni.ReadInteger(section, ident, default);
end;

function TFHIRServerIniFile.ReadString(versioning: TFHIRServerIniFileVersionOption; const Section, Ident, Default: string): string;
begin
  if (versioning = voMustBeVersioned) then
    result := FIni.ReadString(section+'-'+FHIR_GENERATED_PUBLICATION, ident, default)
  else if (versioning = voMaybeVersioned) and Fini.ValueExists(section+'-'+FHIR_GENERATED_PUBLICATION, ident) then
    result := FIni.ReadString(section+'-'+FHIR_GENERATED_PUBLICATION, ident, default)
  else
    result := FIni.ReadString(section, ident, default);
end;

procedure TFHIRServerIniFile.ReadSection(versioning: TFHIRServerIniFileVersionOption; const Section: string; Strings: TStrings);
begin
  if (versioning = voMustBeVersioned) then
    FIni.ReadSection(section+'-'+FHIR_GENERATED_PUBLICATION, Strings)
  else if (versioning = voMaybeVersioned) and Fini.SectionExists(section+'-'+FHIR_GENERATED_PUBLICATION) then
    FIni.ReadSection(section+'-'+FHIR_GENERATED_PUBLICATION, Strings)
  else
    FIni.ReadSection(section, Strings);
end;

function TFHIRServerIniFile.SectionExists(versioning: TFHIRServerIniFileVersionOption; const Section: string): Boolean;
begin
  if (versioning = voMustBeVersioned) then
    result := FIni.SectionExists(section+'-'+FHIR_GENERATED_PUBLICATION)
  else if (versioning = voMaybeVersioned) and Fini.SectionExists(section+'-'+FHIR_GENERATED_PUBLICATION) then
    result := true
  else
    result := FIni.SectionExists(section);
end;

function TFHIRServerIniFile.ValueExists(versioning: TFHIRServerIniFileVersionOption; const Section, Ident: string): Boolean;
begin
  if (versioning = voMustBeVersioned) then
    result := FIni.ValueExists(section+'-'+FHIR_GENERATED_PUBLICATION, Ident)
  else if (versioning = voMaybeVersioned) and Fini.ValueExists(section+'-'+FHIR_GENERATED_PUBLICATION, Ident) then
    result := true
  else
    result := FIni.ValueExists(section, Ident);
end;

procedure TFHIRServerIniFile.WriteInteger(const Section, Ident: string; Value: Integer);
begin
  FIni.WriteInteger(section, ident, value);
end;

procedure TFHIRServerIniFile.WriteString(const Section, Ident, Value: String);
begin
  FIni.WriteString(section, ident, value);
end;

function buildCompartmentsSQL(resconfig : TAdvMap<TFHIRResourceConfig>; compartment : TFHIRCompartmentId; sessionCompartments : TAdvList<TFHIRCompartmentId>) : String;
var
  first : boolean;
  c : TFHIRCompartmentId;
begin
  result := '';
  if (compartment <> nil) then
    if compartment.Id = '*' then
      result := ' and Ids.ResourceKey in (select ResourceKey from Compartments where TypeKey = '+inttostr(ResConfig[CODES_TFHIRResourceType[compartment.Enum]].key)+' and Id is not null)'
    else
      result := ' and Ids.ResourceKey in (select ResourceKey from Compartments where TypeKey = '+inttostr(ResConfig[CODES_TFHIRResourceType[compartment.Enum]].key)+' and Id = '''+compartment.Id+''')';

  if (sessionCompartments <> nil) and (sessionCompartments.Count > 0) then
  begin
    result := result +' and Ids.ResourceKey in (select ResourceKey from Compartments where ';
    first := true;
    for c in sessionCompartments do
    begin
      if first then
        first := false
      else
        result := result + ' or ';
      result := result + 'TypeKey = '+inttostr(ResConfig[CODES_TFHIRResourceType[c.Enum]].key)+' and Id = '''+c.id+'''';
    end;
    result := result + ')';
  end;
end;

end.
