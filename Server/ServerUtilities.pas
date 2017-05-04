unit ServerUtilities;

interface

uses
  SysUtils, Classes, IniFiles,
  AdvObjects,
  FHIRResources, FHIRConstants;

type
  TFHIRResourceConfig = class (TAdvObject)
  public
    name : String;
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
    FIni : TIniFile;
    function GetFileName: string;
  public
    constructor Create(const FileName: string);
    Destructor Destroy; override;

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
  FIni := TIniFile.Create(filename);
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

function TFHIRServerIniFile.ReadBool(versioning: TFHIRServerIniFileVersionOption; const Section, Ident: string; Default: Boolean): Boolean;
begin
  if (versioning = voMustBeVersioned) then
    result := FIni.ReadBool(section+'-'+FHIR_GENERATED_VERSION, ident, default)
  else if (versioning = voMaybeVersioned) and Fini.ValueExists(section+'-'+FHIR_GENERATED_VERSION, ident) then
    result := FIni.ReadBool(section+'-'+FHIR_GENERATED_VERSION, ident, default)
  else
    result := FIni.ReadBool(section, ident, default);
end;

function TFHIRServerIniFile.ReadInteger(versioning: TFHIRServerIniFileVersionOption; const Section, Ident: string; Default: Integer): Integer;
begin
  if (versioning = voMustBeVersioned) then
    result := FIni.ReadInteger(section+'-'+FHIR_GENERATED_VERSION, ident, default)
  else if (versioning = voMaybeVersioned) and Fini.ValueExists(section+'-'+FHIR_GENERATED_VERSION, ident) then
    result := FIni.ReadInteger(section+'-'+FHIR_GENERATED_VERSION, ident, default)
  else
    result := FIni.ReadInteger(section, ident, default);
end;

function TFHIRServerIniFile.ReadString(versioning: TFHIRServerIniFileVersionOption; const Section, Ident, Default: string): string;
begin
  if (versioning = voMustBeVersioned) then
    result := FIni.ReadString(section+'-'+FHIR_GENERATED_VERSION, ident, default)
  else if (versioning = voMaybeVersioned) and Fini.ValueExists(section+'-'+FHIR_GENERATED_VERSION, ident) then
    result := FIni.ReadString(section+'-'+FHIR_GENERATED_VERSION, ident, default)
  else
    result := FIni.ReadString(section, ident, default);
end;

procedure TFHIRServerIniFile.ReadSection(versioning: TFHIRServerIniFileVersionOption; const Section: string; Strings: TStrings);
begin
  if (versioning = voMustBeVersioned) then
    FIni.ReadSection(section+'-'+FHIR_GENERATED_VERSION, Strings)
  else if (versioning = voMaybeVersioned) and Fini.SectionExists(section+'-'+FHIR_GENERATED_VERSION) then
    FIni.ReadSection(section+'-'+FHIR_GENERATED_VERSION, Strings)
  else
    FIni.ReadSection(section, Strings);
end;

function TFHIRServerIniFile.SectionExists(versioning: TFHIRServerIniFileVersionOption; const Section: string): Boolean;
begin
  if (versioning = voMustBeVersioned) then
    result := FIni.SectionExists(section+'-'+FHIR_GENERATED_VERSION)
  else if (versioning = voMaybeVersioned) and Fini.SectionExists(section+'-'+FHIR_GENERATED_VERSION) then
    result := true
  else
    result := FIni.SectionExists(section);
end;

function TFHIRServerIniFile.ValueExists(versioning: TFHIRServerIniFileVersionOption; const Section, Ident: string): Boolean;
begin
  if (versioning = voMustBeVersioned) then
    result := FIni.ValueExists(section+'-'+FHIR_GENERATED_VERSION, Ident)
  else if (versioning = voMaybeVersioned) and Fini.ValueExists(section+'-'+FHIR_GENERATED_VERSION, Ident) then
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

end.
