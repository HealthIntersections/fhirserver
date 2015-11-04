unit FHIRPluginSettings;

interface

uses
  SysUtils, IniFiles;

type
  TFHIRPluginSettings = class
  private
    ini : TIniFIle;
    function GetToolboxVisible: boolean;
    procedure SetToolboxVisible(const Value: boolean);
    function GetDefinitionsSource: string;
    function GetTerminologyServer: string;
    procedure SetDefinitionsSource(const Value: string);
    procedure SetTerminologyServer(const Value: string);
  public
    Constructor Create(folder: String);
    Destructor Destroy; override;

    property ToolboxVisible : boolean read GetToolboxVisible write SetToolboxVisible;
    property TerminologyServer : string read GetTerminologyServer write SetTerminologyServer;
    property DefinitionsSource : string read GetDefinitionsSource write SetDefinitionsSource;
  end;

var
  Settings : TFHIRPluginSettings;

implementation

{ TFHIRPluginSettings }

constructor TFHIRPluginSettings.Create(folder: String);
begin
  Inherited Create;
  Ini := TIniFile.Create(IncludeTrailingPathDelimiter(folder)+'fhirplugin.ini');
end;

destructor TFHIRPluginSettings.Destroy;
begin
  ini.Free;
  inherited;
end;

function TFHIRPluginSettings.GetDefinitionsSource: string;
begin
  result := ini.ReadString('Validation', 'DefinitionsSource', '');
end;

function TFHIRPluginSettings.GetTerminologyServer: string;
begin
  result := ini.ReadString('Validation', 'TerminologyServer', 'http://fhir2.healthintersections.com.au/open');
end;

function TFHIRPluginSettings.GetToolboxVisible: boolean;
begin
  result := ini.ReadBool('Toolbox', 'Visible', false);
end;

procedure TFHIRPluginSettings.SetDefinitionsSource(const Value: string);
begin
  ini.WriteString('Validation', 'DefinitionsSource', Value);
end;

procedure TFHIRPluginSettings.SetTerminologyServer(const Value: string);
begin
  ini.WriteString('Validation', 'TerminologyServer', Value);
end;

procedure TFHIRPluginSettings.SetToolboxVisible(const Value: boolean);
begin
  ini.WriteBool('Toolbox', 'Visible', Value);
end;

end.
