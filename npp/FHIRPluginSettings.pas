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
  public
    Constructor Create(folder: String);
    Destructor Destroy; override;
    property ToolboxVisible : boolean read GetToolboxVisible write SetToolboxVisible;
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

function TFHIRPluginSettings.GetToolboxVisible: boolean;
begin
  result := ini.ReadBool('Toolbox', 'Visible', false);
end;

procedure TFHIRPluginSettings.SetToolboxVisible(const Value: boolean);
begin
  ini.WriteBool('Toolbox', 'Visible', Value);
end;

end.
