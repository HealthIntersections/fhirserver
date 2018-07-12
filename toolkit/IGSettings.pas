unit IGSettings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Dialogs, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation;

type
  TIGSettingsForm = class(TForm)
    btnCheckDependencies: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  IGSettingsForm: TIGSettingsForm;

implementation

{$R *.fmx}

procedure TIGSettingsForm.Button1Click(Sender: TObject);
var dir, folder:string;

begin
if SelectDirectory('Select path to IG framework (contains License.md)', '', dir) then
  folder:=dir;

end;

end.
