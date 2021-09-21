unit frm_project_editor;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, fsl_base, ftk_project_tree;

type

  { TProjectSettingsForm }

  TProjectSettingsForm = class(TForm)
    btnOk: TButton;
    Button2: TButton;
    edtName: TEdit;
    edtFolder: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    fd: TSelectDirectoryDialog;
    SpeedButton1: TSpeedButton;
    procedure btnOkClick(Sender: TObject);
    procedure edtNameChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FProject: TFHIRProjectNode;
    FProjects: TFslList<TFHIRProjectNode>;
    procedure SetProject(AValue: TFHIRProjectNode);
    procedure SetProjects(AValue: TFslList<TFHIRProjectNode>);
    function NameIsUsed(name : String) : boolean;
  public
    property Project : TFHIRProjectNode read FProject write SetProject;
    property Projects : TFslList<TFHIRProjectNode> read FProjects write SetProjects;
  end;

var
  ProjectSettingsForm: TProjectSettingsForm;

implementation

{$R *.lfm}

{ TProjectSettingsForm }

procedure TProjectSettingsForm.FormDestroy(Sender: TObject);
begin
  FProject.Free;
  FProjects.Free;
end;

procedure TProjectSettingsForm.SpeedButton1Click(Sender: TObject);
begin
  fd.InitialDir := edtFolder.text;
  if fd.execute then
    edtFolder.text := fd.FileName;
  if edtName.text = '' then
    edtName.text := ExtractFileName(edtFolder.Text);
end;

procedure TProjectSettingsForm.edtNameChange(Sender: TObject);
begin
  btnOK.enabled := not NameIsUsed(edtName.text);
end;

procedure TProjectSettingsForm.btnOkClick(Sender: TObject);
begin
  project.name := edtName.text;
  project.address := edtFolder.text;
end;

procedure TProjectSettingsForm.SetProject(AValue: TFHIRProjectNode);
begin
  FProject.Free;
  FProject := AValue;
end;

procedure TProjectSettingsForm.SetProjects(AValue: TFslList<TFHIRProjectNode>);
begin
  FProjects.Free;
  FProjects := AValue;
end;

function TProjectSettingsForm.NameIsUsed(name: String): boolean;
var
  t : TFHIRProjectNode;
begin
  result := false;
  for t in FProjects do
    if (t.name = name) and (t.id <> project.id) then
      exit(true);
end;

end.

