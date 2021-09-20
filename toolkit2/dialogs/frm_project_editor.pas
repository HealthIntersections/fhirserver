unit frm_project_editor;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  fsl_base,
  ftk_project_tree;

type

  { TProjectSettingsForm }

  TProjectSettingsForm = class(TForm)
    btnOk: TButton;
    Button2: TButton;
    edtName: TEdit;
    Label3: TLabel;
    Panel1: TPanel;
    procedure btnOkClick(Sender: TObject);
    procedure edtNameChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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

procedure TProjectSettingsForm.edtNameChange(Sender: TObject);
begin
  btnOK.enabled := not NameIsUsed(edtName.text);
end;

procedure TProjectSettingsForm.btnOkClick(Sender: TObject);
begin
  project.name := edtName.text;
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

