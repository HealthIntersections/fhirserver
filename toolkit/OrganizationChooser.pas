unit OrganizationChooser;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.StdCtrls, FMX.Controls.Presentation,
  FHIRResources;

type
  TOrganizationSelectionForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    ListBox1: TListBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OrganizationSelectionForm: TOrganizationSelectionForm;

function pickOrganization(owner : TComponent; orgs : TFhirOrganizationList; var org : TFhirOrganization) : boolean;

implementation

{$R *.fmx}

function pickOrganization(owner : TComponent; orgs : TFhirOrganizationList; var org : TFhirOrganization) : boolean;
var
  o : TFhirOrganization;
begin
  OrganizationSelectionForm := TOrganizationSelectionForm.Create(owner);
  try
    for o in orgs do
      OrganizationSelectionForm.ListBox1.Items.Add(o.name);
    if orgs.Count > 0 then
      OrganizationSelectionForm.ListBox1.ItemIndex := 0;
    result := OrganizationSelectionForm.ShowModal = mrOk;
    if result then
      org := orgs[OrganizationSelectionForm.ListBox1.ItemIndex];
  finally
    FreeAndNil(OrganizationSelectionForm);
  end;
end;

end.
