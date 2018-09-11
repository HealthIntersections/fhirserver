unit ExpressionEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.ComboEdit, FMX.StdCtrls, System.ImageList, FMX.ImgList,
  FMX.Edit, FMX.Controls.Presentation,
  FHIR.R4.Types, FHIR.R4.Resources,
  TranslationsEditorDialog;

type
  TExpressionEditorForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    edtDescription: TEdit;
    ToolbarImages: TImageList;
    btnText: TButton;
    edtName: TEdit;
    cbeLanguage: TComboEdit;
    edtReference: TEdit;
    memExpression: TMemo;
    btnOk: TButton;
    Button3: TButton;
    lblDoco: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    btnClear: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure btnTextClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    exp : TFHIRExpression;
    resource : TFHIRResource;
  public
    { Public declarations }
  end;

var
  ExpressionEditorForm: TExpressionEditorForm;

function editExpression(parent : TComponent; resource : TFHIRResource; exp : TFHIRExpression; doco : String) : TFHIRExpression;

implementation

{$R *.fmx}

function editExpression(parent : TComponent; resource : TFHIRResource; exp : TFHIRExpression; doco : String) : TFHIRExpression;
begin
  ExpressionEditorForm := TExpressionEditorForm.create(parent);
  try
    ExpressionEditorForm.lblDoco.text := doco;

    if exp <> nil then
    begin
      ExpressionEditorForm.edtDescription.text := exp.description;
      ExpressionEditorForm.edtName.text := exp.name;
      ExpressionEditorForm.cbeLanguage.text := exp.language;
      ExpressionEditorForm.edtReference.text := exp.reference;
      ExpressionEditorForm.memExpression.text := exp.expression;
      ExpressionEditorForm.exp := exp.link;
      ExpressionEditorForm.resource := resource.link;
    end
    else
    begin
      ExpressionEditorForm.btnClear.enabled := false;
      ExpressionEditorForm.exp := TFHIRExpression.create;
      ExpressionEditorForm.resource := resource.link;
    end;
    case ExpressionEditorForm.showModal of
      mrOK:
        begin
          result := ExpressionEditorForm.exp.link;
          result.description := ExpressionEditorForm.edtDescription.text;
          result.name := ExpressionEditorForm.edtName.text;
          result.language := ExpressionEditorForm.cbeLanguage.text;
          result.reference := ExpressionEditorForm.edtReference.text;
          result.expression := ExpressionEditorForm.memExpression.text;
        end;
      mrCancel: result := exp.link;
      mrAbort: result := nil;
    end;
  finally
    ExpressionEditorForm.free;
  end;
end;

procedure TExpressionEditorForm.btnOkClick(Sender: TObject);
begin
  if (edtReference.text = '') and (memExpression.text = '') then
    raise Exception.Create('Must provide at least one of reference of content');
end;

procedure TExpressionEditorForm.btnTextClick(Sender: TObject);
begin
  if exp.descriptionElement = nil then
    exp.descriptionElement := TFhirString.Create;
  editStringDialog(self, 'Expression Description', btnText, edtDescription, resource, exp.descriptionElement);
end;

procedure TExpressionEditorForm.FormDestroy(Sender: TObject);
begin
  resource.free;
  exp.free;
end;

end.
