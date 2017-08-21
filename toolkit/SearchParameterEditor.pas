unit SearchParameterEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FHIRTypes, FHIRResources, FMX.ScrollBox, FMX.Memo, FMX.ListBox, FMX.Edit,
  FMX.StdCtrls, FMX.Controls.Presentation;

type
  TSearchParameterEditorForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edtName: TEdit;
    edtDefinition: TEdit;
    cbxType: TComboBox;
    mDocumentation: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FParam: TFhirCapabilityStatementRestResourceSearchParam;
    procedure SetParam(const Value: TFhirCapabilityStatementRestResourceSearchParam);
  public
    destructor Destroy; override;
    property Param : TFhirCapabilityStatementRestResourceSearchParam read FParam write SetParam;
  end;

var
  SearchParameterEditorForm: TSearchParameterEditorForm;

implementation

{$R *.fmx}

{ TForm1 }

procedure TSearchParameterEditorForm.Button1Click(Sender: TObject);
begin
  param.name := edtName.text;
  param.type_ := TFhirSearchParamTypeEnum(cbxType.ItemIndex);
  param.definition := edtDefinition.Text;
  param.documentation := mDocumentation.Text;
  if not IsValidIdent(param.name) then
    raise Exception.Create('The parameter name "'+param.name+'" is not valid');
  if param.type_ = SearchParamTypeNull then
    raise Exception.Create('Please choose a parameter type');
end;

destructor TSearchParameterEditorForm.Destroy;
begin
  FParam.Free;
  inherited;
end;

procedure TSearchParameterEditorForm.FormShow(Sender: TObject);
begin
  edtName.text := param.name;
  cbxType.ItemIndex := ord(param.type_);
  edtDefinition.Text := param.definition;
  mDocumentation.Text := param.documentation;
end;

procedure TSearchParameterEditorForm.SetParam(const Value: TFhirCapabilityStatementRestResourceSearchParam);
begin
  FParam.Free;
  FParam := Value;
end;

end.
