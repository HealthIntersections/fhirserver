unit CodeGenerationForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  nppforms,
  FHIRResources, FHIRCodeGenerator, FHIRContext;

type
  TCodeGeneratorForm = class(TNppForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Button1: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    cbxLanguage: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure cbxLanguageChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FResource : TFHIRResource;
    FContext: TWorkerContext;
    procedure SetResource(const Value: TFHIRResource);
    procedure SetContext(const Value: TWorkerContext);
    { Private declarations }
  public
    { Public declarations }
    property Context : TWorkerContext read FContext write SetContext;
    property Resource : TFHIRResource read FResource write SetResource;
  end;

var
  CodeGeneratorForm: TCodeGeneratorForm;

implementation

{$R *.dfm}

procedure TCodeGeneratorForm.cbxLanguageChange(Sender: TObject);
var
  codegen : TFHIRCodeGenerator;
begin
  case cbxLanguage.ItemIndex of
    0: codegen := TFHIRCodeGeneratorJavaRI.create;
    1: codegen := TFHIRCodeGeneratorJavaHapi.create;
    2: codegen := TFHIRCodeGeneratorPascal.create;
    3: codegen := TFHIRCodeGeneratorDotNet.create;
  end;
  try
    codegen.Resource := Resource.Link;
    codegen.Context := Context.link;

    Memo1.Lines.Text := codegen.generate;
  finally
    codegen.free;
  end;
end;

procedure TCodeGeneratorForm.FormDestroy(Sender: TObject);
begin
  FContext.Free;
  FResource.Free;
end;

procedure TCodeGeneratorForm.FormShow(Sender: TObject);
begin
  cbxLanguageChange(nil);
end;

procedure TCodeGeneratorForm.SetContext(const Value: TWorkerContext);
begin
  FContext.Free;
  FContext := Value;
end;

procedure TCodeGeneratorForm.SetResource(const Value: TFHIRResource);
begin
  FResource.Free;
  FResource := Value;
end;

end.
