unit CodeGenerationForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ClipBrd,
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
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure cbxLanguageChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FResource : TFHIRResource;
    FContext: TFHIRWorkerContext;
    procedure SetResource(const Value: TFHIRResource);
    procedure SetContext(const Value: TFHIRWorkerContext);
    { Private declarations }
  public
    { Public declarations }
    property Context : TFHIRWorkerContext read FContext write SetContext;
    property Resource : TFHIRResource read FResource write SetResource;
  end;

var
  CodeGeneratorForm: TCodeGeneratorForm;

implementation

{$R *.dfm}

procedure TCodeGeneratorForm.Button2Click(Sender: TObject);
var
  clp : TClipboard;
begin
  clp := TClipboard.Create;
  try
    if memo1.SelText <> '' then
      clp.AsText := memo1.SelText
    else
      clp.AsText := memo1.Text;
  finally
    clp.Free;
  end;
end;

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

procedure TCodeGeneratorForm.SetContext(const Value: TFHIRWorkerContext);
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
