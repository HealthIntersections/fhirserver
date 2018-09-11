unit QuestionnaireContextDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.Controls.Presentation,
  FHIR.Version.Types;

type
  TQuestionnaireContextForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    procedure FormDestroy(Sender: TObject);
  private
    FContext: TFHIRExtension;
    procedure SetContext(const Value: TFHIRExtension);
  public
    property Context : TFHIRExtension read FContext write SetContext;
  end;

var
  QuestionnaireContextForm: TQuestionnaireContextForm;

implementation

{$R *.fmx}

{ TQuestionnaireContextForm }

procedure TQuestionnaireContextForm.FormDestroy(Sender: TObject);
begin
  FContext.free;
end;

procedure TQuestionnaireContextForm.SetContext(const Value: TFHIRExtension);
begin
  FContext.free;
  FContext := Value;
end;

end.
