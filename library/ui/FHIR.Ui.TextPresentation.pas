unit FHIR.Ui.TextPresentation;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TTextPresentationDialog = class(TForm)
    Panel1: TPanel;
    btnCancel: TButton;
    Panel2: TPanel;
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure showTextPresentation(owner : TComponent; title, text : String);

implementation

{$R *.dfm}

procedure showTextPresentation(owner : TComponent; title, text : String);
var
  TextPresentationDialog: TTextPresentationDialog;
begin
  TextPresentationDialog := TTextPresentationDialog.Create(owner);
  try
    TextPresentationDialog.Caption := title;
    TextPresentationDialog.Memo1.Text := text;
    TextPresentationDialog.ShowModal;
  finally
    TextPresentationDialog.Free;
  end;
end;

end.
