unit FHIRPathDocumentation;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFHIRPathDocumentationForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Memo1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Memo1MouseLeave(Sender: TObject);
    procedure Memo1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Memo1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Memo1Exit(Sender: TObject);
    procedure Memo1Enter(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FHIRPathDocumentationForm: TFHIRPathDocumentationForm;

implementation

{$R *.dfm}

uses
  FHIRToolboxForm;

procedure TFHIRPathDocumentationForm.Button2Click(Sender: TObject);
begin
  FHIRToolbox.mPath.Text := Memo1.SelText;
end;

procedure TFHIRPathDocumentationForm.Memo1Click(Sender: TObject);
begin
  Button1.Enabled := Length(memo1.SelText) > 0;
end;

procedure TFHIRPathDocumentationForm.Memo1Enter(Sender: TObject);
begin
  Button1.Enabled := Length(memo1.SelText) > 0;
end;

procedure TFHIRPathDocumentationForm.Memo1Exit(Sender: TObject);
begin
  Button1.Enabled := Length(memo1.SelText) > 0;
end;

procedure TFHIRPathDocumentationForm.Memo1KeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  Button1.Enabled := Length(memo1.SelText) > 0;
end;

procedure TFHIRPathDocumentationForm.Memo1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Button1.Enabled := Length(memo1.SelText) > 0;
end;

procedure TFHIRPathDocumentationForm.Memo1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Button1.Enabled := Length(memo1.SelText) > 0;
end;

procedure TFHIRPathDocumentationForm.Memo1MouseLeave(Sender: TObject);
begin
  Button1.Enabled := Length(memo1.SelText) > 0;
end;

procedure TFHIRPathDocumentationForm.Memo1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Button1.Enabled := Length(memo1.SelText) > 0;
end;

end.
