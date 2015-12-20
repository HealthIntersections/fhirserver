unit ClosureManagerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Grids, ValueSetEditorCore;

type
  TClosureManagerForm = class(TForm)
    Panel1: TPanel;
    grid: TStringGrid;
    Label1: TLabel;
    cbxClosures: TComboBox;
    Button1: TButton;
    Bevel1: TBevel;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbxClosuresChange(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FContext: TValueSetEditorContext;
    procedure SetContext(const Value: TValueSetEditorContext);
    { Private declarations }
  public
    { Public declarations }
    Property Context : TValueSetEditorContext read FContext write SetContext;
  end;

var
  ClosureManagerForm: TClosureManagerForm;

implementation

{$R *.dfm}

{ TForm3 }

procedure TClosureManagerForm.Button1Click(Sender: TObject);
var
  s : String;
begin
  if InputQuery('Closure Name', 'New Closure', s) then
  begin
    FContext.AddClosure(s);
    cbxClosures.Items.Add(s);
    cbxClosures.ItemIndex := cbxClosures.Items.Count - 1;
    cbxClosuresChange(nil);
  end;
end;

procedure TClosureManagerForm.Button4Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TClosureManagerForm.cbxClosuresChange(Sender: TObject);
begin
  grid.ColCount := 1;
  grid.RowCount := 1;
  grid.Cells[0, 0] := 'Not Done Yet';
end;

procedure TClosureManagerForm.FormDestroy(Sender: TObject);
begin
  FContext.Free;
end;

procedure TClosureManagerForm.FormShow(Sender: TObject);
begin
  FContext.LoadClosures(cbxClosures.Items);
  cbxClosuresChange(nil);
end;

procedure TClosureManagerForm.SetContext(const Value: TValueSetEditorContext);
begin
  FContext.Free;
  FContext := Value;
end;

end.
