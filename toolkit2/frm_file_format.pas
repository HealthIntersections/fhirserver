unit frm_file_format;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls;

type

  { TFileFormatChooser }

  TFileFormatChooser = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    ListBox1: TListBox;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private

  public
    procedure setFHIRResource;
  end;

var
  FileFormatChooser: TFileFormatChooser;

implementation

{$R *.lfm}

{ TFileFormatChooser }

procedure TFileFormatChooser.ListBox1DblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFileFormatChooser.setFHIRResource;
begin
  ListBox1.ItemIndex := 0;
  ListBox1Click(self);
end;

procedure TFileFormatChooser.ListBox1Click(Sender: TObject);
begin
  btnOk.enabled := ListBox1.ItemIndex > -1;
  if ListBox1.ItemIndex = 0 then
  begin
    btnOk.caption := '&Next >>';
    btnOk.modalresult := mrNone;
  end
  else
  begin
    btnOk.caption := 'OK';
    btnOk.modalresult := mrOK;
  end;
end;

procedure TFileFormatChooser.btnOkClick(Sender: TObject);
begin
  if ListBox1.ItemIndex = 0 then
  begin
    PageControl1.ActivePage := TabSheet2;
    btnOk.caption := 'OK';
    btnOk.modalresult := mrOK;
    btnCancel.caption := 'Back';
    btnCancel.ModalResult := mrNone;
  end;
end;

procedure TFileFormatChooser.btnCancelClick(Sender: TObject);
begin
  PageControl1.ActivePage := TabSheet1;
  btnCancel.caption := 'Cancel';
  btnCancel.ModalResult := mrCancel;
  ListBox1Click(self);
end;

end.

