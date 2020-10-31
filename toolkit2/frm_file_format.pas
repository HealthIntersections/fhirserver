unit frm_file_format;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFileFormatChooser }

  TFileFormatChooser = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure ListBox1DblClick(Sender: TObject);
  private

  public

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

end.

