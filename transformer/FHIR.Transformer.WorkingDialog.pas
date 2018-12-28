unit FHIR.Transformer.WorkingDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls;

type
  TWorkingForm = class(TForm)
    lblStatus: TLabel;
    pbPercent: TProgressBar;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WorkingForm: TWorkingForm;

implementation

{$R *.dfm}

end.
