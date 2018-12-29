unit FHIR.Transformer.FileChangedDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList, Vcl.ImgList,
  Vcl.StdCtrls;

type
  TFileChangedForm = class(TForm)
    lblinfo: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ImageList1: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FileChangedForm: TFileChangedForm;

implementation

{$R *.dfm}

end.
