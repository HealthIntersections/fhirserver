unit ProcessForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, System.ImageList, FMX.ImgList;

type
  TProcessingForm = class(TForm)
    lblOperation: TLabel;
    Button1: TButton;
    ImageList1: TImageList;
    StyleBook1: TStyleBook;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ProcessingForm: TProcessingForm;

implementation

{$R *.fmx}

end.
