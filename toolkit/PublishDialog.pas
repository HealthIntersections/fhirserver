unit PublishDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Effects;

type
  TBuildDialog = class(TForm)
    Panel1: TPanel;
    Button10: TButton;
    Button2: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BuildDialog: TBuildDialog;

implementation

{$R *.fmx}

end.
