unit install_log;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ComCtrls,
  ExtCtrls, StdCtrls;

type

  { TInstallProgressForm }

  TInstallProgressForm = class(TForm)
    BitBtn1: TBitBtn;
    btnDBTest3: TBitBtn;
    lblStatus: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
  private

  public

  end;

var
  InstallProgressForm: TInstallProgressForm;

implementation

{$R *.lfm}

end.

