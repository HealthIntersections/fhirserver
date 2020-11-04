unit frm_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TToolkitSettingsForm }

  TToolkitSettingsForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    chkSideBySide: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
  private

  public

  end;

var
  ToolkitSettingsForm: TToolkitSettingsForm;

implementation

{$R *.lfm}

end.

