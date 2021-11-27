unit dlg_igpub_config;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LclIntf,
  fui_lcl_utilities;

type

  { TIGPublisherConfigForm }

  TIGPublisherConfigForm = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    Button1: TButton;
    edtDevParams: TEdit;
    edtJavaCmd: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label2Click(Sender: TObject);
  private
    FOnReloadVersionList: TNotifyEvent;

  public
     property OnReloadVersionList : TNotifyEvent read FOnReloadVersionList write FOnReloadVersionList;
  end;

var
  IGPublisherConfigForm: TIGPublisherConfigForm;

implementation

{$R *.lfm}

{ TIGPublisherConfigForm }

procedure TIGPublisherConfigForm.FormCreate(Sender: TObject);
begin
  setForOs(btnOk, btnCancel);
end;

procedure TIGPublisherConfigForm.Button1Click(Sender: TObject);
begin
  OnReloadVersionList(self);
end;

procedure TIGPublisherConfigForm.Label2Click(Sender: TObject);
begin
  OpenURL('https://stackoverflow.com/questions/18902934/compile-and-run-eclipse-project-from-command-prompt');
end;

end.

