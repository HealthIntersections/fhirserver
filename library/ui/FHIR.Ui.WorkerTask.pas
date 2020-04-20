unit FHIR.Ui.WorkerTask;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  FHIR.Support.Base;

type
  TWorkingForm = class(TForm)
    lblStatus: TLabel;
    pbPercent: TProgressBar;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TWorkerObject = class abstract (TFslObject)
  private
    FForm : TWorkingForm;
  protected
    procedure execute; virtual; abstract;
    function caption : String; virtual; abstract;
    function canCancel : boolean; virtual;
    procedure progress(sender : TObject; pct : integer; done : boolean; desc : String); // may raise EAbort
  public
    procedure runTask(owner : TComponent);
  end;

var
  WorkingForm: TWorkingForm;

implementation

{$R *.dfm}

{ TWorkerObject }


function TWorkerObject.canCancel: boolean;
begin
  result := false;
end;

procedure TWorkerObject.progress(sender : TObject; pct : integer; done : boolean; desc : String);
begin
  FForm.lblStatus.caption := desc;
  FForm.lblStatus.Update;
  FForm.pbPercent.Position := pct;
  FForm.pbPercent.Update;
  Application.ProcessMessages;
end;

procedure TWorkerObject.runTask(owner: TComponent);
begin
  FForm := TWorkingForm.Create(nil);
  try
    FForm.Caption := caption;
    if canCancel then
      FForm.BorderIcons := [biSystemMenu]
    else
      FForm.BorderIcons := [];
    FForm.Show;
    Application.ProcessMessages;
    execute;
  finally
    FForm.Free;
  end;
end;

end.
