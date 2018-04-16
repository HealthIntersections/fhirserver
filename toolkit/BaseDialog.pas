unit BaseDialog;

interface

uses
  FMX.Forms,
  BaseFrame, ToolkitSettings;

type
  TBaseForm = class (TForm)
  private
    FSettings: TFHIRToolkitSettings;
    FOnWork: TWorkEvent;
    procedure SetSettings(const Value: TFHIRToolkitSettings);
  public
    destructor Destroy; override;

    procedure work(opName : String; canCancel : boolean; proc : TWorkProc);

    property Settings : TFHIRToolkitSettings read FSettings write SetSettings;
    property OnWork : TWorkEvent read FOnWork write FOnWork;
  end;
implementation

{ TBaseForm }

destructor TBaseForm.Destroy;
begin
  FSettings.Free;
  inherited;
end;

procedure TBaseForm.SetSettings(const Value: TFHIRToolkitSettings);
begin
  FSettings.Free;
  FSettings := Value;
end;

procedure TBaseForm.work(opName: String; canCancel: boolean; proc: TWorkProc);
begin
  OnWork(self, opname, canCancel, proc);
end;

end.
