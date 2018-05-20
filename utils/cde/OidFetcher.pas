unit OidFetcher;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, IniFiles,
  Controls, Forms, Dialogs, StdCtrls,
  FHIR.Support.WinInet, FHIR.Support.Zip, FHIR.Support.Stream;

const
  UM_ACTIVATED = WM_USER + 1;

type
  TOidFetcherForm = class(TForm)
    btnFetch: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    lblStatus: TLabel;
    procedure btnFetchClick(Sender: TObject);
    procedure DoProgress(sender : TObject; msg : String);
    procedure FormActivate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
    wantclose : boolean;
  public
    { Public declarations }
    procedure UMActivated(var Message: TMessage); message UM_ACTIVATED;
  end;

var
  OidFetcherForm: TOidFetcherForm;

implementation

{$R *.dfm}

procedure TOidFetcherForm.btnCancelClick(Sender: TObject);
begin
  wantclose := true;
end;

procedure TOidFetcherForm.btnFetchClick(Sender: TObject);
var
  zip : TFslZipReader;
  http : TFslWinInetClient;
  mem : TFslMemoryStream;
begin
  btnFetch.Enabled := false;
  DoProgress(self, 'Fetching OID pack');
  http := TFslWinInetClient.Create;
  try
    http.SetAddress('http://www.healthintersections.com.au/oids.zip');
    http.requestMethod := 'GET';
    http.response := TFslBuffer.create;
    http.OnProgress := DoProgress;
    http.Execute;
    DoProgress(self, 'Processsing OIDs...');
    mem := TFslMemoryStream.Create;
    try
      mem.Buffer := http.Response.Link;
      zip := TFslZipReader.Create;
      try
        zip.Stream := mem.Link;
        zip.ReadZip;
        zip.Parts[0].SaveToFileName('oids.csv');
      finally
        zip.Free;
      end;
    finally
      mem.Free;
    end;
  finally
    http.free;
  end;
  ModalResult := mrOK;
end;

procedure TOidFetcherForm.DoProgress;
begin
  lblStatus.Caption := msg;
  lblStatus.Update;
  Application.ProcessMessages;
  if WantClose then
  begin
    ModalResult := mrOK;
    abort;
  end;
end;

procedure TOidFetcherForm.FormActivate(Sender: TObject);
begin
  PostMessage(Handle, UM_ACTIVATED, 0, 0);
end;

procedure TOidFetcherForm.UMActivated(var Message: TMessage);
begin
  btnFetchClick(self);
end;

end.
