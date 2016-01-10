unit CDSBrowserForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.OleCtrls, SHDocVw,
  Vcl.ExtCtrls, NppForms;

type
  TCDSBrowser = class(TNppForm)
    Panel1: TPanel;
    WebBrowser1: TWebBrowser;
    btnBack: TButton;
    btnClose: TButton;
    btnRefresh: TButton;
    procedure btnBackClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure WebBrowser1BeforeNavigate2(ASender: TObject;
      const pDisp: IDispatch; const URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CDSBrowser: TCDSBrowser;

implementation

{$R *.dfm}

procedure TCDSBrowser.btnBackClick(Sender: TObject);
begin
  WebBrowser1.GoBack;
end;

procedure TCDSBrowser.btnCloseClick(Sender: TObject);
begin
  close;
end;

procedure TCDSBrowser.btnRefreshClick(Sender: TObject);
begin
  WebBrowser1.Refresh;
end;

procedure TCDSBrowser.WebBrowser1BeforeNavigate2(ASender: TObject; const pDisp: IDispatch; const URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
var
  s : String;
begin
  s := url;
  if s.StartsWith('http://localhost') then
  begin
    cancel := true;
    close;
  end
  else
    cancel := false;
end;

end.
