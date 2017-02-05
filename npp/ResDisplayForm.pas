unit ResDisplayForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, TextUtilities, NppForms, NppPlugin,
  Vcl.OleCtrls, SHDocVw, Vcl.ComCtrls, FileSupport, SystemSupport;

type
  TResourceDisplayForm = class(TNppForm)
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    sd: TSaveDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    WebBrowser1: TWebBrowser;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ResourceDisplayForm: TResourceDisplayForm;

procedure ShowResource(owner : TNppPlugin; caption, html, Content : String);

implementation

{$R *.dfm}

procedure TResourceDisplayForm.Button2Click(Sender: TObject);
begin
  Memo1.CopyToClipboard;
end;

procedure TResourceDisplayForm.Button3Click(Sender: TObject);
begin
  if sd.Execute then
    StringToFile(memo1.Text, sd.FileName, TEncoding.UTF8);
end;

procedure ShowResource(owner : TNppPlugin; caption, html, Content : String);
var
  fn : String;
begin
  ResourceDisplayForm := TResourceDisplayForm.Create(owner);
  try
    ResourceDisplayForm.Caption := caption;
    ResourceDisplayForm.Memo1.Text := Content;
    fn := IncludeTrailingBackslash(SystemTemp)+ 'npp-text.html';
    StringToFile(html, fn, TEncoding.UTF8);
    ResourceDisplayForm.WebBrowser1.Navigate('file:'+fn);
    ResourceDisplayForm.ShowModal;
  finally
    ResourceDisplayForm.Free;
  end;
end;

end.
