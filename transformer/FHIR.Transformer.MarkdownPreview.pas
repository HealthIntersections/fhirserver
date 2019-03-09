unit FHIR.Transformer.MarkdownPreview;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, ActiveX,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.OleCtrls, SHDocVw,
  Vcl.ExtCtrls,
  ScintEdit, ScintInt, ScintFormats;

type
  TMarkdownPreviewForm = class(TForm)
    Panel1: TPanel;
    btnCancel: TButton;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    WebBrowser1: TWebBrowser;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
  private
    memo : TScintEdit;
    FHTML: String;
    pct : double;
  public
    property html : String read FHTML write FHtml;
    property percent : double read pct write pct;
  end;

var
  MarkdownPreviewForm: TMarkdownPreviewForm;

implementation

{$R *.dfm}

uses
  MarkdownCommonMark;

procedure LoadHtmlIntoBrowser(browser: TWebBrowser; const html: String);
var
  stream : TStringStream;
begin
  //-------------------
  // Load a blank page.
  //-------------------
  browser.Navigate('about:blank');
  while browser.ReadyState <> READYSTATE_COMPLETE do
  begin
    Sleep(5);
    Application.ProcessMessages;
  end;
  //---------------
  // Load the html.
  //---------------
  stream := TStringStream.Create(html, TEncoding.UTF8);
  try
    (browser.Document as IPersistStreamInit).Load(TStreamAdapter.Create(stream));
  finally
    stream.Free;
  end;
end;

procedure TMarkdownPreviewForm.FormCreate(Sender: TObject);
begin
  memo := TScintEdit.create(self);
  memo.Parent := Panel7;
  memo.Align := alClient;
  memo.LineNumbers := true;
  memo.Styler := TXmlStyler.Create(self);
  memo.lines.Add('test');
  pct := 0.5;
end;

procedure TMarkdownPreviewForm.FormResize(Sender: TObject);
begin
 Panel2.Width := trunc(ClientWidth * pct);
end;

procedure TMarkdownPreviewForm.FormShow(Sender: TObject);
begin
  memo.Lines.Text := FHTML;
  memo.ReadOnly := true;
  LoadHtmlIntoBrowser(MarkdownPreviewForm.WebBrowser1, FHtml);
end;

procedure TMarkdownPreviewForm.Splitter1Moved(Sender: TObject);
begin
  pct := Panel2.Width / ClientWidth;
end;

end.
