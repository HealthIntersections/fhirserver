unit FHIR.Transformer.MarkdownPreview;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

{$I fhir.inc}

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
