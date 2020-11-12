unit ResDisplayForm;

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


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, fsl_stream, FHIR.Npp.Form, FHIR.Npp.Base,
  Vcl.OleCtrls, SHDocVw, Vcl.ComCtrls, fsl_utilities;

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
    fn := IncludeTrailingPathDelimiter(SystemTemp)+ 'npp-text.html';
    StringToFile(html, fn, TEncoding.UTF8);
    ResourceDisplayForm.WebBrowser1.Navigate('file:'+fn);
    ResourceDisplayForm.ShowModal;
  finally
    ResourceDisplayForm.Free;
  end;
end;

end.
