unit OidFetcher;

{
Copyright (c) 2014+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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
  Windows, Messages, SysUtils, Classes, Graphics, IniFiles,
  Controls, Forms, Dialogs, StdCtrls,
  fsl_wininet, fsl_stream;

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
