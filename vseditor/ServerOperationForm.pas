unit ServerOperationForm;

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
  ActiveX,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DProgress, Vcl.StdCtrls, Vcl.ExtCtrls,
  ValueSetEditorCore, FHIRClient;

type
  TServerOperationEvent = procedure (event : TFhirHTTPClientStatusEvent; details : String) of Object;

  TForm2 = class;

  TExecutionThread = class (TThread)
  private
    form : TForm2;
  protected
    procedure Execute; override;
  public
    constructor Create(form : TForm2);
  end;

  TForm2 = class(TForm)
    Panel1: TPanel;
    lblOpName: TLabel;
    DProgressBar1: TDProgressBar;
    Timer1: TTimer;
    btnCancel: TButton;
    lblProgress: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure updateProgress(sender : TObject; details : String);
  private
    { Private declarations }
    started : boolean;
    error : String;
    details : String;
    status : String;
    event : TServerOperationEvent;
    done : boolean;
    thread : TExecutionThread;
  public
    { Public declarations }

  end;

var
  Form2: TForm2;

procedure ServerOperation(op : TServerOperationEvent; details : String; caption : String; canCancel : boolean);

implementation

{$R *.dfm}

procedure ServerOperation(op : TServerOperationEvent; details : String; caption : String; canCancel : boolean);
begin
  Form2.details := details;
  Form2.event := op;
  Form2.error := '';
  Form2.lblOpName.Caption := caption;
  Form2.btnCancel.Enabled := canCancel;
  Form2.started := false;
  Form2.done := false;
  Form2.ShowModal;
  if form2.ModalResult = mrCancel then
    abort
  else if form2.error <> '' then
    raise Exception.Create(form2.error);
end;

{ TExecutionThread }

constructor TExecutionThread.Create(form: TForm2);
begin
  self.form := form;
  inherited Create;
end;

procedure TExecutionThread.Execute;
begin
  CoInitialize(nil);
  try
    try
      form.event(form.updateProgress, form.details);
      form.done := true;
    except
      on e:exception do
      begin
        form.error := e.Message;
        form.done := true;
      end;
    end;
  finally
    CoUninitialize;
  end;
end;

procedure TForm2.btnCancelClick(Sender: TObject);
begin
  thread.Terminate;
end;

procedure TForm2.FormActivate(Sender: TObject);
var
  thread : TExecutionThread;
begin
  if not started then
  begin
    started := true;
    thread := TExecutionThread.Create(self);
    thread.FreeOnTerminate := true;
  end;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  lblProgress.Caption := status;
  if done then
    ModalResult := mrOk;
end;

procedure TForm2.updateProgress(sender : TObject; details: String);
begin
  status := details;
end;

end.
