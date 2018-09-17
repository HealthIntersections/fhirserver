unit ProgressDialog;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{
Simple Dialog that shows while an action is occuring, and
gives the user to cancel the action
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TProgressWindow = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    lbCounter: TLabel;
    Timer: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FStopped : Boolean;
    FMessage: string;
    procedure SetMessage(const Value: string);
  public
    Property Message : string read FMessage write SetMessage;
    property Stopped : boolean read FStopped;
  end;

var
  ProgressWindow: TProgressWindow;

implementation

{$R *.dfm}


procedure TProgressWindow.FormShow(Sender: TObject);
begin
  Timer.Enabled := True;
end;

procedure TProgressWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer.Enabled := False;
  lbCounter.Caption := '0';
end;

procedure TProgressWindow.Button1Click(Sender: TObject);
begin
  FStopped := true;
  Close;
end;

procedure TProgressWindow.SetMessage(const Value: string);
begin
  FMessage := Value;
  Label1.Caption := Value;
  Label1.Update;
end;

procedure TProgressWindow.TimerTimer(Sender: TObject);
var
  cnt: Integer;
begin
  cnt := StrToInt(lbCounter.Caption);
  inc(cnt);
  lbCounter.Caption := IntToStr(cnt);
end;

end.
