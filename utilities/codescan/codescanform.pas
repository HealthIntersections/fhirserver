unit codescanform;

{
Copyright (c) 2001+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  TLogEvent = procedure (msg : String; line, ack : boolean) of object;

  { TCodeScannerForm }

  TCodeScannerForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
  private
    FOnExecute: TNotifyEvent;
    FLastWasLine : boolean;
  public
    property OnExecute : TNotifyEvent read FOnExecute write FOnExecute;
    procedure log(msg : String; line, ack : boolean);
  end;

var
  CodeScannerForm: TCodeScannerForm;

implementation

{$R *.lfm}

{ TCodeScannerForm }

procedure TCodeScannerForm.Button1Click(Sender: TObject);
begin
  OnExecute(self);
end;

procedure TCodeScannerForm.log(msg: String; line, ack: boolean);
begin
  if (ack) then
    showMessage(msg)
  else if (line) then
  begin
    if not FLastWasLine then
      memo1.lines.add('');
    memo1.lines.add(msg);
    FLastWasLine := true;
  end
  else
  begin
    FLastWasLine := false;
    memo1.lines[memo1.Lines.count - 1] := memo1.lines[memo1.Lines.count - 1] + msg;
  end;
  Application.ProcessMessages;
end;

end.

