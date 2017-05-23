unit FHIRPathDocumentation;


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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFHIRPathDocumentationForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Memo1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Memo1MouseLeave(Sender: TObject);
    procedure Memo1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Memo1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Memo1Exit(Sender: TObject);
    procedure Memo1Enter(Sender: TObject);
    procedure Memo1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FHIRPathDocumentationForm: TFHIRPathDocumentationForm;

implementation

{$R *.dfm}

uses
  FHIRToolboxForm;

procedure TFHIRPathDocumentationForm.Button2Click(Sender: TObject);
begin
  FHIRToolbox.mPath.Text := Memo1.SelText;
end;

procedure TFHIRPathDocumentationForm.Memo1Click(Sender: TObject);
begin
  Button1.Enabled := Length(memo1.SelText) > 0;
end;

procedure TFHIRPathDocumentationForm.Memo1Enter(Sender: TObject);
begin
  Button1.Enabled := Length(memo1.SelText) > 0;
end;

procedure TFHIRPathDocumentationForm.Memo1Exit(Sender: TObject);
begin
  Button1.Enabled := Length(memo1.SelText) > 0;
end;

procedure TFHIRPathDocumentationForm.Memo1KeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  Button1.Enabled := Length(memo1.SelText) > 0;
end;

procedure TFHIRPathDocumentationForm.Memo1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Button1.Enabled := Length(memo1.SelText) > 0;
end;

procedure TFHIRPathDocumentationForm.Memo1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Button1.Enabled := Length(memo1.SelText) > 0;
end;

procedure TFHIRPathDocumentationForm.Memo1MouseLeave(Sender: TObject);
begin
  Button1.Enabled := Length(memo1.SelText) > 0;
end;

procedure TFHIRPathDocumentationForm.Memo1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Button1.Enabled := Length(memo1.SelText) > 0;
end;

end.
