unit FHIR.Ui.WorkerTask;

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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  fsl_base;

type
  TWorkingForm = class(TForm)
    lblStatus: TLabel;
    pbPercent: TProgressBar;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TWorkerObject = class abstract (TFslObject)
  private
    FForm : TWorkingForm;
  protected
    procedure execute; virtual; abstract;
    function caption : String; virtual; abstract;
    function canCancel : boolean; virtual;
    procedure progress(sender : TObject; pct : integer; done : boolean; desc : String); // may raise EAbort
  public
    procedure runTask(owner : TComponent);
  end;

var
  WorkingForm: TWorkingForm;

implementation

{$R *.dfm}

{ TWorkerObject }

function TWorkerObject.canCancel: boolean;
begin
  result := false;
end;

procedure TWorkerObject.progress(sender : TObject; pct : integer; done : boolean; desc : String);
begin
  FForm.lblStatus.caption := desc;
  FForm.lblStatus.Update;
  FForm.pbPercent.Position := pct;
  FForm.pbPercent.Update;
  Application.ProcessMessages;
end;

procedure TWorkerObject.runTask(owner: TComponent);
begin
  FForm := TWorkingForm.Create(nil);
  try
    FForm.Caption := caption;
    if canCancel then
      FForm.BorderIcons := [biSystemMenu]
    else
      FForm.BorderIcons := [];
    FForm.Show;
    Application.ProcessMessages;
    execute;
  finally
    FForm.Free;
  end;
end;

end.
