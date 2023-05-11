unit BaseDialog;

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

interface

uses
  FMX.Forms,
  BaseFrame, ToolkitSettings;

type
  TBaseForm = class (TForm)
  private
    FSettings: TFHIRToolkitSettings;
    FOnWork: TWorkEvent;
    procedure SetSettings(const Value: TFHIRToolkitSettings);
  public
    destructor Destroy; override;

    procedure work(opName : String; canCancel : boolean; proc : TWorkProc);

    property Settings : TFHIRToolkitSettings read FSettings write SetSettings;
    property OnWork : TWorkEvent read FOnWork write FOnWork;
  end;
implementation

{ TBaseForm }

destructor TBaseForm.Destroy;
begin
  FSettings.Free;
  inherited;
end;

procedure TBaseForm.SetSettings(const Value: TFHIRToolkitSettings);
begin
  FSettings.Free;
  FSettings := Value;
end;

procedure TBaseForm.work(opName: String; canCancel: boolean; proc: TWorkProc);
begin
  OnWork(self, opname, canCancel, proc);
end;

end.
