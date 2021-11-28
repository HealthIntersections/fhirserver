unit dlg_igpub_config;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LclIntf,
  fui_lcl_utilities;

type

  { TIGPublisherConfigForm }

  TIGPublisherConfigForm = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    Button1: TButton;
    edtDevParams: TEdit;
    edtJavaCmd: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label2Click(Sender: TObject);
  private
    FOnReloadVersionList: TNotifyEvent;

  public
     property OnReloadVersionList : TNotifyEvent read FOnReloadVersionList write FOnReloadVersionList;
  end;

var
  IGPublisherConfigForm: TIGPublisherConfigForm;

implementation

{$R *.lfm}

{ TIGPublisherConfigForm }

procedure TIGPublisherConfigForm.FormCreate(Sender: TObject);
begin
  setForOs(btnOk, btnCancel);
end;

procedure TIGPublisherConfigForm.Button1Click(Sender: TObject);
begin
  OnReloadVersionList(self);
end;

procedure TIGPublisherConfigForm.Label2Click(Sender: TObject);
begin
  OpenURL('https://stackoverflow.com/questions/18902934/compile-and-run-eclipse-project-from-command-prompt');
end;

end.

