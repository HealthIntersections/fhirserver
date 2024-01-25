unit dlg_server_upload;

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

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  fsl_npm_client,
  fhir_factory, fhir_client, fhir_package_upload;

type

  { TServerPackageUploadForm }

  TServerPackageUploadForm = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    CheckGroup1: TCheckGroup;
    CheckGroup2: TCheckGroup;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
  private
    FClient: TFHIRClientV;
    FFactory: TFHIRFactory;
    procedure SetClient(AValue: TFHIRClientV);
    procedure SetFactory(AValue: TFHIRFactory);

  public
    destructor Destroy; override;

    property client : TFHIRClientV read FClient write SetClient;
    property factory : TFHIRFactory read FFactory write SetFactory;
  end;

var
  ServerPackageUploadForm: TServerPackageUploadForm;

implementation

{$R *.lfm}

{ TServerPackageUploadForm }

procedure TServerPackageUploadForm.SetClient(AValue: TFHIRClientV);
begin
  FClient.free;
  FClient := AValue;
end;

procedure TServerPackageUploadForm.SetFactory(AValue: TFHIRFactory);
begin
  FFactory.free;
  FFactory := AValue;
end;

destructor TServerPackageUploadForm.Destroy;
begin
  FClient.free;
  FFactory.free;
  inherited Destroy;
end;

end.

