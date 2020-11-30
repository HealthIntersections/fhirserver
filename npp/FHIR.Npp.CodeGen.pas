unit FHIR.Npp.CodeGen;

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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ClipBrd,
  FHIR.Npp.Form,
  fhir_objects, fhir_factory, 
  fhir_codegen;

type
  TCodeGeneratorForm = class(TNppForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Button1: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    cbxLanguage: TComboBox;
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure cbxLanguageChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FResource : TFHIRResourceV;
    FContext: TFHIRWorkerContextWithFactory;
    procedure SetResource(const Value: TFHIRResourceV);
    procedure SetContext(const Value: TFHIRWorkerContextWithFactory);
    { Private declarations }
  public
    { Public declarations }
    property Context : TFHIRWorkerContextWithFactory read FContext write SetContext;
    property Resource : TFHIRResourceV read FResource write SetResource;
  end;

var
  CodeGeneratorForm: TCodeGeneratorForm;

implementation

{$R *.dfm}

procedure TCodeGeneratorForm.Button2Click(Sender: TObject);
var
  clp : TClipboard;
begin
  clp := TClipboard.Create;
  try
    if memo1.SelText <> '' then
      clp.AsText := memo1.SelText
    else
      clp.AsText := memo1.Text;
  finally
    clp.Free;
  end;
end;

procedure TCodeGeneratorForm.cbxLanguageChange(Sender: TObject);
var
  codegen : TFHIRCodeGenerator;
begin
  case cbxLanguage.ItemIndex of
    0: codegen := TFHIRCodeGeneratorJavaRI.create;
    1: codegen := TFHIRCodeGeneratorJavaHapi.create;
    2: codegen := TFHIRCodeGeneratorPascal.create;
    3: codegen := TFHIRCodeGeneratorDotNet.create;
  else
    raise EFHIRException.create('Unknown language');
  end;
  try
    codegen.Resource := Resource.Link;
    codegen.Context := Context.link;

    Memo1.Lines.Text := codegen.generate;
  finally
    codegen.free;
  end;
end;

procedure TCodeGeneratorForm.FormDestroy(Sender: TObject);
begin
  FContext.Free;
  FResource.Free;
end;

procedure TCodeGeneratorForm.FormShow(Sender: TObject);
begin
  cbxLanguageChange(nil);
end;

procedure TCodeGeneratorForm.SetContext(const Value: TFHIRWorkerContextWithFactory);
begin
  FContext.Free;
  FContext := Value;
end;

procedure TCodeGeneratorForm.SetResource(const Value: TFHIRResourceV);
begin
  FResource.Free;
  FResource := Value;
end;

end.
