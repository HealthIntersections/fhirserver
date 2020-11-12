unit ValidationOutcomes;

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


// todo:
//   look ahead on search parameters on past values by name
//   better date entry

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, clipbrd, Vcl.OleCtrls,
  SHDocVw, ActiveX,
  VirtualTrees, FHIR.Npp.Form, FHIR.Npp.Base,
  fsl_utilities, fsl_stream, fsl_base,
  fhir_objects, fhir_common, fhir_factory, fhir_narrative;

const
  MIN_COL_WIDTH = 260;
  SEARCH_PANEL_HEIGHT = 26;

type
  TValidationOutcomeForm = class(TNppForm)
    Panel2: TPanel;
    btnCancel: TButton;
    btnOpen: TButton;
    CheckBox1: TCheckBox;
    Panel1: TPanel;
    WebBrowser1: TWebBrowser;
    sd: TSaveDialog;
    procedure btnOpenClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    html : String;
    procedure loadHtml(s :  String);
  public
    { Public declarations }
  end;

var
  ValidationOutcomeForm: TValidationOutcomeForm;

function ValidationSummary(owner : TNppPlugin; Context : TFHIRWorkerContextWithFactory; outcome : TFslList<TFHIROperationOutcomeIssueW>) : boolean;
function ValidationError(owner : TNppPlugin; message : String) : boolean;

implementation

{$R *.dfm}

uses
  FHIR.Npp.Settings;

function ValidationSummary(owner : TNppPlugin; Context : TFHIRWorkerContextWithFactory; outcome : TFslList<TFHIROperationOutcomeIssueW>) : boolean;
var
  op : TFHIROperationOutcomeW;
  iss : TFhirOperationOutcomeIssueW;
  gen : TFHIRNarrativeGeneratorBase;
begin
  op := context.Factory.wrapOperationOutcome(context.Factory.makeByName('OperationOutcome') as TFHIRResourceV);
  try
    for iss in outcome do
      op.addIssue(iss, false);
    gen := context.Factory.makeGenerator(context.link);
    try
      gen.description := 'Validation Outcomes';
      gen.generate(op.Resource);
    finally
      gen.Free;
    end;
    result := not Settings.NoValidationSummary;
    if result then
    begin
      ValidationOutcomeForm := TValidationOutcomeForm.create(owner);
      try
        ValidationOutcomeForm.loadHtml(context.Factory.getXhtml(op.Resource).AsHtmlPage);
        ValidationOutcomeForm.ShowModal;
      finally
        FreeAndNil(ValidationOutcomeForm);
      end;
    end;
  finally
    op.Free;
  end;
end;

function ValidationError(owner : TNppPlugin; message : String) : boolean;
begin
  result := not Settings.NoValidationSummary;
  if result then
  begin
    ValidationOutcomeForm := TValidationOutcomeForm.create(owner);
    try
      ValidationOutcomeForm.loadHtml('<html><body>Error validating: '+message+'</body></html>');
      ValidationOutcomeForm.ShowModal;
    finally
      FreeAndNil(ValidationOutcomeForm);
    end;
  end;
end;


{ TValidationOutcomeForm }

procedure TValidationOutcomeForm.btnOpenClick(Sender: TObject);
begin
  if sd.Execute then
    StringToFile(html, sd.FileName, TEncoding.UTF8);
end;

procedure TValidationOutcomeForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  Settings.NoValidationSummary := CheckBox1.Checked;
end;

procedure TValidationOutcomeForm.loadHtml(s: String);
var
  fn : String;
begin
  fn := IncludeTrailingPathDelimiter(SystemTemp)+'validation-outcomes-npp-fhir.html';
  StringToFile(s, fn, TEncoding.UTF8);
  WebBrowser1.Navigate('file://'+fn);
  html := s;
end;

end.
