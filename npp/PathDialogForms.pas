unit PathDialogForms;

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
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, FHIR.Npp.Form, Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls, FHIR.Npp.Base, 
  fhir_objects, fhir_pathengine;

type
  TPathOutcomeDialogMode = (pomError, pomNoMatch, pomMatch);

  TPathDialogForm = class(TNppForm)
    Panel2: TPanel;
    Label1: TLabel;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PathDialogForm: TPathDialogForm;

procedure pathOutcomeDialog(owner : TNppPlugin; path, rtype : String; types : TFHIRTypeDetailsV; mode : TPathOutcomeDialogMode; outcome : String);

implementation

{$R *.dfm}

uses
  FHIR.Npp.Settings;

function summary(types : TArray<String>) : String;
var
  s : String;
  b : TStringBuilder;
  f : boolean;
begin
  if Length(types) = 0 then
    exit('?? unknown');

  f := true;
  b := TStringBuilder.Create;
  try
    for s in types do
    begin
      if f then
        f := false
      else
        b.Append(', ');
      b.Append(s);
    end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;

procedure pathOutcomeDialog(owner : TNppPlugin; path, rtype : String; types : TFHIRTypeDetailsV; mode : TPathOutcomeDialogMode; outcome : String);
var
  t : string;
begin
  if not Settings.NoPathSummary or (mode = pomError) then
  begin
    PathDialogForm := TPathDialogForm.create(owner);
    try
      PathDialogForm.CheckBox1.Checked := Settings.NoPathSummary;
      if types = nil then
        t := ''
      else
        t := types.ToString;
      case mode of
        pomError :
          PathDialogForm.Memo1.Text := 'Path: '+path+#13#10#13#10+'When evaluated against a '+rtype+', this path may return the following types: '+t+#13#10#13#10+'Error Message: '+outcome+#13#10;
        pomNoMatch :
          PathDialogForm.Memo1.Text := 'Path: '+path+#13#10#13#10+'When evaluated against a '+rtype+', this path may return the following types: '+t+#13#10#13#10+'Outcome: '+outcome+#13#10;
        pomMatch :
          PathDialogForm.Memo1.Text := 'Path: '+path+#13#10#13#10+'When evaluated against a '+rtype+', this path may return the following types: '+t+#13#10#13#10+'Outcome: '+outcome+#13#10#13#10+'Matching Items are shown with a green squiggly: '+#13#10;
      end;
      PathDialogForm.ShowModal;
      Settings.NoPathSummary := PathDialogForm.CheckBox1.Checked;
    finally
      FreeAndNil(PathDialogForm);
    end;
  end;
end;

end.
