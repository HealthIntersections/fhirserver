program UITester;

{
Copyright (c) 1996+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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


uses
  FastMM4 in '..\dependencies\FMM\FastMM4.pas',
  Vcl.Forms,
  UITesterForm in 'UITesterForm.pas' {Form10},
  FHIR.Ui.Graph in '..\library\ui\FHIR.Ui.Graph.pas',
  FHIR.Support.Utilities in '..\library\support\FHIR.Support.Utilities.pas',
  FHIR.Support.Base in '..\library\support\FHIR.Support.Base.pas',
  FHIR.Ui.GraphDesigner in '..\library\ui\FHIR.Ui.GraphDesigner.pas' {GraphDesignerForm},
  FastMM4Messages in '..\dependencies\FMM\FastMM4Messages.pas',
  FHIR.Support.Fpc in '..\library\support\FHIR.Support.Fpc.pas',
  FHIR.Support.Collections in '..\library\support\FHIR.Support.Collections.pas',
  FHIR.Support.Stream in '..\library\support\FHIR.Support.Stream.pas',
  GraphTester in 'GraphTester.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm10, Form10);
  Application.CreateForm(TGraphDesignerForm, GraphDesignerForm);
  Application.Run;
end.
