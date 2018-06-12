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
  FastMM4 in '..\Libraries\FMM\FastMM4.pas',
  Vcl.Forms,
  UITesterForm in 'UITesterForm.pas' {Form10},
  FHIR.Ui.Graph in '..\Libraries\ui\FHIR.Ui.Graph.pas',
  FHIR.Support.System in '..\reference-platform\support\FHIR.Support.System.pas',
  FHIR.Support.Strings in '..\reference-platform\support\FHIR.Support.Strings.pas',
  FHIR.Support.Objects in '..\reference-platform\support\FHIR.Support.Objects.pas',
  FHIR.Support.Generics in '..\reference-platform\support\FHIR.Support.Generics.pas',
  FHIR.Support.Exceptions in '..\reference-platform\support\FHIR.Support.Exceptions.pas',
  FHIR.Ui.GraphDesigner in '..\Libraries\ui\FHIR.Ui.GraphDesigner.pas' {GraphDesignerForm},
  FastMM4Messages in '..\Libraries\FMM\FastMM4Messages.pas',
  FHIR.Support.Fpc in '..\reference-platform\support\FHIR.Support.Fpc.pas',
  FHIR.Support.Math in '..\reference-platform\support\FHIR.Support.Math.pas',
  FHIR.Support.DateTime in '..\reference-platform\support\FHIR.Support.DateTime.pas',
  FHIR.Support.Collections in '..\reference-platform\support\FHIR.Support.Collections.pas',
  FHIR.Support.Stream in '..\reference-platform\support\FHIR.Support.Stream.pas',
  FHIR.Support.Binary in '..\reference-platform\support\FHIR.Support.Binary.pas',
  FHIR.Support.Text in '..\reference-platform\support\FHIR.Support.Text.pas',
  FHIR.Support.Decimal in '..\reference-platform\support\FHIR.Support.Decimal.pas',
  GraphTester in 'GraphTester.pas',
  FHIR.Tools.ObsGraph in '..\Libraries\ui\FHIR.Tools.ObsGraph.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm10, Form10);
  Application.CreateForm(TGraphDesignerForm, GraphDesignerForm);
  Application.Run;
end.
