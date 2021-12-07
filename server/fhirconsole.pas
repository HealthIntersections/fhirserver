program fhirconsole;

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

uses
  {$IFNDEF WINDOWS}
  cmem, cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils, Forms, Dialogs, datetimectrls, lazcontrols,
  IdOpenSSLLoader,
  fsl_base, fsl_fpc, fsl_utilities, fsl_openssl,
  fdb_odbc_fpc,
  console_form,
  console_tx_edit, console_ep_edit, install_form, install_log, installer, 
  test_form;

{$R *.res}

var
  ok : boolean;
begin
  try
    InitialiseODBC;
    {$IFNDEF STATICLOAD_OPENSSL}
    {$IFDEF WINDOWS}
    GetOpenSSLLoader.OpenSSLPath := ExtractFilePath(Paramstr(0));
    {$ENDIF}
    {$IFDEF OSX}
    GetOpenSSLLoader.OpenSSLPath := '/opt/homebrew/Cellar/openssl@1.1/1.1.1l/lib/';
    {$ENDIF}
    {$ENDIF}

    InitOpenSSL;
    ok := true;
  except
    on e : Exception do
    begin
      MessageDlg('Initialization failure', e.message, mtError, [mbClose], 0);
      ok := false;
    end;
  end;

  if ok then
  begin
    RequireDerivedFormResource := True;
    Application.Scaled := True;

    Application.Initialize;
    Application.CreateForm(TMainConsoleForm, MainConsoleForm);
    Application.Run;
  end;
end.

