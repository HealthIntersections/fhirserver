unit frm_about;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, LclIntf,
  fsl_utilities,
  ftk_version;

type

  { TToolkitAboutForm }

  TToolkitAboutForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Button1: TButton;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblToolkit: TLabel;
    mInfo: TMemo;
    Panel1: TPanel;
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
  private

  public

  end;

var
  ToolkitAboutForm: TToolkitAboutForm;

implementation

{$R *.lfm}

{ TToolkitAboutForm }

procedure TToolkitAboutForm.Image1Click(Sender: TObject);
begin
  OpenURL('http://www.healthintersections.com.au');
end;

procedure TToolkitAboutForm.Label1Click(Sender: TObject);
begin
  OpenURL('https://github.com/HealthIntersections/fhirserver/blob/master/toolkit2/readme.md');
end;

procedure TToolkitAboutForm.Label2Click(Sender: TObject);
begin
  OpenURL('https://github.com/HealthIntersections/fhirserver/blob/master/install/licence.txt');
end;

procedure TToolkitAboutForm.FormShow(Sender: TObject);
var
  dt : TFslDateTime;
  v : String;
  cmd : TCommandLineParameters;
begin
  v := TOOLKIT_VERSION;
  mInfo.Lines.clear;
  if (v.EndsWith('-SNAPSHOT')) then
  begin
    mInfo.Lines.Add('Unreleased version (work in progress)');
    v := v.replace('-SNAPSHOT', '');
  end
  else
  begin
    dt := TFslDateTime.fromHL7(TOOLKIT_RELEASE_DATE);
    mInfo.Lines.Add('Released '+dt.truncToDay.toString+' ('+DescribePeriod(now - dt.DateTime)+' Ago)');
  end;

  lblToolkit.Caption := 'FHIR Toolkit Version '+v;
  {$IFOPT D+}
  mInfo.Lines.Add('This is the debug version');
  {$ENDIF}

  cmd := TCommandLineParameters.create;
  try
    mInfo.Lines.add('Run as "'+cmd.AsString+'"');
    mInfo.Lines.add('Host is '+SystemPlatform+' '+DescribeBytes(SystemMemory.physicalMem, true)+'/' + DescribeBytes(SystemMemory.virtualMem, true));
  finally
    cmd.free;
  end;
end;

end.

