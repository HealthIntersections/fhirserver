unit fhir_tools_settings;

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

{$i fhir.inc}

interface

uses
  IniFiles,
  {$IFNDEF WINDOWS}
  LazFileUtils,
  {$ENDIF}
  fsl_base, fsl_utilities;

type
  TToolsGlobalSettings = class (TFslObject)
  private
    FNpmPath : String;
    FRubyPath : String;
    FTestsPath : String;
    FComparePath : String;
    FTempPath : String;
    FTestIgsPath : String;
    FFhirServerPath: String;
    FMarkdownPath: String;
    procedure load;
  public
    property npmPath : String read FNpmPath;
    property rubyPath : String read FRubyPath;
    property testsPath : String read FTestsPath;
    property comparePath : String read FComparePath;
    property tempPath : String read FTempPath;
    property testIgsPath : String read FTestIgsPath;
    property fhirServerPath : String read FFhirServerPath;
    property markdownPath : String read FMarkdownPath;
  end;

var
  ToolsGlobalSettings : TToolsGlobalSettings;

implementation

{ TToolsGlobalSettings }

procedure TToolsGlobalSettings.load;
var
  fn : String;
  ini : TIniFile;
begin
  {$IFDEF WINDOWS}
  fn := FilePath([UserFolder, '.fhir', 'fhir-tool-settings.conf']);
  {$ELSE}
  fn := ExpandFileNameUTF8('~/.fhir/fhir-tool-settings.conf');
  {$ENDIF}

  ini := TIniFile.Create(fn);
  try
    FnpmPath := ini.ReadString('paths', 'npm', '');
    FrubyPath := ini.ReadString('paths', 'ruby', '');
    FtestsPath := ini.ReadString('paths', 'tests', '');
    FcomparePath := ini.ReadString('paths', 'compare', '');
    FtempPath := ini.ReadString('paths', 'temp', '');
    FtestIgsPath := ini.ReadString('paths', 'test-igs', '');
    FFhirServerPath := ini.ReadString('paths', 'server', '');
    FMarkdownPath := ini.ReadString('paths', 'markdown', '');
  finally
    ini.free;
  end;
end;

initialization
  ToolsGlobalSettings := TToolsGlobalSettings.Create;
  ToolsGlobalSettings.load;
finalization
  ToolsGlobalSettings.free;
end.
