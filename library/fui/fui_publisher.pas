unit fui_publisher;

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

{$I fhir.inc}

interface

uses
  Windows,
  SysUtils, Classes, IniFiles, Forms, Vcl.Dialogs,
  JclSysUtils,
  fsl_base, fsl_threads;

type
  TProgressProc = procedure (pct : integer);
  TLogProcedure = procedure (msg : String);

  TFHIRIGPublisher = class;

  TPublishThread = class (TFslThread)
  private
    FOwner : TFHIRIGPublisher;
    FAbort : boolean;
    FOk : boolean;
    FRunning : boolean;
    cmd : String;
  protected
    Procedure Execute; Override;
  public
    constructor create(owner : TFHIRIGPublisher; cmd : string);
  end;

  TFHIRIGPublisher = class (TFslObject)
  private
    FJarFile : String;
    FIni : TIniFile;
    procedure cmdOutput(const Text: string);
  public
    constructor Create(ini : TIniFile);
    destructor Destroy;

    // check that the jar location is known, and if it's not, ask the user how to set it up
    function Configure(owner : TComponent) : boolean;

    // check that the jar is up to date, and it's not, return an error
    // message to the user to help choose whether to update it
    // folder is optional
    function needsUpgrade(folder : String; var msg: String) : boolean;

    // actually upgrade the jarl; call progressProc during the download
    procedure upgrade(folder : String; progressProc : TProgressProc);

    // execute the pulisher, return true if it succeeds, and send output
    // from the publisher to logProc
    function execute(folder : String; logProc : TLogProcedure) : boolean;

    property Location : String read FJarFile;
  end;

implementation

{ TPublishThread }

constructor TPublishThread.create(owner: TFHIRIGPublisher; cmd: string);
begin
  inherited create;
  self.FOwner := owner;
  self.cmd := cmd;
end;

Procedure TPublishThread.Execute;
var
  c : cardinal;
begin
  Fabort := false;
  FRunning := true;
  c := 0;
  try
    c := JclSysUtils.execute(cmd, FOwner.cmdOutput, false, @Fabort);
    if (c <> 0) and (c <> 1223) and (c <> 1) then
    begin
      FOwner.cmdOutput('Error running IG: '+SysErrorMessage(GetLastError));
      FOk := false;
    end
    else
      FOk := true;
  finally
    FRunning := false;
  end;
end;

{ TFHIRIGPublisher }

constructor TFHIRIGPublisher.Create(ini: TIniFile);
begin
  Inherited Create;
  FIni := ini;
end;

destructor TFHIRIGPublisher.Destroy;
begin
  Inherited;
end;

procedure TFHIRIGPublisher.cmdOutput(const Text: string);
begin

end;

function TFHIRIGPublisher.Configure(owner : TComponent): boolean;
var
  od : TFileOpenDialog;
begin
  FJarFile := FIni.ReadString('fhir-tools', 'jar', ''); // 'C:\work\org.hl7.fhir\latest-ig-publisher\org.hl7.fhir.publisher.jar');
  if FileExists(FJarFile) then
    result := true
  else
  begin
    od := TFileOpenDialog.Create(owner);
    try
      od.ClientGuid := '{D807B1E2-67B6-473E-B17C-85636B9CA493}';
      od.DefaultExtension := 'jar';
      od.FileName := 'org.hl7.fhir.publisher.jar';
      with od.FileTypes.Add do
      begin
        DisplayName := 'Jar Files (*.jar)';
        FileMask := '*.jar';
      end;
      with od.FileTypes.Add do
      begin
        DisplayName := 'All Files';
        FileMask := '*.*';
      end;
      od.Options := [fdoFileMustExist];
      od.Title := 'FIND IG Publisher Jar file';
      result := od.Execute;
      if (result) then
      begin
        FJarFile := od.FileName;
        FIni.writeString('tools', 'jar', FJarFile);
      end;
    finally
      od.Free;
    end;
  end;
end;

function TFHIRIGPublisher.execute(folder : String; logProc: TLogProcedure): boolean;
var
  thread : TPublishThread;
begin
//  raise Exception.Create('Not done yet');
//    if fileExists(ExtractFileDir(ExcludeTrailingBackslash(IGtoPublish))+'\org.hl7.fhir.publisher.jar' then
//      FjarFile:= ExtractFileDir(ExcludeTrailingBackslash(IGtoPublish))+'\org.hl7.fhir.publisher.jar'
//    else if fileExists(IGtoPublish+'\input-cache\org.hl7.fhir.publisher.jar')
//      then FjarFile:= IGtoPublish+'\input-cache\org.hl7.fhir.publisher.jar';
  thread := TPublishThread.create(self, 'java -jar '+FJarFile+' -ig '+folder);
  try
    thread.Open;
    while thread.Running do
      Application.ProcessMessages;
  finally
    thread.Free;
  end;
end;

function TFHIRIGPublisher.needsUpgrade(folder : String; var msg: String): boolean;
begin
  raise Exception.Create('Not done yet');
end;

procedure TFHIRIGPublisher.upgrade(folder : String; progressProc: TProgressProc);
begin
  raise Exception.Create('Not done yet');
end;

end.
