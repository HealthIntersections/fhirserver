unit usage_stats;

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
  SysUtils, Classes, {$IFDEF DELPHI} IOUtils, {$ENDIF}
  IdHTTPServer, IdCustomHTTPServer, IdContext,
  fsl_base, fsl_utilities, fsl_stream, fsl_json, fsl_fpc;

type
  TUsageStatsServer = class (TFslObject)
  private
    FEnabled: boolean;
    FPath: String;
    FDirectory : String;
    function singleJson : String;
  public
    constructor create(directory : String);

    property enabled: boolean read FEnabled write FEnabled;
    property path: String read FPath write FPath;
    property directory: String read FDirectory write FDirectory;

    Procedure HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
  end;

implementation

{ TUsageStatsServer }

constructor TUsageStatsServer.create(directory: String);
begin
  inherited Create;
  FDirectory := directory;
  enabled := FolderExists(FDirectory);
  FPath := '/usage-stats';
end;

procedure TUsageStatsServer.HandleRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  json : TJsonObject;
  id, ver : String;
begin
  if request.CommandType = hcGET then
  begin
    response.ContentText := singleJson;
    response.ContentType := 'application/json';
    response.ResponseNo := 200;
  end
  else if request.RawHeaders.Values['Authorization'] <> 'Bearer 9D1C23C0-3915-4542-882E-2BCC55645DF8' then
  begin
    response.ResponseNo := 404;
    response.ContentText := 'Unknown';
  end
  else
  begin
    try
      json := TJSONParser.Parse(request.PostStream);
      try
        id := json.str['package'];
        if id = '' then
          raise Exception.Create('no package id');
        if not id.Contains('.') then
          raise Exception.Create('invalid package id');
        if not id.length > 32 then
          raise Exception.Create('invalid package id');
        ver := json.str['version'];
        if ver = '' then
          raise Exception.Create('no package ver');
        if not ver.length > 32 then
          raise Exception.Create('invalid package ver');
        json.str['date'] := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', TFslDateTime.makeUTC.DateTime);
        BytesToFile(TJSONWriter.writeObject(json), fsl_utilities.Path([FDirectory, id+'#'+ver+'.json']));
      finally
        json.free;
      end;
      response.ResponseNo := 202;
    except
      on e : Exception do
      begin
      response.ResponseNo := 422;
      response.ContentText := 'Unacceptable: '+e.Message;
      end;
    end;
  end;

  // if it's a get: zip up the contents of the directory
  // else
  // check the header (else return 404)
  // load the json
  // check that it's got a package id and version
  //
end;

function TUsageStatsServer.singleJson: String;
var
  json : TJsonObject;
  s : String;
begin
  json := TJsonObject.Create;
  try
    for s in TDirectory.GetFiles(FDirectory) do
      json.obj[ExtractFileName(s)] := TJSONParser.ParseFile(s);
    result := TJSONWriter.writeObjectStr(json);
  finally
    json.Free;
  end;
end;

end.
