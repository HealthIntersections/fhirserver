unit ClientApplicationVerifier;

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
  SysUtils, Classes,
  IdHTTP, IdSSLOpenSSL,
  TextUtilities,
  AdvObjects,
  AdvJson, JWT,
  FHIRTypes, FHIRResources, FHIRUtilities;

type
  TClientApplicationVerifier = class (TAdvObject)
  private
    FServer: String;
    FCertificate: String;
    FPassword: String;
    procedure getSSLpassword(var Password: String);
    function performVerification(jwt : String) : TJsonObject;
  public
    function Link : TClientApplicationVerifier; overload;
    property Server : String read FServer write FServer;
    property Certificate : String read FCertificate write FCertificate;
    property Password : String read FPassword write FPassword;

    function check(jwt : TJWT; html : TStringBuilder; var summary : String) : boolean; overload;
    function check(jwt : TJWT; params : TFHIRParameters) : boolean; overload;
  end;

implementation

{ TClientApplicationVerifier }

function TClientApplicationVerifier.check(jwt : TJWT; html : TStringBuilder; var summary : String) : boolean;
var
  json, obj : TJsonObject;
  item : TJsonNode;
begin
  json := performVerification(jwt.originalSource);
  try
    result := json.str['status'] = 'approved';
    summary := json.str['message'];
    for item in json.forceArr['endorsements'] do
      if item is TJsonObject then
      begin
        obj := item as TJsonObject;
        html.Append('<li>');
        if obj.str['type'] = 'warning' then
          html.Append('<img src="warning.png"/>')
        else if obj.str['type'] = 'usage-note' then
          html.Append('<img src="usagenote.png"/>')
        else if obj.str['type'] = 'approval' then
          html.Append('<img src="approval.png"/>');
        html.Append(FormatTextToHTML(obj.str['comment']));
        if (obj.obj['endorser'].has('url')) then
          html.Append('<a href="'+obj.obj['endorser'].str['url']+'">');
        html.Append(FormatTextToHTML(obj.obj['endorser'].str['name']));
        if (obj.obj['endorser'].has('url')) then
          html.Append('</a>');
      end;
  finally
    json.Free;
  end;
end;

function TClientApplicationVerifier.check(jwt : TJWT; params : TFHIRParameters) : boolean;
var
  p, p1 : TFhirParametersParameter;
  json, obj : TJsonObject;
  item : TJsonNode;
begin
  json := performVerification(jwt.originalSource);
  try
    result := json.str['status'] = 'approved';
    p := params.parameterList.Append;
    p.name := 'message';
    p.value := TFHIRString.Create(json.str['message']);
    for item in json.forceArr['endorsements'] do
      if item is TJsonObject then
      begin
        obj := item as TJsonObject;
        p := params.parameterList.Append;
        p.name := 'endorsement';
        if (obj.has('type')) then
        begin
          p1 := p.partList.Append;
          p1.name := 'type';
          p1.value := TFHIRCode.Create(obj.str['type']);
        end;
        if (obj.has('comment')) then
        begin
          p1 := p.partList.Append;
          p1.name := 'comment';
          p1.value := TFHIRString.Create(obj.str['comment']);
        end;
        if (obj.has('endorser')) then
        begin
          if (obj.obj['endorser'].has('name')) then
          begin
            p1 := p.partList.Append;
            p1.name := 'endorser';
            p1.value := TFHIRString.Create(obj.obj['endorser'].str['name']);
          end;
          if (obj.obj['endorser'].has('url')) then
          begin
            p1 := p.partList.Append;
            p1.name := 'url';
            p1.value := TFHIRString.Create(obj.obj['endorser'].str['url']);
          end;
        end;
      end;
  finally
    json.Free;
  end;
end;


function TClientApplicationVerifier.Link: TClientApplicationVerifier;
begin
  result := TClientApplicationVerifier(inherited Link);
end;

function TClientApplicationVerifier.performVerification(jwt: String): TJsonObject;
var
  indy : TIdHTTP;
  ssl : TIdSSLIOHandlerSocketOpenSSL;
  src, resp : TMemoryStream;
begin
  ssl := nil;
  indy := TIdHTTP.create(nil);
  try
    indy.HandleRedirects := true;
    if FServer.StartsWith('https:') then
    begin
      ssl := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
      indy.IOHandler := ssl;
      ssl.SSLOptions.Mode := sslmClient;
      ssl.SSLOptions.SSLVersions := [sslvTLSv1_2];
      if Certificate <> '' then
      begin
        ssl.SSLOptions.CertFile := Certificate;
        ssl.SSLOptions.KeyFile := ChangeFileExt(Certificate, '.key');
        ssl.OnGetPassword := getSSLpassword;
      end;
    end;
    indy.Request.ContentType := 'application/x-www-form-urlencoded';
    indy.Request.Accept := 'application/json';

    src := TMemoryStream.Create;
    resp := TMemoryStream.create;
    Try
      StringToStream('jwt='+jwt, src, TEncoding.ASCII);
      src.Position := 0;
      indy.Post(FServer, src, resp);
      if (indy.ResponseCode < 200) or (indy.ResponseCode >= 300) Then
        raise exception.create('unexpected condition');
      resp.Position := 0;
      result := TJSONParser.Parse(resp);
    Finally
      resp.free;
      src.Free;
    End;
  finally
    ssl.Free;
    indy.free;
  end;
end;

procedure TClientApplicationVerifier.getSSLpassword(var Password: String);
begin
  Password := FPassword;
end;


end.
