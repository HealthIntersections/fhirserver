unit fsl_zulip;

{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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


{$I fhir.inc}

// see https://zulip.com/api/send-message

interface

uses
  SysUtils,
  fsl_base, fsl_utilities, fsl_json, fsl_fetcher;

type
  TZulipSender = class (TFslObject)
  private
    FAddress : string;
    FBotEmail : string;
    FBotAccessKey : String;
  public
    constructor create(address, botEmail, botAccessKey : String);

    procedure sendMessage(stream, topic, message : String);
  end;

implementation


{ TZulipSender }

constructor TZulipSender.create(address, botEmail, botAccessKey: String);
begin
  inherited Create;
  FAddress := address;
  FBotEmail := botEmail;
  FBotAccessKey := botAccessKey;
end;

procedure TZulipSender.sendMessage(stream, topic, message: String);
var
  client : TInternetFetcher;
  json : TJsonObject;
begin
  client := TInternetFetcher.Create;
  try
    client.URL := FAddress;
    client.Username := FBotEmail;
    client.Password := FBotAccessKey;
    client.Method := imfPost;
    client.NoErrors := true;
    client.ContentType := 'application/x-www-form-urlencoded';
    client.Buffer.AsText :=
        'type=stream&'+
        'to='+EncodePercent(stream)+'&'+
        'topic='+EncodePercent(topic)+'&'+
        'content='+EncodePercent(message);
    client.Fetch;
    if client.ResponseCode <> 200 then
    begin
      if (client.Buffer.AsText <> '') then
      begin
        json := TJSONParser.Parse(client.Buffer.AsText);
        try
          raise Exception.Create('Error Sending Message '+json.str['msg']);
        finally
          json.free;
        end;
      end
      else
        raise Exception.Create('Zulip send failed ('+inttostr(client.ResponseCode)+')');
    end;

  finally
    client.Free;
  end;
end;

end.
