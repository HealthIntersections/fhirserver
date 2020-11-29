unit fsl_twilio;

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

Uses
  SysUtils, Classes,
  IdHTTP, IdAuthentication, IdMultipartFormData,
  IdOpenSSLIOHandlerClient, IdOpenSSLVersion,
  fsl_base;

Type
  TTwilioClient = class (TFslObject)
  private
    FBody: String;
    FFrom: String;
    FTo: String;
    FToken: String;
    FAccount: String;

//    procedure handleAuthentication(Sender: TObject; Authentication: TIdAuthentication; var Handled: Boolean);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    Procedure send;

    class procedure RunTests;

    Property Account: String read FAccount write FAccount;
    Property Token : String read FToken write FToken;
    Property From : String read FFrom write FFrom;
    Property Dest : String read FTo write FTo;
    Property Body : String read FBody write FBody;
  end;

implementation

{ TTwilioClient }

constructor TTwilioClient.Create;
begin
  inherited;

end;

destructor TTwilioClient.Destroy;
begin

  inherited;
end;

procedure TTwilioClient.send;
var
  http : TIdHTTP;
  ssl : TIdOpenSSLIOHandlerClient;
  response : TMemoryStream;
  url : String;
  params : TIdMultiPartFormDataStream;
begin
  url := 'https://api.twilio.com/2010-04-01/Accounts/'+Account+'/Messages';
  params := TIdMultiPartFormDataStream.create;
  try
    params.AddFormField('To', Dest);
    params.AddFormField('From', From);
    params.AddFormField('Body', Body);
    http := TIdHTTP.create(nil);
    response := TMemoryStream.Create;
    ssl := TIdOpenSSLIOHandlerClient.Create(nil);
    try
      http.IOHandler := ssl;
      http.Request.BasicAuthentication := true;
      http.Request.Username := Account;
      http.Request.Password := Token;
      http.Post(url, params, response);
    finally
      response.Free;
      http.free;
      ssl.Free;
    end;
  finally
    params.free;
  end;
end;


function TTwilioClient.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FBody.length * sizeof(char)) + 12);
  inc(result, (FFrom.length * sizeof(char)) + 12);
  inc(result, (FTo.length * sizeof(char)) + 12);
  inc(result, (FToken.length * sizeof(char)) + 12);
  inc(result, (FAccount.length * sizeof(char)) + 12);
end;

class procedure TTwilioClient.RunTests;
var
  this : TTwilioClient;
begin
  this :=  TTwilioClient.Create;
  try
     this.Account := 'ACaab8ee23cacbf7f30da842053d91f3aa';
     this.Token := 'fill this out';
     this.From := '+12678434041';
     this.Dest := '+61411867065';
     this.Body := 'Someone is running FHIR Server tests @ '+FormatDateTime('c', now);
     this.send;
  finally
    this.Free;
  end;
end;

end.
