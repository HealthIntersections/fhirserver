unit TwilioClient;

interface

Uses
  SysUtils, Classes,
  IdHTTP, IdSSLOpenSSL, IdAuthentication, IdMultipartFormData,
  AdvObjects;

Type
  TTwilioClient = class (TAdvObject)
  private
    FBody: String;
    FFrom: String;
    FTo: String;
    FToken: String;
    FAccount: String;

//    procedure handleAuthentication(Sender: TObject; Authentication: TIdAuthentication; var Handled: Boolean);
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

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
  ssl : TIdSSLIOHandlerSocketOpenSSL;
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
    ssl := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    try
      http.IOHandler := ssl;
      ssl.SSLOptions.Mode := sslmClient;
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
