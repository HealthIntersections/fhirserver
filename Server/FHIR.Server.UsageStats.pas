unit FHIR.Server.UsageStats;

interface

uses
  SysUtils, Classes,
  IdHTTPServer, IdCustomHTTPServer, IdContext,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.Json;

type
  TUsageStatsServer = class (TFslObject)
  private
    FEnabled: boolean;
    FPath: String;
    FDirectory : String;
    function zipDirectory : TStream;
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
    response.ContentStream := zipDirectory;
    response.FreeContentStream := true;
    response.ContentType := 'application/zip';
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
        BytesToFile(TJSONWriter.writeObject(json), FHIR.Support.Utilities.Path([FDirectory, id+'#'+ver+'.json']));
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

function TUsageStatsServer.zipDirectory: TStream;
begin
  result := nil;
end;

end.
