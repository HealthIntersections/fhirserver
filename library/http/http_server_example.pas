unit http_server_example;

{$i fhir.inc}

interface

uses
  SysUtils, Classes, DateUtils,
  http_support, http_server;

type
  TMyWebServer = class
  private
    FServer: TLightHttpServer;
    procedure HandleRequest(Request: THttpRequest; Response: THttpResponse);
    procedure HandleApiStatus(Request: THttpRequest; Response: THttpResponse);
    procedure HandleApiEcho(Request: THttpRequest; Response: THttpResponse);
    procedure HandleStaticFile(Request: THttpRequest; Response: THttpResponse);
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Start(Port: Word);
    procedure Stop;
  end;

implementation

{ TMyWebServer }

constructor TMyWebServer.Create;
begin
  inherited Create;
  FServer := CreateHttpServer;
  FServer.SetRequestHandler(HandleRequest);
end;

destructor TMyWebServer.Destroy;
begin
  Stop;
  FServer.Free;
  inherited Destroy;
end;

procedure TMyWebServer.Start(Port: Word);
begin
  FServer.Port := Port;
  FServer.Start;
  WriteLn('Server started on port ', Port);
  WriteLn('Try these URLs:');
  WriteLn('  http://localhost:', Port, '/api/status');
  WriteLn('  http://localhost:', Port, '/api/echo?message=hello');
  WriteLn('  http://localhost:', Port, '/static/test.html');
  WriteLn('Press ENTER to stop server...');
end;

procedure TMyWebServer.Stop;
begin
  if FServer.Active then
  begin
    FServer.Stop;
    WriteLn('Server stopped');
  end;
end;

procedure TMyWebServer.HandleRequest(Request: THttpRequest; Response: THttpResponse);
begin
  // Simple routing based on path
  if Request.Path = '/api/status' then
    HandleApiStatus(Request, Response)
  else if Request.Path = '/api/echo' then
    HandleApiEcho(Request, Response)
  else if Request.Path.StartsWith('/static/') then
    HandleStaticFile(Request, Response)
  else
  begin
    Response.SetStatus(404, 'Not Found');
    Response.SetHTMLContent(
      '<html><body>' +
      '<h1>404 - Not Found</h1>' +
      '<p>The requested path "' + Request.Path + '" was not found.</p>' +
      '<p>Available endpoints:</p>' +
      '<ul>' +
      '<li><a href="/api/status">/api/status</a> - Server status</li>' +
      '<li><a href="/api/echo?message=test">/api/echo?message=test</a> - Echo service</li>' +
      '<li><a href="/static/test.html">/static/test.html</a> - Static files</li>' +
      '</ul>' +
      '</body></html>'
    );
  end;
end;

procedure TMyWebServer.HandleApiStatus(Request: THttpRequest; Response: THttpResponse);
var
  Stats: TServerStats;
  JsonResponse: string;
begin
  // Handle CORS for OPTIONS requests
  if Request.Method = hmOPTIONS then
  begin
    Response.SetStatus(200, 'OK');
    Response.SetHeader('Access-Control-Allow-Origin', '*');
    Response.SetHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
    Response.SetHeader('Access-Control-Allow-Headers', 'Content-Type');
    Response.SetTextContent('');
    Exit;
  end;
  
  if Request.Method <> hmGET then
  begin
    Response.SetStatus(405, 'Method Not Allowed');
    Response.SetHeader('Allow', 'GET, OPTIONS');
    Response.SetJSONContent('{"error": "Method not allowed"}');
    Exit;
  end;
  
  Stats := FServer.Stats;
  
  JsonResponse := Format(
    '{' +
    '"status": "running",' +
    '"uptime_seconds": %d,' +
    '"active_requests": %d,' +
    '"total_requests": %d,' +
    '"average_response_time_ms": %.2f,' +
    '"server_time": "%s"' +
    '}',
    [
      SecondsBetween(Now, Stats.ServerStartTime),
      Stats.ActiveRequests,
      Stats.TotalRequests,
      Stats.AverageResponseTimeMs,
      FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)
    ]
  );
  
  Response.SetHeader('Access-Control-Allow-Origin', '*');
  Response.SetJSONContent(JsonResponse);
end;

procedure TMyWebServer.HandleApiEcho(Request: THttpRequest; Response: THttpResponse);
var
  Message: string;
  JsonResponse: string;
  PostData: string;
begin
  // Handle CORS for OPTIONS requests
  if Request.Method = hmOPTIONS then
  begin
    Response.SetStatus(200, 'OK');
    Response.SetHeader('Access-Control-Allow-Origin', '*');
    Response.SetHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
    Response.SetHeader('Access-Control-Allow-Headers', 'Content-Type');
    Response.SetTextContent('');
    Exit;
  end;
  
  case Request.Method of
    hmGET:
      begin
        Message := Request.GetParameter('message', 'No message provided');
        JsonResponse := Format(
          '{' +
          '"method": "GET",' +
          '"message": "%s",' +
          '"query_params": %d,' +
          '"timestamp": "%s"' +
          '}',
          [
            Message,
            Length(Request.Parameters.GetParameterNames),
            FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)
          ]
        );
      end;
      
    hmPOST, hmPUT:
      begin
        PostData := Request.GetBodyAsString;
        if PostData = '' then
          PostData := 'No body data';
          
        JsonResponse := Format(
          '{' +
          '"method": "%s",' +
          '"body": "%s",' +
          '"content_type": "%s",' +
          '"content_length": %d,' +
          '"timestamp": "%s"' +
          '}',
          [
            MethodToString(Request.Method),
            PostData,
            Request.GetContentType,
            Request.GetContentLength,
            FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)
          ]
        );
      end;
      
    hmDELETE:
      begin
        JsonResponse := Format(
          '{' +
          '"method": "DELETE",' +
          '"path": "%s",' +
          '"message": "Resource would be deleted",' +
          '"timestamp": "%s"' +
          '}',
          [
            Request.Path,
            FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)
          ]
        );
      end;
      
  else
    Response.SetStatus(405, 'Method Not Allowed');
    Response.SetHeader('Allow', 'GET, POST, PUT, DELETE, OPTIONS');
    Response.SetJSONContent('{"error": "Method not allowed"}');
    Exit;
  end;
  
  Response.SetHeader('Access-Control-Allow-Origin', '*');
  Response.SetJSONContent(JsonResponse);
end;

procedure TMyWebServer.HandleStaticFile(Request: THttpRequest; Response: THttpResponse);
var
  FileName: string;
  FilePath: string;
  HtmlContent: string;
begin
  if Request.Method <> hmGET then
  begin
    Response.SetStatus(405, 'Method Not Allowed');
    Response.SetHeader('Allow', 'GET');
    Response.SetTextContent('Method not allowed');
    Exit;
  end;
  
  // Extract filename (remove /static/ prefix)
  FileName := Copy(Request.Path, 9, Length(Request.Path)); // Skip "/static/"
  
  // For demo purposes, serve a few hardcoded "files"
  if FileName = 'test.html' then
  begin
    HtmlContent := 
      '<!DOCTYPE html>' +
      '<html lang="en">' +
      '<head>' +
      '    <meta charset="UTF-8">' +
      '    <meta name="viewport" content="width=device-width, initial-scale=1.0">' +
      '    <title>Test Page</title>' +
      '    <style>' +
      '        body { font-family: Arial, sans-serif; margin: 40px; }' +
      '        .container { max-width: 600px; }' +
      '        button { padding: 10px 20px; margin: 5px; cursor: pointer; }' +
      '        #result { margin-top: 20px; padding: 10px; background: #f5f5f5; }' +
      '    </style>' +
      '</head>' +
      '<body>' +
      '    <div class="container">' +
      '        <h1>Light HTTP Server Test Page</h1>' +
      '        <p>This page is served by the Pascal HTTP server!</p>' +
      '        ' +
      '        <h3>API Tests:</h3>' +
      '        <button onclick="testStatus()">Test Status API</button>' +
      '        <button onclick="testEcho()">Test Echo API</button>' +
      '        <button onclick="testPost()">Test POST</button>' +
      '        ' +
      '        <div id="result"></div>' +
      '    </div>' +
      '    ' +
      '    <script>' +
      '        async function testStatus() {' +
      '            try {' +
      '                const response = await fetch("/api/status");' +
      '                const data = await response.json();' +
      '                document.getElementById("result").innerHTML = ' +
      '                    "<h4>Status API Result:</h4><pre>" + JSON.stringify(data, null, 2) + "</pre>";' +
      '            } catch (error) {' +
      '                document.getElementById("result").innerHTML = "<h4>Error:</h4>" + error;' +
      '            }' +
      '        }' +
      '        ' +
      '        async function testEcho() {' +
      '            try {' +
      '                const response = await fetch("/api/echo?message=Hello from JavaScript!");' +
      '                const data = await response.json();' +
      '                document.getElementById("result").innerHTML = ' +
      '                    "<h4>Echo API Result:</h4><pre>" + JSON.stringify(data, null, 2) + "</pre>";' +
      '            } catch (error) {' +
      '                document.getElementById("result").innerHTML = "<h4>Error:</h4>" + error;' +
      '            }' +
      '        }' +
      '        ' +
      '        async function testPost() {' +
      '            try {' +
      '                const response = await fetch("/api/echo", {' +
      '                    method: "POST",' +
      '                    headers: { "Content-Type": "application/json" },' +
      '                    body: JSON.stringify({ test: "data", number: 42 })' +
      '                });' +
      '                const data = await response.json();' +
      '                document.getElementById("result").innerHTML = ' +
      '                    "<h4>POST API Result:</h4><pre>" + JSON.stringify(data, null, 2) + "</pre>";' +
      '            } catch (error) {' +
      '                document.getElementById("result").innerHTML = "<h4>Error:</h4>" + error;' +
      '            }' +
      '        }' +
      '    </script>' +
      '</body>' +
      '</html>';
    
    Response.SetHTMLContent(HtmlContent);
    Response.SetCacheControl('no-cache');
  end
  else if FileName = 'style.css' then
  begin
    Response.SetTextContent(
      'body { font-family: Arial, sans-serif; margin: 40px; background-color: #f8f9fa; }' +
      '.container { max-width: 800px; margin: 0 auto; background: white; padding: 20px; border-radius: 5px; }' +
      'h1 { color: #333; border-bottom: 2px solid #007bff; padding-bottom: 10px; }',
      'text/css'
    );
  end
  else
  begin
    Response.SetStatus(404, 'Not Found');
    Response.SetHTMLContent(
      '<html><body>' +
      '<h1>File Not Found</h1>' +
      '<p>The file "' + FileName + '" was not found.</p>' +
      '<p>Available static files:</p>' +
      '<ul>' +
      '<li><a href="/static/test.html">test.html</a> - Demo page with API tests</li>' +
      '<li><a href="/static/style.css">style.css</a> - Sample CSS file</li>' +
      '</ul>' +
      '</body></html>'
    );
  end;
end;

end.
