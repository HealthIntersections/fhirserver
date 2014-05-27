unit TerminologyWebServer;

interface

uses
  SysUtils, Classes,
  EncodeSupport, StringSupport,
  AdvObjects, AdvStringMatches,
  IdContext, IdCustomHTTPServer,
  HtmlPublisher, SnomedPublisher, SnomedServices, LoincPublisher, LoincServices, SnomedExpressions,
  TerminologyServer;

Type
  TTerminologyWebServer = class (TAdvObject)
  private
    FServer : TTerminologyServer;
    Procedure HandleLoincRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure HandleSnomedRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure HandleTxRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure BuildCsByName(html : THtmlPublisher);
    Procedure BuildCsByURL(html : THtmlPublisher);
    Procedure BuildVsByName(html : THtmlPublisher);
    Procedure BuildVsByURL(html : THtmlPublisher);
    function processSnomedForTool(code : String) : String;
  public
    constructor create(server : TTerminologyServer); overload;

    function HandlesRequest(path : String) : boolean;
    Procedure Process(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);

  end;

implementation

{ TTerminologyWebServer }

constructor TTerminologyWebServer.create(server: TTerminologyServer);
begin
  create;
  FServer := server;
end;

function TTerminologyWebServer.HandlesRequest(path: String): boolean;
begin
  result := path.StartsWith('/tx') or path.StartsWith('/snomed') or path.StartsWith('/snomed') ;
end;

procedure TTerminologyWebServer.Process(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  path : string;
begin
  path := request.Document;
  if path.StartsWith('/tx') then
    HandleTxRequest(AContext, request, response)
  else if path.StartsWith('/snomed') and (FServer.Snomed <> nil) then
    HandleSnomedRequest(AContext, request, response)
  else if request.Document.StartsWith('/loinc') and (FServer.Loinc <> nil) then
    HandleLoincRequest(AContext, request, response)

end;

procedure TTerminologyWebServer.HandleTxRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  html : THtmlPublisher;
  list : TAdvStringMatch;
  ts : TStringList;
  i: Integer;
begin
  try
    html := THtmlPublisher.Create;
    try
      html.BaseURL := '/tx';
      html.Lang := request.AcceptLanguage;
      if request.Document.StartsWith('/tx/vs-name') then
        BuildVsByName(html)
      else if request.Document.StartsWith('/tx/vs-uri') then
        BuildVsByURL(html)
      else if request.Document.StartsWith('/tx/cs-name') then
        BuildCsByName(html)
      else if request.Document.StartsWith('/tx/cs-uri') then
        BuildCsByURL(html)
      else
      begin
        writeln('Tx Server home page');
        html.Header('Terminology Server');
        html.StartList;
        html.StartListItem;
        html.URL('All Value Sets (by Name)', 'tx/vs-name');
        html.EndListItem;
        html.StartListItem;
        html.URL('All Value Sets (by URL)', 'tx/vs-uri');
        html.EndListItem;
        html.StartListItem;
        html.URL('All Code systems (by Name)', 'tx/cs-name');
        html.EndListItem;
        html.StartListItem;
        html.URL('All Code systems (by URL)', 'tx/cs-uri');
        html.EndListItem;
        html.EndList;
      end;

      html.Done;
      response.ContentText := html.output;
      response.ContentType := 'text/html';
      response.ResponseNo := 200;
    finally
      html.Free;
    end;
  except
    on e:exception do
    begin
      response.ResponseNo := 500;
      response.ContentType := 'text/plain';
      response.ContentText := 'error:'+e.Message;
    end;
  end;
end;

Procedure TTerminologyWebServer.BuildCsByName(html : THtmlPublisher);
var
  list : TAdvStringMatch;
  ts : TStringList;
  i: Integer;
begin
  writeln('Tx: CS By Name');
  html.Header('Terminology Server');
  html.Heading(2, 'CodeSystems');
  html.StartList;
  ts := TStringList.Create;
  list := FServer.GetCodeSystemList;
  try
    for i := 0 to list.Count - 1 do
      ts.AddObject(list.ValueByIndex[i], TObject(i));
    ts.sort;
    for i := 0 to ts.Count - 1 do
    begin
      html.StartListItem;
      html.URL(ts[i], 'tx/cs/'+list.KeyByIndex[Integer(ts.Objects[i])]);
      html.EndListItem;
    end;
  finally
    list.Free;
    ts.Free;
  end;
  html.EndList;
end;

Procedure TTerminologyWebServer.BuildCsByURL(html : THtmlPublisher);
var
  list : TAdvStringMatch;
  ts : TStringList;
  i: Integer;
begin
  writeln('Tx: CS By URL');
  html.Header('Terminology Server');
  html.Heading(2, 'Code Systems (by URL)');
  html.StartList;
  ts := TStringList.Create;
  list := FServer.GetCodeSystemList;
  try
    for i := 0 to list.Count - 1 do
      ts.AddObject(list.KeyByIndex[i], TObject(i));
    ts.sort;
    for i := 0 to ts.Count - 1 do
    begin
      html.StartListItem;
      html.URL(ts[i], 'tx/cs/'+ts[i]);
      html.AddTextPlain(': '+list.ValueByIndex[Integer(ts.Objects[i])]);
      html.EndListItem;
    end;
  finally
    list.Free;
    ts.Free;
  end;
  html.EndList;
end;

Procedure TTerminologyWebServer.BuildVsByName(html : THtmlPublisher);
var
  list : TAdvStringMatch;
  ts : TStringList;
  i: Integer;
begin
  writeln('Tx: VS By Name');
  html.Header('Terminology Server');
  html.Heading(2, 'Value Sets (By Name)');
  html.StartList;
  ts := TStringList.Create;
  list := FServer.GetValueSetList;
  try
    for i := 0 to list.Count - 1 do
      ts.AddObject(list.ValueByIndex[i], TObject(i));
    ts.sort;
    for i := 0 to ts.Count - 1 do
    begin
      html.StartListItem;
      html.URL(ts[i], 'tx/vs/'+list.KeyByIndex[Integer(ts.Objects[i])]);
      html.EndListItem;
    end;
  finally
    list.Free;
    ts.Free;
  end;
  html.EndList;
end;

Procedure TTerminologyWebServer.BuildVsByURL(html : THtmlPublisher);
var
  list : TAdvStringMatch;
  ts : TStringList;
  i: Integer;
begin
  writeln('Tx: VS By URL');
  html.Header('Terminology Server');
  html.Heading(2, 'Value Sets (By URL)');
  html.StartList;
  ts := TStringList.Create;
  list := FServer.GetValueSetList;
  try
    for i := 0 to list.Count - 1 do
      ts.AddObject(list.KeyByIndex[i], TObject(i));
    ts.sort;
    for i := 0 to ts.Count - 1 do
    begin
      html.StartListItem;
      html.URL(ts[i], 'tx/vs/'+ts[i]);
      html.AddTextPlain(': '+list.ValueByIndex[Integer(ts.Objects[i])]);
      html.EndListItem;
    end;
  finally
    list.Free;
    ts.Free;
  end;
  html.EndList;
end;

procedure TTerminologyWebServer.HandleSnomedRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  code : String;
  pub : TSnomedPublisher;
  html : THtmlPublisher;
begin
  if request.Document.StartsWith('/snomed/tool/') then // FHIR build process support
  begin
    response.ContentType := 'text/xml';
    try
      response.ContentText := processSnomedForTool(request.Document.Substring(13));
      response.ResponseNo := 200;
    except
      on e : Exception do
      begin
        response.ResponseNo := 500;
        response.ContentText := '<snomed version="'+FServer.snomed.Version+'" type="error" message="'+EncodeXML(e.Message)+'"/>';
      end;
    end;
  end
  else if request.Document.StartsWith('/snomed/doco/')  then
  begin
    code := request.UnparsedParams;
    writeln('Snomed Doco: '+code);

    try
      html := THtmlPublisher.Create;
      pub := TSnomedPublisher.create(FServer.Snomed);
      try
        html.BaseURL := '/snomed/doco/';
        html.Lang := request.AcceptLanguage;
        pub.PublishDict(code, '/snomed/doco/', html);
        response.ContentText := html.output;
        response.ResponseNo := 200;
      finally
        html.free;
        pub.free;
      end;
    except
      on e:exception do
      begin
        response.ResponseNo := 500;
        response.ContentText := 'error:'+EncodeXML(e.Message);
      end;
    end;
  end
  else
  begin
    response.ResponseNo := 404;
    response.ContentText := 'Document '+request.Document+' not found';
    writeln('miss: '+request.Document);
  end;
end;

procedure TTerminologyWebServer.HandleLoincRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  code : String;
  pub : TLoincPublisher;
  html : THtmlPublisher;
  loinc : TLoincServices;
begin
  if request.Document.StartsWith('/loinc/doco/')  then
  begin
    code := request.UnparsedParams;
    writeln('Loinc Doco: '+code);

    try
      html := THtmlPublisher.Create;
      pub := TLoincPublisher.create(FServer.Loinc);
      try
        html.BaseURL := '/loinc/doco/';
        html.Lang := request.AcceptLanguage;
        pub.PublishDict(code, '/loinc/doco/', html);
        response.ContentText := html.output;
        response.ResponseNo := 200;
      finally
        html.free;
        pub.free;
      end;
    except
      on e:exception do
      begin
        response.ResponseNo := 500;
        response.ContentText := 'error:'+EncodeXML(e.Message);
      end;
    end;
  end
  else
  begin
    response.ResponseNo := 404;
    response.ContentText := 'Document '+request.Document+' not found';
    writeln('miss: '+request.Document);
  end;
end;

function TTerminologyWebServer.processSnomedForTool(code : String) : String;
var
  sl : TStringList;
  s : String;
  id : int64;
  exp : TSnomedExpression;
begin
  writeln('Snomed: '+code);
  if StringIsInteger64(code) then
  begin
    if FServer.Snomed.IsValidConcept(code) then
    begin
      result := '<snomed version="'+FServer.Snomed.Version+'" type="concept" concept="'+code+'" display="'+EncodeXml(FServer.Snomed.GetDisplayName(code, ''))+'">';
      sl := TStringList.Create;
      try
        FServer.Snomed.ListDisplayNames(sl, code, '', ALL_DISPLAY_NAMES);
        for s in sl do
          result := result + '<display value="'+EncodeXML(s)+'"/>';
      finally
        sl.free;
      end;
      result := result + '</snomed>';
    end
    else if FServer.Snomed.IsValidDescription(code, id, s) then
    begin
      result := '<snomed version="'+FServer.Snomed.Version+'" type="description" description="'+code+'" concept="'+inttostr(id)+'" display="'+EncodeXml(s)+'">';
      sl := TStringList.Create;
      try
        FServer.Snomed.ListDisplayNames(sl, inttostr(id), '', ALL_DISPLAY_NAMES);
        for s in sl do
          result := result + '<display value="'+EncodeXML(s)+'"/>';
      finally
        sl.free;
      end;
      result := result + '</snomed>';
    end
    else
      raise Exception.Create('Snomed ID '+code+' not known');
  end
  else
  begin
    exp := TSnomedExpressionParser.Parse(FServer.Snomed, code);
    try
      result := '<snomed version="'+FServer.Snomed.Version+'" type="expression" expression="'+code+'" expressionMinimal="'+EncodeXml(TSnomedExpressionParser.Render(FServer.Snomed, exp, sroMinimal))+'" expressionMax="'+
      EncodeXml(TSnomedExpressionParser.Render(FServer.Snomed, exp, sroReplaceAll))+'" display="'+EncodeXml(TSnomedExpressionParser.Display(FServer.Snomed, exp))+'" ok="true"/>';
    finally
      exp.Free;
    end;
  end;
end;


end.
