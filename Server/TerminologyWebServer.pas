unit TerminologyWebServer;

interface

uses
  SysUtils, Classes,
  EncodeSupport, StringSupport,
  AdvObjects, AdvStringMatches,
  IdContext, IdCustomHTTPServer,
  FHIRLang,
  HtmlPublisher, SnomedPublisher, SnomedServices, LoincPublisher, LoincServices, SnomedExpressions,
  TerminologyServer;

Type
  TTerminologyWebServer = class (TAdvObject)
  private
    FServer : TTerminologyServer;
    FFHIRPath : String;
    Procedure HandleLoincRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure HandleSnomedRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure HandleTxRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure BuildCsByName(html : THtmlPublisher; id : String);
    Procedure BuildCsByURL(html : THtmlPublisher; id : String);
    Procedure BuildVsByName(html : THtmlPublisher; id : String);
    Procedure BuildVsByURL(html : THtmlPublisher; id : String);
    function processSnomedForTool(code : String) : String;
  public
    constructor create(server : TTerminologyServer; FHIRPath : String); overload;

    function HandlesRequest(path : String) : boolean;
    Procedure Process(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);

  end;

implementation

uses
  FHIRParser,
  FHIRResources;

{ TTerminologyWebServer }

constructor TTerminologyWebServer.create(server: TTerminologyServer; FHIRPath : String);
begin
  create;
  FServer := server;
  FFHIRPath := FHIRPath;
end;

function TTerminologyWebServer.HandlesRequest(path: String): boolean;
begin
  result := path.StartsWith('/tx') or path.StartsWith('/snomed') or path.StartsWith('/loinc') ;
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

function GetId(url, prefix : string) : String;
begin
  if length(url) <= length(prefix) then
    result := ''
  else
    result := url.Substring(length(prefix)+1);
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
      html.BaseURL := '';
      html.Lang := request.AcceptLanguage;
      if request.Document.StartsWith('/tx/vs-name') then
        BuildVsByName(html, getId(request.Document, '/tx/vs-name'))
      else if request.Document.StartsWith('/tx/vs-uri') then
        BuildVsByURL(html, getId(request.Document, '/tx/vs-uri'))
      else if request.Document.StartsWith('/tx/cs-name') then
        BuildCsByName(html, getId(request.Document, '/tx/vs-name'))
      else if request.Document.StartsWith('/tx/cs-uri') then
        BuildCsByURL(html, getId(request.Document, '/tx/vs-uri'))
      else
      begin
        writeln(''+GetFhirMessage('TERMINOLOGY', request.AcceptLanguage)+' '+GetFhirMessage('SERVER_HOME', request.AcceptLanguage));
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

Procedure TTerminologyWebServer.BuildCsByName(html : THtmlPublisher; id : String);
var
  list : TAdvStringMatch;
  ts : TStringList;
  i: Integer;
begin
  writeln('Tx: CS By Name '+Id);
  ts := TStringList.Create;
  list := FServer.GetCodeSystemList;
  try
    html.Header('Terminology Server');
    html.Heading(2, 'CodeSystems');
    html.StartList;

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

Procedure TTerminologyWebServer.BuildCsByURL(html : THtmlPublisher; id : String);
var
  list : TAdvStringMatch;
  ts : TStringList;
  i: Integer;
begin
  writeln('Tx: CS By URL '+Id);
  ts := TStringList.Create;
  list := FServer.GetCodeSystemList;
  try
    html.Header('Terminology Server');
    html.Heading(2, 'Code Systems (by URL)');
    html.StartList;
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

Procedure TTerminologyWebServer.BuildVsByName(html : THtmlPublisher; id : String);
var
  list : TAdvStringMatch;
  ts : TStringList;
  i: Integer;
begin
  writeln('Tx: VS By Name '+Id);
  ts := TStringList.Create;
  list := FServer.GetValueSetList;
  try
    html.Header('Terminology Server');
    html.Heading(2, 'Value Sets (By Name)');
    html.StartList;
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

Procedure TTerminologyWebServer.BuildVsByURL(html : THtmlPublisher; id : String);
var
  list : TAdvStringMatch;
  ts : TStringList;
  i: Integer;
  vs : TFHIRValueSet;
  xml : TFHIRXmlComposer;
  s : TStringStream;
begin
  writeln('Tx: VS By URL '+Id);
  html.Header('Terminology Server');
  html.Heading(2, 'Value Sets (By URL)');
  if (id <> '') then
  begin
    vs := FServer.getValueSetByUrl(id);
    if (vs.text <> nil) and (vs.text.div_ <> nil) then
    begin
      html.writeXhtml(vs.text.div_);
      html.Line;
    end;
    s := TStringStream.Create;
    xml := TFHIRXmlComposer.Create('en');
    try
      xml.Compose(s, '', '', vs, true);
      html.startPre;
      html.AddTextPlain(s.DataString);
      html.endPre;
    finally
      xml.Free;
      s.Free;
    end;
  end
  else
  begin
    ts := TStringList.Create;
    list := FServer.GetValueSetList;
    try
      html.StartList;
      for i := 0 to list.Count - 1 do
        ts.AddObject(list.KeyByIndex[i], TObject(i));
      ts.sort;
      for i := 0 to ts.Count - 1 do
      begin
        html.StartListItem;
        html.URL(ts[i], 'vs-uri/'+EncodeMime(ts[i]));
        html.AddTextPlain(': '+list.ValueByIndex[Integer(ts.Objects[i])]);
        html.EndListItem;
      end;
    finally
      list.Free;
      ts.Free;
    end;
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
      pub := TSnomedPublisher.create(FServer.Snomed, FFHIRPath);
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
      pub := TLoincPublisher.create(FServer.Loinc, FFHIRPath);
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
