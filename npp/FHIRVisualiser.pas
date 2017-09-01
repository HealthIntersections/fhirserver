unit FHIRVisualiser;


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

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, NppDockingForms, Vcl.StdCtrls, NppPlugin, Vcl.ToolWin, SystemSupport,
  FHIRTypes, FHIRResources, FHIRUtilities, CDSHooksUtilities,
  Vcl.ComCtrls, System.ImageList, Vcl.ImgList, Vcl.ExtCtrls, Vcl.Styles, Vcl.Themes,
  FHIRPathDocumentation, Vcl.Buttons, Vcl.OleCtrls, SHDocVw, TextUtilities, FHIRBase,
  AdvGenerics, PluginUtilities, VirtualTrees, kCritSct, AdvBuffers, ShellSupport,
  IdSocketHandle, IdContext, IdHTTPServer, IdCustomHTTPServer, SmartOnFhirUtilities,
  GUIDSupport, CDSBrowserForm, CDSHooksClientManager;

const
  UMSG = WM_USER + 1;

type
  TVisualiserMode = (
    vmNone,  // the visualiser is not showing
    vmNarrative, // show the narrative of the resource
    vmValidation, // show validation outcomes
    vmPath, // show the path output
    vmFocus // information about the current selected element
  );

  TWebBuffer = class (TAdvBuffer)
  private
    FContentType: String;
  public
    function link : TWebBuffer; overload;
    property ContentType : String read FContentType write FContentType;
  end;

  TFHIRVisualizer = class(TNppDockingForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    webNarrative: TWebBrowser;
    ListView1: TListView;
    Panel1: TPanel;
    Splitter1: TSplitter;
    lstMatches: TListView;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    lstValidation: TListView;
    cbErrorsOnly: TCheckBox;
    vtExpressions: TVirtualStringTree;
    TabSheet4: TTabSheet;
    webFocus: TWebBrowser;
    Panel5: TPanel;
    Button1: TButton;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormFloat(Sender: TObject);
    procedure FormDock(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lstValidationCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lstValidationSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure vtExpressionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtExpressionsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure vtExpressionsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure Button1Click(Sender: TObject);
    procedure webFocusBeforeNavigate2(ASender: TObject; const pDisp: IDispatch;
      const URL, Flags, TargetFrameName, PostData, Headers: OleVariant;
      var Cancel: WordBool);
  private
    { Private declarations }

    FLastHtml : String;
    FValList : TAdvList<TFHIRAnnotation>;
    FMatchList : TAdvList<TFHIRAnnotation>;
    FExpression : TFHIRPathExpressionNode;
    FFocusPath : String;
    FFocusObjects : TAdvList<TFHIRObject>;
    FCDSManager : TCDSHooksManager;

    FLock : TCriticalSection;
    FCards : TAdvList<TCDSHookCard>;
    FCDSErrors : TStringList;
    FWebServer : TIdHTTPServer;
    FWebCache : TAdvMap<TWebBuffer>;

    function generateBasicCard(path: String; focus: TFHIRObject): TCDSHookCard;
    procedure generateTypeCard(focus, next: TFHIRObject);
    function differentObjects(focus: array of TFHIRObject): boolean;
    procedure queryCDS(hook : string; context : TFhirResource);
    procedure queryCDSPatient(hook : string; patient : TFhirPatient);

    procedure DoCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    function postToWeb(contentType : String; bytes : TBytes) : String; overload;
    function postToWeb(page : String) : String; overload;

    procedure OnCDSResponse(manager : TCDSHooksManager; server : TRegisteredFHIRServer; context : TObject; response : TCDSHookResponse; error : String);
    procedure DoUpdateCards(var Msg: TMessage); message UMSG;
    procedure UpdateCards;
  public
    { Public declarations }
    procedure setNarrative(s : String);
    procedure setValidationOutcomes(errors : TAdvList<TFHIRAnnotation>);
    procedure setPathOutcomes(matches : TAdvList<TFHIRAnnotation>; expression : TFHIRPathExpressionNode);
    procedure setFocusInfo(path : String; focus : Array of TFHIRObject);

    property CDSManager : TCDSHooksManager read FCDSManager;
    procedure reregisterAllCDSServers;
  end;

var
  FHIRVisualizer: TFHIRVisualizer;
  VisualiserMode : TVisualiserMode = vmNone;

implementation

{$R *.dfm}

Uses
  FHIRPluginSettings,
  FHIRToolboxForm,
  FHIRPlugin;

{
Card Visualisers for data types:

Attachment - if it's an image, show the image
markdown - markdown display

Identifier - use cds-hook to look up information
code/Coding/CodeableConcept/Quantity - use cds-hook to look up information
contact point - clickable link if it's valid
Signature - use it to validate the content and report whether it's valid?
Timing - interpret?
}

procedure TFHIRVisualizer.FormKeyPress(Sender: TObject;
  var Key: Char);
begin
  inherited;
end;

// Docking code calls this when the form is hidden by either "x" or self.Hide
procedure TFHIRVisualizer.DoCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  doc : string;
  wb : TWebBuffer;
  bytes : TBytes;
  ct : String;
begin
  doc := ARequestInfo.Document.Substring(1);
  FLock.Lock;
  try
    if FWebCache.TryGetValue(doc, wb) then
    begin
      bytes := wb.AsBytes;
      ct := wb.ContentType;
      FWebCache.Remove(doc);
    end;
  finally
    FLock.Unlock;
  end;
  if length(bytes) > 0 then
  begin
    AResponseInfo.ResponseNo := 200;
    AResponseInfo.ResponseText := 'OK';
    AResponseInfo.ContentLength := length(bytes);
    AResponseInfo.FreeContentStream := true;
    AResponseInfo.ContentStream := TBytesStream.Create(bytes);
  end
  else
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ResponseText := 'Not Found';
    AResponseInfo.ContentText := 'Not Found';
  end
end;

procedure TFHIRVisualizer.DoUpdateCards(var Msg: TMessage);
begin
  UpdateCards;
end;

procedure TFHIRVisualizer.UpdateCards;
var
  ts : TStringList;
begin
  FLock.Lock;
  try
    ts := TStringList.Create;
    try
      FCDSManager.listInProgress(ts);
      webFocus.Navigate('http://localhost:45654/'+postToWeb(presentAsHtml(FCards, ts, FCDSErrors)));
    finally
      ts.Free;
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRVisualizer.FormCreate(Sender: TObject);
var
  SHandle: TIdSocketHandle;
begin
  inherited;
  VisualiserMode := TVisualiserMode(PageControl1.TabIndex+1);
  FCards := TAdvList<TCDSHookCard>.create;
  FCDSErrors := TStringList.Create;
  FFocusObjects := TAdvList<TFHIRObject>.create;

  FLock := TCriticalSection.Create('vis.web');
  FWebCache := TAdvMap<TWebBuffer>.create;
  FWebserver := TIdHTTPServer.Create(nil);
  SHandle := FWebserver.Bindings.Add;
  SHandle.IP := '127.0.0.1';
  SHandle.Port := 45654;
  FWebserver.OnCommandGet := DoCommandGet;
  FWebserver.Active := true;
  FCDSManager := TCDSHooksManager.create;
  reregisterAllCDSServers;
end;

procedure TFHIRVisualizer.reregisterAllCDSServers;
var
  i : integer;
  server : TRegisteredFHIRServer;
begin
  FCDSManager.clearServers;
  for i := 0 to Settings.ServerCount('') - 1 do
  begin
    server := Settings.serverInfo('', i);
    try
      if server.cdshooks.Count > 0 then
        FCDSManager.registerServer(server);
    finally
      server.Free;
    end;
  end;
end;

procedure TFHIRVisualizer.FormDestroy(Sender: TObject);
begin
  lstValidation.Items.clear;
  FValList.Free;
  lstMatches.Items.Clear;
  FMatchList.Free;
  FExpression.Free;
  FExpression := nil;
  FFocusObjects.Free;
  FFocusObjects := nil;
  FCDSManager.Free;
  FCDSErrors.Free;
  FCards.Free;
  FWebServer.Free;
end;

procedure TFHIRVisualizer.FormDock(Sender: TObject);
begin
  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 1);
end;

procedure TFHIRVisualizer.FormFloat(Sender: TObject);
begin
  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 1);
end;

procedure TFHIRVisualizer.FormHide(Sender: TObject);
begin
  inherited;
  Settings.VisualiserVisible := false;
  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 0);
  VisualiserMode := vmNone;
end;

procedure TFHIRVisualizer.FormShow(Sender: TObject);
begin
  inherited;
  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 1);
  Settings.VisualiserVisible := true;
  FLastHtml := '';
  TabControl1Change(nil);
end;

procedure TFHIRVisualizer.lstValidationCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  a : TFHIRAnnotation;
begin
  DefaultDraw := true;
  if Item.Data = nil then
    lstValidation.Canvas.Font.Color := clblack
  else
  begin
    a := TFHIRAnnotation(item.Data);
    case a.level of
      alError: begin
          lstValidation.Canvas.Font.Color := $000077;
          lstValidation.Canvas.Font.Style := [fsBold];
        end;
      alWarning: lstValidation.Canvas.Font.Color := $7777FF;
      alHint: lstValidation.Canvas.Font.Color := $770000;
      alMatch: lstValidation.Canvas.Font.Color := $007700;
    end;
  end;
end;


procedure TFHIRVisualizer.lstValidationSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if selected and (item.Data <> nil) then
    FNpp.SetSelection(TFHIRAnnotation(item.Data).start, TFHIRAnnotation(item.Data).stop);
end;

procedure TFHIRVisualizer.OnCDSResponse(manager: TCDSHooksManager; server: TRegisteredFHIRServer; context: TObject; response: TCDSHookResponse; error: String);
begin
  FLock.Lock;
  try
    if error <> '' then
      FCDSErrors.add(error+' (from '+server.name+')')
    else
      FCards.AddAll(response.cards);
  finally
    FLock.UnLock;
  end;
  PostMessage(handle, UMSG, 0, 0);
end;

function TFHIRVisualizer.postToWeb(page: String): String;
var
  wb : TWebBuffer;
begin
  result := NewGuidId;
  wb := TWebBuffer.Create;
  try
    wb.ContentType := 'text/html';
    wb.AsUnicode := page;
    FLock.Lock;
    try
      FWebCache.Add(result, wb.Link);
    finally
      FLock.Unlock;
    end;
  finally
    wb.Free;
  end;
end;

function TFHIRVisualizer.postToWeb(contentType: String; bytes: TBytes): String;
var
  wb : TWebBuffer;
begin
  result := NewGuidId;
  wb := TWebBuffer.Create;
  try
    wb.ContentType := contentType;
    wb.AsBytes := bytes;
    FLock.Lock;
    try
      FWebCache.Add(result, wb.Link);
    finally
      FLock.Unlock;
    end;
  finally
    wb.Free;
  end;
end;

procedure TFHIRVisualizer.queryCDS(hook : string; context: TFhirResource);
var
  req : TCDSHookRequest;
begin
  req := TCDSHookRequest.Create;
  try
    req.hook := hook;
    req.hookInstance := 'notepad++.fhirgplugin.instance';  // arbitrary global
    req.redirect := 'http://localhost:45654/redirect';
    req.context.Add(context.Link);
    if context is TFHIRPatient then
      req.patient := TFHIRPatient(context).id;
    FCDSManager.makeRequest(req, OnCDSResponse, nil);
  finally
    req.Free;
  end;
end;

procedure TFHIRVisualizer.queryCDSPatient(hook : string; patient: TFhirPatient);
var
  req : TCDSHookRequest;
  entry : TFHIRBundleEntry;
begin
  req := TCDSHookRequest.Create;
  try
    req.hook := hook;
    req.hookInstance := 'notepad++.fhirgplugin.instance';  // arbitrary global
    req.redirect := 'http://localhost:45654/redirect';
    req.patient := patient.id;
    entry := TFhirBundleEntry.Create;
    req.preFetch.add('patient', entry);
    entry.resource := patient.Link;
    FCDSManager.makeRequest(req, OnCDSResponse, nil);
  finally
    req.Free;
  end;
end;

function TFHIRVisualizer.generateBasicCard(path: String; focus: TFHIRObject) : TCDSHookCard;
begin
  result := TCDSHookCard.Create;
  try
    result.summary := path+' : '+TFHIRObject(focus).FhirType;
    result.sourceLabel := 'Object Model';

    result.Link;
  finally
    result.Free;
  end;
end;

procedure TFHIRVisualizer.generateTypeCard(focus, next: TFHIRObject);
var
  att : TFhirAttachment;
  md : TFhirMarkdown;
  p : TFhirParameters;
  card : TCDSHookCard;
begin
  if (focus.FhirType = 'Attachment') then
  begin
    att := TFhirAttachment(focus);
    if att.contentType.StartsWith('image/') then
    begin
      // ok we're going to return a card that displays the image
      if length(att.data) > 0 then
        FCards.Add(TCDSHookCard.Create('Image Preview', '![Preview](http://localhost:45654/'+postToWeb(att.contentType, att.data)+')', 'Element Visualizer'))
      else if isAbsoluteUrl(att.url) then
        FCards.Add(TCDSHookCard.Create('Image Preview', '![Preview]('+att.url+')', 'Element Visualizer'));
    end;
  end;

  if (focus.FhirType = 'CodeableConcept') or ((focus.FhirType = 'Coding') and (next.FhirType <> 'CodeableConcept')) then
  begin
    p := TFhirParameters.Create;
    try
       p.AddParameter('code', focus.Link as TFHIRType);
       queryCDS(TCDSHooks.codeView, p);
    finally
      p.Free;
    end;
  end;

  if (focus.FhirType = 'Identifier') then
  begin
    p := TFhirParameters.Create;
    try
       p.AddParameter('identifier', focus.Link as TFHIRType);
       queryCDS(TCDSHooks.identifierView, p);
    finally
      p.Free;
    end;
  end;

  if (focus.FhirType = 'Patient') then
  begin
    queryCDSPatient(TCDSHooks.patientView, focus as TFHIRPatient);
  end;

  {
  if (base.FhirType = 'markdown') then
  begin
    md := TFhirMarkdown(base);
    // ok we're going to return a card that displays the image
  end;

}
end;

procedure TFHIRVisualizer.Button1Click(Sender: TObject);
begin
  FCDSManager.dropCache;
end;

function TFHIRVisualizer.differentObjects(focus: array of TFHIRObject) : boolean;
var
  i : integer;
begin
  if length(focus) <> FFocusObjects.Count then
    exit(false);
  for I := 0 to length(focus) - 1 do
    if focus[i] <> FFocusObjects[i] then
      exit(false);
  result := true;
end;

procedure TFHIRVisualizer.setFocusInfo(path: String; focus: array of TFHIRObject);
var
  f : TFHIRObject;
  i : integer;
begin
  if (path <> FFocusPath) or differentObjects(focus) then
  begin
    FCDSManager.cancelAllRequests;
    sleep(1);
    FFocusPath := path;
    FFocusObjects.clear;
    for f in focus do
      if f is TFHIRObject then
      FFocusObjects.Add(TFHIRObject(f).Link);
    FLock.Lock;
    try
      FCards.Clear;
      FCDSErrors.Clear;
      if FFocusPath <> '' then
      begin
        FCards.Add(generateBasicCard(path, FFocusObjects.Last));
        for i := FFocusObjects.Count - 1 downto 0 do
        begin
          if i = 0  then
            generateTypeCard(FFocusObjects[i], nil)
          else
            generateTypeCard(FFocusObjects[i], FFocusObjects[i-1]);
        end;
      end;
    finally
      FLock.Unlock;
    end;
    if FFocusPath <> '' then
      UpdateCards
    else
      webFocus.Navigate('about:blank');
  end;
end;

procedure TFHIRVisualizer.setNarrative(s: String);
begin
  if (s <> FLastHtml) then
  begin
    webNarrative.Navigate('http://localhost:45654/'+postToWeb(s));
    FLastHtml := s;
  end;
end;

procedure TFHIRVisualizer.setPathOutcomes(matches : TAdvList<TFHIRAnnotation>; expression : TFHIRPathExpressionNode);
var
  a : TFHIRAnnotation;
  li : TListItem;
begin
  FMatchList.free;
  FMatchList := matches.link;
  lstMatches.Items.Clear;
  if (FMatchList = nil) or (FMatchList.Empty) then
    lstMatches.Items.Add.Caption := 'No Matches'
  else
  begin
    for a in FMatchList do
    begin
      li := lstMatches.Items.Add;
      li.Caption := a.description;
      li.Data := a;
    end;
  end;
  FExpression.Free;
  FExpression := expression.Link;
  vtExpressions.RootNodeCount := 0;
  if (assigned(FExpression)) then
    vtExpressions.RootNodeCount := 1;
end;

procedure TFHIRVisualizer.setValidationOutcomes(errors: TAdvList<TFHIRAnnotation>);
var
  a : TFHIRAnnotation;
  li : TListItem;
begin
  FValList.free;
  FValList := errors.link;
  lstValidation.Items.Clear;
  if FValList = nil then
    lstValidation.Items.Add.Caption := 'No Validation Results'
  else if FValList.Empty then
    lstValidation.Items.Add.Caption := 'All OK'
  else
  begin
    for a in FValList do
      if not cbErrorsOnly.Checked or (a.level = alError) then
      begin
        li := lstValidation.Items.Add;
        li.Caption := a.message;
        li.Data := a;
      end;
  end;
end;

procedure TFHIRVisualizer.TabControl1Change(Sender: TObject);
begin
  VisualiserMode := TVisualiserMode(PageControl1.TabIndex+1);
  FNpp.reset;
  FNpp.DoNppnTextModified;
end;

procedure TFHIRVisualizer.vtExpressionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  p : PTreeDataPointer;
begin
  p := vtExpressions.GetNodeData(node);
  if p.expr = nil then
    CellText := '??'
  else if (p.op) then
  begin
    CellText := CODES_TFHIRPathOperation[p.expr.Operation];
    CellText := CellText + ': '+p.expr.OpTypes.describe;
  end
  else
  begin
    case p.expr.kind of
      enkName : CellText := p.expr.name;
      enkFunction : CellText := CODES_TFHIRPathFunctions[p.expr.FunctionId]+'()';
      enkConstant : CellText := '"'+p.expr.constant+'"';
      enkGroup : CellText := '(Group)';
    end;
    CellText := CellText + ': '+p.expr.Types.describe;
  end;
end;

procedure TFHIRVisualizer.vtExpressionsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  p : PTreeDataPointer;
begin
  p := vtExpressions.GetNodeData(node);
  if p.op then
    ChildCount := 0
  else
  begin
    ChildCount := p.expr.ParameterCount;
    if (p.expr.Inner <> nil) then
      inc(ChildCount);
    if (p.expr.Group <> nil) then
      inc(ChildCount);
    if (p.expr.Operation <> popNull) then
      inc(ChildCount, 2);
  end;
end;

procedure TFHIRVisualizer.vtExpressionsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  p, pp : PTreeDataPointer;
  pe : TFHIRPathExpressionNode;
  i : integer;
begin
  p := vtExpressions.GetNodeData(Node);
  if ParentNode = nil then
  begin
    p.expr := FExpression;
  end
  else
  begin
    pp := vtExpressions.GetNodeData(parentNode);
    pe := pp.expr;

    // possible nodes:
    case pe.kind of
      enkName: i := 0; // no child nodes
      enkFunction:
        begin
          i := pe.Parameters.Count;
          if node.Index < i then
            p.expr := pe.Parameters[node.Index];
        end;
      enkConstant: i := 0; // no children
      enkGroup:
        begin
        i := 1;
        if node.Index = 0 then
          p.expr := pe.Group;
        end;
    end;
    if (pe.Inner <> nil) then
    begin
      if node.Index = i then
        p.expr := pe.Inner;
      inc(i);
    end;
    case node.Index - i of
      0: begin
         p.expr := pe;
         p.op := true;
         end;
      1: p.expr := pe.OpNext;
    else if node.index - i > 0 then
      raise Exception.Create('not done yet');
    end;
  end;
  if p.expr.tag = 1 then
    node.CheckState := csCheckedNormal;
  if not p.op and ((p.expr.Inner <> nil) or (p.expr.Group <> nil) or (p.expr.ParameterCount > 0) or (p.expr.OpNext <> nil)) then
     InitialStates := [ivsHasChildren];
end;

procedure TFHIRVisualizer.webFocusBeforeNavigate2(ASender: TObject; const pDisp: IDispatch; const URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
var
  u : String;
begin
  u := URL;
  if u = 'about:security-risk' then
  begin
    ShowMessage('This link was deemed a security risk and cannot be used');
    Cancel := true;
  end
  else if not (u.startsWith('http://localhost:45654') or u.StartsWith('about:')) then
  begin
    u := StringReplace(u, '"', '%22', [rfReplaceAll]);
    if CDSBrowser = nil then
      CDSBrowser := TCDSBrowser.Create(self);
    CDSBrowser.WebBrowser1.Navigate(u);
    CDSBrowser.ShowModal;
//    ExecuteLaunch('open', pchar(u), '', true, true);
    Cancel := true;
  end
  else
    Cancel := false;
end;

{ TWebBuffer }

function TWebBuffer.link: TWebBuffer;
begin
  result := TWebBuffer(inherited Link);
end;

end.
