unit FHIRVisualiser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, NppDockingForms, Vcl.StdCtrls, NppPlugin, Vcl.ToolWin, SystemSupport,
  Vcl.ComCtrls, System.ImageList, Vcl.ImgList, Vcl.ExtCtrls, Vcl.Styles, Vcl.Themes,
  FHIRPathDocumentation, Vcl.Buttons, Vcl.OleCtrls, SHDocVw, TextUtilities, FHIRBase,
  AdvGenerics, PluginUtilities, VirtualTrees;

type
  TVisualiserMode = (
    vmNone,  // the visualiser is not showing
    vmNarrative, // show the narrative of the resource
    vmValidation, // show validation outcomes
    vmPath, // show the path output
    vmMessages, // show messages about the content
    vmElement // information about the current selected element
  );

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
  private
    { Private declarations }

    FLastHtml : String;
    FValList : TAdvList<TFHIRAnnotation>;
    FMatchList : TAdvList<TFHIRAnnotation>;
    FExpression : TFHIRExpressionNode;
  public
    { Public declarations }
    procedure setNarrative(s : String);
    procedure setValidationOutcomes(errors : TAdvList<TFHIRAnnotation>);
    procedure setPathOutcomes(matches : TAdvList<TFHIRAnnotation>; expression : TFHIRExpressionNode);
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

procedure TFHIRVisualizer.FormKeyPress(Sender: TObject;
  var Key: Char);
begin
  inherited;
end;

// Docking code calls this when the form is hidden by either "x" or self.Hide
procedure TFHIRVisualizer.FormCreate(Sender: TObject);
begin
  inherited;
  VisualiserMode := TVisualiserMode(PageControl1.TabIndex+1);
end;

procedure TFHIRVisualizer.FormDestroy(Sender: TObject);
begin
  lstValidation.Items.clear;
  FValList.Free;
  lstMatches.Items.Clear;
  FMatchList.Free;
  FExpression.Free;
  FExpression := nil;
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

procedure TFHIRVisualizer.setNarrative(s: String);
var
  fn : String;
begin
  if (s <> FLastHtml) then
  begin
    fn := IncludeTrailingBackslash(SystemTemp)+'validation-outcomes-npp-fhir.html';
    StringToFile(s, fn, TEncoding.UTF8);
    webNarrative.Navigate('file://'+fn);
    FLastHtml := s;
  end;
end;

procedure TFHIRVisualizer.setPathOutcomes(matches : TAdvList<TFHIRAnnotation>; expression : TFHIRExpressionNode);
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
    CellText := CellText + ': '+p.expr.OpTypes.AsString(' | ');
  end
  else
  begin
    case p.expr.kind of
      entName : CellText := p.expr.name;
      entFunction : CellText := CODES_TFHIRPathFunctions[p.expr.FunctionId]+'()';
      entConstant : CellText := '"'+p.expr.constant+'"';
      entGroup : CellText := '(Group)';
    end;
    CellText := CellText + ': '+p.expr.Types.AsString(' | ');
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
    if (p.expr.Operation <> opNull) then
      inc(ChildCount, 2);
  end;
end;

procedure TFHIRVisualizer.vtExpressionsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  p, pp : PTreeDataPointer;
  pe : TFHIRExpressionNode;
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
      entName: i := 0; // no child nodes
      entFunction:
        begin
          i := pe.Parameters.Count;
          if node.Index < i then
            p.expr := pe.Parameters[node.Index];
        end;
      entConstant: i := 0; // no children
      entGroup:
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

end.
