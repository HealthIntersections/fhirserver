unit FHIR.Toolkit.FHIREditor;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  Controls, StdCtrls, ComCtrls,
  SynEditHighlighter, SynHighlighterXml, SynHighlighterJson,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.MXml, FHIR.Support.Logging, FHIR.Support.Stream,
  FHIR.Web.Parsers,
  FHIR.Base.Objects, FHIR.Base.Factory, FHIR.Base.Parser,
  {FHIR.R2.Parsers, FHIR.R3.Parsers, }FHIR.R4.Factory, {FHIR.R5.Parsers, }
  FHIR.R4.Resources.Canonical,
  FHIR.LCL.Synchroniser,
  FHIR.Toolkit.Context, FHIR.Toolkit.Store,
  FHIR.Toolkit.BaseEditor;

type

  { TFHIREditor }

  TFHIREditor = class (TBaseEditor)
  private
    FFormat : TFHIRFormat;
    FVersion : TFHIRVersion;
    FFactory : TFHIRFactory;
    FResource : TFHIRResourceV;
    actTestEditing : TContentAction;
    FTree : TTreeView;
    FMemo : TMemo;
    FSync : TFHIRSynEditSynchroniser;
    function parseResource(source : String) : TFHirResourceV;
    procedure DoTestEditing(sender : TObject);
    procedure DoTreeClick(sender : TObject);
    procedure LoadObject(item : TTreeNode; obj : TFHIRObject);
  protected
    function AddActions(tb : TToolBar) : boolean; override;
    function makeHighlighter : TSynCustomHighlighter; override;
    procedure getNavigationList(navpoints : TStringList); override;
  public
    constructor Create(context : TToolkitContext; session : TToolkitEditSession; store : TStorageService); override;
    destructor Destroy; override;

    procedure ContentChanged; override;
    procedure newContent(); override;
    function FileExtension : String; override;
    procedure validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList); override;

    function hasDesigner : boolean; override;
    procedure makeDesigner; override;
    procedure updateDesigner; override;
  end;


implementation

constructor TFHIREditor.Create(context: TToolkitContext; session: TToolkitEditSession; store: TStorageService);
begin
  inherited Create(context, session, store);
  FFormat := TFHIRFormat(ord(StringArrayIndexOf(CODES_TFHIRFormat, session.info.Values['Format'])));
  FFactory := TFHIRFactoryR4.create;
end;

destructor TFHIREditor.Destroy;
begin
  FResource.Free;
  FSync.Free;
  inherited Destroy;
end;

function TFHIREditor.parseResource(source: String): TFHirResourceV;
var
  p : TFHIRParser;
begin
  p := FFactory.makeParser(nil, FFormat, THTTPLanguages.Create('en'));
  try
    p.KeepParseLocations := true;
    result := p.parseResource(source);
  finally
    p.Free;
  end;
end;

procedure TFHIREditor.DoTestEditing(sender: TObject);
var
  cs : TFHIRCodeSystem;
begin
  cs := FSync.resource as TFHIRCodeSystem;
  FSync.changeProperty(cs, cs.nameElement);
  cs.name := 'My Test';
  FSync.commit;
end;

procedure TFHIREditor.DoTreeClick(sender: TObject);
var
  o : TFHIRObject;
  c : TFHIRComposer;
begin
  if FTree.Selected <> nil then
  begin
    o := TFhirObject(FTree.Selected.Data);
    if o.LocationData.hasLocation2 then
    begin
      TextEditor.SelStart := TextEditor.RowColToCharIndex(o.LocationData.parseStart2.toPoint);
      TextEditor.SelEnd := TextEditor.RowColToCharIndex(o.LocationData.parseFinish2.toPoint);
    end
    else
    begin
      TextEditor.SelStart := TextEditor.RowColToCharIndex(o.LocationData.parseStart.toPoint);
      TextEditor.SelEnd := TextEditor.RowColToCharIndex(o.LocationData.parseFinish.toPoint);
    end;
  end;

  //c := FFactory.makeComposer(nil, FFormat, THTTPLanguages.create('en'), TFHIROutputStyle.OutputStylePretty);
  //try
  //  FMemo.text := c.composeBase(o);
  //finally
  //  sync.free;
  //end;
end;

function TFHIREditor.AddActions(tb : TToolBar): boolean;
begin
  actTestEditing := makeAction(tb, 'Test Editing', 11, 0, DoTestEditing);
  Result:= true;
end;

function TFHIREditor.makeHighlighter: TSynCustomHighlighter;
begin
  if FFormat = ffJson then
    Result := TSynJSonSyn.create(nil)
  else
    Result := TSynXmlSyn.create(nil);
end;

procedure TFHIREditor.getNavigationList(navpoints: TStringList);
var
  de, e : TMXmlElement;
  c : integer;
  properties : TFHIRPropertyList;
  prop : TFHIRProperty;
  incNext : boolean;
  v : TFHIRObject;
begin
  if (FResource = nil) then
  try
    FResource := parseResource(FContent.text);
  except
  end;
  if FResource <> nil then
  begin
    properties := FResource.createPropertyList(false);
    try
      for prop in properties do
      begin
        if prop.hasValue then
        begin
          if incNext then
          begin
            navpoints.addObject(prop.Name, TObject(prop.Values[0].LocationData.ParseStart.line));
            incNext := false;
          end
          else if prop.Name = 'text' then
          begin
            incNext := true;
          end
          else if prop.Name = 'contained' then
          begin
            for v in prop.Values do
              if (navpoints.count < 30) then
                navpoints.addObject(v.fhirType, TObject(v.LocationData.ParseStart.line));
          end
          else if prop.Type_ = 'BackboneElement' then
          begin
            for v in prop.Values do;
              if (navpoints.count < 30) then
                navpoints.addObject(prop.name, TObject(v.LocationData.ParseStart.line));
          end;
        end;
      end;
    finally
      properties.Free;
    end;
  end;
end;

procedure TFHIREditor.ContentChanged;
begin
  FResource.Free;
  FResource := nil;
end;

procedure TFHIREditor.newContent();
begin
  Session.HasBOM := false;
  Session.EndOfLines := PLATFORM_DEFAULT_EOLN;
  Session.Encoding := senUTF8;

  TextEditor.Text := '<Resource xmlns="http://hl7.org/fhir">'+#13#10+'  <id value="xx"/>'+#13#10+'</Resource>'+#13#10;
  updateToolbarButtons;
end;

function TFHIREditor.FileExtension: String;
begin
  if FFormat = ffJson then
    result := 'json'
  else
    result := 'xml';
end;

function asPathExpression(path : TFslList<TFHIRLocatedNode>) : String;
var
  loc : TFHIRLocatedNode;
begin
  result := '';
  for loc in path do
  begin
    if loc.prop = nil then
      result := loc.value.fhirType
    else if loc.prop.IsList then
      result := result + '.'+ loc.prop.Name+'['+inttostr(loc.prop.values.IndexByReference(loc.prop))+']'
    else if loc.prop.name.endsWith('[x]') then
      result := result + '.'+ loc.prop.Name.replace('[x]', '.ofType('+loc.value.fhirType+')')
    else
      result := result + '.'+ loc.prop.Name;
  end;
end;

procedure TFHIREditor.validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList);
var
  i : integer;
  s : String;
  t : QWord;
  path : TFslList<TFHIRLocatedNode>;
begin
  updateToContent;
  t := StartValidating;
  try
    if (validate) then
    begin
      for i := 0 to FContent.count - 1 do
      begin
        s := TextEditor.lines[i];
        checkForEncoding(s, i);
      end;
    end;
    FResource.Free;
    FResource := nil;
    try
      FResource := parseResource(FContent.text);
      inc(cursor.line);
      inc(cursor.col);
      path := FResource.findLocation(cursor);
      try
        inspection.AddPair('Version', FFactory.versionString);
        inspection.AddPair('Path', asPathExpression(path));
        if (path.count > 0) then
        begin
          inspection.AddPair('Type', path[path.count - 1].value.fhirType);
          if (path.count > 1) then
          begin
            if (path[path.count - 1].prop.IsList) then
              inspection.AddPair('Repeats', 'True')
            else
              inspection.AddPair('Repeats', 'False');
          end;
        end;
      finally
        path.free;
      end;
    except
      on e : EParserException do
      begin
        validationError(e.Line, e.Col, e.message);
      end;
      on e : Exception do
      begin
        validationError(1, 1, 'Error Parsing Resource: '+e.message);
      end;
    end;
  finally
    finishValidating(validate, t);
  end;
end;

function TFHIREditor.hasDesigner: boolean;
begin
  Result := true;
end;

procedure TFHIREditor.makeDesigner;
begin
  inherited makeDesigner;
  FSync := TFHIRSynEditSynchroniser.create;
  FSync.SynEdit := TextEditor;
  FSync.Factory := TFHIRFactoryR4.create;
  FSync.Format := FFormat;

  FMemo := TMemo.create(FDesignerPanelWork);
  FMemo.parent := FDesignerPanelWork;
  FMemo.align := alBottom;
  FMemo.ReadOnly := true;
  FMemo.Height := 300;

  FTree := TTreeView.create(FDesignerPanelWork);
  FTree.parent := FDesignerPanelWork;
  FTree.align := alClient;
  FTree.ReadOnly := true;
  FTree.OnClick := DoTreeClick;
end;

procedure TFHIREditor.LoadObject(item : TTreeNode; obj : TFHIRObject);
var
  prop : TFHIRNamedValue;
  child : TTreeNode;
begin
  for prop in obj.getNamedChildren.forEnum do
  begin
    if prop.value.isPrimitive then
    begin
      child := FTree.items.AddChildObject(item, prop.Name +': '+prop.value.fhirType+' = '+prop.value.primitiveValue, prop.value);
    end
    else
    begin
      child := FTree.Items.AddChildObject(item, prop.Name +': '+prop.value.fhirType, prop.value);
      LoadObject(child, prop.value);
    end;
  end;

end;

procedure TFHIREditor.updateDesigner;
var
  root : TTreeNode;
begin
  FResource.Free;
  FResource := nil;
  FSync.load;
  FResource := FSync.Resource.link;
  FTree.items.Clear;
  root := FTree.Items.Add (nil, 'Resource '+FResource.fhirType);
  root.Data := FResource;
  loadObject(root, FResource);
end;


end.

