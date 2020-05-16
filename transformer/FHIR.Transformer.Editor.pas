unit FHIR.Transformer.Editor;

interface

uses
  SysUtils, Classes, Controls, Vcl.ComCtrls, IOUtils,
  ScintEdit, ScintFormats, FHIR.Cda.Scint, FHIR.V2.Scint,
  FHIR.Support.Base, FHIR.Support.Utilities,
  FHIR.Base.Objects, FHIR.Base.Parser,
  FHIR.v2.Message,
  FHIR.R4.Scint, FHIR.R4.Context, FHIR.R4.ElementModel, FHIR.R4.Parser, FHIR.R4.Xml, FHIR.R4.Json,
  FHIR.Transformer.Workspace, FHIR.Transformer.Utilities;

type
  TEditorInformation = class (TFslObject)
  private
    FErrorLine: Integer;
    FStepLine: Integer;
    FIsDirty: boolean;
    FInfo: TWorkspaceFile;
    FMemo: TScintEdit;
    FTab: TTabSheet;
    FFileIsReadOnly: boolean;
    FReadOnly: boolean;
    FFileTime : TDateTime;
    Ffocus: boolean;

    procedure SetInfo(const Value: TWorkspaceFile);
    procedure SetReadOnly(const Value: boolean);
    function parseCDA(context: TFHIRWorkerContext): TFHIRObject;
    function parseResource(context: TFHIRWorkerContext): TFHIRObject;
  public
    constructor Create(f : TWorkspaceFile);
    destructor Destroy; override;
    property id : TWorkspaceFile read FInfo write SetInfo;
    property tab : TTabSheet read FTab write FTab;
    property memo : TScintEdit read FMemo write FMemo;
    property fileIsReadOnly : boolean read FFileIsReadOnly write FFileIsReadOnly;
    property isDirty : boolean read FIsDirty write FIsDirty;
    property ErrorLine : Integer read FErrorLine write FErrorLine;
    property StepLine : Integer read FStepLine write FStepLine;
    property readOnly : boolean read FReadOnly write SetReadOnly;
    property FileTime : TDateTime read FFileTime write FFileTime;
    property focus : boolean read Ffocus write Ffocus;
    procedure SetErrorLine(ALine: Integer);
    procedure UpdateLineMarkers(const Line: Integer);
    procedure UpdateAllLineMarkers();
    procedure HideError();

    function parse(context: TFHIRWorkerContext) : TFHIRObject;

    procedure init;
    procedure save;
  end;


implementation


{ TEditorInformation }

constructor TEditorInformation.Create;
begin
  inherited Create;
  FInfo := f;
end;

destructor TEditorInformation.destroy;
begin
  FInfo.free;
  inherited;
end;

procedure TEditorInformation.HideError;
begin
  SetErrorLine(-1);
end;

procedure TEditorInformation.init;
var
  s : String;
  bpi : TBreakPointInfo;
begin
  FMemo := TIDEScintEdit.create(tab);
  FMemo.Parent := tab;
  FMemo.LineNumbers := true;
  FMemo.Align := alClient;
  FMemo.context := self;
  s := FInfo.actualName;
  FMemo.ReadOnly := SysUtils.FileIsReadOnly(s);
  FileTime := TFile.GetLastWriteTimeUtc(s);
  fileIsReadOnly := FMemo.ReadOnly;
  FMemo.Lines.LoadFromFile(s);
  case FInfo.format of
    fmtV2 :  FMemo.Styler := TV2Styler.Create(tab);
    fmtCDA  : FMemo.Styler := TCDAStyler.Create(tab);
    fmtResource:
      if isXml(FMemo.rawText) then
        FMemo.Styler := TXmlStyler.Create(tab)
      else
        FMemo.Styler := TJsonStyler.Create(tab);
    fmtJS : FMemo.Styler := TJSStyler.Create(tab);
    fmtMap : FMemo.Styler := TFHIRMapStyler.Create(tab);
    fmtTemplate : FMemo.Styler := TLiquidStyler.Create(tab);
    fmtMarkdown : FMemo.Styler := TCommonMarkStyler.Create(tab);
  end;
  FMemo.ClearUndo;
  FMemo.CaretLine := fInfo.row;
  isDirty := false;
  if FInfo.compileStatus = csError then
    ErrorLine := FInfo.ErrorLine
  else
    ErrorLine := -1;

  StepLine := -1;
  for bpi in FInfo.BreakPoints do
    UpdateLineMarkers(bpi.line);
end;

function TEditorInformation.parseCDA(context: TFHIRWorkerContext): TFHIRObject;
var
  ss : TStringStream;
begin
  ss := TStringStream.Create(memo.RawText, TEncoding.UTF8);
  try
    result := TFHIRMMManager.parse(context, ss, ffXml);
  finally
    ss.Free;
  end;
end;

function TEditorInformation.parseResource(context: TFHIRWorkerContext): TFHIRObject;
var
  p : TFHIRParser;
begin
  if isXml(memo.RawText) then
    p := TFHIRXmlParser.Create(context.link, context.lang)
  else
    p := TFHIRJsonParser.Create(context.link, context.lang);
  try
    result := p.parseResource(memo.RawText);
  finally
    p.Free;
  end;
end;


function TEditorInformation.parse(context: TFHIRWorkerContext): TFHIRObject;
begin
  case id.format of
    fmtV2: result := TV2Parser.parse(memo.RawText);
    fmtCDA: result := parseCDA(context);
    fmtResource: result := parseResource(context);
    fmtJS: raise EFslException.Create('Not supported - you cannot use FHIRPath with this type');
    fmtMap: raise EFslException.Create('Not done yet');
    fmtTemplate: raise EFslException.Create('Not supported - you cannot use FHIRPath with this type');
    fmtMarkdown: raise EFslException.Create('Not supported - you cannot use FHIRPath with this type');
  end;

end;

procedure TEditorInformation.save;
var
  s : String;
begin
  if self = nil then
    exit;
  s := id.actualName;
  memo.Lines.SaveToFile(s);
  FileTime := TFile.GetLastWriteTimeUtc(s);
  isDirty := false;
end;

procedure TEditorInformation.SetInfo(const Value: TWorkspaceFile);
begin
  FInfo.free;
  FInfo := Value;
end;

procedure TEditorInformation.SetReadOnly(const Value: boolean);
begin
  FReadOnly := Value;
  if FReadOnly then
    memo.ReadOnly := true
  else
    memo.ReadOnly := fileIsReadOnly;
end;

procedure TEditorInformation.SetErrorLine(ALine: Integer);
var
  OldLine: Integer;
begin
  if ErrorLine <> ALine then
  begin
    OldLine := ErrorLine;
    ErrorLine := ALine;
    if OldLine >= 0 then
      UpdateLineMarkers(OldLine);
    if ErrorLine >= 0 then
    begin
      memo.CaretLine := ErrorLine;
      memo.ScrollCaretIntoView;
      UpdateLineMarkers(ErrorLine);
    end;
  end;
end;

procedure TEditorInformation.UpdateLineMarkers(const Line: Integer);
var
  bpi : TBreakPointInfo;
begin
  if Line >= Memo.Lines.Count then
    Exit;

  { Delete all markers on the line. To flush out any possible duplicates,
    even the markers we'll be adding next are deleted. }
  if Memo.GetMarkers(Line) <> [] then
    Memo.DeleteAllMarkersOnLine(Line);

  if StepLine = Line then
    Memo.AddMarker(Line, mmLineStep)
  else if ErrorLine = Line then
    Memo.AddMarker(Line, mmLineError)
  else if id.hasBreakPoint(line, bpi) then
    if (bpi.invalid) then
      Memo.AddMarker(Line, mmLineBreakpointBad)
    else
      Memo.AddMarker(Line, mmLineBreakpoint);
end;

procedure TEditorInformation.UpdateAllLineMarkers;
var
  Line: Integer;
begin
  for Line := 0 to Memo.Lines.Count-1 do
    UpdateLineMarkers(Line);
end;


end.
