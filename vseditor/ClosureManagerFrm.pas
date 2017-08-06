unit ClosureManagerFrm;

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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Grids,
  FHIRResources, Vcl.ComCtrls, FHIRtypes, StringSupport, NppForms, FHIRContext, FHIRClient,
  AdvObjects, AdvGenerics, AdvJson, GuidSupport, FHIRUtilities;

type
  TClosureDirection = (cdNull, cdSubsumes, cdSubsumed);

  TClosureTableRecordSource = class (TFHIRCoding)
  private
    FTargets: TAdvList<TFHIRCoding>;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link :  TClosureTableRecordSource; overload;

    property targets : TAdvList<TFHIRCoding> read FTargets;
    procedure add(uri, code : String);
  end;

  TClosureTableRecord = class (TAdvObject)
  private
    FConcepts: TAdvList<TFHIRCoding>;
    FMaps : TAdvList<TClosureTableRecordSource>;

    FId: string;
    FName: String;
    FFilename : String;
    FVersion: String;
    function GetLinks(row, col: integer): TClosureDirection;
    function hasMap(src, tgt : TFHIRCoding) : boolean;
    procedure save;
    procedure process(cm : TFHIRConceptMap);
    function getSource(uri, code : String) : TClosureTableRecordSource;
    function GetMapCount: integer;
  public
    constructor create; overload; override;
    constructor create(filename : String); overload;
    constructor create(filename, id, name : String); overload;
    destructor Destroy; override;

    function link :  TClosureTableRecord; overload;

    property id : string read FId write FId;
    property name : String read FName write FName;
    property version : String read FVersion write FVersion;
    property concepts : TAdvList<TFHIRCoding> read FConcepts;
    property maps : TAdvList<TClosureTableRecordSource> read FMaps;
    property links[row, col: integer] : TClosureDirection read GetLinks;
    property mapCount : integer read GetMapCount;
  end;

  TClosureManagerForm = class(TNppForm)
    Panel1: TPanel;
    Label1: TLabel;
    cbxClosures: TComboBox;
    Button1: TButton;
    Bevel1: TBevel;
    btnAddConcept: TButton;
    btnReset: TButton;
    Button4: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    grid: TStringGrid;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    Panel4: TPanel;
    Panel5: TPanel;
    lbConcepts: TListBox;
    lbClosures: TListBox;
    btnupdate: TButton;
    pnlStatus: TPanel;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbxClosuresChange(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnAddConceptClick(Sender: TObject);
    procedure gridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure gridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure btnResetClick(Sender: TObject);
    procedure btnupdateClick(Sender: TObject);
  private
    FWorker: TFHIRWorkerContext;
    FClient: TFhirHTTPClient;
    procedure generateClosureSummary;
    procedure addClosure(s: String);
    procedure SetClient(const Value: TFhirHTTPClient);
    procedure SetWorker(const Value: TFHIRWorkerContext);
    { Private declarations }
  public
    { Public declarations }
    Property Worker : TFHIRWorkerContext read FWorker write SetWorker;
    Property Client : TFhirHTTPClient read FClient write SetClient;
  end;

var
  ClosureManagerForm: TClosureManagerForm;

implementation

{$R *.dfm}

//uses ConceptLookupFrm;

procedure TClosureManagerForm.addClosure(s: String);
var
  pin : TFhirParameters;
  pout : TFhirResource;
  ct : TClosureTableRecord;
begin
  ct := TClosureTableRecord.create;
  try
    ct.id := NewGuidId;
    ct.Name := s;
    ct.version := '0';
    ct.FFilename := IncludeTrailingBackslash('c:\temp')+'ct-'+ct.id+'.json';
    pin := TFhirParameters.Create;
    try
      pin.AddParameter('name', ct.id);
      pout := FClient.operation(frtConceptMap, 'closure', pin);
      try
        if not (pout as TFhirParameters).bool['outcome'] then
          raise Exception.Create('Unexpected response from server');
      finally
        pout.Free;
      end;
    finally
      pin.Free;
    end;
    ct.save;
//    ini.WriteString('closures', ct.id, ct.name);
//    closures.Add(s, ct.link);
  finally
    ct.Free;
  end;
end;

{ TForm3 }

procedure TClosureManagerForm.Button1Click(Sender: TObject);
var
  s : String;
begin
  if InputQuery('Closure Name', 'New Closure', s) then
  begin
    AddClosure(s);
    cbxClosures.Items.Add(s);
    cbxClosures.ItemIndex := cbxClosures.Items.Count - 1;
    cbxClosuresChange(nil);
  end;
end;

procedure TClosureManagerForm.btnAddConceptClick(Sender: TObject);
begin
//  ConceptLookupForm.Context := FContext.Link;
//  if ConceptLookupForm.ShowModal = mrOk then
//  begin
//    FContext.AddToClosure(cbxClosures.text, ConceptLookupForm.Selected);
//    generateClosureSummary;
//  end;
end;

procedure TClosureManagerForm.btnResetClick(Sender: TObject);
begin
//  FContext.ResetClosure(cbxClosures.text);
//  generateClosureSummary;
end;

procedure TClosureManagerForm.Button4Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TClosureManagerForm.btnupdateClick(Sender: TObject);
begin
//  FContext.UpdateClosure(cbxClosures.text);
//  generateClosureSummary;
end;

procedure TClosureManagerForm.cbxClosuresChange(Sender: TObject);
begin
  generateClosureSummary;
end;

procedure TClosureManagerForm.FormDestroy(Sender: TObject);
begin
  FWorker.Free;
  FClient.Free;
end;

procedure TClosureManagerForm.FormShow(Sender: TObject);
begin
//  FContext.LoadClosures(cbxClosures.Items);
//  if cbxClosures.Items.Count > 0 then
//    cbxClosures.ItemIndex := 0;
//  cbxClosuresChange(nil);
end;

procedure TClosureManagerForm.generateClosureSummary;
//var
//  ct : TClosureTableRecord;
//  c : TFHIRCoding;
//  s :  TClosureTableRecordSource;
//  i, j : integer;
begin
//  ct := FContext.ClosureDetails(cbxClosures.text);
//  lbConcepts.items.Clear;
//  lbClosures.items.Clear;
//  if ct = nil then
//  begin
//    grid.ColCount := 1;
//    grid.RowCount := 1;
//    grid.Cells[0, 0] := 'Error - no match';
//    pnlStatus.caption := 'No Closure';
//    btnAddConcept.Enabled := false;
//    btnUpdate.Enabled := false;
//    btnReset.Enabled := false;
//  end
//  else
//  begin
//    btnAddConcept.Enabled := true;
//    btnUpdate.Enabled := true;
//    btnReset.Enabled := true;
//    pnlStatus.caption := '  Closure id = '+ct.id+', version = '+ct.version+', '+inttostr(ct.concepts.Count)+' concepts, '+inttostr(ct.mapCount)+' maps';
//
//    for c in ct.concepts do
//      lbConcepts.items.Add(c.display+' ('+c.code+')');
//
//    for s in ct.Maps do
//      for c in s.targets do
//        lbClosures.items.Add(s.code + ' subsumes '+c.code);
//
//    grid.ColCount := ct.concepts.count + 1;
//    grid.RowCount := ct.concepts.count + 1;
//    grid.Cells[0, 0] := 'Map';
//    for i := 0 to ct.concepts.Count - 1 do
//    begin
//      grid.Cells[0, i+1] := ct.concepts[i].display;
//      grid.Cells[i+1, 0] := ct.concepts[i].display;
//      for j := 0 to ct.concepts.count - 1 do
//        case ct.links[i, j] of
//          cdNull :
//            begin
//            grid.Cells[j+1, i+1] := '';
//            grid.Cells[i+1, j+1] := '';
//            end;
//          cdSubsumes :
//            begin
//            grid.Cells[j+1, i+1] := '<';
//            grid.Cells[i+1, j+1] := '>';
//            end;
//          cdSubsumed :
//            begin
//            grid.Cells[j+1, i+1] := '>';
//            grid.Cells[i+1, j+1] := '<';
//            end;
//        end;
//      grid.Cells[i+1, i+1] := 'Y';
//    end;
//  end;
end;

procedure TClosureManagerForm.gridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  c : TColor;
  s : String;
begin
  if (ACol > 0) and (aRow > 0) then
  begin
    s := Grid.Cells[aCol, ARow];
    c := clWhite;
    if s = '<' then
      c := $b3ffb3
    else if s = '>' then
      c := $cce5ff
    else if (s = 'Y') then
      c := $e6e6e6;
    Grid.Canvas.Brush.Color := c;
    Grid.Canvas.FillRect(Rect);
    Grid.Canvas.TextOut(Rect.Left+2,Rect.Top+2, Grid.Cells[ACol, ARow]);
  end;
end;

procedure TClosureManagerForm.gridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  MouseCell: TGridCoord;
  cpos : TPoint;
  wait : longint;
begin
  MouseCell := TStringGrid(Sender).MouseCoord(X, Y);
  if MouseCell.X = 0 then
    Grid.hint := 'This text'
  else
    Grid.Hint := 'Other text';
  wait := 5;
  Grid.Perform(CM_HINTSHOWPAUSE, Ord(true), Longint(@Wait));
  Application.HintColor := clYellow;
  cpos.X := x;
  cpos.Y := y;
  Application.ActivateHint(cpos);
end;


procedure TClosureManagerForm.SetClient(const Value: TFhirHTTPClient);
begin
  FCLient.Free;
  FClient := Value;
end;

procedure TClosureManagerForm.SetWorker(const Value: TFHIRWorkerContext);
begin
  FWorker.Free;
  FWorker := Value;
end;

{ TClosureTableRecordSource }

procedure TClosureTableRecordSource.add(uri, code: String);
var
  c : TFhirCoding;
begin
  for c in targets do
    if (c.system = uri) and (c.code = code) then
      exit;
  c := TFhirCoding.Create;
  try
    c.system := uri;
    c.code := code;
    targets.Add(c.link);
  finally
    c.Free;
  end;
end;

constructor TClosureTableRecordSource.Create;
begin
  inherited Create;
  FTargets := TAdvList<TFHIRCoding>.create;
end;

destructor TClosureTableRecordSource.Destroy;
begin
  FTargets.Free;
  inherited;
end;

function TClosureTableRecordSource.link: TClosureTableRecordSource;
begin
  result := TClosureTableRecordSource(inherited Link);
end;

{ TClosureTableRecord }

constructor TClosureTableRecord.create(filename: String);
var
  f : TFileStream;
  json, ao, ao1 : TJsonObject;
  a, a1 : TJsonNode;
  c : TFhirCoding;
  s : TClosureTableRecordSource;
begin
  Create;
  FFilename := filename;
  f := TFileStream.Create(filename, fmOpenRead + fmShareDenyWrite);
  try
    json := TJSONParser.Parse(f);
    try
      id := json.str['id'];
      name := json.str['name'];
      version := json.str['version'];
      for a in json.arr['concepts'] do
      begin
        ao := a as TJsonObject;
        c := TFhirCoding.Create;
        FConcepts.Add(c);
        c.system := ao.str['system'];
        c.code := ao.str['code'];
        c.display := ao.str['display'];
      end;
      for a in json.arr['maps'] do
      begin
        ao := a as TJsonObject;
        s := TClosureTableRecordSource.Create;
        FMaps.Add(s);
        s.system := ao.str['system'];
        s.code := ao.str['code'];
        for a1 in ao.arr['targets'] do
        begin
          ao1 := a1 as TJsonObject;
          c := TFhirCoding.Create;
          s.targets.Add(c);
          c.system := ao1.str['system'];
          c.code := ao1.str['code'];
        end;
      end;
    finally
      json.Free;
    end;
  finally
    f.Free;
  end;
end;

procedure TClosureTableRecord.save;
var
  f : TFileStream;
  json, ao, ao1 : TJsonObject;
  arr, arr1 : TJsonArray;
  c : TFhirCoding;
  s : TClosureTableRecordSource;
begin
  json := TJsonObject.Create;
  try
    json.str['id'] := id;
    json.str['name'] := name;
    json.str['version'] := version;
    arr := json.forceArr['concepts'];
    for c in FConcepts do
    begin
      ao := arr.addObject;
      ao.str['system'] := c.system;
      ao.str['code'] := c.code;
      ao.str['display'] := c.display;
    end;
    arr := json.forceArr['maps'];
    for s in FMaps do
    begin
      ao := arr.addObject;
      ao.str['system'] := s.system;
      ao.str['code'] := s.code;
      arr1 := ao.forceArr['targets'];
      for c in s.targets do
      begin
        ao1 := arr1.addObject;
        ao1.str['system'] := c.system;
        ao1.str['code'] := c.code;
      end;
    end;

    f := TFileStream.Create(Ffilename, fmCreate);
    try
      TJSONWriter.writeObject(f, json, true);
    finally
      f.Free;
    end;
  finally
    json.Free;
  end;
end;

constructor TClosureTableRecord.create(filename, id, name: String);
begin
  Create;
  FFilename := filename;
  self.id := id;
  self.name := name;
  version := '0';
end;

destructor TClosureTableRecord.Destroy;
begin
  FMaps.Free;
  FConcepts.Free;
  inherited;
end;

constructor TClosureTableRecord.create;
begin
  inherited;
  FMaps := TAdvList<TClosureTableRecordSource>.create;
  FConcepts := TAdvList<TFHIRCoding>.create;
end;

function TClosureTableRecord.GetLinks(row, col: integer): TClosureDirection;
var
  r, c : TFHIRCoding;
begin
  r := FConcepts[row];
  c := FConcepts[col];
  if hasMap(r, c) then
    result := cdSubsumes
  else if hasMap(c, r) then
    result := cdSubsumed
  else
    result := cdNull;
end;

function TClosureTableRecord.GetMapCount: integer;
var
  s : TClosureTableRecordSource;
begin
  result := FMaps.Count;
  for s in FMaps do
    result := result + s.targets.Count;

end;

function TClosureTableRecord.getSource(uri, code: String): TClosureTableRecordSource;
begin
  for result in FMaps do
    if (result.system = uri) and (result.code = code) then
      exit;
  result := TClosureTableRecordSource.Create;
  try
    result.system := uri;
    result.code := code;
    FMaps.Add(result.link);
  finally
    result.Free;
  end;
end;

function TClosureTableRecord.hasMap(src, tgt: TFHIRCoding): boolean;
var
  s : TClosureTableRecordSource;
  c : TFHIRCoding;
begin
  result := false;
  for s in FMaps do
    if (s.system = src.system) and (s.code = src.code) then
      for c in s.targets do
        if (c.system = tgt.system) and (c.code = tgt.code) then
          exit(true);
end;

function TClosureTableRecord.link: TClosureTableRecord;
begin
  result := TClosureTableRecord(inherited Link);
end;

procedure TClosureTableRecord.process(cm: TFHIRConceptMap);
var
  element : TFhirConceptMapElement;
  src : TClosureTableRecordSource;
  target : TFhirConceptMapElementTarget;
begin
  version := cm.version;
  for element in cm.elementList do
    for target in element.targetList do
      if target.equivalence = ConceptMapEquivalenceSubsumes then
      begin
        src := getSource(element.system, element.code);
        src.add(target.system, target.code);
      end
      else if target.equivalence = ConceptMapEquivalenceSpecializes then
      begin
        src := getSource(target.system, target.code);
        src.add(element.system, element.code);
      end
      else
        raise Exception.Create('Unhandled equivalance '+CODES_TFhirConceptMapEquivalenceEnum[target.equivalence]);
end;

end.
