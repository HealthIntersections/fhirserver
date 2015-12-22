unit ClosureManagerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Grids, ValueSetEditorCore,
  FHIRResources, ConceptLookupFrm, Vcl.ComCtrls, FHIRtypes, StringSupport;

type
  TClosureManagerForm = class(TForm)
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
    FContext: TValueSetEditorContext;
    procedure SetContext(const Value: TValueSetEditorContext);
    procedure generateClosureSummary;
    { Private declarations }
  public
    { Public declarations }
    Property Context : TValueSetEditorContext read FContext write SetContext;

  end;

var
  ClosureManagerForm: TClosureManagerForm;

implementation

{$R *.dfm}

{ TForm3 }

procedure TClosureManagerForm.Button1Click(Sender: TObject);
var
  s : String;
begin
  if InputQuery('Closure Name', 'New Closure', s) then
  begin
    FContext.AddClosure(s);
    cbxClosures.Items.Add(s);
    cbxClosures.ItemIndex := cbxClosures.Items.Count - 1;
    cbxClosuresChange(nil);
  end;
end;

procedure TClosureManagerForm.btnAddConceptClick(Sender: TObject);
begin
  ConceptLookupForm.Context := FContext.Link;
  if ConceptLookupForm.ShowModal = mrOk then
  begin
    FContext.AddToClosure(cbxClosures.text, ConceptLookupForm.Selected);
    generateClosureSummary;
  end;
end;

procedure TClosureManagerForm.btnResetClick(Sender: TObject);
begin
  FContext.ResetClosure(cbxClosures.text);
  generateClosureSummary;
end;

procedure TClosureManagerForm.Button4Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TClosureManagerForm.btnupdateClick(Sender: TObject);
begin
  FContext.UpdateClosure(cbxClosures.text);
  generateClosureSummary;
end;

procedure TClosureManagerForm.cbxClosuresChange(Sender: TObject);
begin
  generateClosureSummary;
end;

procedure TClosureManagerForm.FormDestroy(Sender: TObject);
begin
  FContext.Free;
end;

procedure TClosureManagerForm.FormShow(Sender: TObject);
begin
  FContext.LoadClosures(cbxClosures.Items);
  if cbxClosures.Items.Count > 0 then
    cbxClosures.ItemIndex := 0;
  cbxClosuresChange(nil);
end;

procedure TClosureManagerForm.generateClosureSummary;
var
  ct : TClosureTableRecord;
  c : TFHIRCoding;
  s :  TClosureTableRecordSource;
  i, j : integer;
begin
  ct := FContext.ClosureDetails(cbxClosures.text);
  lbConcepts.items.Clear;
  lbClosures.items.Clear;
  if ct = nil then
  begin
    grid.ColCount := 1;
    grid.RowCount := 1;
    grid.Cells[0, 0] := 'Error - no match';
    pnlStatus.caption := 'No Closure';
    btnAddConcept.Enabled := false;
    btnUpdate.Enabled := false;
    btnReset.Enabled := false;
  end
  else
  begin
    btnAddConcept.Enabled := true;
    btnUpdate.Enabled := true;
    btnReset.Enabled := true;
    pnlStatus.caption := '  Closure id = '+ct.id+', version = '+ct.version+', '+inttostr(ct.concepts.Count)+' concepts, '+inttostr(ct.mapCount)+' maps';

    for c in ct.concepts do
      lbConcepts.items.Add(c.display+' ('+c.code+')');

    for s in ct.Maps do
      for c in s.targets do
        lbClosures.items.Add(s.code + ' subsumes '+c.code);

    grid.ColCount := ct.concepts.count + 1;
    grid.RowCount := ct.concepts.count + 1;
    grid.Cells[0, 0] := 'Map';
    for i := 0 to ct.concepts.Count - 1 do
    begin
      grid.Cells[0, i+1] := ct.concepts[i].display;
      grid.Cells[i+1, 0] := ct.concepts[i].display;
      for j := 0 to ct.concepts.count - 1 do
        case ct.links[i, j] of
          cdNull :
            begin
            grid.Cells[j+1, i+1] := '';
            grid.Cells[i+1, j+1] := '';
            end;
          cdSubsumes :
            begin
            grid.Cells[j+1, i+1] := '<';
            grid.Cells[i+1, j+1] := '>';
            end;
          cdSubsumed :
            begin
            grid.Cells[j+1, i+1] := '>';
            grid.Cells[i+1, j+1] := '<';
            end;
        end;
      grid.Cells[i+1, i+1] := 'Y';
    end;
  end;
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

procedure TClosureManagerForm.SetContext(const Value: TValueSetEditorContext);
begin
  FContext.Free;
  FContext := Value;
end;

end.
