unit ftk_text_view;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, IniFiles,
  fsl_base, fsl_utilities,
  ftk_engine_text;

const
  STEP_HEIGHT = 26;

type
  { TToolkitTextViewTriple }

  TToolkitTextViewTriple = class (TFslObject)
  private
    FBtn: TButton;
    FCombo: TComboBox;
    FLbl: TLabel;
  public
    constructor create(lbl : TLabel; combo : TComboBox; btn : TButton);
    function link : TToolkitTextViewTriple; overload;

    property lbl : TLabel read FLbl write FLbl;
    property combo : TComboBox read FCombo write FCombo;
    property btn : TButton read FBtn write FBtn;
  end;

  { TToolkitTextViewManager }

  TToolkitTextViewManager = class (TFslObject)
  private
    FIni: TIniFile;
    FPanel : TPanel;
    FTriples : TFslList<TToolkitTextViewTriple>;
    procedure SetIni(AValue: TIniFile);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure registerBase(panel : TPanel; lbl : TLabel; cbx : TComboBox; btn : TButton);
    property ini : TIniFile read FIni write SetIni;

    function canDelete(index: integer) : boolean;
    function canUp(index: integer) : boolean;
    function canDown(index: integer) : boolean;

    procedure add(index : integer);
    procedure delete(index : integer);
    procedure up(index : integer);
    procedure down(index : integer);

    procedure configureEngine(engine : TTextEngine);
  end;

implementation

{ TToolkitTextViewTriple }

constructor TToolkitTextViewTriple.create(lbl: TLabel; combo: TComboBox; btn: TButton);
begin
  inherited create;
  self.lbl := lbl;
  self.combo := combo;
  self.btn := btn;
end;

function TToolkitTextViewTriple.link: TToolkitTextViewTriple;
begin
  result := TToolkitTextViewTriple(inherited create);
end;

{ TToolkitTextViewManager }

constructor TToolkitTextViewManager.Create;
begin
  inherited Create;
  FTriples := TFslList<TToolkitTextViewTriple>.create;
end;

destructor TToolkitTextViewManager.Destroy;
var
  s : String;
  t : TToolkitTextViewTriple;
begin
  s := '';
  for t in FTriples do
    CommaAdd(s, inttostr(t.combo.ItemIndex));
  FIni.WriteString('views', 'text-pipeline', s);
  FTriples.free;
  inherited Destroy;
end;

procedure TToolkitTextViewManager.SetIni(AValue: TIniFile);
var
  s : string;
  i : integer;
begin
  FIni := AValue;
  i := 0;
  for s in FIni.readString('views', 'text-pipeline', '0').split([',']) do
  begin
    if i > 0 then
      add(i);
    FTriples[i].combo.ItemIndex := StrToIntDef(s, 0);
    inc(i);
  end;
end;

procedure TToolkitTextViewManager.registerBase(panel: TPanel; lbl: TLabel; cbx: TComboBox; btn: TButton);
begin
  FPanel := panel;
  FTriples.add(TToolkitTextViewTriple.create(lbl, cbx, btn));
end;

procedure TToolkitTextViewManager.add(index: integer);
var
  i : integer;
  lbl, lblo: TLabel;
  cbx, cbxo: TComboBox;
  btn, btno: TButton;
begin
  // we only add to the end, and reshuffle
  FPanel.Height := FPanel.Height + STEP_HEIGHT;

  lblo := FTriples[0].lbl;
  lbl := TLabel.create(lblo.Owner);
  lbl.Parent := lblo.Parent;
  lbl.top := lblo.top + STEP_HEIGHT * FTriples.count;
  lbl.left := lblo.left;
  lbl.caption := lblo.caption;

  cbxo := FTriples[0].combo;
  cbx := TComboBox.create(cbxo.Owner);
  cbx.Parent := cbxo.Parent;
  cbx.top := cbxo.top + STEP_HEIGHT * FTriples.count;
  cbx.left := cbxo.left;
  cbx.width := cbxo.width;
  cbx.anchors := cbxo.anchors;
  cbx.items.assign(cbxo.items);
  cbx.itemIndex := 0;

  btno := FTriples[0].btn;
  btn := TButton.create(btno.Owner);
  btn.Parent := btno.Parent;
  btn.top := btno.top + STEP_HEIGHT * FTriples.count;
  btn.left := btno.left;
  btn.width := btno.width;
  btn.height := btno.width;
  btn.caption := btno.caption;
  btn.anchors := btno.anchors;
  btn.OnClick := btno.OnClick;
  btn.tag := FTriples.count;

  FTriples.add(TToolkitTextViewTriple.create(lbl, cbx, btn));
  for i := FTriples.count - 1 downto index + 1 do
    FTriples[i].combo.itemIndex := FTriples[i-1].combo.itemIndex;
end;

function TToolkitTextViewManager.canDelete(index: integer) : boolean;
begin
  result := FTriples.count > 1;
end;

function TToolkitTextViewManager.canUp(index: integer): boolean;
begin
  result := index > 0;
end;

function TToolkitTextViewManager.canDown(index: integer): boolean;
begin
  result := index < FTriples.Count - 1;
end;

procedure TToolkitTextViewManager.delete(index: integer);
var
  i : integer;
begin
  for i := index to FTriples.count - 2 do
    FTriples[i].combo.itemIndex := FTriples[i+1].combo.itemIndex;
  FTriples[FTriples.count-1].lbl.free;
  FTriples[FTriples.count-1].combo.free;
  FTriples[FTriples.count-1].btn.free;
  FTriples.Delete(FTriples.count-1);
  FPanel.Height := FPanel.Height - STEP_HEIGHT;
end;

procedure TToolkitTextViewManager.up(index: integer);
var
  t : integer;
begin
  t := FTriples[index-1].combo.itemIndex;
  FTriples[index-1].combo.itemIndex := FTriples[index].combo.itemIndex;
  FTriples[index].combo.itemIndex := t;
end;

procedure TToolkitTextViewManager.down(index: integer);
var
  t : integer;
begin
  t := FTriples[index+1].combo.itemIndex;
  FTriples[index+1].combo.itemIndex := FTriples[index].combo.itemIndex;
  FTriples[index].combo.itemIndex := t;
end;

procedure TToolkitTextViewManager.configureEngine(engine: TTextEngine);
var
  t : TToolkitTextViewTriple;
begin
  for t in FTriples do
    engine.addStep(TTextEngineTransform(t.combo.itemIndex));
end;

end.

