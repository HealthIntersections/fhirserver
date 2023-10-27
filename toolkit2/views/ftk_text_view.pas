unit ftk_text_view;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

{$i fhir.inc}

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
    constructor Create(lbl : TLabel; combo : TComboBox; btn : TButton);
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

constructor TToolkitTextViewTriple.Create(lbl: TLabel; combo: TComboBox; btn: TButton);
begin
  inherited Create;
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
  FTriples := TFslList<TToolkitTextViewTriple>.Create;
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
  FTriples.add(TToolkitTextViewTriple.Create(lbl, cbx, btn));
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
  lbl := TLabel.Create(lblo.Owner);
  lbl.Parent := lblo.Parent;
  lbl.top := lblo.top + STEP_HEIGHT * FTriples.count;
  lbl.left := lblo.left;
  lbl.caption := lblo.caption;

  cbxo := FTriples[0].combo;
  cbx := TComboBox.Create(cbxo.Owner);
  cbx.Parent := cbxo.Parent;
  cbx.top := cbxo.top + STEP_HEIGHT * FTriples.count;
  cbx.left := cbxo.left;
  cbx.width := cbxo.width;
  cbx.anchors := cbxo.anchors;
  cbx.items.assign(cbxo.items);
  cbx.itemIndex := 0;

  btno := FTriples[0].btn;
  btn := TButton.Create(btno.Owner);
  btn.Parent := btno.Parent;
  btn.top := btno.top + STEP_HEIGHT * FTriples.count;
  btn.left := btno.left;
  btn.width := btno.width;
  btn.height := btno.width;
  btn.caption := btno.caption;
  btn.anchors := btno.anchors;
  btn.OnClick := btno.OnClick;
  btn.tag := FTriples.count;

  FTriples.add(TToolkitTextViewTriple.Create(lbl, cbx, btn));
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

