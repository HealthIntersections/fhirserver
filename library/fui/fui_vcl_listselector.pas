unit FHIR.Ui.ListSelector;

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

{$I fhir.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst, Vcl.ExtCtrls;

type
  TListSelectorForm = class(TForm)
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    btnok: TButton;
    btnCancel: TButton;
    Panel2: TPanel;
    ListBox1: TCheckListBox;
    btnOkAll: TButton;
    cbDontAsk: TCheckBox;
    procedure CheckBox1Click(Sender: TObject);
    procedure ListBox1ClickCheck(Sender: TObject);
    procedure btnOkAllClick(Sender: TObject);
  private
    FOkWithNoneSelected: boolean;
    FVerb: String;
    procedure SetVerb(const Value: String);
  public
    property okWithNoneSelected : boolean read FOkWithNoneSelected write FOkWithNoneSelected;
    property verb : String read FVerb write SetVerb;
  end;

var
  ListSelectorForm: TListSelectorForm;

implementation

{$R *.dfm}

procedure TListSelectorForm.btnOkAllClick(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to ListBox1.Items.Count - 1 do
    ListBox1.Checked[i] := true;
end;

procedure TListSelectorForm.CheckBox1Click(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to ListBox1.Items.Count - 1 do
    ListBox1.Checked[i] := CheckBox1.Checked;
end;

procedure TListSelectorForm.ListBox1ClickCheck(Sender: TObject);
var
  i : integer;
begin
  btnok.Enabled := FOkWithNoneSelected;
  for i := 0 to ListBox1.Items.Count - 1 do
    if ListBox1.Checked[i] then
      btnok.Enabled := true;
end;

procedure TListSelectorForm.SetVerb(const Value: String);
begin
  FVerb := Value;
  btnOk.Caption := FVerb;
  btnOkAll.Caption := 'All + '+FVerb;
  cbDontAsk.Caption := FVerb+' automatically without asking';
end;

end.
