unit ListSelector;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TListSelectorForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    CheckBox1: TCheckBox;
    procedure ListBox1ChangeCheck(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    FOkWithNoneSelected: boolean;
    { Private declarations }
  public
    { Public declarations }
    property okWithNoneSelected : boolean read FOkWithNoneSelected write FOkWithNoneSelected;
  end;

var
  ListSelectorForm: TListSelectorForm;

implementation

{$R *.fmx}

procedure TListSelectorForm.CheckBox1Change(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to ListBox1.Items.Count - 1 do
    ListBox1.ListItems[i].IsChecked := CheckBox1.IsChecked;
end;

procedure TListSelectorForm.FormShow(Sender: TObject);
begin
  CheckBox1.Visible := ListBox1.ShowCheckboxes;
end;

procedure TListSelectorForm.ListBox1ChangeCheck(Sender: TObject);
var
  i : integer;
begin
  if not ListBox1.ShowCheckboxes then
    Button1.Enabled := FOkWithNoneSelected or (ListBox1.ItemIndex > -1)
  else
  begin
    Button1.Enabled := FOkWithNoneSelected;
    for i := 0 to ListBox1.Items.Count - 1 do
      if ListBox1.ListItems[i].IsChecked then
        Button1.Enabled := true;
  end;
end;

procedure TListSelectorForm.ListBox1DblClick(Sender: TObject);
begin
  if not ListBox1.ShowCheckboxes then
   ModalResult := mrOk;
end;

end.
