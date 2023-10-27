unit OsxPopupmenuWorkaround;

{
Copyright (c) 2018+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.Menus;

type
  TPopupMenuWorkaroundForm = class(TForm)
    ListBox1: TListBox;
    procedure FormShow(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1KeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  private
    FMenu: TPopupMenu;
    FSelected: integer;
  public
    property menu : TPopupMenu read FMenu write FMenu;
    property Selected : integer read FSelected write FSelected;
  end;

var
  PopupMenuWorkaroundForm: TPopupMenuWorkaroundForm;

function runPopupAlternative(owner : TComponent; menu : TPopupMenu; left, top : integer) : integer;

implementation

{$R *.fmx}

function runPopupAlternative(owner : TComponent; menu : TPopupMenu; left, top : integer) : integer;
begin
  PopupMenuWorkaroundForm := TPopupMenuWorkaroundForm.Create(owner);
  try
    PopupMenuWorkaroundForm.Left := left;
    PopupMenuWorkaroundForm.Top := top;
    PopupMenuWorkaroundForm.menu := menu;
    PopupMenuWorkaroundForm.ShowModal;
    result := PopupMenuWorkaroundForm.Selected;
  finally
    PopupMenuWorkaroundForm.free;
  end;
end;

procedure TPopupMenuWorkaroundForm.FormShow(Sender: TObject);
var
  i : integer;
begin
  ListBox1.Items.Clear;
  for i := 0 to menu.ItemsCount - 1 do
    listBox1.Items.Add(menu.Items[i].Text);
end;

procedure TPopupMenuWorkaroundForm.ListBox1Click(Sender: TObject);
begin
  FSelected := ListBox1.ItemIndex;
  ModalResult := mrOk;
end;

procedure TPopupMenuWorkaroundForm.ListBox1KeyDown(Sender: TObject;
  var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkEscape then
     ModalResult := mrClose;
end;

end.
