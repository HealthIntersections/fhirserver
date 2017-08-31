unit AddRestResourceDialog;

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
  TAddRestResourceForm = class(TForm)
    cbRead: TCheckBox;
    cbVRead: TCheckBox;
    cbSearch: TCheckBox;
    cbCreate: TCheckBox;
    cbUpdate: TCheckBox;
    cbPatch: TCheckBox;
    cbDelete: TCheckBox;
    cbHistoryInstance: TCheckBox;
    cbHistoryType: TCheckBox;
    cbUpdateCreate: TCheckBox;
    cbCondCreate: TCheckBox;
    cbCondUpdate: TCheckBox;
    cbCondDelete: TCheckBox;
    cbxVersioning: TComboBox;
    cbxReadCondition: TComboBox;
    Label22: TLabel;
    cbRefLocal: TCheckBox;
    cbRefEnforced: TCheckBox;
    cbRefResolve: TCheckBox;
    cbRefLogical: TCheckBox;
    Label23: TLabel;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    Label21: TLabel;
    Label24: TLabel;
    cbRefLiteral: TCheckBox;
    cbStandardSearch: TCheckBox;
    Panel3: TPanel;
    ListBox1: TListBox;
    procedure ListBox1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AddRestResourceForm: TAddRestResourceForm;

implementation

{$R *.fmx}

procedure TAddRestResourceForm.FormActivate(Sender: TObject);
begin
  ListBox1Click(nil);
end;

procedure TAddRestResourceForm.ListBox1Click(Sender: TObject);
var
  i : integer;
begin
  button1.Enabled := false;
  for i := 0 to ListBox1.Items.Count - 1 do
    if ListBox1.ListItems[i].IsChecked then
      Button1.Enabled := true;
end;

end.
