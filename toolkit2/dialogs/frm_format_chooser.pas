unit frm_format_chooser;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  fui_lcl_utilities,
  ftk_context;

type

  { TFormatChooserForm }

  TFormatChooserForm = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    lbChoices: TListBox;
    mSource: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure lbChoicesClick(Sender: TObject);
    procedure lbChoicesDblClick(Sender: TObject);
  private

  public

  end;

var
  FormatChooserForm: TFormatChooserForm;

function ChooseFormat(owner : TForm; fmts : TSourceEditorKindSet; source : String) : TSourceEditorKind;

implementation

{$R *.lfm}

function ChooseFormat(owner : TForm; fmts : TSourceEditorKindSet; source : String) : TSourceEditorKind;
var
  FormatChooserForm: TFormatChooserForm;
  a : TSourceEditorKind;
begin
  FormatChooserForm := TFormatChooserForm.create(owner);
  try
    FormatChooserForm.mSource.Text := source;
    FormatChooserForm.lbChoices.items.clear;
    for a in TSourceEditorKindSet do
      if a in fmts then
        FormatChooserForm.lbChoices.items.AddObject(NAMES_TSourceEditorKind[a], TObject(a));
    if FormatChooserForm.ShowModal = mrOk then
      result := TSourceEditorKind(integer(FormatChooserForm.lbChoices.items.Objects[FormatChooserForm.lbChoices.ItemIndex]))
    else
      result := sekNull;
  finally
    FormatChooserForm.free;
  end;
end;

{ TFormatChooserForm }

procedure TFormatChooserForm.lbChoicesClick(Sender: TObject);
begin
  btnOk.enabled := lbChoices.ItemIndex <> -1;
end;

procedure TFormatChooserForm.FormCreate(Sender: TObject);
begin
  setForOs(btnOk, btnCancel);
end;

procedure TFormatChooserForm.lbChoicesDblClick(Sender: TObject);
begin
  if lbChoices.ItemIndex <> -1 then
    ModalResult := mrOk;
end;

end.

