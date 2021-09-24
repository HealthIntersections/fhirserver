unit frm_file_format;

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
  ComCtrls;

type

  { TFileFormatChooser }

  TFileFormatChooser = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    ListBox1: TListBox;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private

  public
    procedure setFHIRResource;
  end;

var
  FileFormatChooser: TFileFormatChooser;

implementation

{$R *.lfm}

{ TFileFormatChooser }

procedure TFileFormatChooser.ListBox1DblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFileFormatChooser.setFHIRResource;
begin
  ListBox1.ItemIndex := 0;
  ListBox1Click(self);
end;

procedure TFileFormatChooser.ListBox1Click(Sender: TObject);
begin
  btnOk.enabled := ListBox1.ItemIndex > -1;
  if ListBox1.ItemIndex = 0 then
  begin
    btnOk.caption := '&Next >>';
    btnOk.modalresult := mrNone;
  end
  else
  begin
    btnOk.caption := 'OK';
    btnOk.modalresult := mrOK;
  end;
end;

procedure TFileFormatChooser.btnOkClick(Sender: TObject);
begin
  if ListBox1.ItemIndex = 0 then
  begin
    PageControl1.ActivePage := TabSheet2;
    btnOk.caption := 'OK';
    btnOk.modalresult := mrOK;
    btnCancel.caption := 'Back';
    btnCancel.ModalResult := mrNone;
  end;
end;

procedure TFileFormatChooser.btnCancelClick(Sender: TObject);
begin
  PageControl1.ActivePage := TabSheet1;
  btnCancel.caption := 'Cancel';
  btnCancel.ModalResult := mrCancel;
  ListBox1Click(self);
end;

end.

