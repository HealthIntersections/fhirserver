unit frm_clip_chooser;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Clipbrd, LclType,
  fsl_base, fsl_utilities, fsl_stream,
  fui_lcl_utilities;

type
  { TClipboardChooserForm }

  TClipboardChooserForm = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    lbFormats: TListBox;
    mSource: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Splitter1: TSplitter;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbFormatsClick(Sender: TObject);
    procedure lbFormatsDblClick(Sender: TObject);
  private
    FFormats : TFslStringDictionary;
    function LoadFmt(n : String) : String;
  public

  end;

var
  ClipboardChooserForm: TClipboardChooserForm;

function chooseClipboardContent(owner : TForm) : String;

implementation

{$R *.lfm}

function chooseClipboardContent(owner : TForm) : String;
begin
  ClipboardChooserForm := TClipboardChooserForm.create(owner);
  try
    if ClipboardChooserForm.ShowModal = mrOk then
      result := ClipboardChooserForm.mSource.text
    else
      result := '';
  finally
    ClipboardChooserForm.Free;
  end;
end;

{ TClipboardChooserForm }

procedure TClipboardChooserForm.FormShow(Sender: TObject);
begin

end;

procedure TClipboardChooserForm.lbFormatsClick(Sender: TObject);
var
  n : String;
begin
  n := lbFormats.Items[lbFormats.ItemIndex];
  mSource.Text := FFormats.Values[n];
  btnOk.enabled := mSource.Text <> '';
end;

procedure TClipboardChooserForm.lbFormatsDblClick(Sender: TObject);
begin
  if btnOk.enabled then
    ModalResult := mrOK;
end;

function TClipboardChooserForm.LoadFmt(n: String): String;
var
  Stream: TMemoryStream;
  Fmt : TClipboardFormat;
  b : TBytes;
  i : integer;
begin
  if n = '' then
    exit('');
  Stream := TMemoryStream.Create;
  try
    if Clipboard.HasFormatName(n) then
    begin
      Fmt := ClipBoard.FindFormatID(n);
      ClipBoard.GetFormat(Fmt, Stream);
      b := StreamToBytes(stream);
      try
        result := TEncoding.UTF8.GetString(b);
      except
        try
          result := TEncoding.ASCII.GetString(b);
          for i := 1 to length(result) do
          if result[i] < ' ' then
            result[i] := char($25A1);
        except
          result := '';
        end;
      end;
    end
    else
      result := '';
  finally
    Stream.Free;
  end;
end;

procedure TClipboardChooserForm.FormCreate(Sender: TObject);
begin
  setForOs(btnOk, btnCancel);
  FFormats := TFslStringDictionary.create;
end;

procedure TClipboardChooserForm.FormActivate(Sender: TObject);
var
  fmts : TStringList;
  n, v : String;
begin
  FFormats.Clear;
  fmts := TStringList.create;
  try
    Clipboard.SupportedFormats(fmts);
    for n in fmts do
    begin
      v := loadFmt(n);
      if (v <> '') then
        FFormats.addorSetValue(n, v);
    end;
  finally
    fmts.free;
  end;
  lbFormats.Items.Clear;
  for n in FFormats.Keys do
    lbFormats.Items.add(n);
  if lbFormats.Items.Count = 0 then
  begin
    lbFormats.ItemIndex := 0;
    lbFormatsClick(self);
  end
  else
    btnOk.enabled := false;
end;

procedure TClipboardChooserForm.FormDestroy(Sender: TObject);
begin
  FFormats.Free;
end;

end.

