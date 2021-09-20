unit frm_clip_chooser;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Clipbrd, LclType,
  fsl_base, fsl_utilities, fsl_stream;

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

