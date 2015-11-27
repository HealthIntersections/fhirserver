unit PathDialogForms;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, NppForms, Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls, nppplugin, AdvGenerics;

type
  TPathOutcomeDialogMode = (pomError, pomNoMatch, pomMatch);

  TPathDialogForm = class(TNppForm)
    Panel2: TPanel;
    Label1: TLabel;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PathDialogForm: TPathDialogForm;

procedure pathOutcomeDialog(owner : TNppPlugin; path, rtype : String; types : TAdvStringSet; mode : TPathOutcomeDialogMode; outcome : String);

implementation

{$R *.dfm}

uses
  FHIRPluginSettings;

function summary(types : TArray<String>) : String;
var
  s : String;
  b : TStringBuilder;
  f : boolean;
begin
  if Length(types) = 0 then
    exit('?? unknown');

  f := true;
  b := TStringBuilder.Create;
  try
    for s in types do
    begin
      if f then
        f := false
      else
        b.Append(', ');
      b.Append(s);
    end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;

procedure pathOutcomeDialog(owner : TNppPlugin; path, rtype : String; types : TAdvStringSet; mode : TPathOutcomeDialogMode; outcome : String);
var
  t : string;
begin
  if not Settings.NoPathSummary or (mode = pomError) then
  begin
    PathDialogForm := TPathDialogForm.create(owner);
    try
      PathDialogForm.CheckBox1.Checked := Settings.NoPathSummary;
      if types = nil then
        t := ''
      else
        t := types.ToString;
      case mode of
        pomError :
          PathDialogForm.Memo1.Text := 'Path: '+path+#13#10#13#10+'When evaluated against a '+rtype+', this path may return the following types: '+t+#13#10#13#10+'Error Message: '+outcome+#13#10;
        pomNoMatch :
          PathDialogForm.Memo1.Text := 'Path: '+path+#13#10#13#10+'When evaluated against a '+rtype+', this path may return the following types: '+t+#13#10#13#10+'Outcome: '+outcome+#13#10;
        pomMatch :
          PathDialogForm.Memo1.Text := 'Path: '+path+#13#10#13#10+'When evaluated against a '+rtype+', this path may return the following types: '+t+#13#10#13#10+'Outcome: '+outcome+#13#10#13#10+'Matching Items are shown with a green squiggly: '+#13#10;
      end;
      PathDialogForm.ShowModal;
      Settings.NoPathSummary := PathDialogForm.CheckBox1.Checked;
    finally
      FreeAndNil(PathDialogForm);
    end;
  end;
end;

end.
