unit CDEOptions;

{
Copyright (c) 2014+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  Windows, Messages, SysUtils, Classes, Graphics, IniFiles,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  wp_printing_win,
  FHIR.WP.Control, FHIR.WP.Settings;

type
  TCDEOptionsForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    labelx: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    cbLowLight: TCheckBox;
    cbxPrinters: TComboBox;
    cbSpellingErrors: TCheckBox;
    cbUppercase: TCheckBox;
    cbTouchMode: TCheckBox;
    cbSmartMode: TCheckBox;
    Shape1: TShape;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbLowLightClick(Sender: TObject);
  private
    { Private declarations }
    FWP: TWordProcessor;
    FPrinters : TFslPrinterList;

  public
    { Public declarations }
    destructor Destroy; Override;

    Property WP : TWordProcessor read FWP write FWP;

    class procedure Initialise(WP : TWordProcessor);
  end;

var
  CDEOptionsForm: TCDEOptionsForm;

implementation

{$R *.dfm}

uses
  fsl_utilities;


{ TForm1 }

class procedure TCDEOptionsForm.Initialise(WP: TWordProcessor);
var
  config : TIniFile;
  s : String;
  manager : TFslPrinterManager;
  printers : TFslPrinterList;
  i : integer;
begin
  config := TIniFile.Create('cde.ini');
  try
    WP.Settings.Hold;
    try
      WP.Settings.LowLight := config.ReadBool('WP', 'LowLight', false);
      WP.Settings.SpellingErrors := config.ReadBool('WP', 'LowLight', true);
      WP.Settings.SpellCheckUppercaseWords := config.ReadBool('WP', 'SpellCheckUppercaseWords', false);
      WP.Settings.SmartParagraphs := config.ReadBool('WP', 'SmartParagraphs', true);
      WP.Settings.CapitaliseFirstInSentence := config.ReadBool('WP', 'SmartParagraphs', true);
      if config.ReadBool('WP', 'TouchMode', false) then
        WP.Settings.TouchMode := wptmAlways
      else
        WP.Settings.TouchMode := wptmNever;
    finally
      WP.Settings.Release;
    end;

    s := config.ReadString('WP', 'Paginator', '');
    WP.Printer := nil;
    manager := TFslPrinterManager.Create;
    printers := TFslPrinterList.Create;
    try
      manager.Open;
      manager.CompilePrinterList(printers);
      for i := 0 to printers.Count - 1 do
      begin
        try
          printers[i].open;
          if printers[i].Settings.Definition.Name = s then
            WP.Printer := printers[i].Link;
        except
          // nothing, for now
        end;
      end;
    finally
      manager.free;
      printers.free;
    end;
  finally
    config.Free;
  end;
end;

procedure TCDEOptionsForm.Button1Click(Sender: TObject);
var
  config : TIniFile;
begin
  config := TIniFile.Create('cde.ini');
  try
    WP.Settings.Hold;
    try
      config.WriteBool('WP', 'LowLight', cbLowLight.Checked);
      WP.Settings.LowLight := cbLowLight.Checked;

      config.WriteBool('WP', 'SpellingErrors', cbSpellingErrors.Checked);
      WP.Settings.SpellingErrors := cbSpellingErrors.Checked;

      config.WriteBool('WP', 'SpellCheckUppercaseWords', cbUppercase.Checked);
      WP.Settings.SpellCheckUppercaseWords := cbUppercase.Checked;

      config.WriteBool('WP', 'SmartParagraphs', cbSmartMode.Checked);
      WP.Settings.SmartParagraphs := cbSmartMode.Checked;
      WP.Settings.CapitaliseFirstInSentence := cbSmartMode.Checked;

      config.WriteBool('WP', 'TouchMode', cbTouchMode.Checked);
      if cbTouchMode.Checked then
        WP.Settings.TouchMode := wptmAlways
      else
        WP.Settings.TouchMode := wptmNever;
    finally
      WP.Settings.Release;
    end;
    if cbxPrinters.ItemIndex = 0 then
    begin
      config.WriteString('WP', 'Paginator', '');
      WP.Printer := nil;
    end
    else
    begin
      config.WriteString('WP', 'Paginator', FPrinters[cbxPrinters.ItemIndex-1].Settings.Definition.Name);
      WP.Printer := FPrinters[cbxPrinters.ItemIndex-1].Link;
    end;
  finally
    config.Free;
  end;
  ModalResult := mrOk;
end;

procedure TCDEOptionsForm.cbLowLightClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    TCheckBox(sender).Caption := 'Yes'
  else
    TCheckBox(sender).Caption := 'No';
end;

destructor TCDEOptionsForm.Destroy;
begin
  FPrinters.Free;
  inherited;
end;

procedure TCDEOptionsForm.FormCreate(Sender: TObject);
var
  manager : TFslPrinterManager;
  i : integer;
begin
  FPrinters := TFslPrinterList.Create;
  manager := TFslPrinterManager.Create;
  try
    manager.Open;
    manager.CompilePrinterList(FPrinters);
  finally
    manager.free;
  end;
  cbxPrinters.Items.Clear;
  cbxPrinters.Items.Add('');
  for i := 0 to FPrinters.Count - 1 do
  begin
    FPrinters[i].Open;
    cbxPrinters.Items.Add(FPrinters[i].Settings.Definition.Name);
  end;
end;

procedure TCDEOptionsForm.FormShow(Sender: TObject);
var
  i : integer;
begin
  cbLowLight.Checked := WP.Settings.LowLight;
  cbSpellingErrors.Checked := WP.Settings.SpellingErrors;
  cbUppercase.Checked := WP.Settings.SpellCheckUppercaseWords;
  cbTouchMode.Checked := WP.Settings.TouchMode = wptmAlways;
  cbSmartMode.Checked := WP.Settings.SmartParagraphs;
  cbxPrinters.ItemIndex := 0;
  if WP.Printer <> nil then
    for i := 0 to FPrinters.Count - 1 do
      if FPrinters[i].Settings.Definition.Name = WP.Printer.Settings.Definition.Name then
        cbxPrinters.ItemIndex := i + 1;

end;

end.
