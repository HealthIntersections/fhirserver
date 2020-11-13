unit DiffEngineFrame;

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
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  BaseFrame, FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.ComboEdit, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, FMX.Platform,
  fsl_utilities, fsl_http,
  fhir_objects, fhir_factory, fhir_common,
  FHIR.Version.Resources, FHIR.Version.Parser, FHIR.Version.Factory,
  fhir_diff;

type
  TFrame = TBaseFrame; // re-aliasing the Frame to work around a designer bug

  TDiffEngineEngineFrame = class(TFrame)
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Splitter2: TSplitter;
    mSource: TMemo;
    mDest: TMemo;
    mDiff: TMemo;
    Panel5: TPanel;
    Label1: TLabel;
    Panel6: TPanel;
    Label2: TLabel;
    Panel7: TPanel;
    Label3: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    dlgOpen: TOpenDialog;
    Button4: TButton;
    dlgSave: TSaveDialog;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    FLoading, FDirty : boolean;
    FSource, FDest, FDiff : String;
    Ffactory : TFHIRFactory;
    procedure saveSettings;
    function parseResource(memo : TMemo; desc : String) : TFhirResource;
    procedure writeResource(src : TFhirResource; memo : TMemo);
  public
    destructor Destroy; override;

    procedure SettingsChanged; override;
    procedure load; override;
    function canSave : boolean; override;
    function canSaveAs : boolean; override;
    function isDirty : boolean; override;
    function save : boolean; override;
    function nameForSaveDialog : String; override;
 end;

implementation

{$R *.fmx}

{ TDiffEngineEngineFrame }

procedure TDiffEngineEngineFrame.Button1Click(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    FSource := dlgOpen.FileName;
    mSource.Lines.LoadFromFile(FSource);
    FDirty := true;
  end;
end;

procedure TDiffEngineEngineFrame.Button2Click(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    FDest := dlgOpen.FileName;
    mDest.Lines.LoadFromFile(FDest);
    FDirty := true;
  end;
end;

procedure TDiffEngineEngineFrame.Button3Click(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    FDiff := dlgOpen.FileName;
    mDiff.Lines.SaveToFile(FDiff);
    FDirty := true;
  end;
end;

procedure TDiffEngineEngineFrame.Button4Click(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    FDiff := dlgOpen.FileName;
    mDiff.Lines.LoadFromFile(FDiff);
    FDirty := true;
  end;
end;

procedure TDiffEngineEngineFrame.Button5Click(Sender: TObject);
var
  src, res : TFHIRResource;
  diff : TFhirParametersW;
  engine : TDifferenceEngine;
begin
  src := parseResource(mSource, 'Source');
  try
    diff := FFactory.wrapParams(parseResource(mDiff, 'Diff'));
    try
      engine := TDifferenceEngine.Create(nil, FFactory.link);
      try
        res := engine.applyDifference(src, diff) as TFhirResource;
        try
          writeResource(res, mDest);
        finally
          res.Free;
        end;
      finally
        engine.Free;
      end;
    finally
      diff.free;
    end;
  finally
    src.Free;
  end;
end;

procedure TDiffEngineEngineFrame.Button6Click(Sender: TObject);
var
  src, res : TFHIRResource;
  diff : TFhirParametersW;
  engine : TDifferenceEngine;
  h : string;
begin
  src := parseResource(mSource, 'Source');
  try
    res := parseResource(mDest, 'Dest');
    try
      engine := TDifferenceEngine.Create(nil, Ffactory.link);
      try
        diff := engine.generateDifference(src, res, h);
        try
          writeResource(diff.Resource as TFhirResource, mDiff);
        finally
          diff.Free;
        end;
      finally
        engine.Free;
      end;
    finally
      res.free;
    end;
  finally
    src.Free;
  end;
end;

function TDiffEngineEngineFrame.canSave: boolean;
begin
  result := true;
end;

function TDiffEngineEngineFrame.canSaveAs: boolean;
begin
  result := false;
end;

destructor TDiffEngineEngineFrame.Destroy;
begin
  inherited;
end;

function TDiffEngineEngineFrame.isDirty: boolean;
begin
  result := FDirty;
end;

procedure TDiffEngineEngineFrame.load;
begin
  Ffactory := TFHIRFactoryX.Create;
  FLoading := true;
  try
    FSource := Settings.getValue('DiffEngine', 'source', '');
    if FSource <> '' then
      mSource.Lines.LoadFromFile(FSource);
    FDest := Settings.getValue('DiffEngine', 'dest', '');
    if FDest <> '' then
      mDest.Lines.LoadFromFile(FDest);
    FDiff := Settings.getValue('DiffEngine', 'diff', '');
    if FDiff <> '' then
      mDiff.Lines.LoadFromFile(FDiff);
  finally
    FLoading := false;
  end;
  FDirty := false;
end;

function TDiffEngineEngineFrame.nameForSaveDialog: String;
begin
  result := 'DiffEngine Configuration';
end;

function TDiffEngineEngineFrame.parseResource(memo: TMemo; desc: String): TFhirResource;
var
  x : TFHIRXmlParser;
begin
  x := TFHIRXmlParser.Create(nil, THTTPLanguages.create('en'));
  try
    result := x.parseResource(memo.Text) as TFhirResource;
  finally
    x.Free;
  end;
end;

function TDiffEngineEngineFrame.save: boolean;
begin
  saveSettings;
  result := true;
end;

procedure TDiffEngineEngineFrame.saveSettings;
var
  i : integer;
begin
  if FLoading then
    exit;
  Settings.storeValue('DiffEngine', 'source', FSource);
  Settings.storeValue('DiffEngine', 'dest', FDest);
  Settings.storeValue('DiffEngine', 'diff', FDiff);
  Settings.Save;
  FDirty := false;
end;

procedure TDiffEngineEngineFrame.SettingsChanged;
begin
end;

procedure TDiffEngineEngineFrame.writeResource(src: TFhirResource; memo: TMemo);
var
  x : TFHIRXmlComposer;
begin
  x := TFHIRXmlComposer.Create(nil, OutputStylePretty, THTTPLanguages.create('en'));
  try
    memo.Text := x.Compose(src);
  finally
    x.Free;
  end;
end;

end.
