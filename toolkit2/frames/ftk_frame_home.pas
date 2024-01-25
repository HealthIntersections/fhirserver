unit ftk_frame_home;

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
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Dialogs, LclIntf,
  MarkdownProcessor,
  fsl_utilities, fsl_json,
  fui_lcl_managers,
  fhir_common, fhir_factory, fhir_client,
  ftk_utilities, ftk_constants, ftk_context, HtmlView, ftk_store_temp,
  ftk_worker_base, HtmlGlobals;

type
  TFrame = TBaseWorkerFrame;
  THomePageFrame = class;

  { THomePageFrame }

  THomePageFrame = class(TFrame)
    HtmlViewer1: THtmlViewer;
    lbMRU: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure FrameResize(Sender: TObject);
    procedure HtmlViewer1HotSpotClick(Sender: TObject; const SRC: ThtString;
      var Handled: Boolean);
    procedure lbMRUDblClick(Sender: TObject);
    procedure lbMRUShowHint(Sender: TObject; HintInfo: PHintInfo);
  private
    FTempStore: TFHIRToolkitTemporaryStorage;
    FMru : TStringList;
    procedure SetTempStore(AValue: TFHIRToolkitTemporaryStorage);

  protected
    procedure save(json : TJsonObject); override;
  public
    destructor Destroy; override;
    procedure init(json : TJsonObject); override;
    procedure saveStatus; override;
    procedure getFocus; override;

    property TempStore : TFHIRToolkitTemporaryStorage read FTempStore write SetTempStore;
  end;

implementation

{$R *.lfm}

uses
  frm_main;

const
  HOME_PAGE =
    '## Welcome to the FHIR Toolkit'+#13#10+#13#10+
    'The FHIR Toolkit allows you to edit and validate all sorts of files that are useful when working with FHIR content.'+#13#10+
    'In addition, you can connect to servers and explore them.'+#13#10+
    ''+#13#10+
    '## User Help'+#13#10+
    'The best place to task for help is on the [tooling stream on chat.fhir.org](https://chat.fhir.org/#narrow/stream/179239-tooling).'+#13#10+
    'Note that you might be ''encouraged'' to contribute to the documentation '#$F609'. '+#13#10+
    ''+#13#10;

{ THomePageFrame }

destructor THomePageFrame.Destroy;
begin
  FMru.free;
  FTempStore.free;
  inherited;
end;

procedure THomePageFrame.saveStatus;
begin
  inherited;
end;

procedure THomePageFrame.getFocus;
var
  s : String;
begin
  FMRU.clear;
  TempStore.getMRUListRaw(FMRU);
  lbMRU.items.clear;
  for s in FMru do
    lbMRU.Items.add(s.Substring(s.IndexOf('|')+1));
end;

procedure THomePageFrame.init(json: TJsonObject);
var
  proc : TMarkdownProcessor;
begin
  FMru := TStringList.Create;
  proc := TMarkdownProcessor.createDialect(mdCommonMark); // or flavor of your choice
  try
    proc.allowUnsafe := false;
    HtmlViewer1.LoadFromString(proc.process(HOME_PAGE));
  finally
    proc.free;
  end;
end;

procedure THomePageFrame.lbMRUDblClick(Sender: TObject);
var
  s : string;
begin
  s := FMru[lbMRU.ItemIndex];
  s := s.Substring(0, s.IndexOf('|'));
  MainToolkitForm.openFile(s);
end;

procedure THomePageFrame.FrameResize(Sender: TObject);
begin
  panel1.width := width div 2;
end;

procedure THomePageFrame.HtmlViewer1HotSpotClick(Sender: TObject; const SRC: ThtString; var Handled: Boolean);
begin
  Handled := true;
  OpenURL(src);
end;

procedure THomePageFrame.lbMRUShowHint(Sender: TObject; HintInfo: PHintInfo);
begin

end;

procedure THomePageFrame.SetTempStore(AValue: TFHIRToolkitTemporaryStorage);
begin
  FTempStore.free;
  FTempStore := AValue;
end;

procedure THomePageFrame.save(json: TJsonObject);
begin
  json.str['home-page'] := 'true';
end;

end.

