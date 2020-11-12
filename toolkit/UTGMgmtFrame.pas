unit UTGMgmtFrame;

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
  fsl_utilities,
  fhir_objects, fhir_factory, fhir_common,
  FHIR.Version.Resources, FHIR.Version.Parser, FHIR.Version.Factory,
  fhir_diff, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView;

type
  TFrame = TBaseFrame; // re-aliasing the Frame to work around a designer bug

  TUTGManagementFrame = class(TFrame)
    Panel1: TPanel;
    Panel2: TPanel;
    ListView1: TListView;
    Panel3: TPanel;
    btnNewProposal: TButton;
    Label1: TLabel;
    Panel4: TPanel;
    btnFetchProposal: TButton;
    Label2: TLabel;
    Panel5: TPanel;
    btnCommitProposal: TButton;
    Label3: TLabel;
    Panel6: TPanel;
    btnDeleteProposal: TButton;
    Label4: TLabel;
    Panel7: TPanel;
    btnMakeActive: TButton;
    Label5: TLabel;
    Panel8: TPanel;
    btnPublish: TButton;
    Label6: TLabel;
    ProgressBar1: TProgressBar;
    Panel9: TPanel;
    btnOpen: TButton;
    Label7: TLabel;
    Panel10: TPanel;
    btnOpenFile: TButton;
    Label8: TLabel;
    procedure btnNewProposalClick(Sender: TObject);
    procedure btnFetchProposalClick(Sender: TObject);
    procedure btnCommitProposalClick(Sender: TObject);
    procedure btnDeleteProposalClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnMakeActiveClick(Sender: TObject);
    procedure btnPublishClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
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

{ TUTGManagementFrame }

procedure TUTGManagementFrame.btnCommitProposalClick(Sender: TObject);
begin
  // confirm this action
  // push to repo using branch name
  // check outcome
end;

procedure TUTGManagementFrame.btnDeleteProposalClick(Sender: TObject);
begin
  // confirm this action
  // delete local copy
end;

procedure TUTGManagementFrame.btnFetchProposalClick(Sender: TObject);
var
  ok : boolean;
begin
  // list all the names in the branch
  // new := does already exist locally
  // if not new, check that's what's meant
  // if new, create the local directory
  try
    ok := false;
    // if new, clone the master UTG content into the local directory
    // else if not new, pull the master UTG content into the local directory
    ok := true;
  except
    // if new, delete the local directory
  end;
  if (ok) then
    // if new, record the local proposal
end;

procedure TUTGManagementFrame.btnMakeActiveClick(Sender: TObject);
begin
  // if another UTG are is active, confirm to replace it
  // start server pointing at selected UTG area
end;

procedure TUTGManagementFrame.btnNewProposalClick(Sender: TObject);
var
  ok : boolean;
begin
  // list all the names in the branch
  // get a name that isn't already used
  // create the local directory
  try
    ok := false;
    // clone the master UTG content into the local directory
    ok := true;
  except
    // delete the local directory
  end;
  if (ok) then
    // record the local proposal
end;

procedure TUTGManagementFrame.btnOpenClick(Sender: TObject);
begin
  // if output exists,
  //   open output/index.html
  // else
  //   ask if user wants to create it
end;

procedure TUTGManagementFrame.btnOpenFileClick(Sender: TObject);
begin
//  if runOpenDialog then
//    openFile (mark as UTG)
end;

procedure TUTGManagementFrame.btnPublishClick(Sender: TObject);
begin
  // check that it's not already running
  // if it is,
  //   abort the run
  //   change button to "Stopping" & disable it
  // it it isn't
  //   start it running
  //   change button to "Stop"
end;

function TUTGManagementFrame.canSave: boolean;
begin
end;

function TUTGManagementFrame.canSaveAs: boolean;
begin
end;

destructor TUTGManagementFrame.Destroy;
begin
end;

function TUTGManagementFrame.isDirty: boolean;
begin
end;

procedure TUTGManagementFrame.load;
begin
end;

function TUTGManagementFrame.nameForSaveDialog: String;
begin
end;

function TUTGManagementFrame.parseResource(memo: TMemo; desc: String): TFhirResource;
begin
end;

function TUTGManagementFrame.save: boolean;
begin
end;

procedure TUTGManagementFrame.saveSettings;
begin
end;

procedure TUTGManagementFrame.SettingsChanged;
begin
end;

procedure TUTGManagementFrame.writeResource(src: TFhirResource; memo: TMemo);
begin
end;

end.
