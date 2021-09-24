unit frm_project_editor;

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
  Buttons, fsl_base, ftk_project_tree;

type

  { TProjectSettingsForm }

  TProjectSettingsForm = class(TForm)
    btnOk: TButton;
    Button2: TButton;
    edtName: TEdit;
    edtFolder: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    fd: TSelectDirectoryDialog;
    SpeedButton1: TSpeedButton;
    procedure btnOkClick(Sender: TObject);
    procedure edtNameChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FProject: TFHIRProjectNode;
    FProjects: TFslList<TFHIRProjectNode>;
    procedure SetProject(AValue: TFHIRProjectNode);
    procedure SetProjects(AValue: TFslList<TFHIRProjectNode>);
    function NameIsUsed(name : String) : boolean;
  public
    property Project : TFHIRProjectNode read FProject write SetProject;
    property Projects : TFslList<TFHIRProjectNode> read FProjects write SetProjects;
  end;

var
  ProjectSettingsForm: TProjectSettingsForm;

implementation

{$R *.lfm}

{ TProjectSettingsForm }

procedure TProjectSettingsForm.FormDestroy(Sender: TObject);
begin
  FProject.Free;
  FProjects.Free;
end;

procedure TProjectSettingsForm.SpeedButton1Click(Sender: TObject);
begin
  fd.InitialDir := edtFolder.text;
  if fd.execute then
    edtFolder.text := fd.FileName;
  if edtName.text = '' then
    edtName.text := ExtractFileName(edtFolder.Text);
end;

procedure TProjectSettingsForm.edtNameChange(Sender: TObject);
begin
  btnOK.enabled := not NameIsUsed(edtName.text);
end;

procedure TProjectSettingsForm.btnOkClick(Sender: TObject);
begin
  project.name := edtName.text;
  project.address := edtFolder.text;
end;

procedure TProjectSettingsForm.SetProject(AValue: TFHIRProjectNode);
begin
  FProject.Free;
  FProject := AValue;
end;

procedure TProjectSettingsForm.SetProjects(AValue: TFslList<TFHIRProjectNode>);
begin
  FProjects.Free;
  FProjects := AValue;
end;

function TProjectSettingsForm.NameIsUsed(name: String): boolean;
var
  t : TFHIRProjectNode;
begin
  result := false;
  for t in FProjects do
    if (t.name = name) and (t.id <> project.id) then
      exit(true);
end;

end.

