unit PackageEditorFrame;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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
  FMX.TabControl, FMX.Layouts, FMX.TreeView, FMX.Controls.Presentation, System.ImageList,
  FMX.ImgList, FMX.ScrollBox, FMX.Memo, FMX.DateTimeCtrls, FMX.ListBox, FMX.Edit,
  System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.Menus,
  BaseFrame,
  fsl_base, fsl_utilities, FHIR.Ui.Fmx,
  fhir_objects, FHIR.Version.Constants, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Utilities, fhir_indexing, FHIR.Version.IndexInfo, FHIR.Version.Factory, FHIR.Version.Common,
  fsl_npm_cache, fsl_npm;

type
  TFrame = TBaseFrame; // re-aliasing the Frame to work around a designer bug

  TPackageEditorFrame = class(TFrame)
    Panel1: TPanel;
    Panel2: TPanel;
    grid: TStringGrid;
    Label1: TLabel;
    lblVersion: TLabel;
    lblType: TLabel;
    lblLicense: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblUrl: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblAuthor: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Panel4: TPanel;
    edtPackageId: TEdit;
    edtFHIRVersions: TEdit;
    edtCanonical: TEdit;
    edtHomePage: TEdit;
    edtDescription: TEdit;
    edtTitle: TEdit;
    edtVersion: TEdit;
    edtType: TEdit;
    edtLicense: TEdit;
    edtUrl: TEdit;
    edtAuthor: TEdit;
    edtDependencies: TEdit;
    Label2: TLabel;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    StringColumn7: TStringColumn;
    StringColumn8: TStringColumn;
    StringColumn9: TStringColumn;
    Label3: TLabel;
    edtFilter: TEdit;
    procedure Panel1Resize(Sender: TObject);
    procedure edtFilterChangeTracking(Sender: TObject);
  private
    FSource: String;
    FPackage : TNpmPackage;
    procedure filter;
  public
    destructor Destroy; override;
    property Source : String read FSource write FSource;
    procedure load; override;
  end;

implementation

{$R *.fmx}

destructor TPackageEditorFrame.Destroy;
begin
  FPackage.Free;
  inherited;
end;

procedure TPackageEditorFrame.edtFilterChangeTracking(Sender: TObject);
begin
  filter;
end;

procedure TPackageEditorFrame.filter;
var
  fl : TFslList<TNpmPackageResource>;
  fi : TNpmPackageResource;
  i : integer;
begin
  fl := TFslList<TNpmPackageResource>.create;
  try
    for fi in FPackage.Folders['package'].Resources do
      if (edtFilter.Text = '') or fi.matches(edtFilter.Text) then
        fl.Add(fi.Link);
    grid.RowCount := 0;
    grid.RowCount := fl.Count;
    for i := 0 to fl.Count - 1 do
    begin
      fi := fl[i];
      grid.Cells[0, i] := fi.Name;
      grid.Cells[1, i] := fi.ResourceType;
      grid.Cells[2, i] := fi.Id;
      grid.Cells[3, i] := inttostr(fi.size);
      grid.Cells[4, i] := fi.URL;
      grid.Cells[5, i] := fi.version;
      grid.Cells[6, i] := fi.kind;
      grid.Cells[7, i] := fi.typeV;
    end;
  finally
    fl.Free;
  end;
end;

procedure TPackageEditorFrame.load;
begin
  FPackage := TNpmPackage.fromSource(FSource);

  edtPackageId.Text := FPackage.name;
  edtFHIRVersions.Text := FPackage.fhirVersionList;
  edtCanonical.Text := FPackage.canonical;
  edtHomePage.Text := FPackage.homePage;
  edtTitle.Text := FPackage.title;
  edtVersion.Text := FPackage.version;
  edtType.Text := NAMES_TFHIRPackageKind[FPackage.kind];
  edtLicense.Text := FPackage.license;
  edtUrl.Text := FPackage.url;
  edtAuthor.Text := FPackage.author;
  edtDescription.Text := FPackage.description;
  edtDependencies.Text := FPackage.dependencySummary;

  filter;
end;

procedure TPackageEditorFrame.Panel1Resize(Sender: TObject);
begin
  lblVersion.Position.X := Panel1.Width / 2;
  lblType.Position.X := lblVersion.Position.X;
  lblLicense.Position.X := lblVersion.Position.X;
  lblUrl.Position.X := lblVersion.Position.X;
  lblAuthor.Position.X := lblVersion.Position.X;

  edtPackageId.Width := lblVersion.Position.X - edtPackageId.Position.X - 8;
  edtFHIRVersions.Width := edtPackageId.Width;
  edtCanonical.Width := edtPackageId.Width;
  edtHomePage.Width := edtPackageId.Width;
  edtTitle.Width := edtPackageId.Width;

  edtVersion.Width := edtPackageId.Width;
  edtVersion.Position.X := Panel1.Width - edtPackageId.Width - 8;
  edtType.Width := edtPackageId.Width;
  edtType.Position.X := edtVersion.Position.X;
  edtLicense.Width := edtPackageId.Width;
  edtLicense.Position.X := edtVersion.Position.X;
  edtUrl.Width := edtPackageId.Width;
  edtUrl.Position.X := edtVersion.Position.X;
  edtAuthor.Width := edtPackageId.Width;
  edtAuthor.Position.X := edtVersion.Position.X;

  edtDescription.Width := Panel1.Width - edtPackageId.Position.X - 8;
  edtDependencies.Width := Panel1.Width - edtPackageId.Position.X - 8;
end;

end.
