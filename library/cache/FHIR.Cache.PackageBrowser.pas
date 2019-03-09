unit FHIR.Cache.PackageBrowser;

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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  {$IFDEF NPPUNICODE}FHIR.Npp.Form, {$ENDIF}
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  VirtualTrees,
  FHIR.Support.Base, FHIR.Support.Json, FHIR.Web.Fetcher, FHIR.Support.Utilities,
  FHIR.Cache.PackageManager;

type
  TOnLoadUrlEvent = procedure (sender : TObject; url : String) of object;

  TPackageFinderForm = class({$IFDEF NPPUNICODE} TNppForm {$ELSE} TForm {$ENDIF})
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    edtFilter: TEdit;
    Label2: TLabel;
    ComboBox1: TComboBox;
    btnClose: TButton;
    btnInstall: TButton;
    Panel3: TPanel;
    grid: TVirtualStringTree;
    procedure edtFilterChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
    procedure gridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure gridRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure gridAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    FLoaded : boolean;
    FList : TFslList<TPackageDefinition>;
    FFiltered : TFslList<TPackageDefinition>;
    FOnLoad : TOnLoadUrlEvent;
    FIndex : integer;
    procedure loadPackages;
    procedure applyFilter;
    function matchesFilter(pck : TPackageDefinition) : boolean;
  public
    property OnLoadUrl : TOnLoadUrlEvent read FOnLoad write FOnLoad;
  end;

var
  PackageFinderForm: TPackageFinderForm;

implementation

{$R *.dfm}


procedure TPackageFinderForm.btnInstallClick(Sender: TObject);
begin
  FOnLoad(self, FFiltered[FIndex].Url);
end;

procedure TPackageFinderForm.edtFilterChange(Sender: TObject);
begin
  applyFilter;
  grid.RootNodeCount := 0;
  grid.RootNodeCount := FFiltered.Count;
//  gridCellClick(nil, grid.Row);
end;

procedure TPackageFinderForm.FormActivate(Sender: TObject);
begin
  if not FLoaded then
    loadPackages;
end;

procedure TPackageFinderForm.FormCreate(Sender: TObject);
begin
  FList := TFslList<TPackageDefinition>.create;
  FFiltered := TFslList<TPackageDefinition>.create;
end;

procedure TPackageFinderForm.FormDestroy(Sender: TObject);
begin
  FFiltered.Free;
  FList.Free;
end;

procedure TPackageFinderForm.gridAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  btnInstall.Enabled := true;
  FIndex := Node.Index;
end;

procedure TPackageFinderForm.gridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  pck : TPackageDefinition;
begin
  pck := FFiltered[Node.Index];
  case Column of
    0: CellText := pck.Id;
    1: CellText := pck.Version;
    2: CellText := pck.Description;
    3: CellText := pck.Canonical;
    4: CellText := pck.FHIRVersion;
    5: CellText := FormatDateTime('c', pck.Date);
  end;
end;

procedure TPackageFinderForm.gridRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  btnInstall.Enabled := false;
end;

procedure TPackageFinderForm.loadPackages;
begin
  FList.clear;
  TPackageDefinition.AddStandardPackages(FList);
  TPackageDefinition.addPackagesFromBuild(FList);
  TPackageDefinition.AddCustomPackages(FList);
  FLoaded := true;
  applyFilter;
  grid.RootNodeCount := 0;
  grid.RootNodeCount := FFiltered.Count;
//  gridCellClick(nil, grid.Row);
end;

function TPackageFinderForm.matchesFilter(pck: TPackageDefinition): boolean;
begin
  result :=
    pck.id.Contains(edtFilter.Text) or
    pck.Version.Contains(edtFilter.Text) or
    pck.Description.Contains(edtFilter.Text) or
    pck.FHIRVersion.Contains(edtFilter.Text) or
    pck.Canonical.Contains(edtFilter.Text) or
    formatDateTime('c', pck.Date).Contains(edtFilter.Text);
end;

procedure TPackageFinderForm.applyFilter;
var
  pck : TPackageDefinition;
begin
  FFiltered.Clear;
  if edtFilter.Text = '' then
    FFiltered.AddAll(FList)
  else
  begin
    for pck in FList do
      if matchesFilter(pck) then
        FFiltered.Add(pck.link);
  end;
end;

end.
