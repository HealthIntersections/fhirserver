unit PackageBrowser;

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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.Edit, FMX.ListBox, FMX.StdCtrls, FMX.Grid, FMX.ScrollBox,
  FMX.Controls.Presentation,
  fsl_base, fsl_utilities, fsl_json, fsl_fetcher,
  fsl_npm_cache;

type
  TOnLoadUrlEvent = procedure (sender : TObject; url : String; pbar : TProgressBar) of object;

  TPackageFinderForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Button1: TButton;
    btnInstall: TButton;
    Label1: TLabel;
    cbxServer: TComboBox;
    Label2: TLabel;
    edtFilter: TEdit;
    grid: TGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    StringColumn5: TStringColumn;
    DateColumn1: TDateColumn;
    ProgressBar1: TProgressBar;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure gridGetValue(Sender: TObject; const ACol, ARow: Integer;
      var Value: TValue);
    procedure edtFilterChange(Sender: TObject);
    procedure gridCellClick(const Column: TColumn; const Row: Integer);
    procedure btnInstallClick(Sender: TObject);
  private
    FChanged: boolean;
    FLoaded : boolean;
    FList : TFslList<TPackageDefinition>;
    FFiltered : TFslList<TPackageDefinition>;
    FOnLoad : TOnLoadUrlEvent;
    procedure loadServer;
    procedure applyFilter;
    function matchesFilter(pck : TPackageDefinition) : boolean;
  public
    property OnLoadUrl : TOnLoadUrlEvent read FOnLoad write FOnLoad;
  end;

var
  PackageFinderForm: TPackageFinderForm;

implementation

{$R *.fmx}

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

procedure TPackageFinderForm.btnInstallClick(Sender: TObject);
begin
  FOnLoad(self, FFiltered[grid.Row].Url, ProgressBar1);
end;

procedure TPackageFinderForm.edtFilterChange(Sender: TObject);
begin
  applyFilter;
  grid.RowCount := 0;
  grid.RowCount := FFiltered.Count;
  gridCellClick(nil, grid.Row);
end;

procedure TPackageFinderForm.FormActivate(Sender: TObject);
begin
  if not FLoaded then
    loadServer;
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

procedure TPackageFinderForm.gridCellClick(const Column: TColumn; const Row: Integer);
begin
  btnInstall.Enabled := Row <> -1;
end;

procedure TPackageFinderForm.gridGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  pck : TPackageDefinition;
begin
  if ARow >= FFiltered.count then
    value := ''
  else
  begin
    pck := FFiltered[ARow];
    case aCol of
      0: value := pck.Id;
      1: value := pck.Version;
      2: value := pck.Description;
      3: value := pck.Canonical;
      4: value := pck.FHIRVersion;
      5: value := pck.Date;
    end;
  end;
end;

procedure TPackageFinderForm.loadServer;
var
  j : TJsonObject;
  a : TJsonArray;
  i : TJsonNode;
  p : TPackageDefinition;
begin
  FList.clear;
//  TPackageDefinition.AddStandardPackages(Flist);
//  TPackageDefinition.AddCustomPackages(Flist);
  a := TInternetFetcher.fetchJsonArray('https://build.fhir.org/ig/qas.json');
  try
    for i in a do
    begin
      j := i as TJsonObject;
      if (j.str['package-id'].Contains('.')) then
      begin
        p := TPackageDefinition.Create;
        try
          p.Id := j.str['package-id'];
          p.Version := j.str['ig-ver'];
          p.Canonical := j.str['url'];
          p.Date := TFslDateTime.fromFormat('DDD, dd mmm, yyyy hh:nn:ss Z', j.str['date']).DateTime;
          p.Description := j.str['name'];
          p.FHIRVersion := j.str['version'];
          p.Url := 'https://build.fhir.org/ig/'+j.str['repo'];
          FList.Add(p.Link);
        finally
          p.Free;
        end;
      end;
    end;
  finally
    a.Free;
  end;
  FLoaded := true;
  applyFilter;
  grid.RowCount := 0;
  grid.RowCount := FFiltered.Count;
  gridCellClick(nil, grid.Row);
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

end.
