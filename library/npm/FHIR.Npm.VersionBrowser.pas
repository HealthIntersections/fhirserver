unit FHIR.Npm.VersionBrowser;

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
  fsl_base, fsl_json, fsl_fetcher, fsl_utilities,
  fsl_npm_cache, fsl_npm_client, Vcl.ComCtrls, FHIR.Ui.WorkerTask;

type
  TPackageVersionChooserForm = class({$IFDEF NPPUNICODE} TNppForm {$ELSE} TForm {$ENDIF})
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    btnInstall: TButton;
    Panel3: TPanel;
    grid: TVirtualStringTree;
    btnClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure gridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure gridRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure gridAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure gridHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure gridDblClick(Sender: TObject);
  private
    FList : TFslList<TFHIRPackageInfo>;
    FIndex : integer;
    procedure sortPackages;
  public
    procedure LoadPackages(list : TFslList<TFHIRPackageInfo>);
    property Index : integer read FIndex;
  end;

var
  PackageVersionChooserForm: TPackageVersionChooserForm;

implementation

{$R *.dfm}


procedure TPackageVersionChooserForm.FormCreate(Sender: TObject);
begin
  FList := TFslList<TFHIRPackageInfo>.create;
end;

procedure TPackageVersionChooserForm.FormDestroy(Sender: TObject);
begin
  FList.Free;
end;

procedure TPackageVersionChooserForm.gridAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  btnInstall.Enabled := true;
  FIndex := Node.Index;
end;

procedure TPackageVersionChooserForm.gridDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TPackageVersionChooserForm.gridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  pck : TFHIRPackageInfo;
begin
  pck := Flist[Node.Index];
  case Column of
    0: CellText := pck.Id;
    1: CellText := pck.Version;
    2: CellText := pck.Description;
    3: CellText := pck.canonical;
    4: CellText := pck.FHIRVersion;
    5: CellText := pck.presentDate;
  end;
end;

procedure TPackageVersionChooserForm.gridHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.Column = grid.Header.SortColumn then
    if grid.Header.SortDirection = sdDescending then
      grid.Header.SortDirection := sdAscending
    else
      grid.Header.SortDirection := sdDescending
  else
  begin
    grid.Header.SortColumn := HitInfo.Column;
    grid.Header.SortDirection := sdAscending;
  end;
  sortPackages;
end;

procedure TPackageVersionChooserForm.gridRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  btnInstall.Enabled := false;
end;

procedure TPackageVersionChooserForm.LoadPackages(list : TFslList<TFHIRPackageInfo>);
begin
  Flist.AddAll(list);
  grid.RootNodeCount := 0;
  grid.RootNodeCount := Flist.Count;
end;

procedure TPackageVersionChooserForm.sortPackages;
begin
  FList.SortF(function (const l, r: TFHIRPackageInfo): Integer
    begin
      case grid.Header.SortColumn of
        0: result := CompareStr(l.Id, r.Id);
        1: result := CompareStr(l.Version, r.Version);
        2: result := CompareStr(l.Description, r.Description);
        3: result := CompareStr(l.canonical, r.canonical);
        4: result := CompareStr(l.FHIRVersion, r.FHIRVersion);
        5: if (l.Date > r.Date) then
             result := 1
           else if (l.Date < r.Date) then
             result := -1
           else
             result := 0;
      else
        result := 0;
      end;
      if grid.Header.SortDirection = sdDescending then
        result := 0 - result;
    end);
  grid.RootNodeCount := 0;
  grid.RootNodeCount := FList.Count;
end;

end.
