unit ftk_frame_server;

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
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Dialogs,
  fsl_utilities, fsl_json,
  fui_lcl_managers,
  fhir_common, fhir_factory, fhir_client,
  ftk_utilities, ftk_constants, ftk_context,
  ftk_worker_base;

type
  TFrame = TBaseWorkerFrame;
  TServerWorkerFrame = class;

  { TPatientSearchManager }

  TPatientSearchManager = class (TListManager<TFHIRPatientW>)
  private
    FView : TServerWorkerFrame;
  public
    function canSort : boolean; override;
    function doubleClickEdit : boolean; override;
    function allowedOperations(item : TFHIRPatientW) : TNodeOperationSet; override;
    function loadList : boolean; override;

    procedure buildMenu; override;
    function getCellText(item : TFHIRPatientW; col : integer) : String; override;
    function getSummaryText(item : TFHIRPatientW) : String; override;
    function compareItem(left, right : TFHIRPatientW; col : integer) : integer; override;
    function filterItem(item : TFHIRPatientW; s : String) : boolean; override;

    function executeItem(item : TFHIRPatientW; mode : String) : boolean; override;
  end;

  { TServerWorkerFrame }

  TServerWorkerFrame = class(TFrame)
    btnSearch: TButton;
    cbxSearchType: TComboBox;
    cbxGender: TComboBox;
    cbAll: TCheckBox;
    edtId: TEdit;
    edtName: TEdit;
    edtDob: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lvPatients: TListView;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlSearchOutcome: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    procedure btnSearchClick(Sender: TObject);
    procedure cbxGenderChange(Sender: TObject);
    procedure edtDobChange(Sender: TObject);
    procedure edtIdChange(Sender: TObject);
    procedure edtNameChange(Sender: TObject);
    procedure FrameClick(Sender: TObject);
  private
    FServer: TFHIRServerEntry;

    FSearch : TFHIRBundleW;
    FFactory : TFHIRFactory;

    FPatientManager : TPatientSearchManager;

    procedure SetServer(AValue: TFHIRServerEntry);
    procedure doPatientSearch;
  protected
    procedure save(json : TJsonObject); override;
  public
    destructor Destroy; override;
    procedure init(json : TJsonObject); override;
    procedure saveStatus; override;

    property server : TFHIRServerEntry read FServer write SetServer;
  end;

implementation

{$R *.lfm}

{ TPatientSearchManager }

function TPatientSearchManager.canSort: boolean;
begin
  Result := true;
end;

function TPatientSearchManager.doubleClickEdit: boolean;
begin
  Result := false;
end;

function TPatientSearchManager.allowedOperations(item: TFHIRPatientW): TNodeOperationSet;
begin
  result := [opExecute];
end;

function TPatientSearchManager.loadList: boolean;
var
  be : TFhirBundleEntryW;
  pat : TFhirPatientW;
begin
  result := true;
  for be in FView.FSearch.entries.forEnum do
  begin
    if be.resource.fhirType = 'Patient' then
    begin
      pat := FView.FFactory.wrapPatient(be.resource.link);
      Data.Add(pat);
//      pat.Tags['url'] := be.url;
    end;
  end;
end;

procedure TPatientSearchManager.buildMenu;
begin
  inherited buildMenu;
  registerMenuEntry('Open', ICON_OPEN, copExecute);
end;

function TPatientSearchManager.getCellText(item: TFHIRPatientW; col: integer): String;
begin
  case col of
    0: result := item.id;
    1: result := item.active;
    2: result := item.nameSummary;
    3: result := item.gender;
    4: result := item.dob;
    5: result := item.identifierSummary;
    6: result := item.contactSummary;
  else
    result := '??';
  end;
end;

function TPatientSearchManager.getSummaryText(item: TFHIRPatientW): String;
begin
  Result := item.describe;
end;

function TPatientSearchManager.compareItem(left, right: TFHIRPatientW; col: integer): integer;
begin
  case col of
    0: result := String.Compare(left.id, right.id);
    1: result := String.Compare(left.active, right.active);
    2: result := String.Compare(left.nameSummary, right.nameSummary);
    3: result := String.Compare(left.gender, right.gender);
    4: result := String.Compare(left.dob, right.dob);
    5: result := String.Compare(left.identifierSummary, right.identifierSummary);
    6: result := String.Compare(left.contactSummary, right.contactSummary);
  else
    result := 0;
  end;
end;

function TPatientSearchManager.filterItem(item: TFHIRPatientW; s: String): boolean;
begin
  Result := item.nameSummary.contains(s);
end;

function TPatientSearchManager.executeItem(item: TFHIRPatientW; mode: String): boolean;
begin
  Result := true;
  FView.Context.OpenResource(URLPath([FView.FServer.URL, 'Patient', item.id]));
end;

{ TServerWorkerFrame }

destructor TServerWorkerFrame.Destroy;
begin
  FPatientManager.Free;
  FServer.Free;
  FFactory.Free;
  FSearch.Free;
  inherited;
end;

procedure TServerWorkerFrame.saveStatus;
begin
  inherited;
  FPatientManager.saveStatus;
end;

procedure TServerWorkerFrame.init(json: TJsonObject);
begin
  FPatientManager := TPatientSearchManager.create;
  FPatientManager.Images := FContext.images;
  FPatientManager.Settings := FContext.Settings;
  FPatientManager.List := lvPatients;
  FPatientManager.FView := self;

  FFactory := FContext.factory(FServer.version);

  edtName.Text := json.str['patient-name'];
  cbxGender.Text := json.str['patient-gender'];
  edtDoB.Text := json.str['patient-dob'];
  edtId.Text := json.str['patient-id'];
end;

procedure TServerWorkerFrame.doPatientSearch;
var
  params : TStringList;
  t : UInt64;
  s : String;
begin
  cursor := crHourGlass;
  try
    if FServer.client = nil then
      Context.OnConnectToServer(self, FServer);
    FSearch.free;
    FSearch := nil;
    params := TStringList.create;
    try
      if edtName.Text <> '' then
        params.add('name='+edtName.text);
      if cbxGender.Text <> '' then
        params.add('gender='+cbxGender.text);
      if edtDoB.Text <> '' then
        params.add('birthdate='+edtDoB.text);
      if edtId.Text <> '' then
        params.add('identifier='+edtDoB.text);
      t := GetTickCount64;
      FSearch := FFactory.wrapBundle(FServer.client.searchV('Patient', cbAll.checked, params));
      t := GetTickCount64 - t;
      FPatientManager.doLoad;
      s := '  Search: '+inttostr(FSearch.count('Patient'))+' matches';
      if FSearch.Total > 0 then
        s := s + ' of '+inttostr(FSearch.total);
      if FSearch.timestamp.notNull then
        s := s + ' as of '+FSearch.timestamp.toXML+' (server)'
      else
        s := s + ' as of '+TFslDateTime.makeLocal.toXML+' (local)';
      s := s + ' ('+inttostr(t)+'ms)';
      pnlSearchOutcome.caption := s;
    finally
      params.free;
    end;
  finally
    cursor := crDefault;
  end;
end;

procedure TServerWorkerFrame.FrameClick(Sender: TObject);
begin
end;

procedure TServerWorkerFrame.btnSearchClick(Sender: TObject);
begin
  case cbxSearchType.itemIndex of
    0: doPatientSearch;
  else
    abort;
  end;
end;

procedure TServerWorkerFrame.cbxGenderChange(Sender: TObject);
begin
  changed;
end;

procedure TServerWorkerFrame.edtDobChange(Sender: TObject);
begin
  changed;
end;

procedure TServerWorkerFrame.edtIdChange(Sender: TObject);
begin
  changed;
end;

procedure TServerWorkerFrame.edtNameChange(Sender: TObject);
begin
  changed;
end;

procedure TServerWorkerFrame.SetServer(AValue: TFHIRServerEntry);
begin
  FServer.Free;
  FServer := AValue;
end;

procedure TServerWorkerFrame.save(json: TJsonObject);
begin
  json.str['patient-name'] := edtName.Text;
  json.str['patient-gender'] := cbxGender.Text;
  json.str['patient-dob'] := edtDoB.Text;
  json.str['patient-id'] := edtId.Text;
end;

end.

