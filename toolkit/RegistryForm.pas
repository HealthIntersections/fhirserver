unit RegistryForm;

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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Generics.Collections,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.TabControl, FMX.ListBox, FMX.Layouts, FMX.DateTimeCtrls,
  FMX.Edit, System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox, FMX.Platform,
  IdComponent,
  fsl_base, fsl_utilities,
  fhir_objects, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Resources.Base, FHIR.Version.Client, FHIR.Version.Utilities, fhir_xhtml,
  BaseFrame, CapabilityStatementEditor, FMX.WebBrowser,
  FMX.Memo, FMX.Memo.Types;

type
  TFrame = TBaseFrame; // re-aliasing the Frame to work around a designer bug

  TRegistryFrame = class (TFrame)
    Panel2: TPanel;
    Panel3: TPanel;
    lblOutcome: TLabel;
    btnFetchMore: TButton;
    Panel1: TPanel;
    Panel4: TPanel;
    pnlSearch: TPanel;
    Label3: TLabel;
    btnConfSearch: TButton;
    cbConfUseLastUpdated: TCheckBox;
    edtConfJurisdiction: TComboBox;
    edtConfText: TEdit;
    edtConfUpdated: TDateEdit;
    edtConfUrl: TEdit;
    Label11: TLabel;
    Label8: TLabel;
    Label1: TLabel;
    cbxType: TComboBox;
    Label2: TLabel;
    cbxProfile: TComboBox;
    gridConfMatches: TGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    StringColumn7: TStringColumn;
    StringColumn8: TStringColumn;
    Splitter1: TSplitter;
    memSource: TMemo;
    pnlMessages: TPanel;
    lblMessages: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure btnConfSearchClick(Sender: TObject);
    procedure gridConfMatchesGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure gridConfMatchesCellDblClick(const Column: TColumn; const Row: Integer);
    procedure btnFetchMoreClick(Sender: TObject);
    procedure cbxTypeChange(Sender: TObject);
    procedure cbConfUseLastUpdatedClick(Sender: TObject);
    procedure gridConfMatchesSelectCell(Sender: TObject; const ACol, ARow: Integer; var CanSelect: Boolean);
    procedure lblOutcomeClick(Sender: TObject);
    procedure lblMessagesClick(Sender: TObject);
  private
    FClient: TFHIRClient;
    FCSTab : TTabItem;
    FCsForm : TCapabilityStatementEditorFrame;
    FPatBundle, FConfBundle : TFhirBundle;
    FConfMatches : TFslList<TFHIRResource>;
    function client : TFhirClient;
    procedure DoWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
  public
    destructor Destroy; override;

    procedure load; override;
  end;

implementation

{$R *.fmx}

uses
  ResourceEditingSupport;

{ TRegistryFrame }

procedure TRegistryFrame.btnCloseClick(Sender: TObject);
begin
  try
    Settings.storeValue('Registry-search', 'url', edtConfUrl.Text);
    Settings.storeValue('Registry-search', 'text', edtConfText.Text);
    Settings.storeValue('Registry-search', 'jurisdiction', edtConfJurisdiction.ItemIndex);
    Settings.storeValue('Registry-search', 'updated', edtConfUpdated.Text);
    Settings.storeValue('Registry-search', 'updated-opt', cbConfUseLastUpdated.IsChecked);
    Settings.storeValue('Registry-search', 'type', cbxType.ItemIndex);
    Settings.storeValue('Registry-search', 'profile', cbxProfile.ItemIndex);
    Settings.save;
  except
  end;
  Close;
end;

procedure TRegistryFrame.btnConfSearchClick(Sender: TObject);
var
  t : TFHIRResourceType;
  op : TFhirOperationOutcome;
begin
  pnlMessages.Height := 0;
  work('Fetch Resources', true,
    procedure (context : pointer)
    var
      be : TFhirBundleEntry;
      params : TStringList;
      start : TDateTime;
    begin
      FConfMatches.Clear;
      gridConfMatches.RowCount := FConfMatches.Count;
      FConfBundle.Free;
      FConfBundle := nil;

      params := TStringList.create;
      try
        case cbxType.ItemIndex of
          0 : {Profiles}
            begin
            t := frtStructureDefinition;
            end;
          1 : {Value Sets}
            begin
            t := frtValueSet;
            end;
          2 : {Code Systems}
            begin
            t := frtCodeSystem;
            end;
          3 : {Capability Statements}
            begin
            t := frtCapabilityStatement;
            end;
          4 : {Search Parameters}
            begin
            t := frtSearchParameter;
            end;
          5 : {Operation Definitions}
            begin
            t := frtOperationDefinition;
            end;
          6 : {All Conformance Resources}
            begin
            t := frtNull;
            params.addPair('_type', 'CapabilityStatement,StructureDefinition,ImplementationGuide,SearchParameter,MessageDefinition,OperationDefinition,CompartmentDefinition,StructureMap,GraphDefinition,CodeSystem,ValueSet,ConceptMap,ExpansionProfile,NamingSystem');
            end;
        else{All Resources}
          begin
          t := frtNull;
          end;
        end;
        params.addPair('_summary', 'true');

        if cbxProfile.Enabled  then
          params.addPair('type', cbxProfile.items[cbxProfile.itemIndex]);
        if edtConfUrl.Text <> '' then
          params.addPair('url', edtConfUrl.Text);
        if edtConfText.Text <> '' then
          params.addPair('_text', edtConfText.Text);
        if cbConfUseLastUpdated.IsChecked then
          params.addPair('_lastUpdated', edtConfUpdated.Text);
        if edtConfJurisdiction.ItemIndex > 0 then
          params.addPair('jurisdiction', getJurisdictionSearch(edtConfJurisdiction.ItemIndex));

        start := now;
        FConfBundle := Client.search(t, false, params);
        for be in FConfBundle.entryList do
          if ((be.search = nil) or (be.search.mode = SearchEntryModeMatch)) and (be.resource <> nil) then
            if (be.resource is TFhirOperationOutcome) then
            begin
              op := TFhirOperationOutcome(be.resource);
              pnlMessages.Height := 26;
              lblMessages.Text := op.asExceptionMessage;
            end
            else
              FConfMatches.Add(be.resource.Link);
        gridConfMatches.RowCount := FConfMatches.Count;
        lblOutcome.Text := 'Fetched '+inttostr(FConfMatches.Count)+' of '+FConfBundle.total+' resources in '+describePeriod(now - start)+'. search String = '+FMXescape(FClient.lastURL);
        btnFetchMore.Visible := FConfBundle.Links['next'] <> '';
      finally
        params.Free;
      end;
    end);
end;

procedure TRegistryFrame.btnFetchMoreClick(Sender: TObject);
begin
  work('Fetch More', true,
    procedure (context : pointer)
    var
      be : TFhirBundleEntry;
      start : TDateTime;
      l : TFhirBundleLink;
      i : integer;
      url : String;
    begin
      btnFetchMore.Visible := false;
      url := FConfBundle.Links['next'];
      FConfBundle.Free;
      FConfBundle := nil;
      start := now;
      FConfBundle := Client.searchAgain(url);
      i := 0;
      for be in FConfBundle.entryList do
        if (be.search.mode = SearchEntryModeMatch) and (be.resource <> nil) then
        begin
          FConfMatches.Add(be.resource.Link);
          inc(i);
        end;
      gridConfMatches.RowCount := FConfMatches.Count;
      lblOutcome.Text := 'Fetched '+inttostr(i)+' of '+FConfBundle.total+' resources in '+describePeriod(now - start);
      btnFetchMore.Visible := FConfBundle.Links['next'] <> '';
    end);
end;

procedure TRegistryFrame.cbConfUseLastUpdatedClick(Sender: TObject);
begin
  edtConfUpdated.Enabled := cbConfUseLastUpdated.IsChecked;
end;

procedure TRegistryFrame.cbxTypeChange(Sender: TObject);
begin
  cbxProfile.Enabled := cbxType.ItemIndex = 0;
end;

function TRegistryFrame.client: TFhirClient;
begin
  if FClient = nil then
    FClient := TFhirClients.makeHTTP(nil, 'http://registry-api.fhir.org/open', false);
  result := FClient;
end;

destructor TRegistryFrame.Destroy;
begin
  FClient.free;
  FConfBundle.Free;
  FPatBundle.Free;
  FConfMatches.Free;

  inherited;
end;

procedure TRegistryFrame.DoWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  Application.ProcessMessages;
  if assigned(OnStopped) and OnStopped(nil) then
    abort;
end;

procedure TRegistryFrame.gridConfMatchesCellDblClick(const Column: TColumn; const Row: Integer);
var
  res : TFhirResource;
begin
  if canOpenResourceType(FConfMatches[Row].ResourceType) then
  begin
  res := Client.readResource(FConfMatches[Row].ResourceType, FConfMatches[Row].id);
    try
      OnOpenResource(self, client, client.format, res);
    finally
      res.Free;
    end;
  end;
end;

procedure TRegistryFrame.gridConfMatchesGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  res : TFhirMetadataResource;
begin
  if not (FConfMatches[aRow] is TFhirMetadataResource) then
  begin
    case ACol of
      0: Value := FConfMatches[aRow].fhirType;
      1: Value := FConfMatches[aRow].id;
    end;
  end
  else
  begin
    res := FConfMatches[aRow] as TFhirMetadataResource;
    case ACol of
      0: Value := res.fhirType;
      1: Value := res.id;
      2: Value := res.url;
      3: Value := res.name;
      4: Value := res.version;
      5: Value := CODES_TFhirPublicationStatusEnum[res.status];
      6: Value := res.date.toXML;
      7: Value := readJurisdiction(res);
    end;
  end;
end;

function top200(s : String) : String;
var
  i, t : integer;
begin
  i := 1;
  t := 0;
  while (i < length(s)) do
  begin
    if s[i] = #10 then
      inc(t);
    if t = 200 then
      exit(s.Substring(0, i+1));
    inc(i);
  end;
  result := s;
end;

procedure TRegistryFrame.gridConfMatchesSelectCell(Sender: TObject; const ACol, ARow: Integer; var CanSelect: Boolean);
var
  res : TFHIRResource;
begin
  if aRow = -1 then
    memSource.Text := ''
  else
  begin
    res := FConfMatches[aRow];
    memSource.Text := top200(resourceToString(res, ffXml, OutputStylePretty));
  end;
end;

procedure TRegistryFrame.lblMessagesClick(Sender: TObject);
//var
//  Svc: IFMXClipboardService;
begin
//  TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc);
//  svc.SetClipboard(lblMessages.Text);
end;

procedure TRegistryFrame.lblOutcomeClick(Sender: TObject);
var
  Svc: IFMXClipboardService;
begin
  TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc);
  svc.SetClipboard(lblOutcome.Text);
end;

procedure TRegistryFrame.load;
begin
  edtConfUrl.Text := Settings.getValue('Registry-search', 'url', '');
  edtConfText.Text := Settings.getValue('Registry-search', 'text', '');
  edtConfJurisdiction.ItemIndex := Settings.getValue('Registry-search', 'jurisdiction', 0);
  edtConfUpdated.Text := Settings.getValue('Registry-search', 'updated', '');
  cbConfUseLastUpdated.IsChecked := Settings.getValue('Registry-search', 'updated-opt', false);
  cbxType.ItemIndex := Settings.getValue('Registry-search', 'type', 0);
  cbxProfile.ItemIndex := Settings.getValue('Registry-search', 'profile', 0);

  btnFetchMore.Visible := false;
  FConfMatches := TFslList<TFHIRResource>.create;
end;


end.
