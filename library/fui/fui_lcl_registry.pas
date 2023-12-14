unit fui_lcl_registry;

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


{$I fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, IniFiles, DateUtils,
  fsl_base, fsl_utilities,
  fui_lcl_managers, fsl_npm_client;

type
  { TPackageRegistryManager }
  TPackageRegistryManager = class (TListManager<TFHIRPackageInfo>)
  private
    FClient : TFHIRPackageClient;
    FVersion : String;
  public
    Constructor Create; override;
    destructor Destroy; override;

    function canSort : boolean; override;
    function allowedOperations(item : TFHIRPackageInfo) : TNodeOperationSet; override;
    function loadList : boolean; override;

    function getCellText(item : TFHIRPackageInfo; col : integer) : String; override;
    function compareItem(left, right : TFHIRPackageInfo; col : integer) : integer; override;
    function filterItem(item : TFHIRPackageInfo; s : String) : boolean; override;
  end;

  { TPackageRegistryForm }

  TPackageRegistryForm = class(TForm)
    btnCancel: TButton;
    btnClose: TButton;
    btnInstall: TButton;
    cbxServer: TComboBox;
    cbxVersion: TComboBox;
    edtFilter: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblDownload: TLabel;
    ListView1: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pbDownload: TProgressBar;
    procedure cbxServerChange(Sender: TObject);
    procedure cbxVersionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FIni: TIniFile;

    FManager : TPackageRegistryManager;
    FStop : boolean;
    procedure packageWork(sender : TObject; pct : integer; done : boolean; msg : String);
    procedure SetIni(AValue: TIniFile);
  public
    property Ini : TIniFile read FIni write SetIni;
  end;

var
  PackageRegistryForm: TPackageRegistryForm;

implementation

{$R *.lfm}

{ TPackageRegistryManager }

constructor TPackageRegistryManager.Create;
begin
  inherited Create;
  FClient := TFHIRPackageClient.Create('http://packages.fhir.org');
end;

destructor TPackageRegistryManager.Destroy;
begin
  FClient.free;
  inherited Destroy;
end;

function TPackageRegistryManager.canSort: boolean;
begin
  Result := true;
end;

function TPackageRegistryManager.allowedOperations(item: TFHIRPackageInfo): TNodeOperationSet;
begin
  result := [opExecute];
end;

function TPackageRegistryManager.loadList: boolean;
var
  list : TFslList<TFHIRPackageInfo>;
begin
  try
    list := FClient.search('', '', FVersion, false);
    try
      Data.addAll(list);
    finally
      list.free;
    end;
    result := true;
  except
    on e : Exception do
    begin
      result := false;
      ShowMessage('Error loading registry: '+e.message);
    end;
  end;
end;

function TPackageRegistryManager.getCellText(item: TFHIRPackageInfo; col: integer): String;
begin
  case col of
    0: result := item.id;
    1: result := item.version;
    2: result := item.fhirVersion;
    3: if item.date = 0 then result := '' else result := DescribePeriod(now - item.date);
    4: if item.size = 0 then result := '' else result := DescribeBytes(item.size);
    5: result := item.canonical;
    6: result := item.description;
  end;
end;

function TPackageRegistryManager.compareItem(left, right: TFHIRPackageInfo; col: integer): integer;
begin
  case col of
     -1: if left.id = right.id then
           result := CompareStr(left.version, right.version)
         else
           result := CompareStr(left.id, right.id);
     0: result := CompareStr(left.id, right.id);
     1: result := CompareStr(left.version, right.version);
     2: result := CompareStr(left.fhirVersion, right.fhirVersion);
     3: result := compareDate(left.date, right.date);
     4: result := left.size - right.size;
     5: result := CompareStr(left.canonical, right.canonical);
     6: result := CompareStr(left.description, right.description);
   else
     result := 0;
   end;
end;

function TPackageRegistryManager.filterItem(item: TFHIRPackageInfo; s: String): boolean;
begin
  Result := item.id.ToLower.Contains(s) or item.version.ToLower.Contains(s) or item.canonical.ToLower.Contains(s) or item.description.ToLower.Contains(s);
end;

{ TPackageRegistryForm }

procedure TPackageRegistryForm.FormCreate(Sender: TObject);
begin
  FManager := TPackageRegistryManager.Create;
  FManager.Settings := ini;
  FManager.List := ListView1;
  FManager.Filter := edtFilter;
  FManager.registerControl(btnInstall, copExecute, 'install');
end;

procedure TPackageRegistryForm.cbxServerChange(Sender: TObject);
begin
  case cbxServer.ItemIndex of
    0: FManager.FClient := TFHIRPackageClient.Create('http://packages.fhir.org');
    1: FManager.FClient := TFHIRPackageClient.Create('http://packages2.fhir.org/packages');
    2: FManager.FClient := TFHIRPackageClient.Create('http://packages.fhir.org');
  end;
  FManager.doLoad;
end;

procedure TPackageRegistryForm.cbxVersionChange(Sender: TObject);
begin
  case cbxVersion.ItemIndex of
    0: FManager.FVersion := '';
    1: FManager.FVersion := 'R4';
    2: FManager.FVersion := 'STU3';
    3: FManager.FVersion := 'DSTU2';
  end;
  FManager.doLoad;
end;

procedure TPackageRegistryForm.FormDestroy(Sender: TObject);
begin
  ini.writeInteger('package-browser-view', 'width-name', ListView1.Columns[0].width);
  ini.writeInteger('package-browser-view', 'width-ver', ListView1.Columns[1].width);
  ini.writeInteger('package-browser-view', 'width-fver', ListView1.Columns[2].width);
  ini.writeInteger('package-browser-view', 'width-age', ListView1.Columns[3].width);
  ini.writeInteger('package-browser-view', 'width-size', ListView1.Columns[4].width);
  ini.writeInteger('package-browser-view', 'width-canonical', ListView1.Columns[5].width);
  ini.writeInteger('package-browser-view', 'width', width);
  ini.writeInteger('package-browser-view', 'height', height);
  FManager.free;
end;

procedure TPackageRegistryForm.FormShow(Sender: TObject);
begin
  ListView1.Columns[0].width := ini.readInteger('package-browser-view', 'width-name', ListView1.Columns[0].width);
  ListView1.Columns[1].width := ini.readInteger('package-browser-view', 'width-ver', ListView1.Columns[1].width);
  ListView1.Columns[2].width := ini.readInteger('package-browser-view', 'width-fver', ListView1.Columns[2].width);
  ListView1.Columns[3].width := ini.readInteger('package-browser-view', 'width-age', ListView1.Columns[3].width);
  ListView1.Columns[4].width := ini.readInteger('package-browser-view', 'width-size', ListView1.Columns[4].width);
  ListView1.Columns[5].width := ini.readInteger('package-browser-view', 'width-canonical', ListView1.Columns[5].width);
  width := ini.readInteger('package-browser-view', 'width', width);
  height := ini.readInteger('package-browser-view', 'height', height);
  if not FManager.doLoad then
    Close;
end;

procedure TPackageRegistryForm.packageWork(sender: TObject; pct: integer; done: boolean; msg: String);
begin

end;

procedure TPackageRegistryForm.SetIni(AValue: TIniFile);
begin
  FIni := AValue;
  FManager.Settings := FIni;
end;

end.

