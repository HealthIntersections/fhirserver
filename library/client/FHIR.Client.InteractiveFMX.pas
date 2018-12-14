unit FHIR.Client.InteractiveFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.ListBox, FMX.StdCtrls, FMX.Memo, FMX.Grid, FMX.ScrollBox,
  FMX.Edit, FMX.Controls.Presentation,
  FHIR.Base.Objects, FHIR.Base.Lang, FHIR.Base.Parser,
  FHIR.Client.Base, FHIR.Client.Threaded;

type
  TInteractiveClientForm = class(TForm)
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Splitter1: TSplitter;
    Panel5: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    edtAddressReq: TEdit;
    Panel6: TPanel;
    memBodyReq: TMemo;
    Panel7: TPanel;
    Label4: TLabel;
    Panel8: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    edtStatusResp: TEdit;
    Panel9: TPanel;
    memBodyResp: TMemo;
    Panel10: TPanel;
    Label7: TLabel;
    Panel2: TPanel;
    Label1: TLabel;
    edtServer: TEdit;
    btnAbort: TButton;
    btnReturn: TButton;
    btnServer: TButton;
    btnServerReturn: TButton;
    btnLoad: TButton;
    btnSave: TButton;
    ComboBox1: TComboBox;
    Panel11: TPanel;
    grdHeadersReq: TGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    Panel12: TPanel;
    Label8: TLabel;
    Button1: TButton;
    Button2: TButton;
    Panel13: TPanel;
    grdHeadersResp: TGrid;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    Panel14: TPanel;
    Label9: TLabel;
    Button3: TButton;
    Button4: TButton;
    Label10: TLabel;
    cbxFormat: TComboBox;
    procedure Panel3Resize(Sender: TObject);
    procedure Panel9Resize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FRequestHeaders : TStringList;
    FResponseHeaders : TStringList;
    FPackage: TFhirThreadedClientPackage;
    FClient: TFhirClientV;
    procedure SetPackage(const Value: TFhirThreadedClientPackage);
    procedure LoadRequest;
    procedure LoadResponse;
    procedure SetClient(const Value: TFhirClientV);
  public
    property package : TFhirThreadedClientPackage read FPackage write SetPackage;
    property client : TFhirClientV read FClient write SetClient;
  end;

var
  InteractiveClientForm: TInteractiveClientForm;

implementation

{$R *.fmx}

procedure TInteractiveClientForm.FormCreate(Sender: TObject);
begin
  FRequestHeaders := TStringList.create;
  FResponseHeaders := TStringList.create;
end;

procedure TInteractiveClientForm.FormDestroy(Sender: TObject);
begin
  FPackage.Free;
  FClient.free;
  FRequestHeaders.Free;
  FResponseHeaders.Free;
end;

procedure TInteractiveClientForm.FormShow(Sender: TObject);
begin
  LoadRequest;
  LoadResponse;
end;

procedure TInteractiveClientForm.LoadRequest;
var
  s, p : String;
  i : integer;
  cmp : TFHIRComposer;
begin
  if package = nil then
    exit;
  p := package.paramString;
  for s in package.params do
    p := p+'&'+s;
  if package.summary then
    p := p+'&_summary=true';
  if p.StartsWith('&') then
    p := p.Substring(1);
  if p <> '' then
    p := '?'+p;
  case package.command of
    fcmdRead : edtAddressReq.Text := 'GET [base]/'+package.resourceType+'/'+package.id;
    fcmdVersionRead : edtAddressReq.Text := 'GET [base]/'+package.resourceType+'/'+package.id+'/_history/'+package.vid;
    fcmdUpdate : edtAddressReq.Text := 'PUT [base]/'+package.resourceType+'/'+package.id;
    fcmdDelete : edtAddressReq.Text := 'DELETE [base]/'+package.resourceType+'/'+package.id;
    fcmdHistoryInstance : edtAddressReq.Text := 'GET [base]/'+package.resourceType+'/'+package.id+'/_history';
    fcmdCreate : edtAddressReq.Text := 'POST [base]/'+package.resourceType;
    fcmdSearch :
      if FPackage.resourceType = '' then
        edtAddressReq.Text := 'GET [base]'+p
      else
        edtAddressReq.Text := 'GET [base]/'+package.resourceType+p;
    fcmdHistoryType : edtAddressReq.Text := 'GET [base]/'+package.resourceType+'/_history';
    // fcmdValidate : edtAddressReq.Text := '';
    fcmdMetadata : if FPackage.summary then
        edtAddressReq.Text := 'GET [base]/metadata?_summary=true'
      else
        edtAddressReq.Text := 'GET [base]/metadata';
    fcmdTransaction : edtAddressReq.Text := 'POST [base]';
    fcmdHistorySystem : edtAddressReq.Text := 'GET [base]/_history';
    fcmdOperation :
      begin
        if FPackage.resource <> nil then
          s := 'POST'
        else
          s := 'GET';
        if FPackage.id <> '' then
          edtAddressReq.Text := s+' [base]/'+package.resourceType+'/'+package.id+'/$'+FPackage.name+p
        else
          edtAddressReq.Text := s+' [base]/'+package.resourceType+'/$'+FPackage.name+p;
      end;
    fcmdPatch : edtAddressReq.Text := 'PATCH [base]/'+package.resourceType+'/'+package.id;
    fcmdBatch : edtAddressReq.Text := 'POST [base]';
  else
    raise EFHIRException.Create('Unknown interaction');
  end;

  FRequestHeaders.Clear;
  case cbxFormat.ItemIndex of
    0: FRequestHeaders.AddPair('Accept', 'application/fhir+json');
    1: FRequestHeaders.AddPair('Accept', 'application/fhir+xml');
    2: FRequestHeaders.AddPair('Accept', 'application/fhir+turtle');
  end;
  package.headers.addToHeaders(FRequestHeaders);

  memBodyReq.Text := '';
  if package.resource <> nil then
  begin
    case cbxFormat.ItemIndex of
      0: cmp := client.makeComposer(ffJson, OutputStylePretty);
      1: cmp := client.makeComposer(ffXml, OutputStylePretty);
      2: cmp := client.makeComposer(ffTurtle, OutputStylePretty);
    end;
    try
      memBodyReq.text := cmp.Compose(package.resource);
    finally
      cmp.Free;
    end;
  end;
end;

procedure TInteractiveClientForm.LoadResponse;
begin
  if package = nil then
    exit;

end;

procedure TInteractiveClientForm.Panel3Resize(Sender: TObject);
begin
  grdHeadersReq.Columns[0].Width := Panel3.Width / 3;
  grdHeadersReq.Columns[1].Width := (Panel3.Width / 3 * 2) - 20; // 20 for scrollbar
end;

procedure TInteractiveClientForm.Panel9Resize(Sender: TObject);
begin
  grdHeadersResp.Columns[0].Width := Panel9.Width / 3;
  grdHeadersResp.Columns[1].Width := (Panel9.Width / 3 * 2) - 20; // 20 for scrollbar
end;

procedure TInteractiveClientForm.SetClient(const Value: TFhirClientV);
begin
  FClient.free;
  FClient := Value;
end;

procedure TInteractiveClientForm.SetPackage(const Value: TFhirThreadedClientPackage);
begin
  FPackage.Free;
  FPackage := Value;
end;

end.
