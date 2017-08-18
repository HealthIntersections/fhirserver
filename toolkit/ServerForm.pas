unit ServerForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.TabControl, FMX.ListBox, FMX.Layouts,
  FHIRTypes, FHIRResources, FHIRClient,
  BaseFrame, AppEndorserFrame, CapabilityStatementEditor;

type
  TFrame = TBaseFrame; // re-aliasing the Frame to work around a designer bug

  TServerFrame = class (TFrame)
    btnTest: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    Button1: TButton;
    pnlSearch: TPanel;
    Splitter1: TSplitter;
    Label2: TLabel;
    cbxSearchType: TComboBox;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Panel2: TPanel;
    ListBox1: TListBox;
    btnClose: TButton;
    procedure btnTestClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FClient: TFHIRClient;
    FCapabilityStatement: TFhirCapabilityStatement;
    FCSTab : TTabItem;
    FCsForm : TCapabilityStatementEditorFrame;
    procedure SetClient(const Value: TFHIRClient);
    procedure SetCapabilityStatement(const Value: TFhirCapabilityStatement);
    { Private declarations }
  public
    { Public declarations }
    Destructor Destroy; override;
    property Client : TFHIRClient read FClient write SetClient;
    property CapabilityStatement : TFhirCapabilityStatement read FCapabilityStatement write SetCapabilityStatement;

    procedure load; override;
  end;

implementation

{$R *.fmx}

{ TServerFrame }

procedure TServerFrame.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TServerFrame.btnTestClick(Sender: TObject);
var
  tab : TTabItem;
  appForm : TAppEndorsementFrame;
begin
  tab := Tabs.Add(TTabItem);
  Tabs.ActiveTab := tab;
  tab.Text := 'AppEndorser for '+FClient.address;
  appForm := TAppEndorsementFrame.create(tab);
  tab.TagObject := appForm;
  appForm.TagObject := tab;
  appForm.Parent := tab;
  appForm.Tabs := tabs;
  appForm.tab := tab;
  appForm.Align := TAlignLayout.Client;
  appForm.Client := client.link;
  appForm.load;
end;

procedure TServerFrame.Button1Click(Sender: TObject);
begin
  if FCSTab <> nil then
  begin
    FcsForm.Load;
    Tabs.ActiveTab := FCSTab;
  end
  else
  begin
    FCSTab := Tabs.Add(TTabItem);
    Tabs.ActiveTab := FCSTab;
    FCSTab.Text := 'Capability Statement for '+FClient.address;
    FcsForm := TCapabilityStatementEditorFrame.create(tab);
    FCSTab.TagObject := FCsForm;
    FCsForm.TagObject := FCSTab;
    FcsForm.Parent := FCSTab;
    FcsForm.Tabs := tabs;
    FcsForm.tab := FCSTab;
    FcsForm.Align := TAlignLayout.Client;
    FcsForm.Client := client.link;
    FcsForm.Resource := CapabilityStatement.Link;
    FcsForm.Filename := '$$';
    FcsForm.Load;
  end;
end;

destructor TServerFrame.Destroy;
begin
  FClient.free;
  FCapabilityStatement.free;
  inherited;
end;

procedure TServerFrame.load;
begin
  inherited;

end;

procedure TServerFrame.SetCapabilityStatement(const Value: TFhirCapabilityStatement);
begin
  FCapabilityStatement.free;
  FCapabilityStatement := Value;
end;

procedure TServerFrame.SetClient(const Value: TFHIRClient);
begin
  FClient.free;
  FClient := Value;
end;

end.
