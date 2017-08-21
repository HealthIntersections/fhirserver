unit ServerForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.TabControl, FMX.ListBox, FMX.Layouts,
  FHIRClient,
  AppEndorserForm;

type
  TServerFrameForm = class (TFrame)
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
    procedure btnTestClick(Sender: TObject);
  private
    FClient: TFHIRClient;
    FTabs : TTabControl;
    procedure SetClient(const Value: TFHIRClient);
    { Private declarations }
  public
    { Public declarations }
    Destructor Destroy; override;
    property Client : TFHIRClient read FClient write SetClient;
    property Tabs : TTabControl read FTabs write FTabs;
  end;

implementation

{$R *.fmx}

{ TServerFrameForm }

procedure TServerFrameForm.btnTestClick(Sender: TObject);
var
  tab : TTabItem;
  appForm : TAppEndorsementForm;
begin
  tab := FTabs.Add(TTabItem);
  FTabs.ActiveTab := tab;
  tab.Text := 'AppEndorser for '+FClient.address;
  appForm := TAppEndorsementForm.create(tab);
  appForm.Parent := tab;
//  appForm.Align := TAlignLayout.Client;
//  appForm.Client := client.link;
end;

destructor TServerFrameForm.Destroy;
begin
  FClient.free;
  inherited;
end;

procedure TServerFrameForm.SetClient(const Value: TFHIRClient);
begin
  FClient.free;
  FClient := Value;
end;

end.
