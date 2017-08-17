unit CapabilityStatementEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, FMX.Layouts, FMX.TreeView, FMX.Controls.Presentation,
  BaseResourceFrame;

type
  TCapabilityStatementEditorFrame = class(TBaseResourceFrame)
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
    TreeView1: TTreeView;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Label1: TLabel;
    btnClose: TButton;
    btnSave: TButton;
  private
    { Private declarations }

  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
