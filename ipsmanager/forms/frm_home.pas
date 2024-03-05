unit frm_home;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Menus, ActnList, laz.VirtualTrees,
  mvBase, mvDataSources;

type

  { TIPSManagerForm }

  TIPSManagerForm = class(TForm)
    actionViewDataSources: TAction;
    actionViewDocumentList: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    imgMain: TImageList;
    vtNavigator: TLazVirtualStringTree;
    MainMenu1: TMainMenu;
    mnuApple: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    mnuItemSettings: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    mnuFileExit: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    pnlPresentation: TPanel;
    pnlNavigator: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure actionViewDataSourcesExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure mnuFileExitClick(Sender: TObject);
  private
    FView : TViewManager;
    procedure SetView(AValue: TViewManager);
  public
    property View : TViewManager read FView write SetView;
  end;

var
  IPSManagerForm: TIPSManagerForm;

implementation

{$R *.lfm}

{ TIPSManagerForm }

procedure TIPSManagerForm.MenuItem2Click(Sender: TObject);
begin

end;

procedure TIPSManagerForm.mnuFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TIPSManagerForm.SetView(AValue: TViewManager);
begin
  FView.Free;
  FView := AValue;
  FView.navigator := vtNavigator;
  FView.NavPanel := pnlNavigator;
  FView.presentation := pnlPresentation;
end;

procedure TIPSManagerForm.FormCreate(Sender: TObject);
begin
  {$IFDEF OSX}
  mnuApple.caption := #$EF#$A3#$BF;
  mnuItemSettings.caption := 'Preferences...';
  {$ELSE}
  mnuApple.Visible := false;
  {$ENDIF}
  self.actionViewDataSources.OnExecute(self);
end;

procedure TIPSManagerForm.FormDestroy(Sender: TObject);
begin
  FView.free;
end;

procedure TIPSManagerForm.actionViewDataSourcesExecute(Sender: TObject);
begin
  //View := TDataSourceViewManager.create(FIniFile);
  //View.Initialize;
end;

end.

