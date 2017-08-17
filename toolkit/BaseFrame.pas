unit BaseFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox, FMX.Edit, FMX.TabControl, FMX.TreeView, FMX.Layouts,
  FMX.Controls.Presentation, FMX.Platform;

type
  TBaseFrame = class(TFrame)
  private
    FTabs : TTabControl;
    FTab  : TTabItem;
  public
    property Tabs : TTabControl read FTabs write FTabs;
    property Tab : TTabItem read FTab write FTab;

    procedure Close;

    function markbusy : IFMXCursorService;
    procedure markNotBusy(cs : IFMXCursorService);
  end;

implementation

{ TBaseFrame }

procedure TBaseFrame.Close;
var
  i : integer;
begin
  i := tabs.TabIndex;
  tab.Free;
  if i > 0 then
    tabs.TabIndex := i - 1
  else
    tabs.TabIndex := 0;
end;

function TBaseFrame.markbusy : IFMXCursorService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    result := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService
  else
    result := nil;
  if Assigned(result) then
  begin
    TForm(Tabs.Owner).Cursor := result.GetCursor;
    result.SetCursor(crHourGlass);
  end;
end;

procedure TBaseFrame.markNotBusy(cs: IFMXCursorService);
begin
  if Assigned(CS) then
    cs.setCursor(TForm(Tabs.Owner).Cursor);
end;

end.
