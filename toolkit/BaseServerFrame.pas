unit BaseServerFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox, FMX.Edit, FMX.TabControl, FMX.TreeView, FMX.Layouts,
  FMX.Controls.Presentation, FMX.Platform,
  FHIRClient,
  BaseFrame;

type
  TBaseServerFrame = class(TBaseFrame)
  private
    FClient : TFHIRClient;
    procedure SetClient(const Value: TFHIRClient);
  public
    Destructor Destroy; override;
    property Client : TFHIRClient read FClient write SetClient;
  end;

implementation


{ TBaseServerFrame }

destructor TBaseServerFrame.Destroy;
begin
  FClient.free;
  inherited;
end;

procedure TBaseServerFrame.SetClient(const Value: TFHIRClient);
begin
  FClient.free;
  FClient := Value;
end;

end.
