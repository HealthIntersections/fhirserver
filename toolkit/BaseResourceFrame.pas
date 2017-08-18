unit BaseResourceFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox, FMX.Edit, FMX.TabControl, FMX.TreeView, FMX.Layouts,
  FMX.Controls.Presentation, FMX.Platform,
  FHIRTypes, FHIRResources,
  BaseServerFrame;

type
  TBaseResourceFrame = class(TBaseServerFrame)
  private
    FFilename : String;
    FResource : TFHIRResource;
    procedure SetResource(const Value: TFHIRResource);
  public
    Destructor Destroy; override;

    property Filename : String read FFilename write FFilename;
    property Resource : TFHIRResource read FResource write SetResource;
  end;
  TBaseResourceFrameClass = class of TBaseResourceFrame;

implementation


{ TBaseResourceFrame }

destructor TBaseResourceFrame.Destroy;
begin
  FResource.Free;
  inherited;
end;

procedure TBaseResourceFrame.SetResource(const Value: TFHIRResource);
begin
  FResource.Free;
  FResource := Value;
end;

end.
