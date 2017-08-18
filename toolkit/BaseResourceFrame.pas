unit BaseResourceFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox, FMX.Edit, FMX.TabControl, FMX.TreeView, FMX.Layouts,
  FMX.Controls.Presentation, FMX.Platform,
  FHIRBase, FHIRTypes, FHIRResources, FHIRUtilities,
  BaseServerFrame;

type
  TBaseResourceFrame = class(TBaseServerFrame)
  private
    FFilename : String;
    FResource : TFHIRResource;
    FFormat: TFHIRFormat;
    FInputIsDirty: boolean;
    FResourceIsDirty: boolean;
    FLoading: boolean;
    procedure SetResource(const Value: TFHIRResource);
  public
    Destructor Destroy; override;

    property Filename : String read FFilename write FFilename;
    property Format : TFHIRFormat read FFormat write FFormat;
    property Resource : TFHIRResource read FResource write SetResource;
    function canSave : boolean; override;
    function canSaveAs : boolean; override;
    function save : boolean; override;
    function saveAs(filename : String; format : TFHIRFormat) : boolean; override;
    function isDirty : boolean; override;

    procedure commit; virtual;
    procedure cancel; virtual;

    Property ResourceIsDirty : boolean read FResourceIsDirty write FResourceIsDirty;
    Property InputIsDirty : boolean read FInputIsDirty write FInputIsDirty;
    Property Loading : boolean read FLoading write FLoading;
  end;
  TBaseResourceFrameClass = class of TBaseResourceFrame;

implementation

{ TBaseResourceFrame }

procedure TBaseResourceFrame.cancel;
begin
  // nothing
end;

function TBaseResourceFrame.canSave: boolean;
begin
  result := ((Filename <> '') and (Filename <> '$$')) or (Client <> nil);
end;

function TBaseResourceFrame.canSaveAs: boolean;
begin
  result := true;
end;

procedure TBaseResourceFrame.commit;
begin
  // nothing
end;

destructor TBaseResourceFrame.Destroy;
begin
  FResource.Free;
  inherited;
end;

function TBaseResourceFrame.isDirty: boolean;
begin
  result := FResourceIsDirty or FInputIsDirty;
end;

function TBaseResourceFrame.save : boolean;
begin
  if InputIsDirty then
    commit;
  result := FFilename <> '';
  if result then
    resourceToFile(resource, FFilename, format);
  ResourceIsDirty := false;
end;

function TBaseResourceFrame.saveAs(filename: String; format: TFHIRFormat): boolean;
var
  tab : TTabItem;
begin
  self.Filename := filename;
  self.Format := Format;
  result := save;
  tab := TTabItem(tagObject);
  tab.Text := ExtractFileName(filename);
  tab.Hint := filename;
end;

procedure TBaseResourceFrame.SetResource(const Value: TFHIRResource);
begin
  FResource.Free;
  FResource := Value;
end;

end.
