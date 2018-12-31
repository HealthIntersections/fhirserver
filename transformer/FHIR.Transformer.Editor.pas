unit FHIR.Transformer.Editor;

interface

uses
  SysUtils, Vcl.ComCtrls,
  ScintEdit, ScintFormats,
  FHIR.Support.Base,
  FHIR.Transformer.Workspace;

type
  TEditorInformation = class (TFslObject)
  private
    FErrorLine: Integer;
    FStepLine: Integer;
    FIsDirty: boolean;
    FInfo: TWorkspaceFile;
    FMemo: TScintEdit;
    FTab: TTabSheet;
    FFileIsReadOnly: boolean;
    FReadOnly: boolean;
    FFileTime : TDateTime;

    procedure SetInfo(const Value: TWorkspaceFile);
    procedure SetReadOnly(const Value: boolean);

  public
    constructor Create; override;
    destructor Destroy; override;
    property id : TWorkspaceFile read FInfo write SetInfo;
    property tab : TTabSheet read FTab write FTab;
    property memo : TScintEdit read FMemo write FMemo;
    property fileIsReadOnly : boolean read FFileIsReadOnly write FFileIsReadOnly;
    property isDirty : boolean read FIsDirty write FIsDirty;
    property ErrorLine : Integer read FErrorLine write FErrorLine;
    property StepLine : Integer read FStepLine write FStepLine;
    property readOnly : boolean read FReadOnly write SetReadOnly;
    property FileTime : TDateTime read FFileTime write FFileTime;
  end;


implementation


{ TEditorInformation }

constructor TEditorInformation.Create;
begin
  inherited;
end;

destructor TEditorInformation.destroy;
begin
  FInfo.free;
  inherited;
end;

procedure TEditorInformation.SetInfo(const Value: TWorkspaceFile);
begin
  FInfo.free;
  FInfo := Value;
end;

procedure TEditorInformation.SetReadOnly(const Value: boolean);
begin
  FReadOnly := Value;
  if FReadOnly then
    memo.ReadOnly := true
  else
    memo.ReadOnly := fileIsReadOnly;
end;


end.
