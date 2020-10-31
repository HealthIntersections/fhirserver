unit FHIR.Toolkit.Context;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  FHIR.Support.Base, FHIR.Support.Stream;

// supported formats:
// ini
// xml - plain, schema, xslt
// json
// hl7 v2 vertical bar
// cda
// dicom
// javascript
// map
// liquid
// markdown
// resource

type
  TToolkitContext = class;

  { TToolkitContextObject }

  TToolkitContextObject = class (TFslObject)
  private
    FContext : TToolkitContext; // no ownership
  public
    constructor create(context : TToolkitContext); virtual;
    property Context : TToolkitContext read FContext write FContext;
  end;

  { TToolkitEditor }

  TToolkitEditor = class (TToolkitContextObject)
  private
    FAddress: String;
    FGuid : String;
    FCanCut: boolean;
    FCanPaste: boolean;
    FCanRedo: boolean;
    FCanUndo: boolean;
    FHasText: boolean;
    FInSource: boolean;
    FLoadState: boolean;
    function GetBytes: TBytes;
    function GetCanBeSaved: boolean;
    procedure SetBytes(AValue: TBytes);
  public
    constructor create(context : TToolkitContext; guid : String);
    property Context : TToolkitContext read FContext;

    // storage related properties
    property Guid : String read FGuid;
    property Address : String read FAddress;
    property LoadState : boolean read FLoadState write FLoadState;

    property AsBytes : TBytes read GetBytes write SetBytes;

    // editing related functionality
    procedure local(location : TSourceLocation);

    // status for actions
    property CanBeSaved : boolean read GetCanBeSaved;
    property hasText : boolean read FHasText write FHasText;
    property canUndo : boolean read FCanUndo write FCanUndo;
    property canRedo : boolean read FCanRedo write FCanRedo;
    property canCut : boolean read FCanCut write FCanCut;
    property canPaste : boolean read FCanPaste write FCanPaste;
    property inSource : boolean read FInSource write FInSource;
  end;

  { TToolkitContext }

  TToolkitContext = class (TFslObject)
  private
    function GetFocus: TToolkitEditor;
    function GetHasFocus: boolean;
  public
    property hasFocus : boolean read GetHasFocus;
    property focus : TToolkitEditor read GetFocus;
//    property persistenceServices : TToolkitPersistenceServices;
//    property EditorList : TToolkitEditorList;
////
//    property ProjectManager : TToolkitProjectManagerView;
//    property Messages : TToolkitMessagesView;
//    property Console : TToolkitConsoleView;
//    property Search : TToolkitSearchView;
//    property Inspector : TToolkitEditorInspectorView;
//    property MetadataView : TToolkitEditorMetadataView;
//    property Tasks : TToolkitEditorTasksView;
//    property Variables : TToolkitEditorVariablesView;
//    property Breakpoints : TToolkitEditorBreakpointsView;
//    property CallStack : TToolkitEditorCallStackView;
//    property FHIRPath : TToolkitFHIRPathView;
  end;

implementation

{ TToolkitEditor }

function TToolkitEditor.GetBytes: TBytes;
begin

end;

function TToolkitEditor.GetCanBeSaved: boolean;
begin

end;

procedure TToolkitEditor.SetBytes(AValue: TBytes);
begin

end;

constructor TToolkitEditor.create(context : TToolkitContext; guid: String);
begin
  inherited create(context);
  FGuid := guid;
end;

procedure TToolkitEditor.local(location: TSourceLocation);
begin

end;

{ TToolkitContextObject }

constructor TToolkitContextObject.create(context: TToolkitContext);
begin
  inherited create;
  FContext := context;
end;

{ TToolkitContext }

function TToolkitContext.GetFocus: TToolkitEditor;
begin
  result := nil;
end;

function TToolkitContext.GetHasFocus: boolean;
begin
  result := false;
end;

end.

