unit FHIR.Toolkit.Context;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FHIR.Support.Base;

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
    FCanCut: boolean;
    FCanPaste: boolean;
    FCanRedo: boolean;
    FCanUndo: boolean;
    FHasText: boolean;
    FInSource: boolean;
  public
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

