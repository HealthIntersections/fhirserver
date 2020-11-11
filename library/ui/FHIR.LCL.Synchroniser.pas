unit FHIR.LCL.Synchroniser;

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  SynEdit, SynEditTypes,
  FHIR.Support.Base, FHIR.Support.Stream,
  FHIR.Base.Objects, FHIR.Base.Factory, FHIR.Base.Parser,
  FHIR.Web.Parsers;

{
This object maintains a SynEdit source code for a resource in sync with an object model
that represents the source code. The object instantiates the model, and then any changes
made to the object (by code, usually,
}

type
  TFHIRSynEditSynchroniserOpStatus = (opNone, opChange);

  { TFHIRSynEditSynchroniser }

  TFHIRSynEditSynchroniser = class (TFslObject)
  private
    FEdit: TSynEdit;
    FFactory: TFHIRFactory;
    FFormat: TFHIRFormat;
    FResource: TFHIRResourceV;

    FOpInProgress : TFHIRSynEditSynchroniserOpStatus;
    FContainer : TFHIRObject;
    FFocus : TFHIRObject;
    FStart : TPoint;
    FFinish : TPoint;

    procedure SetFactory(AValue: TFHIRFactory);

    procedure loadXml;
    procedure loadJson;

    function writeToSource : String;
    function extractFromLines(lines : TStringList; start, finish : TSourceLocation) : String;
    function measure(source : String) : TSourceLocation;

    procedure finishOpChange;
  public
    destructor Destroy; override;

    // set up
    property SynEdit : TSynEdit read FEdit write FEdit;
    property Factory : TFHIRFactory read FFactory write SetFactory;
    property Format : TFHIRFormat read FFormat write FFormat;

    // working status
    property Resource : TFHIRResourceV read FResource;
    procedure load;

    // editing interface
    // editing is a 3 phase process.
    //  1. set up an edit operation
    //  2. make the changes to the object tree
    //  3. call commit
    //
    // Only one operation per commit

    // set the value of the provided property, which already exists. The property is an object, which may be quite extensive.
    // from a user point of view, smaller operations are better than big ones, but they are technically possible.
    // the objects maye be (typically are) primitives. The object may be in a repeating list
    procedure changeProperty(container : TFHIRObject; obj : TFHIRObject);

    // add a property, not to a list
    procedure addProperty(owner : TFHIRObject; name : String);

    // delete property, not from a list
    procedure deleteProperty(owner : TFHIRObject; name : String);

    // add to a list, after = nil to add in start
    procedure addToList(owner : TFHIRObject; name : String; after : TFHIRObject);

    // delete from the list
    procedure deleteFromList(owner : TFHIRObject; name : String; obj : TFHIRObject);

    // move in the list,
    procedure moveInList(owner : TFHIRObject; name : String; obj : TFHIRObject; up : boolean);

    procedure commit;
    procedure abandon;
  end;
  
  
implementation 

// we start by parsing the source.
//
// we don't how much of the source is pretty-printed, and even if it was, it might not be the way we do it exactly
// so we don't make any assumptions about that. But we do use out style of pretty printing when generating text
//
// so we parse the source, and have the source locations on all the objects.
// note that for json primitives, we may have 2 locations.
//
// when an operation is initiated, we determine the start and end of the object in question.
// at the end of the operation, we re-render the object in question (actually, the entire resource)
// then we delete the old content, and insert the new content (if any), and then adjust the
// source locations for anything that follows
//
//


{ TFHIRSynEditSynchroniser }

destructor TFHIRSynEditSynchroniser.Destroy;
begin
  FFocus.Free;  // though we really expect it to nil
  FFactory.free;
  inherited Destroy;
end;

procedure TFHIRSynEditSynchroniser.SetFactory(AValue: TFHIRFactory);
begin
  FFactory.free;
  FFactory := AValue;
end;

procedure TFHIRSynEditSynchroniser.loadXml;
var
  p : TFHIRParser;
begin
  FResource.Free;
  FResource := nil;

  // todo: for the synchronizer, we require that we're using default namespaces. It's not clear what's the best way
  // to manage this
  p := FFactory.makeParser(nil, FFormat, THTTPLanguages.Create('en'));
  try
    p.KeepParseLocations := true;
    FResource := p.parseResource(SynEdit.text); // this will use UTF8, so that positions match, since we are UTF-8 internally
  finally
    p.free;
  end;
end;

procedure TFHIRSynEditSynchroniser.loadJson;
var
  p : TFHIRParser;
begin
  FResource.Free;
  FResource := nil;

  p := FFactory.makeParser(nil, FFormat, THTTPLanguages.Create('en'));
  try
    p.KeepParseLocations := true;
    FResource := p.parseResource(SynEdit.text); // this will use UTF8, so that positions match, since we are UTF-8 internally
  finally
    p.free;
  end;
end;

function TFHIRSynEditSynchroniser.writeToSource: String;
var
  ss : TStringStream;
  c : TFHIRComposer;
begin
  ss := TStringStream.create('', TEncoding.UTF8);
  try
    c := FFactory.makeComposer(nil, FFormat, THTTPLanguages.Create('en'), OutputStylePretty);
    try
      c.KeepLocationData := true;
      c.compose(ss, FResource);
    finally
      c.free;
    end;
    result := ss.DataString;
  finally
    ss.free;
  end;
end;

function TFHIRSynEditSynchroniser.extractFromLines(lines: TStringList; start, finish: TSourceLocation): String;
var
  i : integer;
begin
  if start.line > finish.line then
    result := ''
  else if start.line = finish.line then
    result := lines[start.line].subString(start.col, finish.col - start.col)
  else
  begin
    result :=  lines[start.line].subString(start.col);
    for i := start.line +1 to finish.line - 1 do
      result := result + #13#10 + lines[i];
    result := result + #13#10 + lines[finish.line].subString(0, finish.col);
  end;
end;

function TFHIRSynEditSynchroniser.measure(source: String): TSourceLocation;
var
  i : integer;
begin
  result := TSourceLocation.Create;
  i := 1;
  while i <= source.length do
  begin
    case source[i] of
      #10:
          result.incLine;
      #13:
        begin
          result.incLine;
          if (i < source.Length) and (source[i + 1] = #10) then
            inc(i);
        end;
      else
        result.incCol;
    end;
    inc(i);
  end;
end;

procedure TFHIRSynEditSynchroniser.finishOpChange;
var
  src : String;
  lines : TStringList;
  span : TSourceLocation;
  ps, pf : TPoint;
  do1, do2 : boolean;
begin
  do1 := FFocus.LocationData.hasLocation1;
  do2 := FFocus.LocationData.hasLocation2;
  if do1 and do2 then
    raise Exception.create('not supported yet');

  lines := TStringList.create;
  try
    // write the parent object to the selected format
    lines.Text := writeToSource; // for more efficiency, try just the immediate parent (not ready yet)

    // figure out the new text to insert
    if do1 then
      src := extractFromLines(lines, FFocus.LocationData.composeStart, FFocus.LocationData.composeFinish).Trim
    else
      src := extractFromLines(lines, FFocus.LocationData.composeStart2, FFocus.LocationData.composeFinish2).Trim;

    if (FFormat = ffJson) and (src.EndsWith(',') then
      src := src.Substring(0, src.length-1);

    span := measure(src);
  finally
    lines.Free;
  end;

  // start a transaction
  SynEdit.BeginUndoBlock;

  // replace the existing content
  if (do1) then
  begin
    ps := FFocus.LocationData.parseStart.toPoint;
    pf := FFocus.LocationData.parseFinish.toPoint;
  end
  else
  begin
    ps := FFocus.LocationData.parseStart2.toPoint;
    pf := FFocus.LocationData.parseFinish2.toPoint;
  end;
  SynEdit.SetTextBetweenPoints(ps, pf, src, [setSelect], scamIgnore, smaMoveUp, smNormal);
  SynEdit.EndUndoBlock;

  // update remaining content for new content metrics
  if (do1) then
    FResource.updateLocationData(FFocus.LocationData.parseStart, FFocus.LocationData.parseFinish, span)
  else
    FResource.updateLocationData(FFocus.LocationData.parseStart2, FFocus.LocationData.parseFinish2, span);
end;

procedure TFHIRSynEditSynchroniser.load;
begin
  case format of
    ffXml : loadXml;
    ffJson : loadJson;
  else
    raise Exception.create('This format is not supported');
  end;
end;

procedure TFHIRSynEditSynchroniser.changeProperty(container : TFHIRObject; obj: TFHIRObject);
begin
  FOpInProgress := opChange;
  FStart := obj.LocationData.ParseStart.toPoint;
  FFinish := obj.LocationData.parseFinish.toPoint;
  FContainer := container.link;
  FFocus := obj.link;
end;

procedure TFHIRSynEditSynchroniser.addProperty(owner: TFHIRObject; name: String);
begin
  raise Exception.create('not done yet');
end;

procedure TFHIRSynEditSynchroniser.deleteProperty(owner: TFHIRObject; name: String);
begin
  raise Exception.create('not done yet');
end;

procedure TFHIRSynEditSynchroniser.addToList(owner: TFHIRObject; name: String; after: TFHIRObject);
begin
  raise Exception.create('not done yet');
end;

procedure TFHIRSynEditSynchroniser.deleteFromList(owner: TFHIRObject; name: String; obj: TFHIRObject);
begin
  raise Exception.create('not done yet');
end;

procedure TFHIRSynEditSynchroniser.moveInList(owner: TFHIRObject; name: String; obj: TFHIRObject; up: boolean);
begin
  raise Exception.create('not done yet');
end;

procedure TFHIRSynEditSynchroniser.commit;
begin
  case FOpInProgress of
    opNone : raise Exception.create('No operation in process');
    opChange : finishOpChange;
  else
    raise Exception.create('not done yet');
  end;
end;

procedure TFHIRSynEditSynchroniser.abandon;
begin
  FOpInProgress := opNone;
  FFocus.Free;
end;

end.
