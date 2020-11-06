unit FHIR.LCL.Synchroniser;

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  SynEdit,
  FHIR.Support.Base,
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

    FOpInProgress : boolean;
    FFocus : TFHIRObject;
    FStart : TPoint;
    FStop : TPoint;

    procedure SetFactory(AValue: TFHIRFactory);

    procedure loadXml;
    procedure loadJson;

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
    procedure  changeProperty({owner : TFHIRObject; name : String; }obj : TFHIRObject);

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
begin
  raise Exception.create('not done yet');
end;

procedure TFHIRSynEditSynchroniser.loadJson;
var
  p : TFHIRParser;
begin
  FResource.Free;
  FResource := nil;

  p := FFactory.makeParser(nil, FFormat, THTTPLanguages.Create('en'));
  try
    FResource := p.parseResource(SynEdit.text); // this will use UTF8, so that positions match, since we are UTF-8 internally
  finally
    p.free;
  end;
end;

procedure TFHIRSynEditSynchroniser.finishOpChange;
begin
  !

  // delete the old range
  SynEdit.CaretXY;
  // figure out the bytes for the new range
  // insert them
  // update all the following content
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

procedure TFHIRSynEditSynchroniser.changeProperty(obj: TFHIRObject);
begin
  FOpInProgress := opChange;
  FStart := obj.LocationStart;
  FEnd := obj.LocationStop;
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
