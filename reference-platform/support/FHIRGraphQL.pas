unit FHIRGraphQL;

interface

uses
  SysUtils, Classes,
  AdvObjects, AdvGenerics,
  MsXml, MsXmlparser,
  GraphQL,
  FHIRBase, FHIRTypes, FHIRResources, FHIRParser;

type
  TFHIRGraphQLEngineDereferenceEvent = function(context : TFHIRResource; reference : TFHIRReference; out targetContext, target : TFHIRResource) : boolean of Object;

  TFHIRGraphQLEngine = class (TAdvObject)
  private
    FFocus: TFHIRResource;
    FQueryName: String;
    FOutput: TGraphQLObjectValue;
    FDocument: TGraphQLDocument;
    FOnFollowReference: TFHIRGraphQLEngineDereferenceEvent;
    procedure SetDocument(const Value: TGraphQLDocument);
    procedure SetFocus(const Value: TFHIRResource);

    function hasArgument(arguments : TAdvList<TGraphQLArgument>; name, value : String) : boolean;
    procedure processObject(context : TFHIRResource; source : TFHIRObject; target : TGraphQLObjectValue; selection : TAdvList<TGraphQLSelection>);
    procedure processPrimitive(arg : TGraphQLArgument; value : TFHIRObject);
    procedure processReference(context : TFHIRResource; source : TFHIRObject; field : TGraphQLField; target : TGraphQLObjectValue);
    procedure processValues(context : TFHIRResource; sel: TGraphQLSelection; prop: TFHIRProperty; target: TGraphQLObjectValue);

  public
    Constructor Create; override;
    Destructor Destroy; override;

    // the focus resource - if there is one. If there isn't, then the focus is a collection
    property focus : TFHIRResource read FFocus write SetFocus;

    // the graphql document to execute
    property queryDocument : TGraphQLDocument read FDocument write SetDocument;

    // the name of the query (if there is more than one
    property queryName : String read FQueryName write FQueryName;

    // where output is going to go
    property output : TGraphQLObjectValue read FOutput;

    property OnFollowReference : TFHIRGraphQLEngineDereferenceEvent read FOnFollowReference write FOnFollowReference;

    procedure execute;
  end;

implementation

{ TFHIRGraphQLEngine }

constructor TFHIRGraphQLEngine.Create;
begin
  inherited;

end;

destructor TFHIRGraphQLEngine.Destroy;
begin
  FFocus.Free;
  FOutput.Free;
  FDocument.Free;
  inherited;
end;

procedure TFHIRGraphQLEngine.SetDocument(const Value: TGraphQLDocument);
begin
  FDocument.Free;
  FDocument := Value;
end;

procedure TFHIRGraphQLEngine.SetFocus(const Value: TFHIRResource);
begin
  FFocus.Free;
  FFocus := Value;
end;

procedure TFHIRGraphQLEngine.execute;
begin
  FOutput.Free;
  FOutput := TGraphQLObjectValue.Create;

  // todo: initial conditions
  if FDocument.Operations.Count <> 1 then
    raise Exception.Create('Unable to process graphql');

  processObject(FFocus, FFocus, FOutput, FDocument.Operations[0].SelectionSet);
end;

function TFHIRGraphQLEngine.hasArgument(arguments: TAdvList<TGraphQLArgument>; name, value: String): boolean;
var
  arg : TGraphQLArgument;
begin
  for arg in arguments do
    if (arg.Name = name) and arg.hasValue(value) then
      exit(true);
  result := false;
end;

procedure TFHIRGraphQLEngine.processValues(context : TFHIRResource; sel: TGraphQLSelection; prop: TFHIRProperty; target: TGraphQLObjectValue);
var
  arg: TGraphQLArgument;
  value: TFHIRObject;
  new: TGraphQLObjectValue;
begin
  arg := target.addField(sel.field.Alias, prop.IsList);
  for value in prop.Values do
  begin
    if value.isPrimitive then
      processPrimitive(arg, value)
    else
    begin
      if sel.field.SelectionSet.Empty then
        raise EGraphQLException.Create('No Fields selected on a complex object');
      new := TGraphQLObjectValue.Create;
      try
        arg.Values.Add(new.Link);
        processObject(context, value, new, sel.field.SelectionSet);
      finally
        new.Free;
      end;
    end;
  end;
end;

procedure TFHIRGraphQLEngine.processObject(context : TFHIRResource; source: TFHIRObject; target: TGraphQLObjectValue; selection: TAdvList<TGraphQLSelection>);
var
  sel : TGraphQLSelection;
  prop : TFHIRProperty;
begin
  for sel in selection do
  begin
    if (sel.Field <> nil) then
    begin
      prop := source.getPropertyValue(sel.field.Name);
      try
        if prop = nil then
        begin
          if (sel.field.Name = 'resource') and (source.fhirType = 'Reference') then
            processReference(context, source, sel.field, target)
          else
            raise EGraphQLException.Create('Unknown property '+sel.field.Name+' on '+source.fhirType);
        end
        else if prop.hasValue then
          processValues(context, sel, prop, target);
      finally
        prop.Free;
      end;
    end;
  end;
end;

procedure TFHIRGraphQLEngine.processPrimitive(arg : TGraphQLArgument; value: TFHIRObject);
var
  s : String;
begin
  s := value.fhirType;
  if (s = 'integer') or (s = 'decimal') or (s = 'unsignedInt') or (s = 'positiveInt') then
    arg.values.add(TGraphQLNumberValue.Create(value.primitiveValue))
  else if (s = 'boolean') then
    arg.values.add(TGraphQLNameValue.Create(value.primitiveValue))
  else
    arg.values.add(TGraphQLStringValue.Create(value.primitiveValue));
end;


procedure TFHIRGraphQLEngine.processReference(context : TFHIRResource; source: TFHIRObject; field: TGraphQLField; target: TGraphQLObjectValue);
var
  ref : TFhirReference;
  ok : boolean;
  ctxt, dest : TFhirResource;
  arg: TGraphQLArgument;
  new : TGraphQLObjectValue;
begin
  if not (source is TFhirReference) then
    raise EGraphQLException.Create('Not done yet');
  if not assigned(FOnFollowReference) then
    raise EGraphQLException.Create('Resource Referencing services not provided');

  ref := TFhirReference(source).Link;
  try
    ok := FOnFollowReference(context, ref, ctxt, dest);
    if ok then
      try
        arg := target.addField(field.Alias, false);
        new := TGraphQLObjectValue.Create;
        try
          arg.Values.Add(new.Link);
          processObject(ctxt, dest, new, field.SelectionSet);
        finally
          new.Free;
        end;
      finally
        ctxt.Free;
        dest.Free;
      end
    else if not hasArgument(field.Arguments, 'optional', 'true') then
      raise EGraphQLException.Create('Unable to resolve reference to '+ref.reference);
  finally
    ref.Free;
  end;
end;

end.
