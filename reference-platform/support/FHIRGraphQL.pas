unit FHIRGraphQL;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}


interface

uses
  SysUtils, Classes,
  StringSupport,
  AdvObjects, AdvGenerics,
  ParseMap, GraphQL,
  FHIRBase, FHIRTypes, FHIRResources, FHIRConstants, FHIRParser, FHIRUtilities, FHIRPath;

type
  TFHIRGraphQLEngineDereferenceEvent = function(appInfo : TAdvObject; context : TFHIRResource; reference : TFHIRReference; out targetContext, target : TFHIRResource) : boolean of Object;
  TFHIRGraphQLEngineLookupEvent = function (appInfo : TAdvObject; requestType, id : String; var res : TFHIRResource) : boolean of Object;
  TFHIRGraphQLEngineListResourcesEvent = procedure (appInfo : TAdvObject; requestType: String; params : TAdvList<TGraphQLArgument>; list : TAdvList<TFHIRResource>) of Object;
  TFHIRGraphQLEngineSearchEvent = function (appInfo : TAdvObject; requestType: String; params : TAdvList<TGraphQLArgument>) : TFHIRBundle of Object;

  TFHIRGraphQLEngine = class (TAdvObject)
  private
    FOnFollowReference : TFHIRGraphQLEngineDereferenceEvent;
    FOnSearch : TFHIRGraphQLEngineSearchEvent;
    FOnListResources : TFHIRGraphQLEngineListResourcesEvent;
    FOnLookup : TFHIRGraphQLEngineLookupEvent;

    FFocus : TFHIRResource;
    FOutput : TGraphQLObjectValue;
    FGraphQL : TGraphQLPackage;
    FAppinfo : TAdvObject;
    FWorkingVariables: TAdvMap<TGraphQLArgument>;
    FPathEngine : TFHIRPathEngine;
    FMagicExpression : TFHIRPathExpressionNode;

    procedure SetGraphQL(const Value: TGraphQLPackage);
    procedure SetFocus(const Value: TFHIRResource);

    function listStatus(field: TGraphQLField; isList: boolean): TGraphQLArgumentListStatus;

    function getSingleValue(arg : TGraphQLArgument) : string;
    function resolveValues(arg : TGraphQLArgument; max : integer = -1; vars : String = '') : TAdvList<TGraphQLValue>;

    function checkBooleanDirective(dir : TGraphQLDirective) : Boolean;
    function checkDirectives(directives : TAdvList<TGraphQLDirective>) : boolean;
    procedure checkNoDirectives(directives : TAdvList<TGraphQLDirective>);
    function hasArgument(arguments : TAdvList<TGraphQLArgument>; name, value : String) : boolean;
    function targetTypeOk(arguments : TAdvList<TGraphQLArgument>; dest : TFHIRResource) : boolean;

    function filter(context : TFhirResource; prop:TFHIRProperty; arguments : TAdvList<TGraphQLArgument>; values : TFHIRObjectList; extensionMode : boolean)  : TAdvList<TFHIRObject>;
    function filterResources(fhirpath : TGraphQLArgument; bnd : TFHIRBundle)  : TAdvList<TFHIRResource>; overload;
    function filterResources(fhirpath : TGraphQLArgument; list : TAdvList<TFHIRResource>)  : TAdvList<TFHIRResource>; overload;
    procedure processObject(context : TFHIRResource; source : TFHIRObject; target : TGraphQLObjectValue; selection : TAdvList<TGraphQLSelection>; inheritedList : boolean; suffix : string);
    procedure processPrimitive(arg : TGraphQLArgument; value : TFHIRObject);
    procedure processReference(context : TFHIRResource; source : TFHIRObject; field : TGraphQLField; target : TGraphQLObjectValue; inheritedList : boolean; suffix : string);
    procedure processReverseReferenceList(source : TFHIRResource; field : TGraphQLField; target : TGraphQLObjectValue; inheritedList : boolean; suffix : string);
    procedure processReverseReferenceSearch(source : TFHIRResource; field : TGraphQLField; target : TGraphQLObjectValue; inheritedList : boolean; suffix : string);
    procedure processValues(context : TFHIRResource; sel: TGraphQLSelection; prop: TFHIRProperty; target: TGraphQLObjectValue; values : TAdvList<TFHIRObject>; extensionMode, inheritedList : boolean; suffix : string);
    procedure processSearch(target : TGraphQLObjectValue; selection : TAdvList<TGraphQLSelection>; inheritedList : boolean; suffix : string);
    procedure processSearchSingle(target : TGraphQLObjectValue; field : TGraphQLField; inheritedList : boolean; suffix : string);
    procedure processSearchSimple(target : TGraphQLObjectValue; field : TGraphQLField; inheritedList : boolean; suffix : string);
    procedure processSearchFull(target : TGraphQLObjectValue; field : TGraphQLField; inheritedList : boolean; suffix : string);
    procedure SetAppInfo(const Value: TAdvObject);
    procedure processVariables(op : TGraphQLOperation);
  public
    Constructor Create; override;
    Destructor Destroy; override;
    property appInfo : TAdvObject read FAppinfo write SetAppInfo;

    // the focus resource - if there is one. If there isn't, then the focus is a collection
    property focus : TFHIRResource read FFocus write SetFocus;

    // the graphql document to execute
    property GraphQL : TGraphQLPackage read FGraphQL write SetGraphQL;

    // where output is going to go
    property output : TGraphQLObjectValue read FOutput;

    property OnFollowReference : TFHIRGraphQLEngineDereferenceEvent read FOnFollowReference write FOnFollowReference;
    property OnSearch : TFHIRGraphQLEngineSearchEvent read FOnSearch write FOnSearch;
    property OnListResources : TFHIRGraphQLEngineListResourcesEvent read FOnListResources write FOnListResources;
    property OnLookup : TFHIRGraphQLEngineLookupEvent read FOnLookup write FOnLookup;

    procedure execute;
  end;

  TFHIRGraphQLSearchWrapper = class (TFHIRObject)
  private
    FBundle: TFhirBundle;
    FParseMap : TParseMap;
    procedure SetBundle(const Value: TFhirBundle);
    function extractLink(name : String) : TFhirString;
    function extractParam(name : String; int : boolean) : TFhirObject;
  public
    Constructor Create(bundle : TFhirBundle);
    Destructor Destroy; override;

    property Bundle : TFhirBundle read FBundle write SetBundle;
    function fhirType : String; override;

    function getPropertyValue(propName : string): TFHIRProperty; override;
  end;

  TFHIRGraphQLSearchEdge = class (TFHIRObject)
  private
    FEntry: TFhirBundleEntry;
    procedure SetEntry(const Value: TFhirBundleEntry);
  public
    Constructor Create(entry : TFhirBundleEntry);
    Destructor Destroy; override;

    property Entry : TFhirBundleEntry read FEntry write SetEntry;
    function fhirType : String; override;

    function getPropertyValue(propName : string): TFHIRProperty; override;
  end;

implementation

{ TFHIRGraphQLEngine }

function TFHIRGraphQLEngine.checkBooleanDirective(dir: TGraphQLDirective): Boolean;
var
  vl : TAdvList<TGraphQLValue>;
begin
  if dir.Arguments.Count <> 1 then
    raise EGraphQLException.Create('Unable to process @'+dir.Name+': expected a single argument "if"');
  if dir.Arguments[0].Name <> 'if' then
    raise EGraphQLException.Create('Unable to process @'+dir.Name+': expected a single argument "if"');
  vl := resolveValues(dir.Arguments[0], 1);
  try
    result := vl[0].ToString = 'true';
  finally
    vl.Free;
  end;
end;

function TFHIRGraphQLEngine.checkDirectives(directives: TAdvList<TGraphQLDirective>) : boolean;
var
  dir, skip, include : TGraphQLDirective;
begin
  skip := nil;
  include := nil;
  for dir in directives do
  begin
    if dir.Name = 'skip' then
    begin
      if (skip = nil) then
        skip := dir
      else
        raise EGraphQLException.Create('Duplicate @skip directives');
    end
    else if dir.Name = 'include' then
    begin
      if (include = nil) then
        include := dir
      else
        raise EGraphQLException.Create('Duplicate @include directives');
    end
    else if not StringArrayExistsSensitive(['flatten', 'first', 'singleton', 'slice'], dir.Name) then
      raise EGraphQLException.Create('Directive "'+dir.Name+'" is not recognised');
  end;
  if (skip <> nil) and (include <> nil) then
    raise EGraphQLException.Create('Cannot mix @skip and @include directives');
  if skip <> nil then
    result := not checkBooleanDirective(skip)
  else if include <> nil then
    result := checkBooleanDirective(include)
  else
    result := true;
end;

procedure TFHIRGraphQLEngine.checkNoDirectives(
  directives: TAdvList<TGraphQLDirective>);
begin

end;

constructor TFHIRGraphQLEngine.Create;
begin
  inherited;
  FWorkingVariables := TAdvMap<TGraphQLArgument>.create;
  FPathEngine := TFHIRPathEngine.Create(nil);
  FMagicExpression := TFHIRPathExpressionNode.Create(0);
end;

destructor TFHIRGraphQLEngine.Destroy;
begin
  FMagicExpression.Free;
  FPathEngine.Free;
  FAppinfo.Free;
  FFocus.Free;
  FOutput.Free;
  FGraphQL.Free;
  FWorkingVariables.Free;
  inherited;
end;

procedure TFHIRGraphQLEngine.SetAppInfo(const Value: TAdvObject);
begin
  FAppinfo.Free;
  FAppinfo := Value;
end;

procedure TFHIRGraphQLEngine.SetGraphQL(const Value: TGraphQLPackage);
begin
  FGraphQL.Free;
  FGraphQL := Value;
end;

procedure TFHIRGraphQLEngine.SetFocus(const Value: TFHIRResource);
begin
  FFocus.Free;
  FFocus := Value;
end;

function TFHIRGraphQLEngine.listStatus(field : TGraphQLField; isList : boolean) : TGraphQLArgumentListStatus;
begin
  if field.hasDirective('singleton') then
    result := listStatusSingleton
  else if isList then
    result := listStatusRepeating
  else
    result := listStatusNotSpecified;
end;

function TFHIRGraphQLEngine.targetTypeOk(arguments: TAdvList<TGraphQLArgument>; dest: TFHIRResource): boolean;
var
  list : TStringList;
  arg : TGraphQLArgument;
  vl : TAdvList<TGraphQLValue>;
  v : TGraphQLValue;
begin
  list := TStringList.Create;
  try
    for arg in arguments do
      if (arg.Name = 'type') then
      begin
        vl := resolveValues(arg);
        try
          for v in vl do
            list.Add(v.ToString);
        finally
          vl.Free;
        end;
      end;
    if list.Count = 0 then
      result := true
    else
      result := list.IndexOf(dest.fhirType) > -1;
  finally
    list.Free;
  end;
end;

procedure TFHIRGraphQLEngine.execute;
var
  op : TGraphQLOperation;
begin
  if FGraphQL = nil then
    raise Exception.Create('Unable to process graphql - graphql document missing');

  FOutput.Free;
  FOutput := TGraphQLObjectValue.Create;

  // todo: initial conditions
  if GraphQL.OperationName <> '' then
  begin
    op := GraphQL.Document.operation(GraphQL.OperationName);
    if op = nil then
      raise EGraphQLException.Create('Unable to find operation "'+GraphQL.OperationName+'"');
  end
  else if (GraphQL.Document.Operations.Count = 1) then
    op := GraphQL.Document.Operations[0]
  else
    raise EGraphQLException.Create('No operation name provided, so expected to find a single operation');

  if op.operationType = qglotMutation then
    raise EGraphQLException.Create('Mutation operations are not supported');

  checkNoDirectives(op.Directives);
  processVariables(op);
  if FFocus = nil then
    processSearch(FOutput, op.SelectionSet, false, '')
  else
    processObject(FFocus, FFocus, FOutput, op.SelectionSet, false, '');
end;

function TFHIRGraphQLEngine.filter(context : TFhirResource; prop:TFHIRProperty; arguments: TAdvList<TGraphQLArgument>; values: TFHIRObjectList; extensionMode : boolean): TAdvList<TFHIRObject>;
  function hasExtensions(obj : TFHIRObject) : boolean;
  begin
    if obj is TFhirBackboneElement then
      result := (TFhirBackboneElement(obj).extensionList.Count > 0) or (TFhirBackboneElement(obj).modifierExtensionList.Count > 0)
    else if obj is TFhirDomainResource then
      result := (TFhirDomainResource(obj).extensionList.Count > 0) or (TFhirDomainResource(obj).modifierExtensionList.Count > 0)
    else if obj is TFhirElement then
      result := (TFhirElement(obj).extensionList.Count > 0)
    else
      result := false;
  end;
  function passesExtensionMode(obj : TFHIRObject) : boolean;
  begin
    if not obj.isPrimitive then
      result := not extensionMode
    else if extensionMode then
      result := (obj.getId <> '') or hasExtensions(obj)
    else
      result := obj.primitiveValue <> '';
  end;
var
  fp : TStringBuilder;
  arg : TGraphQLArgument;
  p : TFHIRProperty;
  v : TFHIRObject;
  node: TFHIRPathExpressionNode;
  vl : TAdvList<TGraphQLValue>;
begin
  result := TAdvList<TFHIRObject>.create;
  try
    if values.Count > 0 then
    begin
      fp := TStringBuilder.Create;
      try
        for arg in arguments do
        begin
          vl := resolveValues(arg);
          try
            if (vl.Count <> 1) then
              raise Exception.Create('Incorrect number of arguments');
            if values[0].isPrimitive then
              raise Exception.Create('Attempt to use a filter ('+arg.Name+') on a primtive type ('+prop.Type_+')');
            if (arg.Name = 'fhirpath') then
              fp.Append(' and '+vl[0].ToString)
            else
            begin
              p := values[0].getPropertyValue(arg.Name);
              if p = nil then
                raise Exception.Create('Attempt to use an unknown filter ('+arg.Name+') on a type ('+prop.Type_+')');
              p.Free;
              fp.Append(' and '+arg.Name+' = '''+vl[0].ToString+'''');
            end;
          finally
            vl.Free;
          end;
        end;
        if fp.Length = 0 then
          for v in values do
          begin
            if passesExtensionMode(v) then
              result.Add(v.Link)
          end
        else
        begin
          node := FPathEngine.parse(fp.ToString.Substring(5));
          try
            for v in values do
             if passesExtensionMode(v) and FPathEngine.evaluateToBoolean(nil, context, v, node) then
               result.Add(v.Link)
          finally
            node.Free;
          end;
        end;
      finally
        fp.Free;
      end;
    end;
    result.link;
  finally
    result.Free;
  end;
end;

function TFHIRGraphQLEngine.filterResources(fhirpath : TGraphQLArgument; bnd : TFHIRBundle): TAdvList<TFHIRResource>;
var
  be : TFhirBundleEntry;
  fpe : TFHIRPathEngine;
  node : TFHIRPathExpressionNode;
begin
  result := TAdvList<TFHIRResource>.create;
  try
    if bnd.entryList.Count > 0 then
    begin
      if (fhirpath = nil) then
        for be in bnd.entryList do
          result.Add(be.resource.Link)
      else
      begin
        fpe := TFHIRPathEngine.Create(nil);
        try
          node := fpe.parse(getSingleValue(fhirpath));
          try
            for be in bnd.entryList do
              if fpe.evaluateToBoolean(nil, be.resource, be.resource, node) then
                result.Add(be.resource.Link)
          finally
            node.Free;
          end;
        finally
          fpe.Free;
        end;
      end;
    end;
    result.link;
  finally
    result.Free;
  end;
end;

function TFHIRGraphQLEngine.filterResources(fhirpath : TGraphQLArgument; list : TAdvList<TFhirResource>): TAdvList<TFHIRResource>;
var
  v : TFHIRResource;
  fpe : TFHIRPathEngine;
  node : TFHIRPathExpressionNode;
begin
  result := TAdvList<TFHIRResource>.create;
  try
    if list.Count > 0 then
    begin
      if (fhirpath = nil) then
        for v in list do
          result.Add(v.Link)
      else
      begin
        fpe := TFHIRPathEngine.Create(nil);
        try
          node := fpe.parse(getSingleValue(fhirpath));
          try
            for v in list do
              if fpe.evaluateToBoolean(nil, v, v, node) then
                result.Add(v.Link)
          finally
            node.Free;
          end;
        finally
          fpe.Free;
        end;
      end;
    end;
    result.link;
  finally
    result.Free;
  end;
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

procedure TFHIRGraphQLEngine.processValues(context : TFHIRResource; sel: TGraphQLSelection; prop: TFHIRProperty; target: TGraphQLObjectValue; values : TAdvList<TFHIRObject>; extensionMode, inheritedList : boolean; suffix : string);
var
  arg: TGraphQLArgument;
  value: TFHIRObject;
  new: TGraphQLObjectValue;
  il : boolean;
  expression : TFHIRPathExpressionNode;
  dir : TGraphQLDirective;
  s, ss : String;
  index : integer;
begin
  il := false;
  arg := nil;
  expression := nil;
  if sel.field.hasDirective('slice') then
  begin
    dir := sel.field.directive('slice');
    s := (dir.Arguments[0].Values[0] as TGraphQLStringValue).Value;
    if (s = '$index') then
      expression := FMagicExpression.Link
    else
      expression := FPathEngine.parse(s);
  end;
  try
    if sel.field.hasDirective('flatten') then // special: instruction to drop this node...
      il := prop.isList and not sel.field.hasDirective('first')
    else if sel.field.hasDirective('first') {or sel.field.hasDirective('last')} then
    begin
      if expression <> nil then
        raise Exception.Create('You cannot mix @slice and @first');
      arg := target.addField(sel.field.Alias+suffix, listStatus(sel.field, inheritedList))
    end
    else if expression = nil then
      arg := target.addField(sel.field.Alias+suffix, listStatus(sel.field, prop.IsList or inheritedList));

    index := 0;
    for value in values do
    begin
      if expression <> nil then
      begin
        if expression = FMagicExpression then
          ss := suffix+'.'+inttostr(index)
        else
          ss := suffix+'.'+FPathEngine.evaluateToString(nil, value, expression);
        if not sel.field.hasDirective('flatten') then
          arg := target.addField(sel.field.Alias+suffix, listStatus(sel.field, prop.IsList or inheritedList));
      end;
      if value.isPrimitive and not extensionMode then
      begin
        if not sel.field.SelectionSet.Empty then
          raise EGraphQLException.Create('Encountered a selection set on a scalar field type');
        processPrimitive(arg, value)
      end
      else
      begin
        if sel.field.SelectionSet.Empty then
          raise EGraphQLException.Create('No Fields selected on a complex object');
        if arg = nil then
          processObject(context, value, target, sel.field.SelectionSet, il, ss)
        else
        begin
          new := TGraphQLObjectValue.Create;
          try
            arg.addValue(new.Link);
            processObject(context, value, new, sel.field.SelectionSet, il, ss);
          finally
            new.Free;
          end;
        end;
      end;
      if sel.field.hasDirective('first') then
        break;
      inc(index);
    end;
  finally
    expression.Free;
  end;
end;

procedure TFHIRGraphQLEngine.processVariables(op : TGraphQLOperation);
var
  varRef : TGraphQLVariable;
  v, varDef : TGraphQLArgument;
begin
  for varRef in op.Variables do
  begin
    varDef := nil;
    for v in FGraphQL.Variables do
      if v.Name = varRef.Name then
        varDef := v;
    if varDef <> nil then
      FWorkingVariables.add(varRef.Name, varDef.Link)// todo: check type?
    else if varRef.DefaultValue <> nil then
      FWorkingVariables.Add(varRef.Name, TGraphQLArgument.Create(varRef.Name, varRef.DefaultValue.Link))
    else
      raise EGraphQLException.Create('No value found for variable ');
  end;
end;

function IsPrimitive(typename : String) : boolean;
begin
  result := StringArrayExistsSensitive(['boolean', 'integer', 'string', 'decimal', 'uri', 'base64Binary', 'instant', 'date', 'dateTime', 'time', 'code', 'oid', 'id', 'markdown', 'unsignedInt', 'positiveInt'], typename);
end;

function isResourceName(name : String; suffix : string) : boolean;
var
  s : String;
begin
  result := false;
  for s in CODES_TFhirResourceType do
    if s + suffix = name then
      exit(true);
end;


procedure TFHIRGraphQLEngine.processObject(context : TFHIRResource; source: TFHIRObject; target: TGraphQLObjectValue; selection: TAdvList<TGraphQLSelection>; inheritedList : boolean; suffix : string);
var
  sel : TGraphQLSelection;
  prop : TFHIRProperty;
  fragment : TGraphQLFragment;
  vl : TAdvList<TFHIRObject>;
begin
  for sel in selection do
  begin
    if (sel.Field <> nil) then
    begin
      if (checkDirectives(sel.Field.Directives)) then
      begin
        prop := source.getPropertyValue(sel.field.Name);
        if (prop = nil) and sel.field.Name.startsWith('_') then
          prop := source.getPropertyValue(sel.field.Name.substring(1));
        if prop = nil then
        begin
          if (sel.field.Name = 'resourceType') and (source is TFHIRResource) then
            target.addField('resourceType', listStatusSingleton).addValue(TGraphQLStringValue.Create(source.fhirType))
          else if (sel.field.Name = 'resource') and (source.fhirType = 'Reference') then
            processReference(context, source, sel.field, target, inheritedList, suffix)
          else if isResourceName(sel.field.Name, 'List') and (source is TFhirResource) then
            processReverseReferenceList(source as TFhirResource, sel.field, target, inheritedList, suffix)
          else if isResourceName(sel.field.Name, 'Connection') and (source is TFhirResource) then
            processReverseReferenceSearch(source as TFhirResource, sel.field, target, inheritedList, suffix)
          else
            raise EGraphQLException.Create('Unknown property '+sel.field.Name+' on '+source.fhirType);
        end
        else
        begin
          try
            if not IsPrimitive(prop.Type_) and sel.field.Name.startsWith('_') then
              raise EGraphQLException.Create('Unknown property '+sel.field.Name+' on '+source.fhirType);

            vl := filter(context, prop, sel.field.Arguments, prop.Values, sel.field.Name.startsWith('_'));
            try
             if not vl.Empty then
               processValues(context, sel, prop, target, vl, sel.field.Name.startsWith('_'), inheritedList, suffix);
            finally
              vl.Free;
            end;
          finally
            prop.Free;
          end;
        end;
      end;
    end
    else if sel.InlineFragment <> nil then
    begin
      if (checkDirectives(sel.InlineFragment.Directives)) then
      begin
        if sel.InlineFragment.TypeCondition = '' then
          raise EGraphQLException.Create('Not done yet - inline fragment with no type condition'); // cause why? why is it even valid?
        if source.fhirType = sel.InlineFragment.TypeCondition then
          processObject(context, source, target, sel.InlineFragment.SelectionSet, inheritedList, suffix);
      end;
    end
    else if checkDirectives(sel.FragmentSpread.Directives) then
    begin
      fragment := FGraphQL.Document.fragment(sel.FragmentSpread.Name);
      if fragment = nil then
        raise EGraphQLException.Create('Unable to resolve fragment '+sel.FragmentSpread.Name);

      if fragment.TypeCondition = '' then
        raise EGraphQLException.Create('Not done yet - inline fragment with no type condition'); // cause why? why is it even valid?
      if source.fhirType = fragment.TypeCondition then
        processObject(context, source, target, fragment.SelectionSet, inheritedList, suffix);
    end;
  end;
end;

procedure TFHIRGraphQLEngine.processPrimitive(arg : TGraphQLArgument; value: TFHIRObject);
var
  s : String;
begin
  s := value.fhirType;
  if (s = 'integer') or (s = 'decimal') or (s = 'unsignedInt') or (s = 'positiveInt') then
    arg.addValue(TGraphQLNumberValue.Create(value.primitiveValue))
  else if (s = 'boolean') then
    arg.addValue(TGraphQLNameValue.Create(value.primitiveValue))
  else
    arg.addValue(TGraphQLStringValue.Create(value.primitiveValue));
end;


procedure TFHIRGraphQLEngine.processReference(context : TFHIRResource; source: TFHIRObject; field: TGraphQLField; target: TGraphQLObjectValue; inheritedList : boolean; suffix : string);
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
    ok := FOnFollowReference(appInfo, context, ref, ctxt, dest);
    if ok then
      try
        if targetTypeOk(field.Arguments, dest) then
        begin
          arg := target.addField(field.Alias, listStatus(field, inheritedList));
          new := TGraphQLObjectValue.Create;
          try
            arg.addValue(new.Link);
            processObject(ctxt, dest, new, field.SelectionSet, inheritedList, suffix);
          finally
            new.Free;
          end;
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

procedure TFHIRGraphQLEngine.processReverseReferenceList(source: TFHIRResource; field: TGraphQLField; target: TGraphQLObjectValue; inheritedList : boolean; suffix : string);
var
  list, vl : TAdvList<TFHIRResource>;
  v : TFhirResource;
  params : TAdvList<TGraphQLArgument>;
  a, arg, parg : TGraphQLArgument;
  new : TGraphQLObjectValue;
begin
  if not assigned(FOnListResources) then
    raise EGraphQLException.Create('Resource Referencing services not provided');
  list := TAdvList<TFhirResource>.create;
  try
    params := TAdvList<TGraphQLArgument>.create;
    try
      parg := nil;
      for a in field.Arguments do
        if (a.Name <> '_reference') then
          params.Add(a.Link)
        else if (parg = nil) then
          parg := a
        else
          raise EGraphQLException.Create('Duplicate parameter _reference');
      if parg = nil then
        raise EGraphQLException.Create('Missing parameter _reference');
      arg := TGraphQLArgument.Create;
      params.Add(arg);
      arg.Name := getSingleValue(parg);
      arg.addValue(TGraphQLStringValue.Create(source.fhirType+'/'+source.id));
      FOnListResources(FAppinfo, field.Name.Substring(0, field.Name.Length - 4), params, list);
    finally
      params.Free;
    end;
    arg := nil;
    new := nil;

    vl := filterResources(field.argument('fhirpath'), list);
    try
      if not vl.Empty then
      begin
        arg := target.addField(field.Alias, listStatus(field, true));
        for v in vl do
        begin
          new := TGraphQLObjectValue.Create;
          try
            arg.addValue(new.Link);
            processObject(v, v, new, field.SelectionSet, inheritedList, suffix);
          finally
            new.Free;
          end;
        end;
      end;
    finally
      vl.Free;
    end;
  finally
    list.Free;
  end;
end;

procedure TFHIRGraphQLEngine.processReverseReferenceSearch(source: TFHIRResource; field: TGraphQLField; target: TGraphQLObjectValue; inheritedList : boolean; suffix : string);
var
  bnd : TFHIRBundle;
  bndWrapper : TFHIRGraphQLSearchWrapper;
  params : TAdvList<TGraphQLArgument>;
  a, arg, parg : TGraphQLArgument;
  new : TGraphQLObjectValue;
begin
  if not assigned(FOnSearch) then
    raise EGraphQLException.Create('Resource Referencing services not provided');
  params := TAdvList<TGraphQLArgument>.create;
  try
    parg := nil;
    for a in field.Arguments do
      if (a.Name <> '_reference') then
        params.Add(a.Link)
      else if (parg = nil) then
        parg := a
      else
        raise EGraphQLException.Create('Duplicate parameter _reference');
    if parg = nil then
      raise EGraphQLException.Create('Missing parameter _reference');
    arg := TGraphQLArgument.Create;
    params.Add(arg);
    arg.Name := getSingleValue(parg);
    arg.addValue(TGraphQLStringValue.Create(source.fhirType+'/'+source.id));
    bnd := FOnSearch(FAppinfo, field.Name.Substring(0, field.Name.Length-10), params);
    try
      bndWrapper := TFHIRGraphQLSearchWrapper.create(bnd.link);
      try
        arg := target.addField(field.Alias, listStatus(field, false));
        new := TGraphQLObjectValue.Create;
        arg.addValue(new);
        processObject(nil, bndWrapper, new, field.SelectionSet, inheritedList, suffix);
      finally
        bndWrapper.Free;
      end;
    finally
      bnd.Free;
    end;
  finally
    params.Free;
  end;
end;

procedure TFHIRGraphQLEngine.processSearch(target: TGraphQLObjectValue; selection: TAdvList<TGraphQLSelection>; inheritedList : boolean; suffix : string);
var
  sel : TGraphQLSelection;
begin
  for sel in selection do
  begin
    if (sel.Field = nil) then
      raise EGraphQLException.Create('Only field selections are allowed in this context');
    checkNoDirectives(sel.Field.Directives);

    if (isResourceName(sel.Field.Name, '')) then
      processSearchSingle(target, sel.Field, inheritedList, suffix)
    else if (isResourceName(sel.Field.Name, 'List')) then
      processSearchSimple(target, sel.Field, inheritedList, suffix)
    else if (isResourceName(sel.Field.Name, 'Connection')) then
      processSearchFull(target, sel.Field, inheritedList, suffix);
  end;
end;

procedure TFHIRGraphQLEngine.processSearchSingle(target: TGraphQLObjectValue; field: TGraphQLField; inheritedList : boolean; suffix : string);
var
  id : String;
  arg : TGraphQLArgument;
  res : TFhirResource;
  new : TGraphQLObjectValue;
begin
  if not assigned(FOnLookup) then
    raise EGraphQLException.Create('Resource Referencing services not provided');
  id := '';
  for arg in field.Arguments do
    if (arg.Name = 'id') then
      id := getSingleValue(arg)
    else
      raise EGraphQLException.Create('Unknown/invalid parameter '+arg.Name);
  if (id = '') then
    raise EGraphQLException.Create('No id found');
  if not FOnLookup(FAppinfo, field.Name, id, res) then
    raise EGraphQLException.Create('Resource '+field.Name+'/'+id+' not found');
  try
    arg := target.addField(field.Alias, listStatus(field, false));
    new := TGraphQLObjectValue.Create;
    try
      arg.addValue(new.Link);
      processObject(res, res, new, field.SelectionSet, inheritedList, suffix);
    finally
      new.Free;
    end;
  finally
    res.Free;
  end;
end;

procedure TFHIRGraphQLEngine.processSearchSimple(target: TGraphQLObjectValue; field: TGraphQLField; inheritedList : boolean; suffix : string);
var
  list, vl : TAdvList<TFHIRResource>;
  v : TFhirResource;
  arg : TGraphQLArgument;
  new : TGraphQLObjectValue;
begin
  if not assigned(FOnListResources) then
    raise EGraphQLException.Create('Resource Referencing services not provided');
  list := TAdvList<TFhirResource>.create;
  try
    FOnListResources(FAppinfo, field.Name.Substring(0, field.Name.Length - 4), field.Arguments, list);
    arg := nil;
    new := nil;

    vl := filterResources(field.argument('fhirpath'), list);
    try
      if not vl.Empty then
      begin
        arg := target.addField(field.Alias, listStatus(field, true));
        for v in vl do
        begin
          new := TGraphQLObjectValue.Create;
          try
            arg.addValue(new.Link);
            processObject(v, v, new, field.SelectionSet, inheritedList, suffix);
          finally
            new.Free;
          end;
        end;
      end;
    finally
      vl.Free;
    end;
  finally
    list.Free;
  end;
end;

procedure TFHIRGraphQLEngine.processSearchFull(target: TGraphQLObjectValue; field: TGraphQLField; inheritedList : boolean; suffix : string);
var
  bnd : TFHIRBundle;
  bndWrapper : TFHIRGraphQLSearchWrapper;
  arg, carg : TGraphQLArgument;
  new : TGraphQLObjectValue;
  params : TAdvList<TGraphQLArgument>;
  l,r: String;
begin
  if not assigned(FOnSearch) then
    raise EGraphQLException.Create('Resource Referencing services not provided');
  params := TAdvList<TGraphQLArgument>.create;
  try
    carg := nil;
    for arg in field.Arguments do
      if arg.Name = 'cursor' then
        carg := arg
      else
        params.Add(arg.Link);
    if (carg <> nil) then
    begin
      params.Clear;
      StringSplit(getSingleValue(carg), ':', l, r);
      params.Add(TGraphQLArgument.Create('search-id', TGraphQLStringValue.Create(l)));
      params.Add(TGraphQLArgument.Create('search-offset', TGraphQLStringValue.Create(r)));
    end;

    bnd := FOnSearch(FAppinfo, field.Name.Substring(0, field.Name.Length-10), params);
    try
      bndWrapper := TFHIRGraphQLSearchWrapper.create(bnd.link);
      try
        arg := target.addField(field.Alias, listStatus(field, false));
        new := TGraphQLObjectValue.Create;
        arg.addValue(new);
        processObject(nil, bndWrapper, new, field.SelectionSet, inheritedList, suffix);
      finally
        bndWrapper.Free;
      end;
    finally
      bnd.Free;
    end;
  finally
    params.Free;
  end;
end;

{ TFHIRGraphQLSearchWrapper }

constructor TFHIRGraphQLSearchWrapper.Create(bundle : TFhirBundle);
var
  s : String;
begin
  inherited Create;
  FBundle := bundle;
  s := bundle.link_List.Matches['self'];
  FParseMap := TParseMap.create(s.Substring(s.IndexOf('?')+1));
end;

destructor TFHIRGraphQLSearchWrapper.Destroy;
begin
  FParseMap.free;
  FBundle.Free;
  inherited;
end;

function TFHIRGraphQLSearchWrapper.extractLink(name: String): TFhirString;
var
  s : String;
  pm : TParseMap;
begin
  s := FBundle.link_List.Matches[name];
  if s = '' then
    result := nil
  else
  begin
    pm := TParseMap.create(s.Substring(s.IndexOf('?')+1));
    try
      result := TFhirString.Create(pm.GetVar('search-id')+':'+pm.GetVar('search-offset'));
    finally
      pm.Free;
    end;
  end;
end;

function TFHIRGraphQLSearchWrapper.extractParam(name: String; int : boolean): TFhirObject;
var
  s : String;
begin
  s := FParseMap.GetVar(name);
  if s = '' then
    result := nil
  else if int then
    result := TFhirInteger.Create(s)
  else
    result := TFhirString.Create(s);
end;

function TFHIRGraphQLSearchWrapper.fhirType: String;
begin
  result := '*Connection';
end;

  // http://test.fhir.org/r3/Patient?_format=text/xhtml&search-id=77c97e03-8a6c-415f-a63d-11c80cf73f&&active=true&_sort=_id&search-offset=50&_count=50

function TFHIRGraphQLSearchWrapper.getPropertyValue(propName: string): TFHIRProperty;
var
  list : TAdvList<TFHIRGraphQLSearchEdge>;
  be : TFHIRBundleEntry;
begin
  if propName = 'first' then
    result := TFHIRProperty.Create(self, propname, 'string', false, TFhirString, extractLink('first'))
  else if propName = 'previous' then
    result := TFHIRProperty.Create(self, propname, 'string', false, TFhirString, extractLink('previous'))
  else if propName = 'next' then
    result := TFHIRProperty.Create(self, propname, 'string', false, TFhirString, extractLink('next'))
  else if propName = 'last' then
    result := TFHIRProperty.Create(self, propname, 'string', false, TFhirString, extractLink('last'))
  else if propName = 'count' then
    result := TFHIRProperty.Create(self, propname, 'integer', false, TFhirString, FBundle.totalElement.link)
  else if propName = 'offset' then
    result := TFHIRProperty.Create(self, propname, 'integer', false, TFhirInteger, extractParam('search-offset', true))
  else if propName = 'pagesize' then
    result := TFHIRProperty.Create(self, propname, 'integer', false, TFhirInteger, extractParam('_count', true))
  else if propName = 'edges' then
  begin
    list := TAdvList<TFHIRGraphQLSearchEdge>.create;
    try
      for be in FBundle.entryList do
        list.Add(TFHIRGraphQLSearchEdge.create(be.Link));
      result := TFHIRProperty.Create(self, propname, 'edge', true, TFhirInteger, TAdvList<TFHIRObject>(list));
    finally
      list.Free;
    end;
  end
  else
    result := nil;
end;

procedure TFHIRGraphQLSearchWrapper.SetBundle(const Value: TFhirBundle);
begin
  FBundle.Free;
  FBundle := Value;
end;

{ TFHIRGraphQLSearchEdge }

constructor TFHIRGraphQLSearchEdge.Create(entry: TFhirBundleEntry);
begin
  inherited Create;
  FEntry := entry;
end;

destructor TFHIRGraphQLSearchEdge.Destroy;
begin
  FEntry.Free;
  inherited;
end;

function TFHIRGraphQLSearchEdge.fhirType: String;
begin
  result := '*Edge';
end;

function TFHIRGraphQLSearchEdge.getPropertyValue(propName: string): TFHIRProperty;
begin
  if propName = 'mode' then
  begin
    if FEntry.search <> nil then
      result := TFHIRProperty.Create(self, propname, 'code', false, TFhirEnum, FEntry.search.modeElement.Link)
    else
      result := TFHIRProperty.Create(self, propname, 'code', false, TFhirEnum, TFHIRObject(nil));
  end
  else if propName = 'score' then
  begin
    if FEntry.search <> nil then
      result := TFHIRProperty.Create(self, propname, 'decimal', false, TFhirDecimal, FEntry.search.scoreElement.Link)
    else
      result := TFHIRProperty.Create(self, propname, 'decimal', false, TFhirDecimal, TFHIRObject(nil));
  end
  else if propName = 'resource' then
    result := TFHIRProperty.Create(self, propname, 'resource', false, TFhirResource, FEntry.resource.Link)
  else
    result := nil;
end;

procedure TFHIRGraphQLSearchEdge.SetEntry(const Value: TFhirBundleEntry);
begin
  FEntry.Free;
  FEntry := value;
end;

function TFHIRGraphQLEngine.getSingleValue(arg: TGraphQLArgument): string;
var
  vl : TAdvList<TGraphQLValue>;
begin
  vl := resolveValues(arg, 1);
  try
    if vl.Count = 0 then
      exit('');
    result := vl[0].ToString;
  finally
    vl.Free;
  end;
end;

function TFHIRGraphQLEngine.resolveValues(arg: TGraphQLArgument; max: integer; vars : String): TAdvList<TGraphQLValue>;
var
  v : TGraphQLValue;
  a : TGraphQLArgument;
  vl : TAdvList<TGraphQLValue>;
begin
  result := TAdvList<TGraphQLValue>.create;
  try
    for v in arg.Values do
      if not (v is TGraphQLVariableValue) then
        result.Add(v.Link)
      else
      begin
        if vars.Contains(':'+v.ToString+':') then
          raise EGraphQLException.Create('Recursive reference to variable '+v.ToString);
        if FWorkingVariables.TryGetValue(v.ToString, a) then
        begin
          vl := resolveValues(a, -1, vars+':'+v.ToString+':');
          try
            result.AddAll(vl);
          finally
            vl.Free;
          end;
        end
        else
          raise EGraphQLException.Create('No value found for variable "'+v.ToString+'" in "'+arg.Name+'"');
      end;
    if (max <> -1) and (result.Count > max) then
      raise EGraphQLException.Create('Only '+integer.ToString(max)+' values are allowed for "'+arg.Name+'", but '+inttostr(result.Count)+' enoucntered');
    result.link;
  finally
    result.free;
  end;
end;


end.
