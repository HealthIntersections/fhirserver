unit fhir_graphql;

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

{$I fhir.inc}

interface

uses
  SysUtils, Classes,
  fsl_utilities, fsl_base,
  fsl_http, fsl_graphql,
  fhir_objects, fhir_common, fhir_pathengine, fhir_factory;

type
  TFHIRGraphQLEngineDereferenceEvent = function(appInfo : TFslObject; context : TFHIRResourceV; reference : TFHIRObject; out targetContext, target : TFHIRResourceV) : boolean of Object;
  TFHIRGraphQLEngineLookupEvent = function (appInfo : TFslObject; requestType, id : String; var res : TFHIRResourceV) : boolean of Object;
  TFHIRGraphQLEngineListResourcesEvent = procedure (appInfo : TFslObject; requestType: String; params : TFslList<TGraphQLArgument>; list : TFslList<TFHIRResourceV>) of Object;
  TFHIRGraphQLEngineSearchEvent = function (appInfo : TFslObject; requestType: String; params : TFslList<TGraphQLArgument>) : TFHIRBundleW of Object;

  TFHIRGraphQLEngine = class (TFslObject)
  private
    FOnFollowReference : TFHIRGraphQLEngineDereferenceEvent;
    FOnSearch : TFHIRGraphQLEngineSearchEvent;
    FOnListResources : TFHIRGraphQLEngineListResourcesEvent;
    FOnLookup : TFHIRGraphQLEngineLookupEvent;

    FFactory : TFHIRFactory;
    FFocus : TFHIRResourceV;
    FOutput : TGraphQLObjectValue;
    FGraphQL : TGraphQLPackage;
    FAppinfo : TFslObject;
    FWorkingVariables: TFslMap<TGraphQLArgument>;
    FPathEngine : TFHIRPathEngineV;
    FMagicExpression : TFHIRPathExpressionNodeV;

    procedure SetGraphQL(const Value: TGraphQLPackage);
    procedure SetFocus(const Value: TFHIRResourceV);

    function listStatus(field: TGraphQLField; isList: boolean): TGraphQLArgumentListStatus;

    function getSingleValue(arg : TGraphQLArgument) : string;
    function resolveValues(arg : TGraphQLArgument; max : integer = -1; vars : String = '') : TFslList<TGraphQLValue>;

    function checkBooleanDirective(dir : TGraphQLDirective) : Boolean;
    function checkDirectives(directives : TFslList<TGraphQLDirective>) : boolean;
    procedure checkNoDirectives(directives : TFslList<TGraphQLDirective>);
    function hasArgument(arguments : TFslList<TGraphQLArgument>; name, value : String) : boolean;
    function targetTypeOk(arguments : TFslList<TGraphQLArgument>; dest : TFHIRResourceV) : boolean;

    function filter(context : TFHIRResourceV; prop:TFHIRProperty; arguments : TFslList<TGraphQLArgument>; values : TFHIRObjectList; extensionMode : boolean)  : TFslList<TFHIRObject>;
//    function filterResources(FHIRPathEngine : TGraphQLArgument; bnd : TFHIRBundleW)  : TFslList<TFHIRResourceV>; overload;
    function filterResources(FHIRPathEngine : TGraphQLArgument; list : TFslList<TFHIRResourceV>)  : TFslList<TFHIRResourceV>; overload;
    procedure processObject(context : TFHIRResourceV; source : TFHIRObject; target : TGraphQLObjectValue; selection : TFslList<TGraphQLSelection>; inheritedList : boolean; suffix : string);
    procedure processPrimitive(arg : TGraphQLArgument; value : TFHIRObject);
    procedure processReference(context : TFHIRResourceV; source : TFHIRObject; field : TGraphQLField; target : TGraphQLObjectValue; inheritedList : boolean; suffix : string);
    procedure processReverseReferenceList(source : TFHIRResourceV; field : TGraphQLField; target : TGraphQLObjectValue; inheritedList : boolean; suffix : string);
    procedure processReverseReferenceSearch(source : TFHIRResourceV; field : TGraphQLField; target : TGraphQLObjectValue; inheritedList : boolean; suffix : string);
    procedure processValues(context : TFHIRResourceV; sel: TGraphQLSelection; prop: TFHIRProperty; target: TGraphQLObjectValue; values : TFslList<TFHIRObject>; extensionMode, inheritedList : boolean; suffix : string);
    procedure processSearch(target : TGraphQLObjectValue; selection : TFslList<TGraphQLSelection>; inheritedList : boolean; suffix : string);
    procedure processSearchSingle(target : TGraphQLObjectValue; field : TGraphQLField; inheritedList : boolean; suffix : string);
    procedure processSearchSimple(target : TGraphQLObjectValue; field : TGraphQLField; inheritedList : boolean; suffix : string);
    procedure processSearchFull(target : TGraphQLObjectValue; field : TGraphQLField; inheritedList : boolean; suffix : string);
    procedure SetAppInfo(const Value: TFslObject);
    procedure processVariables(op : TGraphQLOperation);
    function isResourceName(name, suffix: string): boolean;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(factory : TFHIRFactory);
    destructor Destroy; override;
    property appInfo : TFslObject read FAppinfo write SetAppInfo;

    // the focus resource - if there is one. If there isn't, then the focus is a collection
    property focus : TFHIRResourceV read FFocus write SetFocus;

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
    FFactory : TFHIRFactory;
    FBundle: TFHIRBundleW;
    FParseMap : THTTPParameters;
    procedure SetBundle(const Value: TFHIRBundleW);
    function extractLink(name : String) : TFhirObject;
    function extractParam(name : String; int : boolean) : TFhirObject;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(factory : TFHIRFactory; bundle : TFHIRBundleW);
    destructor Destroy; override;

    property Bundle : TFHIRBundleW read FBundle write SetBundle;
    function fhirType : String; override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function hasExtensions : boolean; override;

    function getPropertyValue(propName : string): TFHIRProperty; override;
  end;

  TFHIRGraphQLSearchEdge = class (TFHIRObject)
  private
    FFactory : TFHIRFactory;
    FEntry: TFHIRBundleEntryW;
    procedure SetEntry(const Value: TFHIRBundleEntryW);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(factory : TFHIRFactory; entry : TFHIRBundleEntryW);
    destructor Destroy; override;

    property Entry : TFHIRBundleEntryW read FEntry write SetEntry;
    function fhirType : String; override;

    function createPropertyValue(propName : string): TFHIRObject; override;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function hasExtensions : boolean; override;

    function getPropertyValue(propName : string): TFHIRProperty; override;
  end;

implementation

{ TFHIRGraphQLEngine }

function TFHIRGraphQLEngine.checkBooleanDirective(dir: TGraphQLDirective): Boolean;
var
  vl : TFslList<TGraphQLValue>;
begin
  if dir.Arguments.Count <> 1 then
    raise EJsonException.Create('Unable to process @'+dir.Name+': expected a single argument "if"');
  if dir.Arguments[0].Name <> 'if' then
    raise EJsonException.Create('Unable to process @'+dir.Name+': expected a single argument "if"');
  vl := resolveValues(dir.Arguments[0], 1);
  try
    result := vl[0].ToString = 'true';
  finally
    vl.Free;
  end;
end;

function TFHIRGraphQLEngine.checkDirectives(directives: TFslList<TGraphQLDirective>) : boolean;
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
        raise EJsonException.Create('Duplicate @skip directives');
    end
    else if dir.Name = 'include' then
    begin
      if (include = nil) then
        include := dir
      else
        raise EJsonException.Create('Duplicate @include directives');
    end
    else if not StringArrayExistsSensitive(['flatten', 'first', 'singleton', 'slice'], dir.Name) then
      raise EJsonException.Create('Directive "'+dir.Name+'" is not recognised');
  end;
  if (skip <> nil) and (include <> nil) then
    raise EJsonException.Create('Cannot mix @skip and @include directives');
  if skip <> nil then
    result := not checkBooleanDirective(skip)
  else if include <> nil then
    result := checkBooleanDirective(include)
  else
    result := true;
end;

procedure TFHIRGraphQLEngine.checkNoDirectives(
  directives: TFslList<TGraphQLDirective>);
begin

end;

constructor TFHIRGraphQLEngine.Create(factory : TFHIRFactory);
begin
  inherited Create;
  FFactory := Factory.link;
  FWorkingVariables := TFslMap<TGraphQLArgument>.create('graphql');
  FPathEngine := factory.makePathEngine(nil, nil);
  FMagicExpression := FPathEngine.parseV('0');
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
  FFactory.Free;
  inherited;
end;

procedure TFHIRGraphQLEngine.SetAppInfo(const Value: TFslObject);
begin
  FAppinfo.Free;
  FAppinfo := Value;
end;

procedure TFHIRGraphQLEngine.SetGraphQL(const Value: TGraphQLPackage);
begin
  FGraphQL.Free;
  FGraphQL := Value;
end;

procedure TFHIRGraphQLEngine.SetFocus(const Value: TFHIRResourceV);
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

function TFHIRGraphQLEngine.targetTypeOk(arguments: TFslList<TGraphQLArgument>; dest: TFHIRResourceV): boolean;
var
  list : TStringList;
  arg : TGraphQLArgument;
  vl : TFslList<TGraphQLValue>;
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
    raise EFHIRException.create('Unable to process graphql - graphql document missing');

  FOutput.Free;
  FOutput := TGraphQLObjectValue.Create;

  // todo: initial conditions
  if GraphQL.OperationName <> '' then
  begin
    op := GraphQL.Document.operation(GraphQL.OperationName);
    if op = nil then
      raise EJsonException.Create('Unable to find operation "'+GraphQL.OperationName+'"');
  end
  else if (GraphQL.Document.Operations.Count = 1) then
    op := GraphQL.Document.Operations[0]
  else
    raise EJsonException.Create('No operation name provided, so expected to find a single operation');

  if op.operationType = qglotMutation then
    raise EJsonException.Create('Mutation operations are not supported');

  checkNoDirectives(op.Directives);
  processVariables(op);
  if FFocus = nil then
    processSearch(FOutput, op.SelectionSet, false, '')
  else
    processObject(FFocus, FFocus, FOutput, op.SelectionSet, false, '');
end;

function TFHIRGraphQLEngine.filter(context : TFHIRResourceV; prop:TFHIRProperty; arguments: TFslList<TGraphQLArgument>; values: TFHIRObjectList; extensionMode : boolean): TFslList<TFHIRObject>;
  function passesExtensionMode(obj : TFHIRObject) : boolean;
  begin
    if not obj.isPrimitive then
      result := not extensionMode
    else if extensionMode then
      result := (obj.getId <> '') or (obj.extensionCount('') > 0)
    else
      result := obj.primitiveValue <> '';
  end;
var
  fp : TStringBuilder;
  arg : TGraphQLArgument;
  p : TFHIRProperty;
  v : TFHIRObject;
  node: TFHIRPathExpressionNodeV;
  vl : TFslList<TGraphQLValue>;
  i, t, offset, count : integer;
begin
  offset := 0;
  count := MAXINT;

  result := TFslList<TFHIRObject>.create;
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
              raise EFHIRException.create('Incorrect number of arguments');
            if values[0].isPrimitive then
              raise EFHIRException.create('Attempt to use a filter ('+arg.Name+') on a primtive type ('+prop.Type_+')');
            if (arg.Name = 'fhirpath') then
              fp.Append(' and '+vl[0].ToString)
            else if (arg.Name = '_offset') then
              offset := StrToInt(vl[0].ToString)
            else if (arg.Name = '_count') then
              count := StrToInt(vl[0].ToString)
            else
            begin
              p := values[0].getPropertyValue(arg.Name);
              if p = nil then
                raise EFHIRException.create('Attempt to use an unknown filter ('+arg.Name+') on a type ('+prop.Type_+')');
              p.Free;
              fp.Append(' and '+arg.Name+' = '''+vl[0].ToString+'''');
            end;
          finally
            vl.Free;
          end;
        end;
        if fp.Length = 0 then
        begin
          i := 0;
          t := 0;
          for v in values do
          begin
            if (i >= offset) and passesExtensionMode(v) then
            begin
              result.Add(v.Link);
              inc(t);
              if (t >= count) then
               break;
            end;
            inc(i);
          end
        end
        else
        begin
          node := FPathEngine.parseV(fp.ToString.Substring(5));
          try
            i := 0;
            t := 0;
            for v in values do
            begin
              if (i >= offset) and passesExtensionMode(v) and FPathEngine.evaluateToBoolean(nil, context, v, node) then
              begin
                result.Add(v.Link);
                inc(t);
                if (t >= count) then
                  break;
              end;
              inc(i);
            end;
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

//function TFHIRGraphQLEngine.filterResources(FHIRPathEngine : TGraphQLArgument; bnd : TFHIRBundleW): TFslList<TFHIRResourceV>;
//var
//  be : TFHIRBundleEntryW;
//  node : TFHIRPathExpressionNodeV;
//  bel : TFslList<TFHIRBundleEntryW>;
//begin
//  result := TFslList<TFHIRResourceV>.create;
//  try
//    bel := bnd.entries;
//    try
//      if bel.Count > 0 then
//      begin
//        if (FHIRPathEngine = nil) then
//          for be in bel do
//            result.Add(be.resource.Link)
//        else
//        begin
//          node := FPathEngine.parseV(getSingleValue(FHIRPathEngine));
//          try
//            for be in bel do
//              if FPathEngine.evaluateToBoolean(nil, be.resource, be.resource, node) then
//                result.Add(be.resource.Link)
//          finally
//            node.Free;
//          end;
//        end;
//      end;
//    finally
//      bel.Free;
//    end;
//    result.link;
//  finally
//    result.Free;
//  end;
//end;

function TFHIRGraphQLEngine.filterResources(FHIRPathEngine : TGraphQLArgument; list : TFslList<TFHIRResourceV>): TFslList<TFHIRResourceV>;
var
  v : TFHIRResourceV;
  node : TFHIRPathExpressionNodeV;
begin
  result := TFslList<TFHIRResourceV>.create;
  try
    if list.Count > 0 then
    begin
      if (FHIRPathEngine = nil) then
        for v in list do
          result.Add(v.Link)
      else
      begin
        node := FPathEngine.parseV(getSingleValue(FHIRPathEngine));
        try
          for v in list do
            if FPathEngine.evaluateToBoolean(nil, v, v, node) then
              result.Add(v.Link)
        finally
          node.Free;
        end;
      end;
    end;
    result.link;
  finally
    result.Free;
  end;
end;



function TFHIRGraphQLEngine.hasArgument(arguments: TFslList<TGraphQLArgument>; name, value: String): boolean;
var
  arg : TGraphQLArgument;
begin
  for arg in arguments do
    if (arg.Name = name) and arg.hasValue(value) then
      exit(true);
  result := false;
end;

procedure TFHIRGraphQLEngine.processValues(context : TFHIRResourceV; sel: TGraphQLSelection; prop: TFHIRProperty; target: TGraphQLObjectValue; values : TFslList<TFHIRObject>; extensionMode, inheritedList : boolean; suffix : string);
var
  arg: TGraphQLArgument;
  value: TFHIRObject;
  new: TGraphQLObjectValue;
  il : boolean;
  expression : TFHIRPathExpressionNodeV;
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
      expression := FPathEngine.parseV(s);
  end;
  try
    if sel.field.hasDirective('flatten') then // special: instruction to drop this node...
      il := prop.isList and not sel.field.hasDirective('first')
    else if sel.field.hasDirective('first') {or sel.field.hasDirective('last')} then
    begin
      if expression <> nil then
        raise EFHIRException.create('You cannot mix @slice and @first');
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
          raise EJsonException.Create('Encountered a selection set on a scalar field type');
        processPrimitive(arg, value)
      end
      else
      begin
        if sel.field.SelectionSet.Empty then
          raise EJsonException.Create('No Fields selected on a complex object');
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
      raise EJsonException.Create('No value found for variable ');
  end;
end;

function IsPrimitive(typename : String) : boolean;
begin
  result := StringArrayExistsSensitive(['boolean', 'integer', 'string', 'decimal', 'uri', 'base64Binary', 'instant', 'date', 'dateTime', 'time', 'code', 'oid', 'id', 'markdown', 'unsignedInt', 'positiveInt'], typename);
end;

function TFHIRGraphQLEngine.isResourceName(name : String; suffix : string) : boolean;
var
  s : String;
begin
  result := false;
  for s in FFactory.ResourceNames do
    if s + suffix = name then
      exit(true);
end;


procedure TFHIRGraphQLEngine.processObject(context : TFHIRResourceV; source: TFHIRObject; target: TGraphQLObjectValue; selection: TFslList<TGraphQLSelection>; inheritedList : boolean; suffix : string);
var
  sel : TGraphQLSelection;
  prop : TFHIRProperty;
  fragment : TGraphQLFragment;
  vl : TFslList<TFHIRObject>;
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
          if (sel.field.Name = 'resourceType') and (source is TFHIRResourceV) then
            target.addField('resourceType', listStatusSingleton).addValue(TGraphQLStringValue.Create(source.fhirType))
          else if (sel.field.Name = 'resource') and (source.fhirType = 'Reference') then
            processReference(context, source, sel.field, target, inheritedList, suffix)
          else if isResourceName(sel.field.Name, 'List') and (source is TFHIRResourceV) then
            processReverseReferenceList(source as TFHIRResourceV, sel.field, target, inheritedList, suffix)
          else if isResourceName(sel.field.Name, 'Connection') and (source is TFHIRResourceV) then
            processReverseReferenceSearch(source as TFHIRResourceV, sel.field, target, inheritedList, suffix)
          else
            raise EJsonException.Create('Unknown property '+sel.field.Name+' on '+source.fhirType);
        end
        else
        begin
          try
            if not IsPrimitive(prop.Type_) and sel.field.Name.startsWith('_') then
              raise EJsonException.Create('Unknown property '+sel.field.Name+' on '+source.fhirType);

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
          raise EJsonException.Create('Not done yet - inline fragment with no type condition'); // cause why? why is it even valid?
        if source.fhirType = sel.InlineFragment.TypeCondition then
          processObject(context, source, target, sel.InlineFragment.SelectionSet, inheritedList, suffix);
      end;
    end
    else if checkDirectives(sel.FragmentSpread.Directives) then
    begin
      fragment := FGraphQL.Document.fragment(sel.FragmentSpread.Name);
      if fragment = nil then
        raise EJsonException.Create('Unable to resolve fragment '+sel.FragmentSpread.Name);

      if fragment.TypeCondition = '' then
        raise EJsonException.Create('Not done yet - inline fragment with no type condition'); // cause why? why is it even valid?
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


procedure TFHIRGraphQLEngine.processReference(context : TFHIRResourceV; source: TFHIRObject; field: TGraphQLField; target: TGraphQLObjectValue; inheritedList : boolean; suffix : string);
var
  ok : boolean;
  ctxt, dest : TFHIRResourceV;
  arg: TGraphQLArgument;
  new : TGraphQLObjectValue;
  prop : TFHIRProperty;
begin
  if not (source.fhirType = 'Reference') then
    raise EJsonTodo.create('TFHIRGraphQLEngine.processReference');
  if not assigned(FOnFollowReference) then
    raise EJsonException.Create('Resource Referencing services not provided');

  prop := source.getPropertyValue('reference');
  try
    ok := FOnFollowReference(appInfo, context, source, ctxt, dest);
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
      raise EJsonException.Create('Unable to resolve reference to '+prop.Values[0].primitiveValue);
  finally
    prop.Free;
  end;
end;

procedure TFHIRGraphQLEngine.processReverseReferenceList(source: TFHIRResourceV; field: TGraphQLField; target: TGraphQLObjectValue; inheritedList : boolean; suffix : string);
var
  list, vl : TFslList<TFHIRResourceV>;
  v : TFHIRResourceV;
  params : TFslList<TGraphQLArgument>;
  a, arg, parg : TGraphQLArgument;
  new : TGraphQLObjectValue;
begin
  if not assigned(FOnListResources) then
    raise EJsonException.Create('Resource Referencing services not provided');
  list := TFslList<TFHIRResourceV>.create;
  try
    params := TFslList<TGraphQLArgument>.create;
    try
      parg := nil;
      for a in field.Arguments do
        if (a.Name <> '_reference') then
          params.Add(a.Link)
        else if (parg = nil) then
          parg := a
        else
          raise EJsonException.Create('Duplicate parameter _reference');
      if parg = nil then
        raise EJsonException.Create('Missing parameter _reference');
      arg := TGraphQLArgument.Create;
      params.Add(arg);
      arg.Name := getSingleValue(parg);
      arg.addValue(TGraphQLStringValue.Create(source.fhirType+'/'+source.id));
      FOnListResources(FAppinfo, field.Name.Substring(0, field.Name.Length - 4), params, list);
    finally
      params.Free;
    end;

    vl := filterResources(field.argument('FHIRPathEngine'), list);
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

procedure TFHIRGraphQLEngine.processReverseReferenceSearch(source: TFHIRResourceV; field: TGraphQLField; target: TGraphQLObjectValue; inheritedList : boolean; suffix : string);
var
  bnd : TFHIRBundleW;
  bndWrapper : TFHIRGraphQLSearchWrapper;
  params : TFslList<TGraphQLArgument>;
  a, arg, parg : TGraphQLArgument;
  new : TGraphQLObjectValue;
begin
  if not assigned(FOnSearch) then
    raise EJsonException.Create('Resource Referencing services not provided');
  params := TFslList<TGraphQLArgument>.create;
  try
    parg := nil;
    for a in field.Arguments do
      if (a.Name <> '_reference') then
        params.Add(a.Link)
      else if (parg = nil) then
        parg := a
      else
        raise EJsonException.Create('Duplicate parameter _reference');
    if parg = nil then
      raise EJsonException.Create('Missing parameter _reference');
    arg := TGraphQLArgument.Create;
    params.Add(arg);
    arg.Name := getSingleValue(parg);
    arg.addValue(TGraphQLStringValue.Create(source.fhirType+'/'+source.id));
    bnd := FOnSearch(FAppinfo, field.Name.Substring(0, field.Name.Length-10), params);
    try
      bndWrapper := TFHIRGraphQLSearchWrapper.create(FFactory.link, bnd.link);
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

procedure TFHIRGraphQLEngine.processSearch(target: TGraphQLObjectValue; selection: TFslList<TGraphQLSelection>; inheritedList : boolean; suffix : string);
var
  sel : TGraphQLSelection;
begin
  for sel in selection do
  begin
    if (sel.Field = nil) then
      raise EJsonException.Create('Only field selections are allowed in this context');
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
  res : TFHIRResourceV;
  new : TGraphQLObjectValue;
begin
  if not assigned(FOnLookup) then
    raise EJsonException.Create('Resource Referencing services not provided');
  id := '';
  for arg in field.Arguments do
    if (arg.Name = 'id') then
      id := getSingleValue(arg)
    else
      raise EJsonException.Create('Unknown/invalid parameter '+arg.Name);
  if (id = '') then
    raise EJsonException.Create('No id found');
  if not FOnLookup(FAppinfo, field.Name, id, res) then
    raise EJsonException.Create('Resource '+field.Name+'/'+id+' not found');
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
  list, vl : TFslList<TFHIRResourceV>;
  v : TFHIRResourceV;
  arg : TGraphQLArgument;
  new : TGraphQLObjectValue;
begin
  if not assigned(FOnListResources) then
    raise EJsonException.Create('Resource Referencing services not provided');
  list := TFslList<TFHIRResourceV>.create;
  try
    FOnListResources(FAppinfo, field.Name.Substring(0, field.Name.Length - 4), field.Arguments, list);

    vl := filterResources(field.argument('FHIRPathEngine'), list);
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
  bnd : TFHIRBundleW;
  bndWrapper : TFHIRGraphQLSearchWrapper;
  arg, carg : TGraphQLArgument;
  new : TGraphQLObjectValue;
  params : TFslList<TGraphQLArgument>;
  l,r: String;
begin
  if not assigned(FOnSearch) then
    raise EJsonException.Create('Resource Referencing services not provided');
  params := TFslList<TGraphQLArgument>.create;
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
      bndWrapper := TFHIRGraphQLSearchWrapper.create(FFactory.link, bnd.link);
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

function TFHIRGraphQLEngine.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFactory.sizeInBytes);
  inc(result, FFocus.sizeInBytes);
  inc(result, FOutput.sizeInBytes);
  inc(result, FGraphQL.sizeInBytes);
  inc(result, FAppinfo.sizeInBytes);
  inc(result, FWorkingVariables.sizeInBytes);
  inc(result, FPathEngine.sizeInBytes);
  inc(result, FMagicExpression.sizeInBytes);
end;

{ TFHIRGraphQLSearchWrapper }

constructor TFHIRGraphQLSearchWrapper.Create(factory : TFHIRFactory; bundle : TFHIRBundleW);
var
  s : String;
begin
  inherited Create;
  FFactory := factory;
  FBundle := bundle;
  s := bundle.links['self'];
  FParseMap := THTTPParameters.create(s.Substring(s.IndexOf('?')+1));
end;

function TFHIRGraphQLSearchWrapper.createPropertyValue(propName: string): TFHIRObject;
begin
  result := nil;
end;

destructor TFHIRGraphQLSearchWrapper.Destroy;
begin
  FParseMap.free;
  FBundle.Free;
  FFactory.Free;
  inherited;
end;

function TFHIRGraphQLSearchWrapper.extractLink(name: String): TFHIRObject;
var
  s : String;
  pm : THTTPParameters;
begin
  s := FBundle.links[name];
  if s = '' then
    result := nil
  else
  begin
    pm := THTTPParameters.create(s.Substring(s.IndexOf('?')+1));
    try
      result := FBundle.makeStringValue(pm['search-id']+':'+pm['search-offset']);
    finally
      pm.Free;
    end;
  end;
end;

function TFHIRGraphQLSearchWrapper.extractParam(name: String; int : boolean): TFhirObject;
var
  s : String;
begin
  s := FParseMap[name];
  if s = '' then
    result := nil
  else if int then
    result := FBundle.makeIntValue(s)
  else
    result := FBundle.makeStringValue(s);
end;

function TFHIRGraphQLSearchWrapper.fhirType: String;
begin
  result := '*Connection';
end;

  // http://test.fhir.org/r3/Patient?_format=text/xhtml&search-id=77c97e03-8a6c-415f-a63d-11c80cf73f&&active=true&_sort=_id&search-offset=50&_count=50

function TFHIRGraphQLSearchWrapper.getId: String;
begin
  result := '';
end;

function TFHIRGraphQLSearchWrapper.getPropertyValue(propName: string): TFHIRProperty;
var
  list : TFslList<TFslObject>;
  be : TFHIRBundleEntryW;
  bel : TFslList<TFHIRBundleEntryW>;
begin
  if propName = 'first' then
    result := TFHIRProperty.Create(self, propname, 'string', false, nil, extractLink('first'))
  else if propName = 'previous' then
    result := TFHIRProperty.Create(self, propname, 'string', false, nil, extractLink('previous'))
  else if propName = 'next' then
    result := TFHIRProperty.Create(self, propname, 'string', false, nil, extractLink('next'))
  else if propName = 'last' then
    result := TFHIRProperty.Create(self, propname, 'string', false, nil, extractLink('last'))
  else if propName = 'count' then
    result := TFHIRProperty.Create(self, propname, 'integer', false, nil, makeIntValue(inttostr(FBundle.total)))
  else if propName = 'offset' then
    result := TFHIRProperty.Create(self, propname, 'integer', false, nil, extractParam('search-offset', true))
  else if propName = 'pagesize' then
    result := TFHIRProperty.Create(self, propname, 'integer', false, nil, extractParam('_count', true))
  else if propName = 'edges' then
  begin
    list := TFslList<TFslObject>.create;
    try
      bel := FBundle.entries;
      try
        for be in bel do
          list.Add(TFHIRGraphQLSearchEdge.create(ffactory.link, be.Link));
        result := TFHIRProperty.Create(self, propname, 'edge', true, nil, list);
      finally
        bel.Free;
      end;
    finally
      list.Free;
    end;
  end
  else
    result := nil;
end;

function TFHIRGraphQLSearchWrapper.hasExtensions: boolean;
begin
  result := false;
end;

function TFHIRGraphQLSearchWrapper.makeCodeValue(v: String): TFHIRObject;
begin
  result := FBundle.makeCodeValue(v);
end;

function TFHIRGraphQLSearchWrapper.makeIntValue(v: String): TFHIRObject;
begin
  result := FBundle.makeIntValue(v);
end;

function TFHIRGraphQLSearchWrapper.makeStringValue(v: String): TFHIRObject;
begin
  result := FBundle.makeStringValue(v);
end;

procedure TFHIRGraphQLSearchWrapper.SetBundle(const Value: TFHIRBundleW);
begin
  FBundle.Free;
  FBundle := Value;
end;

procedure TFHIRGraphQLSearchWrapper.setIdValue(id: String);
begin
end;

function TFHIRGraphQLSearchWrapper.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  result := nil;
end;

function TFHIRGraphQLSearchWrapper.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFactory.sizeInBytes);
  inc(result, FBundle.sizeInBytes);
  inc(result, FParseMap.sizeInBytes);
end;

{ TFHIRGraphQLSearchEdge }

constructor TFHIRGraphQLSearchEdge.Create(factory : TFHIRFactory; entry: TFHIRBundleEntryW);
begin
  inherited Create;
  FEntry := entry;
  FFactory := factory;
end;

function TFHIRGraphQLSearchEdge.createPropertyValue(propName: string): TFHIRObject;
begin
  result := nil;
end;

destructor TFHIRGraphQLSearchEdge.Destroy;
begin
  FEntry.Free;
  FFactory.Free;
  inherited;
end;

function TFHIRGraphQLSearchEdge.fhirType: String;
begin
  result := '*Edge';
end;

function TFHIRGraphQLSearchEdge.getId: String;
begin

end;

function TFHIRGraphQLSearchEdge.getPropertyValue(propName: string): TFHIRProperty;
begin
  if propName = 'mode' then
  begin
    if FEntry.searchMode <> smUnknown then
      result := TFHIRProperty.Create(self, propname, 'code', false, nil, ffactory.makeCode(CODES_TFHIRBundleEntrySearchMode[FEntry.searchMode]))
    else
      result := TFHIRProperty.Create(self, propname, 'code', false, nil, TFHIRObject(nil));
  end
  else if propName = 'score' then
  begin
    if FEntry.searchScore <> '' then
      result := TFHIRProperty.Create(self, propname, 'decimal', false, nil, FFactory.makeDecimal(FEntry.searchScore))
    else
      result := TFHIRProperty.Create(self, propname, 'decimal', false, nil, TFHIRObject(nil));
  end
  else if propName = 'resource' then
    result := TFHIRProperty.Create(self, propname, 'resource', false, TFHIRResourceV, FEntry.resource.Link)
  else
    result := nil;
end;

function TFHIRGraphQLSearchEdge.hasExtensions: boolean;
begin
  result := false;
end;

function TFHIRGraphQLSearchEdge.makeCodeValue(v: String): TFHIRObject;
begin
  result := FEntry.makeCodeValue(v);
end;

function TFHIRGraphQLSearchEdge.makeIntValue(v: String): TFHIRObject;
begin
  result := FEntry.makeIntValue(v);
end;

function TFHIRGraphQLSearchEdge.makeStringValue(v: String): TFHIRObject;
begin
  result := FEntry.makeStringValue(v);
end;

procedure TFHIRGraphQLSearchEdge.SetEntry(const Value: TFHIRBundleEntryW);
begin
  FEntry.Free;
  FEntry := value;
end;

procedure TFHIRGraphQLSearchEdge.setIdValue(id: String);
begin
end;

function TFHIRGraphQLSearchEdge.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  result := nil;
end;

function TFHIRGraphQLSearchEdge.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFactory.sizeInBytes);
  inc(result, FEntry.sizeInBytes);
end;

function TFHIRGraphQLEngine.getSingleValue(arg: TGraphQLArgument): string;
var
  vl : TFslList<TGraphQLValue>;
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

function TFHIRGraphQLEngine.resolveValues(arg: TGraphQLArgument; max: integer; vars : String): TFslList<TGraphQLValue>;
var
  v : TGraphQLValue;
  a : TGraphQLArgument;
  vl : TFslList<TGraphQLValue>;
begin
  result := TFslList<TGraphQLValue>.create;
  try
    for v in arg.Values do
      if not (v is TGraphQLVariableValue) then
        result.Add(v.Link)
      else
      begin
        if vars.Contains(':'+v.ToString+':') then
          raise EJsonException.Create('Recursive reference to variable '+v.ToString);
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
          raise EJsonException.Create('No value found for variable "'+v.ToString+'" in "'+arg.Name+'"');
      end;
    if (max <> -1) and (result.Count > max) then
      raise EJsonException.Create('Only '+integer.ToString(max)+' values are allowed for "'+arg.Name+'", but '+inttostr(result.Count)+' enoucntered');
    result.link;
  finally
    result.free;
  end;
end;


end.
