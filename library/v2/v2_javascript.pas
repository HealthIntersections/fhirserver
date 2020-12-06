unit v2_javascript;

{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)

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

// wrapper layer for chakra for v2 messages

{$I fhir.inc}

interface

uses
  fsl_base, fsl_utilities,
  fhir_objects, fhir_pathengine,
  fsl_javascript, fhir_javascript,
  v2_message;

type
  TV2JavascriptHelper = class (TFslObject)
  private
    defContent : TJavascriptClassDefinition;
    defCell : TJavascriptClassDefinition;
    defField : TJavascriptClassDefinition;
    defSegment : TJavascriptClassDefinition;
    defMessage : TJavascriptClassDefinition;
    defUtils : TJavascriptClassDefinition;

    function getObjectId(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    procedure setObjectId(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    function fnObjectIsEmpty(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function fnObjectType(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    procedure defineObjectProps(js : TJavascript; def : TJavascriptClassDefinition);

    function getContentKind(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    procedure setContentKind(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    function getContentValue(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    procedure setContentValue(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    function factCreateContent(js: TJavascript; classDef : TJavascriptClassDefinition; params : TJsValues; out owns : boolean): TObject;
    procedure defineContent(js : TJavascript);

    function getCellContents(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    procedure setCellContents(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    function getCellComponents(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    procedure setCellComponents(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    function getCellText(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    procedure setCellText(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    function factCreateCell(js: TJavascript; classDef : TJavascriptClassDefinition; params : TJsValues; out owns : boolean): TObject;
    procedure defineCell(js : TJavascript);

    function getFieldElements(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    procedure setFieldElements(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    function fnFieldElement(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function factCreateField(js: TJavascript; classDef : TJavascriptClassDefinition; params : TJsValues; out owns : boolean): TObject;
    procedure defineField(js : TJavascript);

    function getSegmentCode(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    procedure setSegmentCode(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    function getSegmentFields(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    procedure setSegmentFields(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    function fnSegmentElement(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function fnSegmentField(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function factCreateSegment(js: TJavascript; classDef : TJavascriptClassDefinition; params : TJsValues; out owns : boolean): TObject;
    procedure defineSegment(js : TJavascript);

    function getMessageSegments(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
    procedure setMessageSegments(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
    function fnMessageAddSegment(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function fnMessageSegment(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function factCreateMessage(js: TJavascript; classDef : TJavascriptClassDefinition; params : TJsValues; out owns : boolean): TObject;
    procedure defineMessage(js : TJavascript);

    function fnUtilsEncode(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function fnUtilsDecode(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function fnUtilsCheckEquals(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    function fnUtilsQuery(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
    procedure defineUtils(js : TJavascript);
  public
    class procedure registerv2Objects(js : TJavascript); overload;
    class procedure registerv2Objects(js : TJavascript; engine : TFHIRPathEngineV); overload;
  end;

implementation

{ TV2JavascriptHelper }

class procedure TV2JavascriptHelper.registerv2Objects(js : TJavascript);
begin
  registerv2Objects(js, nil);
end;

class procedure TV2JavascriptHelper.registerv2Objects(js : TJavascript; engine : TFHIRPathEngineV);
var
  this : TV2JavascriptHelper;
begin
  this := TV2JavascriptHelper.Create;
  js.ownObject(this);
  this.defineContent(js);
  this.defineCell(js);
  this.defineField(js);
  this.defineSegment(js);
  this.defineMessage(js);
  this.defineUtils(js);
  if engine <> nil then
    js.addGlobal('v2', js.wrap(engine.Link, 'V2Utilities', true))
  else
    js.addGlobal('v2', js.wrap(TObject.create, 'V2Utilities', true));
end;

procedure TV2JavascriptHelper.defineObjectProps(js : TJavascript; def : TJavascriptClassDefinition);
begin
  def.defineProperty('isEmpty', nil, getObjectId, setObjectId);
  def.defineRoutine('id', nil, fnObjectIsEmpty);
  def.defineRoutine('type', nil, fnObjectType);
end;

procedure TV2JavascriptHelper.defineContent(js: TJavascript);
begin
  defContent := js.defineClass('Content', nil, 'Content', factCreateContent);
  defineObjectProps(js, defContent);
  defContent.defineProperty('kind', nil, getContentKind, setContentKind);
  defContent.defineProperty('value', nil, getContentValue, setContentValue);
end;

procedure TV2JavascriptHelper.defineCell(js: TJavascript);
begin
  defCell := js.defineClass('Cell', nil, 'Cell', factCreateCell);
  defineObjectProps(js, defCell);
  defCell.defineProperty('contents', nil, getCellContents, setCellContents);
  defCell.defineProperty('components', nil, getCellComponents, setCellComponents);
  defCell.defineProperty('text', nil, getCellText, setCellText);
end;

procedure TV2JavascriptHelper.defineField(js: TJavascript);
begin
  defField := js.defineClass('Field', nil, 'Field', factCreateField);
  defineObjectProps(js, defField);
  defField.defineProperty('elements', nil, getFieldElements, setFieldElements);
  defField.defineRoutine('element', nil, fnFieldElement);
end;

procedure TV2JavascriptHelper.defineSegment(js: TJavascript);
begin
  defSegment := js.defineClass('Segment', nil, 'Segment', factCreateSegment);
  defineObjectProps(js, defSegment);
  defSegment.defineProperty('code', nil, getSegmentCode, setSegmentCode);
  defSegment.defineProperty('fieldList', nil, getSegmentFields, setSegmentFields);
  defSegment.defineRoutine('field', nil, fnSegmentField);
  defSegment.defineRoutine('element', nil, fnSegmentElement);
end;

procedure TV2JavascriptHelper.defineMessage(js: TJavascript);
begin
  defMessage := js.defineClass('Message', nil, 'Message', factCreateMessage);
  defineObjectProps(js, defMessage);
  defMessage.defineProperty('segmentList', nil, getMessageSegments, setMessageSegments);
  defMessage.defineRoutine('segment', nil, fnMessageSegment);
  defMessage.defineRoutine('add', nil, fnMessageAddSegment);
end;

procedure TV2JavascriptHelper.defineUtils(js: TJavascript);
begin
  defUtils := js.defineClass('V2Utilities', nil);
  defUtils.defineRoutine('encode', nil, fnUtilsEncode);
  defUtils.defineRoutine('parse', nil, fnUtilsDecode);
  defUtils.defineRoutine('decode', nil, fnUtilsDecode);
  defUtils.defineRoutine('checkEquals', nil, fnUtilsCheckEquals);
  defUtils.defineRoutine('query', nil, fnUtilsQuery);
  defUtils.defineRoutine('q', nil, fnUtilsQuery);
end;

function TV2JavascriptHelper.factCreateCell(js: TJavascript; classDef: TJavascriptClassDefinition; params: TJsValues; out owns: boolean): TObject;
begin
  owns := true; //?
  result := TV2Cell.create;
  if length(params) = 1 then
    TV2Cell(result).contentList.Add(TV2Content.Create(ckString, js.asString(params[0])));
end;

function TV2JavascriptHelper.factCreateContent(js: TJavascript; classDef: TJavascriptClassDefinition; params: TJsValues; out owns: boolean): TObject;
begin
  owns := true; //?
  if length(params) = 1 then
    result := TV2Content.Create(ckString, js.asString(params[0]))
  else
    result := TV2Cell.create;
end;

function TV2JavascriptHelper.factCreateField(js: TJavascript; classDef: TJavascriptClassDefinition; params: TJsValues; out owns: boolean): TObject;
begin
  owns := true; //?
  result := TV2Field.create;
  if length(params) = 1 then
  begin
    TV2Field(result).elementList.Add(TV2Cell.Create);
    TV2Field(result).elementList[0].contentList.Add(TV2Content.Create(ckString, js.asString(params[0])));
  end;
end;

function TV2JavascriptHelper.factCreateMessage(js: TJavascript; classDef: TJavascriptClassDefinition; params: TJsValues; out owns: boolean): TObject;
begin
  owns := true; //?
  result := TV2Message.create;
end;

function TV2JavascriptHelper.factCreateSegment(js: TJavascript; classDef: TJavascriptClassDefinition; params: TJsValues; out owns: boolean): TObject;
begin
  owns := true; //?
  if length(params) <> 1 then
    raise EJavascriptSource.Create('A single parameter is required for creating a segment (the segment code)');
  result := TV2Segment.Create(js.asString(params[0]))
end;

function TV2JavascriptHelper.getCellText(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
begin
  result := js.wrap((this as TV2Cell).text);
end;

function TV2JavascriptHelper.fnFieldElement(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
var
  i : integer;
begin
  i := js.asInteger(parameters[0]);
  result := js.wrap((this as TV2Field).element[i], 'Cell', false);
end;

function TV2JavascriptHelper.fnMessageAddSegment(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
var
  message : TV2Message;
  segment : TV2Segment;
begin
  if length(parameters) <> 1 then
    raise EJavascriptSource.Create('A single parameter is required for creating a segment (the segment code)');
  message := this as TV2Message;
  segment := TV2Segment.Create(js.asString(parameters[0]));
  try
    message.segmentList.Add(segment.link);
    result := js.wrap(segment, defSegment, false);
  finally
    segment.free;
  end;
end;

function TV2JavascriptHelper.fnMessageSegment(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
var
  message : TV2Message;
begin
  message := this as TV2Message;
  result := js.wrap(message.segment[js.asInteger(parameters[0])], defSegment, false);
end;

function TV2JavascriptHelper.fnObjectIsEmpty(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
begin
  result := js.wrap((this as TV2Object).isEmpty);
end;

function TV2JavascriptHelper.fnObjectType(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
begin
  result := js.wrap((this as TV2Object).fhirType);
end;

function TV2JavascriptHelper.fnSegmentElement(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
var
  i : integer;
begin
  i := js.asInteger(parameters[0]);
  result := js.wrap((this as TV2Segment).element(i), 'Cell', false);
end;

function TV2JavascriptHelper.fnSegmentField(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
var
  i : integer;
begin
  i := js.asInteger(parameters[0]);
  result := js.wrap((this as TV2Segment).field[i], 'Field', false);
end;

function TV2JavascriptHelper.fnUtilsCheckEquals(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
var
  s1, s2 : String;
begin
  s1 := js.asString(parameters[0]);
  s2 := js.asString(parameters[1]);
  if s1 <> s2 then
    raise EJavascriptSource.Create('CheckEquals fails: "'+s1+'" != "'+s2+'"');
  result := nil;
end;

function TV2JavascriptHelper.fnUtilsDecode(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
var
  s : String;
  decoder : TV2Parser;
begin
  if length(parameters) = 0 then
    raise EJavascriptSource.Create('Decode must have at least one parameter, the content to decode');
  s := js.asString(parameters[0]);
  decoder := TV2Parser.Create;
  try
    result := js.wrap(decoder.parse(s), defMessage, true);
  finally
    decoder.Free;
  end;
end;

function TV2JavascriptHelper.fnUtilsEncode(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
var
  message : TV2Message;
  encoder : TV2Composer;
begin
  if length(parameters) = 0 then
    raise EJavascriptSource.Create('Encode must have at least one parameter, the message to encode');
  message := js.getWrapped<TV2Message>(parameters[0]);
  encoder := TV2Composer.Create;
  try
    result := js.wrap(encoder.composeString(message, []));
  finally
    encoder.Free;
  end;
end;

function TV2JavascriptHelper.fnUtilsQuery(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; parameters: TJsValues): JsValueRef;
var
  engine : TFHIRPathEngineV;
  focus : TV2Object;
  query : String;
  outcome : TFHIRSelectionList;
  list : TFslList<TFHIRObject>;
  o : TFHIRSelection;
begin
  if this is TFHIRPathEngineV then
  begin
    engine := TFHIRPathEngineV(this);
    focus := js.getWrapped<TV2Object>(parameters[0]);
    query := js.asString(parameters[1]);
    outcome := engine.evaluate(nil, focus, query);
    try
      list := TFslList<TFHIRObject>.create;
      try
        for o in outcome do
          list.Add(o.value.link);
        result := js.makeManagedArray(TFslListManager<TFHIRObject>.Create(list.link, TFHIRListManagerResolver.Create(js)));
      finally
        list.Free;
      end;
    finally
       outcome.Free;
    end;
  end
  else
    result := js.getNull;
end;

function TV2JavascriptHelper.getCellComponents(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
var
  cell : TV2Cell;
begin
  cell := this as TV2Cell;
  if (cell.componentList.jsInstance = js.instanceId) then
    result := cell.componentList.jsHandle
  else
  begin
    result := js.makeManagedArray(TFslListManager<TV2Cell>.Create(cell.componentList.Link, defCell));
    cell.componentList.jsHandle := result;
    cell.componentList.jsInstance := js.InstanceId;
  end;
end;

function TV2JavascriptHelper.getCellContents(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
var
  cell : TV2Cell;
begin
  cell := this as TV2Cell;
  if (cell.contentList.jsInstance = js.instanceId) then
    result := cell.contentList.jsHandle
  else
  begin
    result := js.makeManagedArray(TFslListManager<TV2Content>.Create(cell.contentList.Link, defContent));
    cell.contentList.jsHandle := result;
    cell.contentList.jsInstance := js.InstanceId;
  end;
end;

function TV2JavascriptHelper.getContentKind(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
var
  content : TV2Content;
begin
  content := this as TV2Content;
  result := js.wrap(CODES_TV2ContentKind[content.kind]);
end;

function TV2JavascriptHelper.getContentValue(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
var
  content : TV2Content;
begin
  content := this as TV2Content;
  result := js.wrap(content.value);
end;

function TV2JavascriptHelper.getFieldElements(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
var
  field : TV2Field;
begin
  field := this as TV2Field;
  if (field.elementList.jsInstance = js.instanceId) then
    result := field.elementList.jsHandle
  else
  begin
    result := js.makeManagedArray(TFslListManager<TV2Cell>.Create(field.elementList.Link, defCell));
    field.elementList.jsHandle := result;
    field.elementList.jsInstance := js.InstanceId;
  end;
end;

function TV2JavascriptHelper.getMessageSegments(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
var
  message : TV2Message;
begin
  message := this as TV2Message;
  if (message.segmentList.jsInstance = js.instanceId) then
    result := message.segmentList.jsHandle
  else
  begin
    result := js.makeManagedArray(TFslListManager<TV2Segment>.Create(message.segmentList.Link, defSegment));
    message.segmentList.jsHandle := result;
    message.segmentList.jsInstance := js.InstanceId;
  end;
end;

function TV2JavascriptHelper.getObjectId(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
var
  obj : TV2Object;
begin
  obj := this as TV2Object;
  result := js.wrap(obj.id);
end;

function TV2JavascriptHelper.getSegmentCode(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
var
  segment : TV2Segment;
begin
  segment := this as TV2Segment;
  result := js.wrap(segment.code);
end;

function TV2JavascriptHelper.getSegmentFields(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject): JsValueRef;
var
  segment : TV2Segment;
begin
  segment := this as TV2Segment;
  if (segment.fieldList.jsInstance = js.instanceId) then
    result := segment.fieldList.jsHandle
  else
  begin
    result := js.makeManagedArray(TFslListManager<TV2Field>.Create(segment.fieldList.Link, defField));
    segment.fieldList.jsHandle := result;
    segment.fieldList.jsInstance := js.InstanceId;
  end;
end;

type
  TV2JavascriptCallBackCellContext = record
    params : TJsValues;
    owns : boolean;
    cell : TV2Cell;
    defCell : TJavascriptClassDefinition;
  end;
  PV2JavascriptCallBackCellContext = ^TV2JavascriptCallBackCellContext;


procedure iterateArrayComponents(js : TJavascript; context : pointer; i : integer; v : JsValueRef);
var
  o : TV2Cell;
  ctxt : PV2JavascriptCallBackCellContext;
begin
  ctxt := context;
  o := js.getWrapped<TV2Cell>(v).Link;
  try
    if (o = nil) then
    begin
      ctxt.params[0] := v;
      o := ctxt.defCell.factory(js, ctxt.defCell, ctxt.params, ctxt.owns) as TV2Cell;
    end;
    ctxt.cell.componentList.Add(o.Link);
  finally
    o.Free;
  end;
end;

procedure TV2JavascriptHelper.setCellComponents(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
var
  ctxt : TV2JavascriptCallBackCellContext;
begin
  ctxt.cell := this as TV2Cell;
  ctxt.cell.componentList.Clear;
  ctxt.cell.componentList.jsInstance := 0;
  ctxt.defCell := defCell;
  setLength(ctxt.params, 1);
  js.iterateArray(value, iterateArrayComponents, @ctxt);
end;

procedure iterateArrayContents(js : TJavascript; context : pointer; i : integer; v : JsValueRef);
var
  ctxt : PV2JavascriptCallBackCellContext;
  o : TV2Content;
begin
  ctxt := context;
  o := js.getWrapped<TV2Content>(v).Link;
  try
    if (o = nil) then
    begin
      ctxt.params[0] := v;
      o := ctxt.defCell.factory(js, ctxt.defCell, ctxt.params, ctxt.owns) as TV2Content;
    end;
    ctxt.cell.contentList.Add(o.Link);
  finally
    o.Free;
  end;
end;

procedure TV2JavascriptHelper.setCellContents(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
var
  ctxt : TV2JavascriptCallBackCellContext;
begin
  ctxt.cell := this as TV2Cell;
  ctxt.cell.contentList.Clear;
  ctxt.cell.contentList.jsInstance := 0;
  setLength(ctxt.params, 1);
  ctxt.defCell := defCell;
  js.iterateArray(value, iterateArrayContents, @ctxt);
end;

procedure TV2JavascriptHelper.setCellText(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
var
  cell : TV2Cell;
begin
  cell := this as TV2Cell;
  cell.Text := js.asString(value);
end;

procedure TV2JavascriptHelper.setContentKind(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
var
  content : TV2Content;
  i : integer;
begin
  content := this as TV2Content;
  i := StringArrayIndexOfSensitive(CODES_TV2ContentKind, js.asString(value));
  if i = -1 then
    raise EJavascriptSource.Create('The value "'+js.asString(value)+'" is not a valid content kind');
  content.kind := TV2ContentKind(i);
end;

procedure TV2JavascriptHelper.setContentValue(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
var
  content : TV2Content;
begin
  content := this as TV2Content;
  content.value := js.asString(value);
end;

type
  TV2JavascriptCallBackFieldContext = record
    params : TJsValues;
    owns : boolean;
    field : TV2Field;
    defCell : TJavascriptClassDefinition;
  end;
  PV2JavascriptCallBackFieldContext = ^TV2JavascriptCallBackFieldContext;

procedure iterateArrayFields(js : TJavascript; context : pointer; i : integer; v : JsValueRef);
var
  ctxt : PV2JavascriptCallBackFieldContext;
  o : TV2Cell;
begin
  ctxt := context;
  o := js.getWrapped<TV2Cell>(v).Link;
  try
    if (o = nil) then
    begin
      ctxt.params[0] := v;
      o := ctxt.defCell.factory(js, ctxt.defCell, ctxt.params, ctxt.owns) as TV2Cell;
    end;
    ctxt.field.elementList.Add(o.Link);
  finally
    o.Free;
  end;
end;

procedure TV2JavascriptHelper.setFieldElements(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
var
  ctxt : TV2JavascriptCallBackFieldContext;
begin
  ctxt.field := this as TV2Field;
  ctxt.field.elementList.Clear;
  ctxt.field.elementList.jsInstance := 0;
  ctxt.defCell := defCell;
  setLength(ctxt.params, 1);
  js.iterateArray(value, iterateArrayFields, @ctxt);
end;

type
  TV2JavascriptCallBackMessageContext = record
    params : TJsValues;
    owns : boolean;
    message : TV2Message;
    defCell : TJavascriptClassDefinition;
  end;
  PV2JavascriptCallBackMessageContext = ^TV2JavascriptCallBackMessageContext;

procedure iterateArraySegments(js : TJavascript; context : pointer; i : integer; v : JsValueRef);
var
  ctxt : PV2JavascriptCallBackMessageContext;
  o : TV2Segment;
begin
  ctxt := context;
  o := js.getWrapped<TV2Segment>(v).Link;
  try
    if (o = nil) then
    begin
      ctxt.params[0] := v;
      o := ctxt.defCell.factory(js, ctxt.defCell, ctxt.params, ctxt.owns) as TV2Segment;
    end;
    ctxt.message.segmentList.Add(o.Link);
  finally
    o.Free;
  end;
end;

procedure TV2JavascriptHelper.setMessageSegments(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
var
  ctxt : TV2JavascriptCallBackMessageContext;
begin
  ctxt.message := this as TV2Message;
  ctxt.message.segmentList.Clear;
  ctxt.message.segmentList.jsInstance := 0;
  ctxt.defCell := defCell;
  setLength(ctxt.params, 1);
  js.iterateArray(value, iterateArraySegments, @ctxt);
end;

procedure TV2JavascriptHelper.setObjectId(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
var
  obj : TV2Object;
begin
  obj := this as TV2Object;
  obj.id := js.asString(value);
end;

procedure TV2JavascriptHelper.setSegmentCode(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
var
  segment : TV2Segment;
begin
  segment := this as TV2Segment;
  segment.code := js.asString(value);
end;

type
  TV2JavascriptCallBackSegmentContext = record
    params : TJsValues;
    owns : boolean;
    segment : TV2Segment;
    defCell : TJavascriptClassDefinition;
  end;
  PV2JavascriptCallBackSegmentContext = ^TV2JavascriptCallBackSegmentContext;

procedure iterateArraySegmentFields(js : TJavascript; context : pointer; i : integer; v : JsValueRef);
var
  ctxt : PV2JavascriptCallBackSegmentContext;
  o : TV2Field;
begin
  ctxt := context;
  o := js.getWrapped<TV2Field>(v).Link;
  try
    if (o = nil) then
    begin
      ctxt.params[0] := v;
      o := ctxt.defCell.factory(js, ctxt.defCell, ctxt.params, ctxt.owns) as TV2Field;
    end;
    ctxt.segment.fieldList.Add(o.Link);
  finally
    o.Free;
  end;
end;

procedure TV2JavascriptHelper.setSegmentFields(js: TJavascript; propDef: TJavascriptRegisteredProperty; this: TObject; value: TJsValue);
var
  ctxt : TV2JavascriptCallBackSegmentContext;
begin
  ctxt.segment := this as TV2Segment;
  ctxt.segment.fieldList.Clear;
  ctxt.segment.fieldList.jsInstance := 0;
  ctxt.defCell := defCell;
  setLength(ctxt.params, 1);
  js.iterateArray(value, iterateArraySegmentFields, @ctxt);
end;

end.
