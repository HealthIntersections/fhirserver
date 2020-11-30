unit fhir_diff;

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
  fsl_base, fsl_utilities, fsl_stream, fsl_xml,
  fhir_objects, fhir_parser, fhir_xhtml, fhir_pathengine,
  fhir_factory,  fhir_common;


type
  TDifferenceOperation = (diffAdd, diffInsert, diffDelete, diffReplace, diffMove);

const
  CODES_DIFF_OP : Array [TDifferenceOperation] of String = ('add', 'insert', 'delete', 'replace', 'move');

type
  TDifference = class (TFslObject)
  private
    FPath : String;
    FOp : TDIfferenceOperation;
    FName : String;
    FValue : TFHIRObject;
    FIndex : integer;
    FIndex2 : integer;
  protected
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; override;

    property Path : String read FPath;
    property Op : TDIfferenceOperation read FOp;
    property Name : String read FName;
    property Value : TFHIRObject read FValue;
    property Index : integer read FIndex;
    property Index2 : integer read FIndex2;
  end;

  TDifferenceList = class (TFslList<TDifference>)
  public
    procedure replace(path : String; value : TFHIRObject);
    procedure add(path, name : String; value : TFHIRObject);
    procedure insert(path : String; index : integer; value : TFHIRObject);
    procedure delete(path : String);
    procedure move(path : String; source, target : integer);
  end;

  TDifferenceMatch = class (TFslObject)
  private
    FSourceIndex : integer;
    FTargetIndex : integer;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(sourceIndex, targetIndex : integer);
  end;

  TDifferenceMatchList = class (TFslList<TDifferenceMatch>)
  public
    function listUnchanged(l1, l2 : integer) : boolean;
    function matchedTarget(ti : integer) : TDifferenceMatch;
    function matchedSource(si : integer) : TDifferenceMatch;
  end;

  TOffset = class (TFslObject)
  private
    FInserted : boolean;
    FIndex: integer;
  protected
    function sizeInBytesV : cardinal; override;
  public
    property index : integer read FIndex;
  end;

  TOffSetList = class (TFslList<TOffset>)
  public
    function hasUsed(index : integer) : boolean;
    procedure inc(var index : integer);
    procedure skip(index : integer);
    procedure ins(index : integer);
    function adjust(index : integer) : integer;
  end;

  TDifferenceEngine = class (TFslObject)
  private
    FFactory : TFHIRFactory;
    FContext : TFHIRWorkerContextV;
    fpe : TFHIRPathEngineV;

    function matchRating(obj1, obj2 : TFHIRObject) : Double;
    procedure findCertainMatches(matches : TDifferenceMatchList; bl, ml : TFHIRObjectList);
    procedure findPossibleMatches(matches : TDifferenceMatchList; bl, ml : TFHIRObjectList);
    procedure makeListChanges(path : String; name : String; matches : TDifferenceMatchList; bl, ml : TFHIRObjectList; differences : TDifferenceList);
    procedure generate(path : String; base, modified : TFHIRObject; differences : TDifferenceList);
    procedure encodeValue(part : TFhirParametersParameterW; value : TFHIRObject);
    function asParams(differences : TDifferenceList) : TFHIRParametersW;
    function asValue(value : TFHIRObject) : string;
    function asHtml(differences : TDifferenceList) : string;

    procedure populateObject(res : TFHIRObject; props : TFhirParametersParameterW);
    procedure applyAdd(res : TFHIRObject; path : String; name : String; value : TFhirParametersParameterW);
    procedure applyInsert(res : TFHIRObject; path : String; index : integer; value : TFhirParametersParameterW);
    procedure applyDelete(res : TFHIRObject; path : String);
    procedure applyReplace(res : TFHIRObject; path : String; value : TFhirParametersParameterW);
    procedure applyMove(res : TFHIRObject; path : String; source, destination : integer);

    function applyOperation(res : TFHIRObject; op : TFhirParametersParameterW) : boolean;

  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(context : TFHIRWorkerContextV; factory : TFHIRFactory);
    destructor Destroy; override;
    function generateDifference(base, modified : TFHIRObject; var html : String) : TFHIRParametersW;
    function applyDifference(base : TFHIRObject; delta : TFHIRParametersW) : TFHIRObject;
  end;

implementation

{ TDifference }

destructor TDifference.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TDifference.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FPath.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FValue.sizeInBytes);
end;

{ TDifferenceList }

procedure TDifferenceList.replace(path: String; value: TFHIRObject);
var
  d : TDifference;
begin
  d := TDifference.create;
  inherited add(d);
  d.FPath := path;
  d.FOp := diffReplace;
  d.FValue := value;
end;

procedure TDifferenceList.add(path, name: String; value: TFHIRObject);
var
  d : TDifference;
begin
  d := TDifference.create;
  inherited add(d);
  d.FPath := path;
  d.FOp := diffAdd;
  d.FName := name;
  d.FValue := value;
end;

procedure TDifferenceList.delete(path: String);
var
  d : TDifference;
begin
  d := TDifference.create;
  inherited add(d);
  d.FPath := path;
  d.FOp := diffDelete;
end;


procedure TDifferenceList.insert(path: String; index: integer; value: TFHIRObject);
var
  d : TDifference;
begin
  d := TDifference.create;
  inherited add(d);
  d.FPath := path;
  d.FOp := diffInsert;
  d.FIndex := index;
  d.FValue := value;
end;

procedure TDifferenceList.move(path: String; source, target: integer);
var
  d : TDifference;
begin
  d := TDifference.create;
  inherited add(d);
  d.FPath := path;
  d.FOp := diffMove;
  d.FIndex := source;
  d.FIndex2 := target;
end;

{ TDifferenceMatch }

constructor TDifferenceMatch.Create(sourceIndex, targetIndex: integer);
begin
  inherited Create;
  FSourceIndex := sourceIndex;
  FTargetIndex := targetIndex;
end;

function TDifferenceMatch.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ TDifferenceMatchList }

function TDifferenceMatchList.listUnchanged(l1, l2 : integer): boolean;
var
  i : integer;
begin
  result := true;
  if (l1 <> l2) or (l1 <> Count) then
    exit(false);
  for I := 0 to Count - 1 do
    if (items[i].FSourceIndex <> i) or (items[i].FTargetIndex <> i)  then
      exit(false);
end;

function TDifferenceMatchList.matchedTarget(ti: integer): TDifferenceMatch;
var
  dm : TDifferenceMatch;
begin
  result := nil;
  for dm in self do
    if dm.FTargetIndex = ti then
      exit(dm);
end;

function TDifferenceMatchList.matchedSource(si: integer): TDifferenceMatch;
var
  dm : TDifferenceMatch;
begin
  result := nil;
  for dm in self do
    if dm.FSourceIndex = si then
      exit(dm);
end;

{ TDifferenceEngine }

procedure TDifferenceEngine.applyAdd(res : TFHIRObject; path, name: String; value: TFhirParametersParameterW);
var
  dest : TFHIRSelectionList;
  v : TFHIRObject;
begin
  dest := fpe.evaluate(nil, res, path);
  try
    if dest.Count = 0 then
      raise EFHIRException.create('No content found at '+path+' when adding');
    if dest.Count > 1 then
      raise EFHIRException.create('Multiple locations found at '+path+' when adding');

    if value.hasValue then
      dest[0].value.setProperty(name, value.value.Link)
    else
    begin
      v := dest[0].value.createPropertyValue(name);
      try
        dest[0].value.setProperty(name, v.Link);
        populateObject(v, value);
      finally
        v.free;
      end;
    end;
  finally
    dest.Free;
  end;
end;

procedure TDifferenceEngine.applyDelete(res : TFHIRObject; path: String);
var
  dest : TFHIRSelectionList;
begin
  dest := fpe.evaluate(nil, res, path);
  try
    if dest.Count = 0 then
      raise EFHIRException.create('No content found at '+path+' when adding');
    if dest.Count > 1 then
      raise EFHIRException.create('Multiple locations found at '+path+' when adding');
    if dest[0].parent = nil then
      raise EFHIRException.create('Content returned from Path is not part of Resource');
    dest[0].parent.deleteProperty(dest[0].name, dest[0].value);
  finally
    dest.Free;
  end;
end;

function TDifferenceEngine.applyDifference(base: TFHIRObject; delta: TFHIRParametersW): TFHIRObject;
var
  op : TFhirParametersParameterW;
  de : boolean;
begin
  de := false;
  result := base.Clone;
  try
    for op in delta.parameterList do
      de := applyOperation(result, op) or de;
    if de then
      result.dropEmpty;
    result.Link;
  finally
    result.Free;
  end;
end;

function TDifferenceEngine.generateDifference(base, modified: TFHIRObject; var html : String): TFHIRParametersW;
var
  list : TDifferenceList;
begin
  list := TDifferenceList.create;
  try
    generate(base.fhirType, base, modified, list);
    result := asParams(list);
    html := asHtml(list);
  finally
    list.free;
  end;
end;

function TDifferenceEngine.matchRating(obj1, obj2: TFHIRObject): Double;
var
  ol1, ol2 : TFHIRPropertyList;
  o1, o2 : TFHIRProperty;
  ov1, ov2 : TFHIRObject;
  t, c, i1, i2 : integer;
  m : boolean;
begin
  ol1 := obj1.createPropertyList(true);
  ol2 := obj2.createPropertyList(true);
  try
    t := 0;
    c := 0;
    for o1 in ol1 do
    begin
      o2 := ol2.ByName[o1.Name];
      o1.forceValues;
      o2.forceValues;
      if (o1.Values.Count > 0) or (o2.Values.Count > 0) then
      begin
        if o1.IsList then
        begin
          for i1 := 0 to o1.Values.Count - 1 do
          begin
            ov1 := o1.Values[i1] as TFHIRObject;
            inc(t);
            m := false;
            for i2 := 0 to o2.Values.Count - 1 do
            begin
              ov2 := o2.Values[0] as TFHIRObject;
              if (ov1.fhirType = ov2.fhirType) and ((ov1.getid = ov2.getid) or compareDeep(ov1, ov2, false)) then
                m := true;
            end;
            if (m) then
              inc(c);
          end;
        end
        else
        begin
          inc(t);
          if (o1.Values.Count > 0) and (o2.Values.Count > 0) then
          begin
            ov1 := o1.Values[0] as TFHIRObject;
            ov2 := o2.Values[0] as TFHIRObject;
            if (ov1.fhirType = ov2.fhirType) and ((ov1.getid = ov2.getid) or compareDeep(ov1, ov2, false)) then
              inc(c);
          end;
        end;
      end;
    end;
  finally
    ol1.Free;
    ol2.Free;
  end;
  if (t = 0) then
    result := 0
  else
    result := c / t;
end;

procedure TDifferenceEngine.populateObject(res: TFHIRObject; props: TFhirParametersParameterW);
var
  pp : TFhirParametersParameterW;
  v : TFHIRObject;
begin
  for pp in props.partList do
  begin
    if pp.value <> nil then
      res.setProperty(pp.name, pp.value.Link)
    else
    begin
      v := res.createPropertyValue(pp.name);
      try
        res.setProperty(pp.name, v);
        populateObject(v, pp);
      finally
        v.Free;
      end;
    end;
  end;
end;

procedure TDifferenceEngine.makeListChanges(path, name: String; matches: TDifferenceMatchList; bl, ml: TFHIRObjectList; differences: TDifferenceList);
var
  cb, cm : integer;
  dm, dm2 : TDifferenceMatch;
  ol : TOffSetList;
begin
  cb := 0;
  cm := 0;
  ol := TOffSetList.Create;
  try
    while (cb < bl.Count) or (cm < ml.Count) do
    begin
      if (cm = ml.Count) then
      begin
        // just delete the entry
        differences.delete(path+'.'+name+'['+inttostr(cm)+']');
        ol.inc(cb);
      end
      else
      begin
        // if the next target does not exists in the base list, create it
        dm := matches.matchedTarget(cm);
        if dm = nil then
        begin
          differences.insert(path+'.'+name, cm, ml[cm].Link as TFHIRObject);
          ol.Ins(cm);
        end
        else
        begin
          if dm.FSourceIndex = cb then // expected; nothing to move, just check for changes internally
          begin
            generate(path+'.'+Name+'['+inttostr(cm)+']', bl[cb] as TFHIRObject, ml[cm] as TFHIRObject, differences);
            ol.inc(cb);
          end
          else if dm.FSourceIndex < cb then
          begin
            raise ETodo.create('TDifferenceEngine.makeListChanges (<)');
            // actually, this can't happen (https://xkcd.com/2200/); a move forwards will become a series of moves backwards?
          end
          else
          begin
            // we're getting this ahead of where we are in bl. if this is because the next bl is deleted,
            dm2 := matches.matchedSource(cb);
            if (dm2 = nil) then
            begin
              // delete it, and try again
              differences.delete(path+'.'+name+'['+inttostr(cm)+']');
              ol.inc(cb);
              dec(cm); // correct for inc(cm) below - we don't want to in this caseu
            end
            else
            begin
              differences.move(path+'.'+name, ol.adjust(dm.FSourceIndex), cm);
              ol.skip(dm.FsourceIndex);
              ol.ins(cm);
              generate(path+'.'+Name+'['+inttostr(cm)+']', bl[dm.FSourceIndex] as TFHIRObject, ml[cm] as TFHIRObject, differences);
            end;
          end;
        end;
        inc(cm);
      end;
    end;
  finally
    ol.Free;
  end;
end;

procedure TDifferenceEngine.applyInsert(res : TFHIRObject; path: String; index: integer; value: TFhirParametersParameterW);
var
  dest : TFHIRSelectionList;
  v : TFHIRObject;
begin
  dest := fpe.evaluate(nil, res, path);
  try
    if dest.Count = 0 then
      raise EFHIRException.create('No content found at '+path+' when inserting');

    if value.value <> nil then
      dest[0].parent.insertProperty(dest[0].name, value.value.Link, index)
    else
    begin
      v := dest[0].value.createPropertyValue(dest[0].name);
      try
        dest[0].parent.insertProperty(dest[0].name, v.Link, index);
        populateObject(v, value);
      finally
        v.free;
      end;
    end;
  finally
    dest.Free;
  end;
end;

procedure TDifferenceEngine.applyMove(res : TFHIRObject; path: String; source, destination: integer);
var
  dest : TFHIRSelectionList;
begin
  dest := fpe.evaluate(nil, res, path);
  try
    if dest.Count = 0 then
      raise EFHIRException.create('No content found at '+path+' when moving');
    if dest.Count < 2 then
      raise EFHIRException.create('Only a single location found at '+path+' when moving');
    if dest[0].parent = nil then
      raise EFHIRException.create('Content returned from Path is not part of Resource');
    dest[0].parent.reorderProperty(dest[0].name, source, destination);
  finally
    dest.Free;
  end;
end;

function TDifferenceEngine.applyOperation(res: TFHIRObject; op: TFhirParametersParameterW) : boolean;
var
  t : string;
  d : TDifferenceOperation;
begin
  t := op.str['type'];
  d := TDifferenceOperation(StringArrayIndexOfSensitive(CODES_DIFF_OP, t));
  case d of
    diffAdd :     applyAdd(res, op.str['path'], op.str['name'], op.param['value']);
    diffInsert :  applyInsert(res, op.str['path'], StrToInt(op.str['index']), op.param['value']);
    diffDelete :  applyDelete(res, op.str['path']);
    diffReplace : applyReplace(res, op.str['path'], op.param['value']);
    diffMove :    applyMove(res, op.str['path'], StrToInt(op.str['source']), StrToInt(op.str['destination']));
  else
    raise EFHIRException.create('Unknown Operation '+t);
  end;
  result := d = diffDelete;
end;

procedure TDifferenceEngine.applyReplace(res : TFHIRObject; path: String; value: TFhirParametersParameterW);
var
  dest : TFHIRSelectionList;
  v : TFHIRObject;
begin
  dest := fpe.evaluate(nil, res, path);
  try
    if dest.Count = 0 then
      raise EFHIRException.create('No content found at '+path+' when adding');
    if dest.Count > 1 then
      raise EFHIRException.create('Multiple locations found at '+path+' when adding');
    if dest[0].parent = nil then
      raise EFHIRException.create('Content returned from Path is not part of Resource');
    if value.value <> nil then
      dest[0].parent.replaceProperty(dest[0].name, dest[0].value, value.value.Link)
    else
    begin
      v := dest[0].parent.createPropertyValue(dest[0].name);
      try
        dest[0].parent.replaceProperty(dest[0].name, dest[0].value, v.Link);
        populateObject(v, value);
      finally
        v.Free;
      end;
    end;
  finally
    dest.Free;
  end;
end;

function TDifferenceEngine.asHtml(differences: TDifferenceList): string;
var
  b : TStringBuilder;
  diff : TDifference;
begin
  b := TStringBuilder.Create;
  try
    b.append('<table>'#13#10);
    b.Append('<tr>');
    b.Append('<td><b>Operation</b></td>');
    b.Append('<td><b>Path</b></td>');
    b.Append('<td><b>Details</b></td>');
    b.Append('<td><b>Value</b></td>');
    b.Append('</tr>'#13#10);

    for diff in differences do
    begin
      b.Append('<tr>');
      if diff.FOp = diffDelete then
      begin
        b.Append('<td>Delete</td>');
        b.Append('<td>'+diff.Path+'</td>');
        b.Append('<td></td>');
        b.Append('<td></td>');
      end
      else if diff.FOp = diffMove then
      begin
        b.Append('<td>Move</td>');
        b.Append('<td>'+diff.Path+'</td>');
        b.Append('<td>'+inttostr(diff.Index)+'-&gt; '+inttostr(diff.Index2)+'</td>');
        b.Append('<td></td>');
      end
      else
      begin
        b.Append('<td>'+CODES_DIFF_OP[diff.FOp]+'</td>');
        b.Append('<td>'+diff.Path+'</td>');
        if diff.Name <> '' then
          b.Append('<td>'+diff.Name+'</td>')
        else if diff.FOp = diffInsert then
          b.Append('<td>['+inttostr(diff.Index)+']</td>')
        else
          b.Append('<td></td>');
        b.Append('<td>'+asValue(diff.Value)+'</td>');
      end;
      b.Append('</tr>'#13#10);
    end;
    b.append('</table>'#13#10);
    result := b.ToString;
  finally
    b.Free;
  end;
end;

function TDifferenceEngine.asParams(differences: TDifferenceList): TFHIRParametersW;
var
  diff : TDifference;
  p, pp : TFhirParametersParameterW;
begin
  result := FFactory.makeParameters;
  try
    for diff in differences do
    begin
      if diff.FOp = diffDelete then
      begin
        p := result.addParam('operation');
        pp := p.addParam('type');
        pp.value := FFactory.makeCode(CODES_DIFF_OP[diffDelete]);
        pp := p.addParam('path');
        pp.value := FFactory.makeString(diff.Path);
      end
      else if diff.FOp = diffMove then
      begin
        p := result.addParam('operation');
        pp := p.addParam('type');
        pp.value := FFactory.makeCode(CODES_DIFF_OP[diffMove]);
        pp := p.addParam('path');
        pp.value := FFactory.makeString(diff.Path);
        pp := p.addParam('source');
        pp.value := FFactory.makeInteger(inttostr(diff.Index));
        pp := p.addParam('destination');
        pp.value := FFactory.makeInteger(inttostr(diff.Index2));
      end
      else
      begin
        p := result.addParam('operation');
        pp := p.addParam('type');
        pp.value := FFactory.makeCode(CODES_DIFF_OP[diff.FOp]);
        pp := p.addParam('path');
        pp.value := FFactory.makeString(diff.Path);
        if diff.Name <> '' then
        begin
          pp := p.addParam('name');
          pp.value := FFactory.makeString(diff.Name);
        end;
        if diff.FOp = diffInsert then
        begin
          pp := p.addParam('index');
          pp.value := FFactory.makeInteger(inttostr(diff.Index));
        end;
        pp := p.addParam('value');
        encodeValue(pp, diff.Value);
      end;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TDifferenceEngine.asValue(value: TFHIRObject): string;
var
  pl : TFHIRPropertyList;
  c : TFHIRComposer;
  b : TStringBuilder;
begin
  if value.isEnum then
    result := FormatTextToXml(value.primitiveValue, xmlText)
  else if (value is TFhirXHtmlNode) then
    result := FormatTextToXml(TFHIRXhtmlParser.compose(TFhirXHtmlNode(value)), xmlText)
  else if (value.isType) and (Value.isPrimitive) then
    result := FormatTextToXml(Value.primitiveValue, xmlText)
  else if (value.isType) and StringArrayExistsSensitive(['Annotation', 'Attachment', 'Identifier', 'CodeableConcept', 'Coding', 'Quantity', 'Range', 'Period', 'Ratio', 'SampledData', 'Signature', 'HumanName', 'Address', 'ContactPoint', 'Timing', 'Reference', 'Meta'], Value.fhirType) then
  begin
    c := FFactory.makeComposer(FContext.link, ffJson, FContext.lang, OutputStyleNormal);
    try
      result := c.Compose('value', value);
    finally
      c.Free;
    end;
  end
  else
  begin
    b := TStringBuilder.Create;
    try
      b.Append('{');
      // an anonymous type. So what we do here is create the value, but the value
      // part has no actual value. instead, it contains parts for the properties
      pl := value.createPropertyList(true);
      try
//        for p in pl do
//        begin
//          p.forceValues;
//          for b in p.Values do
//          begin
//            pp := part.partList.Append;
//            pp.name := p.Name;
//            encodeValue(pp, b as TFHIRObject);
//          end;
//        end;
        b.Append('}');
      finally
        pl.free;
      end;
      result := b.ToString;
    finally
      b.free;
    end;
  end;

end;

constructor TDifferenceEngine.Create(context: TFHIRWorkerContextV; factory : TFHIRFactory);
begin
  inherited create;
  FFactory := factory;
  FContext := context;
  fpe := FFactory.makePathEngine(context.link, nil);
end;

destructor TDifferenceEngine.Destroy;
begin
  fpe.Free;
  FContext.Free;
  FFactory.free;
  inherited;
end;

procedure TDifferenceEngine.encodeValue(part: TFhirParametersParameterW; value: TFHIRObject);
var
  pl : TFHIRPropertyList;
  p : TFHIRProperty;
  b : TFHIRObject;
  pp : TFhirParametersParameterW;
begin
  if value.isEnum then
    part.value := FFactory.makeCode(value.primitiveValue)
  else if (value is TFhirXHtmlNode) then
  begin
    part.value := FFactory.makeString(TFHIRXhtmlParser.compose(TFhirXHtmlNode(value)));
  end
  else if (value.isType) and (Value.isPrimitive or StringArrayExistsSensitive(['Annotation', 'Attachment', 'Identifier', 'CodeableConcept', 'Coding', 'Quantity', 'Range', 'Period', 'Ratio', 'SampledData', 'Signature', 'HumanName', 'Address', 'ContactPoint', 'Timing', 'Reference', 'Meta'], Value.fhirType)) then
    part.value := Value.Link
  else
  begin
    // an anonymous type. So what we do here is create the value, but the value
    // part has no actual value. instead, it contains parts for the properties
    pl := value.createPropertyList(false);
    try
      for p in pl do
      begin
        p.forceValues;
        for b in p.Values do
        begin
          pp := part.addParam(p.Name);
          encodeValue(pp, b as TFHIRObject);
        end;
      end;
    finally
      pl.free;
    end;
  end;
end;

procedure TDifferenceEngine.findCertainMatches(matches: TDifferenceMatchList; bl, ml: TFHIRObjectList);
var
  bi, mi : integer;
  bv, mv : TFHIRObject;
begin
  for bi := 0 to bl.Count - 1 do
  begin
    for mi := 0 to ml.Count - 1 do
    begin
      if matches.matchedTarget(mi) = nil then
      begin
        mv := ml[mi] as TFHIRObject;
        bv := bl[bi] as TFHIRObject;
        if (bv.fhirType = mv.fhirType) then
        begin
          if (bv.getId <> '') and (bv.getid = mv.getid) then
             matches.Add(TDifferenceMatch.Create(bi, mi))
          else if compareDeep(bv, mv, false) then
             matches.Add(TDifferenceMatch.Create(bi, mi))
        end;
      end;
    end;
  end;
end;

procedure TDifferenceEngine.findPossibleMatches(matches: TDifferenceMatchList; bl, ml: TFHIRObjectList);
var
  bi, mi : integer;
  bv, mv : TFHIRObject;
begin
  for bi := 0 to bl.Count - 1 do
  begin
    if matches.matchedSource(bi) = nil then
    begin
      for mi := 0 to ml.Count - 1 do
      begin
        if matches.matchedTarget(mi) = nil then
        begin
          mv := ml[mi] as TFHIRObject;
          bv := bl[bi] as TFHIRObject;
          if (bv.fhirType = mv.fhirType) then
          begin
            if matchRating(mv, bv) > 0.5 then
              matches.Add(TDifferenceMatch.Create(bi, mi))
          end;
        end;
      end;
    end;
  end;
end;

procedure TDifferenceEngine.generate(path : String; base, modified : TFHIRObject; differences : TDifferenceList);
var
  bl, ml : TFHIRPropertyList;
  b, m : TFHIRProperty;
  bv, mv : TFHIRObject;
  i : integer;
  matches : TDifferenceMatchList;
  n : String;
begin
  if base.fhirType <> modified.fhirType then
    raise EFHIRException.create('Unable to generate difference for different types ('+base.fhirType+'/'+modified.fhirType+')');
  bl := base.createPropertyList(true);
  ml := modified.createPropertyList(true);
  try
    for b in bl do
    begin
      m := ml.ByName[b.Name];
      b.forceValues;
      m.forceValues;
      if b.IsList and ((b.Values.Count > 1) or (m.Values.Count > 1)) then
      begin
        matches := TDifferenceMatchList.create;
        try
          // map the certain matches between lists
          findCertainMatches(matches, b.Values, m.Values);
          // map the probabilistic matches between the lists
          findPossibleMatches(matches, b.Values, m.Values);
          if matches.listUnchanged(b.Values.Count, m.Values.Count) then
            for i := 0 to b.Values.Count - 1 do
              generate(path+'.'+b.Name+'['+inttostr(i)+']', b.Values[i] as TFHIRObject, m.Values[i] as TFHIRObject, differences)
          else
            makeListChanges(path, b.Name, matches, b.Values, m.Values, differences);
        finally
          matches.Free;
        end;
      end
      else
      begin
        if b.IsList then
          n := b.Name+'[0]'
        else
          n := b.Name;
        if b.Values.Count = 0 then
        begin
          if m.Values.Count <> 0 then
            differences.add(path, b.Name, m.Values[0].link as TFHIRObject);
        end
        else
        begin
          if m.Values.Count = 0 then
            differences.delete(path+'.'+n)
          else
          begin
            bv := b.Values[0] as TFHIRObject;
            mv := m.Values[0] as TFHIRObject;
            // If their type is different, replace
            // if their id is different, replace
            if (bv.fhirType <> mv.fhirType) or (bv.getid <> mv.getid) then
              differences.replace(path+'.'+n, mv.link)
            // if it's a primitive, replace
            else if bv.isPrimitive then
            begin
              if not compareDeep(bv, mv, false) then
                differences.replace(path+'.'+n, mv.Link)
            end
            // otherwise, generate for the type
            else
              generate(path+'.'+n, bv, mv, differences);
          end;
        end;
      end;
    end;
  finally
    bl.Free;
    ml.Free;
  end;
end;

function TDifferenceEngine.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFactory.sizeInBytes);
  inc(result, FContext.sizeInBytes);
  inc(result, fpe.sizeInBytes);
end;

{ TOffSetList }

function TOffSetList.adjust(index: integer): integer;
var
  o : TOffset;
begin
  result := index;
  for o in self do
  begin
    if (o.index < result) then
      if (o.FInserted) then
        result := result + 1
      else
        result := result - 1;
  end;
end;

function TOffSetList.hasUsed(index: integer): boolean;
var
  o : TOffset;
begin
  result := false;
  for o in Self do
    if (o.FIndex = index) and (not o.FInserted) then
     exit(true);
end;

procedure TOffSetList.inc(var index: integer);
begin
  System.inc(index);
  while (hasUsed(index)) do
    System.inc(index);
end;

procedure TOffSetList.ins(index: integer);
var
  o : TOffset;
begin
  o := TOffset.Create;
  add(o);
  o.FIndex := index;
  o.FInserted := true;
end;

procedure TOffSetList.skip(index: integer);
var
  o : TOffset;
begin
  o := TOffset.Create;
  add(o);
  o.FIndex := index;
  o.FInserted := false;
end;

function TOffset.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

end.
