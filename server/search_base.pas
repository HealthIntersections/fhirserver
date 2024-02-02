unit search_base;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  fsl_base, fsl_utilities, fsl_http,
  fhir_objects, fhir_common,
  fhir_indexing, indexing;{,
  FHIR.Version.Types, fhir_objects, FHIR.Version.PathEngine, , , FHIR.Version.Utilities}

Type
  TFHIRSearchParamModifier = (spmNull, spmMissing, spmExact, spmContains, spmText, spmIn, spmBelow, spmAbove, spmNotIn, spmType);
  TFHIRSearchParamPrefix = (sppNull, sppNotEqual, sppGreaterThan, sppLessThan, sppGreaterOrEquals, sppLesserOrEquals, sppStartsAfter, sppEndsBefore, sppAproximately);

  TFHIRSearchControlParameter = (scpNull, scpContained, scpContainedType, scpCount, scpElements, scpGraph, scpInclude, scpMaxResults, scpRevInclude, scpScore, scpSort, scpSummary, scpTotal);
  TFHIRSearchControlParameterSet = set of TFHIRSearchControlParameter;

  TSearchParameter = class (TFslObject)
  private
    FModifier: TFHIRSearchParamModifier;
    FControl : TFHIRSearchControlParameter;
    FValue: String;
    FNext: TSearchParameter;
    FIndex: TFhirIndex;
    FNamespace: String;
    FPrefix: TFHIRSearchParamPrefix;
    FModifierType: String;
    procedure SetNext(const Value: TSearchParameter);
    procedure SetIndex(const Value: TFhirIndex);
    procedure checkOrderingPrefix;
    procedure processPrefix;

    procedure build(b : TFslStringBuilder);
  public
    destructor Destroy; override;
    function link : TSearchParameter; overload;

    property index : TFhirIndex read FIndex write SetIndex;
    property control : TFHIRSearchControlParameter read FControl write FControl;
    property value : String read FValue write FValue;
    property modifier : TFHIRSearchParamModifier read FModifier write FModifier;
    property prefix : TFHIRSearchParamPrefix read FPrefix write FPrefix;
    property namespace : String read FNamespace write FNamespace;
    property modifierType : String read FModifierType write FModifierType;

    property next : TSearchParameter read FNext write SetNext;
  end;

  { TSearchParser }

  TSearchParser = class (TFslObject)
  private
    FElements : TStringList;
    FAllowedParams : TStringList;
    FSearchControls : TFHIRSearchControlParameterSet;
    function processParam(indexes : TFHIRIndexInformation; resourceType, name, value : String) : TSearchParameter;
  public
    constructor Create(searchControls : TFHIRSearchControlParameterSet);
    destructor Destroy; override;

    function parse(indexes : TFHIRIndexInformation; resourceType : String; pm : THTTPParameters) : TFslList<TSearchParameter>;
    function buildUrl(base : String; search : TFslList<TSearchParameter>): String;
    property Elements : TStringList read FElements;
    property allowedParams : TStringList read FAllowedParams;
  end;

const
  CODES_TFHIRSearchControlParameter : Array[TFHIRSearchControlParameter] of String =
    ('', '_contained', '_containedType', '_count', '_elements', '_graph', '_include', '_maxresults', '_revinclude', '_score', '_sort', '_summary', '_total');

implementation

{ TSearchParameter }

procedure TSearchParameter.build(b: TFslStringBuilder);
begin
  if (FControl <> scpNull) then
    b.append(CODES_TFHIRSearchControlParameter[FControl])
  else
  begin
    b.Append(index.Name);
    case modifier of
      spmMissing: b.Create.Append(':missing');
      spmExact: b.Create.Append(':exact');
      spmContains: b.Create.Append(':contains');
      spmText: b.Create.Append(':text');
      spmIn: b.Create.Append(':in');
      spmBelow: b.Create.Append(':below');
      spmAbove: b.Create.Append(':above');
      spmNotIn: b.Create.Append(':not-in');
      spmType: b.Append(':'+modifierType);
    end;
  end;
  if next <> nil then
  begin
    b.Append('.');
    next.build(b);
  end
  else
  begin
    b.Append('=');
    case prefix of
     sppNotEqual : b.Append('ne');
     sppGreaterThan : b.Append('gt');
     sppLessThan : b.Append('lt');
     sppGreaterOrEquals : b.Append('ge');
     sppLesserOrEquals : b.Append('le');
     sppStartsAfter : b.Append('sa');
     sppEndsBefore : b.Append('eb');
     sppAproximately : b.Append('ap');
    end;
    b.Append(FValue);
  end;
end;

procedure TSearchParameter.checkOrderingPrefix;
begin
  if value.startsWith('eq') then
    value := value.substring(2)
  else if value.StartsWith('ne') then
  begin
    prefix := sppNotEqual;
    value := value.Substring(2);
  end
  else if value.StartsWith('gt') then
  begin
    prefix := sppGreaterThan;
    value := value.Substring(2);
  end
  else if value.StartsWith('lt') then
  begin
    prefix := sppLessThan;
    value := value.Substring(2);
  end
  else if value.StartsWith('ge') then
  begin
    prefix := sppGreaterOrEquals;
    value := value.Substring(2);
  end
  else if value.StartsWith('le') then
  begin
    prefix := sppLesserOrEquals;
    value := value.Substring(2);
  end
  else if value.StartsWith('sa') then
  begin
    prefix := sppStartsAfter;
    value := value.Substring(2);
  end
  else if value.StartsWith('eb') then
  begin
    prefix := sppEndsBefore;
    value := value.Substring(2);
  end
  else if value.StartsWith('ap') then
  begin
    prefix := sppAproximately;
    value := value.Substring(2);
  end;
end;

destructor TSearchParameter.Destroy;
begin
  FIndex.free;
  FNext.free;
  inherited;
end;

function TSearchParameter.link: TSearchParameter;
begin
  result := TSearchParameter(inherited Link);
end;

procedure TSearchParameter.processPrefix;
begin
  case index.SearchType of
    sptNull : raise EFHIRException.Create('Unknown parameter type for '+Index.Name);
    sptNumber :
      begin
      checkOrderingPrefix;
      if not StringIsInteger32(value) then
        raise EFHIRException.Create('Numerical Parameter value "value" is not an integer');
      end;
    sptDate :
      begin
      checkOrderingPrefix;
      if not TFslDateTime.isValidXmlDate(value) then
        raise EFHIRException.Create('Numerical Parameter value "value" is not a date');
      end;
    sptString : ; // nothing
    sptToken :
      begin
        if value.Contains('|') then
          StringSplit(value, '|', FNamespace, FValue);
      end;
    sptReference : ; // nothing
    sptComposite : raise EFHIRException.Create('composite parameters not done yet');
    sptQuantity : raise EFHIRException.Create('quantity parameters not done yet');
    sptUri : ; // nothing
  end;
end;

procedure TSearchParameter.SetIndex(const Value: TFhirIndex);
begin
  FIndex.free;
  FIndex := Value;
end;

procedure TSearchParameter.SetNext(const Value: TSearchParameter);
begin
  Fnext.free;
  Fnext := Value;
end;

{ TSearchParser }

function TSearchParser.buildUrl(base : String; search: TFslList<TSearchParameter>): String;
var
  b : TFslStringBuilder;
  sp : TSearchParameter;
begin
  b := TFslStringBuilder.Create;
  try
    b.append(base);
    b.append('?');
    for sp in search do
    begin
      b.Append('&');
      sp.build(b);
    end;
    result := b.ToString;
  finally
    b.free;
  end;

end;

function TSearchParser.parse(indexes : TFHIRIndexInformation; resourceType : String; pm: THTTPParameters): TFslList<TSearchParameter>;
var
  iName, iValue : integer;
  name, value : String;
  sp : TSearchParameter;
begin
  result := TFslList<TSearchParameter>.Create;
  try
    for iName := 0 to pm.Count - 1 do
    begin
      name := pm.Name[iName];
      for iValue := 0 to pm.getValueCount(iName) - 1 do
      begin
        pm.retrieveNumberedItem(iName, iValue, value);
        if value <> '' then
        begin
          sp := processParam(indexes, resourceType, name, value);
          if sp <> nil then
            result.Add(sp)
        end;
      end;
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TSearchParser.processParam(indexes : TFHIRIndexInformation; resourceType, name, value: String): TSearchParameter;
var
  l, r, n, m : String;
  index : TFhirIndex;
  cp : TFHIRSearchControlParameter;
  ok : boolean;
begin
  result := nil;
  StringSplit(name, '.', l, r);
  if l.contains(':') then
  begin
    StringSplit(l, ':', n, m);
  end
  else
    n := l;

  index := indexes.Indexes.getByName(resourceType, n);
  if (index <> nil) and ((FAllowedParams.Count = 0) or (FAllowedParams.indexOf(n) > -1)) then
    begin
    result := TSearchParameter.Create;
    try
      result.index := index.Link;
      result.control := scpNull;

      if (r <> '') then
      begin
        if index.SearchType <> sptReference then
          raise EFHIRException.Create('chained, but not a reference: '+name);
        if length(index.TargetTypes) <> 1 then
          raise EFHIRException.Create('not handled yet');
        result.next := processParam(indexes, index.TargetTypes[0], r, value);
      end
      else
      begin
        result.value := value;
        result.processPrefix;
        if (m = 'missing') then
          result.modifier := spmMissing
        else if (m = 'exact') then
          result.modifier := spmExact
        else if (m = 'contains') then
          result.modifier := spmContains
        else if (m = 'text') then
          result.modifier := spmText
        else if (m = 'in') then
          result.modifier := spmIn
        else if (m = 'below') then
          result.modifier := spmBelow
        else if (m = 'above') then
          result.modifier := spmAbove
        else if (m = 'not-in') then
          result.modifier := spmNotIn
        else if (m = '') then
          result.modifier := spmNull
        else if indexes.factory.isResourceName(m) then
        begin
          result.modifier := spmType;
          result.modifierType := m;
        end
        else
          raise EFHIRException.Create('Unknown Modifier '+m);
      end;

      result.Link;
    finally
      result.free;
    end;
  end
  else if (StringArrayExists(CODES_TFHIRSearchControlParameter, n)) then
  begin
    cp := TFHIRSearchControlParameter(StringArrayIndexOf(CODES_TFHIRSearchControlParameter, n));
    ok := false;
    if (cp in FSearchControls) then
    begin
      case cp of
        scpContained : ok := StringArrayExists(['true', 'false', 'both'], value);
        scpContainedType : ok := StringArrayExists(['conainer', 'contained'], value);
        scpCount : ok := StringIsInteger32(value);
        scpElements : ok := true;
        scpGraph : ok := true; // todo: try to resolve it?
        scpInclude : ok := true; // todo: validate this
        scpMaxResults : ok := StringIsInteger32(value);
        scpRevInclude : ok := true; // todo: validate this
        scpScore : ok := StringArrayExists(['true', 'false'], value);
        scpSort : ok := true; // todo: validate this
        scpSummary : ok := StringArrayExists(['true', 'false'], value);
        scpTotal : ok := StringArrayExists(['none', 'estimate', 'accurate'], value);
      end;
    end;
    if (ok) then
    begin
      result := TSearchParameter.Create;
      result.control := cp;
      result.value := value;
      if (cp = scpElements) then
        FElements.CommaText := value;
    end;
  end;
end;

constructor TSearchParser.Create(searchControls: TFHIRSearchControlParameterSet);
begin
  inherited Create;
  FSearchControls := searchControls;
  FAllowedParams := TStringList.create;
  FElements := TStringList.create;
end;

destructor TSearchParser.Destroy;
begin
  FAllowedParams.free;
  FElements.free;
  inherited Destroy;
end;

end.
