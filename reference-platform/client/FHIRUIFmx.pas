unit FHIRUIFmx;

interface

uses
  Classes,
  FHIRResources, FHIRUI,
  FMX.ListBox;

type
  TComboBoxHelper = class helper for TCombobox
  private
    function GetValueSet: TFHIRValueSet;
    procedure SetValueSet(const Value: TFHIRValueSet);
    function GetSearchCode: String;
    function GetCode: String;
  public
    property ValueSet : TFHIRValueSet read GetValueSet write SetValueSet;
    property SearchCode : String read GetSearchCode;
    property code : String read GetCode;
  end;



implementation

{ TComboBoxHelper }

function TComboBoxHelper.GetCode: String;
var
  concept : TFhirValueSetExpansionContains;
begin
  result := '';
  if ItemIndex > -1 then
  begin
    concept := items.Objects[itemIndex] as TFhirValueSetExpansionContains;
    if concept <> nil then
      result := concept.code;
  end;
end;

function TComboBoxHelper.GetSearchCode: String;
var
  concept : TFhirValueSetExpansionContains;
begin
  result := '';
  if ItemIndex > -1 then
  begin
    concept := items.Objects[itemIndex] as TFhirValueSetExpansionContains;
    if concept <> nil then
      result := concept.system+'|'+concept.code;
  end;
end;

function TComboBoxHelper.GetValueSet: TFHIRValueSet;
begin
  result := TagObject as TFhirValueSet;
end;

procedure TComboBoxHelper.SetValueSet(const Value: TFHIRValueSet);
var
  concept : TFhirValueSetExpansionContains;
  ts : TStringList;
begin
  TagObject := Value;
  ts := TStringList.Create;
  try
    ts.AddObject('', nil);
    for concept in Value.expansion.containsList do
      ts.AddObject(concept.display, concept);
    ts.Sort;
    items.assign(ts);
  finally
    ts.Free;
  end;
end;

end.
