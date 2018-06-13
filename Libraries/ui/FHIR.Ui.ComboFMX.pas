unit FHIR.Ui.ComboFMX;

{
Copyright (c) 2010+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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
  Classes,
  FHIR.Support.Strings,
  FHIR.Version.Resources,
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
