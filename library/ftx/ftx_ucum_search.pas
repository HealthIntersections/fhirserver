Unit ftx_ucum_search;

{
Copyright (c) 2001+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$I fhir.inc}

Interface

Uses
  {$IFDEF DELPHI} RegularExpressions, {$ENDIF}
  fsl_utilities, fsl_collections, fsl_fpc,
  fsl_base,
  ftx_ucum_base;

Type
  TUcumSearch = class (TFslObject)
  Private
    FRegex : TRegEx;
    Procedure searchUnits(model : TUcumModel; concepts : TFslList<TUcumConcept>; units : TFslMap<TUcumBaseUnit>; text : String; isRegex : boolean); overload;
    Procedure searchUnits(model : TUcumModel; concepts : TFslList<TUcumConcept>; units : TFslMap<TUcumDefinedUnit>; text : String; isRegex : boolean); overload;
    function matchesUnit(model : TUcumModel; oUnit : TUcumUnit; text : String; isRegex : boolean) : boolean;
    Procedure searchPrefixes(concepts : TFslList<TUcumConcept>; prefixes : TFslList<TUcumPrefix>; text : String; isRegex : boolean);
    function matchesConcept(concept : TUcumConcept; text : String; isRegex : boolean) : boolean;
    function matches(value : String; text : String; isRegex : boolean) : boolean;
  Public
    Function doSearch(model : TUcumModel; aKind : TConceptKind; text : String; isRegex : Boolean) : TFslList<TUcumConcept>;
  End;


Implementation


Function TUcumSearch.doSearch(model : TUcumModel; aKind : TConceptKind; text : String; isRegex : Boolean) : TFslList<TUcumConcept>;
begin
  if isRegex Then
  begin
    FRegex := TRegEx.Create(text, [roCompiled]);
  End;


  result := TFslList<TUcumConcept>.Create;
  Try
    if (akind = UcumNull) or (akind = UcumPREFIX) Then
      searchPrefixes(result, model.prefixes, text, isRegex);
    if (akind = UcumNull) or (akind = UcumBASEUNIT) Then
      searchUnits(model, result, model.baseUnits, text, isRegex);
    if (akind = UcumNull) or (akind = UcumUNIT) Then
      searchUnits(model, result, model.DefinedUnits, text, isRegex);
    result.Link;
  Finally
    result.Free;
  End;
end;

Procedure TUcumSearch.searchUnits(model : TUcumModel; concepts : TFslList<TUcumConcept>; units : TFslMap<TUcumBaseUnit>; text : String; isRegex : boolean);
var
  bu : TUcumBaseUnit;
begin
  for bu in units.Values do
    if matchesUnit(model, bu, text, isRegex) Then
      concepts.add(bu.Link);
end;

Procedure TUcumSearch.searchUnits(model : TUcumModel; concepts : TFslList<TUcumConcept>; units : TFslMap<TUcumDefinedUnit>; text : String; isRegex : boolean);
var
  bu : TUcumDefinedUnit;
begin
  for bu in units.Values do
    if matchesUnit(model, bu, text, isRegex) Then
      concepts.add(bu.Link);
end;

function TUcumSearch.matchesUnit(model : TUcumModel; oUnit : TUcumUnit; text : String; isRegex : boolean) : boolean;
begin
 result := matches(model.Properties[oUnit.PropertyType].Name, text, isRegex) or matchesConcept(oUnit, text, isRegex);
end;


Procedure TUcumSearch.searchPrefixes(concepts : TFslList<TUcumConcept>; prefixes : TFslList<TUcumPrefix>; text : String; isRegex : boolean);
var
  i : integer;
begin
  for i := 0 to prefixes.Count - 1 Do
    if (matchesConcept(prefixes[i], text, isRegex)) Then
      concepts.add(prefixes[i].Link);
end;

function TUcumSearch.matchesConcept(concept : TUcumConcept; text : String; isRegex : boolean) : boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to concept.Names.Count - 1 Do
    if matches(concept.Names[i], text, isRegex) Then
      result := true;

  result := result or matches(concept.Code, text, isRegex);
  result := result or matches(concept.CodeUC, text, isRegex);
  result := result or matches(concept.PrintSymbol, text, isRegex);
end;

function TUcumSearch.matches(value : String; text : String; isRegex : boolean) : boolean;
begin
  if isRegex then
    result := FRegex.IsMatch(text)
  else
    result := StringExistsInsensitive(value, text);
End;

End.
