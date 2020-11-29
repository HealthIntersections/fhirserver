unit ftx_ucum_publisher;

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
  SysUtils, Classes, Math,
  fsl_base, fsl_utilities,
  wp_documents, ftx_ucum_base;

Const
  MAX_ROWS = 200;

Type
  TUcumPublisher = class (TFslObject)
  Private
    FUcum : TUcumServices;
    Procedure UcumHeading(oPublisher : THL7v2DOcumentPublisher; sTitle, sPrefix : String);
    Procedure ProcessMap(Const sPath : String; oMap : TFslStringMatch);
    Procedure PublishDictInternal(oMap : TFslStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
    procedure PublishHome(const sPrefix: String; oBuilder: THL7V2DocumentPublisher);
    procedure PublishAllUnits(const sPrefix: String; oBuilder: THL7V2DocumentPublisher);
    procedure PublishSpecialUnits(const sPrefix: String; oBuilder: THL7V2DocumentPublisher);
    procedure PublishProperty(const sPrefix: String; oBuilder: THL7V2DocumentPublisher; sProperty : String);
    procedure PublishUnit(const sPrefix: String; oBuilder: THL7V2DocumentPublisher; oUnit : TUcumUnit);
    procedure PublishExpression(const sPrefix: String; oBuilder: THL7V2DocumentPublisher; sExpression : String);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(oUcum : TUcumServices);
    destructor Destroy; Override;
    Procedure PublishDict(Const sPath, sPrefix : String; oBuilder : THL7V2DocumentPublisher); Overload; Virtual;
    Procedure PublishDict(oMap : TFslStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher); Overload; Virtual;
  End;

Implementation

Function ShowUnit(s : String; sPost : String = ''):String;
Begin
  if s = '1' then
    result := ''
  else
    result := s+sPost;
End;

Procedure TUcumPublisher.PublishDictInternal(oMap : TFslStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
Var
  sURL : String;
  sType : String;
  sId : String;
Begin
  sURL := sPrefix +'?type=ucum&';
  sType := oMap.Matches['prop'];
  sId := oMap.Matches['id'];
  if (sType = 'unit') And (sId = '') Then
    PublishAllUnits(sURL, oBuilder)
  else if sType = 'expression' Then
    PublishExpression(sURL, oBuilder, oMap.Matches['unit'])
  else if (sType = 'prop') And (sId <> '') Then
    PublishProperty(sURL, oBuilder, sId)
  else if (sType = 'unit') And (sId <> '') Then
    if FUcum.Model.baseUnits.existsByCode(sId) Then
      PublishUnit(sURL, oBuilder, FUcum.Model.baseUnits.GetByCode(sId))
    else if FUcum.Model.DefinedUnits.existsByCode(sId) Then
      PublishUnit(sURL, oBuilder, FUcum.Model.DefinedUnits.GetByCode(sId))
    else if sId = 'special' Then
      PublishSpecialUnits(sURL, oBuilder)
    else
      raise ETerminologyError.create('unable to find unit '+sId)
  Else
    PublishHome(sURL, oBuilder);
End;


Procedure TUcumPublisher.ProcessMap(Const sPath : String; oMap : TFslStringMatch);
Var
  sLeft, sRight : String;
  sName, sValue : String;
Begin
  Stringsplit(sPath, '?', sLeft, sRight);
  oMap.Forced := True;
  While sRight <> '' Do
  Begin
    StringSplit(sRight, '&', sLeft, sRight);
    StringSplit(sLeft, '=', sName, sValue);
    oMap.Matches[DecodePercent(sName)] := DecodePercent(sValue);
  End;
End;


Procedure TUcumPublisher.PublishDict(Const sPath, sPrefix : String; oBuilder : THL7V2DocumentPublisher);
Var
  oMap : TFslStringMatch;
Begin
  oMap := TFslStringMatch.Create;
  Try
    ProcessMap(sPath, oMap);
    PublishDict(oMap, sPrefix, oBuilder);
  Finally
    oMap.Free;
  End;
End;



procedure TUcumPublisher.PublishDict(oMap: TFslStringMatch; const sPrefix: String; oBuilder: THL7V2DocumentPublisher);
begin
  Try
    PublishDictInternal(oMap, sPrefix, oBuilder);
  Except
    On e:Exception Do
      Begin
      oBuilder.Title := 'Exception: '+e.Message;
      oBuilder.AddParagraph(e.Message);
      End;
  End;
end;

procedure TUcumPublisher.PublishHome(const sPrefix: String; oBuilder: THL7V2DocumentPublisher);
var
  i : Integer;
  o : TFslStringList;
Begin
  oBuilder.AddTitle('Unified Code for Units of Measure (UCUM)');

  if not FUcum.Loaded Then
  Begin
    oBuilder.StartParagraph;
    oBuilder.AddText('UCUM Definitions are not loaded', true, false);
    oBuilder.EndParagraph;
  End
  Else
  Begin
    oBuilder.ParaURL('Enter a unit', sPrefix+'prop=expression&unit=??&caption=Enter Units&prompt=Units');
    oBuilder.AddParagraph;

    oBuilder.StartTable(['Properties', 'Units']);
    oBuilder.StartTableRow;

    o := TFslStringList.Create;
    Try
      FUcum.getProperties(o);
      oBuilder.StartTableCell();
      for i := 0 to o.Count - 1 Do
        oBuilder.ParaURL(o[i], sPrefix+'prop=prop&id='+o[i], '', '');
      oBuilder.EndTableCell;
    Finally
      o.Free;
    End;

    oBuilder.StartTableCell();
    oBuilder.ParaURL('All Units', sPrefix+'prop=unit');
    oBuilder.ParaURL('Special Units', sPrefix+'prop=unit&id=special');
    oBuilder.AddParagraph();
    oBuilder.AddParagraph('Base Units:').Font.Italic := tsTrue;

    for i := 0 to FUcum.Model.baseUnits.Count - 1 Do
      oBuilder.ParaURL(FUcum.Model.baseUnits[i].names[0], sPrefix+'prop=unit&id='+FUcum.Model.baseUnits[i].code, '  ', '('+FUcum.Model.baseUnits[i].printSymbol+' : '+FUcum.Model.Properties[FUcum.Model.baseUnits[i].PropertyType].Name+'/'+FUcum.Model.baseUnits[i].dim+')');

    oBuilder.AddParagraph();
    oBuilder.AddParagraph('Prefixes').Font.Italic := tsTrue;
    for i := 0 to FUcum.Model.prefixes.Count - 1 Do
      oBuilder.AddParagraph('  '+FUcum.Model.prefixes[i].names[0]+'  ('+FUcum.Model.prefixes[i].printSymbol+' : * '+FUcum.Model.prefixes[i].Text+')');

    oBuilder.EndTableCell;



    oBuilder.EndTableRow;
    oBuilder.EndTable;
  End;
End;





procedure TUcumPublisher.PublishAllUnits(const sPrefix: String; oBuilder: THL7V2DocumentPublisher);
var
  i : Integer;

Begin
  UcumHeading(oBuilder, 'All Units', sPrefix);

  oBuilder.AddParagraph;

  oBuilder.StartTable(['Unit', 'Code', 'Print Symbol', 'Value', 'Property']);

  for i := 0 to FUcum.Model.baseUnits.Count - 1 Do
  Begin
    oBuilder.StartTableRow;
    oBuilder.AddTableCellURL(FUcum.Model.baseUnits[i].names[0], sPrefix+'prop=base&id='+FUcum.Model.baseUnits[i].code);
    oBuilder.AddTableCell(FUcum.Model.baseUnits[i].code);
    oBuilder.AddTableCell(FUcum.Model.baseUnits[i].printSymbol);
    oBuilder.AddTableCell('');
    oBuilder.AddTableCell(FUcum.Model.Properties[FUcum.Model.baseUnits[i].PropertyType].Name);
    oBuilder.EndTableRow;
  End;

  for i := 0 to FUcum.Model.definedUnits.Count - 1 Do
  Begin
    oBuilder.StartTableRow;
    oBuilder.AddTableCellURL(FUcum.Model.definedUnits[i].names[0], sPrefix+'prop=unit&id='+FUcum.Model.definedUnits[i].code);
    oBuilder.AddTableCell(FUcum.Model.definedUnits[i].code);
    oBuilder.AddTableCell(StringReplace(FUcum.Model.definedUnits[i].printSymbol, [#13, #10, #9], ' '));
    oBuilder.AddTableCell(StringReplace(ShowUnit(FUcum.Model.definedUnits[i].value.text, ' ')+ShowUnit(FUcum.Model.definedUnits[i].value.unit_), [#13, #10, #9], ' '));
    oBuilder.AddTableCellURL(FUcum.Model.Properties[FUcum.Model.definedUnits[i].PropertyType].Name, sPrefix+'prop=prop&id='+FUcum.Model.Properties[FUcum.Model.definedUnits[i].PropertyType].Name);
    oBuilder.EndTableRow;
  End;
  oBuilder.EndTable;
end;

procedure TUcumPublisher.PublishSpecialUnits(const sPrefix: String; oBuilder: THL7V2DocumentPublisher);
var
  i : Integer;
Begin
  UcumHeading(oBuilder, 'Special Units', sPrefix);

  oBuilder.AddParagraph;

  oBuilder.StartTable(['Unit', 'Code', 'Print Symbol', 'Value', 'Property']);

  for i := 0 to FUcum.Model.definedUnits.Count - 1 Do
    if FUcum.Model.definedUnits[i].isSpecial Then
    Begin
      oBuilder.StartTableRow;
      oBuilder.AddTableCellURL(FUcum.Model.definedUnits[i].names[0], sPrefix+'prop=unit&id='+FUcum.Model.definedUnits[i].code);
      oBuilder.AddTableCell(FUcum.Model.definedUnits[i].code);
      oBuilder.AddTableCell(StringReplace(FUcum.Model.definedUnits[i].printSymbol, [#13, #10, #9], ' '));
      oBuilder.AddTableCell(StringReplace(ShowUnit(FUcum.Model.definedUnits[i].value.text, ' ')+ShowUnit(FUcum.Model.definedUnits[i].value.unit_), [#13, #10, #9], ' '));
      oBuilder.AddTableCellURL(FUcum.Model.Properties[FUcum.Model.definedUnits[i].PropertyType].Name, sPrefix+'prop=prop&id='+FUcum.Model.Properties[FUcum.Model.definedUnits[i].PropertyType].Name);
      oBuilder.EndTableRow;
    End;
  oBuilder.EndTable;
end;

procedure TUcumPublisher.PublishProperty(const sPrefix: String; oBuilder: THL7V2DocumentPublisher; sProperty: String);
var
  i : Integer;
Begin
  UcumHeading(oBuilder, 'Property '+sProperty, sPrefix);

  oBuilder.AddParagraph;

  oBuilder.StartTable(['Unit', 'Code', 'Print Symbol', 'Value']);

  for i := 0 to FUcum.Model.baseUnits.Count - 1 Do
    if SameText(FUcum.Model.Properties[FUcum.Model.baseUnits[i].PropertyType].Name, sProperty) Then
    Begin
      oBuilder.StartTableRow;
      oBuilder.AddTableCellURL(FUcum.Model.baseUnits[i].names[0], sPrefix+'prop=base&id='+FUcum.Model.baseUnits[i].code);
      oBuilder.AddTableCell(FUcum.Model.baseUnits[i].code);
      oBuilder.AddTableCell(FUcum.Model.baseUnits[i].printSymbol);
      oBuilder.AddTableCell('');
      oBuilder.AddTableCell(FUcum.Model.Properties[FUcum.Model.baseUnits[i].PropertyType].Name);
      oBuilder.EndTableRow;
    End;

  for i := 0 to FUcum.Model.definedUnits.Count - 1 Do
    if SameText(FUcum.Model.Properties[FUcum.Model.definedUnits[i].PropertyType].Name, sProperty) Then
    Begin
      oBuilder.StartTableRow;
      oBuilder.AddTableCellURL(FUcum.Model.definedUnits[i].names[0], sPrefix+'prop=unit&id='+FUcum.Model.definedUnits[i].code);
      oBuilder.AddTableCell(FUcum.Model.definedUnits[i].code);
      oBuilder.AddTableCell(StringReplace(FUcum.Model.definedUnits[i].printSymbol, [#13, #10, #9], ' '));
      oBuilder.AddTableCell(StringReplace(ShowUnit(FUcum.Model.definedUnits[i].value.text, ' ')+ShowUnit(FUcum.Model.definedUnits[i].value.unit_), [#13, #10, #9], ' '));
      oBuilder.EndTableRow;
    End;
  oBuilder.EndTable;
end;

procedure TUcumPublisher.UcumHeading(oPublisher: THL7v2DOcumentPublisher; sTitle, sPrefix: String);
var
  oPara : TWPDocumentParagraph;
begin
  oPublisher.Title := 'UCUM: '+sTitle;
  oPara := oPublisher.StartParagraph;
  oPara.Format.MarginBottom := 6;
  oPublisher.URL('UCUM', sPrefix).Style := oPublisher.TitleStyle.Name;
  oPublisher.AddText(': '+sTitle, oPublisher.TitleStyle);
  oPublisher.EndParagraph;
end;


procedure TUcumPublisher.PublishUnit(const sPrefix: String; oBuilder: THL7V2DocumentPublisher; oUnit: TUcumUnit);
Begin
  UcumHeading(oBuilder, 'Unit '+oUnit.names[0], sPrefix);

  oBuilder.AddParagraph;

  oBuilder.StartTable();

  oBuilder.StartTableRow;
  oBuilder.AddTableCell('Names');
  oBuilder.AddTableCell(oUnit.names.AsCSV);
  oBuilder.EndTableRow;

  oBuilder.StartTableRow;
  oBuilder.AddTableCell('Code');
  oBuilder.AddTableCell(oUnit.code);
  oBuilder.EndTableRow;

  oBuilder.StartTableRow;
  oBuilder.AddTableCell('Uppercase Code');
  oBuilder.AddTableCell(oUnit.codeUC);
  oBuilder.EndTableRow;

  oBuilder.StartTableRow;
  oBuilder.AddTableCell('Print Symbol');
  oBuilder.AddTableCell(StringReplace(oUnit.printSymbol, [#13, #10, #9], ' '));
  oBuilder.EndTableRow;

  if oUnit is TUcumDefinedUnit then
  begin
    oBuilder.StartTableRow;
    oBuilder.AddTableCell('Metric?');
    oBuilder.AddTableCell(BoolToYesNo(TUcumDefinedUnit(oUnit).metric));
    oBuilder.EndTableRow;

    oBuilder.StartTableRow;
    oBuilder.AddTableCell('Special?');
    oBuilder.AddTableCell(BoolToYesNo(TUcumDefinedUnit(oUnit).isSpecial));
    oBuilder.EndTableRow;
  End;

  oBuilder.StartTableRow;
  oBuilder.AddTableCell('Property');
  oBuilder.AddTableCell(FUcum.Model.Properties[oUnit.PropertyType].Name);
  oBuilder.EndTableRow;

  oBuilder.StartTableRow;
  oBuilder.AddTableCell('Value');
  if oUnit is TUcumBaseUnit then
    oBuilder.AddTableCell('--')
  else
    oBuilder.AddTableCell(StringReplace(ShowUnit(TUcumDefinedUnit(oUnit).value.text, ' ')+ShowUnit(TUcumDefinedUnit(oUnit).value.unit_), [#13, #10, #9], ' '));
  oBuilder.EndTableRow;

(*  for i := 0 to FUcum.Model.baseUnits.Count - 1 Do
    if SameText(FUcum.Model.baseUnits[i].PropertyType, sProperty) Then
    Begin
      oBuilder.StartTableRow;
      oBuilder.AddTableCellURL(FUcum.Model.baseUnits[i].names[0], sPrefix+'prop=base&id='+FUcum.Model.baseUnits[i].code);
      oBuilder.AddTableCell(FUcum.Model.baseUnits[i].code);
      oBuilder.AddTableCell(FUcum.Model.baseUnits[i].printSymbol);
      oBuilder.AddTableCell('');
      oBuilder.AddTableCell(FUcum.Model.baseUnits[i].PropertyType);
      oBuilder.EndTableRow;
    End;

  for i := 0 to FUcum.Model.definedUnits.Count - 1 Do
    if SameText(FUcum.Model.definedUnits[i].PropertyType, sProperty) Then
    Begin
      oBuilder.StartTableRow;
      oBuilder.AddTableCellURL(FUcum.Model.definedUnits[i].names[0], sPrefix+'prop=unit&id='+FUcum.Model.definedUnits[i].code);
      oBuilder.AddTableCell(FUcum.Model.definedUnits[i].code);
      oBuilder.AddTableCell(StringReplace(FUcum.Model.definedUnits[i].printSymbol, [#13, #10, #9], ' '));
      oBuilder.AddTableCell(StringReplace(FUcum.Model.definedUnits[i].value.text+' '+FUcum.Model.definedUnits[i].value.unit_, [#13, #10, #9], ' '));
      oBuilder.EndTableRow;
    End;*)
    
  oBuilder.EndTable;

end;

constructor TUcumPublisher.Create(oUcum: TUcumServices);
begin
  Inherited Create;
  FUcum := oUcum.Link;
end;

destructor TUcumPublisher.Destroy;
begin
  FUcum.Free;
  inherited;
end;

procedure TUcumPublisher.PublishExpression(const sPrefix: String; oBuilder: THL7V2DocumentPublisher; sExpression: String);
var
  s : String;
begin
  oBuilder.AddParagraph('Analysis of '+sExpression);

  s := FUcum.validate(sExpression);
  if s <> '' Then
    oBuilder.AddParagraph('Error: '+s)
  else
  Begin
    s := FUcum.analyse(sExpression);
    oBuilder.AddParagraph('Analysis: '+s);
  End;
    oBuilder.AddParagraph;
    oBuilder.ParaURL('Enter another unit', sPrefix+'prop=expression&unit=??&caption=Enter Units&prompt=Units');
end;

function TUcumPublisher.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FUcum.sizeInBytes);
end;

End.

