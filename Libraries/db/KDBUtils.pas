unit KDBUtils;

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
  Classes, DB, Contnrs,
   KDBManager, AdvExceptions;

const
  {$IFDEF VER170}
  ColTypeMap: array[TFieldType] of TKDBColumnType =
    ( {ftUnknown}     ctUnknown,    {ftString}      ctChar,
    {ftSmallint}    ctInteger,    {ftInteger}     ctInteger,
    {ftWord}        ctInteger,    {ftBoolean}     ctBoolean,
    {ftFloat}       ctFloat,      {ftCurrency}    ctFloat,
    {ftBCD}         ctFloat,      {ftDate}        ctDateTime,
    {ftTime}        ctDateTime,   {ftDateTime}    ctDateTime,
    {ftBytes}       ctBlob,       {ftVarBytes}    ctBlob,
    {ftAutoInc}     ctInteger,    {ftBlob}        ctBlob,
    {ftMemo}        ctBlob,       {ftGraphic}     ctBlob,
    {ftFmtMemo}     ctBlob,       {ftParadoxOle}  ctBlob,
    {ftDBaseOle}    ctBlob,       {ftTypedBinary} ctUnknown,
    {ftCursor}      ctUnknown,    {ftFixedChar}   ctChar,
    {ftWideString}  ctChar,       {ftLargeint}    ctInteger,
    {ftADT}         ctUnknown,    {ftArray}       ctUnknown,
    {ftReference}   ctUnknown,    {ftDataSet}     ctBlob,
    {ftOraBlob}     ctBlob,       {ftOraClob}     ctBlob,
    {ftVariant}     ctUnknown,    {ftInterface}   ctUnknown,
    {ftIDispatch}   ctUnknown,    {ftGuid}        ctUnknown,
    {ftTimeStamp}   ctUnknown,    {ftFMTBcd}      ctInteger

    );
  {$ENDIF}

  {$IFDEF VER150}
  ColTypeMap: array[TFieldType] of TKDBColumnType =
    ( {ftUnknown}     ctUnknown,    {ftString}      ctChar,
    {ftSmallint}    ctInteger,    {ftInteger}     ctInteger,
    {ftWord}        ctInteger,    {ftBoolean}     ctBoolean,
    {ftFloat}       ctFloat,      {ftCurrency}    ctFloat,
    {ftBCD}         ctFloat,      {ftDate}        ctDateTime,
    {ftTime}        ctDateTime,   {ftDateTime}    ctDateTime,
    {ftBytes}       ctBlob,       {ftVarBytes}    ctBlob,
    {ftAutoInc}     ctInteger,    {ftBlob}        ctBlob,
    {ftMemo}        ctBlob,       {ftGraphic}     ctBlob,
    {ftFmtMemo}     ctBlob,       {ftParadoxOle}  ctBlob,
    {ftDBaseOle}    ctBlob,       {ftTypedBinary} ctUnknown,
    {ftCursor}      ctUnknown,    {ftFixedChar}   ctChar,
    {ftWideString}  ctChar,       {ftLargeint}    ctInteger,
    {ftADT}         ctUnknown,    {ftArray}       ctUnknown,
    {ftReference}   ctUnknown,    {ftDataSet}     ctBlob,
    {ftOraBlob}     ctBlob,       {ftOraClob}     ctBlob,
    {ftVariant}     ctUnknown,    {ftInterface}   ctUnknown,
    {ftIDispatch}   ctUnknown,    {ftGuid}        ctUnknown,
    {ftTimeStamp}   ctUnknown,    {ftFMTBcd}      ctInteger

    );
  {$ENDIF}

  {$IFDEF VER140}
  ColTypeMap: array[TFieldType] of TKDBColumnType =
    ( {ftUnknown}     ctUnknown,    {ftString}      ctChar,
    {ftSmallint}    ctInteger,    {ftInteger}     ctInteger,
    {ftWord}        ctInteger,    {ftBoolean}     ctBoolean,
    {ftFloat}       ctFloat,      {ftCurrency}    ctFloat,
    {ftBCD}         ctFloat,      {ftDate}        ctDateTime,
    {ftTime}        ctDateTime,   {ftDateTime}    ctDateTime,
    {ftBytes}       ctBlob,       {ftVarBytes}    ctBlob,
    {ftAutoInc}     ctInteger,    {ftBlob}        ctBlob,
    {ftMemo}        ctBlob,       {ftGraphic}     ctBlob,
    {ftFmtMemo}     ctBlob,       {ftParadoxOle}  ctBlob,
    {ftDBaseOle}    ctBlob,       {ftTypedBinary} ctUnknown,
    {ftCursor}      ctUnknown,    {ftFixedChar}   ctChar,
    {ftWideString}  ctChar,       {ftLargeint}    ctInteger,
    {ftADT}         ctUnknown,    {ftArray}       ctUnknown,
    {ftReference}   ctUnknown,    {ftDataSet}     ctBlob,
    {ftOraBlob}     ctBlob,       {ftOraClob}     ctBlob,
    {ftVariant}     ctUnknown,    {ftInterface}   ctUnknown,
    {ftIDispatch}   ctUnknown,    {ftGuid}        ctUnknown,
    {ftTimeStamp}   ctUnknown,    {ftFMTBcd}      ctInteger

    );
  {$ENDIF}

  {$IFDEF VER130}
  ColTypeMap: array[TFieldType] of TKDBColumnType =
    ( {ftUnknown}     ctUnknown,    {ftString}      ctChar,
    {ftSmallint}    ctInteger,    {ftInteger}     ctInteger,
    {ftWord}        ctInteger,    {ftBoolean}     ctBoolean,
    {ftFloat}       ctFloat,      {ftCurrency}    ctFloat,
    {ftBCD}         ctFloat,      {ftDate}        ctDateTime,
    {ftTime}        ctDateTime,   {ftDateTime}    ctDateTime,
    {ftBytes}       ctBlob,       {ftVarBytes}    ctBlob,
    {ftAutoInc}     ctInteger,    {ftBlob}        ctBlob,
    {ftMemo}        ctBlob,       {ftGraphic}     ctBlob,
    {ftFmtMemo}     ctBlob,       {ftParadoxOle}  ctBlob,
    {ftDBaseOle}    ctBlob,       {ftTypedBinary} ctUnknown,
    {ftCursor}      ctUnknown,    {ftFixedChar}   ctChar,
    {ftWideString}  ctChar,       {ftLargeint}    ctInt64,
    {ftADT}         ctUnknown,    {ftArray}       ctUnknown,
    {ftReference}   ctUnknown,    {ftDataSet}     ctBlob,
    {ftOraBlob}     ctBlob,       {ftOraClob}     ctBlob,
    {ftVariant}     ctUnknown,    {ftInterface}   ctUnknown,
    {ftIDispatch}   ctUnknown,    {ftGuid}        ctUnknown
    );
  {$ENDIF}

  {$IFDEF VER240}
  ColTypeMap: array[TFieldType] of TKDBColumnType =
    ( {ftUnknown}     ctUnknown,    {ftString}      ctChar,
    {ftSmallint}    ctInteger,    {ftInteger}      ctInteger,
    {ftWord}        ctInteger,    {ftBoolean}      ctBoolean,
    {ftFloat}       ctFloat,      {ftCurrency}     ctFloat,
    {ftBCD}         ctFloat,      {ftDate}         ctDateTime,
    {ftTime}        ctDateTime,   {ftDateTime}     ctDateTime,
    {ftBytes}       ctBlob,       {ftVarBytes}     ctBlob,
    {ftAutoInc}     ctInteger,    {ftBlob}         ctBlob,
    {ftMemo}        ctBlob,       {ftGraphic}      ctBlob,
    {ftFmtMemo}     ctBlob,       {ftParadoxOle}   ctBlob,
    {ftDBaseOle}    ctBlob,       {ftTypedBinary}  ctUnknown,
    {ftCursor}      ctUnknown,    {ftFixedChar}    ctChar,
    {ftWideString}  ctChar,       {ftLargeint}     ctInt64,
    {ftADT}         ctUnknown,    {ftArray}        ctUnknown,
    {ftReference}   ctUnknown,    {ftDataSet}      ctBlob,
    {ftOraBlob}     ctBlob,       {ftOraClob}      ctBlob,
    {ftVariant}     ctUnknown,    {ftInterface}    ctUnknown,
    {ftIDispatch}   ctUnknown,    {ftGuid}         ctUnknown,
    {ftTimeStamp}    ctUnknown,   {ftFMTBcd}      ctUnknown,
    {ftFixedWideChar}ctChar,      {ftWideMemo}    ctUnknown,
    {ftOraTimeStamp} ctUnknown,   {ftOraInterval} ctUnknown,
    {ftLongWord}     ctInteger,   {ftShortint}    ctInteger,
    {ftByte}         ctInteger,   {ftExtended}    ctFloat,
    {ftConnection}   ctUnknown,   {ftParams}      ctUnknown,
    {ftStream}       ctUnknown,   {ftTimeStampOffset} ctUnknown,
    {ftObject}       ctUnknown,   {ftSingle{}     ctFloat
    );
  {$ENDIF}

  {$IFDEF VER260}
  ColTypeMap: array[TFieldType] of TKDBColumnType =
    ( {ftUnknown}     ctUnknown,    {ftString}      ctChar,
    {ftSmallint}    ctInteger,    {ftInteger}      ctInteger,
    {ftWord}        ctInteger,    {ftBoolean}      ctBoolean,
    {ftFloat}       ctFloat,      {ftCurrency}     ctFloat,
    {ftBCD}         ctFloat,      {ftDate}         ctDateTime,
    {ftTime}        ctDateTime,   {ftDateTime}     ctDateTime,
    {ftBytes}       ctBlob,       {ftVarBytes}     ctBlob,
    {ftAutoInc}     ctInteger,    {ftBlob}         ctBlob,
    {ftMemo}        ctBlob,       {ftGraphic}      ctBlob,
    {ftFmtMemo}     ctBlob,       {ftParadoxOle}   ctBlob,
    {ftDBaseOle}    ctBlob,       {ftTypedBinary}  ctUnknown,
    {ftCursor}      ctUnknown,    {ftFixedChar}    ctChar,
    {ftWideString}  ctChar,       {ftLargeint}     ctInt64,
    {ftADT}         ctUnknown,    {ftArray}        ctUnknown,
    {ftReference}   ctUnknown,    {ftDataSet}      ctBlob,
    {ftOraBlob}     ctBlob,       {ftOraClob}      ctBlob,
    {ftVariant}     ctUnknown,    {ftInterface}    ctUnknown,
    {ftIDispatch}   ctUnknown,    {ftGuid}         ctUnknown,
    {ftTimeStamp}    ctUnknown,   {ftFMTBcd}      ctUnknown,
    {ftFixedWideChar}ctChar,      {ftWideMemo}    ctUnknown,
    {ftOraTimeStamp} ctUnknown,   {ftOraInterval} ctUnknown,
    {ftLongWord}     ctInteger,   {ftShortint}    ctInteger,
    {ftByte}         ctInteger,   {ftExtended}    ctFloat,
    {ftConnection}   ctUnknown,   {ftParams}      ctUnknown,
    {ftStream}       ctUnknown,   {ftTimeStampOffset} ctUnknown,
    {ftObject}       ctUnknown,   {ftSingle{}     ctFloat
    );
  {$ENDIF}

  {$IFDEF VER280}
  ColTypeMap: array[TFieldType] of TKDBColumnType =
    ( {ftUnknown}     ctUnknown,    {ftString}      ctChar,
    {ftSmallint}    ctInteger,    {ftInteger}      ctInteger,
    {ftWord}        ctInteger,    {ftBoolean}      ctBoolean,
    {ftFloat}       ctFloat,      {ftCurrency}     ctFloat,
    {ftBCD}         ctFloat,      {ftDate}         ctDateTime,
    {ftTime}        ctDateTime,   {ftDateTime}     ctDateTime,
    {ftBytes}       ctBlob,       {ftVarBytes}     ctBlob,
    {ftAutoInc}     ctInteger,    {ftBlob}         ctBlob,
    {ftMemo}        ctBlob,       {ftGraphic}      ctBlob,
    {ftFmtMemo}     ctBlob,       {ftParadoxOle}   ctBlob,
    {ftDBaseOle}    ctBlob,       {ftTypedBinary}  ctUnknown,
    {ftCursor}      ctUnknown,    {ftFixedChar}    ctChar,
    {ftWideString}  ctChar,       {ftLargeint}     ctInt64,
    {ftADT}         ctUnknown,    {ftArray}        ctUnknown,
    {ftReference}   ctUnknown,    {ftDataSet}      ctBlob,
    {ftOraBlob}     ctBlob,       {ftOraClob}      ctBlob,
    {ftVariant}     ctUnknown,    {ftInterface}    ctUnknown,
    {ftIDispatch}   ctUnknown,    {ftGuid}         ctUnknown,
    {ftTimeStamp}    ctUnknown,   {ftFMTBcd}      ctUnknown,
    {ftFixedWideChar}ctChar,      {ftWideMemo}    ctUnknown,
    {ftOraTimeStamp} ctUnknown,   {ftOraInterval} ctUnknown,
    {ftLongWord}     ctInteger,   {ftShortint}    ctInteger,
    {ftByte}         ctInteger,   {ftExtended}    ctFloat,
    {ftConnection}   ctUnknown,   {ftParams}      ctUnknown,
    {ftStream}       ctUnknown,   {ftTimeStampOffset} ctUnknown,
    {ftObject}       ctUnknown,   {ftSingle{}     ctFloat
    );
  {$ENDIF}

 {$IFDEF VER290}
  ColTypeMap: array[TFieldType] of TKDBColumnType =
    ( {ftUnknown}     ctUnknown,    {ftString}      ctChar,
    {ftSmallint}    ctInteger,    {ftInteger}      ctInteger,
    {ftWord}        ctInteger,    {ftBoolean}      ctBoolean,
    {ftFloat}       ctFloat,      {ftCurrency}     ctFloat,
    {ftBCD}         ctFloat,      {ftDate}         ctDateTime,
    {ftTime}        ctDateTime,   {ftDateTime}     ctDateTime,
    {ftBytes}       ctBlob,       {ftVarBytes}     ctBlob,
    {ftAutoInc}     ctInteger,    {ftBlob}         ctBlob,
    {ftMemo}        ctBlob,       {ftGraphic}      ctBlob,
    {ftFmtMemo}     ctBlob,       {ftParadoxOle}   ctBlob,
    {ftDBaseOle}    ctBlob,       {ftTypedBinary}  ctUnknown,
    {ftCursor}      ctUnknown,    {ftFixedChar}    ctChar,
    {ftWideString}  ctChar,       {ftLargeint}     ctInt64,
    {ftADT}         ctUnknown,    {ftArray}        ctUnknown,
    {ftReference}   ctUnknown,    {ftDataSet}      ctBlob,
    {ftOraBlob}     ctBlob,       {ftOraClob}      ctBlob,
    {ftVariant}     ctUnknown,    {ftInterface}    ctUnknown,
    {ftIDispatch}   ctUnknown,    {ftGuid}         ctUnknown,
    {ftTimeStamp}    ctUnknown,   {ftFMTBcd}      ctUnknown,
    {ftFixedWideChar}ctChar,      {ftWideMemo}    ctUnknown,
    {ftOraTimeStamp} ctUnknown,   {ftOraInterval} ctUnknown,
    {ftLongWord}     ctInteger,   {ftShortint}    ctInteger,
    {ftByte}         ctInteger,   {ftExtended}    ctFloat,
    {ftConnection}   ctUnknown,   {ftParams}      ctUnknown,
    {ftStream}       ctUnknown,   {ftTimeStampOffset} ctUnknown,
    {ftObject}       ctUnknown,   {ftSingle{}     ctFloat
    );
  {$ENDIF}

 {$IFDEF VER300}
  ColTypeMap: array[TFieldType] of TKDBColumnType =
    ( {ftUnknown}     ctUnknown,    {ftString}      ctChar,
    {ftSmallint}    ctInteger,    {ftInteger}      ctInteger,
    {ftWord}        ctInteger,    {ftBoolean}      ctBoolean,
    {ftFloat}       ctFloat,      {ftCurrency}     ctFloat,
    {ftBCD}         ctFloat,      {ftDate}         ctDateTime,
    {ftTime}        ctDateTime,   {ftDateTime}     ctDateTime,
    {ftBytes}       ctBlob,       {ftVarBytes}     ctBlob,
    {ftAutoInc}     ctInteger,    {ftBlob}         ctBlob,
    {ftMemo}        ctBlob,       {ftGraphic}      ctBlob,
    {ftFmtMemo}     ctBlob,       {ftParadoxOle}   ctBlob,
    {ftDBaseOle}    ctBlob,       {ftTypedBinary}  ctUnknown,
    {ftCursor}      ctUnknown,    {ftFixedChar}    ctChar,
    {ftWideString}  ctChar,       {ftLargeint}     ctInt64,
    {ftADT}         ctUnknown,    {ftArray}        ctUnknown,
    {ftReference}   ctUnknown,    {ftDataSet}      ctBlob,
    {ftOraBlob}     ctBlob,       {ftOraClob}      ctBlob,
    {ftVariant}     ctUnknown,    {ftInterface}    ctUnknown,
    {ftIDispatch}   ctUnknown,    {ftGuid}         ctUnknown,
    {ftTimeStamp}    ctUnknown,   {ftFMTBcd}      ctUnknown,
    {ftFixedWideChar}ctChar,      {ftWideMemo}    ctUnknown,
    {ftOraTimeStamp} ctUnknown,   {ftOraInterval} ctUnknown,
    {ftLongWord}     ctInteger,   {ftShortint}    ctInteger,
    {ftByte}         ctInteger,   {ftExtended}    ctFloat,
    {ftConnection}   ctUnknown,   {ftParams}      ctUnknown,
    {ftStream}       ctUnknown,   {ftTimeStampOffset} ctUnknown,
    {ftObject}       ctUnknown,   {ftSingle{}     ctFloat
    );
  {$ENDIF}

 {$IFDEF VER310}
  ColTypeMap: array[TFieldType] of TKDBColumnType =
    ( {ftUnknown}     ctUnknown,    {ftString}      ctChar,
    {ftSmallint}    ctInteger,    {ftInteger}      ctInteger,
    {ftWord}        ctInteger,    {ftBoolean}      ctBoolean,
    {ftFloat}       ctFloat,      {ftCurrency}     ctFloat,
    {ftBCD}         ctFloat,      {ftDate}         ctDateTime,
    {ftTime}        ctDateTime,   {ftDateTime}     ctDateTime,
    {ftBytes}       ctBlob,       {ftVarBytes}     ctBlob,
    {ftAutoInc}     ctInteger,    {ftBlob}         ctBlob,
    {ftMemo}        ctBlob,       {ftGraphic}      ctBlob,
    {ftFmtMemo}     ctBlob,       {ftParadoxOle}   ctBlob,
    {ftDBaseOle}    ctBlob,       {ftTypedBinary}  ctUnknown,
    {ftCursor}      ctUnknown,    {ftFixedChar}    ctChar,
    {ftWideString}  ctChar,       {ftLargeint}     ctInt64,
    {ftADT}         ctUnknown,    {ftArray}        ctUnknown,
    {ftReference}   ctUnknown,    {ftDataSet}      ctBlob,
    {ftOraBlob}     ctBlob,       {ftOraClob}      ctBlob,
    {ftVariant}     ctUnknown,    {ftInterface}    ctUnknown,
    {ftIDispatch}   ctUnknown,    {ftGuid}         ctUnknown,
    {ftTimeStamp}    ctUnknown,   {ftFMTBcd}      ctUnknown,
    {ftFixedWideChar}ctChar,      {ftWideMemo}    ctUnknown,
    {ftOraTimeStamp} ctUnknown,   {ftOraInterval} ctUnknown,
    {ftLongWord}     ctInteger,   {ftShortint}    ctInteger,
    {ftByte}         ctInteger,   {ftExtended}    ctFloat,
    {ftConnection}   ctUnknown,   {ftParams}      ctUnknown,
    {ftStream}       ctUnknown,   {ftTimeStampOffset} ctUnknown,
    {ftObject}       ctUnknown,   {ftSingle{}     ctFloat
    );
  {$ENDIF}

 {$IFDEF VER320}
  ColTypeMap: array[TFieldType] of TKDBColumnType =
    ( {ftUnknown}     ctUnknown,    {ftString}      ctChar,
    {ftSmallint}    ctInteger,    {ftInteger}      ctInteger,
    {ftWord}        ctInteger,    {ftBoolean}      ctBoolean,
    {ftFloat}       ctFloat,      {ftCurrency}     ctFloat,
    {ftBCD}         ctFloat,      {ftDate}         ctDateTime,
    {ftTime}        ctDateTime,   {ftDateTime}     ctDateTime,
    {ftBytes}       ctBlob,       {ftVarBytes}     ctBlob,
    {ftAutoInc}     ctInteger,    {ftBlob}         ctBlob,
    {ftMemo}        ctBlob,       {ftGraphic}      ctBlob,
    {ftFmtMemo}     ctBlob,       {ftParadoxOle}   ctBlob,
    {ftDBaseOle}    ctBlob,       {ftTypedBinary}  ctUnknown,
    {ftCursor}      ctUnknown,    {ftFixedChar}    ctChar,
    {ftWideString}  ctChar,       {ftLargeint}     ctInt64,
    {ftADT}         ctUnknown,    {ftArray}        ctUnknown,
    {ftReference}   ctUnknown,    {ftDataSet}      ctBlob,
    {ftOraBlob}     ctBlob,       {ftOraClob}      ctBlob,
    {ftVariant}     ctUnknown,    {ftInterface}    ctUnknown,
    {ftIDispatch}   ctUnknown,    {ftGuid}         ctUnknown,
    {ftTimeStamp}    ctUnknown,   {ftFMTBcd}      ctUnknown,
    {ftFixedWideChar}ctChar,      {ftWideMemo}    ctUnknown,
    {ftOraTimeStamp} ctUnknown,   {ftOraInterval} ctUnknown,
    {ftLongWord}     ctInteger,   {ftShortint}    ctInteger,
    {ftByte}         ctInteger,   {ftExtended}    ctFloat,
    {ftConnection}   ctUnknown,   {ftParams}      ctUnknown,
    {ftStream}       ctUnknown,   {ftTimeStampOffset} ctUnknown,
    {ftObject}       ctUnknown,   {ftSingle{}     ctFloat
    );
  {$ENDIF}


procedure PopulateDBTableMetaData(ADB : TDataSet; ATable : TKDBTable);
function FetchIndexMetaData(AIndexDef : TIndexDef) : TKDBIndex;

implementation

uses
  SysUtils,
  StringSupport;


function FetchColumnMetaData(AField : TField):TKDBColumn;
begin
  result := TKDBColumn.create;
  try
    result.Name := AField.FieldName;
    result.DataType := ColTypeMap[AField.DataType];
    result.Length := AField.DataSize;
    result.Nullable := not AField.Required;
  except
    on e:exception do
    begin
      result.free;
      recordStack(e);
      raise;
    end;
  end;
end;

procedure PopulateDBTableMetaData(ADB : TDataset; ATable : TKDBTable);
var
   i : integer;
begin
  for i := 0 to ADB.Fields.count - 1 do
    begin
    ATable.Columns.Add(FetchColumnMetaData(ADB.Fields[i]));
    end;
end;

function FetchIndexMetaData(AIndexDef : TIndexDef) : TKDBIndex;
var
  s, t : String;
begin
  result := TKDBindex.create;
  try
    result.Name := AIndexDef.Name;
    result.Unique := ixUnique in AIndexDef.Options;
    s := AIndexDef.Fields;
    while s <> '' do
      begin
      StringSplit(s, ';', t, s);
      result.Columns.Add(TKDBColumn.create(t));
      end;
  except
    on e:exception do
    begin
      result.Free;
      recordStack(e);
      raise;
    end;
  end;
end;

end.

