{! 1 !}
{0.00-008  12 Oct 04 09:31  [21370]  User: Grahame Grieve    D9 compile fix}

unit KDBUtils;

interface

uses
  Classes, DB, Contnrs,
  {$IFDEF WIN32} IdSoapDateTime, {$ENDIF}
  KDate, KDBManager;

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

{$IFDEF WIN32}
function IdSoapDateTimeToTimeStamp(AValue : TIdSoapDateTime):TTimeStamp;
function TimeStampToIdSoapDateTime(AValue : TTimeStamp):TIdSoapDateTime;

procedure BindIdSoapDateTime(AConn : TKDBConnection; AName: String; AValue: TIdSoapDateTime);
Function ColIdSoapDateTime(AConn : TKDBConnection; AName: String): TIdSoapDateTime;
{$ENDIF}

procedure PopulateDBTableMetaData(ADB : TDataSet; ATable : TKDBTable);
function FetchIndexMetaData(AIndexDef : TIndexDef) : TKDBIndex;

implementation

uses
  SysUtils,
  StringSupport;

{$IFDEF WIN32}
function IdSoapDateTimeToTimeStamp(AValue : TIdSoapDateTime):kdate.TTimeStamp;
begin
  if not Assigned(AValue) then
    begin
    FillChar(result, sizeof(kdate.TTimeStamp), 0);
    end
  else
    begin
    result.year := AValue.Year;
    result.month := AValue.Month;
    result.day := AValue.Day;
    result.hour := AValue.Hour;
    result.minute := AValue.Minute;
    result.second := AValue.Second;
    result.fraction := 0;
    end;
end;

function TimeStampToIdSoapDateTime(AValue : kdate.TTimeStamp):TIdSoapDateTime;
var
  LYr : Cardinal;
begin
  LYr := abs(AValue.year);
  if (LYr + AValue.month + AValue.day + AValue.hour + AValue.minute + AValue.minute + AValue.second + AValue.fraction = 0) then
    begin
    result := nil;
    end
  else
    begin
    Result := TIdSoapDateTime.Create;
    Result.year := AValue.Year;
    Result.month := AValue.Month;
    Result.day := AValue.Day;
    Result.hour := AValue.Hour;
    Result.minute := AValue.Minute;
    Result.second := AValue.Second;
    end;
end;

procedure BindIdSoapDateTime(AConn : TKDBConnection; AName: String; AValue: TIdSoapDateTime);
begin
  if not assigned(AValue) then
    begin
    AConn.BindNull(AName);
    end
  else
    begin
    AConn.BindTimeStamp(AName, IdSoapDateTimeToTimeStamp(AValue));
    // timezone?
    end;
end;

function ColIdSoapDateTime(AConn : TKDBConnection; AName: String): TIdSoapDateTime;
begin
  if AConn.GetColNullByName(AName) then
    begin
    Result := NIL;
    end
  else
    begin
    result := TimeStampToIdSoapDateTime(AConn.ColTimeStampByName[AName]);
    end;
end;
{$ENDIF}


function FetchColumnMetaData(AField : TField):TKDBColumn;
begin
  result := TKDBColumn.create;
  try
    result.Name := AField.FieldName;
    result.DataType := ColTypeMap[AField.DataType];
    result.Length := AField.DataSize;
    result.Nullable := not AField.Required;
  except
    result.free;
    raise;
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
      result.Columns.Add(t);
      end;
  except
    result.Free;
    raise;
  end;
end;

end.

