
Unit PISQL;

{! 1 !}
{0.00-002  08 Oct 04 14:14  [18372]  User: Callan Hodgskin   Updates}

Interface

Uses
  AdvObjectLists,
  AdvObjects,
  AdvStringLists,
  KDBDialects;

Type
  TPIFieldType = (pitkey, pitdatetime, pitint, pitblob, pitnchar, pitfloat, pitbool, pitchar, pitint64, pitautoinc);

  TPISQLField = Class (TAdvObject)
  Private
    FNull: Boolean;
    FName: String;
    FFieldType: TPIFieldType;
    FSize: Integer;
    Function TypeHasSize : Boolean;
    Procedure SetName(Const sValue: String);
    Function WriteOutput(aPlatform : TKDBPlatform; sTableName : String; Var vResults : TSQLSet):String;
  Public
    Property Name : String Read FName Write SetName;
    Property FieldType : TPIFieldType Read FFieldType Write FFieldType;
    Property Null : Boolean Read FNull Write FNull;
    Property Size : Integer Read FSize Write FSize;
  End;

  TPISQLFieldList = Class (TAdvObjectList)
  Private
    Function GetField(iIndex: Integer): TPISQLField;
    Function FieldExists(sName : String):Boolean;
  Protected
    Function ItemClass : TAdvObjectClass; Override;
  Public
    Property field[iIndex : Integer] : TPISQLField Read GetField;
  End;

  TPISQLCreateTableModel = Class (TAdvObject)
  Private
    FFields: TPISQLFieldList;
    FName: String;
    FPKName : String;
    FPKField : String;

    FSQL : String;
    FNextToken : String;
    FCursor : Integer;
    Procedure BeginParse(sSQL : String);
    Function GetNextToken : String;
    Procedure ConsumeToken(sToken : String);

    Procedure ParseField;
    Function ReadType : TPIFieldType;
    Function ReadNull : Boolean;
    Procedure ParsePrimaryKey;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure Parse(sSQL : String);
    Procedure WriteOutput(aPlatform : TKDBPlatform; Out vResults : TSQLSet);

    Property Name : String Read FName;
    Property PKName : String Read FPKName;
    Property PKField : String Read FPKField;
    Property Fields : TPISQLFieldList Read FFields;
  End;

Implementation

Uses
  SysUtils,

  AdvExceptions,
  StringSupport;

Const
  ASSERT_UNITNAME = 'PISql';

{ TPISQLFieldList }

Function TPISQLFieldList.FieldExists(sName: String): Boolean;
Var
  i : Integer;
Begin
  Result := False;
  For i := 0 To Count -1 Do
    If SameText(Field[i].Name, sName) Then
      Begin
      Result := True;
      Exit;
      End;
End;

Function TPISQLFieldList.GetField(iIndex: Integer): TPISQLField;
Begin
  Result := TPISQLField(ItemByIndex[iIndex]);
End;

Function TPISQLFieldList.ItemClass: TAdvObjectClass;
Begin
  Result := TPISQLField;
End;

{ TPISQLCreateTableModel }

Procedure TPISQLCreateTableModel.BeginParse(sSQL: String);
Begin
  FSQL := sSQL;
  FCursor := 1;
  GetNextToken;
End;

Procedure TPISQLCreateTableModel.ConsumeToken(sToken: String);
Const ASSERT_LOCATION = ASSERT_UNITNAME+'.TPISQLCreateTableModel.ConsumeToken';
Var
  s : String;
Begin
  s := GetNextToken;
  Condition(sameText(s, sToken), ASSERT_LOCATION, 'Expected Token "'+sToken+'", found token "'+s+'"');
End;

Constructor TPISQLCreateTableModel.Create;
Begin
  Inherited;
  FFields := TPISQLFieldList.Create;
End;

Destructor TPISQLCreateTableModel.Destroy;
Begin
  FFields.Free;
  Inherited;
End;

Function TPISQLCreateTableModel.GetNextToken: String;
Var
  iStart : Integer;
Begin
  Result := FNextToken;
  FNextToken := '';
  iStart := FCursor;
  If FCursor <= Length(FSQL) Then
    Begin
    Assert(Not isWhiteSpace(FSQL[FCursor]));
    If isAlphaNumericChar(FSQL[FCursor]) Or (FSQL[FCursor] = '_') Then
      Begin
      While (FCursor <= Length(FSQL)) And (isAlphaNumericChar(FSQL[FCursor]) Or (FSQL[FCursor] = '_')) Do
        Inc(FCursor);
      FNextToken := Substring(FSQL, iStart, FCursor);
      End
    Else
      Begin
      FNextToken := FSQL[FCursor];
      Inc(FCursor);
      End;
    While (FCursor <= Length(FSQL)) And isWhitespace(FSQL[FCursor]) Do
      Inc(FCursor);
    End;
End;

Procedure TPISQLCreateTableModel.Parse(sSQL: String);
Const ASSERT_LOCATION = ASSERT_UNITNAME+'.TPISQLCreateTableModel.Parse';
Begin
  BeginParse(StringTrimWhitespace(sSQL));
  ConsumeToken('Create');
  ConsumeToken('Table');
  FName := GetNextToken;
  ConsumeToken('(');
  While FNextToken <> ')' Do
    Begin
    If SameText(FNextToken, 'PrimaryKey') Then
      ParsePrimaryKey
    Else
      ParseField;
    If FNextToken = ',' Then
      GetNextToken;
    End;
  ConsumeToken(')');
  Condition(FNextToken = '', ASSERT_LOCATION, 'Text after end of SQL: '+FNextToken);
End;

Procedure TPISQLCreateTableModel.ParseField;
Var
  oField : TPISQLField;
Begin
  oField := TPISQLField.Create;
  Try
    oField.FName := GetNextToken;
    oField.FFieldType := ReadType;
    If oField.TypeHasSize And (FNextToken = '(') Then
      Begin
      ConsumeToken('(');
      oField.Size := StrToIntWithError(GetNextToken, 'Length for field "'+oField.Name+'"');
      ConsumeToken(')');
      End;
    If oField.FFieldType <> pitautoInc Then
      oField.Null := ReadNull;
    FFields.Add(oField.Link);
  Finally
    oField.Free;
  End;
End;

Procedure TPISQLCreateTableModel.ParsePrimaryKey;
Const ASSERT_LOCATION = ASSERT_UNITNAME+'.TPISQLCreateTableModel.ParsePrimaryKey';
Begin
  ConsumeToken('PrimaryKey');
  ConsumeToken('(');
  FPKName := GetNextToken;
  Condition(IsValidIdent(FPKName), ASSERT_LOCATION, 'PrimaryKey Name "'+FPKName+'" is not acceptable');
  ConsumeToken(':');
  FPKField := GetNextToken;
  Condition(IsValidIdent(FPKField), ASSERT_LOCATION, 'PrimaryKey Field "'+FPKField+'" is not acceptable');
  Condition(FFields.FieldExists(FPKField), ASSERT_LOCATION, 'PrimaryKey Field "'+FPKField+'" is not known');
  ConsumeToken(')');
End;

Function TPISQLCreateTableModel.ReadNull: Boolean;
Const ASSERT_LOCATION = ASSERT_UNITNAME+'.TPISQLCreateTableModel.ReadNull';
Var
  s : String;
Begin
  Result := True;
  s := getNextToken;
  If SameText(s, 'NonNull') Then
    Result := False
  Else
    Condition(SameText(s, 'Null'), ASSERT_LOCATION, 'Unexpected token "'+s+'" reading Nullness');
End;

Function TPISQLCreateTableModel.ReadType: TPIFieldType;
Const ASSERT_LOCATION = ASSERT_UNITNAME+'.TPISQLCreateTableModel.ReadType';
Var
  sType : String;
Begin
  sType := GetNextToken;
  If SameText(sType, 'key') Then
    Result := pitkey
  Else If SameText(sType, 'datetime') Then
    Result := pitdatetime
  Else If SameText(sType, 'int') Then
    Result := pitint
  Else If SameText(sType, 'blob') Then
    Result := pitblob
  Else If SameText(sType, 'nchar') Then
    Result := pitnchar
  Else If SameText(sType, 'float') Then
    Result := pitfloat
  Else If SameText(sType, 'bool') Then
    Result := pitbool
  Else If SameText(sType, 'int64') Then
    Result := pitint64
  Else If SameText(sType, 'char') Then
    Result := pitchar
  Else If SameText(sType, 'autoinc') Then
    Result := pitautoinc
  Else
    begin
    result := pitKey;
    Error(ASSERT_LOCATION, 'Unknown type "'+sType+'"');
    end;
End;

Procedure TPISQLCreateTableModel.WriteOutput(aPlatform: TKDBPlatform; Out vResults: TSQLSet);
Var
  sSQL : String;
  i : Integer;
Begin
  SetLength(vResults, 1);
  sSQL := 'CREATE TABLE '+FName+' (';
  For i := 0 To FFields.Count - 1 Do
    Begin
    If i <> 0 Then
      sSQL := sSQL + ', ';
    sSQL := sSQL + FFields.field[i].WriteOutput(aPlatform, FName, vResults);
    End;
  If FPKName <> '' Then
    sSQL := sSQL + ', '+PrimaryKeyType(aPlatform, FPKName, FPKField);
  sSQL := sSQL + ')'+CreateTableInfo(aPlatform);
  vResults[0] := sSQL;
End;

{ TPISQLField }

Procedure TPISQLField.SetName(Const sValue: String);
Const ASSERT_LOCATION = ASSERT_UNITNAME+'.TPISQLField.SetName';
Begin
  Condition(IsValidIdent(sValue), ASSERT_LOCATION, 'The Name "'+sValue+'" is not acceptable for an SQL field');
  FName := sValue;
End;

Function TPISQLField.TypeHasSize: Boolean;
Begin
  Result := FFieldType In [pitnchar, pitchar]
End;

Function TPISQLField.WriteOutput(aPlatform: TKDBPlatform; sTableName : String; Var vResults: TSQLSet): String;
Const ASSERT_LOCATION = ASSERT_UNITNAME+'.TPISQLField.WriteOutput';
Var
  s1, s2 : String;
  Procedure AddSql(s:String);
  Begin
    If s <> '' Then
      Begin
      SetLength(vResults, Length(vResults)+1);
      vResults[High(vResults)] := s;
      End;
  End;
Begin
  Result := FName+' ';
  Case FFieldType Of
    pitkey      : Result := Result + DBKeyType(aPlatform);
    pitdatetime : Result := Result + DBDateTimeType(aPlatform);
    pitint      : Result := Result + 'int';
    pitblob     : Result := Result + DBBlobType(aPlatform);
    pitnchar    : Result := Result + DBUnicodeType(aPlatform, FSize);
    pitfloat    : Result := Result + DBFloatType(aPlatform);
    pitbool     : Result := Result + DBBooleanType(aPlatform);
    pitint64    : Result := Result + DBInt64Type(aPlatform);
    pitchar     :
      Begin
      Result := Result + DBCharType(aPlatform);
      If FSize > 1 Then
        Result := Result + '('+inttostr(FSize)+')';
      End;
    pitautoinc  :
      Begin
      Result := Result + PlatformAutoIncrementSQL(aPlatform);
      GetPlatformOtherAutoIncSQL(aPlatform, sTableName, FName, s1, s2);
      AddSQL(s1);
      AddSQL(s2);
      End;
  Else
    Condition(False, ASSERT_LOCATION, 'Unknown field type');
  End;
  If FFieldType <> pitautoinc Then
    Result := Result + ' '+ColCanBeNull(aPlatform, FNull);
End;

End.