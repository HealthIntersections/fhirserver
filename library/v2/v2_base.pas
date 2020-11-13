unit v2_base;

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

{$I fhir.inc}

interface

uses
  SysUtils,
  fsl_base, fsl_utilities, fsl_collections;

Type
  THL7V2Version = (hv21, hv22, hv23, hv231, hv24, hv25, hv251, hv26, hv27);
  THL7V2Versions = Set Of THL7V2Version;

Const
  HL7V2_SENDER_VERSIONS = [hv231..hv27];

  NAMES_HL7V2_VERSION : Array [THL7V2Version] Of String = ('2.1', '2.2', '2.3', '2.3.1', '2.4', '2.5', '2.5.1', '2.6', '2.7');
  TOKENS_HL7V2_VERSION : Array [THL7V2Version] Of String = ('21', '22', '23', '231', '24', '25', '251', '26', '27');
  LITERALS_HL7V2_VERSION : Array [THL7V2Version] Of String = ('hv21', 'hv22', 'hv23', 'hv231', 'hv24', 'hv25', 'hv251', 'hv26', 'hv27');

Type
   {@Enum THL7ErrorCondition
      these are the possible error conditions described for the Error Condition
      Field MSA-6. In the standard, the error conditions are divided into AE and
      AR types. You can choose to follow this standard but it is not imposed on
      you. Note: There seems to be some inconsistencies in the standard regarding
      the categorisations for ACK types given to the error conditions
     }

  THL7V2ErrorCondition = (hecAccepted,                                                           // AA
    hecSequenceError, hecRequiredField, hecDataTypeError, hecNoTableValue, // AE
    hecUnsMsgType, hecUnsEvntCode, hecUnsProcID, hecUnsVersion,
    hecSuperfluousSeg, hecRequiredSeg,
    hecUnknownKey, hecDuplicateKey, hecRecordLocked, hecInternalError,     // AR

    // internals - all mapped to 207 (hecInternalError) in message
    // but provided here in case useful

    hecDictionaryError,   // internal invalidity in dictionary - i.e. specified non-existent data-type etc
    hecBadSegCode,        // message or application attempted to use an undefined segment.
    // although often this is quashed and handled (Z segment support)
    hecNoDictionary,      // couldn't find dictionary??
    hecHL7LibraryError,   // Error in code - not complete in that section
    hecApplicationError,  // Error in Appliction - tried to do something wrong
    hecBadField,          // contents of a field are OK syntactically but application didn't like them
    hecBadMessage,        // couldn't begin to decode message or format was bad
    hecXML,               // an xml error
    hecValidationFailed,  // some validation profile has failed the message, we aren't exactly sure what category
    hecDuplicateMsgId);   // duplicate message id received

Const
  NAMES_HL7V2ERRORCONDITION : Array [THL7V2ErrorCondition] Of String =
     ('Accepted', 'SequenceError', 'RequiredField', 'DataTypeError', 'NoTableValue',
      'UnsMsgType', 'UnsEvntCode', 'UnsProcID', 'UnsVersion', 'SuperfluousSeg',
      'RequiredSeg', 'UnknownKey', 'DuplicateKey', 'RecordLocked', 'InternalError',
      'DictionaryError', 'BadSegCode', 'NoDictionary', 'HL7LibraryError', 'ApplicationError',
      'BadField', 'BadMessage', 'XML', 'ValidationFailed', 'DuplicateMsgId');

  RESPONSES_HL7V2ERRORCONDITION_NEW : Array [THL7V2ErrorCondition] Of String =
     ('AA', 'AE', 'AE', 'AE', 'AE',
      'AE', 'AR', 'AR', 'AR', 'AE',
      'AE', 'AR', 'AR', 'AR', 'AE',
      'AE', 'AE', 'AE', 'AE', 'AE',
      'AE', 'AE', 'AE', 'AE', 'AE');

  RESPONSES_HL7V2ERRORCONDITION_OLD : Array [THL7V2ErrorCondition] Of String =
     ('AA', 'AE', 'AE', 'AE', 'AE',
      'AE', 'AR', 'AR', 'AR', 'AE',
      'AE', 'AR', 'AR', 'AR', 'AR',
      'AR', 'AR', 'AR', 'AR', 'AR',
      'AR', 'AR', 'AR', 'AR', 'AR');

  CODES_HL7V2ERRORCONDITION : Array [THL7V2ErrorCondition] Of String =
     ('' {0}, '100', '101', '102', '103',
      '200', '201', '202', '203', '207',
      '207', '204', '205', '207', '207',
      '207', '207', '207', '207', '207',
      '207', '207', '207', '207', '207');

function CodeToErrorCondition(sCode : String):THL7V2ErrorCondition;

Function FromVersionCode(Const sValue : String) : THL7V2Version;

Type
  THL7V2AcceptAcknowledgmentType = (hv2aaAlways, hv2aaNever, hv2aaError, hv2aaSuccess);
  THL7V2AcceptAcknowledgmentTypes = Set Of THL7V2AcceptAcknowledgmentType;

Const
  NAMES_HL7V2_ACCEPTACKNOWLEDGEMENTTYPE : Array [THL7V2AcceptAcknowledgmentType] Of String = ('AL', 'NE', 'ER', 'SU');
  DISPLAY_HL7V2_ACCEPTACKNOWLEDGEMENTTYPE : Array [THL7V2AcceptAcknowledgmentType] Of String = ('Always', 'Never', 'Error', 'Success');

Function FromAcceptAcknowledgmentTypeCode(Const sValue : String) : THL7V2AcceptAcknowledgmentType;

Type
  EHL7V2Exception = class(EFslException)
  Private
    FCondition: THL7V2ErrorCondition;
    FCode : String;
  Public
    Constructor Create(oSender : TObject; aHL7Condition: THL7V2ErrorCondition; Const sMethod, sReason : String); Overload; Virtual;
    Constructor Create(oSender : TObject; sHL7Condition: String; Const sMethod, sReason : String); Overload; Virtual;
    Constructor Create(oSender : TObject; sCode : String; aHL7Condition: THL7V2ErrorCondition; Const sMethod, sReason : String); Overload; Virtual;
    Constructor Create(oSender : TObject; sCode : String; sHL7Condition: String; Const sMethod, sReason : String); Overload; Virtual;
    property Condition : THL7V2ErrorCondition read FCondition;
    Property Code : String read FCode;
  end;

  EHL7V2RejectException = class (EHL7V2Exception);
  EHL7V2ErrorException = class (EHL7V2Exception);

  // set the debugger to ignore this one
  EHL7V2TrivialException = class (EHL7V2Exception);

type
  THL7V2WorkerObject = class (TFslObject)
  private
    Procedure Error(aCondition : THL7V2ErrorCondition; Const sMethod, sMessage : String);
  protected

    Function Condition(bCorrect : Boolean; Const sMethod, sMessage : String) : Boolean;
    Procedure ErrorSequence(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorRequiredField(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorDataType(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorNoTableValue(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorUnsMsgType(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorUnsEvntCode(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorUnsProcID(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorUnsVersion(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorSuperfluousSeg(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorRequiredSeg(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorUnknownKey(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorDuplicateKey(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorRecordLocked(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorInternal(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorDictionary(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorBadSegCode(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorNoDictionary(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorHL7Library(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorApplication(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorBadField(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorBadMessage(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorXML(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorValidationFailed(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorDuplicateMsgId(const sMethod, sMessage : String); overload; virtual;
  end;

const
  DEFAULT_DELIMITER_FIELD = '|';
  DEFAULT_DELIMITER_COMPONENT = '^';
  DEFAULT_DELIMITER_SUBCOMPONENT = '&';
  DEFAULT_DELIMITER_REPETITION = '~';
  DEFAULT_CHARACTER_ESCAPE = '\';

Function CheckDateFormat(Const sFormat, sContent : String; Var sError : String) : Boolean;
Function DateFormatForType(Const sDataType : String; aVersion : THL7V2Version; Var sFormat : String; bAllowTimezone : Boolean = True) : Boolean;
Function HL7DateToString(aValue : TDateTime; Const sFormat : String; bTimezone : Boolean) : String;
Function HL7StringToDate(Const sFormat, sValue : String; bCorrectForTimezone : Boolean) : TDateTime;
Procedure CheckDOBReal(Const sDate, sDescription: String);
Procedure CheckDODReal(Const sDate, sDescription: String);

implementation

Function FromVersionCode(Const sValue : String) : THL7V2Version;
Var
  iIndex : Integer;
Begin
  iIndex := StringArrayIndexOfSensitive(NAMES_HL7V2_VERSION, sValue);
  If iIndex = -1 Then
    Raise EHL7V2Exception.Create(Nil, hecApplicationError, 'FromVersionCode', 'Bad HL7 Version '+sValue);
  Result :=  THL7V2Version(iIndex);
End;

function CodeToErrorCondition(sCode : String):THL7V2ErrorCondition;
var
  iIndex : integer;
begin
  iIndex := StringArrayIndexOfInsensitive(CODES_HL7V2ERRORCONDITION, sCode);
  if iIndex = -1 then
    result := hecInternalError
  else
    result := THL7V2ErrorCondition(iIndex);
end;

Function FromAcceptAcknowledgmentTypeCode(Const sValue : String) : THL7V2AcceptAcknowledgmentType;
Var
  iIndex : Integer;
Begin
  iIndex := StringArrayIndexOfInSensitive(NAMES_HL7V2_ACCEPTACKNOWLEDGEMENTTYPE, sValue);
  If iIndex = -1 Then
    Result := hv2aaAlways
  Else
    Result :=  THL7V2AcceptAcknowledgmentType(iIndex);
End;

{ EHL7V2Exception }

constructor EHL7V2Exception.Create(oSender: TObject; aHL7Condition: THL7V2ErrorCondition; const sMethod, sReason: String);
begin
  inherited Create(oSender, sMethod, sReason);
  FCondition := aHL7Condition;
end;

constructor EHL7V2Exception.Create(oSender: TObject; sHL7Condition: String; const sMethod, sReason: String);
begin
  if StringArrayExistsInsensitive(NAMES_HL7V2ERRORCONDITION, sHL7Condition) then
    Create(oSender, THL7V2ErrorCondition(StringArrayIndexOfInsensitive(NAMES_HL7V2ERRORCONDITION, sHL7Condition)), sMethod, sReason)
  else if StringIsInteger16(sHL7Condition) Then
    Create(oSender, THL7V2ErrorCondition(StrToInt(sHL7Condition)), sMethod, sReason)
  Else
    Create(oSender, hecInternalError, sMethod, sReason)
end;

constructor EHL7V2Exception.Create(oSender: TObject; sCode: String; aHL7Condition: THL7V2ErrorCondition; const sMethod, sReason: String);
begin
  inherited Create(oSender, sMethod, sReason);
  FCondition := aHL7Condition;
  FCode := sCode;
end;

constructor EHL7V2Exception.Create(oSender: TObject; sCode, sHL7Condition: String; const sMethod, sReason: String);
begin
  if StringArrayExistsInsensitive(NAMES_HL7V2ERRORCONDITION, sHL7Condition) then
    Create(oSender, THL7V2ErrorCondition(StringArrayIndexOfInsensitive(NAMES_HL7V2ERRORCONDITION, sHL7Condition)), sMethod, sReason)
  else if StringIsInteger16(sHL7Condition) Then
    Create(oSender, THL7V2ErrorCondition(StrToInt(sHL7Condition)), sMethod, sReason)
  Else
    Create(oSender, hecInternalError, sMethod, sReason);
  FCode := sCode;
end;

function THL7V2WorkerObject.Condition(bCorrect: Boolean; const sMethod, sMessage: String): Boolean;
begin
  result := True;
  if not bCorrect then
    Error(hecApplicationError, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.Error(aCondition: THL7V2ErrorCondition; const sMethod, sMessage: String);
begin
  Raise EHL7V2Exception.Create(Self, aCondition, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorApplication(const sMethod, sMessage: String);
begin
  Error(hecApplicationError, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorBadField(const sMethod, sMessage: String);
begin
  Error(hecBadField, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorBadMessage(const sMethod, sMessage: String);
begin
  Error(hecBadMessage, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorBadSegCode(const sMethod, sMessage: String);
begin
  Error(hecBadSegCode, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorDataType(const sMethod, sMessage: String);
begin
  Error(hecDataTypeError, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorDictionary(const sMethod, sMessage: String);
begin
  Error(hecDictionaryError, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorDuplicateKey(const sMethod, sMessage: String);
begin
  Error(hecDuplicateKey, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorDuplicateMsgId(const sMethod, sMessage: String);
begin
  Error(hecDuplicateMsgId, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorHL7Library(const sMethod, sMessage: String);
begin
  Error(hecHL7LibraryError, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorInternal(const sMethod, sMessage: String);
begin
  Error(hecInternalError, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorNoDictionary(const sMethod, sMessage: String);
begin
  Error(hecNoDictionary, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorNoTableValue(const sMethod, sMessage: String);
begin
  Error(hecNoTableValue, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorRecordLocked(const sMethod, sMessage: String);
begin
  Error(hecRecordLocked, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorRequiredField(const sMethod, sMessage: String);
begin
  Error(hecRequiredField, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorRequiredSeg(const sMethod, sMessage: String);
begin
  Error(hecRequiredSeg, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorSequence(const sMethod, sMessage: String);
begin
  Error(hecSequenceError, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorSuperfluousSeg(const sMethod, sMessage: String);
begin
  Error(hecSuperfluousSeg, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorUnknownKey(const sMethod, sMessage: String);
begin
  Error(hecUnknownKey, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorUnsEvntCode(const sMethod, sMessage: String);
begin
  Error(hecUnsEvntCode, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorUnsMsgType(const sMethod, sMessage: String);
begin
  Error(hecUnsMsgType, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorUnsProcID(const sMethod, sMessage: String);
begin
  Error(hecUnsProcID, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorUnsVersion(const sMethod, sMessage: String);
begin
  Error(hecUnsVersion, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorValidationFailed(const sMethod, sMessage: String);
begin
  Error(hecValidationFailed, sMethod, sMessage);
end;

procedure THL7V2WorkerObject.ErrorXML(const sMethod, sMessage: String);
begin
  Error(hecXML, sMethod, sMessage);
end;

Function DateFormatForType(Const sDataType : String; aVersion : THL7V2Version; Var sFormat : String; bAllowTimezone : Boolean = True) : Boolean;
Var
  sTimezone : String;
Begin
  If bAllowTimezone Then
    sTimezone := '[+/-ZZZZ]'
  Else
    sTimezone := '';
  Result := True;
  Case StringArrayIndexOf(['DT', 'TM', 'TS', 'DTM'], sDataType) Of
    0: If aVersion < hv23 Then
         sFormat := 'YYYYMMDD'
       Else
         sFormat := 'YYYYYMM[DD]]';
    1: If aVersion < hv23 Then
         sFormat := 'HHNN[SS[.SSSS]]'+sTimezone
       Else
         sFormat := 'HH[NN[SS[.S[S[S[S]]]]]]'+sTimezone;

    2: If aVersion < hv25 Then
         sFormat := 'YYYY[MM[DD[HHNN[SS[.S[S[S[S]]]]]]]]'+sTimezone
       Else
         sFormat := 'YYYY[MM[DD[HH[NN[SS[.S[S[S[S]]]]]]]]]'+sTimezone;
    3: sFormat := 'YYYY[MM[DD[HH[NN[SS[.S[S[S[S]]]]]]]]]'+sTimezone;
  Else
    Begin
    Result := False;
    sFormat := 'YYYY[MM[DD[HH[NN[SS[.S[S[S[S]]]]]]]]]'+sTimezone
    End;
  End;
End;

Function CheckTimezone(bTimezone : Boolean; Const sContent, sOrigFormat : String; Var sError : String) : Boolean;
Begin
  If sContent <> '' Then
    Begin
    If bTimezone Then
      Begin
      Result := CheckDateFormat('HHNN', sContent, sError);
      If Not Result Then
        sError := sError + ' in the timezone portion';
      End
    Else
      Begin
      sError := 'The value "'+sContent+'" does not conform to the expected format for the date "'+sOrigFormat+'" because a timezone was found';
      Result := False;
      End
    End
  Else
    Result := True;
End;

Function Splice(Var sSource : String; sMatch : String) : Boolean; Overload;
Begin
  If Copy(sSource, 1, Length(sMatch)) = sMatch Then
    Begin
    Result := True;
    sSource := Copy(sSource, Length(sMatch)+ 1, $FF);
    End
  Else
    Result := False;
End;

Function Splice(Var sSource : String; iLength : Integer) : String; Overload;
Begin
  Result := Copy(sSource, 1, iLength);
  sSource := Copy(sSource, iLength + 1, $FF);
End;

Function CheckYear(sValue : String; Var sError : String; Var iYear : Integer) : Boolean;
Begin
  Result := False;
  If Length(sValue) <> 4 Then
    sError := 'Year Value '+sValue+' is not 4 digits in length'
  Else If Not StringIsInteger32(sValue) Then
    sError := 'Year Value '+sValue+' is not numerical'
  Else
    Begin
    iYear := StringToInteger32(sValue);
    If (iYear <= 0) Then
      sError := 'Year Value '+sValue+': negative numbers are not supported'
    Else
      Result := True;
    End;
End;

Function CheckMonth(sValue : String; Var sError : String; Var iMonth : Integer) : Boolean;
Begin
  Result := False;
  If Length(sValue) <> 2 Then
    sError := 'Month Value '+sValue+' is not 2 digits in length'
  Else If Not StringIsInteger32(sValue) Then
    sError := 'Month Value '+sValue+' is not numerical'
  Else
    Begin
    iMonth := StringToInteger32(sValue);
    If (iMonth <= 0) Or (iMonth > 12) Then
      sError := 'Month Value '+sValue+': month must be 1 - 12'
    Else
      Result := True;
    End;
End;

Function CheckDay(sValue : String; iYear, iMonth : Integer; Var sError : String) : Boolean;
Var
  iDay : Integer;
Begin
  Result := False;
  If Length(sValue) <> 2 Then
    sError := 'Day Value '+sValue+' is not 2 digits in length'
  Else If Not StringIsInteger32(sValue) Then
    sError := 'Day Value '+sValue+' is not numerical'
  Else
    Begin
    iDay := StringToInteger32(sValue);
    If (iDay <= 0) Then
      sError := 'Day Value '+sValue+': Day must be >= 1'
    Else If iMonth = 0 Then
      sError := 'Day Value '+sValue+': Month must be known'
    Else If iDay > MONTHS_DAYS[IsLeapYearByYear(iYear)][TMonthOfYear(iMonth-1)] Then
      sError := 'Day Value '+sValue+': Value is not valid for '+MONTHOFYEAR_SHORT[TMonthOfYear(iMonth-1)]+'-'+IntegerToString(iYear)
    Else
      Result := True;
    End;
End;

Function CheckHour(sValue : String; Var sError : String) : Boolean;
Var
  iHour : Integer;
Begin
  Result := False;
  If Length(sValue) <> 2 Then
    sError := 'Hour Value '+sValue+' is not 2 digits in length'
  Else If Not StringIsInteger32(sValue) Then
    sError := 'Hour Value '+sValue+' is not numerical'
  Else
    Begin
    iHour := StringToInteger32(sValue);
    If (iHour < 0) Or (iHour > 23) Then
      sError := 'Hour Value '+sValue+': Hour must be 0 and 23'
    Else
      Result := True;
    End;
End;

Function CheckMinute(sValue : String; Var sError : String) : Boolean;
Var
  iMinute : Integer;
Begin
  Result := False;
  If Length(sValue) <> 2 Then
    sError := 'Minute Value '+sValue+' is not 2 digits in length'
  Else If Not StringIsInteger32(sValue) Then
    sError := 'Minute Value '+sValue+' is not numerical'
  Else
    Begin
    iMinute := StringToInteger32(sValue);
    If (iMinute < 0) Or (iMinute > 59) Then
      sError := 'Minute Value '+sValue+': Minute must be 0 and 59'
    Else
      Result := True;
    End;
End;

Function CheckSecond(sValue : String; Var sError : String) : Boolean;
Var
  iSecond : Integer;
Begin
  Result := False;
  If Length(sValue) <> 2 Then
    sError := 'Second Value '+sValue+' is not 2 digits in length'
  Else If Not StringIsInteger32(sValue) Then
    sError := 'Second Value '+sValue+' is not numerical'
  Else
    Begin
    iSecond := StringToInteger32(sValue);
    If (iSecond < 0) Or (iSecond > 59) Then
      sError := 'Second Value '+sValue+': Second must be 0 and 59'
    Else
      Result := True;
    End;
End;

Function CheckDot(sValue : String; Var sError : String; Var bInFraction : Boolean) : Boolean;
Begin
  If sValue = '.' Then
    Begin
    Result := True;
    bInFraction := True;
    End
  Else
    Begin
    Result := False;
    sError := 'Expected "."';
    End;
End;

Function CheckFraction(sValue : String; Var sError : String) : Boolean;
Begin
  Result := False;
  If Not StringIsInteger32(sValue) Then
    sError := 'Fraction Value '+sValue+' is not numerical'
  Else
    Result := True;
End;

Function CheckSection(sFormat, sContent : String; Var iYear, iMonth : Integer; Var sError : String; Var bInFraction : Boolean) : Boolean;
Begin
  Result := True;

  If Splice(sFormat, 'YYYY') Then
    Result := CheckYear(splice(sContent, 4), sError, iYear);

  If Result And Splice(sFormat, 'MM') Then
    Result := CheckMonth(splice(sContent, 2), sError, iMonth);

  If Result And Splice(sFormat, 'DD') Then
    Result := CheckDay(splice(sContent, 2), iYear, iMonth, sError);

  If Result And Splice(sFormat, 'HH') Then
    Result := CheckHour(splice(sContent, 2), sError);

  If Result And Splice(sFormat, 'NN') Then
    Result := CheckMinute(splice(sContent, 2), sError);

  If Result And Not bInFraction And Splice(sFormat, 'SS') Then
    Result := CheckSecond(splice(sContent, 2), sError);

  If Result And Not bInFraction And Splice(sFormat, '.') Then
    Result := CheckDot(splice(sContent, 2), sError, bInFraction);

  While Result And bInFraction And Splice(sFormat, 'S') Do
    Result := CheckFraction(splice(sContent, 2), sError);

  If sFormat <> '' Then
    Begin
    Result := False;
    sError := 'The Date Format '+sFormat+' is not known';
    End;
End;

Function CheckSections(sFormat, sContent : String; Const  sOrigFormat, sOrigValue : String; Var sError : String) : Boolean;
Var
  bFirst : Boolean;
  sToken : String;
  sSection : String;
  bInFraction : Boolean;
  iYear : Integer;
  iMonth : Integer;
Begin
  Result := True;
  sFormat := StringStrip(sFormat, ']');
  bInFraction := False;
  bFirst := True;
  iYear := 0;
  iMonth := 0;
  Repeat
    StringSplit(sFormat, '[', sToken, sFormat);
    If sToken <> '' Then  // support use of [ at first point to make everything optional
      Begin
      sSection := Copy(sContent, 1, Length(sToken));
      If sSection = '' Then
        Begin
        If bFirst Then
          Begin
          Result := False;
          sError := StringFormat('The section %s in the Date format %s was not found in the value %s', [sToken, sOrigFormat, sOrigValue]);
          End;
        End
      Else If Length(sSection) < Length(sToken) Then
        Begin
        Result := False;
        sError := StringFormat('The section %s in the Date format %s was not completed in the value %s - value was %s', [sToken, sOrigFormat, sSection, sOrigValue]);
        End
      Else If Not CheckSection(sToken, sSection, iYear, iMonth, sError, bInFraction) Then
        Begin
        Result := False;
        sError := StringFormat('%s (in the Date format %s and the value %s)', [sError, sOrigFormat, sOrigValue]);
        End
      Else
        sContent := Copy(sContent, Length(sSection)+1, $FF);
      End;
    bFirst := False;
  Until Not Result Or (sFormat = '') Or (sContent = '');
End;

Function CheckDateFormat(Const sFormat, sContent : String; Var sError : String) : Boolean;
Var
  bTimezone : Boolean;
  sValue : String;
  sTimezone : String;
  sDateFormat : String;
Begin
  sDateFormat := sFormat;
  bTimezone := Pos('[+/-ZZZZ]', sDateFormat) > 0;
  If bTimezone Then
    Delete(sDateFormat, Pos('[+/-ZZZZ]', sDateFormat), 9);
  StringSplit(sContent, ['+', '-'], sValue, sTimezone);

  Result := CheckSections(sDateFormat, sValue, sFormat, sContent, sError);

  If Result Then
    Result := CheckTimezone(bTimezone, sTimezone, sFormat, sError);
End;

Function ReadIntegerSection(Const sValue : String; iStart, iLength : Integer; sformat : String; iPad : Integer = 0) : Integer;
Var
  sVal : String;
Begin
  if length(sFormat) < iStart + iLength - 1 then
    result := 0
  else
  begin
    sVal := Copy(sValue, iStart, iLength);
    If iPad > 0 Then
      sVal := StringPadLeft(sVal, '0', iPad);
    If (sVal <> '') And StringIsInteger32(sVal) Then
      Result := StringToInteger32(sVal)
    Else
      Result := 0;
  end;
End;

Type
  TTimezoneStatus = (tzNone, tzNeg, tzPos);

Procedure ReadDateSections(Const sFormat, sValue : String;
                 Var iYear, iMonth, iDay, iHour, iMin, iSec, iMSec : Integer;
                 Var aTZStatus : TTimezoneStatus;
                 Var iTZHour, iTZMin : Integer);
Var
  sContent : String;
  sTimezone : String;
  iHourStart : Integer;
Begin
  If Pos('+', sValue) > 0 Then
    aTZStatus := tzPos
  Else If Pos('-', sValue) > 0 Then
    aTZStatus := tzNeg
  Else
    aTZStatus := tzNone;

  Stringsplit(sValue, ['+', '-'], sContent, sTimezone);

  If (sFormat <> '') And (sFormat[1] = 'Y') Then
    Begin
    iHourStart := 8;
    iYear := ReadIntegerSection(sContent, 1, 4, sFormat);
    iMonth := ReadIntegerSection(sContent, 5, 2, sFormat);
    iDay := ReadIntegerSection(sContent, 7, 2, sFormat);
    End
  Else
    Begin
    iHourStart := 0;
    iYear := 0;
    iMonth := 0;
    iDay := 0;
    End;

  iHour := ReadIntegerSection(sContent, iHourStart + 1, 2, sFormat);
  iMin := ReadIntegerSection(sContent, iHourStart + 3, 2, sFormat);
  iSec := ReadIntegerSection(sContent, iHourStart + 5, 2, sFormat);
  iMSec := ReadIntegerSection(sContent, iHourStart + 8, 3, sFormat, 3);

  If sTimezone <> '' Then
    Begin
    iTZHour := ReadIntegerSection(sTimezone, 1, 2, 'YYMM');
    iTZMin := ReadIntegerSection(sTimezone, 3, 2, 'YYMM');
    End
  Else
    Begin
    iTZHour := 0;
    iTZMin := 0;
    End;
End;

Function HL7StringToDate(Const sFormat, sValue : String; bCorrectForTimezone : Boolean):TDateTime;
// this is much looser than the tight validation above
Var
  iYear : Integer;
  iMonth : Integer;
  iDay : Integer;
  iHour : Integer;
  iMin : Integer;
  iSec : Integer;
  iMSec : Integer;
  aTZStatus : TTimezoneStatus;
  iTZHour : Integer;
  iTZMin : Integer;
Begin
  ReadDateSections(sFormat, sValue, iYear, iMonth, iDay, iHour, iMin, iSec, iMSec, aTZStatus, iTZHour, iTZMin);

  if iYear+ iMonth + iDay = 0 then
    result := EncodeTime(iHour, iMin, iSec, iMSec)
  Else
    Result := EncodeDate(iYear, iMonth, iDay) + EncodeTime(iHour, iMin, iSec, iMSec);

  If bCorrectForTimezone And (aTZStatus <> tzNone) Then // if no timezone specified then local timezone is assumed
    Begin
    If aTZStatus = tzNeg Then
      Result := Result + EncodeTime(iTZHour, iTZMin, 0, 0) + TimezoneBias // or should it be minus?
    Else
      Result := Result - EncodeTime(iTZHour, iTZMin, 0, 0) + TimezoneBias // or should it be minus?
    End;
End;

Function HL7DateToString(aValue : TDateTime; Const sFormat : String; bTimezone : Boolean) : String;
Begin
  Result := FormatDateTime(sFormat, aValue);
  If bTimezone Then
    If TimezoneBias < 0 Then
      Result := Result+'-'+FormatDateTime('HHNN', TimezoneBias)
    Else
      Result := Result+'+'+FormatDateTime('HHNN', TimezoneBias)
End;

Procedure CheckDOBReal(Const sDate, sDescription: String);
Var
  aValue : TDateTime;
  sError : String;
  iYear : Integer;
  iMonth : Integer;
  iDay : Integer;
  iHour : Integer;
  iMin : Integer;
  iSec : Integer;
  iMSec : Integer;
  aTZStatus : TTimezoneStatus;
  iTZHour : Integer;
  iTZMin : Integer;
Begin
  ReadDateSections('YYYYMMDD', sDate, iYear, iMonth, iDay, iHour, iMin, iSec, iMSec, aTZStatus, iTZHour, iTZMin);

  If Not CheckDateFormat('YYYY[MM[DD[HH[MM[SS[.S[S[S[S]]]]]]]]][+/-ZZZZ]', sDate, sError) Then
    Raise EHL7V2Exception.Create(Nil, hecBadField, 'CheckDOBReal', 'DOB ' + sDescription + ' is not a valid Date: '+sError);

  ReadDateSections('YYYYMMDD', sDate, iYear, iMonth, iDay, iHour, iMin, iSec, iMSec, aTZStatus, iTZHour, iTZMin);

  If iYear < 1850 Then
    Raise EHL7V2Exception.Create(Nil, hecBadField, 'CheckDOBReal', 'DOB ' + sDescription + ' is not a valid Date: Year ' + IntegerToString(iYear) + ' is not plausible');

  aValue := HL7StringToDate('YYYYMMDD', sDate, True);
  If aValue > LocalDateTime + 30 * DATETIME_MINUTE_ONE Then
    Raise EHL7V2Exception.Create(Nil, hecBadField, 'CheckDOBReal', 'DOB ' + sDescription + ' is not a valid Date: Value '+sDate+'" is in the future');
End;

Procedure CheckDODReal(Const sDate, sDescription: String);
Var
  aValue : TDateTime;
  sError : String;
  iYear : Integer;
  iMonth : Integer;
  iDay : Integer;
  iHour : Integer;
  iMin : Integer;
  iSec : Integer;
  iMSec : Integer;
  aTZStatus : TTimezoneStatus;
  iTZHour : Integer;
  iTZMin : Integer;
Begin
  ReadDateSections('YYYYMMDD', sDate, iYear, iMonth, iDay, iHour, iMin, iSec, iMSec, aTZStatus, iTZHour, iTZMin);

  If Not CheckDateFormat('YYYY[MM[DD[HH[NN[SS[.S[S[S[S]]]]]]]]][+/-ZZZZ]', sDate, sError) Then
    Raise EHL7V2Exception.Create(Nil, hecBadField, 'CheckDODReal', 'DOD ' + sDescription + ' is not a valid Date: '+sError);

  ReadDateSections('YYYYMMDD', sDate, iYear, iMonth, iDay, iHour, iMin, iSec, iMSec, aTZStatus, iTZHour, iTZMin);

  If iYear < 1850 Then
    Raise EHL7V2Exception.Create(Nil, hecBadField, 'CheckDODReal', 'DOD ' + sDescription + ' is not a valid Date: Year ' + IntegerToString(iYear) + ' is not plausible');

  aValue := HL7StringToDate('YYYYMMDD', sDate, True);
  If aValue > LocalDateTime + 30 * DATETIME_MINUTE_ONE Then
    Raise EHL7V2Exception.Create(Nil, hecBadField, 'CheckDODReal', 'DOD ' + sDescription + ' is not a valid Date: Value '+sDate+'" is in the future');
End;

end.
