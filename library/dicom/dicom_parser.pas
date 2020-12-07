Unit dicom_parser;

{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

Interface

Uses
  SysUtils,
  fsl_base, fsl_utilities, fsl_stream, fsl_fpc,
  dicom_dictionary, dicom_objects, dicom_writer;

  
Const
  {$IFDEF UNICODE}
  B_0 = 0;
  {$ELSE}
  B_0 = 0;
  {$ENDIF}

Type
  TDicomByteRole = (dburUnknown, dburError, dburUnknownTag, dburKnownTag, dburVRCode, dburLength, dburDead, dburValue,
      dburRepChar, dburSeqStart, dburSeqBreak, dburSeqEnd, dburMarker, dburContextId, dburResult, dburStatus, dburContent, dburFragment);
  TDicomVRRepresentationPolicy = (dvrpImplicit, dvrpExplicit, dvrpDetect);

  TMap = array of TDicomByteRole;
  TMapBool = array of Boolean;

  TDicomParserContext = class (TFslObject)
  Private
    FInput: TFslStream;
    FDictionary : TDicomDictionary;
    FMakeMap : Boolean;
    FMap : TMap;
    FMapBig : TMapBool;
    FMapOffset : Cardinal;
    FErrorMessage : String;
    FVRRepresentation : TDicomVRRepresentationPolicy;
    FBigEndian : Boolean;
    FCheckTypes: Boolean;

    procedure SetInput(const Value: TFslStream);
    procedure SetDictionary(const Value: TDicomDictionary);
    Procedure Initialise;
    Procedure Finalise;
    procedure CheckMapLength(l: cardinal);
    function GetRole(iIndex: Cardinal): TDicomByteRole;
    function GetBig(iIndex: Cardinal): Boolean;
  Public
    destructor Destroy; Override;

    Function Link : TDicomParserContext; Overload;

    Procedure Mark(aRole : TDicomByteRole; iBytes : cardinal);
    Procedure Backmark(aRole : TDicomByteRole; iBytes : cardinal);
    Procedure Unwind(iBytes : cardinal);
    Procedure SetByte(aRole : TDicomByteRole; iByte : Cardinal);


    Property MakeMap : Boolean read FMakeMap write FMakeMap;
    Property Input : TFslStream read FInput write SetInput;
    Property Dictionary : TDicomDictionary read FDictionary write SetDictionary;
    Property VRRepresentation : TDicomVRRepresentationPolicy read FVRRepresentation write FVRRepresentation;
    Property BigEndian : Boolean read FBigEndian write FBigEndian;
    Property ErrorMessage : String read FErrorMessage write FErrorMessage;
    Property CheckTypes : Boolean read FCheckTypes write FCheckTypes;
    Property Role[iIndex : Cardinal] : TDicomByteRole read GetRole;
    Property Big[iIndex : Cardinal] : Boolean read GetBig;
  End;

  TDicomParserBase = class (TFslObject)
  Private
    FMaxLength: Cardinal;
    FContext : TDicomParserContext;
    FOffset : Cardinal;
    FFileBufferCutoffSize: Integer;
    Procedure SetContext(oValue : TDicomParserContext);
    Function MakeError(iBack : cardinal; sMessage : String) : Exception;
//    Function BufferFactory(iSize : Integer): TFslBuffer;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(iMaxLength : Integer);
    destructor Destroy; Override;

    Property MaxLength : Cardinal read FMaxLength write FMaxLength;
    Property Context : TDicomParserContext read FContext write SetContext;
    Property FileBufferCutoffSize : Integer read FFileBufferCutoffSize write FFileBufferCutoffSize;
  End;

  TDicomParser = class (TDicomParserBase)
  private
    FCache : Word;
    FHasCache : Boolean;
    FVRCheckTotal : Integer;
    FVRCheckMatch : Integer;
    FMapStart : Cardinal;


    Procedure ReadPair(p : Pointer; aRole : TDicomByteRole);
    Function Peek : Word;
    Function ReadWord(aRole : TDicomByteRole) : Word;
    Function ReadCardinal(aRole : TDicomByteRole) : Cardinal;
    Function ReadLength(bExplicit, bIs4Byte : Boolean; out iLengthLen : Cardinal) : Cardinal;

    Function CheckNextIsVR : Boolean;
    Procedure ParseValues(oValues : TDicomValueList; iLength, iLengthLen : Cardinal; sTag : String);

    Function ParseElement(iGroupId, iElementId : Word) : TDicomDataElement;

    Function ParseObject(iLength : Cardinal; iGroupOnly : Word) : TDicomObject;
    Procedure ParseObjectsExplicit(oObjects : TDicomObjectList);
    Procedure ParseObjectsMeasured(oObjects : TDicomObjectList; iLength : Cardinal);
    Procedure ParseObjects(oObjects : TDicomObjectList; iLength : Cardinal);

    Function PrivateExecute : TDicomObject;
  Public
    Function Execute : TDicomObject;
  End;

  TDicomFileDecoder = class (TDicomParser)
  Private
    Function PrivateExecuteFile(bParseContent : Boolean) : TDicomFile;
  Public
    Function ExecuteFile(bParseContent : Boolean = true) : TDicomFile;
  End;

  TDicomMessageDecoder = class (TDicomParser)
  Private
    Function PrivateExecuteMessage() : TDicomMessage;
  Public
    Function ExecuteMessage() : TDicomMessage;
  End;

  TDicomPDUDecoder = class (TDicomParserBase)
  private
    FInitContext: Boolean;
    Procedure ReadPair(p : Pointer; aRole : TDicomByteRole);
    Function ReadByte(aRole : TDicomByteRole) : Byte;
    Function ReadWord(aRole : TDicomByteRole) : Word;
    Function ReadCardinal(aRole : TDicomByteRole) : Cardinal;
    Function ReadEnum(iByte : Byte; aValues: Array of Byte) : Byte;
    Function ReadHeaderString(iCount : integer) : TDicomString;

    Function ParseAssociateRequest : TDicomAssociateRequestPDU;
    Function ParseAssociateAccept : TDicomAssociateAcceptPDU;
    Function ParseHeaderString : TDicomString;
    Function ParsePresentationContext : TDicomPresentationContextInfo;
    Function ParseAcceptPresentationContext : TDicomPresentationAcceptContextInfo;
    Function ParseUserData(oData : TDicomUserDataList) : Cardinal;
    Function ParseAssociateReject : TDicomAssociateRejectPDU;
    Function ParseData : TDicomDataPDU;
    Function ParseReleaseRequest : TDicomReleaseRequestPDU;
    Function ParseReleaseResponse : TDicomReleaseResponsePDU;
    Function ParseAbort : TDicomAbortPDU;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Execute : TDicomPDU;
    Function ExecuteInstance : TDicomInstance;
    Property InitContext : Boolean read FInitContext write FInitContext;
  End;

  TDicomInstanceDecoder = class (TDicomParserBase)
  private
  Public
    Function Execute : TDicomInstance;
  End;

CONST
  BYTE_ROLE_NAMES : array [TDicomByteRole] of AnsiString =
    ('Unprocessed', 'Error', 'Tag', 'Known Tag', 'VR Code', 'Length', 'Dead Bytes', 'Value', 'Repeat Character', 'Sequence Start', 'Sequence Break', 'Sequence End', 'Status Marker', 'Presentation Context ID', 'Result', 'Status Flag', 'PDU Content', 'Fragment Marker');

Implementation

Function clength(const s : TMap):Cardinal; Overload;
Begin
  result := length(s);
End;

Function clength(const s : TMapBool):Cardinal; Overload;
Begin
  result := length(s);
End;

{ TDicomParserContext }

Destructor TDicomParserContext.Destroy;
Begin
  FDictionary.Free;
  FInput.Free;
  Inherited;
End;

procedure TDicomParserContext.SetInput(const Value: TFslStream);
begin
  FInput.Free;
  FInput := Value;
end;


procedure TDicomParserContext.SetDictionary(const Value: TDicomDictionary);
begin
  FDictionary.Free;
  FDictionary := Value;
end;

Procedure TDicomParserContext.Initialise;
Begin
  if FInput = nil Then
    raise EDicomException.create('no Input provided');
  if FDictionary = nil Then
    raise EDicomException.create('no Dictionary provided');
  FErrorMessage := '';
  FMapOffset := 0;
  if MakeMap then
  Begin
    SetLength(FMap, 0);
    SetLength(FMap, 1000);
    SetLength(FMapBig, 0);
    SetLength(FMapBig, 1000);
  End;
End;


Procedure TDicomParserContext.Finalise;
Begin
  SetLength(FMap, FMapOffset);
  SetLength(FMapBig, FMapOffset);
End;


Procedure TDicomParserContext.Mark(aRole : TDicomByteRole; iBytes : cardinal);
var
  i : cardinal;
Begin
  if FMakeMap Then
  Begin
    CheckMapLength(iBytes);
    For i := FMapOffset to FMapOffset + iBytes - 1 do
      FMap[i] := aRole;
    For i := FMapOffset to FMapOffset + iBytes - 1 do
      FMapBig[i] := FBigEndian;
  End;
  inc(FMapOffset, iBytes);
End;


Procedure TDicomParserContext.Backmark(aRole : TDicomByteRole; iBytes : cardinal);
var
  i : Cardinal;
Begin
  if FMakeMap Then
    for i := FMapOffset - 1 downto FMapOffset - iBytes do
      FMap[i] := aRole;
End;

Procedure TDicomParserContext.Unwind(iBytes : cardinal);
Begin
  Dec(FMapOffset, iBytes);
End;

procedure TDicomParserContext.CheckMapLength(l: cardinal);
begin
  if not FMakeMap Then
    exit;
  if FMapOffset + l >= cLength(FMap) Then
  Begin
    SetLength(FMap, cLength(FMap)+l+1000);
    SetLength(FMapBig, cLength(FMapBig)+l+1000);
  End;
end;

Procedure TDicomParserContext.SetByte(aRole : TDicomByteRole; iByte : Cardinal);
Begin
  if not FMakeMap Then
    exit;
  assert(iByte < FMapOffset);
  FMap[iByte] := aRole;
End;


(*
function TDicomParserContext.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FInput.sizeInBytes);
  inc(result, FDictionary.sizeInBytes);
  inc(result, FMap.sizeInBytes);
  inc(result, FMapBig.sizeInBytes);
  inc(result, FMapOffset.sizeInBytes);
  inc(result, (FErrorMessage.length * sizeof(char)) + 12);
  inc(result, FVRRepresentation.sizeInBytes);
end;

      function TDicomParser.IsPDU: Boolean;
var
  iPos, iLength : int64;
  iByte : Integer;
  iLen : Cardinal;
begin
  if not (FInput is TFslAccessStream) Then
    raise EDicomException.create('Cannot determine whether stream is PDU unless stream supports positioning');

  result := False;
  iPos := TFslAccessStream(FInput).Position;
  iLength := TFslAccessStream(FInput).Size - iPos;
  Try
    iByte := readByte(dbrUnprocessed);
    if (iByte > 0) And (iByte <= 7) Then
    Begin
      iByte := ReadByte(dbrUnprocessed);
      if (iByte = 0) Then
      Begin
        iLen := ReadCardinal(true, dbrUnprocessed);
        result := iLen + 6 = iLength;
      End;
    End;
  Finally
    TFslAccessStream(FInput).Position := iPos;
    FMapOffset := 0;
  End;
end;
*)

function TDicomParserContext.Link: TDicomParserContext;
begin
  result := TDicomParserContext(Inherited Link);
end;

function TDicomParserContext.GetRole(iIndex: Cardinal): TDicomByteRole;
begin
  if iIndex >= cLength(FMap) Then
    result := dburDead
  Else
    result := FMap[iIndex];
end;

function TDicomParserContext.GetBig(iIndex: Cardinal): Boolean;
begin
  if iIndex >= cLength(FMapBig) Then
    result := False
  Else
    result := FMapBig[iIndex];
end;


{ TDicomParserBase }

{
function TDicomParserBase.BufferFactory(iSize: Integer): TFslBuffer;
begin
  if iSize < FFileBufferCutoffSize Then
    result := TFslBuffer.Create
  Else
    result := TFslBuffer.Create; // File
end;
}

constructor TDicomParserBase.Create(iMaxLength: Integer);
begin
  inherited Create;
  FMaxLength := iMaxLength;
  FContext := TDicomParserContext.Create;
end;

destructor TDicomParserBase.Destroy;
begin
  FContext.Free;
  inherited;
end;

function TDicomParserBase.MakeError(iBack: Cardinal; sMessage: String): Exception;
begin
  FContext.Backmark(dburError, iBack);
  FContext.ErrorMessage := sMessage;
  result := Exception.Create(sMessage +' @$' + inttohex(FContext.FMapOffset, 8));
end;

procedure TDicomParserBase.SetContext(oValue: TDicomParserContext);
begin
  FContext.Free;
  FContext := oValue;
  FOffset := FContext.FMapOffset;
end;


function TDicomParserBase.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FContext.sizeInBytes);
end;

{ TDicomParser }

Procedure TDicomParser.ParseObjects(oObjects : TDicomObjectList; iLength : Cardinal);
Begin
  if iLength = $FFFFFFFF then
    ParseObjectsExplicit(oObjects)
  else
    ParseObjectsMeasured(oObjects, iLength);
End;

Function TDicomParser.ParseElement(iGroupId, iElementId : Word) : TDicomDataElement;
var
  oDefn : TDicomDictionaryElement;
  sCode : AnsiString;
  aType : TDicomVRType;
  aTypes : TDicomVRTypes;
  iLength : Cardinal;
  iLengthLen : Cardinal;
  iStart : Cardinal;
  bSeq : Boolean;
  bExplicit : Boolean;
Begin
  bExplicit := false;
  aType := dvtUN;

  oDefn :=  FContext.Dictionary.FindMatchingElement(iGroupId, iElementId, false);
  iStart := FOffset - 4;

  if oDefn <> nil Then
  Begin
    FContext.Backmark(dburKnownTag, 4);
  {$IFDEF DICOMDEBUG}
    writeln('   ' +oDefn.Name);
  {$ENDIF}
  End;

  bSeq := false;
  case FContext.VRRepresentation of
    dvrpImplicit : bExplicit := false;
    dvrpExplicit : bExplicit := true;
    dvrpDetect : bExplicit := CheckNextIsVR;
  End;

  // now we have to determine: is this a sequence or a simple?
  if bExplicit then
  Begin
    SetLength(sCode, 2);
    readPair(@sCode[1], dburVRCode);
    if sCode = 'SQ' Then
    Begin
      bSeq := true;
      iLength := ReadLength(bExplicit, true, iLengthLen)
    End
    else
    begin
      aType := FContext.FDictionary.DetermineVRType(sCode);
      iLength := ReadLength(bExplicit, aType in DICOM_VRTYPES_L4, iLengthlen);
    End
  End
  Else
  Begin
    iLength := ReadLength(bExplicit, false, iLengthLen); // doesn't matter
    if oDefn <> nil Then
    Begin
      if oDefn.IsSequence then
        bSeq := true
      Else
        aType := preferredVRType(oDefn.VRTypes);
    End
    Else if iLength = $FFFFFFFF Then
      bSeq := true
    Else
      aType := dvtUN;
  End;

  if bSeq Then
  Begin
    Result := TDicomDataElement.CreateComplex(FContext.FDictionary.Link);
    Try
      result.OFfsetStart := iStart;
      Result.GroupId := iGroupId;
      Result.ElementId := iElementId;
      Result.ElementDefinition := oDefn.Link;
      ParseObjects(Result.Objects, iLength);
      result.OffsetEnd := FOffset;
      result.Link;
    Finally
      result.Free;
    End;
  End
  Else
  Begin
    if (oDefn = nil) Then
      aTypes := [aType]
    Else
    Begin
      aTypes := oDefn.VRTypes;
      if not (aType in aTypes) and (iLength > 0) and FContext.CheckTypes Then
      Begin
        raise MakeError(4, 'Expected type is not the same as encoded type on '+FormatTagValues(iGroupId, iElementId)+' (expected = '+DescribeTypes(aTypes)+', found = '+DICOM_VR_TYPE_NAMES_S[aType]);
      End;
    End;

    result := TDicomDataElement.Create(FContext.FDictionary.Link, aType, aTypes);
    Try
      result.OFfsetStart := iStart;
      result.GroupId := iGroupId;
      result.ElementId := iElementId;
      result.ElementDefinition := oDefn.Link;
      ParseValues(result.Values, iLength, iLengthLen, FormatTagValues(result.GroupId, result.ElementId));
      result.OffsetEnd := FOffset;
      result.Link;
    Finally
      result.Free;
    End;
  End;
End;


Function TDicomParser.ParseObject(iLength : Cardinal; iGroupOnly : Word) : TDicomObject;
var
  iGroupId, iElementId : Word;
  bEnded : Boolean;
  iStop : Cardinal;
Begin
  if iLength <> 0 Then
    iStop := FOffset + iLength
  Else
    iStop := 0;
  bEnded := false;
  result := TDicomObject.Create(FContext.Dictionary.Link);
  Try
    result.offsetStart := FOffset;
    while not bEnded And (FOffset <> iStop) Do
    Begin
      if (iStop <> 0) And (FOffset > iStop) Then
        raise MakeError(4, 'overrun: read past the end of the object');
      iGroupId := Peek;
      if (iGroupOnly <> $FFFF) And (iGroupId <> iGroupOnly) Then
        bEnded := true
      Else
      Begin
        iGroupId := ReadWord(dburUnknownTag);
        iElementId := ReadWord(dburUnknownTag);
        {$IFDEF DICOMDEBUG}
        writeln('read '+inttoHex(iGroupId, 4)+'/'+inttoHex(iElementId, 4));
        {$ENDIF}
        if (iElementId = 0) Then
        Begin
          FContext.Backmark(dburKnownTag, 4);
          ParseElement(0, 0).Free; // just dispose of this length
        End
        Else if (iGroupId = $FFFE) And (iElementId = $E00D) Then
        Begin
          // todo: do we care whether we expected to find one of these or not?
          FContext.Backmark(dburSeqBreak, 4);
          bEnded := (iStop = 0);
          ReadCardinal(dburDead);
        End
        Else
          result.Elements.Add(ParseElement(iGroupid, iElementId));
      End;
    End;
    result.offsetEnd := FOffset;
    result.Link;
  Finally
    result.Free;
  End;
End;

Procedure TDicomParser.ParseObjectsExplicit(oObjects : TDicomObjectList);
var
  iGroupId, iElementId : Word;
  bEnded : Boolean;
Begin
  bEnded := false;
  while not bEnded Do
  Begin
    ReadPair(@iGroupId, dburUnknownTag);
    ReadPair(@iElementId, dburUnknownTag);
    ReadCardinal(dburDead); // dispose of length
    if (iGroupId = $FFFE) And (iElementId = $E000) Then
    Begin
      FContext.Backmark(dburSeqStart, 8);
      FContext.Backmark(dburDead, 4);
      oObjects.Add(ParseObject(0, $FFFF))
    End
    Else if (iGroupId = $FFFE) And (iElementId = $E0DD) Then
    begin
      FContext.Backmark(dburSeqEnd, 8);
      FContext.Backmark(dburDead, 4);
      bEnded := true
    End
    Else
      Raise MakeError(4, 'Unexpected Tag Value '+FormatTagValues(iGroupId, iElementId)+' reading a sequence');
  End;
End;

Procedure TDicomParser.ParseObjectsMeasured(oObjects : TDicomObjectList; iLength : Cardinal);
var
  iGroupId, iElementId : Word;
  iStop : Cardinal;
Begin
  iStop := FOffset + iLength;
  while (FOffset <> iStop) Do
  Begin
    if (FOffset > iStop) Then
      raise MakeError(4, 'overun: read past the end of the sequence');
    ReadPair(@iGroupId, dburSeqStart);
    ReadPair(@iElementId, dburSeqStart);
    if (iGroupId = $FFFE) And (iElementId = $E000) Then
      oObjects.Add(ParseObject(ReadCardinal(dburLength), $FFFF))
    Else
      Raise MakeError(4, 'Unexpected Tag Value '+FormatTagValues(iGroupId, iElementId)+' reading a sequence of known length');
  End;
End;

function TDicomParser.Execute(): TDicomObject;
Begin
  FContext.Initialise;
  Try
    result := PrivateExecute;
  Finally
    FContext.Finalise;
  End;
End;

function TDicomParser.PrivateExecute(): TDicomObject;
begin
  FOFfset := 0;
  FCache := 0;
  FHasCache := false;
  FVRCheckTotal := 0;
  FVRCheckMatch := 0;
  FMapStart := FContext.FMapOffset;
  Result := ParseObject(MaxLength, $FFFF);
end;

Procedure TDicomParser.ReadPair(p : Pointer; aRole : TDicomByteRole);
var
  iCache : Word;
Begin
  if FHasCache Then
  Begin
    iCache := FCache;
    Move(iCache, p^, 2);
    FHasCache := false;
  End
  Else
    FContext.Input.Read(p^, 2);
  Inc(FOffset, 2);
  FContext.Mark(aRole, 2);
End;

Function TDicomParser.ReadLength(bExplicit, bIs4Byte : Boolean; out iLengthLen : Cardinal) : Cardinal;
Begin
  iLengthLen := 4;
  if Not bExplicit Then
  Begin
    result := ReadCardinal(dburLength);
  End
  Else if bIs4Byte Then
  Begin
    ReadWord(dburDead);
    Result := readCardinal(dburLength);
  End
  Else
  Begin
    result := ReadWord(dburLength);
    iLengthLen := 2;
  End;
End;

Function TDicomParser.ReadWord(aRole : TDicomByteRole) : Word;
Begin
  readPair(@result, aRole);
  if FContext.FBigEndian Then
    result := ((result mod $100) * $100) + (result div $100)
End;

Function TDicomParser.ReadCardinal(aRole : TDicomByteRole) : Cardinal;
var
  p : PAnsiChar;
Begin
  p := @result;
  readPair(p, aRole);
  inc(p, 2);
  readPair(p, aRole);

  if FContext.FBigEndian Then
    result := (((result mod $100) * $1000000) + (((result div $100) mod $100) * $10000) + (((result div $10000) mod $100) * $100) + (result div $1000000))
End;


Procedure CorrectForBigEndian(aType : TDicomVRType; var aBytes : TBytes);
var
  b : TByte;
Begin
  case aType of
  dvtUS:
    Begin
    b := aBytes[B_0];
    aBytes[B_0] := aBytes[B_0+1];
    aBytes[B_0+1] := b;
    End;

  dvtUL:
    Begin
    b := aBytes[B_0];
    aBytes[B_0] := aBytes[B_0+3];
    aBytes[B_0+3] := b;
    b := aBytes[B_0+1];
    aBytes[B_0+1] := aBytes[B_0+2];
    aBytes[B_0+2] := b;
    End;
  Else
    raise EDicomException.create('Unknown big endian correction for '+ DICOM_VR_TYPE_NAMES_S[aType]);
  End;
End;

Procedure TDicomParser.ParseValues(oValues : TDicomValueList; iLength, iLengthLen : Cardinal; sTag : String);
var
  aBytes : TBytes;
  oVRDefn : TDicomDictionaryVR;
  s, l : String;
  i, j : integer;
  iStart : Cardinal;
  oVR : TDicomValue;
  bRep : Boolean;
  iElem, iGr : Word;
  iLen : Cardinal;
begin
  oVRDefn := FContext.Dictionary.VRSet[oValues.KnownType];

  if (iLength >= $7FFFFFFF) And (iLength <> $FFFFFFFF) then
    raise MakeError(iLengthLen, 'Sanity Check: VR sizes greater than 2GB ('+IntToHex(iLength, 8)+') are not supported ('+sTag+')');
  if iLength > 0 Then
  Begin
    if oVRDefn.Fixed Then
    Begin
      if iLength < oVRDefn.Length Then
        Raise MakeError(iLengthLen, 'Expected a length of '+inttostr(oVRDefn.Length)+' reading '+oVRDefn.Code+' ('+sTag+') but found '+inttostr(iLength))
      Else if iLength > oVRDefn.Length Then
      Begin
        if not oVRDefn.Repeatable Then
          Raise MakeError(iLengthLen, 'Expected a length of '+inttostr(oVRDefn.Length)+' reading '+oVRDefn.Code+' ('+sTag+') but found '+inttostr(iLength))
        Else if iLength mod oVRDefn.Length <> 0 Then
          Raise MakeError(iLengthLen, 'Expected a multiple of '+inttostr(oVRDefn.Length)+' for length reading '+oVRDefn.Code+' ('+sTag+') but found '+inttostr(iLength))
      End;
      if iLength > MaxLength Then
        Raise MakeError(iLengthLen, 'Specified length reads past maximum Length ('+sTag+')');
      SetLength(aBytes, oVRDefn.Length);
      iStart := FOffset;
      for i := 0 to iLength div oVRDefn.Length - 1 Do
      Begin
        For j := 0 to (length(aBytes) div 2) - 1 Do
          ReadPair(@aBytes[(j*2)], dburValue);
        if FContext.BigEndian Then
          CorrectForBigEndian(oVRDefn.VRType, aBytes);
        oVr := oValues.Add(aBytes);
        oVr.offSetStart := iStart;
        oVr.OffsetEnd := FOffset;
      End
    End
    Else if iLength = $FFFFFFFF Then
    Begin
      // encapsulated
      iGr := ReadWord(dburFragment);
      iElem := ReadWord(dburFragment);
      while (iGr = $FFFE) And (iElem = $E000) Do
      Begin
        iLen := ReadCardinal(dburLength);
        if iLen <> 0 Then
        Begin
          iStart := FOffset;
          SetLength(aBytes, iLen);
          FContext.Input.Read(aBytes[B_0], iLen);
          FContext.Mark(dburValue, iLen);
          inc(FOffset, iLen);
          oVr := oValues.Add(aBytes);
          oVr.offSetStart := iStart;
          oVr.OffsetEnd := FOffset;
        End;
        iGr := ReadWord(dburFragment);
        iElem := ReadWord(dburFragment);
      End;
      iLen := ReadCardinal(dburLength);
      if (iGr <> $FFFE) Or (iElem = $E0D) Or (iLen <> 0) Then
        raise EDicomException.create('unexpected termination tag ('+sTag+')');
    End
    Else
    Begin
      if iLength > MaxLength Then
        Raise MakeError(iLengthLen, 'Specified length reads past maximum Length ('+sTag+')');
      SetLength(aBytes, iLength);
      iStart := FOffset;
      if iLength > 0 then
        FContext.Input.Read(aBytes[0], iLength);
      FContext.Mark(dburValue, iLength);
      inc(FOffset, iLength);
      //For i := 0 to (length(aBytes) div 2) - 1 Do
      //  ReadPair(@aBytes[(i*2)+1], dbrValue);
      if oVRDefn.Repeatable Then
      Begin
        s := Trim(BytesAsString(aBytes));
        while (s <> '') Do
        Begin
          // todo: mark this
          bRep := StringSplit(s, REP_CHAR, l, s);
          oVr := oValues.Add(StringAsBytes(l));
          oVr.offSetStart := iStart;
          oVr.OffsetEnd := iStart + clength(l);
          if bRep Then
            FContext.SetByte(dburRepChar, FMapStart + iStart + clength(l)+1);
        End;
      End
      else if oVRDefn.IsString Then
      Begin
        oVr := oValues.Add(StringAsBytes(Trim(BytesAsString(aBytes))));
        oVr.offSetStart := iStart;
        oVr.OffsetEnd := FOffset;
      End
      Else
      Begin
        oVr := oValues.Add(aBytes);
        oVr.offSetStart := iStart;
        oVr.OffsetEnd := FOffset;
      End;
    End;
  End;
end;


function TDicomParser.CheckNextIsVR: Boolean;
var
  sCode : AnsiString;
  iCache : Word;
begin
  SetLength(sCode, 2);
  iCache := Peek;
  Move(iCache, sCode[1], 2);
  if (sCode = 'SQ') or FContext.Dictionary.isValidVRType(sCode) Then
  Begin
    // it's possible that a length accidentally matches a SQ (somewhat lower than 1 in 3000 chance)
    // the chance gets greater as we parse further into the content as the length of the contents starts to grow
    if FVRCheckTotal > 5 then
      result := (FVRCheckMatch = FVRCheckTotal)
    else
      result := true;
  End
  else
    result := false;
  inc(FVRCheckTotal);
  if result then
    inc(FVRCheckMatch);

end;


function TDicomParser.Peek: Word;
begin
  if Not FHasCache then
  Begin
    FContext.Input.Read(FCache, 2);
    FHasCache := true;
  End;
  if FContext.BigEndian Then
    result := ((FCache mod $100) * $100) + (FCache div $100)
  else
    Result := FCache;
end;

{ TDicomPDUDecoder }

function TDicomPDUDecoder.ExecuteInstance: TDicomInstance;
var
  iType : Byte;
begin
  if FInitContext Then
    FContext.Initialise;
  Try
    result := TDicomInstance.Create;
    Try
      iType := ReadByte(dburUnknownTag);
      ReadByte(dburDead);
      case iType of
        1 : result.AssociateRequest := ParseAssociateRequest;
        2 : result.AssociateAccept := ParseAssociateAccept;
        3 : result.AssociateReject := ParseAssociateReject;
        4 : result.Data := ParseData;
        5 : result.ReleaseRequest := ParseReleaseRequest;
        6 : result.ReleaseResponse := ParseReleaseResponse;
        7 : result.Abort := ParseAbort;
  ///      8 : raise EDicomException.create('not known');
      Else
        raise MakeError(2, 'Unknown PDU Type '+inttostr(iType));
      End;
      result.Link;
    Finally
      result.Free;
    End;

  Finally
    if FInitContext Then
      FContext.Finalise;
  End;
end;

function TDicomPDUDecoder.Execute: TDicomPDU;
var
  iType : Byte;
begin
  if FInitContext Then
    FContext.Initialise;
  Try
    iType := ReadByte(dburUnknownTag);
    ReadByte(dburDead);
    case iType of
      1 : result := ParseAssociateRequest;
      2 : result := ParseAssociateAccept;
      3 : result := ParseAssociateReject;
      4 : result := ParseData;
      5 : result := ParseReleaseRequest;
      6 : result := ParseReleaseResponse;
      7 : result := ParseAbort;
///      8 : raise EDicomException.create('not known');
    Else
      raise MakeError(2, 'Unknown PDU Type '+inttostr(iType));
    End;

  Finally
    if FInitContext Then
      FContext.Finalise;
  End;
end;

function TDicomPDUDecoder.ParseAssociateRequest: TDicomAssociateRequestPDU;
var
  iLength : Cardinal;
  iVersion : Word;
  iByte : Byte;
begin
  result := TDicomAssociateRequestPDU.Create;
  Try
    result.OffsetStart := FOffset;
    iLength := ReadCardinal(dburLength);
    iVersion := ReadWord(dburValue);
    if iVersion mod 2 = 0 Then
      Raise MakeError(1, 'Unsupported version '+inttostr(iVersion));
    ReadByte(dburDead);
    ReadByte(dburDead);
    result.CalledEntity := ReadHeaderString(16);
    result.CallingEntity := ReadHeaderString(16);
    For iByte := 1 to 32 do
      readByte(dburDead);
    while FOffset < iLength + 6 Do
    Begin
      iByte := readByte(dburUnknownTag);
      case iByte of
        $10: Result.ApplicationContext := ParseHeaderString;
        $20: result.PresentationContexts.add(ParsePresentationContext);
        $50: result.MaxLength := ParseUserData(result.UserData);
      Else
        Raise MakeError(1, 'Unknown Tag Value $'+inttoHex(iByte, 2));
      End;
    End;

    result.OffsetEnd := FOffset;
    result.Link;
  Finally
    result.Free;
  End;
end;

function TDicomPDUDecoder.ParseAssociateAccept : TDicomAssociateAcceptPDU;
var
  iLength : Cardinal;
  iVersion : Word;
  iByte : Byte;
begin
  result := TDicomAssociateAcceptPDU.Create;
  Try
    result.OffsetStart := FOffset;
    iLength := ReadCardinal(dburLength);
    iVersion := ReadWord(dburMarker);
    if iVersion mod 2 = 0 Then
      Raise MakeError(1, 'Unsupported version '+inttostr(iVersion));
    ReadByte(dburDead);
    ReadByte(dburDead);
    result.CalledEntity := ReadHeaderString(16);
    result.CallingEntity := ReadHeaderString(16);
    For iByte := 1 to 32 do
      readByte(dburDead);
    while FOffset < iLength + 6 Do
    Begin
      iByte := readByte(dburUnknownTag);
      case iByte of
        $10: Result.ApplicationContext := ParseHeaderString;
        $21: result.PresentationContexts.add(ParseAcceptPresentationContext);
        $50: result.MaxLength := ParseUserData(result.UserData);
      Else
        Raise MakeError(1, 'Unknown Tag Value $'+inttohex(iByte, 2));
      End;
    End;

    result.OffsetEnd := FOffset;
    result.Link;
  Finally
    result.Free;
  End;
end;

function TDicomPDUDecoder.ReadHeaderString(iCount: integer): TDicomString;
var
  sValue : String;
begin
  result := TDicomString.Create;
  Try
    result.OffsetStart := FOffset;
    Setlength(sValue, iCount);
    FContext.Input.Read(sValue[1], iCount);
    Inc(FOffset, iCount);
    FContext.Mark(dburValue, iCount);
    result.Value := trim(sValue);
    result.OffsetEnd := FOffset;
    result.Link;
  Finally
    result.Free;
  End;
end;

function TDicomPDUDecoder.ParseHeaderString: TDicomString;
var
  iLen : Word;
begin
  ReadByte(dburDead);
  iLen := ReadWord(dburLength);
  result := ReadHeaderString(iLen);
end;

function TDicomPDUDecoder.ParsePresentationContext: TDicomPresentationContextInfo;
var
  iEnd : Cardinal;
  iByte : Byte;
begin
  result := TDicomPresentationContextInfo.Create;
  try
    result.OffsetStart := FOffset;
    ReadByte(dburDead);
    iEnd := ReadWord(dburLength) + FOffset;
    result.Id := readByte(dburContextId);
    if result.Id mod 2 = 0 then
      Raise MakeError(1, 'Presentation Context Id '+inttostr(result.Id)+' is not acceptable (most be odd number)');
    ReadByte(dburDead);
    ReadByte(dburDead);
    ReadByte(dburDead);
    while FOffset < iEnd Do
    begin
      iByte := ReadByte(dburUnknownTag);
      case iByte of
        $30 : result.AbstractSyntax := ParseHeaderString;
        $40 : result.TransferSyntaxes.add(ParseHeaderString);
      Else
        Raise MakeError(1, 'Unknown Tag Value $'+inttohex(iByte, 2));
      End;
    End;
    result.OffsetEnd := FOffset;
    result.Link;
  Finally
    result.Free;
  End;
end;

function TDicomPDUDecoder.ParseAcceptPresentationContext: TDicomPresentationAcceptContextInfo;
var
  iEnd : Cardinal;
  iByte : Byte;
begin
  result := TDicomPresentationAcceptContextInfo.Create;
  try
    result.OffsetStart := FOffset;
    ReadByte(dburDead);
    iEnd := ReadWord(dburLength) + FOffset;
    result.Id := readByte(dburContextId);
    if result.Id mod 2 = 0 then
      Raise MakeError(1, 'Presentation Context Id '+inttostr(result.Id)+' is not acceptable (most be odd number)');
    ReadByte(dburDead);
    result.Result := TPresentationAcceptResult(ReadEnum(ReadByte(dburResult), BYTE_VALUES_PresentationAcceptResult));
    ReadByte(dburDead);
    while FOffset < iEnd Do
    begin
      iByte := ReadByte(dburUnknownTag);
      case iByte of
        $40 : result.TransferSyntax := ParseHeaderString;
      Else
        Raise MakeError(1, 'Unknown Tag Value $'+inttoHex(iByte, 2));
      End;
    End;
    result.OffsetEnd := FOffset;
    result.Link;
  Finally
    result.Free;
  End;
end;

function TDicomPDUDecoder.ParseUserData(oData : TDicomUserDataList): Cardinal;
var
  iEnd : Cardinal;
  iByte : Byte;
  iLen : Word;
begin
  result := 0;
  ReadByte(dburDead);
  iEnd := ReadWord(dburLength) + FOffset;
  while (FOffset < iEnd) Do
  Begin
    iByte := ReadByte(dburUnknownTag);
    ReadByte(dburDead);
    iLen := ReadWord(dburLength);
    if (iByte = $51) And (iLen = 4) Then
      result := ReadCardinal(dburLength)
    Else if iLen + FOffset <= iEnd Then
      oData.Add(iByte, ReadHeaderString(iLen), FOffset - 4, FOffset + iLen)
    Else
    Begin
      FContext.BackMark(dburDead, 4);
      Break;
    End;
  End;
  while (FOffset < iEnd) Do
    ReadByte(dburDead);
end;

function TDicomPDUDecoder.ReadEnum(iByte: Byte; aValues: array of Byte): Byte;
var
  i : integer;
begin
  result := 0;
  for i := 0 to Length(aValues) - 1 Do
    if iByte = aValues[i] then
      result := i;
end;

Function TDicomPDUDecoder.ParseAssociateReject : TDicomAssociateRejectPDU;
Begin
  result := TDicomAssociateRejectPDU.Create;
  try
    result.OffsetStart := FOffset;
    ReadCardinal(dburLength);
    ReadByte(dburDead);
    result.Result := TDicomAssociateRejectResult(ReadEnum(ReadByte(dburResult), BYTE_VALUES_DicomAssociateRejectResult));
    result.Source := TDicomAssociateRejectSource(ReadEnum(ReadByte(dburMarker), BYTE_VALUES_DicomAssociateRejectSource));
    Case result.Source of
      RejectedUser : result.Reason := TDicomAssociateRejectReason(ReadEnum(ReadByte(dburMarker), BYTE_VALUES_DicomAssociateRejectReason_Souce_User));
      RejectedACSE : result.Reason := TDicomAssociateRejectReason(ReadEnum(ReadByte(dburValue), BYTE_VALUES_DicomAssociateRejectReason_Souce_ACSE));
    else //  RejectedPresentation
      result.Reason := TDicomAssociateRejectReason(ReadEnum(ReadByte(dburMarker), BYTE_VALUES_DicomAssociateRejectReason_Souce_Presentation));
    End;
    result.OffsetEnd := FOffset;
    result.Link;
  Finally
    result.Free;
  End;
End;

Function TDicomPDUDecoder.ParseData : TDicomDataPDU;
var
  iLen : Cardinal;
  iStatus : Byte;
  iEnd : Cardinal;
  b : TBytes;
  oValue : TDicomPresentationDataValue;
Begin
  result := TDicomDataPDU.Create;
  try
    result.OffsetStart := FOffset;
    iEnd := FOffset + ReadCardinal(dburLength);
    While FOffset < iEnd Do
    Begin
      oValue := TDicomPresentationDataValue.Create;
      Try
        oValue.OffsetStart := FOffset;
        iLen := ReadCardinal(dburLength);
        result.ContextId := ReadByte(dburContextId);
        dec(iLen);
        iStatus := ReadByte(dburStatus);
        oValue.Command := iStatus and 1 > 0;
        oValue.Last := iStatus and 2 > 0;
        dec(iLen);
        SetLength(b, iLen);
        FContext.Input.Read(b[B_0], iLen);
        inc(FOffset, iLen);
        FContext.Mark(dburContent, iLen);
        oValue.Bytes := b;
        oValue.OffsetEnd := FOffset;
        result.DataValues.Add(oValue.Link);
      Finally
        oValue.Free;
      End;
    End;
    result.OffsetEnd := FOffset;
    result.Link;
  Finally
    result.Free;
  End;
End;

Function TDicomPDUDecoder.ParseReleaseRequest : TDicomReleaseRequestPDU;
Begin
  result := TDicomReleaseRequestPDU.Create;
  try
    result.OffsetStart := FOffset;
    ReadCardinal(dburLength);
    ReadCardinal(dburDead);
    result.OffsetEnd := FOffset;
    result.Link;
  Finally
    result.Free;
  End;
End;

Function TDicomPDUDecoder.ParseReleaseResponse : TDicomReleaseResponsePDU;
Begin
  result := TDicomReleaseResponsePDU.Create;
  try
    result.OffsetStart := FOffset;
    ReadCardinal(dburLength);
    ReadCardinal(dburDead);
    result.OffsetEnd := FOffset;
    result.Link;
  Finally
    result.Free;
  End;
End;

Function TDicomPDUDecoder.ParseAbort : TDicomAbortPDU;
Begin
  result := TDicomAbortPDU.Create;
  try
    result.OffsetStart := FOffset;
    ReadCardinal(dburLength);
    ReadByte(dburDead);
    ReadByte(dburDead);
    result.Source := TDicomAbortSource(ReadEnum(ReadByte(dburResult), BYTE_VALUES_DicomAbortSource));
    result.Reason := TDicomAbortReason(ReadEnum(ReadByte(dburResult), BYTE_VALUES_DicomAbortReason));
    result.OffsetEnd := FOffset;
    result.Link;
  Finally
    result.Free;
  End;
End;


function TDicomPDUDecoder.ReadByte(aRole: TDicomByteRole): Byte;
begin
  FContext.Input.Read(result, 1);
  Inc(FOffset, 1);
  FContext.Mark(aRole, 1);
end;

Function TDicomPDUDecoder.ReadWord(aRole : TDicomByteRole) : Word;
Begin
  readPair(@result, aRole);
End;

Function TDicomPDUDecoder.ReadCardinal(aRole : TDicomByteRole) : Cardinal;
var
  p : PAnsiChar;
Begin
  p := @result;
  inc(p, 2);
  readPair(p, aRole);
  dec(p, 2);
  readPair(p, aRole);
End;

procedure TDicomPDUDecoder.ReadPair(p: Pointer; aRole: TDicomByteRole);
var
  pc : PAnsiChar;
Begin
  pc := p;
  inc(pc);
  FContext.Input.Read(pc^, 1);
  dec(pc);
  FContext.Input.Read(pc^, 1);
  Inc(FOffset, 2);
  FContext.Mark(aRole, 2);
end;

function TDicomPDUDecoder.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ TDicomFileDecoder }

function TDicomFileDecoder.ExecuteFile(bParseContent : Boolean): TDicomFile;
begin
  FContext.Initialise;
  Try
    result := PrivateExecuteFile(bParseContent);
  Finally
    FContext.Finalise;
  End;
end;

function TDicomFileDecoder.PrivateExecuteFile(bParseContent : Boolean): TDicomFile;
var
  aBytes : TBytes;
  oInput : TFslAccessStream;
  oSyntax : TDicomDictionaryTransferSyntax;
begin
  if not (FContext.FInput is TFslAccessStream) Then
    RaiseError('ExecuteFile', 'Files must be repositional streams');

  oInput := TFslAccessStream(FContext.FInput);
  FOFfset := 0;
  FCache := 0;
  FHasCache := false;
  FVRCheckTotal := 0;
  FVRCheckMatch := 0;
  FMapStart := FContext.FMapOffset;

  result := TDicomFile.Create;
  try
    SetLength(aBytes, 128);
    oInput.Read(aBytes[B_0], 128);
    inc(FOffset, 128);
    FContext.Mark(dburDead, 128);
    result.Prelude := aBytes;

    SetLength(aBytes, 4);
    oInput.Read(aBytes[B_0], 4);
    inc(FOffset, 4);
    FContext.Mark(dburMarker, 4);
    if BytesAsAnsiString(aBytes) <> 'DICM' Then
      raise MakeError(4, 'Error. Looking for "DICM" at position 128, and found "'+EncodePercent(BytesAsString(aBytes))+'"');

    FContext.VRRepresentation := dvrpExplicit;
    result.Header := ParseObject(0, 2);
    if bParseContent Then
    Begin
      oSyntax := FContext.Dictionary.TransferSyntaxes.GetTransferSyntaxByUID(result.TransferSyntaxUID);
      if oSyntax = nil then
      Begin
        FContext.FVRRepresentation := dvrpDetect;
        FContext.FBigEndian := False;
      End
      else
      Begin
        if oSyntax.IsExplicitVR Then
          FContext.FVRRepresentation := dvrpExplicit
        else
          FContext.FVRRepresentation := dvrpImplicit;
        FContext.FBigEndian := oSyntax.IsBigEndian;
      End;
      Result.Content := ParseObject(TFslAccessStream(FContext.Input).Size - FOffset, $FFFF);
    End;
    result.Link;
  finally
    result.Free;
  end;
end;

{ TDicomMessageDecoder }

function TDicomMessageDecoder.ExecuteMessage(): TDicomMessage;
begin
  FContext.Initialise;
  Try
    result := PrivateExecuteMessage();
  Finally
    FContext.Finalise;
  End;
end;

function TDicomMessageDecoder.PrivateExecuteMessage(): TDicomMessage;
var
  b : byte;
  aBytes : TBytes;
  oInput : TFslAccessStream;
begin
  oInput := TFslAccessStream(FContext.FInput);
  FOFfset := 0;
  FCache := 0;
  FHasCache := false;
  FVRCheckTotal := 0;
  FVRCheckMatch := 0;
  FMapStart := FContext.FMapOffset;

  result := TDicomMessage.Create;
  try
    SetLength(aBytes, 4);
    oInput.Read(aBytes[B_0], 4);
    inc(FOffset, 4);
    FContext.Mark(dburMarker, 4);
    if BytesAsAnsiString(aBytes) = 'KDM2' Then
    Begin
      FContext.Input.Read(b, 1);
      Inc(FOffset, 1);
      FContext.Mark(dburMarker, 1);
      SetLength(aBytes, b);
      oInput.Read(aBytes[B_0], b);
      inc(FOffset, b);
      FContext.Mark(dburValue, b);
      result.AbstractSyntax := BytesAsString(aBytes);

      FContext.Input.Read(b, 1);
      Inc(FOffset, 1);
      FContext.Mark(dburMarker, 1);
      SetLength(aBytes, b);
      oInput.Read(aBytes[B_0], b);
      inc(FOffset, b);
      FContext.Mark(dburValue, b);
      result.TransferSyntax := BytesAsString(aBytes);
    End
    Else if BytesAsAnsiString(aBytes) <> 'KDMS' Then
      raise MakeError(4, 'Error. Looking for "KDMS" at position 0, and found "'+EncodePercent(BytesAsString(aBytes))+'"');

    FContext.VRRepresentation := dvrpExplicit;
    Result.Command := ParseObject(0, $0000);
    Result.Data := ParseObject(TFslAccessStream(FContext.Input).Size - FOffset, $FFFF);

    Result.Link;
  finally
    result.Free;
  end;
end;

{ TDicomInstanceDecoder }

function TDicomInstanceDecoder.Execute: TDicomInstance;
var
  aBytes : TBytes;
  iLength : Cardinal;
  oFile : TDicomFileDecoder;
  oMessage : TDicomMessageDecoder;
  oObj : TDicomParser;
  oPDU : TDicomPDUDecoder;
  oInput : TFslAccessStream;
  sComp : TBytes;
  sDecomp : TBytes;
begin
  if not (FContext.FInput is TFslAccessStream) Then
    RaiseError('ExecuteFile', 'Instances must be repositional streams');

  oInput := TFslAccessStream(FContext.FInput);
  result := nil;

  // this could be a pdu, a file, or an object
  // it's a file if the characters 129-132 contain "DCIM"
  // it's a pde if the characters 3 - 6 contain the length - 6
  // otherwise we try to read it as an object
  if oInput.Size > 132 then
  Begin
    oInput.Position := 128;
    SetLength(aBytes, 4);
    FContext.FInput.Read(aBytes[B_0], 4);
    if BytesAsAnsiString(aBytes) = 'DICM' Then
    Begin
      oFile := TDicomFileDecoder.Create(oInput.Size);
      Try
        oInput.Position := 0;
        oFile.Context := Context.Link;
        result := TDicomInstance.Create;
        Try
          result.FileObject := oFile.ExecuteFile;
          result.Link;
        Finally
          result.Free;
        End;
      Finally
        oFile.Free;
      End;
    End;
  End;

  if oInput.Size > 10 then
  Begin
    oInput.Position := 0;
    SetLength(aBytes, 4);
    FContext.FInput.Read(aBytes[B_0], 4);
    if (BytesAsAnsiString(aBytes) = 'KDMS') or (BytesAsAnsiString(aBytes) = 'KDM2') Then
    Begin
      // custom kestral message format
      oMessage := TDicomMessageDecoder.Create(oInput.Size);
      Try
        oInput.Position := 0;
        oMessage.Context := Context.Link;
        result := TDicomInstance.Create;
        Try
          result.Message := oMessage.ExecuteMessage;
          result.Link;
        Finally
          result.Free;
        End;
      Finally
        oMessage.Free;
      End;
    End;
  End;

  if (result = nil) And (oInput.Size > 6) then
  Begin
    oInput.Position := 2;
    FContext.FInput.Read(iLength, 4);
    iLength := (((iLength mod $100) * $1000000) + (((iLength div $100) mod $100) * $10000) + (((iLength div $10000) mod $100) * $100) + (iLength div $1000000));
    if iLength = oInput.Size - 6 Then
    Begin
      oInput.Position := 0;
      oPDU := TDicomPDUDecoder.Create(oInput.Size);
      Try
        oPDU.Context := Context.Link;
        result := oPDU.ExecuteInstance;
      Finally
        oPDU.Free;
      End;
    End
    else if false and (iLength < oInput.Size - 6) Then
    Begin
      // what if it's a multi pdu stream? (like a stream dump?)
      oInput.Position := 0;
      Context.Initialise;
      Try
        while oInput.Readable > 0 Do
        Begin
          oPDU := TDicomPDUDecoder.Create(oInput.Size);
          Try
            oPDU.Context := Context.Link;
            oPDU.InitContext := false;
            result.Free;
            result := nil;
            result := oPDU.ExecuteInstance;
          Finally
            oPDU.Free;
          End;
        End;
      Finally
        Context.Finalise;
      End;
    End;
  End;

  if (result = nil) And (oInput.Size > 4) then
  Begin
    oInput.Position := 0;
    SetLength(aBytes, 4);
    FContext.FInput.Read(aBytes[B_0], 4);
    if BytesAsAnsiString(aBytes) = #$1F#$9D#$90#$08 Then
    Begin
      SetLength(sComp, oInput.Size);
      oInput.Position := 0;
      oInput.Read(sComp[1], oInput.Size);
      sDecomp := ZcompressBytes(sComp);
      oInput.Position := 0;

      oPDU := TDicomPDUDecoder.Create(oInput.Size);
      Try
        oPDU.Context := Context.Link;
        result := oPDU.ExecuteInstance;
      Finally
        oPDU.Free;
      End;
    End;
  End;

  if (result = nil) Then
  Begin
    oInput.Position := 0;
    oObj := TDicomParser.Create(oInput.Size);
    Try
      oObj.Context := Context.Link;
      oObj.Context.VRRepresentation := dvrpDetect;
      result := TDicomInstance.Create;
      Try
        result.SimpleObject := oObj.Execute;
        result.Link;
      Finally
        result.Free;
      End;
    Finally
      oObj.Free;
    End;
  End;
end;

End.


