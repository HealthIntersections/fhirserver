Unit dicom_Writer;

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
  fsl_base, fsl_stream, fsl_utilities,
  dicom_objects, dicom_dictionary;

Const
  MAX_WORD = 65535;
  DICOM_VRTYPES_L4 = [dvtOB, dvtOW, dvtOF, dvtUT, dvtUN];

Type
  TDicomWriter = class (TFslObject)
  private
    FOutput: TFslStream;
    FDictionary : TDicomDictionary;
    FExplicit: Boolean;
    FExplicitSQ: Boolean;
    FBigEndian : Boolean;
    FBuilder : TFslBytesBuilder;
    FOffset : Cardinal;
    FNoGroupLengths: Boolean;
    FStripped: Boolean;

    procedure SetOutput(const Value: TFslStream);
    Procedure SetDictionary(Const Value : TDicomDictionary);

    procedure writeByte(b : Byte);
    Procedure WritePair(p : pointer; bHeader : Boolean);
    Procedure WriteWord(w : Word; bHeader : Boolean);
    Procedure WriteCardinal(c : Cardinal; bHeader : Boolean);
    procedure WriteHeaderString(oValue : TDicomString; iLen : Word);

    Procedure EncodeValue(oValues : TDicomValueList);
    Procedure EncodeBytes(aBytes : TBytes);
    Procedure EncodeLength(iLong : Cardinal; bIs4Byte : Boolean);

    Procedure EncodeGroupMarker(iGroup : Word; iSize : integer);
    Procedure EncodeHeaderString(iId : Byte; oValue : TDicomString);

    Function MeasureGroup(sPath : string; oElements : TDicomDataElementList; iGroup : Word) : Cardinal;
    function MeasureElement(sPath : string; oElement: TDicomDataElement): Cardinal;
    Function MeasureValue(sPath : string; oValues : TDicomValueList) : Cardinal;
    Function MeasureSequence(sPath : string; oObjects : TDicomObjectList) : Cardinal;
    Function MeasureObject(sPath : string; oObject : TDicomObject) : Cardinal;
    function MeasureAssociateRequest(oAssociate : TDicomAssociateRequestPDU) : Cardinal;
    function MeasureAssociateAccept(oAssociate : TDicomAssociateAcceptPDU) : Cardinal;
    function MeasureUserData(oAssociate : TDicomAssociateRequestPDU) : Cardinal; Overload;
    function MeasureUserData(oAssociate : TDicomAssociateAcceptPDU) : Cardinal; Overload;
    function MeasurePresentationContext(oPresentationContext: TDicomPresentationContextInfo) : Cardinal;
    function MeasurePresentationAcceptContext(oPresentationContext: TDicomPresentationAcceptContextInfo) : Cardinal;

    Procedure EncodeElement(sPath : string; oElement : TDicomDataElement);
    Procedure EncodeObject(sPath : string; oObject : TDicomObject);
    Procedure EncodeFile(sPath : string; oFile : TDicomFile);
    Procedure EncodeMessage(sPath : string; oMessage : TDicomMessage);

    Procedure EncodeAssociateRequest(sPath : string; oAssociate : TDicomAssociateRequestPDU);
    Procedure EncodePresentationContext(sPath : string; oPresentationContext : TDicomPresentationContextInfo);
    Procedure EncodeAssociateAccept(sPath : string; oAssociate : TDicomAssociateAcceptPDU);
    Procedure EncodePresentationAcceptContext(sPath : string; oPresentationContext : TDicomPresentationAcceptContextInfo);

    Procedure EncodePresentationDataValue(iContextId : Byte; oDataValue : TDicomPresentationDataValue);

    Procedure EncodeData(sPath : string; oData : TDicomDataPDU);
    Procedure EncodeAssociateReject(sPath : string; oAssociateReject : TDicomAssociateRejectPDU);
    Procedure EncodeAbort(sPath : string; oAbort : TDicomAbortPDU);
    Procedure EncodeReleaseRequest(sPath : string; oReleaseRequest : TDicomReleaseRequestPDU);
    Procedure EncodeReleaseResponse(sPath : string; oReleaseResponse : TDicomReleaseResponsePDU);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Procedure Execute(sPath : string; oInstance : TDicomInstance); Overload;
    Procedure Execute(sPath : string; oData : TDicomObject); Overload;
    Procedure Execute(sPath : string; oFile : TDicomFile); Overload;
    Procedure Execute(sPath : string; oMessage : TDicomMessage); Overload;
    Procedure Execute(sPath : string; oPDU : TDicomPDU); Overload;
    Procedure Execute(sPath : string; oData : TDicomDataElement); Overload;

    Property Output : TFslStream read FOutput write SetOutput;
    Property Dictionary : TDicomDictionary read FDictionary write SetDictionary;
    Property Explicit : Boolean read FExplicit write FExplicit;
    Property ExplicitSQ : Boolean read FExplicitSQ write FExplicitSQ;
    Property BigEndian : Boolean read FBigEndian write FBigEndian;
    Property Stripped : Boolean read FStripped write FStripped;
  End;



Implementation

{ TDicomWriter }

Constructor TDicomWriter.Create;
Begin
  Inherited;
  FBuilder := TFslBytesBuilder.Create;
End;

destructor TDicomWriter.Destroy;
begin
  FOutput.Free;
  FDictionary.Free;
  FBuilder.Free;
  inherited;
end;

procedure TDicomWriter.SetOutput(const Value: TFslStream);
begin
  FOutput.Free;
  FOutput := Value;
end;

procedure TDicomWriter.SetDictionary(const Value: TDicomDictionary);
begin
  FDictionary.Free;
  FDictionary := Value;
end;

procedure TDicomWriter.Execute(sPath : string; oData : TDicomObject);
begin
  Assert(invariants('Execute', FOutput, TFslStream, 'Output'));

  FOffset := 0;

  EncodeObject(sPath, oData);
end;

procedure TDicomWriter.Execute(sPath : string; oData : TDicomDataElement);
begin
  Assert(invariants('Execute', FOutput, TFslStream, 'Output'));

  FOffset := 0;

  EncodeElement(sPath, oData);
end;

procedure TDicomWriter.writeByte(b: Byte);
begin
  Output.Write(b, 1);
  inc(FOffset);
end;

procedure TDicomWriter.WritePair(p : Pointer; bHeader : Boolean);
var
  pc : PAnsiChar;
begin
  if bHeader Then
  Begin
    pc := p;
    inc(pc);
    Output.Write(pc^, 1);
    dec(pc);
    Output.Write(pc^, 1);
    inc(FOffset, 2);
  End
  Else
  Begin
    Output.Write(p^, 2);
    inc(FOffset, 2);
  End;
end;

procedure TDicomWriter.WriteCardinal(c: Cardinal; bHeader : Boolean);
var
  p : PAnsiChar;
begin
  p := @c;
  if bHeader Then
  Begin
    inc(p, 2);
    WritePair(p, bHeader);
    dec(p, 2);
    WritePair(p, bHeader);
  End
  Else
  Begin
    WritePair(p, bHeader);
    inc(p, 2);
    WritePair(p, bHeader);
  End;
end;

procedure TDicomWriter.WriteWord(w: Word; bHeader : Boolean);
var
  p : PAnsiChar;
begin
  p := @w;
  WritePair(p, bHeader);
end;

Procedure TDicomWriter.EncodeGroupMarker(iGroup : Word; iSize : integer);
var
  s : AnsiString;
Begin
  WriteWord(iGroup, false);
  WriteWord(0, false);
  if Explicit Then
  Begin
    s := 'UL';
    writePair(@s[1], false);
    WriteWord(4, false);
  End
  else
    WriteCardinal(4, false);
  WriteCardinal(iSize, false);
End;

procedure TDicomWriter.EncodeLength(iLong: Cardinal; bis4Byte : Boolean);
begin
  if not FExplicit Then
    WriteCardinal(iLong, false)
  Else if bIs4Byte Then
  Begin
    WriteWord(0, false);
    WriteCardinal(iLong, false);
  End
  Else
    WriteWord(iLong, false);
end;

procedure TDicomWriter.EncodeBytes(aBytes: TBytes);
begin
  if length(aBytes) > 0 Then
  Begin
    inc(FOffset, length(aBytes));
    Output.Write(aBytes[0], length(aBytes));
  End;
end;

Procedure TDicomWriter.EncodeValue(oValues : TDicomValueList);
var
  i : integer;
Begin
  if Stripped And (oValues.KnownType in [dvtOB,dvtOW,dvtOF,dvtUN]) Then
  Begin
    EncodeLength(0, oValues.KnownType in DICOM_VRTYPES_L4);
  End
  Else if (oValues.Count > 1) And (oValues.KnownType in DICOM_VRTYPES_NOTREPEATED) Then
  Begin
    FBuilder.Clear;

    if oValues.Count > 0 then
    Begin
      FBuilder.Append(oValues[0].AsUN);
      oValues[0].ClearOffsets;
    End;
    for i := 1 to oValues.Count - 1 Do
    Begin
      oValues[i].ClearOffsets;
      FBuilder.Append(oValues[i].AsUN);
    End;

    if FBuilder.Length mod 2 = 1 then
      FBuilder.Append(#0);

    EncodeLength(FBuilder.Length, oValues.KnownType in DICOM_VRTYPES_L4);
    EncodeBytes(FBuilder.AsBytes);
  End
  Else
  Begin
    FBuilder.Clear;
    if oValues.Count > 0 then
    Begin
      FBuilder.Append(oValues[0].AsUN);
      oValues[0].ClearOffsets;
    End;
    for i := 1 to oValues.Count - 1 Do
    Begin
      oValues[i].ClearOffsets;
      if oValues.Dictionary.VRSet[oValues.KnownType].IsString Then
        FBuilder.Append(REP_CHAR);
      FBuilder.Append(oValues[i].AsUN);
    End;

    if FBuilder.Length mod 2 = 1 then
      if oValues.KnownType = dvtUI Then
        FBuilder.Append(#0)
      Else
        FBuilder.Append(' ');
    EncodeLength(FBuilder.Length, oValues.KnownType in DICOM_VRTYPES_L4);
    EncodeBytes(FBuilder.AsBytes);
  End;
End;


procedure TDicomWriter.EncodeElement(sPath : string; oElement: TDicomDataElement);
var
  s : AnsiString;
  iLength, iEnd, iStart : Cardinal;
  i : integer;
begin
  Assert(Invariants('Encode', oElement, TDicomDataElement, 'Element'));
  oElement.OffsetStart := FOffset;
  WritePair(@oElement.GroupId, false);
  WritePair(@oElement.ElementId, false);
  if oElement.IsSimple Then
  Begin
    Assert(Invariants('Encode', oElement.Values, TDicomValueList, 'Value'));
    if Explicit Then
    Begin
      s := DICOM_VR_TYPE_NAMES_A[oElement.Values.KnownType];
      writePair(@s[1], false);
    End;
    EncodeValue(oElement.Values);
  End
  Else
  Begin
    if ExplicitSQ Then
      iLength := MeasureSequence(sPath+'\'+oElement.Tag, oElement.Objects)
    Else
      iLength := $FFFFFFFF;

    if Explicit Then
    Begin
      s := 'SQ';         
      writePair(@s[1], false);
      writeWord(0, false);
      writeCardinal(iLength, false);
    End
    else
      writeCardinal(iLength, false);

    for i := 0 to oElement.Objects.Count - 1 Do
    Begin
      WriteWord($FFFE, false);
      WriteWord($E000, false);
      if ExplicitSQ Then
        iLength := MeasureObject(sPath+'\'+oElement.Tag, oElement.Objects[i])
      Else
        iLength := $FFFFFFFF;
      writeCardinal(iLength, false);

      iStart := FOffset;
      if ExplicitSQ Then
        iEnd := iStart + iLength
      else
        iEnd := 0;

      EncodeObject(sPath+'\'+oElement.Tag, oElement.Objects[i]);
      if ExplicitSQ Then
        CheckCondition(FOffset = iEnd, 'EncodeElement', 'Encoding Object '+sPath+'\'+oElement.Tag+' failed. Stated Length was '+inttostr(iLength)+' but actual length was '+inttostr(FOffset - iStart))
      Else
      Begin
        WriteWord($FFFE, false);
        WriteWord($E00D, false);
        WriteCardinal(0, false);
      End;
    End;
    if not ExplicitSQ Then
    Begin
      WriteWord($FFFE, false);
      WriteWord($E0DD, false);
      WriteCardinal(0, false);
    End;
  End;
  oElement.OffsetEnd := FOffset;
end;

Procedure TDicomWriter.EncodeObject(sPath : string; oObject : TDicomObject);
var
  i : integer;
  w : word;
Begin
  oObject.OffsetStart := FOffset;
  w := MAX_WORD;
  for i := 0 to oObject.Elements.Count - 1 Do
  Begin
    if Not FNoGroupLengths And (w <> oObject.Elements[i].GroupId) Then
      EncodeGroupMarker(oObject.Elements[i].GroupId, MeasureGroup(sPath, oObject.Elements, oObject.Elements[i].GroupId));
    w := oObject.Elements[i].GroupId;
    EncodeElement(sPath, oObject.Elements[i]);
  End;
  oObject.OffsetEnd := FOffset;
End;

function TDicomWriter.MeasureGroup(sPath : string; oElements: TDicomDataElementList; iGroup: Word): Cardinal;
var
  i : integer;
begin
  result := 0;
  For i := 0 to oElements.Count - 1 do
    if oElements[i].GroupId = iGroup Then
      result := result + MeasureElement(sPath + '\'+oElements[i].Tag, oElements[i]);
end;


function TDicomWriter.MeasureElement(sPath : string; oElement: TDicomDataElement): Cardinal;
Begin
  if oElement.IsSimple Then
    result := 4 + MeasureValue(sPath, oElement.Values)
  Else if Explicit Then
    result := 12 + MeasureSequence(sPath, oElement.Objects)
  Else
    result := 8 + MeasureSequence(sPath, oElement.Objects);
End;

Function RoundUp(i : integer):Cardinal;
Begin
  if i mod 2 = 1 then
    result := i + 1
  Else
    result := i;
End;

Function TDicomWriter.MeasureValue(sPath : string; oValues : TDicomValueList) : Cardinal;
Var
  i : integer;
  r : cardinal;
Begin
  if FExplicit Then
  Begin
   r := 4;
   if oValues.KnownType in [dvtOB, dvtOW, dvtOF, dvtUT, dvtUN] Then
     inc(r, 4);
  End
  else
    r := 4;

  if oValues.Count > 0 Then
  Begin
    if (oValues.Count > 1) And (oValues.KnownType in DICOM_VRTYPES_NOTREPEATED) Then
    Begin
      // encapsulated
      r := 8; // terminatation sequence
      for i := 0 to oValues.Count - 1 Do
        inc(r, length(oValues[1].AsUN) + 8);
    End
    Else if not oValues.Dictionary.VRSet[oValues.KnownType].IsString and not (oValues.KnownType in [dvtPN, dvtOB]) Then
      inc(r, oValues.Dictionary.VRSet[oValues.KnownType].Length * cardinal(oValues.Count))
    Else
    Begin
      inc(r, length(oValues[0].AsUN));
      for i := 1 to oValues.Count - 1 Do
        inc(r, length(oValues[1].AsUN) + 1);
    End;
  End;
  result := RoundUp(r);
End;

function TDicomWriter.MeasureObject(sPath : string; oObject: TDicomObject): Cardinal;
var
  w : Word;
  i : integer;
  oElem : TDicomDataElement;
begin
  result := 0;
  w := MAX_WORD;
  for i := 0 to oObject.Elements.Count - 1 Do
  Begin
    oElem := oObject.Elements[i];
    if Not FNoGroupLengths And (w <> oElem.GroupId) then
      inc(result, 12);
    w := oElem.GroupId;
    inc(result, MeasureElement(sPath, oElem));
  End;
end;

function TDicomWriter.MeasureSequence(sPath : string; oObjects: TDicomObjectList): Cardinal;
var
  i : integer;
begin
  result := 0;
  for i := 0 to oObjects.Count - 1 Do
  Begin
    inc(result, 8);
    inc(result, MeasureObject(sPath, oObjects[i]));
    if Not ExplicitSQ Then
      inc(result, 8); // for the last delimiter
  End;
  if Not ExplicitSQ Then
    inc(result, 8); // for the last delimiter
end;

procedure TDicomWriter.Execute(sPath : string; oInstance: TDicomInstance);
begin
  case oInstance.InstanceType of
    ditNull : raise EDicomException.create('no content to encode');
    ditSimpleObject : Execute(sPath, oInstance.SimpleObject);
    ditFileObject : EncodeFile(sPath, oInstance.FileObject);
    ditMessage : EncodeMessage(sPath, oInstance.Message);
    ditAssociateRequestPDU : EncodeAssociateRequest(sPath, oInstance.AssociateRequest);
    ditAssociateAcceptPDU : EncodeAssociateAccept(sPath, oInstance.AssociateAccept);
    ditAssociateRejectPDU : EncodeAssociateReject(sPath, oInstance.AssociateReject);
    ditDataPDU : EncodeData(sPath, oInstance.Data);
    ditAbortPDU : EncodeAbort(sPath, oInstance.Abort);
    ditReleaseRequestPDU : EncodeReleaseRequest(sPath, oInstance.ReleaseRequest);
    ditReleaseResponsePDU : EncodeReleaseResponse(sPath, oInstance.ReleaseResponse);
  else
    raise EDicomException.create('not supported');
  End;
end;

procedure TDicomWriter.EncodeAssociateRequest(sPath : string; oAssociate: TDicomAssociateRequestPDU);
var
  i : integer;
begin
  writeByte($1);
  writeByte(0);
  WriteCardinal(MeasureAssociateRequest(oAssociate), true);
  WriteWord(1, true);
  WriteWord(0, true);
  WriteHeaderString(oAssociate.CalledEntity, 16);
  WriteHeaderString(oAssociate.CallingEntity, 16);
  for i := 1 to 32 do
    writeByte(0);
  EncodeHeaderString($10, oAssociate.ApplicationContext);
  for i := 0 to oAssociate.PresentationContexts.Count - 1 do
    EncodePresentationContext(sPath, oAssociate.PresentationContexts[i]);
  writeByte($50);
  writeByte(0);
  WriteWord(MeasureUserData(oAssociate), true);
  writeByte($51);
  writeByte(0);
  writeWord(4, true);
  writeCardinal(oAssociate.MaxLength, true);
  for i := 0 to oAssociate.UserData.Count - 1  do
    EncodeHeaderString(oAssociate.UserData[i].Id, oAssociate.UserData[i]);
end;

procedure TDicomWriter.EncodePresentationContext(sPath : string; oPresentationContext: TDicomPresentationContextInfo);
var
  i : integer;
begin
  WriteByte($20);
  writeByte(0);
  WriteWord(MeasurePresentationContext(oPresentationContext), true);
  WriteByte(oPresentationContext.Id);
  writeByte(0);
  writeByte(0);
  writeByte(0);
  EncodeHeaderString($30, oPresentationContext.AbstractSyntax);
  For i := 0 to oPresentationContext.TransferSyntaxes.Count - 1 Do
    EncodeHeaderString($40, oPresentationContext.TransferSyntaxes[i]);
end;

procedure TDicomWriter.EncodeHeaderString(iId: Byte; oValue: TDicomString);
var
  sValue : String;
begin
  writeByte(iId);
  writeByte(0);
  if oValue <> nil Then
    sValue := oValue.Value;
  WriteWord(length(sValue), true);
  WriteHeaderString(oValue, length(sValue));
end;

procedure TDicomWriter.WriteHeaderString(oValue: TDicomString; iLen: Word);
var
  i : integer;
  v : String;
  sValue : AnsiString;
begin
  if oValue <> nil Then
    v := oValue.Value
  else
    v := '';
  v := StringPadRight(v, ' ', iLen);
  sValue := AnsiString(v);
  for i := 1 to length(sValue) do
    writeByte(byte(sValue[i]));
end;

procedure TDicomWriter.EncodeAssociateAccept(sPath : string; oAssociate: TDicomAssociateAcceptPDU);
var
  i : integer;
begin
  writeByte($2);
  writeByte(0);
  WriteCardinal(MeasureAssociateAccept(oAssociate), true);
  WriteWord(1, true);
  WriteWord(0, true);
  WriteHeaderString(oAssociate.CalledEntity, 16);
  WriteHeaderString(oAssociate.CallingEntity, 16);
  for i := 1 to 32 do
    writeByte(0);
  EncodeHeaderString($10, oAssociate.ApplicationContext);
  for i := 0 to oAssociate.PresentationContexts.Count - 1 do
    EncodePresentationAcceptContext(sPath, oAssociate.PresentationContexts[i]);
  writeByte($50);
  writeByte(0);
  WriteWord(MeasureUserData(oAssociate), true);
  writeByte($51);
  writeByte(0);
  writeWord(4, true);
  writeCardinal(oAssociate.MaxLength, true);
  for i := 0 to oAssociate.UserData.Count - 1  do
    EncodeHeaderString(oAssociate.UserData[i].Id, oAssociate.UserData[i]);
end;

procedure TDicomWriter.EncodePresentationAcceptContext(sPath : string; oPresentationContext: TDicomPresentationAcceptContextInfo);
begin
  WriteByte($21);
  writeByte(0);
  WriteWord(MeasurePresentationAcceptContext(oPresentationContext), true);
  WriteByte(oPresentationContext.Id);
  writeByte(0);
  writeByte(BYTE_VALUES_PresentationAcceptResult[oPresentationContext.result]);
  writeByte(0);
  EncodeHeaderString($40, oPresentationContext.TransferSyntax);
end;


function TDicomWriter.MeasureAssociateRequest(oAssociate: TDicomAssociateRequestPDU): Cardinal;
var
  i : integer;
begin
  result :=
    2 + // protocol version
    2 + // reserved
    16 + // called AE
    16 + // calling AE
    32 + // reserved
    length(oAssociate.ApplicationContext.Value)+4;
  for i := 0 to oAssociate.PresentationContexts.Count - 1 do
    result := result + MeasurePresentationContext(oAssociate.PresentationContexts[i])+4;
  result := result + MeasureUserData(oAssociate)+4;
end;

function TDicomWriter.MeasureAssociateAccept(oAssociate: TDicomAssociateAcceptPDU): Cardinal;
var
  i : integer;
begin
  result :=
    2 + // protocol version
    2 + // reserved
    16 + // called AE
    16 + // calling AE
    32 + // reserved
    length(oAssociate.ApplicationContext.Value)+4;
  for i := 0 to oAssociate.PresentationContexts.Count - 1 do
    result := result + MeasurePresentationAcceptContext(oAssociate.PresentationContexts[i])+4;
  result := result + MeasureUserData(oAssociate)+4;
end;

function TDicomWriter.MeasurePresentationContext(oPresentationContext: TDicomPresentationContextInfo): Cardinal;
var
  i : integer;
  r : integer;
begin
  r :=
    4 + // id + reserved
    4 + length(oPresentationContext.AbstractSyntax.Value);
  for i := 0 to oPresentationContext.TransferSyntaxes.Count - 1 Do
    r := r + 4 + length(oPresentationContext.TransferSyntaxes[i].Value);
  result := r;
end;

function TDicomWriter.MeasureUserData(oAssociate: TDicomAssociateRequestPDU): Cardinal;
var
  i, r : integer;
begin
  r := 8; // maxlength
  for i := 0 to oAssociate.UserData.Count - 1 Do
    r := r + 4 + length(oAssociate.UserData[i].value);
  result := r;
end;

function TDicomWriter.MeasureUserData(oAssociate: TDicomAssociateAcceptPDU): Cardinal;
var
  i, r : integer;
begin
  r := 8; // maxlength
  for i := 0 to oAssociate.UserData.Count - 1 Do
    r := r + 4 + length(oAssociate.UserData[i].value);
  result := r;
end;

function TDicomWriter.MeasurePresentationAcceptContext(oPresentationContext: TDicomPresentationAcceptContextInfo): Cardinal;
begin
  result :=
    4 + // id + reserved
    4 + length(oPresentationContext.TransferSyntax.Value);
end;

Procedure TDicomWriter.EncodeAssociateReject(sPath : string; oAssociateReject : TDicomAssociateRejectPDU);
Begin
  writeByte($3);
  writeByte(0);
  WriteCardinal(4, true);
  writeByte(0);
  writeByte(BYTE_VALUES_DicomAssociateRejectResult[oAssociateReject.result]);
  writeByte(BYTE_VALUES_DicomAssociateRejectSource[oAssociateReject.Source]);
  case oAssociateReject.Source of
    RejectedUser : writeByte(BYTE_VALUES_DicomAssociateRejectReason_Souce_User[oAssociateReject.Reason]);
    RejectedACSE : writeByte(BYTE_VALUES_DicomAssociateRejectReason_Souce_ACSE[oAssociateReject.Reason]);
  else //  RejectedPresentation
    writeByte(BYTE_VALUES_DicomAssociateRejectReason_Souce_Presentation[oAssociateReject.Reason]);
  End;
End;

Procedure TDicomWriter.EncodeAbort(sPath : string; oAbort : TDicomAbortPDU);
Begin
  writeByte($7);
  writeByte(0);
  WriteCardinal(4, true);
  writeByte(0);
  writeByte(0);
  writeByte(BYTE_VALUES_DicomAbortSource[oAbort.source]);
  writeByte(BYTE_VALUES_DicomAbortReason[oAbort.reason]);
End;

Procedure TDicomWriter.EncodeReleaseRequest(sPath : string; oReleaseRequest : TDicomReleaseRequestPDU);
Begin
  writeByte($5);
  writeByte(0);
  WriteCardinal(4, true);
  writeCardinal(0, true);
End;

Procedure TDicomWriter.EncodeReleaseResponse(sPath : string; oReleaseResponse : TDicomReleaseResponsePDU);
Begin
  writeByte($6);
  writeByte(0);
  WriteCardinal(4, true);
  writeCardinal(0, true);
End;

Procedure TDicomWriter.EncodeData(sPath : string; oData : TDicomDataPDU);
var
  iLen : Cardinal;
  i : integer;
Begin
  writeByte($4);
  writeByte(0);
  iLen := 0;
  For i := 0 to oData.DataValues.Count - 1 Do
    inc(iLen, 6 + Length(oData.DataValues[i].Bytes));

  WriteCardinal(iLen, true);

  For i := 0 to oData.DataValues.Count - 1 Do
    EncodePresentationDataValue(oData.ContextId, oData.DataValues[i]);
End;

procedure TDicomWriter.EncodePresentationDataValue(iContextId: Byte; oDataValue: TDicomPresentationDataValue);
var
  iByte : Byte;
begin
  WriteCardinal(Length(oDataValue.Bytes)+2, true);
  WriteByte(iContextId);
  if oDataValue.Command then
    iByte := 1
  Else
    iByte := 0;
  if oDataValue.Last then
    iByte := iByte or 2;
  WriteByte(iByte);
  Output.Write(oDataValue.Bytes[0], length(oDataValue.Bytes));
  inc(FOffset, Length(oDataValue.Bytes));
end;

procedure TDicomWriter.EncodeFile(sPath : string; oFile: TDicomFile);
var
  aBytes : TBytes;
  oSyntax : TDicomDictionaryTransferSyntax;
begin
  aBytes := FillBytes(oFile.Prelude, 0, 128);
  Output.Write(aBytes[0], 128);
  aBytes := AnsiStringAsBytes('DICM');
  Output.Write(aBytes[0], 4);
  FExplicit := true;
  FBigEndian := false;
  Execute(sPath, oFile.Header);
  FNoGroupLengths := false;
  oSyntax := FDictionary.TransferSyntaxes.GetTransferSyntaxByUID(oFile.TransferSyntaxUID);
  if oSyntax = nil then
  Begin
    Explicit := true;
    FBigEndian := False;
  End
  else
  Begin
    Explicit := oSyntax.IsExplicitVR;
    FBigEndian := oSyntax.IsBigEndian;
  End;
  ExplicitSQ := true;
  Execute(sPath, oFile.Content);
end;

procedure TDicomWriter.EncodeMessage(sPath : string; oMessage : TDicomMessage);
var
  aBytes : TBytes;
begin
  aBytes := AnsiStringAsBytes('KDM2');
  Output.Write(aBytes[0], 4);
  inc(FOffset, 4);
  aBytes := StringAsBytes(oMessage.AbstractSyntax);
  writeByte(length(aBytes));
  Output.Write(aBytes[0], length(aBytes));
  inc(FOffset, length(aBytes));
  aBytes := StringAsBytes(oMessage.TransferSyntax);
  writeByte(length(aBytes));
  Output.Write(aBytes[0], length(aBytes));
  inc(FOffset, length(aBytes));

  Explicit := true;
  Execute(sPath+'\Command', oMessage.Command);
  Execute(sPath+'\Data', oMessage.Data);
end;

procedure TDicomWriter.Execute(sPath : string; oPDU: TDicomPDU);
begin
  case oPDU.PacketType of
    pduAssociateRequest : EncodeAssociateRequest(sPath, TDicomAssociateRequestPDU(oPDU));
    pduAssociateAccept : EncodeAssociateAccept(sPath, TDicomAssociateAcceptPDU(oPDU));
    pduAssociateReject : EncodeAssociateReject(sPath, TDicomAssociateRejectPDU(oPDU));
    pduData : EncodeData(sPath, TDicomDataPDU(oPDU));
    pduAbort : EncodeAbort(sPath, TDicomAbortPDU(oPDU));
    pduReleaseRequest : EncodeReleaseRequest(sPath, TDicomReleaseRequestPDU(oPDU));
    pduReleaseResponse : EncodeReleaseResponse(sPath, TDicomReleaseResponsePDU(oPDU));
  else
    raise EDicomException.create('not supported');
  End;
end;

procedure TDicomWriter.Execute(sPath : string; oFile: TDicomFile);
begin
  EncodeFile(sPath, oFile);
end;

procedure TDicomWriter.Execute(sPath : string; oMessage: TDicomMessage);
begin
  EncodeMessage(sPath, oMessage);
end;

function TDicomWriter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FOutput.sizeInBytes);
  inc(result, FDictionary.sizeInBytes);
  inc(result, FBuilder.sizeInBytes);
end;

End.
