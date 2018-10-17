unit FHIR.v2.Tests;

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
  SysUtils, Classes,
  DUnitX.TestFramework,
  IdTCPConnection, FHIR.v2.Protocol,
  FHIR.Support.Stream,
  FHIR.Base.Objects,
  FHIR.R4.PathNode, FHIR.R4.PathEngine,
  FHIR.v2.Base, FHIR.v2.Dictionary, FHIR.v2.Dictionary.Compiled, FHIR.v2.Dictionary.Database, FHIR.v2.Objects, FHIR.v2.Message;

const
  TEST_PORT = 20032; // err, we hope that this is unused

type
  [TextFixture]
  Tv2DictTests = Class (TObject)
  public
    [TestCase] Procedure TestDictionaryCompiled;
    [TestCase] Procedure TestDictionaryAccess;
  end;

  [TextFixture]
  THL7v2ParserTests = Class (TObject)
  private
    function parse(msg : String; fmt : THL7V2Format) : THL7v2Message;
  public
    [SetupFixture] procedure Setup;
    [TestCase] Procedure TestDictionaryParse;
  end;

  [TextFixture]
  Tv2ParserTests = Class (TObject)
  private
    procedure test(source : String);
  public
    [TestCase] Procedure TestSimple;
    [TestCase] Procedure TestFHIRPath;
  end;


type
  [TextFixture]
  TLLPTests = Class (TObject)
  private
    FDelay: Integer;
    procedure MessageReply(Sender: TObject; AConnection: TIdTCPConnection; Msg: TBytes; var VHandled: Boolean; var Reply: TBytes);
  public
    [Setup] procedure Setup;
    [TestCase] procedure TestNoConnectionServer;
    [TestCase] procedure TestNoConnectionClient;
    [TestCase] procedure TestConnection;
    [TestCase] procedure TestConnectionLimit;
    [TestCase] procedure TestSyncForwards;
    [TestCase] procedure TestSyncBackwards;
    [TestCase] procedure TestSyncForwards1000;
    [TestCase] procedure TestSyncBackwards1000;
    [TestCase] procedure TestSingleThread;
    [TestCase] procedure TestSingleThreadTimeout;
  end;

implementation


Function StringAsBytes(s : String):TBytes;
Begin
  {$IFDEF VER130}

  {$ELSE}
  result := TEncoding.UTF8.GetBytes(s);
  {$ENDIF}
End;

Function BytesAsString(a : TBytes) : String;
var
  i : integer;
Begin
  setLength(result, length(a));
  for i := Low(a) to High(a) do
   result[i + 1] := Char(a[i]);
End;

{ Tv2Tests }

procedure Tv2DictTests.TestDictionaryAccess;
var
  dict : THL7V2Dictionary;
begin
  dict := THL7V2AccessDictionary.Create('C:\temp\hl7_94Jul2018.mdb');
  try
    Assert.IsTrue(dict <> nil);
    Assert.IsTrue(dict.Model[hv23].Tables.Count > 0);
  finally
    dict.Free;
  end;
end;

procedure Tv2DictTests.TestDictionaryCompiled;
var
  dict : THL7V2Dictionary;
begin
  dict := THL7V2CompiledDictionary.Create;
  try
    Assert.IsTrue(dict <> nil);
    Assert.IsTrue(dict.Model[hv23].Tables.Count > 0);
  finally
    dict.Free;
  end;
end;

{ TLLPTests }

procedure TLLPTests.Setup;
begin
  FDelay := 0;
end;

procedure TLLPTests.MessageReply(Sender: TObject; AConnection: TIdTCPConnection; Msg: TBytes; var VHandled: Boolean; var Reply: TBytes);
begin
  VHandled := True;
  if FDelay <> 0 then
    begin
    sleep(FDelay);
    end;
  reply := StringAsBytes(BytesAsString(Msg) + 'Return');
end;

procedure TLLPTests.TestConnection;
var
  LIn: Tv2Protocol;
  LOut: Tv2Protocol;
begin
  LIn := Tv2Protocol.Create(NIL);
  try
    LIn.CommunicationMode := cmSynchronous;
    LIn.Port := TEST_PORT;
    LIn.IsListener := True;
    LIn.OnReceiveMessage := MessageReply;
    LIn.Start;
    LOut := Tv2Protocol.Create(NIL);
    try
      LOut.CommunicationMode := cmSynchronous;
      LOut.IsListener := False;
      LOut.Address := 'localhost';
      LOut.Port := TEST_PORT;
      LOut.Start;
      LIn.WaitForConnection(2000);
      Assert.IsTrue(LIn.Connected and LOut.Connected);
      LOut.PreStop;
      LOut.Stop;
    finally
      FreeAndNil(LOut);
      end;
    LIn.PreStop;
    LIn.Stop;
  finally
    FreeAndNil(LIn);
  end;
end;

procedure TLLPTests.TestConnectionLimit;
var
  LIn: Tv2Protocol;
  LOut, LOut2: Tv2Protocol;
begin
  LIn := Tv2Protocol.Create(NIL);
  try
    LIn.CommunicationMode := cmSynchronous;
    LIn.ConnectionLimit := 1;
    LIn.Port := TEST_PORT;
    LIn.IsListener := True;
    LIn.OnReceiveMessage := MessageReply;
    LIn.Start;
    LOut := Tv2Protocol.Create(NIL);
    try
      LOut.CommunicationMode := cmSynchronous;
      LOut.Address := 'localhost';
      LOut.Port := TEST_PORT;
      LOut.IsListener := False;
      LOut.Start;
      LIn.WaitForConnection(2000);
      LOut2 := Tv2Protocol.Create(NIL);
      try
        LOut2.CommunicationMode := cmSynchronous;
        LOut2.Address := 'localhost';
        LOut2.Port := TEST_PORT;
        LOut2.IsListener := False;
        LOut2.Start;
        sleep(500);

        Assert.IsTrue(LIn.Connected and LOut.Connected and not LOut2.connected);
        LOut2.PreStop;
        LOut2.Stop;
      finally
        FreeAndNil(LOut2);
        end;

      LOut.PreStop;
      LOut.Stop;
    finally
      FreeAndNil(LOut);
      end;
    LIn.PreStop;
    LIn.Stop;
  finally
    FreeAndNil(LIn);
  end;
end;

procedure TLLPTests.TestNoConnectionClient;
var
  LHL7: Tv2Protocol;
begin
  LHL7 := Tv2Protocol.Create(NIL);
  try
    LHL7.Address := '127.0.0.1';
    LHL7.Port := TEST_PORT; // hopefully this is not listening
    LHL7.CommunicationMode := cmSynchronous;
    LHL7.IsListener := False;
    Assert.IsTrue(LHL7.Status = isStopped, 'Status not stopped when stopped');
    LHL7.start;
    Assert.IsTrue(LHL7.Status in [isNotConnected, isConnecting, isWaitReconnect], 'Status not connecting when should be connecting');
    sleep(2000);
    Assert.IsTrue(LHL7.Status in [isNotConnected, isConnecting, isWaitReconnect], 'Status not connecting when should be connecting');
    sleep(2000);
    Assert.IsTrue(LHL7.Status in [isNotConnected, isConnecting, isWaitReconnect], 'Status not connecting when should be connecting');
    sleep(2000);
    Assert.IsTrue(LHL7.Status in [isNotConnected, isConnecting, isWaitReconnect], 'Status not connecting when should be connecting');
  finally
    FreeAndNil(LHL7);
  end;
end;

procedure TLLPTests.TestNoConnectionServer;
var
  LHL7: Tv2Protocol;
begin
  LHL7 := Tv2Protocol.Create(NIL);
  try
    LHL7.Address := '';
    LHL7.Port := TEST_PORT; // hopefully this is not listening
    LHL7.CommunicationMode := cmSynchronous;
    LHL7.IsListener := False;
    Assert.IsTrue(LHL7.Status = isStopped, 'Status not stopped when stopped');
    LHL7.start;
    Assert.IsTrue(LHL7.Status = isNotConnected, 'Status not connecting when should be connecting');
    sleep(2000);
    Assert.IsTrue(LHL7.Status = isNotConnected, 'Status not connecting when should be connecting');
    sleep(2000);
    Assert.IsTrue(LHL7.Status = isNotConnected, 'Status not connecting when should be connecting');
    sleep(2000);
    Assert.IsTrue(LHL7.Status = isNotConnected, 'Status not connecting when should be connecting');
  finally
    FreeAndNil(LHL7);
  end;
end;

procedure TLLPTests.TestSingleThread;
var
  LIn: Tv2Protocol;
  LOut: Tv2Protocol;
  LMsg: TBytes;
  LResult: TSendResponse;
begin
  LIn := Tv2Protocol.Create(NIL);
  try
    LIn.CommunicationMode := cmSynchronous;
    LIn.Port := TEST_PORT;
    LIn.IsListener := True;
    LIn.OnReceiveMessage := MessageReply;
    LIn.Start;
    LOut := Tv2Protocol.Create(NIL);
    try
      LOut.CommunicationMode := cmSingleThread;
      LOut.Address := 'localhost';
      LOut.IsListener := False;
      LOut.Port := TEST_PORT;
      LOut.Start;
      LOut.WaitForConnection(2000);
      LOut.SendMessage(StringAsBytes('testsinglethread'));
      repeat
        sleep(20);
        LResult := LOut.GetReply(LMsg);
      until LResult <> srNone;
      Assert.IsTrue(LResult = srOK, 'Status is wrong');
      Assert.IsTrue(SameText(BytesAsString(LMsg), 'testsinglethreadReturn'), 'Did not receive message from responder');
      Assert.IsTrue(LOut.GetReply(LMsg) = srError, 'Status is wrong');
    finally
      FreeAndNil(LOut);
      end;
  finally
    FreeAndNil(LIn);
    end;
end;

procedure TLLPTests.TestSingleThreadTimeout;
var
  LIn: Tv2Protocol;
  LOut: Tv2Protocol;
  LMsg: TBytes;
  LResult: TSendResponse;
begin
  FDelay := 2000;
  LIn := Tv2Protocol.Create(NIL);
  try
    LIn.CommunicationMode := cmSynchronous;
    LIn.Port := TEST_PORT;
    LIn.IsListener := True;
    LIn.OnReceiveMessage := MessageReply;
    LIn.Start;
    LOut := Tv2Protocol.Create(NIL);
    try
      LOut.CommunicationMode := cmSingleThread;
      LOut.Address := 'localhost';
      LOut.IsListener := False;
      LOut.Port := TEST_PORT;
      LOut.TimeOut := 50;
      LOut.Start;
      LOut.WaitForConnection(2000);
      LOut.SendMessage(StringAsBytes('testsinglethread'));
      repeat
        sleep(20);
        LResult := LOut.GetReply(LMsg);
      until LResult <> srNone;
      Assert.IsTrue(LResult = srTimeout, 'Status is wrong');
      Assert.IsTrue(length(LMsg) = 0, 'received message in error');
    finally
      FreeAndNil(LOut);
      end;
  finally
    FreeAndNil(LIn);
    end;
end;

procedure TLLPTests.TestSyncBackwards;
var
  LIn: Tv2Protocol;
  LOut: Tv2Protocol;
  LMsg: TBytes;
begin
  LIn := Tv2Protocol.Create(NIL);
  try
    LIn.CommunicationMode := cmSynchronous;
    LIn.Address := 'localhost';
    LIn.Port := TEST_PORT;
    LIn.IsListener := True;
    LIn.OnReceiveMessage := MessageReply;
    LIn.Start;
    LOut := Tv2Protocol.Create(NIL);
    try
      LOut.CommunicationMode := cmSynchronous;
      LOut.IsListener := False;
      LOut.Port := TEST_PORT;
      LOut.Start;
      LIn.WaitForConnection(6000);
      Sleep(50);
      Assert.IsTrue(LIn.Connected, 'in not connected');
      Assert.IsTrue(LOut.Connected, 'Out not connected');
      LOut.CheckSynchronousSendResult(LOut.SynchronousSend(StringAsBytes('test'), LMsg), '');
      Assert.IsTrue(SameText(BytesAsString(LMsg), 'testReturn'), 'Msg returned was wrong ("' + BytesAsString(LMsg)+ '")');
      LOut.PreStop;
      LOut.Stop;
    finally
      FreeAndNil(LOut);
      end;
    LIn.PreStop;
    LIn.Stop;
  finally
    FreeAndNil(LIn);
   end;
end;

procedure TLLPTests.TestSyncBackwards1000;
var
  LIn: Tv2Protocol;
  LOut: Tv2Protocol;
  LMsg: TBytes;
  i: Integer;
  LRes : TSendResponse;
begin
  LIn := Tv2Protocol.Create(NIL);
  try
    LIn.CommunicationMode := cmSynchronous;
    LIn.Address := 'localhost';
    LIn.Port := TEST_PORT;
    LIn.IsListener := True;
    LIn.OnReceiveMessage := MessageReply;
    LIn.Start;
    LOut := Tv2Protocol.Create(NIL);
    try
      LOut.CommunicationMode := cmSynchronous;
      LOut.IsListener := False;
      LOut.Port := TEST_PORT;
      LOut.Start;
      LIn.WaitForConnection(2000);
      Sleep(50);
      for i := 0 to 1000 do
        begin
        LRes := LOut.SynchronousSend(StringAsBytes('test' + IntToStr(i)), LMsg);
        Assert.IsTrue(LRes = srOK, 'Message '+inttostr(i)+' failed to be sent ('+SEND_RESPONSE_NAMES[LRes]+')');
        Assert.IsTrue(SameText(BytesAsString(LMsg), 'test' + IntToStr(i) + 'Return'), 'Message '+inttostr(i)+' was wrong (expected "test' + IntToStr(i) + 'Return", got "' + BytesAsString(LMsg) + '")');
        end;
      LOut.PreStop;
      LOut.Stop;
    finally
      FreeAndNil(LOut);
      end;
    LIn.PreStop;
    LIn.Stop;
  finally
    FreeAndNil(LIn);
    end;
end;

procedure TLLPTests.TestSyncForwards;
var
  LIn: Tv2Protocol;
  LOut: Tv2Protocol;
  LMsg: TBytes;
begin
  LIn := Tv2Protocol.Create(NIL);
  try
    LIn.CommunicationMode := cmSynchronous;
    LIn.Port := TEST_PORT;
    LIn.OnReceiveMessage := MessageReply;
    LIn.IsListener := True;
    LIn.Start;
    LOut := Tv2Protocol.Create(NIL);
    try
      LOut.CommunicationMode := cmSynchronous;
      LOut.IsListener := False;
      LOut.Address := 'localhost';
      LOut.Port := TEST_PORT;
      LOut.Start;
      LIn.WaitForConnection(2000);
      Assert.IsTrue(LOut.SynchronousSend(StringAsBytes('test'), LMsg) = srOK);
      Assert.IsTrue(SameText(BytesAsString(LMsg), 'testReturn'));
      LOut.PreStop;
      LOut.Stop;
    finally
      FreeAndNil(LOut);
      end;
    LIn.PreStop;
    LIn.Stop;
  finally
    FreeAndNil(LIn);
    end;
end;

procedure TLLPTests.TestSyncForwards1000;
var
  LIn: Tv2Protocol;
  LOut: Tv2Protocol;
  LMsg: TBytes;
  i: Integer;
begin
  LIn := Tv2Protocol.Create(NIL);
  try
    LIn.CommunicationMode := cmSynchronous;
    LIn.Port := TEST_PORT;
    LIn.OnReceiveMessage := MessageReply;
    LIn.IsListener := True;
    LIn.Start;
    LOut := Tv2Protocol.Create(NIL);
    try
      LOut.CommunicationMode := cmSynchronous;
      LOut.IsListener := False;
      LOut.Address := 'localhost';
      LOut.Port := TEST_PORT;
      LOut.Start;
      LIn.WaitForConnection(6000);
      for i := 0 to 1000 do
        begin
        Assert.IsTrue(LOut.SynchronousSend(StringAsBytes('test' + IntToStr(i)), LMsg) = srOK);
        Assert.IsTrue(SameText(BytesAsString(LMsg), 'test' + IntToStr(i) + 'Return'), 'expected "'+'test' + IntToStr(i) + 'Return'+'" but got "'+BytesAsString(LMsg)+'"');
        end;
      LOut.PreStop;
      LOut.Stop;
    finally
      FreeAndNil(LOut);
      end;
    LIn.PreStop;
    LIn.Stop;
  finally
    FreeAndNil(LIn);
  end;
end;

{ THL7v2ParserTests }

function THL7v2ParserTests.parse(msg: String; fmt: THL7V2Format): THL7v2Message;
var
  parser : THL7V2Decoder;
begin
  parser := GHL7V2DecoderFactory.ProduceDecoder(fmt);
  try
    result := parser.parse(msg);
  finally
    GHL7V2DecoderFactory.ConsumeDecoder(parser);
  end;
end;

procedure THL7v2ParserTests.Setup;
begin
  if GHL7Dict = nil then
    GHL7Dict := THL7V2CompiledDictionary.Create;
  if GHL7V2DecoderFactory = nil then
    GHL7V2DecoderFactory := THL7V2DecoderFactory.Create(GHL7Dict.link);
end;

procedure THL7v2ParserTests.TestDictionaryParse;
var
  msg : THL7V2Message;
begin
  msg := parse('MSH|^~\&|GHH LAB|ELAB-3|GHH OE|BLDG4|200202150930||ORU^R01|CNTRL-3456|P|2.4'#13+
    'PID|||555-44-4444||EVERYWOMAN^EVE^E^^^^L|JONES|19620320|F|||153 FERNWOOD DR.^^STATESVILLE^OH^35292||(206)3345232|(206)752-121||||AC555444444||67-A4335^OH^20030520'#13+
    'OBR|1|845439^GHH OE|1045813^GHH LAB|15545^GLUCOSE|||200202150730||||||||| 555-55-5555^PRIMARY^PATRICIA P^^^^MD^^|||||||||F||||||444-44-4444^HIPPOCRATES^HOWARD H^^^^MD'#13+
    'OBX|1|SN|1554-5^GLUCOSE^POST 12H CFST:MCNC:PT:SER/PLAS:QN||^182|mg/dl|70_105|H|||F'#13, hfER7);
  try
    Assert.IsNotNull(msg);
  finally
    msg.Free;
  end;
end;

{ Tv2ParserTests }

procedure Tv2ParserTests.test(source : String);
var
  msg : TV2Message;
  output : String;
begin
  msg := TV2Parser.parse(source);
  try
    output := TV2Composer.composeString(msg);
  finally
    msg.Free;
  end;
  StringToFile(source, 'c:\temp\source.hl7', TEncoding.UTF8);
  StringToFile(output, 'c:\temp\output.hl7', TEncoding.UTF8);
  Assert.AreEqual(source, output);
end;

procedure Tv2ParserTests.TestFHIRPath;
var
  msg : TV2Message;
  path : TFHIRPathEngine;
  list : TFHIRSelectionList;
begin
  msg := TV2Parser.parse('MSH|^~\&|GHH LAB|ELAB-3|GHH OE|BLDG4|200202150930||ORU^R01|CNTRL-3456|P|2.4'#13+
    'PID|||555-44-4444||EVERYWOMAN^EVE^E^^^^L|JONES|19620320|F|||153 FERNWOOD DR.^^STATESVILLE^OH^35292||(206)3345232|(206)752-121||||AC555444444||67-A4335^OH^20030520'#13+
    'OBR|1|845439^GHH OE|1045813^GHH LAB|15545^GLUCOSE|||200202150730||||||||| 555-55-5555^PRIMARY^PATRICIA P^^^^MD|||||||||F||||||444-44-4444^HIPPOCRATES^HOWARD H^^^^MD'#13+
    'OBX|1|SN|1554-5^GLUCOSE^POST 12H CFST:MCNC:PT:SER/PLAS:QN||^182|mg/dl|70_105|H|||F'#13);
  try
    path := TFHIRPathEngine.Create(nil, nil);
    try
      path.registerExtension(TV2FHIRPathExtensions.create);
      list := path.evaluate(nil, msg, 'Message.segment.where(code = ''PID'').field[5].element.first().text()');
      try
        Assert.AreEqual(1, list.count);
      finally
        list.free;
      end;
    finally
      path.Free;
    end;
  finally
    msg.Free;
  end;

end;

procedure Tv2ParserTests.TestSimple;
begin
  test('MSH|^~\&|GHH LAB|ELAB-3|GHH OE|BLDG4|200202150930||ORU^R01|CNTRL-3456|P|2.4'#13+
    'PID|||555-44-4444||EVERYWOMAN^EVE^E^^^^L|JONES|19620320|F|||153 FERNWOOD DR.^^STATESVILLE^OH^35292||(206)3345232|(206)752-121||||AC555444444||67-A4335^OH^20030520'#13+
    'OBR|1|845439^GHH OE|1045813^GHH LAB|15545^GLUCOSE|||200202150730||||||||| 555-55-5555^PRIMARY^PATRICIA P^^^^MD|||||||||F||||||444-44-4444^HIPPOCRATES^HOWARD H^^^^MD'#13+
    'OBX|1|SN|1554-5^GLUCOSE^POST 12H CFST:MCNC:PT:SER/PLAS:QN||^182|mg/dl|70_105|H|||F'#13);
end;

initialization
  TDUnitX.RegisterTestFixture(Tv2DictTests);
  TDUnitX.RegisterTestFixture(THL7v2ParserTests);
  TDUnitX.RegisterTestFixture(Tv2ParserTests);
  TDUnitX.RegisterTestFixture(TLLPTests);
end.
