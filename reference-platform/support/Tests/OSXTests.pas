unit OSXTests;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  DUnitX.TestFramework;

{
to test:

TAdvFile
stuff in file support


}

type
  [TextFixture]
  TOSXTests = class (TObject)
  private
    procedure test60sec;
  public
    [TestCase] procedure TestAdvObject;
    [TestCase] procedure TestCriticalSectionSimple;
    [TestCase] procedure TestCriticalSectionThreaded;
    [TestCase] procedure TestKCriticalSectionThreaded;
    [TestCase] procedure TestKCriticalSectionSimple;
    [TestCase] procedure TestSemaphore;
    [TestCase] procedure TestTemp;
    [TestCase] procedure TestDateTimeEx;
    [TestCase] procedure TestAdvFile;
  end;


implementation

uses
  SysUtils, Classes, SyncObjs,
  {$IFDEF MACOS} OSXUtils, {$ELSE} Windows, {$ENDIF}
  SystemSupport, DateSupport, FileSupport, kCritSct,
  AdvObjects, AdvFiles;

var
  globalInt : cardinal;
  cs : TRTLCriticalSection;
  kcs : TCriticalSection;
  sem : TSemaphore;

Const
  TEST_FILE_CONTENT : AnsiString = 'this is some test content'+#13#10;

procedure TOSXTests.test60sec;
begin
  TDateTimeEx.make(EncodeDate(2013, 4, 5) + EncodeTime(12, 34, 60, 0), dttzUnknown).toHL7
end;

procedure TOSXTests.TestAdvFile;
var
  filename : String;
  f : TAdvFile;
  s : AnsiString;
begin
  filename := Path([SystemTemp, 'delphi.file.test.txt']);
  if FileExists(filename) then
  begin
    FileSetReadOnly(filename, false);
    FileDelete(filename);
  end;
  Assert.IsFalse(FileExists(filename));
  f := TAdvFile.Create(filename, fmCreate);
  try
    f.Write(TEST_FILE_CONTENT[1], length(TEST_FILE_CONTENT));
  finally
    f.Free;
  end;
  Assert.IsTrue(FileExists(filename));
  Assert.IsTrue(FileSize(filename) = 27);
  f := TAdvFile.Create(filename, fmOpenRead);
  try
    SetLength(s, f.Size);
    f.Read(s[1], f.Size);
    Assert.IsTrue(s = TEST_FILE_CONTENT);
  finally
    f.Free;
  end;
  FileSetReadOnly(filename, true);
  FileDelete(filename);
  Assert.IsTrue(FileExists(filename));
  FileSetReadOnly(filename, false);
  FileDelete(filename);
  Assert.IsFalse(FileExists(filename));
end;

procedure TOSXTests.TestAdvObject;
var
  obj : TAdvObject;
begin
  obj := TAdvObject.Create;
  try
    Assert.IsTrue(obj.AdvObjectReferenceCount = 0);
    obj.Link;
    Assert.IsTrue(obj.AdvObjectReferenceCount = 1);
    obj.Free;
    Assert.IsTrue(obj.AdvObjectReferenceCount = 0);
  finally
    obj.Free;
  end;
end;

procedure TOSXTests.TestCriticalSectionSimple;
begin
  InitializeCriticalSection(cs);
  try
    EnterCriticalSection(cs);
    try
      Assert.IsTrue(true);
    finally
      LeaveCriticalSection(cs);
    end;
  finally
    DeleteCriticalSection(cs);
  end;
end;

procedure TOSXTests.TestKCriticalSectionSimple;
begin
  kcs := TCriticalSection.Create('test');
  try
    kcs.Enter;
    try
      Assert.IsTrue(true);
    finally
      kcs.Leave;
    end;
  finally
    kcs.Free;
  end;
end;

type
  TTestCriticalSectionThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TTestKCriticalSectionThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TTestSemaphoreThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TOSXTests.TestCriticalSectionThreaded;
begin
  globalInt := GetCurrentThreadId;
  InitializeCriticalSection(cs);
  try
    EnterCriticalSection(cs);
    try
      TTestCriticalSectionThread.create();
      Sleep(10);
      Assert.IsTrue(globalInt = GetCurrentThreadId);
    finally
      LeaveCriticalSection(cs);
    end;
    sleep(10);
    EnterCriticalSection(cs);
    try
      Assert.IsTrue(globalInt <> GetCurrentThreadId);
    finally
      LeaveCriticalSection(cs);
    end;
  finally
    DeleteCriticalSection(cs);
  end;
end;

procedure TOSXTests.TestKCriticalSectionThreaded;
begin
  globalInt := GetCurrentThreadId;
  kcs := TCriticalSection.Create('none');
  try
    kcs.Enter;
    try
      TTestKCriticalSectionThread.create();
      Sleep(10);
      Assert.IsTrue(globalInt = GetCurrentThreadId);
    finally
      kcs.Leave;
    end;
    sleep(10);
    kcs.Enter;
    try
      Assert.IsTrue(globalInt <> GetCurrentThreadId);
    finally
      kcs.Leave;
    end;
  finally
    kcs.free;
  end;
end;

procedure TOSXTests.TestTemp;
begin
  Assert.IsNotEmpty(SystemTemp);
end;

procedure TOSXTests.TestDateTimeEx;
var
  d1, d2 : TDateTimeEx;
  dt1, dt2 : Double;
begin
  // null
  Assert.IsTrue(d1.null);
  Assert.IsFalse(d1.notNull);
  d1 := TDateTimeEx.makeToday;
  Assert.IsTrue(d1.notNull);
  Assert.IsFalse(d1.null);
  d1 := TDateTimeEx.makeNull;
  Assert.IsTrue(d1.null);
  Assert.IsFalse(d1.notNull);

  // format support
  Assert.IsTrue(TDateTimeEx.fromXML('2013-04-05T12:34:56').toHL7 = '20130405123456');
  Assert.IsTrue(TDateTimeEx.fromXML('2013-04-05T12:34:56Z').toHL7 = '20130405123456Z');
  Assert.IsTrue(TDateTimeEx.fromXML('2013-04-05T12:34:56+10:00').toHL7 = '20130405123456+1000');
  Assert.IsTrue(TDateTimeEx.fromXML('2013-04-05T12:34:56-10:00').toHL7 = '20130405123456-1000');
  Assert.IsTrue(TDateTimeEx.fromXML('2013-04-05').toHL7 = '20130405');
  Assert.IsTrue(TDateTimeEx.fromHL7('20130405123456-1000').toXML = '2013-04-05T12:34:56-10:00');

  // Date Time conversion
  Assert.IsTrue(TDateTimeEx.make(EncodeDate(2013, 4, 5) + EncodeTime(12, 34,56, 0), dttzUnknown).toHL7 = '20130405123456');
  Assert.WillRaise(test60Sec);
  dt1 := EncodeDate(2013, 4, 5) + EncodeTime(12, 34,56, 0);
  dt2 := TDateTimeEx.fromHL7('20130405123456').DateTime;
  Assert.IsTrue(dt1 = dt2);

  // Timezone Wrangling
  d1 := TDateTimeEx.make(EncodeDate(2011, 2, 2)+ EncodeTime(14, 0, 0, 0), dttzLocal); // during daylight savings (+11)
  d2 := TDateTimeEx.make(EncodeDate(2011, 2, 2)+ EncodeTime(3, 0, 0, 0), dttzUTC); // UTC Time
  Assert.IsTrue(sameInstant(d1.DateTime - TimezoneBias(d1.DateTime), d2.DateTime));
  Assert.IsTrue(sameInstant(d1.UTC.DateTime, d2.DateTime));
  Assert.IsTrue(not d1.equal(d2));
  Assert.IsTrue(d1.sameTime(d2));
  d1 := TDateTimeEx.make(EncodeDate(2011, 7, 2)+ EncodeTime(14, 0, 0, 0), dttzLocal); // not during daylight savings (+10)
  d2 := TDateTimeEx.make(EncodeDate(2011, 7, 2)+ EncodeTime(4, 0, 0, 0), dttzUTC); // UTC Time
  dt1 := d1.DateTime - TimezoneBias(d1.DateTime);
  dt2 := d2.DateTime;
  Assert.IsTrue(sameInstant(dt1, dt2));
  Assert.IsTrue(sameInstant(d1.UTC.DateTime, d2.DateTime));
  Assert.IsTrue(not d1.equal(d2));
  Assert.IsTrue(d1.sameTime(d2));

  Assert.IsTrue(TDateTimeEx.fromHL7('20130405120000-1000').sameTime(TDateTimeEx.fromHL7('20130405100000-0800')));

  // Min/Max
  Assert.IsTrue(TDateTimeEx.fromHL7('20130405123456').Min.toHL7 = '20130405123456.000');
  Assert.IsTrue(TDateTimeEx.fromHL7('20130405123456').Max.toHL7 = '20130405123457.000');
  Assert.IsTrue(TDateTimeEx.fromHL7('201304051234').Min.toHL7 = '20130405123400.000');
  Assert.IsTrue(TDateTimeEx.fromHL7('201304051234').Max.toHL7 = '20130405123500.000');

  Assert.IsTrue(TDateTimeEx.fromHL7('201301010000').before(TDateTimeEx.fromHL7('201301010000'), true));
  Assert.IsTrue(not TDateTimeEx.fromHL7('201301010000').before(TDateTimeEx.fromHL7('201301010000'), false));
  Assert.IsTrue(TDateTimeEx.fromHL7('201301010000').before(TDateTimeEx.fromHL7('201301010001'), true));
  Assert.IsTrue(not TDateTimeEx.fromHL7('201301010001').before(TDateTimeEx.fromHL7('201301010000'), true));
  //
//  d1 := UniversalDateTime;
//  d2 := LocalDateTime;
//  d3 := TimeZoneBias;
//  Assert.IsTrue(d1 <> d2);
//  Assert.IsTrue(d1 = d2 - d3);
end;

{ TTestCriticalSectionThread }

procedure TTestCriticalSectionThread.execute;
begin
  EnterCriticalSection(cs);
  try
    globalInt := GetCurrentThreadId;
  finally
    LeaveCriticalSection(cs);
  end;
end;

procedure TOSXTests.TestSemaphore;
var
  thread : TTestSemaphoreThread;
begin
  globalInt := 0;
  sem := TSemaphore.Create(nil, 0, 1, '');
  try
    thread := TTestSemaphoreThread.Create;
    try
      thread.FreeOnTerminate := true;
      while (globalInt = 0) do
        sleep(10);
      Assert.IsTrue(globalInt = 1, '1');
      sem.Release;
      sleep(10);
      Assert.IsTrue(globalInt = 2, '2');
      sem.Release;
      sleep(10);
      Assert.IsTrue(globalInt = 3, '3');
      sleep(900);
      Assert.IsTrue(globalInt = 4, '4');
    finally
      thread.Terminate;
    end;
    sleep(900);
    Assert.IsTrue(globalInt = 100, '100');
  finally
    sem.Free;
  end;
end;

{ TTestSemaphoreThread }

procedure TTestSemaphoreThread.execute;
begin
  inc(globalInt);
  while not Terminated do
  begin
    sem.WaitFor(500);
    inc(globalInt);
  end;
  globalInt := 100;
end;

{ TTestKCriticalSectionThread }

procedure TTestKCriticalSectionThread.Execute;
begin
  kcs.Enter;
  try
    globalInt := GetCurrentThreadId;
  finally
    kcs.Leave;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TOSXTests);
end.
