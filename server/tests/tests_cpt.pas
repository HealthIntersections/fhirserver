unit tests_cpt;

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

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  IdHash, IdHashSHA,
  fsl_testing,
  fsl_base, fsl_stream, fsl_utilities, fsl_http,
  fhir_common, ftx_service,
  fdb_sqlite3,
  tx_cpt;

type

  { TCPTTests }

  TCPTTests = Class (TFslTestCase)
  private
    FCPT : TCPTServices;

  public
    Procedure SetUp; override;
    procedure TearDown; override;
  published
    Procedure TestDB;
    procedure TestCode;
    procedure TestCodeX;
    procedure TestCodeMod;
    procedure TestCodeModX1;
    procedure TestIterator;

    procedure TestModifierFilter;
    procedure TestBaseFilter;
    procedure TestUnModifiedFilter;
    procedure TestModifiedFilter;
    procedure TestKindFilter;

    procedure TestExpression1;  
    procedure TestExpression2;
  end;

procedure registerTests;

implementation

procedure registerTests;
// don't use initialization - give other code time to set up directories etc
begin
  RegisterTest('Terminology.CPT', TCPTTests.Suite);
end;

{ TCPTTests }

procedure TCPTTests.SetUp;
var
  fn : String;
begin
  if GCPTDataFile <> '' then
    fn := GCPTDataFile
  else
    fn := TestSettings.serverTestFile(['testcases', 'cpt', 'cpt-fragment.db']);
  FCPT := TCPTServices.Create(nil, nil, TFDBSQLiteManager.Create('test', fn, true, false, 4));
end;

procedure TCPTTests.TearDown;
begin
  FCPT.free;
end;

procedure TCPTTests.TestDB;
begin
  assertTrue(FCPT.TotalCount > 0);
end;

procedure TCPTTests.TestCode;
var
  ctxt : TCodeSystemProviderContext;
  msg : String;
begin
  ctxt := FCPT.locate('99202', nil, msg);
  try
    assertTrue(ctxt <> nil);
    assertEqual('Office or other outpatient visit for the evaluation and management of a new patient, which '+'requires a medically appropriate history and/or examination and straightforward medical decision making. When using time for code selection, 15-29 minutes of total time is spent on the date of the encounter.', FCPT.Display(ctxt, nil));
  finally
    ctxt.free;
  end;
end;


procedure TCPTTests.TestCodeX;
var
  ctxt : TCodeSystemProviderContext;
  msg : String;
begin
  ctxt := FCPT.locate('99201', nil, msg);
  try
    assertTrue(ctxt = nil);
    assertTrue(msg <> '');
  finally
    ctxt.free;
  end;
end;

procedure TCPTTests.TestCodeMod;
var
  ctxt : TCodeSystemProviderContext;
  msg : String;
begin
  ctxt := FCPT.locate('99202:P1', nil, msg);
  try
    assertTrue(ctxt <> nil);
    assertEqual('', FCPT.Display(ctxt, nil));
  finally
    ctxt.free;
  end;
end;

procedure TCPTTests.TestCodeModX1;
var
  ctxt : TCodeSystemProviderContext;
  msg : String;
begin
  ctxt := FCPT.locate('99202:P1-P1', nil, msg);
  try
    assertTrue(ctxt = nil);
    assertTrue(msg <> '');
  finally
    ctxt.free;
  end;
end;

procedure TCPTTests.TestIterator;   
var
  iter : TCodeSystemIteratorContext;
  c : TCodeSystemProviderContext;
  s : String;
begin
  iter := FCPT.getIterator(nil);
  try
    while iter.more do
    begin
      c := FCPT.getNextContext(iter);
      try
        s := FCPT.code(c);
        AssertTrue(StringArrayExists(['metadata-kinds', 'metadata-designations', '99202', '99203', '0001A', '99252', '25', '95', 'P1', '1P', 'F1'], s), 'Unexpected code '+s);
      finally
        c.free;
      end;
    end;
  finally
    iter.free;
  end;
end;

procedure TCPTTests.TestModifierFilter;
var
  filter : TCodeSystemProviderFilterContext;
  ctxt : TCodeSystemProviderContext;
  c : integer;
  s, msg : String;
begin
  filter := FCPT.filter(true, 'modifier', foEqual, 'true', nil);
  try
    AssertTrue(filter <> nil);
    AssertFalse(FCPT.isNotClosed(nil, filter));
    c := 0;
    while FCPT.FilterMore(filter) do
    begin
      inc(c);
      ctxt := FCPT.FilterConcept(filter);
      try               
        s := FCPT.code(ctxt);
        AssertTrue(StringArrayExists(['25', '95', 'P1', '1P', 'F1'], s), 'Unexpected code '+s);
      finally
        ctxt.free;
      end;
    end;
    AssertEqual(5, c);
    ctxt := FCPT.locate('99202', nil, msg);
    try
      AssertFalse(FCPT.inFilter(filter, ctxt));
    finally
      ctxt.free;
    end;
    ctxt := FCPT.locate('P1', nil, msg);
    try
      AssertTrue(FCPT.inFilter(filter, ctxt));
    finally
      ctxt.free;
    end;
    ctxt := FCPT.locate('99202:P1', nil, msg);
    try
      AssertFalse(FCPT.inFilter(filter, ctxt));
    finally
      ctxt.free;
    end;
  finally
    filter.free;
  end;
end;

procedure TCPTTests.TestBaseFilter;
var
  filter : TCodeSystemProviderFilterContext;
  ctxt : TCodeSystemProviderContext;
  c : integer;
  s, msg : String;
begin
  filter := FCPT.filter(true, 'modifier', foEqual, 'false', nil);
  try
    AssertTrue(filter <> Nil);
    AssertFalse(FCPT.isNotClosed(nil, filter));
    c := 0;
    while FCPT.FilterMore(filter) do
    begin
      inc(c);
      ctxt := FCPT.FilterConcept(filter);
      try
        s := FCPT.code(ctxt);
        AssertTrue(StringArrayExists(['99202', '99203', '0001A', '99252'], s), 'Unexpected code '+s);
      finally
        ctxt.free;
      end;
    end;
    AssertEqual(4, c);
    ctxt := FCPT.locate('99202', nil, msg);
    try
      AssertTrue(FCPT.inFilter(filter, ctxt));
    finally
      ctxt.free;
    end;
    ctxt := FCPT.locate('P1', nil, msg);
    try
      AssertFalse(FCPT.inFilter(filter, ctxt));
    finally
      ctxt.free;
    end;
    ctxt := FCPT.locate('99202:P1', nil, msg);
    try
      AssertFalse(FCPT.inFilter(filter, ctxt));
    finally
      ctxt.free;
    end;
  finally
    filter.free;
  end;
end;

procedure TCPTTests.TestUnModifiedFilter;
var
  filter : TCodeSystemProviderFilterContext;
  ctxt : TCodeSystemProviderContext;
  c : integer;
  s, msg : String;
begin
  filter := FCPT.filter(true, 'modified', foEqual, 'false', nil);
  try
    AssertTrue(filter <> nil);
    AssertFalse(FCPT.isNotClosed(nil, filter));
    c := 0;
    while FCPT.FilterMore(filter) do
    begin
      inc(c);
      ctxt := FCPT.FilterConcept(filter);
      try
        s := FCPT.code(ctxt);
        AssertTrue(StringArrayExists(['99202', '99203', '0001A', '99252', '25', 'P1', '1P', 'F1', '95'], s), 'Unexpected code '+s);
      finally
        ctxt.free;
      end;
    end;
    AssertEqual(9, c);
    ctxt := FCPT.locate('99202', nil, msg);
    try
      AssertTrue(FCPT.inFilter(filter, ctxt));
    finally
      ctxt.free;
    end;
    ctxt := FCPT.locate('P1', nil, msg);
    try
      AssertTrue(FCPT.inFilter(filter, ctxt));
    finally
      ctxt.free;
    end;
    ctxt := FCPT.locate('99202:P1', nil, msg);
    try
      AssertFalse(FCPT.inFilter(filter, ctxt));
    finally
      ctxt.free;
    end;
  finally
    filter.free;
  end;
end;

procedure TCPTTests.TestModifiedFilter;
var
  filter : TCodeSystemProviderFilterContext;
  ctxt : TCodeSystemProviderContext;
  c : integer;
  s, msg : String;
begin
  filter := FCPT.filter(true, 'modified', foEqual, 'true', nil);
  try
    AssertTrue(filter <> nil);
    AssertTrue(FCPT.isNotClosed(nil, filter));
    c := 0;
    while FCPT.FilterMore(filter) do
      inc(c);
    AssertEqual(0, c);
    ctxt := FCPT.locate('99202', nil, msg);
    try
      AssertFalse(FCPT.inFilter(filter, ctxt));
    finally
      ctxt.free;
    end;
    ctxt := FCPT.locate('P1', nil, msg);
    try
      AssertFalse(FCPT.inFilter(filter, ctxt));
    finally
      ctxt.free;
    end;
    ctxt := FCPT.locate('99202:P1', nil, msg);
    try
      AssertTrue(FCPT.inFilter(filter, ctxt));
    finally
      ctxt.free;
    end;
  finally
    filter.free;
  end;
end;

procedure TCPTTests.TestKindFilter;

var
  filter : TCodeSystemProviderFilterContext;
  ctxt : TCodeSystemProviderContext;
  c : integer;
  s, msg : String;
begin
  filter := FCPT.filter(true, 'kind', foEqual, 'code', nil);
  try
    AssertTrue(filter <> nil);
    AssertFalse(FCPT.isNotClosed(nil, filter));
    c := 0;
    while FCPT.FilterMore(filter) do
    begin
      inc(c);
      ctxt := FCPT.FilterConcept(filter);
      try
        s := FCPT.code(ctxt);
        AssertTrue(StringArrayExists(['99202', '99203', '99252'], s), 'Unexpected code '+s);
      finally
        ctxt.free;
      end;
    end;
    AssertEqual(3, c);
    ctxt := FCPT.locate('99202', nil, msg);
    try
      AssertTrue(FCPT.inFilter(filter, ctxt));
    finally
      ctxt.free;
    end;
    ctxt := FCPT.locate('P1', nil, msg);
    try
      AssertFalse(FCPT.inFilter(filter, ctxt));
    finally
      ctxt.free;
    end;
    ctxt := FCPT.locate('99202:P1', nil, msg);
    try
      AssertFalse(FCPT.inFilter(filter, ctxt));
    finally
      ctxt.free;
    end;
  finally
    filter.free;
  end;
end;

procedure TCPTTests.TestExpression1;
var
  ctxt : TCodeSystemProviderContext;
  msg : String;
begin
  ctxt := FCPT.locate('99202:25', nil, msg);
  try
    assertTrue(ctxt <> nil);
    assertTrue(msg = '');
    assertEqual('', FCPT.Display(ctxt, nil));
  finally
    ctxt.free;
  end;
end;

procedure TCPTTests.TestExpression2;
var
  ctxt : TCodeSystemProviderContext;
  msg : String;
begin
  ctxt := FCPT.locate('99252:95', nil, msg);
  try
    assertTrue(ctxt = nil);
    assertEqual('The modifier 95 cannot be used with the code 99252 as it is not designated for telemedicine', msg);
    assertEqual('', FCPT.Display(ctxt, nil));
  finally
    ctxt.free;
  end;
end;

end.


