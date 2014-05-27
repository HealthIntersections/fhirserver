unit FhirServerTests;

interface

uses
  SysUtils,
  IniFiles,
  AdvObjects,
  TerminologyServer;

Type
  TFhirServerTests = class (TAdvObject)
  private
    FIni: TIniFile;
    FTerminologyServer: TTerminologyServer;
    procedure TestSnomedExpressions;
    procedure SetTerminologyServer(const Value: TTerminologyServer);
  public
    destructor Destroy; override;
    property  ini : TIniFile read FIni write FIni;
    property TerminologyServer : TTerminologyServer read FTerminologyServer write SetTerminologyServer;
    procedure executeLibrary; // library functionality to test
    procedure executeBefore; // before server is started
    procedure executeRound1;  // initial state - all loaded, but empty
    procedure executeRound2;  // 2nd cycle: after everything is loaded
    procedure executeAfter;
  end;

implementation

uses
  SnomedServices, SnomedExpressions,
  DecimalTests, UcumTests;

{ TFhirServerTests }

procedure TFhirServerTests.executeBefore;
begin
  TestSnomedExpressions;
end;

procedure TFhirServerTests.executeLibrary;
begin
  TDecimalTests.runTests;
  TUcumTests.runTests(ExtractFilePath(FIni.FileName));
  WriteLn('Library tests Passed');
end;

procedure TFhirServerTests.executeRound1;
begin

end;

procedure TFhirServerTests.executeRound2;
begin

end;

destructor TFhirServerTests.Destroy;
begin
  FTerminologyServer.Free;
  inherited;
end;

procedure TFhirServerTests.SetTerminologyServer(const Value: TTerminologyServer);
begin
  FTerminologyServer.Free;
  FTerminologyServer := Value;
end;

procedure TFhirServerTests.executeAfter;
begin
    // import rf1
    // import rf2
end;

procedure TFhirServerTests.TestSnomedExpressions;
begin
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '297186008 | motorcycle accident |').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '297186008').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '217724009 | accident caused by blizzard | +297186008 | motorcycle accident |').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '217724009 +297186008 | motorcycle accident |').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '217724009'#13#10' + 297186008 | motorcycle accident |'#13#10'').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '217724009 + 297186008 '#13#10'| motorcycle accident |').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '217724009 | accident caused by blizzard |:116680003 | is a | =297186008 | motorcycle accident |').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '297186008 | motorcycle accident |:116680003 | is a | =217724009 | accident caused by blizzard |').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '83152002 | oophorectomy |: 260686004 | method |=257820006| laser excision - action |').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '313056006 | epiphysis of ulna |:272741003 | laterality | =7771000 | left |').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '119189000 | ulna part | + 312845000 | epiphysis of upper limb |:272741003 | laterality | =7771000 | left |').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '83152002 | oophorectomy |:260686004 | method |=257820006| laser excision - action |,363704007 | procedure site | =20837000 | structure of right ovary |').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '120053002 | Salpingectomy |:260686004 | method | =261519002 | diathermy excision - action |,363704007 | procedure site | =113293009 | structure of left fallopian tube |').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '116028008 | salpingo-oophorectomy |').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '71388002 | procedure |:{260686004 | method | =129304002 | excision - action |,405813007 | procedure site - Direct | =15497006 | ovarian '+'structure |}{260686004 | method | =129304002 | excision - action |,405813007 | procedure site - Direct | =31435000 | fallopian tube structure |}').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '116028008 | salpingo-oophorectomy |: {260686004 | method |=257820006| laser excision - action |,363704007 | procedure site | =20837000 | structure of right ovary |}{260686004 | '+'method | =261519002 | diathermy excision - action |,363704007 | procedure site | =113293009 | structure of left fallopian tube |}').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '71620000 | fracture of femur |: 42752001 | due to | = (217724009 | accident caused by blizzard | +297186008 | motorcycle accident |)').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '24136001 | hip joint structure |: 272741003 | laterality | =7771000 | left |').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '397956004 | prosthetic arthroplasty of the hip |:363704007 | procedure site | = (24136001 | hip joint structure | :272741003 | laterality | =7771000 | left |)').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '397956004 | prosthetic arthroplasty of the hip |:363704007 | procedure site | = (24136001 | hip joint structure| :272741003 | laterality | =7771000 | left |) {363699004 |'+' direct device | =304120007 | total hip replacement prosthesis |,260686004 | method | =257867005 | insertion - action |}').Free;
  TSnomedExpressionParser.Parse(TerminologyServer.Snomed, '243796009 | situation with explicit context |: {363589002 | associated procedure | = (397956004 | prosthetic arthroplasty of the hip |:363704007 | procedure site | = (24136001 | '+'hip joint structure | :272741003 | laterality | =7771000 | left |) {363699004 | direct device | =304120007 | total hip replacement prosthesis |, '+'260686004 | method | =257867005 | insertion - action |}), 408730004 | procedure context | =385658003 | done |, 408731000 | temporal context | =410512000 | current or specified |, 408732007 | subject relationship context |=410604004 | subject of record | }').Free;
end;


end.


