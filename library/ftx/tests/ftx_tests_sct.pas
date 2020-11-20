unit ftx_tests_sct;

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

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  SysUtils, Classes,
  fsl_testing,
  fsl_base,
  ftx_sct_services, ftx_sct_expressions;

type
  TSnomedTests = Class (TFslTestCase)
  private
    FServices : TSnomedServices;

    Fc : String; // (for pm)
    procedure p(c : String); // parse
    procedure pm; // parse
    procedure e(e1, e2 : String; yes : boolean); // test equivalence
    procedure n(s, d : String); // normalise from s to d
    procedure s(p, c : String; yes : boolean); // test subsumption
  public
    Procedure SetUp; override;
    procedure TearDown; override;
  published
    Procedure Base;
    Procedure Parse;
    Procedure ParseErrors;
    Procedure Equivalence;
    Procedure Normalise;
    Procedure Subsumes;
  end;


procedure registerTests;


implementation

{ TSnomedTests }

procedure TSnomedTests.Setup;
begin
  FServices := TSnomedServices.Create;
  FServices.Load(TestSettings.serverTestFile(['testcases', 'snomed', 'test.cache']), true);
end;

procedure TSnomedTests.TearDown;
begin
  FServices.Free;
end;

procedure TSnomedTests.Base;
begin
  assertTrue(FServices.ConceptExists('116680003'));
end;

procedure TSnomedTests.Parse;
begin
  p('116680003');
  p('128045006:{363698007=56459004}');
  p('128045006|cellulitis (disorder)|:{363698007|finding site|=56459004|foot structure|}');
  p('31978002: 272741003=7771000');
  p('31978002|fracture of tibia|: 272741003|laterality|=7771000|left|');
  p('64572001|disease|:{116676008|associated morphology|=72704001|fracture|,363698007|finding site|=(12611008|bone structure of  tibia|:272741003|laterality|=7771000|left|)}');
  p('417662000|past history of clinical finding|:246090004|associated finding|=      (31978002|fracture of tibia|: 272741003|laterality|=7771000|left|)');
  p('243796009|situation with explicit context|:246090004|associated finding|=    (64572001|disease|:{116676008|associated morphology|=72704001|fracture|,'+'    363698007|finding site|=(12611008|bone structure of tibia|:    272741003|laterality|=7771000|left|)}),408729009|finding context|=    '+'410515003|known present|,408731000|temporal context|=410513005|past|,    408732007|subject relationship context|=410604004|subject of record|');

  // from IHTSDO expression documentation:
  p('125605004 |fracture of bone|');
  p('284003005 |bone injury| :{ 363698007 |finding site| = 272673000 |bone structure|,116676008 |associated morphology| = 72704001 |fracture| }');
  p('421720008 |spray dose form| + 7946007 |drug suspension|');
  p('182201002 |hip joint| : 272741003 |laterality| = 24028007 |right|');
  p('397956004 |prosthetic arthroplasty of the hip| : 363704007 |procedure site| = (182201002 |hip joint| : 272741003 |laterality| = 24028007 |right|)');
  p('71388002 |procedure| : {260686004 |method| = 129304002 |excision - action|, 405813007 |procedure site - direct| = 28231008 |gallbladder structure|}, {260686004 |method| = 281615006 '+'|exploration|, 405813007 |procedure site - direct| = 28273000 |bile duct structure|}');
  p('27658006 |amoxicillin|:411116001 |has dose form| = 385049006 |capsule|,{ 127489000 |has active ingredient| = 372687004 |amoxicillin|,111115 |has basis of strength| = (111115 |'+'amoxicillin only|:111115 |strength magnitude| = #500,111115 |strength unit| = 258684004 |mg|)}');
  p('91143003 |albuterol|:411116001 |has dose form| = 385023001 |oral solution|,{ 127489000 |has active ingredient| = 372897005 |albuterol|,111115 |has basis of strength| = (111115 |a'+'lbuterol only|:111115 |strength magnitude| = #0.083,111115 |strength unit| = 118582008 |%|)}');
  p('322236009 |paracetamol 500mg tablet| : 111115 |trade name| = "PANADOL"');
  p('=== 46866001 |fracture of lower limb| + 428881005 |injury of tibia| :116676008 |associated morphology| = 72704001 |fracture|,363698007 |finding site| = 12611008 |bone structure of tibia|');
  p('<<< 73211009 |diabetes mellitus| : 363698007 |finding site| = 113331007 |endocrine system|');

  // from Overview of Expression Normalization, Equivalence and Subsumption Testing

  p('28012007 |Closed fracture of shaft of tibia|');
  p('125605004 |Fracture of bone| :{ 363698007 |Finding site| = 52687003 |Bone structure of shaft of tibia|,116676008 |Associated morphology| = 20946005 |Fracture, closed | }');
  p('423125000 |Closed fracture of bone|:363698007 |Finding site| = 52687003 |Bone structure of shaft of tibia|');
  p('6990005 |Fracture of shaft of tibia |: 116676008 |Associated morphology| = 20946005 |Fracture, closed |');
  p('64572001 |Disease| : { 363698007 |Finding site| = 52687003 |Bone structure of shaft of tibia|, 116676008 |Associated morphology| = 20946005 |Fracture, closed | }');
 // p('10925361000119108 |Closed fracture of shaft of left tibia|'); //  US Extension
  p('28012007 |Closed fracture of shaft of tibia| : 363698007 |Finding site| = (52687003 |Bone structure of shaft of tibia| : 272741003 |Laterality|= 7771000 |Left|)');
  p('28012007 |Closed fracture of shaft of tibia| : 272741003 |Laterality|= 7771000 |Left|'); //Close to user form omits restatement of finding site');
  p('64572001 |Disease| : {363698007 |Finding site| = (52687003 |Bone structure of shaft of tibia| : 272741003 |Laterality|= 7771000 |Left|), 116676008 |Associated morphology| = 20946005 |Fracture, closed | }');
  p('28012007 |Closed fracture of shaft of tibia| : 363698007 |Finding site| = 31156008 |Structure of left half of body|');

  assertTrue(true);
end;

procedure TSnomedTests.pm;
begin
  FServices.parseExpression(Fc).free;
end;

procedure TSnomedTests.ParseErrors;
begin
  Fc := '1166800031';
  assertWillRaise(pm, ETerminologyError, '');
  Fc := '1280450061:{363698007=56459004}';
  assertWillRaise(pm, ETerminologyError, '');
  Fc := '128045006:{3636980071=56459004}';
  assertWillRaise(pm, ETerminologyError, '');
  Fc := '128045006:{363698007=564590041}';
  assertWillRaise(pm, ETerminologyError, '');
  Fc := '128045006:{3636980071=56459004}';
  assertWillRaise(pm, ETerminologyError, '');
  Fc := '128045006:3636980071=56459004}';
  assertWillRaise(pm, ETerminologyError, '');
  Fc := '128045006:{3636980071,56459004}';
  assertWillRaise(pm, ETerminologyError, '');
  Fc := '128045006:{3636980071=56459004';
  assertWillRaise(pm, ETerminologyError, '');
  Fc := '128045006:{363698007=56459004},';
  assertWillRaise(pm, ETerminologyError, '');
  Fc := '128045006|cellulitis (disorder)|:{363698007|finding site|=56459004|hand structure|}';
  assertWillRaise(pm, ETerminologyError, '');
end;

procedure TSnomedTests.Subsumes;
begin
  // basic subsumption tests
  s('31978002', '31978002', true); // an expression subsumes itself
  s('31978002', '31978002: 272741003=7771000', true); // additional refinements in the child are ok
  s('31978002: 272741003=7771000', '31978002: 272741003=7771000', true); // an expression subsumes itself
  s('31978002: 272741003=7771000', '31978002', false); // refinements in the parent are not ok

  // from the normalization tests:
  s('28012007 |Closed fracture of shaft of tibia|', '64572001 |Disease|', false);
  s('64572001 |Disease|', '28012007 |Closed fracture of shaft of tibia|', true);
  s('64572001 |Disease|', '64572001 |Disease| : {363698007 |Finding site| = 52687003 |Bone structure of shaft of tibia|, 116676008 |Associated morphology| = 20946005 |Fracture, closed|}', true);
  s('28012007 |Closed fracture of shaft of tibia|', '64572001 |Disease| : {363698007 |Finding site| = 52687003 |Bone structure of shaft of tibia|, 116676008 |Associated morphology| = 20946005 |Fracture, closed|}', true);
  s('64572001 |Disease| : {363698007 |Finding site| = 52687003 |Bone structure of shaft of tibia|, 116676008 |Associated morphology| = 20946005 |Fracture, closed|}', '28012007 |Closed fracture of shaft of tibia|', true);
  assertTrue(true);
end;

procedure TSnomedTests.Equivalence;
begin
  e('128045006|cellulitis (disorder)|:{363698007|finding site|=56459004|foot structure|}',
    '128045006:{363698007=56459004}', true);

  e('128045006:363698007=56459004',
    '128045006:{363698007=56459004}', true);

  e('128045006:363698007=56459004', '71388002:{363698007=56459004}', false);
  e('128045006+71388002:363698007=56459004', '71388002:{363698007=56459004}', false);

  e('71388002:{260686004=129304002,405813007=28231008},{260686004=281615006,405813007=28273000}',
    '71388002:{405813007=28231008,260686004=129304002},{260686004=281615006,405813007=28273000}', true);

  e('71388002:{260686004=129304002,405813007=28231008},{260686004=281615006,405813007=28273000}',
    '71388002:{260686004=281615006,405813007=28273000},{260686004=129304002,405813007=28231008}', true);

  assertTrue(true);
end;

procedure TSnomedTests.Normalise;
begin
  // normal form for a primitive concept
  n('64572001 |Disease|', '64572001 |Disease|');
  // normal form for a defined concept
  n('28012007 |Closed fracture of shaft of tibia|', '64572001 |Disease| : {363698007 |Finding site| = 52687003 |Bone structure of shaft of tibia|, 116676008 |Associated morphology| = 20946005 |Fracture, closed|}');
  // normal form for a normal form
  n('64572001 |Disease| : {363698007 |Finding site| = 52687003 |Bone structure of shaft of tibia|, 116676008 |Associated morphology| = 20946005 |Fracture, closed|}', '64572001 |Disease| : {363698007 |Finding site| = 52687003 |Bone structure of shaft of tibia|, 116676008 |Associated morphology| = 20946005 |Fracture, closed|}');

  n('6990005 |Fracture of shaft of tibia | : {363698007 |Finding site| = 52687003 |Bone structure of shaft of tibia|, 116676008 |Associated morphology| = 20946005 |Fracture, closed|}', '64572001 |Disease| : {363698007 |Finding site| = 52687003 |Bone structure of shaft of tibia|, 116676008 |Associated morphology| = 20946005 |Fracture, closed|}');
  n('6990005 |Fracture of shaft of tibia| + 447139008 |Closed fracture of tibia | : {363698007 |Finding site| = 52687003 |Bone structure of shaft of tibia|,  116676008 |Associated morphology| = 20946005 |Fracture, closed|}', '64572001 |Disease| : {363698007 |Finding site| = 52687003 |Bone structure of shaft of tibia|, 116676008 |Associated morphology| = 20946005 |Fracture, closed|}');
  n('447139008 |Closed fracture of tibia | : 363698007 |Finding site| = 52687003 |Bone structure of shaft of tibia|', '64572001 |Disease| : {363698007 |Finding site| = 52687003 |Bone structure of shaft of tibia|, 116676008 |Associated morphology| = 20946005 |Fracture, closed|}');
  assertTrue(true);
end;

procedure TSnomedTests.p(c: String);
begin
  FServices.parseExpression(c).free;
end;

procedure TSnomedTests.s(p, c: String; yes: boolean);
var
  pe, ce : TSnomedExpression;
  result : boolean;
begin
  pe := FServices.ParseExpression( p);
  try
    ce := FServices.ParseExpression( c);
    try
      result := FServices.expressionSubsumes(pe, ce);
      assertTrue(result = yes, 'Subsumes failed: '+FServices.RenderExpression(pe, sroMinimal)+' :: '+FServices.RenderExpression(ce, sroMinimal));
    finally
      ce.free;
    end;
  finally
    pe.Free;
  end;
end;

procedure TSnomedTests.e(e1, e2 : String; yes: boolean);
var
  ex1, ex2 : TSnomedExpression;
  msg : String;
  result : boolean;
begin
  ex1 := FServices.ParseExpression( e1);
  try
    ex2 := FServices.ParseExpression( e2);
    try
      result := FServices.expressionsEquivalent(ex1, ex2, msg);
      assertTrue(result = yes, 'Equivalent failed: '+FServices.RenderExpression(ex1, sroMinimal)+' :: '+FServices.RenderExpression(ex2, sroMinimal)+' msg = '+msg);
    finally
      ex2.free;
    end;
  finally
    ex1.Free;
  end;
end;

procedure TSnomedTests.n(s, d: String);
var
  ex1, ex2, ex3 : TSnomedExpression;
  msg : String;
  result : boolean;
begin
  ex1 := FServices.ParseExpression( s);
  try
    ex2 := FServices.ParseExpression( d);
    try
      ex3 := FServices.normaliseExpression(ex1);
      try
//        messagebox(0, pchar('Expression: '+FServices.RenderExpression(ex1, sroFillMissing)+#13#10
//             +'Normal: '+FServices.RenderExpression(ex3, sroFillMissing)+#13#10
//             +'Expected: '+FServices.RenderExpression(ex2, sroFillMissing)+#13#10), 'normalise', 0);
        result := FServices.expressionsEquivalent(ex2, ex3, msg);
        assertTrue(result, 'Normalisation failed: normal form for "'+FServices.RenderExpression(ex1, sroMinimal)+'" should be "'+FServices.RenderExpression(ex2, sroMinimal)+'" but was "'+FServices.RenderExpression(ex3, sroMinimal)+'"');
      finally
        ex3.free;
      end;
    finally
      ex2.free;
    end;
  finally
    ex1.Free;
  end;
end;


procedure registerTests;
// don't use initialization - give other code time to set up directories etc
begin
  RegisterTest('Terminology.Snomed Tests', TSnomedTests.Suite);
end;

end.
