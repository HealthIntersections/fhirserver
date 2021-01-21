unit fsl_tests_npm;

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

{$I fhir.inc}

interface

uses
  SysUtils, Classes, Generics.Collections,
  fsl_testing,
  fsl_base, fsl_utilities,
  fsl_npm, fsl_npm_cache;

type
  { TNpmPackageTests }
  TNpmPackageTests = class (TFslTestCase)
  private
    FCache : TFHIRPackageManager;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ClearNpmPackages;
    procedure LoadUSCore;
    procedure TestVersionComparison;
  end;

procedure registerTests;

implementation

{ TNpmPackageTests }

procedure TNpmPackageTests.LoadUSCore;
var
  npm : TNpmPackage;
begin
  npm := FCache.loadPackage('hl7.fhir.us.core');
  try
    assertTrue(npm <> nil);
  finally
    npm.Free;
  end;

end;

procedure TNpmPackageTests.SetUp;
begin
  FCache := TFHIRPackageManager.create(true);
end;

procedure TNpmPackageTests.TearDown;
begin
  FCache.free;
end;


procedure TNpmPackageTests.TestVersionComparison;
begin
  assertTrue(isMoreRecentVersion('1.0.0', '0.9.8'));
end;

procedure TNpmPackageTests.ClearNpmPackages;
var
  ts : TStringList;
begin
  FCache.clear;
  ts := TStringList.create;
  try
    FCache.ListPackageIds(ts);
    AssertTrue(ts.count = 0, 'Should be no packages left, but found '+ts.commaText);
  finally
    ts.free;
  end;
end;

procedure registerTests;
// don't use initialization - give other code time to set up directories etc
begin
  RegisterTest('Library.Npm', TNpmPackageTests.Suite);
end;

end.

