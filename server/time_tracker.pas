unit time_tracker;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_logging;

type
  TTimeTracker = class (TFslObject)
  private
    FStart : int64;
    FLast : Int64;
//    FPoints : TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure step(name : String);
    function total : integer;
    function log : String;
  end;

implementation

{ TTimeTracker }

constructor TTimeTracker.Create;
begin
  inherited;
  FStart := GetTickCount64;
  FLast := FStart;
//  FPoints := TStringList.Create;
end;

destructor TTimeTracker.Destroy;
begin
//  FPoints.Free;
  inherited;
end;

function TTimeTracker.log : String;
var
  s : String;
begin
  result := '';
//  for s in FPoints do
//    CommaAdd(result, s);
//  Logging.log('~~~ '+v);
end;

procedure TTimeTracker.step(name: String);
var
  t : int64;
begin
  t := GetTickCount64;
//  FPoints.Add(name+': '+StringPadLeft(inttostr(t - FLast), ' ', 5));
  FLast := t;
end;

function TTimeTracker.total: integer;
begin
  result := GetTickCount64 - FStart;
end;

end.
