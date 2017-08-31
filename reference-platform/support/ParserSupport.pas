unit ParserSupport;

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

Uses
  AdvObjects;

type
  TSourceLocation = record
    line, col : integer;
  end;

  TSourceLocationObject = class (TAdvObject)
  public
    locationStart : TSourceLocation;
    locationEnd : TSourceLocation;
  end;

const
  MAP_ATTR_NAME = 'B88BF977DA9543B8A5915C84A70F03F7';

function minLoc(src1, src2 : TSourceLocation) : TSourceLocation;
function maxLoc(src1, src2 : TSourceLocation) : TSourceLocation;
function nullLoc : TSourceLocation;
function isNullLoc(src : TSourceLocation) : boolean;
function locLessOrEqual(src1, src2 : TSourceLocation) : boolean;
function locGreatorOrEqual(src1, src2 : TSourceLocation) : boolean;

implementation

function minLoc(src1, src2 : TSourceLocation) : TSourceLocation;
begin
  if (src1.line < src2.line) then
    result := src1
  else if (src2.line < src1.line) then
    result := src2
  else if (src1.col < src2.col) then
    result := src1
  else
    result := src2
end;

function maxLoc(src1, src2 : TSourceLocation) : TSourceLocation;
begin
  if (src1.line > src2.line) then
    result := src1
  else if (src2.line > src1.line) then
    result := src2
  else if (src1.col > src2.col) then
    result := src1
  else
    result := src2
end;

function nullLoc : TSourceLocation;
begin
  result.line := -1;
  result.col := -1;
end;


function isNullLoc(src : TSourceLocation) : boolean;
begin
  result := (src.line = -1) and (src.col = -1);
end;

function locLessOrEqual(src1, src2 : TSourceLocation) : boolean;
begin
  if src1.line < src2.line then
    result := true
  else if src1.line > src2.line then
    result := false
  else
    result := src1.col <= src2.col;
end;

function locGreatorOrEqual(src1, src2 : TSourceLocation) : boolean;
begin
  if src1.line > src2.line then
    result := true
  else if src1.line < src2.line then
    result := false
  else
    result := src1.col >= src2.col;
end;


end.
