unit IOUtils;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}


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
  Classes, SysUtils, Types,
  FHIR.Support.Utilities;

Type

  { TDirectory }

  TDirectory = record
    class function GetFiles(const Path: string): TStringDynArray; overload; inline; static;
    class function getDirectories(const Path: string): TStringDynArray; overload; inline; static;
  end;

implementation

{ TDirectory }

class function TDirectory.GetFiles(const Path: string): TStringDynArray;
var
  ts: TStringList;
  SearchRec: TSearchRec;
begin
  ts := TStringList.create;
  try
    if FindFirst(FHIR.Support.Utilities.path([Path, '*']), faAnyFile, SearchRec) = 0 then // DO NOT LOCALIZE
    begin
      repeat
        if SearchRec.Attr and SysUtils.faDirectory = 0 then
          ts.add(FHIR.Support.Utilities.path([Path, SearchRec.Name]));
      until FindNext(SearchRec) <> 0;
    end;

    result := ts.ToStringArray;
  finally
     ts.free;
  end;
end;

class function TDirectory.getDirectories(const Path: string): TStringDynArray;
var
  ts: TStringList;
  SearchRec: TSearchRec;
begin
  ts := TStringList.create;
  try
    if FindFirst(FHIR.Support.Utilities.path([Path, '*']), faAnyFile, SearchRec) = 0 then // DO NOT LOCALIZE
    begin
      repeat
        if SearchRec.Attr and SysUtils.faDirectory <> 0 then
          ts.add(FHIR.Support.Utilities.path([Path, SearchRec.Name]));
      until FindNext(SearchRec) <> 0;
    end;

    result := ts.ToStringArray;
  finally
     ts.free;
  end;
end;

end.

