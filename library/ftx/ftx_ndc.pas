unit ftx_ndc;

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
  SysUtils,
  fsl_base,
  fdb_manager;

type
  TNdcImporter = class (TFslObject)
  private
    FSource: String;
    FDatabase: TFDBManager;
    procedure SetDatabase(const Value: TFDBManager);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(source : String);
    destructor Destroy; override;

    property Database : TFDBManager read FDatabase write SetDatabase;
    property source : String read FSource write FSource;
  end;

implementation

{ TNdcImporter }

constructor TNdcImporter.Create(source: String);
begin
  inherited create;
  FSource := source;
end;

destructor TNdcImporter.Destroy;
begin
  FDatabase.Free;
  inherited;
end;

procedure TNdcImporter.SetDatabase(const Value: TFDBManager);
begin
  FDatabase := Value;
end;

function TNdcImporter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FSource.length * sizeof(char)) + 12);
  inc(result, FDatabase.sizeInBytes);
end;

end.
