unit FHIR.Npp.Utilities;

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
  fsl_base, System.Generics.Defaults, fhir_objects, fhir_factory, fhir_pathengine;

type
  TFHIRAnnotationLevel = (alError, alWarning, alHint, alMatch);

  TFHIRAnnotation = class (TFslObject)
  private
    FLevel: TFHIRAnnotationLevel;
    FMessage: String;
    FLine : integer;
    FStop: integer;
    FStart: integer;
    FDescription: String;
  public
    constructor Create(level : TFHIRAnnotationLevel; line : integer; start, stop : integer; message, description : String); overload;

    property level : TFHIRAnnotationLevel read FLevel write FLevel;
    property start : integer read FStart write FStart;
    property stop : integer read FStop write FStop;
    property message : String read FMessage write FMessage;
    property description : String read FDescription write FDescription;
    property line : integer read FLine write FLine;
  end;

  TFHIRAnnotationComparer = class (TFslComparer<TFHIRAnnotation>)
  public
    function Compare(const Left, Right: TFHIRAnnotation): Integer;
  end;

implementation

{ TFHIRAnnotation }

constructor TFHIRAnnotation.create(level: TFHIRAnnotationLevel; line, start, stop: integer; message, description: String);
begin
  Create;
  self.level := level;
  self.line := line;
  self.start := start;
  self.stop := stop;
  self.message := message;
  if description <> '' then
    self.description := description
  else
    self.description := message;
end;

{ TFHIRAnnotationComparer }

function TFHIRAnnotationComparer.Compare(const Left, Right: TFHIRAnnotation): Integer;
begin
  if (left.Start < Right.Start) then
    result := -1
  else if (left.Start > Right.Start) then
    result := 1
  else if (left.Stop < Right.Stop) then
    result := -1
  else if (left.Stop > Right.Stop) then
    result := 1
  else if (left.Level < Right.Level) then
    result := -1
  else if (left.Level > Right.Level) then
    result := 1
  else
    result := 0;
end;



end.
