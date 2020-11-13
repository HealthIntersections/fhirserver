unit FHIR.Version.Parser;

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


interface

// FHIR v3.1.0 generated 2017-07-09T00:15:31+10:00

uses
  SysUtils, Classes,
  fsl_base, fsl_stream, fsl_xml,
  fhir_objects, fhir_parser, 

  {$IFDEF FHIR2}
  fhir2_types, fhir2_resources, fhir2_xml, fhir2_json, fhir2_context, fhir2_parser;
  {$ENDIF}
  {$IFDEF FHIR3}
  fhir3_types, fhir3_resources, fhir3_xml, fhir3_json, fhir3_turtle, fhir3_context, fhir3_parser;
  {$ENDIF}
  {$IFDEF FHIR4}
  fhir4_types, fhir4_resources, fhir4_xml, fhir4_json, fhir4_turtle, fhir4_context, fhir4_parser;
  {$ENDIF}

Type
  TFHIRParser = fhir_parser.TFHIRParser;
  TFHIRComposer = fhir_parser.TFHIRComposer;

  {$IFDEF FHIR2}
  TFHIRXmlParser = fhir2_xml.TFHIRXmlParser;
  TFHIRXmlComposer = fhir2_xml.TFHIRXmlComposer;
  TFHIRJsonParser = fhir2_json.TFHIRJsonParser;
  TFHIRJsonComposer = fhir2_json.TFHIRJsonComposer;
  TFHIRParsers = TFHIRParsers2;
  {$ENDIF}
  {$IFDEF FHIR3}
  TFHIRXmlParser = fhir3_xml.TFHIRXmlParser;
  TFHIRXmlComposer = fhir3_xml.TFHIRXmlComposer;
  TFHIRJsonParser = fhir3_json.TFHIRJsonParser;
  TFHIRJsonComposer = fhir3_json.TFHIRJsonComposer;
  TFHIRTurtleComposer = fhir3_turtle.TFHIRTurtleComposer;
  TFHIRTurtleParser = fhir3_turtle.TFHIRTurtleParser;
  TFHIRParsers = TFHIRParsers3;
  {$ENDIF}
  {$IFDEF FHIR4}
  TFHIRXmlParser = fhir4_xml.TFHIRXmlParser;
  TFHIRXmlComposer = fhir4_xml.TFHIRXmlComposer;
  TFHIRJsonParser = fhir4_json.TFHIRJsonParser;
  TFHIRJsonComposer = fhir4_json.TFHIRJsonComposer;
  TFHIRTurtleComposer = fhir4_turtle.TFHIRTurtleComposer;
  TFHIRTurtleParser = fhir4_turtle.TFHIRTurtleParser;
  TFHIRParsers = TFHIRParsers4;
  {$ENDIF}


implementation

end.

