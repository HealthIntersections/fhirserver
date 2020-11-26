unit v2_server;

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
  fsl_base,
  v2_protocol,
  endpoint, web_server;

type
  Tv2ServerInstance = class (TFslObject)
  private
    FScript: String;
    FPort: word;
    FEndPoint: TFhirWebServerEndpoint;
    procedure SetEndPoint(const Value: TFhirWebServerEndpoint);
  public
    property port : word read FPort write FPort;
    property endPoint : TFhirWebServerEndpoint read FEndPoint write SetEndPoint;
    property script : String read FScript write FScript;

    procedure start;
    procedure stop;
  end;

implementation

{ Tv2ServerInstance }

procedure Tv2ServerInstance.SetEndPoint(const Value: TFhirWebServerEndpoint);
begin
  FEndPoint := Value;
end;

procedure Tv2ServerInstance.start;
begin

end;

procedure Tv2ServerInstance.stop;
begin

end;

end.
