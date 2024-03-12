unit mvbase;

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

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  ExtCtrls,
  laz.VirtualTrees,
  fsl_base,
  fhir4_client;

type

  { TViewManager }

  TViewManager = class (TFslObject)
  private
    FClient: TFhirClient4;
    FNavigator: TLazVirtualStringTree;
    FNavPanel: TPanel;
    FPresentation: TPanel;
    procedure SetClient(AValue: TFhirClient4);
    procedure SetNavigator(AValue: TLazVirtualStringTree);
  public
    property client : TFhirClient4 read FClient write SetClient;
    property NavPanel : TPanel read FNavPanel write FNavPanel;
    property navigator :  TLazVirtualStringTree read FNavigator write SetNavigator;
    property presentation : TPanel read FPresentation write FPresentation;

    procedure initialize; virtual;
  end;

implementation

{ TViewManager }

procedure TViewManager.SetClient(AValue: TFhirClient4);
begin
  FClient.free;
  FClient:=AValue;
end;

procedure TViewManager.SetNavigator(AValue: TLazVirtualStringTree);
begin
  FNavigator:=AValue;
end;

procedure TViewManager.initialize;
begin

end;

end.
