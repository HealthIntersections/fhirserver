unit ftk_frame_resource;

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
  Forms,
  fhir_objects, fhir_client,
  fui_lcl_managers,
  ftk_context;

type
   TSelectSourceRangeEvent = procedure (sender : TObject; start, stop : TPoint) of object;

   { TResourceDesignerFrame }

   TResourceDesignerFrame = class (TFrame)
   private
     FClient: TFHIRClientV;
     FContext: TToolkitContext;
     FResource: TFHIRResourceV;
     FSync: TFHIRSynEditSynchroniser;
     FOnSelectSourceRange: TSelectSourceRangeEvent;
     procedure SetClient(AValue: TFHIRClientV);
     procedure SetContext(AValue: TToolkitContext);
     procedure SetResource(AValue: TFHIRResourceV);
     procedure SetSync(AValue: TFHIRSynEditSynchroniser);
   public
     destructor Destroy; override;

     property context : TToolkitContext read FContext write SetContext;
     property sync : TFHIRSynEditSynchroniser read FSync write SetSync;
     property resource : TFHIRResourceV read FResource write SetResource;
     property client : TFHIRClientV read FClient write SetClient; // access to client, if this came from a client (for derived information)

     procedure initialize; virtual; // called once, at start up, to bind all resources etc
     procedure bind; virtual; // called any time that the resource changes
     procedure saveStatus; virtual; // called before shut down because shut down order isn't always predictable

     property OnSelectSourceRange : TSelectSourceRangeEvent read FOnSelectSourceRange write FOnSelectSourceRange;
   end;

implementation

{ TResourceDesignerFrame }

destructor TResourceDesignerFrame.Destroy;
begin
  FResource.free;
  FSync.free;
  FClient.free;
  inherited Destroy;
end;

procedure TResourceDesignerFrame.SetResource(AValue: TFHIRResourceV);
begin
  FResource.free;
  FResource := AValue;
  bind;
end;

procedure TResourceDesignerFrame.SetContext(AValue: TToolkitContext);
begin
  FContext.free;
  FContext := AValue;
end;

procedure TResourceDesignerFrame.SetClient(AValue: TFHIRClientV);
begin
  FClient.free;
  FClient := AValue;
end;

procedure TResourceDesignerFrame.SetSync(AValue: TFHIRSynEditSynchroniser);
begin
  FSync.free;
  FSync := AValue;
end;

procedure TResourceDesignerFrame.initialize;
begin
  // nothing here
end;

procedure TResourceDesignerFrame.bind;
begin
  // nothing here
end;

procedure TResourceDesignerFrame.saveStatus;
begin
  FContext.free;
  FContext := nil;
end;


end.
