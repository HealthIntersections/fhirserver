unit ftk_worker_home;

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
  Classes, SysUtils,
  ftk_context, ftk_store, ftk_store_temp,
  ftk_worker_base,
  ftk_frame_home;

type
  { THomePageWorker }

  THomePageWorker = class (TBaseWorker)
  private
    FTempStore: TFHIRToolkitTemporaryStorage;
    procedure SetTempStore(AValue: TFHIRToolkitTemporaryStorage);
  protected
    function makeFrame(owner : TComponent) : TBaseWorkerFrame; override;
  public
    constructor Create(context : TToolkitContext; session : TToolkitEditSession; store : TStorageService; temp : TFHIRToolkitTemporaryStorage);
    destructor Destroy; override;

    property TempStore : TFHIRToolkitTemporaryStorage read FTempStore write SetTempStore;
    function EditorTitle : String; override;
  end;

implementation

{ THomePageWorker }

constructor THomePageWorker.Create(context: TToolkitContext; session: TToolkitEditSession; store: TStorageService; temp : TFHIRToolkitTemporaryStorage);
begin
  inherited Create(context, session, store);
  FTempStore := temp;
end;

destructor THomePageWorker.Destroy;
begin
  FTempStore.free;
  inherited Destroy;
end;

function THomePageWorker.EditorTitle: String;
begin
  Result := 'Welcome Page';
end;

procedure THomePageWorker.SetTempStore(AValue: TFHIRToolkitTemporaryStorage);
begin
  FTempStore.free;
  FTempStore := AValue;
end;

function THomePageWorker.makeFrame(owner: TComponent): TBaseWorkerFrame;
begin
  result := THomePageFrame.Create(owner);
  (result as THomePageFrame).TempStore := FTempStore.link;
end;

end.

