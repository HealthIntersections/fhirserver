unit user_manager;

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
  SysUtils,
  fsl_base,
  IdContext, IdCustomHTTPServer,
  fhir_objects,
  
  session, fsl_scim,
  utilities;

Const
  SCIM_ANONYMOUS_USER = 'ANONYMOUS';
  SCIM_SYSTEM_USER = 'SYSTEM';

type
  TFHIRUserProvider = class (TFslObject)
  private
    FOnProcessFile : TProcessFileEvent;
  public
    function Link :  TFHIRUserProvider; overload;

    Function loadUser(key : integer) : TSCIMUser; overload; virtual; abstract;
    Function loadUser(id : String; var key : integer) : TSCIMUser; overload; virtual; abstract;
    function CheckLogin(username, password : String; var key : integer) : boolean; virtual; abstract;
    function CheckId(id : String; var username, hash : String) : boolean; virtual; abstract;
    function loadOrCreateUser(id, name, email : String; var key : integer) : TSCIMUser; virtual; abstract;

    Procedure processRequest(context: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFHIRSession; prefix : String); virtual;
    property OnProcessFile : TProcessFileEvent read FOnProcessFile write FOnProcessFile;

    function allowInsecure : boolean; virtual;
  end;

implementation

{ TFHIRUserProvider }

function TFHIRUserProvider.Link: TFHIRUserProvider;
begin
  result := TFHIRUserProvider(inherited Link);
end;

function TFHIRUserProvider.allowInsecure: boolean;
begin
  result := false;
end;

procedure TFHIRUserProvider.processRequest(context: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session: TFHIRSession; prefix : String);
begin
  raise EFHIRException.create('User Management is not supported on this endpoint');
end;

end.

