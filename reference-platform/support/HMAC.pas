unit HMAC;

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
  System.SysUtils, {$IFNDEF VER260} System.NetEncoding, {$ENDIF}
  EncdDecd,
  IdGlobal, IdHMAC, IdSSLOpenSSL, IdHash,
  BytesSupport;

type
  TIdHMACClass = class of TIdHMAC;
  THMACUtils = class
  public
    class function HMAC(alg : TIdHMACClass; aKey, aMessage: TBytes): TBytes;
    class function HMAC_HexStr(alg : TIdHMACClass; aKey, aMessage: TBytes): TBytes;
    class function HMAC_Base64(alg : TIdHMACClass; aKey, aMessage: TBytes): AnsiString;
  end;

function idb(b : TBytes) : TIdBytes;  overload;
function idb(b : TIdBytes) : TBytes;  overload;

implementation

function idb(b : TBytes) : TIdBytes;
begin
  SetLength(result, length(b));
  if (length(b) > 0) then
    move(b[0], result[0], length(b));
end;

function idb(b : TIdBytes) : TBytes;
begin
  SetLength(result, length(b));
  if (length(b) > 0) then
    move(b[0], result[0], length(b));
end;

class function THMACUtils.HMAC(alg : TIdHMACClass; aKey, aMessage: TBytes): TBytes;
var
  _alg : TIdHMAC;
begin
  if not IdSSLOpenSSL.LoadOpenSSLLibrary then Exit;
  _alg := alg.Create;
  try
    _alg.Key := idb(aKey);
    Result:= idb(_alg.HashValue(idb(aMessage)));
  finally
    _alg.Free;
  end;
end;

class function THMACUtils.HMAC_HexStr(alg : TIdHMACClass; aKey, aMessage: TBytes): TBytes;
var
  I: Byte;
begin
  Result:= AnsiStringAsBytes('0x');
  for I in HMAC(alg, aKey, aMessage) do
    Result:= BytesAdd(Result, AnsiStringAsBytes(AnsiString(IntToHex(I, 2))));
end;

class function THMACUtils.HMAC_Base64(alg : TIdHMACClass; aKey, aMessage: TBytes): AnsiString;
var
  _HMAC: TBytes;
begin
  _HMAC:= HMAC(alg, aKey, aMessage);
  Result:= EncodeBase64(_HMAC, Length(_HMAC));
end;

end.
