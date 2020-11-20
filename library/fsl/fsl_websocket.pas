unit fsl_websocket;

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

Uses
  SysUtils, Classes,
  IdGlobal, IdException, IdComponent, IdTCPConnection, IdContext, IdCustomHTTPServer, IdHashSHA, IdCoderMIME;

Type
  TIdWebSocketOperation = (wsoNoOp, wsoText, wsoBinary, wsoClose);

  TIdWebSocketCommand = record
    op : TIdWebSocketOperation;
    text : String;
    bytes : TIdBytes;
    status : integer;
  end;

  TIdWebSocket = class (TIdComponent)
  private
    FConnection : TIdTCPConnection;
    FRequest: TIdHTTPRequestInfo;
    procedure pong;
  public
    function open(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo) : boolean;
    function IsOpen : boolean;
    function read(wait : boolean) : TIdWebSocketCommand;
    procedure write(s : String);
    procedure close;

    Property Request : TIdHTTPRequestInfo read FRequest;
  end;

implementation

{ TIdWebSocket }

function TIdWebSocket.IsOpen: boolean;
begin
  result := assigned(FConnection.IOHandler);
end;

function TIdWebSocket.open(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo): boolean;
var
  s : String;
  hash : TIdHashSHA1;
  base64 : TIdEncoderMIME;
begin
  FRequest := request;
  if request.RawHeaders.Values['Upgrade'] <> 'websocket' then
    raise EIdException.create('Only web sockets supported on this end-point');

  s := request.RawHeaders.Values['Sec-WebSocket-Key']+'258EAFA5-E914-47DA-95CA-C5AB0DC85B11';

  base64 := TIdEncoderMIME.Create(nil);
  hash := TIdHashSHA1.Create;
  try
    s := base64.EncodeBytes(hash.HashString(s, IndyTextEncoding_ASCII));
  finally
    hash.Free;
    base64.Free;
  end;

  response.ResponseNo := 101;
  response.ResponseText := 'Switching Protocols';
  response.CustomHeaders.AddValue('Upgrade', 'websocket');
  response.Connection := 'Upgrade';
  response.CustomHeaders.AddValue('Sec-WebSocket-Accept', s);
  if (request.rawHeaders.IndexOfName('Sec-WebSocket-Protocol') > -1) then
    response.CustomHeaders.AddValue('Sec-WebSocket-Protocol', 'chat');
  response.WriteHeader;

  FConnection := AContext.Connection;
  result := true;
end;

procedure TIdWebSocket.pong;
var
  b : byte;
begin
  b := $80 + $10; // close
  FConnection.IOHandler.write(b);
  b := 0;
  FConnection.IOHandler.write(b);
end;

function TIdWebSocket.read(wait : boolean) : TIdWebSocketCommand;
var
  h, l : byte;
  fin : boolean;
  op : byte;
  mk : TIdBytes;
  len : cardinal;
  i : integer;
begin
  FillChar(result, sizeof(TIdWebSocketCommand), 0);
  if not wait and not FConnection.IOHandler.CheckForDataOnSource then
    result.op := wsoNoOp
  else
  begin
    h := FConnection.IOHandler.ReadByte;
    fin := (h and $80) > 1;
    if not fin then
      raise EIdException.create('Multiple frames not done yet');
    op := h and $0F;
    l := FConnection.IOHandler.ReadByte;
    // ? msk := (l and $80) > 0;
    len := l and $7F;
    if len = 126 then
      len := FConnection.IOHandler.ReadUInt16
    else if len = 127 then
      len := FConnection.IOHandler.ReadUInt32;
    FConnection.IOHandler.ReadBytes(mk, 4);
    FConnection.IOHandler.ReadBytes(result.bytes, len);
    for i := 0 to length(result.bytes) - 1 do
      result.bytes[i] := result.bytes[i] xor mk[i mod 4];
    case op of
      1: begin
         result.op := wsoText;
         result.text := IndyTextEncoding_UTF8.GetString(result.bytes);
         end;
      2: result.op := wsoText;
      8: result.op := wsoClose;
      9: begin
         pong();
         result := read(wait);
         end;
    else
      raise EIdException.create('Unknown OpCode '+inttostr(op));
    end;
  end;
end;

procedure TIdWebSocket.write(s: String);
var
  b : byte;
  w : word;
  bs : TIDBytes;
begin
  b := $80 + $01; // text frame, last
  FConnection.IOHandler.write(b);
  if (length(s) <= 125) then
  begin
    b := length(s);
    FConnection.IOHandler.write(b);
  end
  else if (length(s) <= $FFFF) then
  begin
    b := 126;
    FConnection.IOHandler.write(b);
    w := length(s);
    FConnection.IOHandler.write(w);
  end
  else
  begin
    b := 127;
    FConnection.IOHandler.write(b);
    FConnection.IOHandler.write(length(s));
  end;

  bs := IndyTextEncoding_UTF8.GetBytes(s);
  FConnection.IOHandler.write(bs);
end;

procedure TIdWebSocket.close;
var
  b : byte;
begin
  b := $80 + $08; // close
  FConnection.IOHandler.write(b);
  b := 0;
  FConnection.IOHandler.write(b);
  FConnection.IOHandler.Close;
end;

end.
