Unit AdvFiles;

{
Copyright (c) 2001-2013, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

Interface


Uses
  {$IFDEF MACOS} OSXUtils, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  FileSupport, StringSupport, MathSupport, ErrorSupport,
  AdvStreams, AdvObjects, AdvExceptions;

Type

  TAdvFile = Class(TAdvAccessStream)
  Private
    FStream : TFileStream;
    function GetHandle: THandle;
  Protected
    Function GetPosition : Int64; Override;
    Procedure SetPosition(Const Value : Int64); Override;

    Function GetSize : Int64; Override;
    Procedure SetSize(Const iValue : Int64); Override;

    Procedure RaiseError(aException : EAdvExceptionClass; Const sMethod, sMessage : String); Overload; Override;

    Function ErrorClass : EAdvExceptionClass; Override;

  Public
    constructor Create(const AFileName: string; Mode: Word); overload;
    Destructor Destroy; override;

    function Link : TAdvFile; overload;

    Procedure Read(Var aBuffer; iCount : Cardinal); Override;
    Procedure Write(Const aBuffer; iCount : Cardinal); Override;
    Function Readable : Int64; Override;
    Function Writeable : Int64; Override;

    property Handle: THandle read GetHandle;
  End;

  EAdvFile = Class(EAdvStream);


Implementation

{ TAdvFile }

constructor TAdvFile.Create(const AFileName: string; Mode: Word);
begin
  inherited create;
  FStream := TFileStream.Create(AFileName, mode);
end;

destructor TAdvFile.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TAdvFile.ErrorClass: EAdvExceptionClass;
begin
  Result := EAdvFile;
end;

function TAdvFile.GetHandle: THandle;
begin
  result := FStream.Handle;
end;

function TAdvFile.GetPosition: Int64;
begin
  result := FStream.Position;
end;

function TAdvFile.GetSize: Int64;
begin
  result := FStream.Size;
end;

function TAdvFile.Link: TAdvFile;
begin
  result := TAdvFile(Inherited Link);
end;

procedure TAdvFile.RaiseError(aException: EAdvExceptionClass; const sMethod, sMessage: String);
begin
  Inherited RaiseError(aException, sMethod, StringFormat('%s: ''%s''', [sMessage, FStream.FileName]));
end;

procedure TAdvFile.Read(var aBuffer; iCount: Cardinal);
begin
  if FStream.Read(aBuffer, iCount) < iCount then
    RaiseError('Read', 'Unable to read past end of file');
end;

function TAdvFile.Readable: Int64;
begin
  result := FStream.Size - FStream.Position;
end;

procedure TAdvFile.SetPosition(const Value: Int64);
begin
  FStream.Position := value;
end;

procedure TAdvFile.SetSize(const iValue: Int64);
begin
  FStream.Size := iValue;
end;

procedure TAdvFile.Write(const aBuffer; iCount: Cardinal);
begin
  If (FStream.Write(aBuffer, iCount) < iCount) Then
    RaiseError('Read', 'Unable to write the entire buffer');
end;

function TAdvFile.Writeable: Int64;
begin
  result := 0; // ?
end;

end.
