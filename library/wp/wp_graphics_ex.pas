unit wp_graphics_ex;

{
Copyright (c) 2010+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

uses
  Windows,
  SysUtils, Classes, Graphics, Types,
  GraphicEx,
  fsl_base, fsl_utilities, fsl_stream, fsl_collections, fsl_shell,
  wp_graphics;

type
  TFslTIFFGraphic = Class(TFslVCLGraphic)
    Private
      Function GetHandle: TTIFFGraphic;
      Procedure SetHandle(Const Value: TTIFFGraphic);

    Protected
      Function HandleClass : TGraphicClass; Override;
      Function HandleNew : TGraphic; Override;

    Public
      Function Link : TFslTIFFGraphic;
      Function Clone : TFslTIFFGraphic;

      Procedure LoadFromResource(Const sResource : String);

      Property Handle : TTIFFGraphic Read GetHandle Write SetHandle;
  End;

  TTIFFGraphic = GraphicEx.TTIFFGraphic;

  TFslGIFGraphic = Class(TFslVCLGraphic)
    Private
      Function GetHandle: TGIFGraphic;
      Procedure SetHandle(Const Value: TGIFGraphic);

    Protected
      Function HandleClass : TGraphicClass; Override;
      Function HandleNew : TGraphic; Override;

    Public
      Function Link : TFslGIFGraphic;
      Function Clone : TFslGIFGraphic;

      Procedure LoadFromResource(Const sResource : String);

      Property Handle : TGIFGraphic Read GetHandle Write SetHandle;
  End;

  TGIFGraphic = GraphicEx.TGIFGraphic;


implementation


Function TFslTIFFGraphic.Link: TFslTIFFGraphic;
Begin
  Result := TFslTIFFGraphic(Inherited Link);
End;


Function TFslTIFFGraphic.Clone: TFslTIFFGraphic;
Begin
  Result := TFslTIFFGraphic(Inherited Clone);
End;


Function TFslTIFFGraphic.HandleClass : TGraphicClass;
Begin
  Result := TTIFFGraphic;
End;


Function TFslTIFFGraphic.HandleNew: TGraphic;
Begin
  Result := TTIFFGraphic.Create; // Because TGraphicClass.Create is not a virtual method.
End;


Function TFslTIFFGraphic.GetHandle: TTIFFGraphic;
Begin
  Result := TTIFFGraphic(Inherited Handle);
End;


Procedure TFslTIFFGraphic.SetHandle(Const Value: TTIFFGraphic);
Begin
  Inherited Handle := Value;
End;


Procedure TFslTIFFGraphic.LoadFromResource(Const sResource: String);
Var
  oResourceStream : TFslResourceStream;
  oVCLStream : TFslVCLStream;
Begin
  oResourceStream := TFslResourceStream.Create;
  oVCLStream := TFslVCLStream.Create;
  Try
    oResourceStream.ResourceTypeApplicationDefined;
    oResourceStream.Filename := ProcessName;
    oResourceStream.ResourceName := sResource;
    oResourceStream.Open;
    Try
      oVCLStream.Stream.ReadBuffer(oResourceStream.Buffer.Data^, oResourceStream.Buffer.Capacity);

      Handle.LoadFromStream(oVCLStream.Stream);
    Finally
      oResourceStream.Close;
    End;
  Finally
    oVCLStream.Free;
    oResourceStream.Free;
  End;
End;


Function TFslGIFGraphic.Link: TFslGIFGraphic;
Begin
  Result := TFslGIFGraphic(Inherited Link);
End;


Function TFslGIFGraphic.Clone: TFslGIFGraphic;
Begin
  Result := TFslGIFGraphic(Inherited Clone);
End;


Function TFslGIFGraphic.HandleClass : TGraphicClass;
Begin
  Result := TGIFGraphic;
End;


Function TFslGIFGraphic.HandleNew: TGraphic;
Begin
  Result := TGIFGraphic.Create; // Because TGraphicClass.Create is not a virtual method.
End;


Function TFslGIFGraphic.GetHandle: TGIFGraphic;
Begin
  Result := TGIFGraphic(Inherited Handle);
End;


Procedure TFslGIFGraphic.SetHandle(Const Value: TGIFGraphic);
Begin
  Inherited Handle := Value;
End;


Procedure TFslGIFGraphic.LoadFromResource(Const sResource: String);
Var
  oResourceStream : TFslResourceStream;
  oVCLStream : TFslVCLStream;
Begin
  oResourceStream := TFslResourceStream.Create;
  oVCLStream := TFslVCLStream.Create;
  Try
    oResourceStream.ResourceTypeApplicationDefined;
    oResourceStream.Filename := ProcessName;
    oResourceStream.ResourceName := sResource;
    oResourceStream.Open;
    Try
      oVCLStream.Stream.ReadBuffer(oResourceStream.Buffer.Data^, oResourceStream.Buffer.Capacity);

      Handle.LoadFromStream(oVCLStream.Stream);
    Finally
      oResourceStream.Close;
    End;
  Finally
    oVCLStream.Free;
    oResourceStream.Free;
  End;
End;




end.
