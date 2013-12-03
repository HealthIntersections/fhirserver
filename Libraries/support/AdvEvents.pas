Unit AdvEvents;

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
  AdvObjects, AdvItems, AdvMethods;


Type
  TAdvEvent = Procedure (Sender : TAdvObject) Of Object;

  TAdvEventList = Class(TAdvMethodList)
    Private
      Function GetEvent(iIndex : Integer): TAdvEvent;
      Procedure SetEvent(iIndex : Integer; Const aValue : TAdvEvent);

    Public
      Function IndexByValue(aValue : TAdvEvent) : Integer;
      Function ExistsByValue(aValue : TAdvEvent) : Boolean;
      Function Add(aValue : TAdvEvent) : Integer;
      Procedure Insert(iIndex : Integer; aValue : TAdvEvent);
      Procedure DeleteByValue(aValue : TAdvEvent);

      Property EventByIndex[iIndex : Integer] : TAdvEvent Read GetEvent Write SetEvent; Default;
  End;


Implementation


Function TAdvEventList.IndexByValue(aValue : TAdvEvent): Integer;
Begin
  Result := Inherited IndexByValue(TAdvMethod(aValue));
End;


Function TAdvEventList.ExistsByValue(aValue: TAdvEvent): Boolean;
Begin
  Result := Inherited ExistsByValue(TAdvMethod(aValue));
End;


Function TAdvEventList.Add(aValue : TAdvEvent): Integer;
Begin
  Result := Inherited Add(TAdvMethod(aValue));
End;


Procedure TAdvEventList.Insert(iIndex : Integer; aValue : TAdvEvent);
Begin
  Inherited Insert(iIndex, TAdvMethod(aValue));
End;


Procedure TAdvEventList.DeleteByValue(aValue : TAdvEvent);
Begin
  Inherited DeleteByValue(TAdvMethod(aValue));
End;


Function TAdvEventList.GetEvent(iIndex : Integer): TAdvEvent;
Begin
  Assert(ValidateIndex('GetEvent', iIndex));

  Result := TAdvEvent(MethodByIndex[iIndex]);
End;


Procedure TAdvEventList.SetEvent(iIndex : Integer; Const aValue : TAdvEvent);
Begin
  Assert(ValidateIndex('SetEvent', iIndex));

  MethodByIndex[iIndex] := TAdvMethod(aValue);
End;


End. // AdvEvents //
