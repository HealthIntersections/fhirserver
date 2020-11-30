Unit fsl_mapi;

{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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
  Windows, Mapi,
  fsl_utilities, fsl_base, fsl_collections;


Type
  TMapiRecipientArray = Array Of TMapiRecipDesc;
  TMapiFileDescArray = Array Of TMapiFileDesc;

  TFslMAPI = Class(TFslObject)
    Private
      FHandle : LHANDLE;
      FUsername : AnsiString;
      FPassword : AnsiString;
      FSender : AnsiString;
      FSubject : AnsiString;
      FBody : TFslStringList;
      FTos : TFslStringList;
      FCcs : TFslStringList;
      FBccs : TFslStringList;
      FAttachments : TFslStringList;
      FPreview : Boolean;
      FWindow : HWND;

      Procedure AddRecipients(Var aRecipients: TMapiRecipientArray; oValues: TFslStringList; iClass: Integer);

    Protected
      Function ErrorClass : EFslExceptionClass; Overload; Override;

      Procedure Check(Const sMethod : String; iError : Cardinal);

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure Logon;
      Procedure Logoff;
      Function Send : Boolean;

      Property Handle : LHANDLE Read FHandle Write FHandle;
      Property Window : HWND Read FWindow Write FWindow;
      Property Username : AnsiString Read FUsername Write FUsername;
      Property Password : AnsiString Read FPassword Write FPassword;
      Property Sender : AnsiString Read FSender Write FSender;
      Property Subject : AnsiString Read FSubject Write FSubject;
      Property Tos : TFslStringList Read FTos;
      Property Ccs : TFslStringList Read FCcs;
      Property Bccs : TFslStringList Read FBccs;
      Property Body : TFslStringList Read FBody;
      Property Attachments : TFslStringList Read FAttachments;
      Property Preview : Boolean Read FPreview Write FPreview;
  End;

  EFslMAPI = Class(EFslException);


Function OutlookUpdateApplied : Boolean;


Implementation

uses
  fsl_ole;

Function OutlookUpdateApplied : Boolean;
Var
  oOutlook : OleVariant;
  sVersion : String;
Begin
  oOutlook := OLEClassCreate('Outlook.Application');

  If Not VariantIsNull(oOutlook) Then
  Begin
    sVersion := oOutlook.Version;

    Result := StringToInteger32(Copy(sVersion, Length(sVersion) - 3, 4)) >= 4201;
  End
  Else
  Begin
    Result := False;
  End;
End;


Function MAPIError(iError : Cardinal) : String;
Begin
  Case iError Of
    SUCCESS_SUCCESS                 : Result := 'Operation was successful';
    MAPI_E_USER_ABORT               : Result := 'The user canceled one of the dialog boxes.';
    MAPI_E_FAILURE                  : Result := 'One or more unspecified errors occurred.';
    MAPI_E_LOGIN_FAILURE            : Result := 'There was no default logon, and the user failed to log on successfully when the logon dialog box was displayed.';
    MAPI_E_DISK_FULL                : Result := 'An attachment could not be written to a temporary file because there was not enough space on the disk.';
    MAPI_E_INSUFFICIENT_MEMORY      : Result := 'There was insufficient memory to save the message.';
    MAPI_E_ACCESS_DENIED            : Result := 'Access denied.';
    MAPI_E_TOO_MANY_SESSIONS        : Result := 'There are too many sessions open simultaneously.';
    MAPI_E_TOO_MANY_FILES           : Result := 'There were too many file attachments.';
    MAPI_E_TOO_MANY_RECIPIENTS      : Result := 'There were too many recipients.';
    MAPI_E_ATTACHMENT_NOT_FOUND     : Result := 'An attachment could not be located at the specified path. Either the drive letter was invalid, the path was not found on that drive, or the file was not found in that path.';
    MAPI_E_ATTACHMENT_OPEN_FAILURE  : Result := 'The specified attachment could not be opened.';
    MAPI_E_ATTACHMENT_WRITE_FAILURE : Result := 'An attachment could not be written to a temporary file.';
    MAPI_E_UNKNOWN_RECIPIENT        : Result := 'A recipient did not appear in the address list.';
    MAPI_E_BAD_RECIPTYPE            : Result := 'The recipient type was invalid.';
    MAPI_E_NO_MESSAGES              : Result := 'No messages.';
    MAPI_E_INVALID_MESSAGE          : Result := 'An invalid message identifier was provided.';
    MAPI_E_TEXT_TOO_LARGE           : Result := 'The text in the message was too large.';
    MAPI_E_INVALID_SESSION          : Result := 'An invalid session handle was provided.';
    MAPI_E_TYPE_NOT_SUPPORTED       : Result := 'Type not supported.';
    MAPI_E_AMBIGUOUS_RECIPIENT      : Result := 'The name requested has not been or could not be resolved to a unique address list entry.';
    MAPI_E_MESSAGE_IN_USE           : Result := 'Message in use.';
    MAPI_E_NETWORK_FAILURE          : Result := 'Network failure.';
    MAPI_E_INVALID_EDITFIELDS       : Result := 'The value of the edit fields was outside the range of 0 through 4.';
    MAPI_E_INVALID_RECIPS           : Result := 'One or more recipients were invalid or did not resolve to any address.';
    MAPI_E_NOT_SUPPORTED            : Result := 'The operation was not supported by the underlying messaging system.';
  Else
    Result := StringFormat('Unspecified error [%d]', [iError]);
  End;
End;


Constructor TFslMAPI.Create;
Begin
  Inherited;

  FTos := TFslStringList.Create;
  FCcs := TFslStringList.Create;
  FBccs := TFslStringList.Create;
  FAttachments := TFslStringList.Create;
  FBody := TFslStringList.Create;

  FTos.Symbol := ',';
  FCcs.Symbol := ',';
  FBccs.Symbol := ',';
End;


Destructor TFslMAPI.Destroy;
Begin
  Logoff;

  FTos.Free;
  FCcs.Free;
  FBccs.Free;
  FBody.Free;
  FAttachments.Free;

  Inherited;
End;


Function TFslMAPI.ErrorClass : EFslExceptionClass;
Begin
  Result := EFslMAPI;
End;


Procedure TFslMAPI.Check(Const sMethod: String; iError: Cardinal);
Begin
  If (iError <> SUCCESS_SUCCESS) And (iError <> MAPI_E_USER_ABORT) Then
    RaiseError(sMethod, MAPIError(iError));
End;


Procedure TFslMAPI.Logon;
Begin
  Check('Logon', MAPILogon(0, PAnsiChar(FUsername), PAnsiChar(FPassword), MAPI_NEW_SESSION, 0, @FHandle));
End;


Procedure TFslMAPI.Logoff;
Begin
  If FHandle <> 0 Then
    Check('Logoff', MAPILogoff(FHandle, 0, 0, 0));
End;


Procedure TFslMAPI.AddRecipients(Var aRecipients : TMapiRecipientArray; oValues : TFslStringList; iClass : Integer);
Var
  iLoop, iCount : Integer;
  pRecipient    : ^TMapiRecipDesc;
Begin
  iCount := Length(aRecipients);

  SetLength(aRecipients, iCount + oValues.Count);

  For iLoop := 0 To oValues.Count - 1 Do
  Begin
    oValues[iLoop] := StringTrimWhitespace(oValues[iLoop]);

    pRecipient := @aRecipients[iCount + iLoop];

    FillChar(pRecipient^, SizeOf(pRecipient^), 0);

    pRecipient^.ulRecipClass := iClass;
    pRecipient^.lpszName := PAnsiChar(AnsiString(oValues[iLoop]));
  End;
End;


Function TFslMAPI.Send : Boolean;
Var
  aMessage : TMapiMessage;
  aRecipients: TMapiRecipientArray;
  aAttachments : TMapiFileDescArray;
  aSender : TMapiRecipDesc;
  iLoop : Integer;
  iFlags : Integer;
  iError : Integer;
  sBody : AnsiString;
Begin
  sBody := AnsiString(FBody.AsText);

  FillChar(aMessage, SizeOf(aMessage), 0);
  aMessage.lpszSubject := PAnsiChar(FSubject);
  aMessage.lpszNoteText := PAnsiChar(sBody);

  If FSender <> '' Then
  Begin
    FillChar(aSender, SizeOf(aSender), 0);

    aSender.ulRecipClass := MAPI_ORIG;
    aSender.lpszName := PAnsiChar(FSender);

    aMessage.lpOriginator := @aSender;
  End;

  If FTos.Count > 0 Then
  Begin
    aRecipients := Nil;

    AddRecipients(aRecipients, FTos, MAPI_TO);
    AddRecipients(aRecipients, FCcs, MAPI_CC);
    AddRecipients(aRecipients, FBccs, MAPI_BCC);

    aMessage.nRecipCount := Length(aRecipients);
    aMessage.lpRecips := PMapiRecipDesc(aRecipients);
  End;

  If FAttachments.Count > 0 Then
  Begin
    SetLength(aAttachments, FAttachments.Count);

    For iLoop := 0 To FAttachments.Count - 1 Do
    Begin
      FillChar(aAttachments[iLoop], SizeOf(aAttachments[iLoop]), 0);

      aAttachments[iLoop].nPosition := $FFFFFFFF;
      aAttachments[iLoop].lpszPathName := PAnsiChar(AnsiString(FAttachments[iLoop]));
    End;

    aMessage.nFileCount := Length(aAttachments);
    aMessage.lpFiles := PMapiFileDesc(aAttachments);
  End;

  If FPreview Then
    iFlags := MAPI_DIALOG Or MAPI_LOGON_UI
  Else
    iFlags := MAPI_LOGON_UI;

  iError := MAPISendMail(FHandle, FWindow, aMessage, iFlags, 0);

  Check('Send', iError);

  Result := iError = SUCCESS_SUCCESS;
End;


function TFslMAPI.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FBody.sizeInBytes);
  inc(result, FTos.sizeInBytes);
  inc(result, FCcs.sizeInBytes);
  inc(result, FBccs.sizeInBytes);
  inc(result, FAttachments.sizeInBytes);
end;

End.

