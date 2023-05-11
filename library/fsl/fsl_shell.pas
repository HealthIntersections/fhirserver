Unit fsl_shell;

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

{$I fhir.inc}

Interface

{$IFDEF WINDOWS}

Uses
  SysUtils, Messages, Windows, ShellAPI, ShlObj, MultiMon,
  fsl_utilities;


Type
  THandle = Windows.THandle;


// Shell execution, with handles kept.
Function ExecuteLaunch(Const sVerb, sFilename : String; Const sParameters : String = ''; bShow : Boolean = True; bErrors : Boolean = True) : THandle; Overload;
Procedure ExecuteTerminate(aHandle : THandle); Overload;
Function ExecuteRunning(aHandle : THandle) : Boolean ;Overload;
Function ExecuteWait(aProcess : THandle; Const iTimeout : Cardinal = INFINITE) : Boolean; Overload;
Function ExecutionReturnCode(aProcess : THandle; Var iReturnCode : Cardinal) : Boolean; Overload;

// Shell excecution.
Function ExecuteError(iError : Integer) : String; Overload;
Function ExecuteParameters(Const aParameters : Array Of String) : String; Overload;
Function ExecuteFolder(Const sFolder : String) : Boolean; Overload;
Function ExecuteVerb(Const sVerb, sFilename : String; Const sParameters : String = ''; bErrors : Boolean = True) : Boolean; Overload;
Function ExecuteOpen(Const sFilename : String; Const sParameters : String = '') : Boolean; Overload;
Function ExecutePrint(Const sFilename : String) : Boolean; Overload;
Function ExecuteWait(Const sFilename : String; Const sParameters : String = '') : Boolean; Overload;
Function ExecuteURL(Const sURL : String) : Boolean; Overload;
Function ExecuteMail(Const sTo : String; Const sSubject : String = ''; Const sBody : String = '') : Boolean; Overload;
Function ExecuteAsAdmin(hWnd: HWND; sFile, sParameters: String) : Boolean;



// Current process ID.
Function ProcessID : Cardinal; Overload;
Function ProcessHandle : THandle;
Function ProcessCommandLineParameters: String; Overload;
Function ProcessCommandLineParameters(Const iProcessID : Cardinal): String; Overload;

// Process Security
Type
  TProcessTokenElevationType = (
                                ProcessTokenElevationTypeUnknown, // could not determine elevation type
                                ProcessTokenElevationTypeDefault, // Process is running on a non-admin account that needs to show the elevation dialog AND enter a password
                                ProcessTokenElevationTypeLimited, // Process can be elevated by showing the elevation dialog
                                ProcessTokenElevationTypeFull    // Process is elevated
                               );

Function ProcessIsElevated : Boolean; Overload;
Function ProcessIsElevated(Const aProcessHandle : THandle) : Boolean; Overload;
Function ProcessElevationType(Const aProcessHandle : THandle) : TProcessTokenElevationType;

// Shell folders.
Function ShellDocumentFolder : String; Overload;
Function ShellLocalAppDataFolder : String;
Function ShellProgramFilesFolder: String;
Function ShellProgramFilesx86Folder: String;

Function ShellApplicationFolder : String; Overload;
Function ShellApplicationAllUsersFolder : String; Overload;
Function ShellDesktopRect : TRect; Overload;
Function ShellDesktopBarRect : TRect; Overload;


// Environment variables.
Function EnvironmentGetVariable(Const sName : String) : String; Overload;
Procedure EnvironmentSetVariable(Const sName, sValue : String); Overload;


// Sending files to the recycle bin - should this be in FileSupprt?
Function FileRecycle(Const sFilename : String) : Boolean; Overload;
Function FolderShellMove(Const sSource, sTarget : String) : Boolean; Overload;
Function FolderShellCopy(Const sSource, sTarget : String) : Boolean; Overload;

// Accepting files dragged onto a window.
Procedure FilesAccept(hWindow : HWND); Overload;
Procedure FilesReject(hWindow : HWND); Overload;
Function FilesCount(hDrop : THandle) : Integer; Overload;
Function FilesPoint(hDrop : THandle) : TPoint; Overload;
Function FilesGet(hDrop : THandle; iIndex : Integer) : String; Overload;
Procedure FilesClose(hDrop : THandle); Overload;


// Window of class sForm is active in the system.
Function IsRunning(Const sForm : String) : Boolean; Overload;

{$ENDIF}

Implementation

{$IFDEF WINDOWS}

Type
  TUnicodeString = Record
    Length : Byte;
    MaxLength : Byte;
    Buffer : PWideChar;
  End;

  TProcessBasicInformation = Record
    ExitStatus : DWord;
    PEBBaseAddress : Pointer;
    AffinityMask : DWord;
    BasePriority : DWord;
    UniqueProcessID : Word;
    ParentProcessID : DWord;
  End;


Function GetPEBAddress(inhandle : THandle): pointer;
Type
  NTQIP = Procedure(ProcessHandle : THandle; ProcessInformationClass : DWord; ProcessInformation : Pointer; ProcessInformationLength : DWord; ReturnLength : Pointer); Stdcall;
Var
  pbi : TProcessBasicInformation;
  aNTDLLHandle : THandle;
  aNtQueryInformationProcessFunction : NTQIP;
Begin
  aNTDLLHandle := LoadLibrary('NTDLL.DLL');

  If aNTDLLHandle <> 0 Then
  Begin
    aNtQueryInformationProcessFunction := GetProcAddress(aNTDLLHandle, 'NtQueryInformationProcess');

    If @aNtQueryInformationProcessFunction <> Nil Then
      aNtQueryInformationProcessFunction(inhandle, 0, @pbi, SizeOf(pbi), Nil);
  End;

  FreeLibrary(aNTDLLHandle);

  Result := pbi.PEBBaseAddress;
End;



Const
  CSIDL_LOCAL_APPDATA = $001c;
  CSIDL_PROGRAM_FILESX86 = $002a;
  CSIDL_PROGRAM_FILES = $0026;
  CSIDL_COMMON_APPDATA                       = $0023;

Function ApplicationHandle : THandle;
Begin 
  Result := 0;
End;


Function DesktopHandle : THandle;
Begin 
  Result := GetDesktopWindow;
End;  


Function ShellFolder(iID : Integer) : String;
Var
  sPath : Array[0..2048] Of Char;
  pIDs  : PItemIDList;
Begin 
  Result := '';

  If SHGetSpecialFolderLocation(ApplicationHandle, iID, pIDs) = S_OK Then
  Begin 
    FillChar(sPath, SizeOf(sPath), #0);

    If ShGetPathFromIDList(pIDs, sPath) Then
      Result := IncludeTrailingPathDelimiter(sPath);
  End;  
End;  


Function ShellDocumentFolder : String;
Begin 
  Result := ShellFolder(CSIDL_PERSONAL);
End;


Function ShellLocalAppDataFolder : String;
Begin
  Result := ShellFolder(CSIDL_LOCAL_APPDATA);
End;


Function ShellProgramFilesFolder: String;
Begin
  Result := ShellFolder(CSIDL_PROGRAM_FILES);
End;


Function ShellProgramFilesx86Folder: String;
Begin
  Result := ShellFolder(CSIDL_PROGRAM_FILESx86);
End;


Function ShellApplicationFolder : String;
Begin 
  Result := ShellFolder(CSIDL_APPDATA);
End;  


Function ExecuteLaunch(Const sVerb, sFilename, sParameters : String; bShow, bErrors : Boolean) : THandle;
Var
  aInfo : TSHELLEXECUTEINFOW;
  wV, wF, wp : WideString;
Begin
  wV := sVerb;
  wF := sFilename;
  wp := sParameters;
  FillChar(aInfo, SizeOf(aInfo), 0);

  aInfo.cbSize := SizeOf(aInfo);

  If bShow Then
    aInfo.nShow := SW_SHOW
  Else
    aInfo.nShow := SW_HIDE;

  aInfo.fMask := SEE_MASK_NOCLOSEPROCESS;

  If bErrors Then
  Begin
   aInfo.wnd := ApplicationHandle;
  End
  Else
  Begin
    aInfo.wnd := 0;
    aInfo.fMask := aInfo.fMask Or SEE_MASK_FLAG_NO_UI;
  End;

  aInfo.lpVerb := PWideChar(wV);
  aInfo.lpFile := PWideChar(wF);
  aInfo.lpParameters := PWideChar(wp);

  If ShellExecuteExW(@aInfo) Then
    Result := aInfo.hProcess
  Else
    Result := 0;
End;  


Procedure ExecuteTerminate(aHandle : THandle);
Begin 
  TerminateProcess(aHandle, 1);
End;  


Function ExecuteWait(aProcess : THandle; Const iTimeout : Cardinal) : Boolean;
Begin 
  Result := WaitForSingleObject(aProcess, iTimeout) = WAIT_OBJECT_0;
End;


Function ExecutionReturnCode(aProcess : THandle; Var iReturnCode : Cardinal) : Boolean;
Begin
  Result := GetExitCodeProcess(aProcess, iReturnCode) And (iReturnCode <> STILL_ACTIVE);
End;


Function ExecuteRunning(aHandle : THandle) : Boolean;
Var
  iStatus : Cardinal;
Begin
  Result := GetExitCodeProcess(aHandle, iStatus) And (iStatus = STILL_ACTIVE);
End;  


Function ExecuteError(iError : Integer) : String;
Begin 
  Case iError Of
    ERROR_FILE_NOT_FOUND: Result := 'The specified file was not found.';
    ERROR_PATH_NOT_FOUND: Result := 'The specified path was not found.';
    ERROR_DDE_FAIL: Result := 'The DDE transaction failed.';
    ERROR_NO_ASSOCIATION: Result := 'There is no application associated with the given file name extension.';
    ERROR_ACCESS_DENIED: Result := 'Access to the specified file is denied.';
    ERROR_DLL_NOT_FOUND: Result := 'One of the library files necessary to run the application can''t be found.';
    ERROR_CANCELLED: Result := 'The function prompted the user for additional information, but the user canceled the request.';
    ERROR_NOT_ENOUGH_MEMORY: Result := 'There is not enough memory to perform the specified action.';
    ERROR_SHARING_VIOLATION: Result := 'A sharing violation occurred.';
  End; 
End;  


Function ExecuteFolder(Const sFolder : String) : Boolean;
Begin 
  Result := ShellExecute(DesktopHandle, 'open', PChar(sFolder), Nil, Nil, SW_SHOWNORMAL) > 32;
End;  


Function ExecuteURL(Const sURL : String) : Boolean;
Begin 
  Result := ExecuteVerb('', sURL);
End;  


Function ExecuteWait(Const sFilename, sParameters : String) : Boolean;
Begin 
  Result := ExecuteWait(ExecuteLaunch('', sFilename, sParameters));
End; 


Function ExecuteParameters(Const aParameters : Array Of String) : String;
Var
  iLoop : Integer;
Begin 
  Result := '';
  For iLoop := Low(aParameters) To High(aParameters) Do
    Result := Result + '"' + aParameters[iLoop] + '" ';

  If Result <> '' Then
    Delete(Result, Length(Result), 1);
End;  


Function ExecuteVerb(Const sVerb, sFilename, sParameters : String; bErrors : Boolean) : Boolean;
Var
  aInfo : TShellExecuteInfo;
Begin 
  FillChar(aInfo, SizeOf(aInfo), 0);

  aInfo.cbSize := SizeOf(aInfo);
  aInfo.nShow := SW_SHOW;

  If Not bErrors Then
    aInfo.fMask := aInfo.fMask Or SEE_MASK_FLAG_NO_UI;

  aInfo.wnd := DesktopHandle;
  aInfo.lpVerb := PChar(sVerb);
  aInfo.lpFile := PChar(sFilename);
  aInfo.lpParameters := PChar(sParameters);

  Result := ShellExecuteExW(@aInfo);
End;  


Function ExecuteOpen(Const sFilename : String; Const sParameters : String = '') : Boolean;
Begin 
  Result := ExecuteVerb('open', sFilename, sParameters, False);
End;  


Function ExecutePrint(Const sFilename : String) : Boolean;
Begin 
  Result := ExecuteVerb('print', sFilename);
End;  


Function ExecuteMail(Const sTo, sSubject, sBody : String) : Boolean;
Var
  sShell : String;
Begin 
  // ExecuteMail forms a mailto command ensuring that any non-alphanumeric character is
  // specified by a hex value escaped by %.

  sShell := '';

  If sSubject <> '' Then
    sShell := sShell + '?Subject=' + EncodeMIME(sSubject);

  If sBody <> '' Then
  Begin
    If sShell <> '' Then
      sShell := sShell + '&'
    Else
      sShell := sShell + '?';

    sShell := sShell + 'Body=' + EncodeMIME(sBody);
  End;

  sShell := 'mailto:' + sTo + sShell;

  Result := ShellExecute(ApplicationHandle, 'open', PChar(sShell), Nil, Nil, SW_SHOW) > 32;
End;  


Function FileRecycle(Const sFilename : String) : Boolean;
Var
  aRecycle : TSHFileOpStruct;
Begin 
  FillChar(aRecycle, SizeOf(aRecycle), 0);

  aRecycle.wFunc := FO_DELETE;
  aRecycle.pFrom := PChar(sFilename + #0);
  aRecycle.fFlags := FOF_ALLOWUNDO Or FOF_NOCONFIRMATION Or FOF_SILENT;
  aRecycle.Wnd := ApplicationHandle;

  Result := ShFileOperation(aRecycle) = 0;
End;  


Function FolderShellMove(Const sSource, sTarget : String) : Boolean;
Var
  aMove : TSHFileOpStruct;
Begin 
  FillChar(aMove, SizeOf(aMove), 0);

  aMove.wFunc := FO_MOVE;
  aMove.pFrom := PChar(sSource + #0);
  aMove.pTo := PChar(sTarget + #0);
  aMove.fFlags := FOF_ALLOWUNDO Or FOF_NOCONFIRMATION Or FOF_SILENT;
  aMove.Wnd := ApplicationHandle;

  Result := ShFileOperation(aMove) = 0;
End;  


Function FolderShellCopy(Const sSource, sTarget : String) : Boolean;
Var
  aCopy : TSHFileOpStruct;
Begin 
  FillChar(aCopy, SizeOf(aCopy), 0);

  aCopy.wFunc := FO_COPY;
  aCopy.pFrom := PChar(sSource + #0);
  aCopy.pTo := PChar(sTarget + #0);
  aCopy.fFlags := FOF_ALLOWUNDO Or FOF_NOCONFIRMATION Or FOF_SILENT;
  aCopy.Wnd := ApplicationHandle;

  Result := ShFileOperation(aCopy) = 0;
End;  


Procedure FilesAccept(hWindow : HWND);
Begin 
  // Accept files dragged over the window.

  DragAcceptFiles(hWindow, True);
End;  


Procedure FilesReject(hWindow : HWND);
Begin 
  // Reject files dragged over the window.

  DragAcceptFiles(hWindow, False);
End;  


Function FilesCount(hDrop : THandle) : Integer;
Begin 
  // Number of files dragged and dropped.

  Result := DragQueryFile(hDrop, $FFFFFFFF, Nil, 0);
End;  


Function FilesPoint(hDrop : THandle) : TPoint;
Begin 
  // Point on window where files were dropped.

  DragQueryPoint(hDrop, {$IFDEF FPC}@{$ENDIF} Result);
End;  


Function FilesGet(hDrop : THandle; iIndex : Integer) : String;
Var
  aResult : Array[0..4096] Of Char;
Begin 
  // Get a file by index [0..FilesCount - 1] that was dragged and dropped.

  DragQueryFile(hDrop, iIndex, aResult, SizeOf(aResult));

  Result := PChar(@aResult);
End;  


Procedure FilesClose(hDrop : THandle);
Begin 
  // Stop handling the dragged and dropped files.

  DragFinish(hDrop);
End;  


Function ProcessID : Cardinal;
Begin 
  Result := Windows.GetCurrentProcessID;
End;


Function ProcessHandle : THandle;
Begin
  Result := Windows.GetCurrentProcess;
End;


Function ProcessName : String;
Var
  aBuffer : Array[0..260] Of Char;
Begin
  Result := Copy(aBuffer, 1, GetModuleFileName(HInstance, aBuffer, SizeOf(aBuffer)));
End;


Function ShellDesktopRect : TRect;
Begin
  Result.Top := GetSystemMetrics(SM_YVIRTUALSCREEN);
  Result.Left := GetSystemMetrics(SM_XVIRTUALSCREEN);
  Result.Bottom := Result.Top + GetSystemMetrics(SM_CYVIRTUALSCREEN);
  Result.Right := Result.Left + GetSystemMetrics(SM_CXVIRTUALSCREEN);
End;


Function ShellDesktopBarRect : TRect;
Var
  aData : APPBARDATA;
Begin
  FillChar(aData, SizeOf(aData), 0);
  aData.cbSize := SizeOf(aData);
  aData.hWnd := DesktopHandle;

  SHAppBarMessage(ABM_GETTASKBARPOS, {$IFDEF FPC}@{$ENDIF} aData);

  Result := TRect(aData.rc);
End;



Function EnvironmentGetVariable(Const sName : String) : String;
Var
  iLength : Integer;
Begin
  iLength := 1024;

  SetLength(Result, iLength);

  iLength := Windows.GetEnvironmentVariable(PChar(sName), PChar(Result), iLength);

  // iLength = 0 if the variable was not found.

  SetLength(Result, iLength);
End;


Procedure EnvironmentSetVariable(Const sName, sValue : String);
Begin
  Windows.SetEnvironmentVariable(PChar(sName), PChar(sValue));
End;


Function ProcessCommandLineParameters: String;
Begin
  Result := ProcessCommandLineParameters(ProcessID);
End;


Function ProcessCommandLineParameters(Const iProcessID : Cardinal): String; Overload;
Var
  aProcessHandle: THandle;
  pRTLUserProcAddress: Pointer;
  pPMBAddress: Pointer;
  sCommandLine: TUnicodeString;
  sCommandLineContents: WideString;
  sParameters : String;
  sProcessName : String;
{$IFDEF VER130}
  iBytesRead : DWord;
{$ELSE}
  iBytesRead : NativeUInt;
{$ENDIF}
Begin
  Result := '';

  aProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION Or PROCESS_VM_READ, False, iProcessID);

  If aProcessHandle <> 0 Then
  Begin
    pPMBAddress := GetPEBAddress(aProcessHandle);

    ReadProcessMemory(aProcessHandle, Pointer(LongInt(pPMBAddress) + $10), @pRTLUserProcAddress, SizeOf(Pointer), iBytesRead);
    ReadProcessMemory(aProcessHandle, Pointer(LongInt(pRTLUserProcAddress) + $40), @sCommandLine, SizeOf(sCommandLine), iBytesRead);

    SetLength(sCommandLineContents, sCommandLine.Length);
    ReadProcessMemory(aProcessHandle, sCommandLine.Buffer, @sCommandLineContents[1], sCommandLine.Length, iBytesRead);

    CloseHandle(aProcessHandle);

    sParameters := WideCharLenToString(PWideChar(sCommandLineContents), sCommandLine.Length Div 2);
    sProcessName := ProcessName;

    Result := StringTrimWhitespace(StringExcludeBefore(StringReplace(sParameters, '"', ''), sProcessName));
  End;
End;


{$IFDEF VER130}
Function ExecuteAsAdmin(hWnd: HWND; sFile, sParameters: String) : Boolean;
Var
  sei : TShellExecuteInfoA;
Begin
  FillChar(sei, SizeOf(@sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.Wnd := hWnd;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT Or SEE_MASK_FLAG_NO_UI;
  sei.lpVerb := 'runas';
  sei.lpFile := PAnsiChar(sFile);
  sei.lpParameters := PAnsiChar(sParameters);
  sei.nShow := SW_SHOWNORMAL;

  Result := ShellExecuteEx(@sei);
End;
{$ELSE}
Function ExecuteAsAdmin(hWnd: HWND; sFile, sParameters: String) : Boolean;
Var
  sei : TShellExecuteInfoW;
Begin
  FillChar(sei, SizeOf(@sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.Wnd := hWnd;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT Or SEE_MASK_FLAG_NO_UI;
  sei.lpVerb := 'runas';
  sei.lpFile := PWideChar(sFile);
  sei.lpParameters := PWideChar(sParameters);
  sei.nShow := SW_SHOWNORMAL;

  Result := ShellExecuteExW(@sei);
End;
{$ENDIF}


Function ProcessElevationType(Const aProcessHandle : THandle) : TProcessTokenElevationType;
Const
  TOKEN_ELEVATION_TYPE = 18;
Var
  iElevationType: Integer;
  iSize: Cardinal;
{$IFDEF VER130}
  iToken: Cardinal;
{$ELSE}
  iToken : NativeUInt;
{$ENDIF}
Begin
  Result := ProcessTokenElevationTypeUnknown;

  If OpenProcessToken(aProcessHandle, TOKEN_QUERY, iToken) Then
  Begin
    Try
      If GetTokenInformation(iToken, TTokenInformationClass(TOKEN_ELEVATION_TYPE), @iElevationType, SizeOf(iElevationType), iSize) Then
      Begin
        If (iElevationType >= Ord(Low(TProcessTokenElevationType))) And (iElevationType <= Ord(High(TProcessTokenElevationType))) Then
          Result := TProcessTokenElevationType(iElevationType);
      End;
    Finally
      CloseHandle(iToken);
    End
  End;
End;


Function ProcessIsElevated : Boolean;
Begin
  Result := ProcessIsElevated(ProcessHandle);
End;


Function ProcessIsElevated(Const aProcessHandle : THandle) : Boolean;
Const
  TOKEN_ELEVATION = 20;
Var
  iElevation: DWord;
  iSize: Cardinal;
{$IFDEF VER130}
  iToken: Cardinal;
{$ELSE}
  iToken : NativeUInt;
{$ENDIF}
Begin
  Result := False;

  If OpenProcessToken(aProcessHandle, TOKEN_QUERY, iToken) Then
  Begin
    Try
      If GetTokenInformation(iToken, TTokenInformationClass(TOKEN_ELEVATION), @iElevation, SizeOf(iElevation), iSize) Then
        Result := iElevation <> 0;
    Finally
      CloseHandle(iToken);
    End
  End;
End;


Function ShellApplicationAllUsersFolder : String;
Begin
  Result := ShellFolder(CSIDL_COMMON_APPDATA);
End;


Function IsRunning(Const sForm : String) : Boolean;
Var
  aWindow : HWND;
Begin
  aWindow := FindWindow(PChar(sForm), Nil);

  // Searchs table to see if the form exists
  Result := (aWindow <> 0);

  If Result Then
  Begin
    // If an application instance already exists
    If IsWindowVisible(aWindow) Then
      SetWindowPos(aWindow, HWND_TOP, 0, 0, 0, 0, SWP_NOSIZE Or SWP_NOMOVE)
    Else
    Begin
      ShowWindow(aWindow, SW_SHOWMAXIMIZED);
      PostMessage(aWindow, WM_USER, 0, 0);
    End
  End;
End;

{$ENDIF}

End.

