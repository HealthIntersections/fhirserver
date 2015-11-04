{
    This file is part of DBGP Plugin for Notepad++
    Copyright (C) 2007  Damjan Zobo Cvetko

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

unit nppplugin;

interface

uses
  Windows,Messages,SciSupport,SysUtils,
  Vcl.Dialogs,Classes,Vcl.Forms;

const
  FuncItemNameLen=64;
  MaxFuncs = 11;

  { Most of this defs are outdated... But there is no consistant N++ doc... }
  NOTEPADPLUS_USER = (WM_USER + 1000);
  NPPM_GETCURRENTSCINTILLA = (NOTEPADPLUS_USER + 4);
  NPPM_GETCURRENTLANGTYPE = (NOTEPADPLUS_USER + 5);
  NPPM_SETCURRENTLANGTYPE = (NOTEPADPLUS_USER + 6);
  NPPM_GETNBOPENFILES = (NOTEPADPLUS_USER + 7);
    ALL_OPEN_FILES = 0;
    PRIMARY_VIEW = 1;
    SECOND_VIEW	= 2;
  NPPM_GETOPENFILENAMES = (NOTEPADPLUS_USER + 8);
  WM_CANCEL_SCINTILLAKEY = (NOTEPADPLUS_USER + 9);
  WM_BIND_SCINTILLAKEY = (NOTEPADPLUS_USER + 10);
  WM_SCINTILLAKEY_MODIFIED = (NOTEPADPLUS_USER + 11);
  NPPM_MODELESSDIALOG = (NOTEPADPLUS_USER + 12);
    MODELESSDIALOGADD = 0;
    MODELESSDIALOGREMOVE = 1;

  NPPM_GETNBSESSIONFILES = (NOTEPADPLUS_USER + 13);
  NPPM_GETSESSIONFILES = (NOTEPADPLUS_USER + 14);
  NPPM_SAVESESSION = (NOTEPADPLUS_USER + 15);
  NPPM_SAVECURRENTSESSION  =(NOTEPADPLUS_USER + 16);  // see TSessionInfo
  NPPM_GETOPENFILENAMESPRIMARY = (NOTEPADPLUS_USER + 17);
  NPPM_GETOPENFILENAMESSECOND = (NOTEPADPLUS_USER + 18);
  WM_GETPARENTOF = (NOTEPADPLUS_USER + 19);
  NPPM_CREATESCINTILLAHANDLE = (NOTEPADPLUS_USER + 20);
  NPPM_DESTROYSCINTILLAHANDLE = (NOTEPADPLUS_USER + 21);
  NPPM_GETNBUSERLANG = (NOTEPADPLUS_USER + 22);
  NPPM_GETCURRENTDOCINDEX = (NOTEPADPLUS_USER + 23);
    MAIN_VIEW = 0;
    SUB_VIEW = 1;

  NPPM_SETSTATUSBAR = (NOTEPADPLUS_USER + 24);
    STATUSBAR_DOC_TYPE = 0;
    STATUSBAR_DOC_SIZE = 1;
    STATUSBAR_CUR_POS = 2;
    STATUSBAR_EOF_FORMAT = 3;
    STATUSBAR_UNICODE_TYPE = 4;
    STATUSBAR_TYPING_MODE = 5;

  NPPM_GETMENUHANDLE = (NOTEPADPLUS_USER + 25);
    NPPPLUGINMENU = 0;

  NPPM_ENCODESCI = (NOTEPADPLUS_USER + 26);
  //ascii file to unicode
  //int WM_ENCODE_SCI(MAIN_VIEW/SUB_VIEW, 0)
  //return new unicodeMode

  NPPM_DECODESCI = (NOTEPADPLUS_USER + 27);
  //unicode file to ascii
  //int WM_DECODE_SCI(MAIN_VIEW/SUB_VIEW, 0)
  //return old unicodeMode

  NPPM_ACTIVATEDOC = (NOTEPADPLUS_USER + 28);
  //void WM_ACTIVATE_DOC(int index2Activate, int view)

  NPPM_LAUNCHFINDINFILESDLG = (NOTEPADPLUS_USER + 29);
  //void WM_LAUNCH_FINDINFILESDLG(char * dir2Search, char * filtre)

  NPPM_DMMSHOW = (NOTEPADPLUS_USER + 30);
  NPPM_DMMHIDE	= (NOTEPADPLUS_USER + 31);
  NPPM_DMMUPDATEDISPINFO = (NOTEPADPLUS_USER + 32);
  //void WM_DMM_xxx(0, tTbData->hClient)

  NPPM_DMMREGASDCKDLG = (NOTEPADPLUS_USER + 33);
  //void WM_DMM_REGASDCKDLG(0, &tTbData)

  NPPM_LOADSESSION = (NOTEPADPLUS_USER + 34);
  //void WM_LOADSESSION(0, const char* file name)
  NPPM_DMMVIEWOTHERTAB = (NOTEPADPLUS_USER + 35);
  //void WM_DMM_VIEWOTHERTAB(0, tTbData->hClient)
  NPPM_RELOADFILE = (NOTEPADPLUS_USER + 36);
  //BOOL WM_RELOADFILE(BOOL withAlert, char *filePathName2Reload)
  NPPM_SWITCHTOFILE = (NOTEPADPLUS_USER + 37);
  //BOOL WM_SWITCHTOFILE(0, char *filePathName2switch)
  NPPM_SAVECURRENTFILE = (NOTEPADPLUS_USER + 38);
  //BOOL WM_SWITCHTOFILE(0, 0)
  NPPM_SAVEALLFILES	= (NOTEPADPLUS_USER + 39);
  //BOOL WM_SAVEALLFILES(0, 0)
  NPPM_SETMENUITEMCHECK	= (NOTEPADPLUS_USER + 40);
  //void WM_PIMENU_CHECK(UINT	funcItem[X]._cmdID, TRUE/FALSE)
  NPPM_ADDTOOLBARICON = (NOTEPADPLUS_USER + 41); // see TToolbarIcons
  //void WM_ADDTOOLBARICON(UINT funcItem[X]._cmdID, toolbarIcons icon)
  NPPM_GETWINDOWSVERSION = (NOTEPADPLUS_USER + 42);
  //winVer WM_GETWINDOWSVERSION(0, 0)
  NPPM_DMMGETPLUGINHWNDBYNAME = (NOTEPADPLUS_USER + 43);
  //HWND WM_DMM_GETPLUGINHWNDBYNAME(const char *windowName, const char *moduleName)
  // if moduleName is NULL, then return value is NULL
  // if windowName is NULL, then the first found window handle which matches with the moduleName will be returned
  NPPM_MAKECURRENTBUFFERDIRTY = (NOTEPADPLUS_USER + 44);
  //BOOL NPPM_MAKECURRENTBUFFERDIRTY(0, 0)
  NPPM_GETENABLETHEMETEXTUREFUNC = (NOTEPADPLUS_USER + 45);
  //BOOL NPPM_GETENABLETHEMETEXTUREFUNC(0, 0)
  NPPM_GETPLUGINSCONFIGDIR = (NOTEPADPLUS_USER + 46);
  //void NPPM_GETPLUGINSCONFIGDIR(int strLen, char *str)

  // new
  NPPM_MSGTOPLUGIN = (NOTEPADPLUS_USER + 47); // see TCommunicationInfo
	//BOOL NPPM_MSGTOPLUGIN(TCHAR *destModuleName, CommunicationInfo *info)
	// return value is TRUE when the message arrive to the destination plugins.
	// if destModule or info is NULL, then return value is FALSE
//		struct CommunicationInfo {
//			long internalMsg;
//			const TCHAR * srcModuleName;
//			void * info; // defined by plugin
//		};

	NPPM_MENUCOMMAND = (NOTEPADPLUS_USER + 48);
	//void NPPM_MENUCOMMAND(0, int cmdID)
	// uncomment //#include "menuCmdID.h"
	// in the beginning of this file then use the command symbols defined in "menuCmdID.h" file
	// to access all the Notepad++ menu command items

	NPPM_TRIGGERTABBARCONTEXTMENU = (NOTEPADPLUS_USER + 49);
	//void NPPM_TRIGGERTABBARCONTEXTMENU(int view, int index2Activate)

	NPPM_GETNPPVERSION = (NOTEPADPLUS_USER + 50);
	// int NPPM_GETNPPVERSION(0, 0)
	// return version
	// ex : v4.6
	// HIWORD(version) == 4
	// LOWORD(version) == 6

	NPPM_HIDETABBAR = (NOTEPADPLUS_USER + 51);
	// BOOL NPPM_HIDETABBAR(0, BOOL hideOrNot)
	// if hideOrNot is set as TRUE then tab bar will be hidden
	// otherwise it'll be shown.
	// return value : the old status value

	NPPM_ISTABBARHIDE = (NOTEPADPLUS_USER + 52);
	// BOOL NPPM_ISTABBARHIDE(0, 0)
	// returned value : TRUE if tab bar is hidden, otherwise FALSE

	NPPM_CHECKDOCSTATUS = (NOTEPADPLUS_USER + 53);
	// VOID NPPM_CHECKDOCSTATUS(BOOL, 0)

	NPPM_ENABLECHECKDOCOPT = (NOTEPADPLUS_USER + 54);
	// VOID NPPM_ENABLECHECKDOCOPT(OPT, 0)
		// where OPT is :
		CHECKDOCOPT_NONE = 0;
		CHECKDOCOPT_UPDATESILENTLY = 1;
		CHECKDOCOPT_UPDATEGO2END = 2;

	NPPM_GETCHECKDOCOPT = (NOTEPADPLUS_USER + 55);
	// INT NPPM_GETCHECKDOCOPT(0, 0)
	NPPM_SETCHECKDOCOPT = (NOTEPADPLUS_USER + 56);
	// INT NPPM_SETCHECKDOCOPT(OPT, 0)

	NPPM_GETPOSFROMBUFFERID = (NOTEPADPLUS_USER + 57);
	// INT NPPM_GETPOSFROMBUFFERID(INT bufferID, 0)
	// Return VIEW|INDEX from a buffer ID. -1 if the bufferID non existing
	//
	// VIEW takes 2 highest bits and INDEX (0 based) takes the rest (30 bits)
	// Here's the values for the view :
	//  MAIN_VIEW 0
	//  SUB_VIEW  1

	NPPM_GETFULLPATHFROMBUFFERID = (NOTEPADPLUS_USER + 58);
	// INT NPPM_GETFULLPATHFROMBUFFERID(INT bufferID, CHAR *fullFilePath)
	// Get full path file name from a bufferID.
	// Return -1 if the bufferID non existing, otherwise the number of TCHAR copied/to copy
	// User should call it with fullFilePath be NULL to get the number of TCHAR (not including the nul character),
	// allocate fullFilePath with the return values + 1, then call it again to get  full path file name

	NPPM_GETBUFFERIDFROMPOS = (NOTEPADPLUS_USER + 59);
	//wParam: Position of document
	//lParam: View to use, 0 = Main, 1 = Secondary
	//Returns 0 if invalid

	NPPM_GETCURRENTBUFFERID = (NOTEPADPLUS_USER + 60);
	//Returns active Buffer

	NPPM_RELOADBUFFERID = (NOTEPADPLUS_USER + 61);
	//Reloads Buffer
	//wParam: Buffer to reload
	//lParam: 0 if no alert, else alert

	NPPM_SETFILENAME = (NOTEPADPLUS_USER + 63);
	//wParam: BufferID to rename
	//lParam: name to set (TCHAR*)
	//Buffer must have been previously unnamed (eg "new 1" document types)

	NPPM_GETBUFFERLANGTYPE = (NOTEPADPLUS_USER + 64);
	//wParam: BufferID to get LangType from
	//lParam: 0
	//Returns as int, see LangType. -1 on error

	NPPM_SETBUFFERLANGTYPE = (NOTEPADPLUS_USER + 65);
	//wParam: BufferID to set LangType of
	//lParam: LangType
	//Returns TRUE on success, FALSE otherwise
	//use int, see LangType for possible values
	//L_USER and L_EXTERNAL are not supported

	NPPM_GETBUFFERENCODING = (NOTEPADPLUS_USER + 66);
	//wParam: BufferID to get encoding from
	//lParam: 0
	//returns as int, see UniMode. -1 on error

	NPPM_SETBUFFERENCODING = (NOTEPADPLUS_USER + 67);
	//wParam: BufferID to set encoding of
	//lParam: format
	//Returns TRUE on success, FALSE otherwise
	//use int, see UniMode
	//Can only be done on new, unedited files

	NPPM_GETBUFFERFORMAT = (NOTEPADPLUS_USER + 68);
	//wParam: BufferID to get format from
	//lParam: 0
	//returns as int, see formatType. -1 on error

	NPPM_SETBUFFERFORMAT = (NOTEPADPLUS_USER + 69);
	//wParam: BufferID to set format of
	//lParam: format
	//Returns TRUE on success, FALSE otherwise
	//use int, see formatType



  // Notification code
  NPPN_FIRST = 1000;
  NPPN_READY = (NPPN_FIRST + 1);
  // To notify plugins that all the procedures of launchment of notepad++ are done.
  //scnNotification->nmhdr.code = NPPN_READY;
  //scnNotification->nmhdr.hwndFrom = hwndNpp;
  //scnNotification->nmhdr.idFrom = 0;

  NPPN_TB_MODIFICATION = (NPPN_FIRST + 2);
  // To notify plugins that toolbar icons can be registered
  //scnNotification->nmhdr.code = NPPN_TB_MODIFICATION;
  //scnNotification->nmhdr.hwndFrom = hwndNpp;
  //scnNotification->nmhdr.idFrom = 0;

  NPPN_FILEBEFORECLOSE = (NPPN_FIRST + 3);
  // To notify plugins that the current file is about to be closed
  //scnNotification->nmhdr.code = NPPN_FILEBEFORECLOSE;
  //scnNotification->nmhdr.hwndFrom = hwndNpp;
  //scnNotification->nmhdr.idFrom = 0;

  NPPN_FILEOPENED = (NPPN_FIRST + 4);
  // To notify plugins that the current file is just opened
  //scnNotification->nmhdr.code = NPPN_FILEOPENED;
  //scnNotification->nmhdr.hwndFrom = hwndNpp;
  //scnNotification->nmhdr.idFrom = 0;

  NPPN_FILECLOSED = (NPPN_FIRST + 5);
  // To notify plugins that the current file is just closed
  //scnNotification->nmhdr.code = NPPN_FILECLOSED;
  //scnNotification->nmhdr.hwndFrom = hwndNpp;
  //scnNotification->nmhdr.idFrom = 0;

  NPPN_FILEBEFOREOPEN = (NPPN_FIRST + 6);
  // To notify plugins that the current file is about to be opened
  //scnNotification->nmhdr.code = NPPN_FILEBEFOREOPEN;
  //scnNotification->nmhdr.hwndFrom = hwndNpp;
  //scnNotification->nmhdr.idFrom = 0;

  NPPN_FILEBEFORESAVE = (NPPN_FIRST + 7);
  // To notify plugins that the current file is about to be saved
  //scnNotification->nmhdr.code = NPPN_FILEBEFOREOPEN;
  //scnNotification->nmhdr.hwndFrom = hwndNpp;
  //scnNotification->nmhdr.idFrom = 0;

  NPPN_FILESAVED = (NPPN_FIRST + 8);
  // To notify plugins that the current file is just saved
  //scnNotification->nmhdr.code = NPPN_FILECLOSED;
  //scnNotification->nmhdr.hwndFrom = hwndNpp;
  //scnNotification->nmhdr.idFrom = 0;

  NPPN_SHUTDOWN = (NPPN_FIRST + 9);
  // To notify plugins that Notepad++ is about to be shutdowned.
  //scnNotification->nmhdr.code = NPPN_SHOUTDOWN;
  //scnNotification->nmhdr.hwndFrom = hwndNpp;
  //scnNotification->nmhdr.idFrom = 0;

  NPPN_BUFFERACTIVATED = (NPPN_FIRST + 10);

  RUNCOMMAND_USER    = (WM_USER + 3000);
    VAR_NOT_RECOGNIZED = 0;
    FULL_CURRENT_PATH = 1;
    CURRENT_DIRECTORY = 2;
    FILE_NAME = 3;
    NAME_PART = 4;
    EXT_PART = 5;
    CURRENT_WORD = 6;
    NPP_DIRECTORY = 7;
  NPPM_GETFULLCURRENTPATH = (RUNCOMMAND_USER + FULL_CURRENT_PATH);
  NPPM_GETCURRENTDIRECTORY = (RUNCOMMAND_USER + CURRENT_DIRECTORY);
  NPPM_GETFILENAME = (RUNCOMMAND_USER + FILE_NAME);
  NPPM_GETNAMEPART = (RUNCOMMAND_USER + NAME_PART);
  NPPM_GETEXTPART = (RUNCOMMAND_USER + EXT_PART);
  NPPM_GETCURRENTWORD = (RUNCOMMAND_USER + CURRENT_WORD);
  NPPM_GETNPPDIRECTORY = (RUNCOMMAND_USER + NPP_DIRECTORY);

  MACRO_USER    = (WM_USER + 4000);
  WM_ISCURRENTMACRORECORDED = (MACRO_USER + 01);
  WM_MACRODLGRUNMACRO       = (MACRO_USER + 02);


{ Humm.. is tis npp specific? }
  SCINTILLA_USER = (WM_USER + 2000);
{
#define WM_DOCK_USERDEFINE_DLG      (SCINTILLA_USER + 1)
#define WM_UNDOCK_USERDEFINE_DLG    (SCINTILLA_USER + 2)
#define WM_CLOSE_USERDEFINE_DLG		(SCINTILLA_USER + 3)
#define WM_REMOVE_USERLANG		    (SCINTILLA_USER + 4)
#define WM_RENAME_USERLANG			(SCINTILLA_USER + 5)
#define WM_REPLACEALL_INOPENEDDOC	(SCINTILLA_USER + 6)
#define WM_FINDALL_INOPENEDDOC  	(SCINTILLA_USER + 7)
}
  WM_DOOPEN = (SCINTILLA_USER + 8);
{
#define WM_FINDINFILES			  	(SCINTILLA_USER + 9)
}

{ docking.h }
//   defines for docking manager
  CONT_LEFT = 0;
  CONT_RIGHT = 1;
  CONT_TOP = 2;
  CONT_BOTTOM = 3;
  DOCKCONT_MAX = 4;

// mask params for plugins of internal dialogs
  DWS_ICONTAB = 1; // Icon for tabs are available
  DWS_ICONBAR = 2; // Icon for icon bar are available (currently not supported)
  DWS_ADDINFO = 4; // Additional information are in use

// default docking values for first call of plugin
  DWS_DF_CONT_LEFT = CONT_LEFT shl 28;	        // default docking on left
  DWS_DF_CONT_RIGHT = CONT_RIGHT shl 28;	// default docking on right
  DWS_DF_CONT_TOP = CONT_TOP shl 28;	        // default docking on top
  DWS_DF_CONT_BOTTOM = CONT_BOTTOM shl 28;	// default docking on bottom
  DWS_DF_FLOATING = $80000000;			// default state is floating

{ dockingResource.h }
  DMN_FIRST = 1050;
  DMN_CLOSE = (DMN_FIRST + 1); //nmhdr.code = DWORD(DMN_CLOSE, 0)); //nmhdr.hwndFrom = hwndNpp; //nmhdr.idFrom = ctrlIdNpp;
  DMN_DOCK = (DMN_FIRST + 2);
  DMN_FLOAT = (DMN_FIRST + 3); //nmhdr.code = DWORD(DMN_XXX, int newContainer);	//nmhdr.hwndFrom = hwndNpp; //nmhdr.idFrom = ctrlIdNpp;


type
{$IFDEF NPPUNICODE}
  nppString = WideString;
  nppChar = WChar;
  nppPChar = PWChar;
{$ELSE}
  nppString = AnsiString;
  nppChar = AnsiChar;
  nppPChar = PAnsiChar;
{$ENDIF}

  TNppLang = (L_TXT, L_PHP , L_C, L_CPP, L_CS, L_OBJC, L_JAVA, L_RC,
              L_HTML, L_XML, L_MAKEFILE, L_PASCAL, L_BATCH, L_INI, L_NFO, L_USER,
              L_ASP, L_SQL, L_VB, L_JS, L_CSS, L_PERL, L_PYTHON, L_LUA,
              L_TEX, L_FORTRAN, L_BASH, L_FLASH, L_NSIS, L_TCL, L_LISP, L_SCHEME,
              L_ASM, L_DIFF, L_PROPS, L_PS, L_RUBY, L_SMALLTALK, L_VHDL, L_KIX, L_AU3,
              L_CAML, L_ADA, L_VERILOG, L_MATLAB, L_HASKELL, L_INNO,
              // The end of enumated language type, so it should be always at the end
              L_END);

  TSessionInfo = record
    SessionFilePathName: nppPChar;
    NumFiles: Integer;
    Files: array of nppPChar;
  end;

  TToolbarIcons = record
    ToolbarBmp: HBITMAP;
    ToolbarIcon: HICON;
  end;

  TCommunicationInfo = record
    internalMsg: Cardinal;
    srcModuleName: nppPChar;
    info: Pointer;
  end;

  TNppData = record
    NppHandle: HWND;
    ScintillaMainHandle: HWND;
    ScintillaSecondHandle: HWND;
  end;

  TShortcutKey = record
    IsCtrl: Boolean;
    IsAlt: Boolean;
    IsShift: Boolean;
    Key: Char;
  end;
  PShortcutKey = ^TShortcutKey;

  PFUNCPLUGINCMD = procedure; cdecl;

  _TFuncItem = record
    ItemName: Array[0..FuncItemNameLen-1] of nppChar;
    Func: PFUNCPLUGINCMD;
    CmdID: Integer;
    Checked: Boolean;
    ShortcutKey: PShortcutKey;
  end;

  TToolbarData = record
    ClientHandle: HWND;
    Title: nppPChar;
    DlgId: Integer;
    Mask: Cardinal;
    IconTab: HICON; // still dont know how to use this...
    AdditionalInfo: nppPChar;
    FloatRect: TRect;  // internal
    PrevContainer: Cardinal; // internal
    ModuleName:nppPChar; // name of module GetModuleFileName(0...)
  end;

  TNppPlugin = class(TObject)
  private
    FuncArray: array of _TFuncItem;
    function GetCurrentText: string;
    procedure SetCurrentText(value : String);
  protected
    PluginName: nppString;
    function GetPluginsConfigDir: string;
    function AddFuncItem(Name: nppString; Func: PFUNCPLUGINCMD): Integer; overload;
    function AddFuncItem(Name: nppString; Func: PFUNCPLUGINCMD; ShortcutKey: TShortcutKey): Integer; overload;
  public
    NppData: TNppData;
    constructor Create;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    function CmdIdFromDlgId(DlgId: Integer): Integer;

    // needed for DLL export.. wrappers are in the main dll file.
    procedure SetInfo(NppData: TNppData); virtual;
    function GetName: nppPChar;
    function GetFuncsArray(var FuncsCount: Integer): Pointer;
    procedure BeNotified(sn: PSCNotification);
    procedure MessageProc(var Msg: TMessage); virtual;

    // hooks
    procedure DoNppnReady; virtual;
    procedure DoNppnFileOpened; virtual;
    procedure DoNppnBufferChange; virtual;
    procedure DoNppnToolbarModification; virtual;
    procedure DoNppnShutdown; virtual;
    procedure DoNppnTextModified; virtual;
    procedure DoNppnDwellStart(offset : integer); virtual;
    procedure DoNppnDwellEnd; virtual;

    // df
    function DoOpen(filename: String): boolean; overload;
    function DoOpen(filename: String; Line: Integer): boolean; overload;
    procedure GetFileLine(var filename: String; var Line: Integer);
    function GetWord: string;
    property CurrentText : String read GetCurrentText write SetCurrentText;
  end;


implementation

{ TNppPlugin }

{ This is hacking for troubble...
  We need to unset the Application handler so that the forms
  don't get berserk and start throwing OS error 1004.
  This happens because the main NPP HWND is already lost when the
  DLL_PROCESS_DETACH gets called, and the form tries to allocate a new
  handler for sending the "close" windows message...
}
procedure TNppPlugin.BeforeDestruction;
begin
  Application.Handle := 0;
  Application.Terminate;
  inherited;
end;

constructor TNppPlugin.Create;
begin
  inherited;
end;

destructor TNppPlugin.Destroy;
var i: Integer;
begin
  for i:=0 to Length(self.FuncArray)-1 do
  begin
    if (self.FuncArray[i].ShortcutKey <> nil) then
    begin
      Dispose(self.FuncArray[i].ShortcutKey);
    end;
  end;
  inherited;
end;

function TNppPlugin.AddFuncItem(Name: nppString; Func: PFUNCPLUGINCMD): Integer;
var
  i: Integer;
begin
  i := Length(self.FuncArray);
  SetLength(Self.FuncArray,i+1);
{$IFDEF NPPUNICODE}
  StringToWideChar(Name, self.FuncArray[i].ItemName,1000); // @todo: change to constant
{$ELSE}
  StrCopy(self.FuncArray[i].ItemName, PChar(Name));
{$ENDIF}
  self.FuncArray[i].Func := Func;
  self.FuncArray[i].ShortcutKey := nil;
  Result := i;
end;

function TNppPlugin.AddFuncItem(Name: nppString; Func: PFUNCPLUGINCMD;
  ShortcutKey: TShortcutKey): Integer;
var
  i: Integer;
begin
  i := self.AddFuncItem(Name, Func);
  New(self.FuncArray[i].ShortcutKey);
  self.FuncArray[i].ShortcutKey.IsCtrl := ShortcutKey.IsCtrl;
  self.FuncArray[i].ShortcutKey.IsAlt := ShortcutKey.IsAlt;
  self.FuncArray[i].ShortcutKey.IsShift := ShortcutKey.IsShift;
  self.FuncArray[i].ShortcutKey.Key := ShortcutKey.Key; // need widechar ??
  Result := i;
end;

function TNppPlugin.GetCurrentText: string;
var
  l : integer;
  s: ansistring;
begin
  l := SendMessage(self.NppData.ScintillaMainHandle, SCI_GETTEXTLENGTH, 0, 0)+1;
  SetLength(s, l);
  SendMessage(self.NppData.ScintillaMainHandle, SCI_GETTEXT, l, LPARAM(PChar(s)));
  Result := s;
  setLength(result, length(result)-1);
end;

procedure TNppPlugin.GetFileLine(var filename: String; var Line: Integer);
var
  s: String;
  r: Integer;
begin
  s := '';
  SetLength(s, 300);
  SendMessage(self.NppData.NppHandle, NPPM_GETFULLCURRENTPATH,0, LPARAM(PChar(s)));
  SetLength(s, StrLen(PChar(s)));
  filename := s;

  r := SendMessage(self.NppData.ScintillaMainHandle, SciSupport.SCI_GETCURRENTPOS, 0, 0);
  Line := SendMessage(self.NppData.ScintillaMainHandle, SciSupport.SCI_LINEFROMPOSITION, r, 0);
end;

function TNppPlugin.GetFuncsArray(var FuncsCount: Integer): Pointer;
begin
  FuncsCount := Length(self.FuncArray);
  Result := self.FuncArray;
end;

function TNppPlugin.GetName: nppPChar;
begin
  Result := nppPChar(self.PluginName);
end;

function TNppPlugin.GetPluginsConfigDir: string;
var
  s: string;
  r: integer;
begin
  SetLength(s, 1001);
  r := SendMessage(self.NppData.NppHandle, NPPM_GETPLUGINSCONFIGDIR, 1000, LPARAM(PChar(s)));
  SetString(s, PChar(s), StrLen(PChar(s)));
  Result := s;
end;

procedure TNppPlugin.BeNotified(sn: PSCNotification);
begin
  if (HWND(sn^.nmhdr.hwndFrom) = self.NppData.NppHandle) and (sn^.nmhdr.code = NPPN_TB_MODIFICATION) then
  begin
    self.DoNppnToolbarModification;
  end
  else if (HWND(sn^.nmhdr.hwndFrom) = self.NppData.NppHandle) and (sn^.nmhdr.code = NPPN_SHUTDOWN) then
  begin
    self.DoNppnShutdown;
  end
  else if (HWND(sn^.nmhdr.hwndFrom) = self.NppData.NppHandle) and (sn^.nmhdr.code = NPPN_READY) then
  begin
    self.DoNppnReady;
  end
  else if (HWND(sn^.nmhdr.hwndFrom) = self.NppData.NppHandle) and (sn^.nmhdr.code = NPPN_FILEOPENED) then
  begin
    self.DoNppnFileOpened;
  end
  else if (HWND(sn^.nmhdr.hwndFrom) = self.NppData.NppHandle) and (sn^.nmhdr.code = NPPN_BUFFERACTIVATED) then
  begin
    self.DoNppnBufferChange;
  end
  else if (HWND(sn^.nmhdr.hwndFrom) = self.NppData.ScintillaMainHandle) and (sn^.nmhdr.code = SCN_MODIFIED) then
  begin
    self.DoNppnTextModified;
  end
  else if (HWND(sn^.nmhdr.hwndFrom) = self.NppData.ScintillaMainHandle) and (sn^.nmhdr.code = SCN_DWELLSTART) then
  begin
    self.DoNppnDwellStart(sn^.position);
  end
  else if (HWND(sn^.nmhdr.hwndFrom) = self.NppData.ScintillaMainHandle) and (sn^.nmhdr.code = SCN_DWELLEND) then
  begin
    self.DoNppnDwellEnd;
  end;
  // @todo
end;

procedure TNppPlugin.MessageProc(var Msg: TMessage);
var
  hm: HMENU;
  i: integer;
begin
  if (Msg.Msg = WM_CREATE) then
  begin
    // Change - to separator items
    hm := GetMenu(self.NppData.NppHandle);
    for i:=0 to Length(self.FuncArray)-1 do
      if (self.FuncArray[i].ItemName[0] = '-') then
        ModifyMenu(hm, self.FuncArray[i].CmdID, MF_BYCOMMAND or MF_SEPARATOR, 0, nil);
  end;
  Dispatch(Msg);
end;

procedure TNppPlugin.SetCurrentText(value: String);
var
  v : AnsiString;
  p : PAnsiChar;
begin
  v := value + #0;
  p := PAnsiChar(v);
  SendMessage(self.NppData.ScintillaMainHandle, SCI_SETTEXT, length(value)-1, LPARAM(p));
end;

procedure TNppPlugin.SetInfo(NppData: TNppData);
begin
  self.NppData := NppData;
  Application.Handle := NppData.NppHandle;
end;

// utils

function TNppPlugin.GetWord: string;
var
  s: string;
begin
  SetLength(s, 800);
  SendMessage(self.NppData.NppHandle, NPPM_GETCURRENTWORD,0,LPARAM(PChar(s)));
  Result := s;
end;

function TNppPlugin.DoOpen(filename: String): boolean;
var
  r: integer;
  s: string;
begin
  // ask if we are not already opened
  SetLength(s, 500);
  r := SendMessage(self.NppData.NppHandle, NPPM_GETFULLCURRENTPATH, 0, LPARAM(PChar(s)));
  SetString(s, PChar(s), strlen(PChar(s)));
  Result := true;
  if (s = filename) then exit;
  r := SendMessage(self.NppData.NppHandle, WM_DOOPEN, 0, LPARAM(PChar(filename)));
  Result := (r=0);
end;

function TNppPlugin.DoOpen(filename: String; Line: Integer): boolean;
var
  r: boolean;
begin
  r := self.DoOpen(filename);
  if (r) then
    SendMessage(self.NppData.ScintillaMainHandle, SciSupport.SCI_GOTOLINE, Line,0);
  Result := r;
end;

// overrides
procedure TNppPlugin.DoNppnBufferChange;
begin
  // override these
end;

procedure TNppPlugin.DoNppnDwellEnd;
begin
  // override these
end;

procedure TNppPlugin.DoNppnDwellStart(offset: integer);
begin
  // override these
end;

procedure TNppPlugin.DoNppnFileOpened;
begin
  // override these
end;

procedure TNppPlugin.DoNppnReady;
begin
  // override these
end;

procedure TNppPlugin.DoNppnShutdown;
begin
  // override these
end;

procedure TNppPlugin.DoNppnTextModified;
begin
  // override these
end;

procedure TNppPlugin.DoNppnToolbarModification;
begin
  // override these
end;



function TNppPlugin.CmdIdFromDlgId(DlgId: Integer): Integer;
begin
  Result := self.FuncArray[DlgId].CmdId;
end;

end.
