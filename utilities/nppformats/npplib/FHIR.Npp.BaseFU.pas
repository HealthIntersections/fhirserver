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

unit FHIR.Npp.BaseFU;

interface

uses
  Windows, Messages, FHIR.Npp.ScintillaFU, SysUtils,
  Vcl.Dialogs, Classes, Vcl.Forms;

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
    SECOND_VIEW  = 2;
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
  NPPM_DMMHIDE  = (NOTEPADPLUS_USER + 31);
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
  NPPM_SAVECURRENTFILEAs = (NOTEPADPLUS_USER + 78);
  //BOOL WM_SWITCHTOFILE(0, 0)
  NPPM_SAVEALLFILES  = (NOTEPADPLUS_USER + 39);
  //BOOL WM_SAVEALLFILES(0, 0)
  NPPM_SETMENUITEMCHECK  = (NOTEPADPLUS_USER + 40);
  //void WM_PIMENU_CHECK(UINT  funcItem[X]._cmdID, TRUE/FALSE)
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
//    struct CommunicationInfo {
//      long internalMsg;
//      const TCHAR * srcModuleName;
//      void * info; // defined by plugin
//    };

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
#define WM_CLOSE_USERDEFINE_DLG    (SCINTILLA_USER + 3)
#define WM_REMOVE_USERLANG        (SCINTILLA_USER + 4)
#define WM_RENAME_USERLANG      (SCINTILLA_USER + 5)
#define WM_REPLACEALL_INOPENEDDOC  (SCINTILLA_USER + 6)
#define WM_FINDALL_INOPENEDDOC    (SCINTILLA_USER + 7)
}
  WM_DOOPEN = (SCINTILLA_USER + 8);
{
#define WM_FINDINFILES          (SCINTILLA_USER + 9)
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
  DWS_DF_CONT_LEFT = CONT_LEFT shl 28;          // default docking on left
  DWS_DF_CONT_RIGHT = CONT_RIGHT shl 28;  // default docking on right
  DWS_DF_CONT_TOP = CONT_TOP shl 28;          // default docking on top
  DWS_DF_CONT_BOTTOM = CONT_BOTTOM shl 28;  // default docking on bottom
  DWS_DF_FLOATING = $80000000;      // default state is floating

{ dockingResource.h }
  DMN_FIRST = 1050;
  DMN_CLOSE = (DMN_FIRST + 1); //nmhdr.code = DWORD(DMN_CLOSE, 0)); //nmhdr.hwndFrom = hwndNpp; //nmhdr.idFrom = ctrlIdNpp;
  DMN_DOCK = (DMN_FIRST + 2);
  DMN_FLOAT = (DMN_FIRST + 3); //nmhdr.code = DWORD(DMN_XXX, int newContainer);  //nmhdr.hwndFrom = hwndNpp; //nmhdr.idFrom = ctrlIdNpp;


  IDM = 40000;

  IDM_FILE       = (IDM + 1000);
  IDM_FILE_NEW                         = (IDM_FILE + 1);
  IDM_FILE_OPEN                        = (IDM_FILE + 2);
  IDM_FILE_CLOSE                       = (IDM_FILE + 3);
  IDM_FILE_CLOSEALL                  = (IDM_FILE + 4);
  IDM_FILE_CLOSEALL_BUT_CURRENT   = (IDM_FILE + 5);
  IDM_FILE_SAVE                        = (IDM_FILE + 6);
  IDM_FILE_SAVEALL            = (IDM_FILE + 7);
  IDM_FILE_SAVEAS           = (IDM_FILE + 8);
  IDM_FILE_ASIAN_LANG         = (IDM_FILE + 9);
  IDM_FILE_PRINT           = (IDM_FILE + 10);
  IDM_FILE_PRINTNOW               =  1001;
  IDM_FILE_EXIT           = (IDM_FILE + 11);
  IDM_FILE_LOADSESSION      = (IDM_FILE + 12);
  IDM_FILE_SAVESESSION    = (IDM_FILE + 13);
  IDM_FILE_RELOAD         = (IDM_FILE + 14);
  IDM_FILE_SAVECOPYAS       = (IDM_FILE + 15);
  IDM_FILE_DELETE           = (IDM_FILE + 16);
  IDM_FILE_RENAME           = (IDM_FILE + 17);

 // A mettre ` jour si on ajoute nouveau menu item dans le menu "File"
  IDM_FILEMENU_LASTONE =   IDM_FILE_RENAME;

  IDM_EDIT       = (IDM + 2000);
  IDM_EDIT_CUT          = (IDM_EDIT + 1);
  IDM_EDIT_COPY        = (IDM_EDIT + 2);
  IDM_EDIT_UNDO        = (IDM_EDIT + 3);
  IDM_EDIT_REDO        = (IDM_EDIT + 4);
  IDM_EDIT_PASTE        = (IDM_EDIT + 5);
  IDM_EDIT_DELETE        = (IDM_EDIT + 6);
  IDM_EDIT_SELECTALL          = (IDM_EDIT + 7);

  IDM_EDIT_INS_TAB            = (IDM_EDIT + 8);
  IDM_EDIT_RMV_TAB            = (IDM_EDIT + 9);
  IDM_EDIT_DUP_LINE           = (IDM_EDIT + 10);
  IDM_EDIT_TRANSPOSE_LINE     = (IDM_EDIT + 11);
  IDM_EDIT_SPLIT_LINES        = (IDM_EDIT + 12);
  IDM_EDIT_JOIN_LINES         = (IDM_EDIT + 13);
  IDM_EDIT_LINE_UP            = (IDM_EDIT + 14);
  IDM_EDIT_LINE_DOWN          = (IDM_EDIT + 15);
  IDM_EDIT_UPPERCASE          = (IDM_EDIT + 16);
  IDM_EDIT_LOWERCASE          = (IDM_EDIT + 17);

  IDM_EDIT_BLOCK_COMMENT    = (IDM_EDIT + 22);
  IDM_EDIT_STREAM_COMMENT    = (IDM_EDIT + 23);
  IDM_EDIT_TRIMTRAILING      = (IDM_EDIT + 24);

  IDM_EDIT_RTL        = (IDM_EDIT+26);
  IDM_EDIT_LTR        = (IDM_EDIT+27);
  IDM_EDIT_SETREADONLY    = (IDM_EDIT+28);
  IDM_EDIT_FULLPATHTOCLIP    = (IDM_EDIT+29);
  IDM_EDIT_FILENAMETOCLIP    = (IDM_EDIT+30);
  IDM_EDIT_CURRENTDIRTOCLIP  = (IDM_EDIT+31);

  IDM_EDIT_CLEARREADONLY    = (IDM_EDIT+33);
  IDM_EDIT_COLUMNMODE      = (IDM_EDIT+34);
  IDM_EDIT_BLOCK_COMMENT_SET  = (IDM_EDIT+35);
  IDM_EDIT_BLOCK_UNCOMMENT    = (IDM_EDIT+36);

  IDM_EDIT_AUTOCOMPLETE          = (50000+0);
  IDM_EDIT_AUTOCOMPLETE_CURRENTFILE  = (50000+1);
  IDM_EDIT_FUNCCALLTIP        = (50000+2);

  //Belong to MENU FILE
  IDM_OPEN_ALL_RECENT_FILE  = (IDM_EDIT + 40);
  IDM_CLEAN_RECENT_FILE_LIST  = (IDM_EDIT + 41);

  IDM_SEARCH       = (IDM + 3000);

  IDM_SEARCH_FIND                  = (IDM_SEARCH + 1);
  IDM_SEARCH_FINDNEXT        = (IDM_SEARCH + 2);
  IDM_SEARCH_REPLACE              = (IDM_SEARCH + 3);
  IDM_SEARCH_GOTOLINE        = (IDM_SEARCH + 4);
  IDM_SEARCH_TOGGLE_BOOKMARK    = (IDM_SEARCH + 5);
  IDM_SEARCH_NEXT_BOOKMARK    = (IDM_SEARCH + 6);
  IDM_SEARCH_PREV_BOOKMARK    = (IDM_SEARCH + 7);
  IDM_SEARCH_CLEAR_BOOKMARKS    = (IDM_SEARCH + 8);
  IDM_SEARCH_GOTOMATCHINGBRACE  = (IDM_SEARCH + 9);
  IDM_SEARCH_FINDPREV        = (IDM_SEARCH + 10);
  IDM_SEARCH_FINDINCREMENT    = (IDM_SEARCH + 11);
  IDM_SEARCH_FINDINFILES      = (IDM_SEARCH + 13);
  IDM_SEARCH_VOLATILE_FINDNEXT  = (IDM_SEARCH + 14);
  IDM_SEARCH_VOLATILE_FINDPREV  = (IDM_SEARCH + 15);
  IDM_SEARCH_CUTMARKEDLINES      = (IDM_SEARCH + 18);
  IDM_SEARCH_COPYMARKEDLINES    = (IDM_SEARCH + 19);
  IDM_SEARCH_PASTEMARKEDLINES    = (IDM_SEARCH + 20);
  IDM_SEARCH_DELETEMARKEDLINES    = (IDM_SEARCH + 21);
  IDM_SEARCH_MARKALLEXT1      = (IDM_SEARCH + 22);
  IDM_SEARCH_UNMARKALLEXT1    = (IDM_SEARCH + 23);
  IDM_SEARCH_MARKALLEXT2      = (IDM_SEARCH + 24);
  IDM_SEARCH_UNMARKALLEXT2    = (IDM_SEARCH + 25);
  IDM_SEARCH_MARKALLEXT3      = (IDM_SEARCH + 26);
  IDM_SEARCH_UNMARKALLEXT3    = (IDM_SEARCH + 27);
  IDM_SEARCH_MARKALLEXT4      = (IDM_SEARCH + 28);
  IDM_SEARCH_UNMARKALLEXT4    = (IDM_SEARCH + 29);
  IDM_SEARCH_MARKALLEXT5      = (IDM_SEARCH + 30);
  IDM_SEARCH_UNMARKALLEXT5    = (IDM_SEARCH + 31);
  IDM_SEARCH_CLEARALLMARKS    = (IDM_SEARCH + 32);

  IDM_VIEW  = (IDM + 4000);
  IDM_VIEW_TOOLBAR_HIDE      = (IDM_VIEW + 1);
  IDM_VIEW_TOOLBAR_REDUCE      = (IDM_VIEW + 2);
  IDM_VIEW_TOOLBAR_ENLARGE    = (IDM_VIEW + 3);
  IDM_VIEW_TOOLBAR_STANDARD    = (IDM_VIEW + 4);
  IDM_VIEW_REDUCETABBAR      = (IDM_VIEW + 5);
  IDM_VIEW_LOCKTABBAR        = (IDM_VIEW + 6);
  IDM_VIEW_DRAWTABBAR_TOPBAR     = (IDM_VIEW + 7);
  IDM_VIEW_DRAWTABBAR_INACIVETAB  = (IDM_VIEW + 8);
  IDM_VIEW_POSTIT          = (IDM_VIEW + 9);
  IDM_VIEW_TOGGLE_FOLDALL      = (IDM_VIEW + 10);
  IDM_VIEW_USER_DLG        = (IDM_VIEW + 11);
  IDM_VIEW_LINENUMBER             = (IDM_VIEW + 12);
  IDM_VIEW_SYMBOLMARGIN           = (IDM_VIEW + 13);
  IDM_VIEW_FOLDERMAGIN            = (IDM_VIEW + 14);
  IDM_VIEW_FOLDERMAGIN_SIMPLE     = (IDM_VIEW + 15);
  IDM_VIEW_FOLDERMAGIN_ARROW      = (IDM_VIEW + 16);
  IDM_VIEW_FOLDERMAGIN_CIRCLE     = (IDM_VIEW + 17);
  IDM_VIEW_FOLDERMAGIN_BOX        = (IDM_VIEW + 18);
  IDM_VIEW_ALL_CHARACTERS       = (IDM_VIEW + 19);
  IDM_VIEW_INDENT_GUIDE       = (IDM_VIEW + 20);
  IDM_VIEW_CURLINE_HILITING    = (IDM_VIEW + 21);
  IDM_VIEW_WRAP          = (IDM_VIEW + 22);
  IDM_VIEW_ZOOMIN           = (IDM_VIEW + 23);
  IDM_VIEW_ZOOMOUT         = (IDM_VIEW + 24);
  IDM_VIEW_TAB_SPACE            = (IDM_VIEW + 25);
  IDM_VIEW_EOL              = (IDM_VIEW + 26);
  IDM_VIEW_EDGELINE            = (IDM_VIEW + 27);
  IDM_VIEW_EDGEBACKGROUND          = (IDM_VIEW + 28);
  IDM_VIEW_TOGGLE_UNFOLDALL      = (IDM_VIEW + 29);
  IDM_VIEW_FOLD_CURRENT      = (IDM_VIEW + 30);
  IDM_VIEW_UNFOLD_CURRENT          = (IDM_VIEW + 31);
  IDM_VIEW_FULLSCREENTOGGLE      = (IDM_VIEW + 32);
  IDM_VIEW_ZOOMRESTORE          = (IDM_VIEW + 33);
  IDM_VIEW_ALWAYSONTOP          = (IDM_VIEW + 34);
  IDM_VIEW_SYNSCROLLV      = (IDM_VIEW + 35);
  IDM_VIEW_SYNSCROLLH      = (IDM_VIEW + 36);
  IDM_VIEW_EDGENONE        = (IDM_VIEW + 37);
  IDM_VIEW_DRAWTABBAR_CLOSEBOTTUN  = (IDM_VIEW + 38);
  IDM_VIEW_DRAWTABBAR_DBCLK2CLOSE  = (IDM_VIEW + 39);
  IDM_VIEW_REFRESHTABAR          = (IDM_VIEW + 40);
  IDM_VIEW_WRAP_SYMBOL          = (IDM_VIEW + 41);
  IDM_VIEW_HIDELINES            = (IDM_VIEW + 42);
  IDM_VIEW_DRAWTABBAR_VERTICAL     = (IDM_VIEW + 43);
  IDM_VIEW_DRAWTABBAR_MULTILINE  = (IDM_VIEW + 44);
  IDM_VIEW_DOCCHANGEMARGIN    = (IDM_VIEW + 45);


  IDM_VIEW_FOLD      = (IDM_VIEW + 50);
  IDM_VIEW_FOLD_1    = (IDM_VIEW_FOLD + 1);
  IDM_VIEW_FOLD_2    = (IDM_VIEW_FOLD + 2);
  IDM_VIEW_FOLD_3   = (IDM_VIEW_FOLD + 3);
  IDM_VIEW_FOLD_4      = (IDM_VIEW_FOLD + 4);
  IDM_VIEW_FOLD_5    = (IDM_VIEW_FOLD + 5);
  IDM_VIEW_FOLD_6      = (IDM_VIEW_FOLD + 6);
  IDM_VIEW_FOLD_7      = (IDM_VIEW_FOLD + 7);
  IDM_VIEW_FOLD_8      = (IDM_VIEW_FOLD + 8);

  IDM_VIEW_UNFOLD      = (IDM_VIEW + 60);
  IDM_VIEW_UNFOLD_1    = (IDM_VIEW_UNFOLD + 1);
  IDM_VIEW_UNFOLD_2    = (IDM_VIEW_UNFOLD + 2);
  IDM_VIEW_UNFOLD_3     = (IDM_VIEW_UNFOLD + 3);
  IDM_VIEW_UNFOLD_4      = (IDM_VIEW_UNFOLD + 4);
  IDM_VIEW_UNFOLD_5    = (IDM_VIEW_UNFOLD + 5);
  IDM_VIEW_UNFOLD_6      = (IDM_VIEW_UNFOLD + 6);
  IDM_VIEW_UNFOLD_7      = (IDM_VIEW_UNFOLD + 7);
  IDM_VIEW_UNFOLD_8      = (IDM_VIEW_UNFOLD + 8);


  IDM_VIEW_GOTO_ANOTHER_VIEW   =   10001;
  IDM_VIEW_CLONE_TO_ANOTHER_VIEW=  10002;
  IDM_VIEW_GOTO_NEW_INSTANCE   =   10003;
  IDM_VIEW_LOAD_IN_NEW_INSTANCE  =   10004;

  IDM_VIEW_SWITCHTO_OTHER_VIEW  = (IDM_VIEW + 72);


  IDM_FORMAT  = (IDM + 5000);
  IDM_FORMAT_TODOS      = (IDM_FORMAT + 1);
  IDM_FORMAT_TOUNIX    = (IDM_FORMAT + 2);
  IDM_FORMAT_TOMAC       = (IDM_FORMAT + 3);
  IDM_FORMAT_ANSI       = (IDM_FORMAT + 4);
  IDM_FORMAT_UTF_8      = (IDM_FORMAT + 5);
  IDM_FORMAT_UCS_2BE    = (IDM_FORMAT + 6);
  IDM_FORMAT_UCS_2LE      = (IDM_FORMAT + 7);
  IDM_FORMAT_AS_UTF_8  = (IDM_FORMAT + 8);
  IDM_FORMAT_CONV2_ANSI    = (IDM_FORMAT + 9);
  IDM_FORMAT_CONV2_AS_UTF_8  = (IDM_FORMAT + 10);
  IDM_FORMAT_CONV2_UTF_8    = (IDM_FORMAT + 11);
  IDM_FORMAT_CONV2_UCS_2BE  = (IDM_FORMAT + 12);
  IDM_FORMAT_CONV2_UCS_2LE  = (IDM_FORMAT + 13);

  IDM_LANG   = (IDM + 6000);
  IDM_LANGSTYLE_CONFIG_DLG  = (IDM_LANG + 1);
  IDM_LANG_C       = (IDM_LANG + 2);
  IDM_LANG_CPP     = (IDM_LANG + 3);
  IDM_LANG_JAVA     = (IDM_LANG + 4);
  IDM_LANG_HTML     = (IDM_LANG + 5);
  IDM_LANG_XML    = (IDM_LANG + 6);
  IDM_LANG_JS      = (IDM_LANG + 7);
  IDM_LANG_PHP    = (IDM_LANG + 8);
  IDM_LANG_ASP    = (IDM_LANG + 9);
  IDM_LANG_CSS        = (IDM_LANG + 10);
  IDM_LANG_PASCAL    = (IDM_LANG + 11);
  IDM_LANG_PYTHON    = (IDM_LANG + 12);
  IDM_LANG_PERL    = (IDM_LANG + 13);
  IDM_LANG_OBJC    = (IDM_LANG + 14);
  IDM_LANG_ASCII    = (IDM_LANG + 15);
  IDM_LANG_TEXT    = (IDM_LANG + 16);
  IDM_LANG_RC      = (IDM_LANG + 17);
  IDM_LANG_MAKEFILE  = (IDM_LANG + 18);
  IDM_LANG_INI    = (IDM_LANG + 19);
  IDM_LANG_SQL    = (IDM_LANG + 20);
  IDM_LANG_VB       = (IDM_LANG + 21);
  IDM_LANG_BATCH    = (IDM_LANG + 22);
  IDM_LANG_CS         = (IDM_LANG + 23);
  IDM_LANG_LUA        = (IDM_LANG + 24);
  IDM_LANG_TEX        = (IDM_LANG + 25);
  IDM_LANG_FORTRAN    = (IDM_LANG + 26);
  IDM_LANG_SH         = (IDM_LANG + 27);
  IDM_LANG_FLASH      = (IDM_LANG + 28);
  IDM_LANG_NSIS       = (IDM_LANG + 29);
  IDM_LANG_TCL        = (IDM_LANG + 30);
  IDM_LANG_LISP       = (IDM_LANG + 31);
  IDM_LANG_SCHEME     = (IDM_LANG + 32);
  IDM_LANG_ASM        = (IDM_LANG + 33);
  IDM_LANG_DIFF       = (IDM_LANG + 34);
  IDM_LANG_PROPS      = (IDM_LANG + 35);
  IDM_LANG_PS         = (IDM_LANG + 36);
  IDM_LANG_RUBY       = (IDM_LANG + 37);
  IDM_LANG_SMALLTALK  = (IDM_LANG + 38);
  IDM_LANG_VHDL       = (IDM_LANG + 39);
  IDM_LANG_CAML       = (IDM_LANG + 40);
  IDM_LANG_KIX        = (IDM_LANG + 41);
  IDM_LANG_ADA        = (IDM_LANG + 42);
  IDM_LANG_VERILOG    = (IDM_LANG + 43);
  IDM_LANG_AU3        = (IDM_LANG + 44);
  IDM_LANG_MATLAB     = (IDM_LANG + 45);
  IDM_LANG_HASKELL    = (IDM_LANG + 46);
  IDM_LANG_INNO       = (IDM_LANG + 47);
  IDM_LANG_CMAKE      = (IDM_LANG + 48);
  IDM_LANG_YAML       = (IDM_LANG + 49);

  IDM_LANG_EXTERNAL  = (IDM_LANG + 50);
  IDM_LANG_EXTERNAL_LIMIT  = (IDM_LANG + 79);

  IDM_LANG_USER    = (IDM_LANG + 80);     //46080
  IDM_LANG_USER_LIMIT    = (IDM_LANG + 110);  //46110


  IDM_ABOUT   = (IDM  + 7000);
  IDM_HOMESWEETHOME  = (IDM_ABOUT  + 1);
  IDM_PROJECTPAGE    = (IDM_ABOUT  + 2);
  IDM_ONLINEHELP    = (IDM_ABOUT  + 3);
  IDM_FORUM      = (IDM_ABOUT  + 4);
  IDM_PLUGINSHOME    = (IDM_ABOUT  + 5);
  IDM_UPDATE_NPP    = (IDM_ABOUT  + 6);
  IDM_WIKIFAQ      = (IDM_ABOUT  + 7);
  IDM_HELP      = (IDM_ABOUT  + 8);


  IDM_SETTING    = (IDM + 8000);
  IDM_SETTING_TAB_SIZE            = (IDM_SETTING + 1);
  IDM_SETTING_TAB_REPLCESPACE  = (IDM_SETTING + 2);
  IDM_SETTING_HISTORY_SIZE  = (IDM_SETTING + 3);
  IDM_SETTING_EDGE_SIZE  = (IDM_SETTING + 4);
  IDM_SETTING_IMPORTPLUGIN  = (IDM_SETTING + 5);
  IDM_SETTING_IMPORTSTYLETHEMS  = (IDM_SETTING + 6);

  IDM_SETTING_TRAYICON            = (IDM_SETTING + 8);
  IDM_SETTING_SHORTCUT_MAPPER     = (IDM_SETTING + 9);
  IDM_SETTING_REMEMBER_LAST_SESSION     = (IDM_SETTING + 10);
  IDM_SETTING_PREFERECE     = (IDM_SETTING + 11);

  IDM_SETTING_AUTOCNBCHAR        = (IDM_SETTING + 15);

// Menu macro
  IDM_MACRO_STARTRECORDINGMACRO    = (IDM_EDIT + 18);
  IDM_MACRO_STOPRECORDINGMACRO     = (IDM_EDIT + 19);
  IDM_MACRO_PLAYBACKRECORDEDMACRO  = (IDM_EDIT + 21);
  IDM_MACRO_SAVECURRENTMACRO   = (IDM_EDIT + 25);
  IDM_MACRO_RUNMULTIMACRODLG  = (IDM_EDIT+32);

  IDM_EXECUTE  = (IDM + 9000);

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
    function GetCurrentBytes: TBytes;
    procedure SetCurrentBytes(value : TBytes);
    function GetCurrentName: String;
    function GetSelectedBytes: TBytes;
    procedure SetSelectedBytes(Value: TBytes);
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
    procedure DoNppnFileClosed; virtual;
    procedure DoNppnFilebeforeClose; virtual;
    procedure DoNppnBufferChange; virtual;
    procedure DoNppnToolbarModification; virtual;
    procedure DoNppnShutdown; virtual;
    procedure DoNppnTextModified; virtual;
    procedure DoNppnDwellStart(offset : integer); virtual;
    procedure DoNppnDwellEnd; virtual;
    procedure DoStateChanged; virtual;

    // df
    property CurrentBytes : TBytes read GetCurrentBytes write SetCurrentBytes;
    property SelectedBytes : TBytes read GetSelectedBytes write SetSelectedBytes;
    property currentFileName : String read GetCurrentName;
    Procedure NewFile(content : TBytes);
    procedure SaveFileAs(filename : String);

    function DoOpen(filename: String): boolean; overload;
    function DoOpen(filename: String; Line: Integer): boolean; overload;
    procedure GetFileLine(var filename: String; var Line: Integer);
    function GetWord: string;
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

function TNppPlugin.GetCurrentName: String;
var
  s: string;
begin
  SetLength(s, MAX_PATH);
  FillChar(s[1], MAX_PATH, #0);
  SendMessage(self.NppData.NppHandle, NPPM_GETFULLCURRENTPATH, MAX_PATH, LPARAM(PChar(s)));
  Result := s;
  setLength(result, Pos(#0, result)-1);
end;

function TNppPlugin.GetCurrentBytes: TBytes;
var
  l : integer;
begin
  setLength(result, 0);
  l := SendMessage(self.NppData.ScintillaMainHandle, SCI_GETTEXTLENGTH, 0, 0)+1;
  if l > 1024 * 1024 then
    exit();

  SetLength(result, l);
  SendMessage(self.NppData.ScintillaMainHandle, SCI_GETTEXT, l, LPARAM(Pointer(result)));
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

  r := SendMessage(self.NppData.ScintillaMainHandle, FHIR.Npp.ScintillaFU.SCI_GETCURRENTPOS, 0, 0);
  Line := SendMessage(self.NppData.ScintillaMainHandle, FHIR.Npp.ScintillaFU.SCI_LINEFROMPOSITION, r, 0);
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

function TNppPlugin.GetSelectedBytes: TBytes;
var
  l : integer;
begin
  setLength(result, 0);
  l := SendMessage(self.NppData.ScintillaMainHandle, SCI_GETSELTEXT, 0, 0)+1;
  SetLength(result, l);
  SendMessage(self.NppData.ScintillaMainHandle, SCI_GETSELTEXT, l, LPARAM(Pointer(result)));
  SetLength(result, l-2);
end;

procedure TNppPlugin.BeNotified(sn: PSCNotification);
begin
  if (HWND(sn^.nmhdr.hwndFrom) = self.NppData.NppHandle) then
  begin
    if (sn^.nmhdr.code = NPPN_TB_MODIFICATION) then
      self.DoNppnToolbarModification
    else if (sn^.nmhdr.code = NPPN_SHUTDOWN) then
      self.DoNppnShutdown
    else if (sn^.nmhdr.code = NPPN_READY) then
      self.DoNppnReady
    else if (sn^.nmhdr.code = NPPN_FILEOPENED) then
      self.DoNppnFileOpened
    else if (sn^.nmhdr.code = NPPN_FILEBEFORECLOSE) then
      self.DoNppnFilebeforeClose
    else if (sn^.nmhdr.code = NPPN_FILECLOSED) then
      self.DoNppnFileClosed
    else if (sn^.nmhdr.code = NPPN_BUFFERACTIVATED) then
      self.DoNppnBufferChange
  end
  else if (HWND(sn^.nmhdr.hwndFrom) = self.NppData.ScintillaMainHandle) then
  begin
    if (sn^.nmhdr.code = SCN_MODIFIED) then
      self.DoNppnTextModified
    else if (sn^.nmhdr.code = SCN_UPDATEUI) then
      self.DoStateChanged
    else if (sn^.nmhdr.code = SCN_DWELLSTART) then
      self.DoNppnDwellStart(sn^.position)
    else if (sn^.nmhdr.code = SCN_DWELLEND) then
      self.DoNppnDwellEnd
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


procedure TNppPlugin.NewFile(content: TBytes);
begin
  SendMessage(self.NppData.NppHandle, NPPM_MENUCOMMAND, 0, IDM_FILE_NEW);
  CurrentBytes := content;
end;

// todo: why must this be an ansi string?
procedure TNppPlugin.SaveFileAs(filename: String);
begin
  SendMessage(self.NppData.NppHandle, NPPM_SAVECURRENTFILEAS, 0, LPARAM(PChar(filename)));
end;

procedure TNppPlugin.SetCurrentBytes(value: TBytes);
begin
  SetLength(value, length(Value)+1);
  value[length(Value)-1] := 0;
  SendMessage(self.NppData.ScintillaMainHandle, SCI_SETTEXT, length(value)-1, LPARAM(@value[0]));
end;

procedure TNppPlugin.SetInfo(NppData: TNppData);
begin
  self.NppData := NppData;
  Application.Handle := NppData.NppHandle;
end;

procedure TNppPlugin.SetSelectedBytes(Value: TBytes);
begin
  SetLength(value, length(Value)+1);
  value[length(Value)-1] := 0;
  SendMessage(self.NppData.ScintillaMainHandle, SCI_REPLACESEL, length(value)-1, LPARAM(@value[0]));
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
    SendMessage(self.NppData.ScintillaMainHandle, FHIR.Npp.ScintillaFU.SCI_GOTOLINE, Line,0);
  Result := r;
end;

procedure TNppPlugin.DoStateChanged;
begin
  // override these
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

procedure TNppPlugin.DoNppnFilebeforeClose;
begin
  // override these
end;

procedure TNppPlugin.DoNppnFileClosed;
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
